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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Reading of the U.S. Constitution (1787 Ratification-Fixed Meaning)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story authors the originalist reading of the U.S. Constitution as
 *   ONE structurally distinct constraint within the contested kernel
 *   us_constitution_1787: constitutional meaning is fixed at ratification
 *   (1787-1791), and framers' and ratifiers' original public meaning is
 *   binding on subsequent interpretation. This is not the living-constitution
 *   reading (aspirational, evolving text) nor the positivist reading (text
 *   plus democratic amendment, judicial interpretation bounded by text alone)
 *   — those are separate constraint files linked via
 *   network.affects_constraints. Under its own terms, the originalist reading
 *   narrows the constraint set to what 1787-era evidence supports,
 *   legitimizes pre-1787 practices as interpretive baselines, places modern
 *   social-rights claims outside the boundary by construction, and imposes
 *   high epistemic demands on historical evidence that are frequently
 *   contested by professional historians. The ε authored here is for the
 *   standing arrangement as this reading's own adherents would describe its
 *   operation in practice (including the coordination benefit it claims and
 *   the extraction its critics — and increasingly its own historians —
 *   document), not for either sibling reading's alternative.
 *
 * KEY AGENTS:
 *   - originalist_judiciary_and_scholars: agenda-setting/beneficiary seat, institutional power, arbitrage exit — develops and applies the methodology
 *   - groups_excluded_from_1787_franchise: primary payer seat, powerless, trapped exit — structurally absent from the founding evidence base this reading treats as binding
 *   - claimants_of_unenumerated_modern_rights: payer seat, moderate power, constrained exit — excluded from the constraint boundary by the reading's own terms
 *   - constitutional_historians: analytical observer seat — supplies or withholds the corroborating evidence the reading's legitimacy depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.52).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading of the U.S. Constitution (1787 Ratification-Fixed Meaning)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '5fad5941-beb7-4628-a83f-4cf2d63e30fe').
narrative_ontology:cs_kernel_codification('5fad5941-beb7-4628-a83f-4cf2d63e30fe', fixed_text).
narrative_ontology:cs_authority_grounding('5fad5941-beb7-4628-a83f-4cf2d63e30fe', lineage).
narrative_ontology:cs_interpretation_layer_present('5fad5941-beb7-4628-a83f-4cf2d63e30fe').
narrative_ontology:cs_reading_relation('5fad5941-beb7-4628-a83f-4cf2d63e30fe', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('5fad5941-beb7-4628-a83f-4cf2d63e30fe', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('5fad5941-beb7-4628-a83f-4cf2d63e30fe', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('5fad5941-beb7-4628-a83f-4cf2d63e30fe', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('5fad5941-beb7-4628-a83f-4cf2d63e30fe', foundational, framers_ratifiers_intent_binding_absent_amendment).
narrative_ontology:cs_axiom_status(framers_ratifiers_intent_binding_absent_amendment, holdable).
narrative_ontology:cs_axiom_grounding('5fad5941-beb7-4628-a83f-4cf2d63e30fe', framers_ratifiers_intent_binding_absent_amendment, deontological).
narrative_ontology:cs_axiom('5fad5941-beb7-4628-a83f-4cf2d63e30fe', secondary, judicial_discretion_illegitimate_absent_historical_anchor).
narrative_ontology:cs_axiom_status(judicial_discretion_illegitimate_absent_historical_anchor, holdable).
narrative_ontology:cs_axiom_grounding('5fad5941-beb7-4628-a83f-4cf2d63e30fe', judicial_discretion_illegitimate_absent_historical_anchor, instrumental).
narrative_ontology:cs_reference_frame('5fad5941-beb7-4628-a83f-4cf2d63e30fe', ratification_era_original_public_meaning).
narrative_ontology:cs_drift_state('5fad5941-beb7-4628-a83f-4cf2d63e30fe', post_1937_administrative_state_and_civil_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5fad5941-beb7-4628-a83f-4cf2d63e30fe', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judiciary_and_scholars).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, movements_seeking_deregulatory_or_pre_new_deal_outcomes).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, state_governments_favoring_narrow_federal_power).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, groups_excluded_from_1787_franchise).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, claimants_of_unenumerated_modern_rights).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, administrative_agencies_and_beneficiaries_of_modern_regulatory_state).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, textual_determinacy_at_ratification).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, popular_sovereignty_through_ratifying_conventions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges, law professors, and advocacy organizations (e.g. Federalist Society-aligned networks) who develop and apply originalist methodology, control judicial appointments pipelines, and author the historical-evidence standards that decide cases. They set the interpretive agenda and collect professional, institutional, and policy-outcome benefits from the reading's dominance in appellate and Supreme Court practice.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judiciary_and_scholars, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, originalist_judiciary_and_scholars, beneficiary).

% Business associations, property-rights litigants, and states'-rights advocates who benefit when the ratification-fixed reading narrows the scope of federal regulatory and civil-rights power to what 1787-era evidence supports. They can and do move fluidly between courts, legislatures, and public advocacy depending on where the reading is winning.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, movements_seeking_deregulatory_or_pre_new_deal_outcomes, beneficiary,
    organized, generational, mobile, national).

% State legislatures and attorneys general who gain policy latitude when federal power is read narrowly against the historical ratification baseline. They benefit structurally but are bound within the federal system and cannot simply exit it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, state_governments_favoring_narrow_federal_power, beneficiary,
    powerful, generational, constrained, national).

% Descendants of enslaved people, women, and other groups with no voice in the 1787-1788 ratification process. Under this reading, the binding meaning was fixed by a constituency that excluded them by design; they bear the cost of a fixed-meaning framework whose founding evidence base structurally cannot reflect their interests, and cannot retroactively enter the ratifying convention that supposedly binds them.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, groups_excluded_from_1787_franchise, payer,
    powerless, civilizational, trapped, national).

% Litigants asserting privacy, reproductive, or other rights not traceable to specific 1787-1791 textual commitments or historical practice. Under this reading their claims fall outside the constraint boundary by construction; their only paths are amendment (extremely high threshold) or waiting for doctrinal realignment, neither of which is within their control.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, claimants_of_unenumerated_modern_rights, payer,
    moderate, biographical, constrained, national).

% Federal agencies and the populations who depend on modern regulatory programs (environmental, labor, financial) whose statutory authority rests on post-1937 constitutional doctrine. Under a strict ratification-fixed reading, much of this authority is vulnerable to being read as ultra vires relative to 1787 evidence; agencies must litigate defensively and cannot simply relocate their authority elsewhere.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, administrative_agencies_and_beneficiaries_of_modern_regulatory_state, payer,
    institutional, generational, constrained, national).

% Historians who assess the actual evidentiary basis for claims about ratification-era understanding, often finding the historical record more contested, fragmentary, and internally divided than the originalist reading's confidence implies. They supply (or withhold) the corroborating evidence the reading depends on.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% Jurists and scholars committed to the sibling readings (evolving meaning, or text-plus-amendment) who dispute that 1787 intent is binding at all. They are not absent from the broader constitutional debate, but within THIS reading's own framework their premises are treated as methodologically illegitimate rather than engaged as live alternatives.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_reading_and_positivist_reading_adherents, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_judiciary_and_scholars).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, historically anchored decision procedure for resolving constitutional disputes, aiming to constrain judicial discretion by tying interpretation to a fixed, publicly ratified textual and historical record rather than to shifting judicial policy preferences.
% TRANSFER_FUNCTION: Moves interpretive authority and resulting policy outcomes toward those whose preferred outcomes align with 1787-1791 era social, economic, and political arrangements, and away from claimants whose interests were unrepresented at ratification or whose rights claims postdate it; also shifts litigation and doctrinal risk onto the modern regulatory state.
% ABSENT_VOICES: The ratification-era enslaved population, women, and unpropertied men had no voice in the 1787-1788 conventions this reading treats as the binding source of meaning — they are structurally absent from the very evidentiary record the reading privileges, not merely absent from today's debate. Modern rights-claimants are excluded from the constraint boundary by the reading's own terms.
% DISAPPEARANCE_RATIONALE: If the originalist reading lost its current institutional dominance overnight, doctrines narrowing federal regulatory power, restricting unenumerated rights, and privileging historical-practice tests would lose their operative force; agencies, litigants, and lower courts would reorganize around whichever sibling reading filled the vacuum, and a substantial body of recently decided case law would become vulnerable to reversal.
% FOUNDING_PROBLEM: Originalism was substantially revived and formalized in the late 20th century to solve a perceived problem of unconstrained judicial policymaking under the living-constitution and substantive-due-process doctrines of the mid-20th century — the claim was that judges were legislating from the bench under cover of constitutional interpretation, and a fixed historical anchor was needed to discipline that discretion.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and scholars themselves attest the discretion problem remains live. Legal historians and comparative-constitutionalism scholars outside the originalist movement (and some positivist-reading adherents) attest that the historical record originalism relies on is frequently indeterminate or contested at the level of granularity needed to decide hard cases, meaning the reading substitutes one form of interpretive discretion (selecting among competing historical claims) for another rather than eliminating discretion — a claim originalism's own proponents dispute.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that under this reading's actual operation, outcomes systematically transfer interpretive authority and resulting policy discretion toward parties whose interests align with 1787-era arrangements and away from those excluded from or unaddressed by that record — this is a real cost borne by identifiable groups, not merely an interpretive preference. Suppression (0.52) is moderate: the reading does not physically coerce compliance, but it does foreclose certain rights claims and regulatory authorities as a matter of methodology, and its institutional entrenchment (appointments pipelines, stare decisis) makes departure costly. Accessibility collapse (0.62) is elevated because once the ratification-fixed framework is adopted as the interpretive premise, alternative readings are treated as methodologically illegitimate rather than as live options within that framework — collapse is real but not total, since sibling readings persist as institutionally organized alternatives outside this reading's own logic. Resistance (0.68) is high: historians, positivist-reading adherents, living-reading adherents, and affected claimant groups actively contest both the historical premises and the outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (originalist judiciary and scholars), the arrangement is genuine coordination: a determinate, non-arbitrary decision procedure disciplining judicial discretion against a public, ratified record. From the payer seats (excluded-franchise descendants, unenumerated-rights claimants, regulatory-state stakeholders), the same structure operates as an enforcement mechanism that forecloses their claims by definitional fiat, using a historical record whose formation they never had a voice in. The engine's per-seat computation should reflect this asymmetry directly from the beneficiary/victim and exit-option declarations, not from any narrative reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary/scholars and movements favoring pre-New-Deal outcomes sit near the full-beneficiary end: they set the interpretive terms and collect professional, doctrinal, and policy benefits, with mobile or arbitrage-grade exit (they can shift venues, forums, or strategies). Groups excluded from the 1787 franchise sit at the far target end: trapped exit, powerless, and structurally unable to have influenced the very evidentiary record now treated as binding on them — this is the clearest case for override consideration, though the beneficiary/victim declaration alone should already push d high. Claimants of unenumerated modern rights and regulatory-state stakeholders are constrained-exit targets: they can litigate or lobby for amendment but cannot exit the constitutional system itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial policymaking without a disciplining anchor) is authored as contested rather than resolved, because originalists themselves maintain the problem is live while outside historians document that the reading substitutes contested historical adjudication for the discretion it claims to eliminate. This prevents mislabeling the reading as pure extraction (it does solve a real coordination problem — determinacy and public accountability of the interpretive standard — for at least some seats) while also preventing it from being laundered as pure natural-law-style Mountain: it requires active enforcement (appointments, stare decisis, methodological gatekeeping) and names concrete victims, which is why tangled_rope rather than mountain or rope is the authored claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_record_determinacy,
    'Is the 1787-1791 historical record sufficiently determinate to ground the confident, case-deciding conclusions originalist methodology produces, or is it fragmentary and internally contested enough that originalist outcomes reflect selection among competing historical narratives rather than discovery of a fixed meaning?',
    'Systematic review of ratification-era debates, Federalist/Anti-Federalist writings, and state ratifying convention records by historians outside the originalist legal movement, cross-checked against instances where originalist scholars reach opposing conclusions from the same evidence base.',
    'If the record is genuinely determinate, the reading''s coordination claim (discretion-discipline via fixed historical anchor) is substantially vindicated and extraction is better read as a side effect of correct historical outcomes rather than manipulation. If the record is substantially indeterminate, the reading functions as a discretion-laundering mechanism, and authored extractiveness should be read as a floor rather than a ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_determinacy, empirical, 'Whether ratification-era historical evidence is determinate enough to bear the interpretive weight originalism places on it.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the originalist reading diverge from the positivist reading, given both treat the text as authoritative — is the disagreement located in whether subjective framer intent (versus objective original public meaning) governs, or in whether post-ratification amendment and democratic practice can supersede ratification-era meaning without formal amendment?',
    'Comparative doctrinal analysis of case outcomes where originalist and positivist methodologies are both applied to the same constitutional question and produce different results; identify which methodological step causes the divergence.',
    'If the divergence is primarily about subjective-intent-versus-public-meaning, the readings are closer structurally than commonly presented and the relation to positivist_reading may be better characterized as influences rather than coexists_with. If the divergence is about amendment-supersession, coexists_with is the more accurate relation, as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Precise structural location of disagreement between originalist and positivist readings of the same kernel.').

omega_variable(
    excluded_franchise_beneficiary_status,
    'Are descendants of groups excluded from the 1787-1788 franchise properly classified as victims of this reading specifically, or as victims of the underlying historical exclusion that the reading merely inherits and formalizes without independently causing?',
    'Counterfactual analysis: compare outcomes for these groups under originalist versus living/positivist readings holding the underlying historical exclusion constant — if outcomes differ substantially by reading, the reading itself (not merely the historical exclusion) is doing independent extractive work.',
    'If outcomes differ substantially by reading, the victim declaration and high extraction score are well-grounded as properties of THIS constraint. If outcomes converge regardless of reading, the extraction is better attributed to the underlying kernel or to historical circumstance, and this story''s ε may be inflated relative to the reading''s independent causal contribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_franchise_beneficiary_status, conceptual, 'Whether excluded-franchise harms are attributable to this reading specifically or to the underlying historical exclusion it inherits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__originalist_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_1787__originalist_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__originalist_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_1787__originalist_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(us_c_tr_t2018, us_constitution_1787__originalist_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__originalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__originalist_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_1787__originalist_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__originalist_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_1787__originalist_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(us_c_be_t2018, us_constitution_1787__originalist_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__originalist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__originalist_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_1787__originalist_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__originalist_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_1787__originalist_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(us_c_su_t2018, us_constitution_1787__originalist_reading, suppression_requirement, 2018, 0.49).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__originalist_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint files decomposing the natural-language label 'the U.S. Constitution's binding meaning' per the epsilon-invariance principle: originalist_reading (this file), living_reading, and positivist_reading. Each reading has its own epsilon, beneficiary/victim structure, and classification because each instantiates a structurally distinct constraint on interpretive authority, even though all three share the same underlying text and institutional apparatus (courts, doctrine of stare decisis, amendment process). The three files should be read as a constraint family, not as three measurements of one constraint; disagreement among them is the kernel contest itself, not measurement noise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
