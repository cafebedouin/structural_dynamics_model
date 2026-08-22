% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal_theory/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The four-factor fair use test (17 U.S.C. § 107) provides a legal
 *   framework for determining when use of copyrighted material without
 *   permission is permitted. This story instantiates the CREATOR-CENTRIC
 *   READING of that framework: a reading that interprets the
 *   factors—purpose/character, nature of work, amount/substantiality, and
 *   market harm—as primarily serving to preserve creator (original copyright
 *   holder) incentives and to keep fair use a narrow exception to the
 *   property right. Under this reading, transformative use and derivative
 *   works are analyzed through the lens of market harm to the original and
 *   opportunity cost to the original creator's licensing revenue. This
 *   reading coexists with two sibling readings: the TRANSFORMATIVE-USE
 *   READING (which elevates transformativeness as the dominant factor and
 *   subordinates market harm when new meaning is added) and the USER-CENTRIC
 *   READING (which treats fair use as an affirmative user right protecting
 *   public access and cultural production). The creator-centric reading is
 *   not more accurate than its siblings—it is a different structural
 *   commitment the doctrine could embody. This constraint story models the
 *   measured operation of the doctrine under the creator-centric
 *   interpretation, not a neutral application of the statute.
 *
 * KEY AGENTS:
 *   - copyright_holders: institutional beneficiaries, agenda-setters of the doctrine's application; control the primary litigation path
 *   - transformative_users: moderate-power payers with identity-locked exit; artistic creators whose work faces high fair-use rejection risk
 *   - derivative_work_creators: powerless payers; face licensing requirements where other readings would permit fair use
 *   - public_domain_advocates: organized resistance to doctrine narrowing; excluded from primary case-shaping litigation
 *   - courts_applying_four_factors: institutional observers; their case-by-case holdings instantiate the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.78).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.71).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal_theory/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77').
narrative_ontology:cs_kernel_codification('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', fixed_text).
narrative_ontology:cs_authority_grounding('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', lineage).
narrative_ontology:cs_interpretation_layer_present('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77').
narrative_ontology:cs_reading_relation('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', foundational, copyright_holders_primary_incentive_beneficiaries).
narrative_ontology:cs_axiom_status(copyright_holders_primary_incentive_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', copyright_holders_primary_incentive_beneficiaries, deontological).
narrative_ontology:cs_axiom('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', foundational, fair_use_narrow_exception_not_user_right).
narrative_ontology:cs_axiom_status(fair_use_narrow_exception_not_user_right, holdable).
narrative_ontology:cs_axiom_grounding('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', fair_use_narrow_exception_not_user_right, conventional).
narrative_ontology:cs_reference_frame('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', copyright_protection_primacy_framework).
narrative_ontology:cs_drift_state('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', contemporary_digital_culture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d1e7ac1-5ba0-4d04-90c5-7cc1e077bf77', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, derivative_work_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, derivative_work_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the litigation and licensing path for derivative works. Under the creator-centric reading, they benefit from court interpretations that find market harm from most derivative uses and require transformative creators to seek licenses or face litigation risk. They have the resources to litigate test cases and set precedent.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, national).

% Artists, musicians, writers, and commentators who remix, adapt, or critically engage with existing copyrighted works to create new expression. They face high risk of fair-use loss under the creator-centric reading when their work is deemed commercially harmful to the original or competes with a derivative market the copyright holder controls. Their identity as artists is fused with their transformative practice.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, identity_locked, national).

% Adapters, translators, continuation authors, and multimedia creators building on existing works. The creator-centric reading treats their work as subject to copyright-holder licensing rather than fair use, even when the work adds significant new value. They are also incidental beneficiaries of the coordination function (they can rely on predictable doctrine) but the creator-centric reading's weight assignment makes licensing costs prohibitive for most.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, derivative_work_creators, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, derivative_work_creators, beneficiary).

% Libraries, archivists, academics, and civil-society organizations working to expand public access to cultural works. The creator-centric reading narrows fair use for research, preservation, and digitization projects, requiring licensing negotiations for bulk uses that would advance public knowledge and cultural commons.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates, payer,
    organized, generational, constrained, national).

% Platforms and tools designed around user-generated transformative content (fan-art archives, remix music editors, mashup platforms, remix AI). The creator-centric reading's narrow fair use collapses their defensibility and forces licensing negotiations or restrictive user-filtering policies, which undermines their business model and user communities.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, emerging_remix_platforms, excluded,
    moderate, biographical, trapped, global).

% Judges and appellate courts applying the statutory four-factor test to fair-use claims. Under the creator-centric reading, they are guided to interpret factors 1 and 4 (purpose and market harm) as the decisive factors, finding narrow fair-use protection and presuming market harm from derivative uses.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts_applying_four_factors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal framework (the four-factor test: purpose, nature of work, amount/substantiality, and market harm) for adjudicating permitted reuse of copyrighted material. Solves the coordination problem of distinguishing licensable from non-licensable uses under a single doctrine, reducing litigation unpredictability.
% TRANSFER_FUNCTION: Channels control over derivative uses and transformative reuse back to original copyright holders by interpreting market harm and creator-incentive factors as the primary determinants of fair use scope. Moves power to negotiate fees for derivative work licensing from transformative users to copyright holders.
% ABSENT_VOICES: Remix creators and transformative users with limited resources cannot afford litigation and are structurally absent from the high-stakes appellate cases that shape the doctrine. Small platforms serving fan communities and public-domain advocates with limited funding to litigate precedent are largely unrepresented in the case law that interprets the four-factor test.
% DISAPPEARANCE_RATIONALE: If this reading and its legal fortification vanished and fair use reverted to a user-centric or transformative-use-dominant reading, the landscape of legitimate derivative creation would expand: remix platforms, fan-work communities, and educational adaptations would face lower legal friction; copyright-holder licensing power would compress. The constraint is not natural law — it is the outcome of doctrinal choices courts make in interpreting statutory language.
% FOUNDING_PROBLEM: Early copyright doctrine treated fair use as a vague equitable principle. The four-factor test (17 U.S.C. § 107, codified 1976, drawing on Harper & Row v. Nation) was meant to create predictability and limit judicial ad-hoc balancing by establishing systematic criteria for permitted reuse.
% FOUNDING_PROBLEM_CORROBORATION: Copyright scholars and practitioners confirm the test was designed to create predictability (attested in legislative history and judicial opinions). However, courts and commentators outside the copyright-holder bar argue that the test's application has systematized protection of copyright-holder interests and narrowed fair use in ways that contradict the statute's user-protection intent. Empirical studies (Netanel, Lessig, Balganesh) find the doctrine narrows transformative-use safe harbors relative to statutory language.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 end-state) because the creator-centric reading operates as a de facto taxation on transformative reuse: it narrows the class of unpermitted uses by requiring licensing negotiation, licensing fees, or litigation risk to proceed with derivative works. The constraint extracts control over derivative markets by funneling these uses back to copyright holders rather than permitting them under fair use. Suppression is substantial (0.71) because the mechanism operates through litigation threat and legal uncertainty: transformative users cannot reliably predict whether their use will be found fair, creating a suppressive effect on actual creative attempts. Theater ratio (0.41) reflects that the doctrine maintains a sincere coordination function (the four-factor test is a genuine attempt at systematic adjudication), but an increasing portion of its application defends copyright-holder licensing control rather than serving the statute's stated purpose of promoting learning and culture. The measurement series shows extractiveness rising sharply in the early period (0-21) as the creator-centric reading solidifies in case law, then plateauing (35-50) as the doctrine becomes settled—a trajectory consistent with a constraint that initially extracts value through uncertainty and then stabilizes once all parties internalize the narrow fair-use precedents. Suppression requirement remains steady once established, indicating the doctrine's enforcement machinery stays constant.
 *
 * PERSPECTIVAL GAP:
 *   The copyright-holder seat perceives this constraint as a necessary property-rights framework protecting legitimate incentives for creation and investment. The derivative-creator and transformative-user seats perceive it as an extractive licensing gate that narrows their legal safe harbor and forces them into permission-seeking or litigation risk. Courts perceive themselves as neutrally applying statutory factors, but the creator-centric reading guides them to weight factors that favor copyright-holder control. The engine computes these divergences from the structural data: copyright holders get directionality near 0.0 (beneficiary), transformative users get directionality near 1.0 (target). The constraint's persistence depends on courts making creator-centric factor-weight choices; if courts shifted to transformative-use or user-centric readings, the effective extraction would collapse, because the same statutory text and four factors would then protect a wider range of reuses.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders benefit from the creator-centric interpretation (d~0.1): they collect licensing revenue from derivatives that fair use would otherwise permit, and the reading's emphasis on creator incentives legitimizes their licensing claims. Transformative users and derivative creators bear the costs (d~0.9): they must seek licenses, pay fees, or face litigation risk for uses that the other readings would classify as fair. The identity-locked exit for transformative users (they cannot simply stop being artists or remixers without abandoning their identity) amplifies their directionality toward target status. Public-domain advocates sit near the target end (d~0.8): the narrowing of fair use directly constrains their ability to build research corpora and preservation archives. Courts are analytical seats (d~0.5, neutral); they are not beneficiaries or payers but rather the institutional locus through which the reading is enforced.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (predictability and systematic doctrine) remains partially live: the four-factor test does provide more predictability than pre-1976 equitable balancing. However, the creator-centric reading has ossified that doctrine around copyright-holder interests, turning what was meant as a framework for balancing into a presumption toward property-holder control. The constraint avoids pure snare classification because the coordination function is real—the four-factor test does solve for unified doctrine rather than case-by-case ad-hoc balancing. But it is correctly classified as tangled rope: the coordination (predictable doctrine) is inseparable from the asymmetric extraction (copyright-holder licensing control), and the constraint requires active enforcement (courts must be guided to weigh factors creator-centrically) to persist. A shift to transformative-use or user-centric readings would preserve the coordination function while redistributing who benefits; the constraint is not the four-factor test itself, but rather the specific creator-centric reading of how to weigh those factors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    factor_weight_stability,
    'Is the creator-centric weight assignment to the four factors a stable doctrinal choice, or does appellate case law show systematic variation in factor emphasis across courts and time periods?',
    'Quantitative analysis of appellate fair-use holdings, coding factor emphasis and outcome correlation across decades and circuit courts.',
    'If weights vary substantially, the creator-centric reading is less stable than claimed, and the constraint''s enforcement depends on particular judges and cases rather than settled doctrine. If weights are stable, the reading represents a durable doctrinal equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factor_weight_stability, empirical, 'Whether creator-centric factor weights are consistently applied across time and jurisdictions.').

omega_variable(
    statutory_intent_vs_doctrinal_narrowing,
    'Does the creator-centric interpretation align with the statute''s original stated purpose (to promote learning and the creation of new works), or does it diverge from that legislative intent by narrowing fair use beyond what the statute''s language requires?',
    'Detailed legislative history analysis, comparison of statutory language to early (pre-narrowing) case law, and empirical assessment of whether the doctrine''s current effect matches stated copyright-promotion goals.',
    'If the creator-centric reading narrows fair use beyond statutory intent, the doctrine may be a false summit: a constraint that claims to protect creator incentives while actually suppressing derivative creativity. If the narrowing aligns with intent, the reading is defensible on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_intent_vs_doctrinal_narrowing, conceptual, 'Whether the creator-centric reading''s effect matches the statute''s stated policy aim of promoting new creation.').

omega_variable(
    transformative_use_logical_status,
    'Is transformative use logically distinct from the four-factor test, or is it a different weighting of those same factors?',
    'Doctrinal analysis of whether transformativeness can be derived from the four factors or whether it is an additional, independent criterion courts have introduced.',
    'If transformativeness is a fifth factor courts have added, the creator-centric and transformative-use readings may foreclose each other (one treats transformativeness as derivative from existing factors, the other as primary and distinct). If transformativeness is merely a reweighting, the readings coexist as different hermeneutics of the same statute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformative_use_logical_status, conceptual, 'The logical structure of transformativeness relative to the statutory four-factor test.').

omega_variable(
    derivative_work_market_definition,
    'What counts as ''market harm'' under factor 4? Does the creator-centric reading presume a derivative-work market that the copyright holder has the right to control, or does it assess harm case-by-case?',
    'Analysis of holdings on market-harm findings: whether courts presume harm to licensing markets from derivative uses, or whether they assess actual economic harm empirically.',
    'If courts presume harm to licensing markets, the creator-centric reading operationally assumes copyright holders have a property right to all derivative markets, narrowing fair use substantially. If courts assess harm case-by-case, fair use remains contestable. This is the crux of whether the constraint is extractive or coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_market_definition, empirical, 'Whether factor-4 analysis assumes a derivative-market property right or assesses harm empirically.').

omega_variable(
    suppression_mechanism_internalization,
    'Does the measured suppression (0.71) reflect external legal barriers (litigation risk, licensing requirement) or internalized legal uncertainty where creators avoid risky uses even when they might survive fair-use challenge?',
    'Qualitative research with creators on what deters them from transformative reuse: actual litigation exposure, licensing costs, or learned caution about legal risk.',
    'If suppression is primarily internalized, the constraint carries suppression even after creators gain access to legal defense funds or safe harbors; the creator-centric framing has become part of their decision-making. If it is primarily external, removing legal barriers would permit more derivative creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression operates through external barriers or internalized legal caution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t7, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement_basis(fair_tr_t7, observed).
narrative_ontology:measurement(fair_tr_t14, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement_basis(fair_tr_t14, observed).
narrative_ontology:measurement(fair_tr_t21, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 21, 0.38).
narrative_ontology:measurement_basis(fair_tr_t21, observed).
narrative_ontology:measurement(fair_tr_t28, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement_basis(fair_tr_t28, observed).
narrative_ontology:measurement(fair_tr_t35, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(fair_tr_t35, observed).
narrative_ontology:measurement(fair_tr_t42, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 42, 0.41).
narrative_ontology:measurement_basis(fair_tr_t42, observed).
narrative_ontology:measurement(fair_tr_t50, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(fair_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t7, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 7, 0.65).
narrative_ontology:measurement_basis(fair_be_t7, observed).
narrative_ontology:measurement(fair_be_t14, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 14, 0.69).
narrative_ontology:measurement_basis(fair_be_t14, observed).
narrative_ontology:measurement(fair_be_t21, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 21, 0.73).
narrative_ontology:measurement_basis(fair_be_t21, observed).
narrative_ontology:measurement(fair_be_t28, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 28, 0.76).
narrative_ontology:measurement_basis(fair_be_t28, observed).
narrative_ontology:measurement(fair_be_t35, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 35, 0.77).
narrative_ontology:measurement_basis(fair_be_t35, observed).
narrative_ontology:measurement(fair_be_t42, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 42, 0.78).
narrative_ontology:measurement_basis(fair_be_t42, observed).
narrative_ontology:measurement(fair_be_t50, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(fair_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t7, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 7, 0.64).
narrative_ontology:measurement_basis(fair_su_t7, observed).
narrative_ontology:measurement(fair_su_t14, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 14, 0.66).
narrative_ontology:measurement_basis(fair_su_t14, observed).
narrative_ontology:measurement(fair_su_t21, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement_basis(fair_su_t21, observed).
narrative_ontology:measurement(fair_su_t28, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 28, 0.69).
narrative_ontology:measurement_basis(fair_su_t28, observed).
narrative_ontology:measurement(fair_su_t35, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(fair_su_t35, observed).
narrative_ontology:measurement(fair_su_t42, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 42, 0.71).
narrative_ontology:measurement_basis(fair_su_t42, observed).
narrative_ontology:measurement(fair_su_t50, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(fair_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.14).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair_use_four_factor_test kernel. The sibling readings (transformative_use_reading and user_centric_reading) are not alternative measurements of this constraint; they are structurally distinct constraints with different epsilon values and beneficiary/victim structures, instantiated by different hermeneutic choices applied to the same statutory text. All three readings are linked through network.affects_constraints because they compete for doctrinal dominance in contemporary case law—a shift in which reading courts adopt would change all three constraints' effective extraction simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
