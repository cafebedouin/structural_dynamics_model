% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Fixation Discipline on Constitutional Interpretation
 *   domain: legal/political
 *
 * SUMMARY:
 *   The colloquial label 'how the Constitution means' covers three
 *   structurally distinct claims, decomposed per the epsilon-invariance
 *   principle into a constraint family: this file instantiates the
 *   originalist reading — constitutional meaning fixed at the ratification
 *   moment, judges bound by historical public meaning — as a clean,
 *   single-epsilon constraint. The epsilon referent is the operative fixation
 *   discipline itself (the standing arrangement under contest), not any rival
 *   method this reading would replace. The living-constitutionalist and
 *   positivist readings are separate stories linked through
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: the claimed type is tangled_rope because the arrangement
 *   pairs a genuine coordination function (one shared, democratically
 *   traceable interpretive standard) with asymmetric extraction (denied
 *   remedies for claims lacking enactment-era anchors, governance by meaning
 *   authored by exclusionary publics, suppression of rival methods),
 *   sustained by active enforcement. The metric profile is authored
 *   descriptively: substantial but bounded extraction, high suppression that
 *   intensified as the method moved from academic position to appointment
 *   filter to controlling judicial methodology, moderate theater reflecting
 *   selective historicism alongside real historical constraint.
 *
 * KEY AGENTS:
 *   - - originalist_judiciary: Agenda-setting enforcer and legitimacy collector (institutional/identity_locked) — administers the historical-meaning method and is bound by it
 *   - - counter_majoritarian_constraint_advocates: Primary beneficiary (organized/mobile) — supplies doctrine, staffing pipeline, and political cover
 *   - - electoral_majorities: Secondary beneficiary (institutional/arbitrage) — insulated from unenumerated-principle review, adjusts the method via appointments
 *   - - rights_claimants_without_historical_pedigree: Primary target (powerless/trapped) — claims denied for want of enactment-era anchors
 *   - - groups_excluded_from_ratifying_public: Structural target (moderate/trapped) — governed by meaning authored without their inclusion
 *   - - living_constitutionalist_adjudicators: Suppressed minority seat (institutional/constrained) — method outvoted, voice confined to dissents
 *   - - legal_historians: Analytical observer (analytical/analytical) — supplies the evidentiary record, sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Fixation Discipline on Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '5af05bbf-6d24-479a-aa8e-73a880deab99').
narrative_ontology:cs_kernel_codification('5af05bbf-6d24-479a-aa8e-73a880deab99', fixed_text).
narrative_ontology:cs_authority_grounding('5af05bbf-6d24-479a-aa8e-73a880deab99', lineage).
narrative_ontology:cs_interpretation_layer_present('5af05bbf-6d24-479a-aa8e-73a880deab99').
narrative_ontology:cs_reading_relation('5af05bbf-6d24-479a-aa8e-73a880deab99', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5af05bbf-6d24-479a-aa8e-73a880deab99', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('5af05bbf-6d24-479a-aa8e-73a880deab99', foundational, fixation_at_enactment_binds_judges).
narrative_ontology:cs_axiom_status(fixation_at_enactment_binds_judges, holdable).
narrative_ontology:cs_axiom_grounding('5af05bbf-6d24-479a-aa8e-73a880deab99', fixation_at_enactment_binds_judges, conventional).
narrative_ontology:cs_axiom('5af05bbf-6d24-479a-aa8e-73a880deab99', foundational, judicial_discretion_illegitimate_absent_enacted_warrant).
narrative_ontology:cs_axiom_status(judicial_discretion_illegitimate_absent_enacted_warrant, holdable).
narrative_ontology:cs_axiom_grounding('5af05bbf-6d24-479a-aa8e-73a880deab99', judicial_discretion_illegitimate_absent_enacted_warrant, deontological).
narrative_ontology:cs_reference_frame('5af05bbf-6d24-479a-aa8e-73a880deab99', ratification_moment_public_meaning).
narrative_ontology:cs_drift_state('5af05bbf-6d24-479a-aa8e-73a880deab99', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5af05bbf-6d24-479a-aa8e-73a880deab99', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, electoral_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_pedigree).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, groups_excluded_from_ratifying_public).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, living_constitutionalist_adjudicators).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, fixation_thesis).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, popular_sovereignty_constraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides constitutional questions through enactment-era public-meaning inquiry, polices the method through opinion writing, certiorari selection, and precedent management, and staffs lower courts with method-aligned appointees. Collects legitimacy from grounding judicial review in enacted meaning rather than personal moral judgment, and is simultaneously bound by the method it administers: a sitting justice cannot abandon it without repudiating their own recent jurisprudence and the professional identity built on it.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, originalist_judiciary, beneficiary).

% Scholars, litigators, think tanks, and political actors who supply the intellectual infrastructure, staffing pipeline, and political cover for the fixation method. As the method spreads through the bench, they gain influence, careers, and appointment leverage. Their commitment is portable: if returns diminished they could redirect effort to other projects at modest cost.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% Enact legislation with reduced exposure to judicial invalidation grounded in unenumerated or evolving principles, since review must anchor in enacted meaning. They retain appointment leverage over the bench, so when the method's outputs displease them they adjust its personnel rather than exit the arrangement.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, electoral_majorities, beneficiary,
    institutional, biographical, arbitrage, national).

% Litigants whose claims — unenumerated liberties, modern dignity and privacy interests, evolving equal-protection arguments — find no anchor in enactment-era public meaning. Their remedy path runs through constitutional amendment, which the same supermajority structure renders nearly inaccessible. They cannot opt out of the constitutional order that denies their claims.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_pedigree, payer,
    powerless, biographical, trapped, national).

% Modern communities descended from those excluded from the publics that fixed the meaning — enslaved people, women, the propertyless at 1788 and at later amendment dates. They are governed by a semantic standard authored without their participation; their present political power cannot reach backward into the meaning-setting moment, and the forward amendment path is equally steep.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, groups_excluded_from_ratifying_public, payer,
    moderate, generational, trapped, national).

% Judges and justices whose interpretive method loses decisive votes as method-aligned appointees accumulate. Their contributions survive mainly in dissenting opinions; their doctrinal commitments are progressively overruled or hollowed. Exit means resignation or method conversion, both professionally costly.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_adjudicators, payer,
    institutional, biographical, constrained, national).

% Produce the archival record the method consumes and watch their scholarship recruited selectively into advocacy on both sides. They neither collect from nor bear the costs of the arrangement directly, but see the full structure: which historical evidence enters opinions, and which is left at the courthouse door.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legal_historians, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one publicly discoverable meaning for each constitutional provision at its enactment moment, giving judges, officials, and citizens a shared, predictable interpretive standard and a democratically traceable warrant for judicial review.
% TRANSFER_FUNCTION: Moves interpretive authority over contested constitutional questions from sitting judges' contemporary moral judgment (and from present majorities' self-understanding) to the enactment-moment public's semantic understanding; concretely, it moves remedial outcomes away from rights claimants lacking historical anchors and toward interests the fixed meaning protects.
% ABSENT_VOICES: Those excluded from the ratifying publics — enslaved people, women, the propertyless — could not object at the moments that fixed the meaning governing them, and their descendants object only through an amendment path the same supermajority structure blocks. Every person born after the last relevant amendment is likewise governed by meaning they had no part in making; their seat exists only as a future amendment constituency, not as a participant.
% DISAPPEARANCE_RATIONALE: If the fixation discipline vanished overnight, open method competition would resume on the bench, the precedent portfolio built on historical argument would destabilize, appointment politics organized around method alignment would reorganize around something else, rights litigation would shift to principle-based argument, and the scholarly and advocacy infrastructure supplying the method would lose its organizing object.
% FOUNDING_PROBLEM: The countermajoritarian difficulty: unelected judges wielding judicial review against enacted law needed a legitimacy ground, and discretionary appeal to evolving standards looked like rule by nine lawyers untethered from the sovereign act of We the People.
% FOUNDING_PROBLEM_CORROBORATION: The problem's reality is corroborated from outside the benefiting parties: living-constitutionalist scholars and empirical political scientists independently attest both the countermajoritarian difficulty and the dangers of unbounded judicial creativity. What no outside source attests is that fixation solves it — critics inside and outside the movement document discretion migrating into historical-evidence selection rather than disappearing. The problem is corroborated; the solution's adequacy is disputed by every seat outside the beneficiary set.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial but bounded: the discipline denies remedies to rights claimants lacking historical pedigree and governs present generations by meaning authored by narrow, exclusionary publics, yet it also delivers real coordination goods — predictability, a shared standard, democratically traceable review — and its harshest edge is softened by the meaning/application distinction, which routes contemporary circumstances to application. Suppression (0.72) is high because persistence depends on actively policing rival methods: appointment screening, professional sanction, and progressive overruling of precedent not anchored in historical meaning. Theater (0.34) is moderate: historical inquiry genuinely constrains outcomes in many cases, but a documented share of historical argument is outcome-selective — history recruited after the conclusion is chosen. Accessibility collapse (0.55) is partial: within a method-aligned bench, alternatives collapse, but rival readings persist as live positions in the academy, in dissents, and in electoral politics. Resistance (0.60) is real: dissents, critical scholarship, state-level pushback, and litigant adaptation. The temporal series run on one shared grid (1971/1982/1993/2004/2015/2025) so every tracked metric is authored at every examined point; the rising suppression_requirement series traces genuine enforcement-capacity build-out — from marginal academic theory, through the appointment-filter era, to controlling methodology — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the advocate and electoral-majority seats the arrangement presents as legitimate democratic constraint — the price of popular sovereignty and judicial humility. From the rights-claimant and excluded-descendant seats the same structure operates as governance by a dead and unrepresentative hand, with the amendment valve welded shut. From the judiciary's own seat it presents as a discipline that both empowers (legitimacy, insulation) and binds (loss of discretion, historical indeterminacy risk). The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: advocates (mobile exit, portable commitment) derive career and influence returns; electoral majorities hold arbitrage-grade exit, adjusting the method's personnel rather than bearing its costs; the judiciary collects legitimacy rents while administering the rules. Targets sit near the full-target end: rights claimants are trapped (no exit from the constitutional order, amendment path blocked), excluded-descendant communities are trapped one level deeper (no access even to the meaning-setting moment), and living-constitutionalist adjudicators are constrained — inside the institution, unable to convert method without professional cost. Trapped exits amplify effective extraction for the target seats; arbitrage dampens it for the electoral seat. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the true structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — judicial discretion threatening legitimacy — is contested rather than dead: originalists hold that fixation solved it; critics hold that discretion migrated into historical-evidence selection. Mandatrophy is therefore not resolved, and the founding_problem_status x disappearance_verdict pair (contested x world_rearranges) correctly avoids the zombie flag. The tangled_rope classification prevents mislabeling in both directions: naming the coordination function blocks a pure-snare misread (the discipline really does coordinate interpretation and constrain judges), while the victim declarations block a pure-rope misread (real, identifiable parties pay through the same structure that coordinates). The rising suppression series guards against the opposite drift error — reading the arrangement's early marginal phase as its steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the originalist reading capture the operative constraint on US constitutional interpretation, or is the operative arrangement a hybrid the three readings of the us_constitution_meaning kernel jointly contest?',
    'Track which reading''s premises control outcomes in contested constitutional cases over successive sessions; code the interpretive warrants cited in majority opinions against each reading''s signature commitments.',
    'If the operative arrangement is a hybrid, this story''s epsilon and victim set shift materially — parts of the measured extraction belong to sibling constraints, and the enforcement surface splits across readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether one reading or a hybrid constitutes the operative interpretive constraint.').

omega_variable(
    meaning_application_separability,
    'Is the meaning/application distinction stable enough to route contemporary-circumstance effects to application only, or does application evolution smuggle meaning change?',
    'Compare outcomes across domains where the distinction is load-bearing: if evolved-application rulings systematically redefine the underlying right''s content, the distinction is leaking.',
    'If inseparable, the constraint''s effective extraction expands to include all evolved-application denials, raising epsilon and pushing classification toward the snare boundary; if robust, part of the apparent extraction belongs to the application layer and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaning_application_separability, conceptual, 'Whether the reading''s central conceptual buffer holds under operational stress.').

omega_variable(
    selective_historicism_theater,
    'How much of the historical method''s operation is outcome-driven evidence selection rather than genuine constraint?',
    'Systematic coding of historical argumentation in majority opinions against outcomes: measure how often history cutting against the preferred result changes the result, and how often contrary evidence is omitted or minimized.',
    'Higher selectivity raises theater_ratio, erodes the coordination-function credit, and drifts classification toward degraded or purely extractive types; low selectivity supports the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_historicism_theater, empirical, 'Share of historical argument that is performative rather than constraining.').

omega_variable(
    dead_hand_legitimacy_status,
    'Is governance by enactment-moment majorities — including publics that disenfranchised most of the governed — a legitimacy-preserving constraint or intergenerational extraction?',
    'Not resolvable by data alone: it turns on prior commitments about democratic authorization across time. Comparative constitutional practice and acceptance studies can inform but not settle it.',
    'If extraction, the victim costs are not the price of coordination and the classification hardens toward snare; if legitimate constraint, the same costs are coordination price and the rope component dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_legitimacy_status, preference, 'Normative status of dead-hand governance over the living and previously excluded.').

omega_variable(
    amendment_valve_functionality,
    'Does the Article V amendment path function as a real corrective valve for fixed meaning, or is it so blocked that fixation is effectively permanent?',
    'Amendment success-rate analysis against comparable national constitutions, plus study of near-miss campaigns (ERA, D.C. statehood) to estimate the practical threshold.',
    'A functioning valve lowers measured suppression and supports a transitional, scaffold-adjacent reading of the discipline''s harsher edges; a blocked valve raises suppression and locks the target seats'' trapped status in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_valve_functionality, empirical, 'Whether formal amendment access translates into practical correction capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1971, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1971, us_constitution_meaning__originalist_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement(us_c_tr_t1982, us_constitution_meaning__originalist_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(us_c_tr_t1993, us_constitution_meaning__originalist_reading, theater_ratio, 1993, 0.24).
narrative_ontology:measurement(us_c_tr_t2004, us_constitution_meaning__originalist_reading, theater_ratio, 2004, 0.27).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_meaning__originalist_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_meaning__originalist_reading, theater_ratio, 2025, 0.34).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1971, us_constitution_meaning__originalist_reading, base_extractiveness, 1971, 0.28).
narrative_ontology:measurement(us_c_be_t1982, us_constitution_meaning__originalist_reading, base_extractiveness, 1982, 0.36).
narrative_ontology:measurement(us_c_be_t1993, us_constitution_meaning__originalist_reading, base_extractiveness, 1993, 0.44).
narrative_ontology:measurement(us_c_be_t2004, us_constitution_meaning__originalist_reading, base_extractiveness, 2004, 0.51).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_meaning__originalist_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_meaning__originalist_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1971, us_constitution_meaning__originalist_reading, suppression_requirement, 1971, 0.25).
narrative_ontology:measurement(us_c_su_t1982, us_constitution_meaning__originalist_reading, suppression_requirement, 1982, 0.38).
narrative_ontology:measurement(us_c_su_t1993, us_constitution_meaning__originalist_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(us_c_su_t2004, us_constitution_meaning__originalist_reading, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_meaning__originalist_reading, suppression_requirement, 2015, 0.67).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_meaning__originalist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the us_constitution_meaning kernel. The colloquial label 'constitutional interpretation' conflates three structurally distinct claims with different epsilon values, beneficiary structures, and failure modes: this story (originalist_reading — meaning fixed at enactment, judges bound by historical public meaning; substantial extraction concentrated on rights claimants lacking historical anchors), living_constitutionalist_reading (principles endure, application evolves; extraction profile centered on predictability losses and legitimacy contestation), and positivist_reading (validity from enactment procedures alone; weakest normative binding on judges). Upstream/downstream structure: the originalist reading's institutional ascent changes the operating environment of both siblings — it pressures living-constitutionalist argument into historical idiom and gives the positivist reading a renewed enactment-grounding constituency — without logically eliminating either. Each member links the others via affects_constraints; no member averages over the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
