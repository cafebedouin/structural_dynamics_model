% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation (US Constitution)
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint story captures the originalist reading of the US
 *   Constitution as a single, ε-invariant constraint. The reading
 *   instantiates a rigid interpretive method: constitutional meaning is fixed
 *   at ratification and judicial interpretation must recover original public
 *   understanding through historical evidence. Post-ratification practice is
 *   irrelevant unless it evidences original meaning. The constraint operates
 *   with high suppression of adaptive interpretation through institutional
 *   enforcement (judicial appointments, law review gatekeeping, clerkship
 *   pipelines) and professional identity formation. The conservative legal
 *   movement and originalist judiciary are structural beneficiaries; rights
 *   claimants whose claims lack founding-era grounding and living
 *   constitutionalist practitioners are structural victims. The constraint is
 *   claimed as tangled_rope — it presents as genuine coordination (solving
 *   judicial discretion/legitimacy problems) but carries asymmetric
 *   extraction (conservative policy outcomes, institutional dominance for the
 *   beneficiary network).
 *
 * KEY AGENTS:
 *   - conservative_legal_movement: Primary beneficiary (institutional/arbitrage) — captures judicial appointments, sets interpretive agenda, collects institutional rents
 *   - originalist_judiciary: Agenda setter/beneficiary (institutional/identity_locked) — administers the constraint, career-constituted by it
 *   - federalist_society_network: Beneficiary (organized/identity_locked) — talent pipeline, institutional infrastructure, professional identity
 *   - rights_claimants_ungrounded_in_founding_era: Primary victim (powerless/trapped) — claims excluded by temporal gating, no exit from constitutional system
 *   - living_constitutionalist_practitioners: Victim (moderate/constrained) — professional marginalization, interpretive exclusion
 *   - progressive_legal_academy: Victim (organized/constrained) — institutional exclusion from elite pipelines, but retains academic platforms
 *   - positivist_practitioners: Observer/excluded (moderate/mobile) — distinct reading, not directly targeted but structurally adjacent
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Constitutional Interpretation (US Constitution)").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '3b6ad697-c6de-417f-b16b-53e95a6df10b').
narrative_ontology:cs_kernel_codification('3b6ad697-c6de-417f-b16b-53e95a6df10b', fixed_text).
narrative_ontology:cs_authority_grounding('3b6ad697-c6de-417f-b16b-53e95a6df10b', lineage).
narrative_ontology:cs_interpretation_layer_present('3b6ad697-c6de-417f-b16b-53e95a6df10b').
narrative_ontology:cs_reading_relation('3b6ad697-c6de-417f-b16b-53e95a6df10b', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b6ad697-c6de-417f-b16b-53e95a6df10b', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('3b6ad697-c6de-417f-b16b-53e95a6df10b', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('3b6ad697-c6de-417f-b16b-53e95a6df10b', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('3b6ad697-c6de-417f-b16b-53e95a6df10b', foundational, judicial_duty_to_recover_original_public_meaning).
narrative_ontology:cs_axiom_status(judicial_duty_to_recover_original_public_meaning, holdable).
narrative_ontology:cs_axiom_grounding('3b6ad697-c6de-417f-b16b-53e95a6df10b', judicial_duty_to_recover_original_public_meaning, deontological).
narrative_ontology:cs_axiom('3b6ad697-c6de-417f-b16b-53e95a6df10b', secondary, post_ratification_practice_irrelevant_unless_evidences_original_meaning).
narrative_ontology:cs_axiom_status(post_ratification_practice_irrelevant_unless_evidences_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('3b6ad697-c6de-417f-b16b-53e95a6df10b', post_ratification_practice_irrelevant_unless_evidences_original_meaning, conventional).
narrative_ontology:cs_reference_frame('3b6ad697-c6de-417f-b16b-53e95a6df10b', ratification_era_public_understanding).
narrative_ontology:cs_drift_state('3b6ad697-c6de-417f-b16b-53e95a6df10b', contemporary_originalist_dominance, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('3b6ad697-c6de-417f-b16b-53e95a6df10b', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, federalist_society_network).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_ungrounded_in_founding_era).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, living_constitutionalist_practitioners).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, progressive_legal_academy).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, judicial_restraint_virtue).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, democratic_legitimacy_through_fixed_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Captures judicial appointments, sets the interpretive agenda through law reviews and think tanks, and collects institutional rents (prestige, funding, policy outcomes). The movement built the infrastructure (Federalist Society, originalist scholarship) that makes the constraint operationally effective. Exit is arbitrage-grade: they could pivot to other interpretive methods but the constraint is their primary power base.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, arbitrage, national).

% Administers the constraint from the bench — writes opinions, sets precedent, controls the interpretive methodology. Their professional identity is constituted by originalism (clerks, societies, scholarly output). Exit would require abandoning the interpretive identity that defines their career; identity_locked is structural, not merely psychological.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, originalist_judiciary, beneficiary).

% Provides the talent pipeline (law students → clerks → judges), intellectual infrastructure (conferences, publications), and professional community. Members' careers and professional self-concept are fused with the network's originalist mission. Exit options are identity_locked: leaving means exiting the professional world that constituted them.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, federalist_society_network, beneficiary,
    organized, biographical, identity_locked, national).

% Individuals and groups whose constitutional claims (reproductive rights, LGBTQ+ rights, voting rights expansions, etc.) lack grounding in 18th/19th century public understanding. They are trapped in the constitutional system — no exit to another constitutional order — and the temporal gate categorically excludes their claims regardless of moral weight or contemporary consensus.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_ungrounded_in_founding_era, payer,
    powerless, biographical, trapped, national).

% Judges, scholars, and advocates committed to adaptive interpretation. They face professional marginalization: fewer elite clerkships, law review exclusion, appointment barriers. They can still practice and publish but with reduced institutional influence. Exit is constrained: they could adopt originalist methods but would abandon their interpretive commitments.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_practitioners, payer,
    moderate, biographical, constrained, national).

% Law schools, journals, and academic centers where living constitutionalist and progressive scholarship persists. They retain academic platforms and student audiences but are excluded from the elite pipelines (Supreme Court clerkships, federal judgeships, OLC positions) that the originalist network controls. Exit is constrained: they could reorient but would lose their distinctive intellectual project.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, progressive_legal_academy, payer,
    organized, generational, constrained, national).

% Scholars and judges who ground constitutional validity in formal enactment procedures rather than historical meaning or moral reading. They are not directly targeted by originalist suppression but their distinct reading is partially displaced in institutional competition. Exit is mobile: they can shift emphasis without identity cost.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, positivist_practitioners, observer,
    moderate, biographical, mobile, national).

% Sees the full structural topology: three readings of one kernel, each with different beneficiary/victim structures, different ε values, different institutional commitments. No stake in the dispute; the constraint story itself is the object of analysis.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the legitimacy problem of judicial review by tethering interpretation to fixed historical meaning, constraining judicial discretion, and providing a determinate method for constitutional decision-making that claims democratic legitimacy through the ratification moment.
% TRANSFER_FUNCTION: Moves interpretive authority and policy outcomes from adaptive interpretation (which would empower rights claimants and progressive majorities) to originalist interpretation (which empowers the conservative legal movement and its institutional network). The transfer operates through the temporal gate: claims grounded in founding-era practice pass; claims requiring evolved understanding fail.
% ABSENT_VOICES: Future generations (who will live under constitutional interpretations they had no role in ratifying), non-originalist state court judges (who must apply federal originalist precedent), international human rights bodies (whose interpretive methods are structurally excluded from US constitutional discourse). They would object to the temporal lockout but are not in the conversation.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, the Supreme Court would immediately revert to some form of living constitutionalism or pragmatic adjudication. Rights claims currently excluded (reproductive autonomy, marriage equality, voting rights protections) would become cognizable. The conservative legal movement would lose its primary interpretive infrastructure. The federal judiciary's composition and decision patterns would shift dramatically within a generation.
% FOUNDING_PROBLEM: The legitimacy crisis of judicial review in the Warren/Burger era: unelected judges overturning democratic enactments without a determinate interpretive method, leading to charges of judicial activism and democratic illegitimacy. Originalism was built to solve this by providing a fixed, historical constraint on judicial discretion.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Scalia, Bork, Barnett, Whittington) attest the problem remains live — judicial discretion is still the threat. Living constitutionalist scholars (Brennan, Dworkin, Strauss, Balkin) and progressive jurists attest the problem has shifted — the threat is now judicial entrenchment of minority rule through originalism. No neutral arbiter exists; the dispute is structural.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.62) reflects the systematic exclusion of rights claims not grounded in founding-era practice — the constraint transfers interpretive authority and policy outcomes to the conservative legal movement. Suppression (0.78) is high because the constraint's persistence depends on actively excluding adaptive interpretation through appointment politics, law review gatekeeping, and professional socialization. Theater ratio (0.28) is moderate: the historical recovery function is real (originalist scholarship produces genuine historical knowledge) but a growing share of enforcement activity defends institutional position rather than methodological purity. Accessibility collapse (0.45) is moderate because alternative interpretive methods remain cognitively available and practiced — they are suppressed, not unthinkable. Resistance (0.55) is significant: living constitutionalist scholarship, progressive litigation, and academic critique persist despite institutional pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judiciary seat (agenda_setter/beneficiary), the constraint appears as genuine coordination: it solves the legitimacy problem of judicial review by tethering interpretation to fixed historical meaning. From the rights_claimants seat (payer/trapped), the same structure operates as enforced extraction: their claims are categorically excluded by a temporal gate they cannot pass. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (conservative_legal_movement, originalist_judiciary, federalist_society_network) collect institutional rents: judicial appointments, academic positions, clerkship pipelines, intellectual authority. Their exit options are arbitrage/identity_locked — they benefit from and are constituted by the constraint. Victims (rights_claimants, living_constitutionalist_practitioners, progressive_legal_academy) bear extraction: excluded claims, professional marginalization, lost policy outcomes. Rights_claimants are trapped (no exit from constitutional system); practitioners are constrained (can practice but with reduced influence). The positivist reading sits adjacent — not directly targeted but its enactment-procedure grounding is partially displaced by originalism's historical-meaning grounding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial legitimacy/discretion control) is contested — originalists attest it remains live; living constitutionalists and progressive scholars attest it has shifted to rights protection. The constraint persists beyond its coordination function because the beneficiary network has achieved institutional dominance and the constraint now serves as the mechanism of that dominance. This is mandatrophy: a coordination arrangement whose founding problem is contested or resolved, but which persists because the beneficiaries capture the enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine recovery of original public meaning, or a constructed interpretive method that benefits identifiable institutional actors?',
    'Comparative analysis of originalist methodology across cases: if originalist analysis consistently converges on conservative policy outcomes regardless of historical evidence, the constraint operates as constructed rather than recovered.',
    'If constructed, the constraint is a false summit (mountain claim masking extraction) and the beneficiary structure reflects institutional capture, not natural legal authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether originalism recovers fixed meaning or constructs it for institutional benefit.').

omega_variable(
    living_constitutionalist_foreclosure,
    'Does the originalist reading logically foreclose the living constitutionalist reading within a single legal framework, or do they coexist as competing but simultaneously holdable positions?',
    'Examine whether any single judicial officer or legal system can simultaneously treat constitutional meaning as fixed at ratification AND as evolving with society without internal contradiction.',
    'If forecloses, the kernel has a genuine structural split; if coexists_with, both readings remain live options in the ongoing dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_constitutionalist_foreclosure, conceptual, 'Structural relationship between originalist and living constitutionalist readings of the same kernel.').

omega_variable(
    positivist_relation,
    'Does the originalist reading foreclose, coexist with, or influence the positivist reading (validity from enactment procedures)?',
    'Analyze whether a judge committed to original public meaning can simultaneously hold that constitutional validity derives solely from formal enactment procedures.',
    'Determines the constraint family topology and whether these readings form a genuine three-way split or a two-plus-one structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_relation, conceptual, 'Structural relationship between originalist and positivist readings.').

omega_variable(
    suppression_mechanism,
    'Is the high suppression of adaptive interpretation structural (institutional enforcement via appointment/confirmation) or internalized (professional identity formation in law schools/clerkships)?',
    'Track suppression trajectory after exit from originalist institutional positions: if former originalist judges/practitioners continue to suppress adaptive interpretation, internalization is significant.',
    'If substantially internalized, the constraint''s effective suppression is higher than institutional measures suggest — the constraint travels with the agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Structural vs. internalized suppression in the originalist interpretive community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_text__originalist_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_text__originalist_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(us_c_tr_t1986, us_constitution_text__originalist_reading, theater_ratio, 1986, 0.12).
narrative_ontology:measurement(us_c_tr_t1991, us_constitution_text__originalist_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_text__originalist_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_text__originalist_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_text__originalist_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__originalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_text__originalist_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_text__originalist_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(us_c_be_t1986, us_constitution_text__originalist_reading, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(us_c_be_t1991, us_constitution_text__originalist_reading, base_extractiveness, 1991, 0.42).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_text__originalist_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_text__originalist_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_text__originalist_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__originalist_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_text__originalist_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_text__originalist_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(us_c_su_t1986, us_constitution_text__originalist_reading, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(us_c_su_t1991, us_constitution_text__originalist_reading, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_text__originalist_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_text__originalist_reading, suppression_requirement, 2016, 0.76).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_text__originalist_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_text__originalist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_text__originalist_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three in the us_constitution_text constraint family. The kernel 'US Constitution text' admits three structurally distinct readings with different ε values, different beneficiary/victim structures, and different classifications. Originalist reading: ε=0.62, tangled_rope, conservative beneficiaries, rights-claimant victims. Living constitutionalist reading: expected lower ε, rope/tangled_rope, progressive beneficiaries, originalist victims. Positivist reading: expected lowest ε, mountain/rope, institutional beneficiaries, minimal victims. The ε-invariance principle requires separate stories because the label 'constitutional interpretation' conflates distinct structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_text__originalist_reading, powerless, 0.95).
constraint_indexing:directionality_override(us_constitution_text__originalist_reading, moderate, 0.7).
constraint_indexing:directionality_override(us_constitution_text__originalist_reading, organized, 0.6).
constraint_indexing:directionality_override(us_constitution_text__originalist_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
