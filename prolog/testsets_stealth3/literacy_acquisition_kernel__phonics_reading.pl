% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Mandated Systematic Phonics Instruction Regime
 *   domain: educational/pedagogical/cognitive-science
 *
 * SUMMARY:
 *   A broad legislative wave now requires early-grades reading instruction to
 *   follow an explicit, systematic phoneme-grapheme sequence delivered before
 *   and alongside connected-text work. Operationally the standing arrangement
 *   consists of state statutes and board rules designating approved scripted
 *   curricula, mandated teacher training in the science of reading,
 *   instructional coaches and fidelity walk-throughs checking pacing and
 *   routine adherence, and funding conditioned on compliant adoption.
 *   Classrooms receive daily scripted lessons; teachers deliver them;
 *   monitors document adherence. The constraint under classification is this
 *   mandated arrangement - the requirement that decoding instruction precede
 *   and enable comprehension - not any individual teacher's voluntary
 *   practice of systematic instruction, which predates and survives every
 *   policy regime. KEY AGENTS (by structural relationship): -
 *   state_education_agencies: Agenda setter (institutional/arbitrage) -
 *   writes the statutes, approves the lists, funds the training, monitors
 *   fidelity - students_with_weak_phonological_awareness: Primary beneficiary
 *   (powerless/trapped) - gains decoding they would not otherwise reliably
 *   acquire - classroom_teachers: Primary target (moderate/constrained) -
 *   delivers the scripts, absorbs the loss of curricular judgment and the
 *   compliance workload - commercial_curriculum_publishers: Secondary
 *   beneficiary (organized/arbitrage) - converts mandates into multi-year
 *   adoption and training revenue - typical_developing_readers: Marginal
 *   dual-positioned party (powerless/trapped) - buys insurance they mostly
 *   did not need at the price of narrowed instructional time -
 *   constructivist_literacy_educators: Excluded dissenting bloc
 *   (institutional/constrained) - lost approval channels after enactment -
 *   independent_reading_researchers: Analytical observer - measures outcomes
 *   and costs across the dispute
 *
 * KEY AGENTS:
 *   - state_education_agencies: agenda setter (institutional power, arbitrage exit)
 *   - students_with_weak_phonological_awareness: primary beneficiary (powerless, trapped)
 *   - classroom_teachers: primary target (moderate power, constrained exit)
 *   - commercial_curriculum_publishers: secondary beneficiary (organized power, arbitrage exit)
 *   - typical_developing_readers: marginal dual-positioned party (powerless, trapped)
 *   - constructivist_literacy_educators: excluded dissenting bloc (institutional power, constrained exit)
 *   - parents_of_struggling_readers: mobilized beneficiary constituency (organized power, constrained exit)
 *   - independent_reading_researchers: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.64).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Mandated Systematic Phonics Instruction Regime").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational/pedagogical/cognitive-science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '9d6ff75a-b194-4532-ba58-b08a8c2fb337').
narrative_ontology:cs_kernel_codification('9d6ff75a-b194-4532-ba58-b08a8c2fb337', formalized).
narrative_ontology:cs_authority_grounding('9d6ff75a-b194-4532-ba58-b08a8c2fb337', expertise).
narrative_ontology:cs_interpretation_layer_present('9d6ff75a-b194-4532-ba58-b08a8c2fb337').
narrative_ontology:cs_reading_relation('9d6ff75a-b194-4532-ba58-b08a8c2fb337', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('9d6ff75a-b194-4532-ba58-b08a8c2fb337', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('9d6ff75a-b194-4532-ba58-b08a8c2fb337', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('9d6ff75a-b194-4532-ba58-b08a8c2fb337', foundational, explicit_decoding_instruction_necessary).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('9d6ff75a-b194-4532-ba58-b08a8c2fb337', explicit_decoding_instruction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('9d6ff75a-b194-4532-ba58-b08a8c2fb337', foundational, code_instruction_precedes_connected_text).
narrative_ontology:cs_axiom_status(code_instruction_precedes_connected_text, holdable).
narrative_ontology:cs_axiom_grounding('9d6ff75a-b194-4532-ba58-b08a8c2fb337', code_instruction_precedes_connected_text, empirically_contingent).
narrative_ontology:cs_reference_frame('9d6ff75a-b194-4532-ba58-b08a8c2fb337', explicit_systematic_code_instruction).
narrative_ontology:cs_drift_state('9d6ff75a-b194-4532-ba58-b08a8c2fb337', contemporary_science_of_reading_legislative_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9d6ff75a-b194-4532-ba58-b08a8c2fb337', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, typical_developing_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, commercial_curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, typical_developing_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and implement statutes requiring explicit, systematic phonics instruction in the early grades: publish approved curriculum lists, fund and require teacher training, deploy coaches and fidelity reviewers, and condition funding on compliant adoption. They can revise or repeal the requirements through new legislation or rulemaking, and they answer electorally for reading scores rather than for classroom workload.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, state_education_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Children who do not intuitively map speech sounds to letters. Explicit letter-sound sequences, blending routines, and cumulative review build decoding they are unlikely to assemble incidentally from storybook exposure. They attend the school they are zoned to and cannot select their classroom's method; what reaches them is whatever the adopted program delivers.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, local).

% Children who would likely crack the alphabetic code under almost any reasonable method, often helped by literate homes. For them the mandated sequence is mostly redundant insurance: part of the instructional day goes to routines they did not need, while the same routines buy them earlier accuracy and fewer gaps. They cannot choose their instruction either.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, typical_developing_readers, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__phonics_reading, typical_developing_readers, payer).

% Deliver the mandated lessons day by day. Many report the scripts conflict with what they know about particular children; pacing guides leave little room to extend a lesson that landed badly or follow a spontaneous opening. Deviating risks a flagged walk-through and, in some districts, employment consequences. Leaving means leaving the profession or the grade band they trained for; working for change inside the system is slow and individually costly.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Watch a child fall behind and campaign for systematic instruction: parent advocacy groups testified for the state laws and continue to monitor district compliance. Their leverage is vocal and local; families with resources can also exit to private or home schooling, which softens the stake of the most resourced among them in public-classroom specifics.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, regional).

% Design and sell the scripted programs and training series that approval lists privilege. A place on a state list converts legislative mandates into multi-year district subscriptions and trainer-certification fees; product teams revise existing lines to match new statute language faster than competitors can enter.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, commercial_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Teacher-education faculty, literacy-coach networks, and curriculum authors whose approach centered immersion in authentic text and responsive mini-lessons. After the statutes passed, their graduate courses, workshop circuits, and published programs lost approval status and funding channels; they publish critiques and support holdout districts, but they are no longer in the rooms where adoption decisions are made.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, constructivist_literacy_educators, excluded,
    institutional, generational, constrained, national).

% Synthesize trial evidence, run longitudinal cohort studies, and audit decoding gains alongside comprehension, motivation, and teacher-retention effects. Several hold commitments inside the dispute; the seat modeled here is the evaluative one - measuring whether the mandated arrangement produces what it promises, and at what cost.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, independent_reading_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, commercial_curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes early reading instruction around an explicit phoneme-grapheme sequence so that every entering kindergartner, regardless of teacher background or district wealth, receives cumulative decoding instruction - solving the variability problem that made decoding acquisition depend on individual teacher knowledge.
% TRANSFER_FUNCTION: Moves instructional decision-making (sequence, pacing, materials) from classroom teachers to state-approved program designs; moves compliance time from teachers to documentation; moves public money to approved publishers and training vendors; moves decoding skill to students.
% ABSENT_VOICES: Constructivist literacy educators and aligned teacher-preparation faculty lost their approval channels after enactment and stand outside adoption decisions; students have no seat anywhere in the process; dissenting teachers reach the conversation mainly through union channels rather than curriculum committees.
% DISAPPEARANCE_RATIONALE: Repeal overnight and districts would revert to heterogeneous local practice within a year or two: many teachers would blend methods by personal conviction, struggling decoders would again depend on which classroom they drew, approval-driven publisher revenue would collapse, and the accountability machinery (lists, fidelity reviews, mandated training) would dissolve with nothing left to monitor.
% FOUNDING_PROBLEM: Mass reading failure treated as a pedagogy problem: mid-century look-say and later whole-language eras left large shares of children - disproportionately poor children - unable to decode, with the gap attributed to the absence of systematic code instruction.
% FOUNDING_PROBLEM_CORROBORATION: Federal assessment trends (NAEP) and commissioned research syntheses (National Reading Panel) attest from outside any publisher's interest that reading failure is persistent and that explicit code instruction improves word-recognition outcomes. No source outside the dispute neutrally attests that instructional method - rather than poverty, instructional time, or assessment design - is the operative cause, and teacher organizations formally dispute the causal attribution. Corroboration thus supports the problem's existence and partial responsiveness to the remedy, not the reading's full causal story.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim (tangled_rope) states the structure as I judge it: a genuine coordination function carrying an asymmetric charge. The metrics describe operation as I observe it. Extractiveness 0.58: costs concentrate on one party - classroom teachers surrender sequencing, pacing, and materials judgment to scripts and add unpaid documentation labor - while another party receives a lifetime-return skill; substantial but not confiscatory on average. Suppression 0.64 is a raw structural property (unscaled by power or scope): approved-curriculum lists close the materials channel, mandated training occupies preparation time, fidelity reviews police deviation, and preparation-program accreditation shifted; the enforcement machinery visibly matured across the interval (see suppression series). Theater_ratio 0.36: pacing-guide signatures, attendance-logged training, and walk-through forms increasingly document rather than improve instruction, though coaching remains partly functional. Accessibility_collapse 0.45: inside a mandated system alternatives are largely closed, but private and home education escapes persist and rival paradigms survive in higher education - alternatives dim rather than vanish. Resistance 0.65: union resolutions, legislative amendments, publisher counter-lobbying, and open teacher dissent meet each mandate wave. Temporal shape is CYCLICAL, not monotonic: the shared eight-point grid covers one full pendulum cycle - code-emphasis rise (t0-t2), rival-paradigm displacement collapsing enforcement (t3-t4), reconstruction (t5-t7) - and the oscillation is driven by external paradigm competition, not intermittent reinforcement; base_properties values are authored at the current revival crest. fixing_cost is authored 'prohibitive': repeal would reopen the reading-failure blame contest that produced the statutes, the dyslexia-advocacy coalition powering them is durable, and whoever could fix it (legislatures) bears costs exceeding any certain benefit.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently, and the engine computes that divergence from the structural data. From the struggling-decoder seat the arrangement operates as subsidy: the same script that binds the teacher hands her the one tool that unlocks print for that child. From the teacher seat the identical artifact is enforced dispossession of judgment, with constrained exit and career-length exposure. From the publisher seat it is a guaranteed market behind an approval moat with arbitrage-grade exit. From the agency seat it is accountable uniformity it can revise at will. Same-power divergence: classroom_teachers and constructivist_literacy_educators hold comparable institutional weight, but the teachers sit inside the delivery path (deviation policed, exit means leaving the profession) while the educator bloc sits outside the approval channel it lost (voice-only, constrained) - equal standing, different exits, different computed positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations plus exit atoms drive the derivation; no directionality overrides were needed because the declarations already differentiate every seat. students_with_weak_phonological_awareness, parents_of_struggling_readers, and typical_developing_readers derive low d near the beneficiary pole (effective extraction damped, inverted into subsidy for the weakest decoders). classroom_teachers derives high d near the target pole, amplified by constrained exit and biographical-horizon identity fusion with curricular professionalism. commercial_curriculum_publishers derives near-full beneficiary d from declaration plus arbitrage exit. state_education_agencies derives low d as administrator-collector. Spatial scope (national statutes) makes fidelity verification harder, so the engine scales effective extraction modestly upward at every seat; suppression enters unscaled, per its raw-structural-property status.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification blocks two symmetrical misreadings. Calling this a rope would erase the asymmetry: a rope requires participants to be net beneficiaries, and teachers are not - their judgment is the charged party. Calling it a snare would commit the inverse error: the coordination function is real and load-bearing (children with weak phonological awareness demonstrably fail to decode without explicit sequencing), so the mandate is not extraction wearing a coordination costume - the costume is the garment. Tangled rope holds both facts. On obsolescence: the founding problem is contested, not dead - assessment stagnation and dyslexia prevalence keep it live for the mandate's sponsors while rivals attribute failure to poverty and instructional time, so no mandatrophy_resolved flag is declared. Leading drift risk: theater accumulation (0.10 to 0.36) hardening into ritual compliance - a piton trajectory - if decoding failure falls while fidelity paperwork persists. The mismatch consumer reads status x verdict: contested x world_rearranges raises no zombie flag today.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story is the phonics_reading instantiation of kernel literacy_acquisition_kernel; which structural elements would change under each sibling reading?',
    'Cross-file comparison of the four sibling stories'' beneficiary/victim sets, axioms, and epsilon values; the delta is located in the necessity and sequencing axioms, not in the enforcement machinery the mandate-era siblings share.',
    'Adopting balanced_literacy_reading symmetrizes the cost structure (teachers regain partial discretion); adopting whole_language_reading dissolves the mandate layer entirely; adopting structured_literacy_reading widens the instructional specification and re-centers the patient population. Classification of THIS story is unchanged unless the kernel itself is reframed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer/kernel-positioning structure routed to omega per Rule 2.').

omega_variable(
    scripted_fidelity_tradeoff,
    'Does enforcing scripted fidelity raise decoding outcomes enough to justify the judgment and morale costs it imposes on teachers, or does stripping discretion degrade implementation quality (differentiation, oral-language richness, motivation) enough to offset the gains?',
    'Randomized or well-matched comparisons of identical content delivered with versus without scripted-fidelity controls, with decoding AND comprehension/motivation/retention endpoints.',
    'If fidelity controls add little beyond unscripted delivery of the same content, the charge on teacher judgment is dead overhead and the teacher seat''s effective extraction rises toward pure-target; if fidelity is load-bearing, the charge prices real coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scripted_fidelity_tradeoff, empirical, 'Whether the script is load-bearing or parasitic on teacher judgment.').

omega_variable(
    accountability_framing_underdetermination,
    'Two framings fit the same statutes: a pedagogical-standard frame (mandating the best-evidenced method) and an accountability-regime frame (standardized tests make decoding cheaply measurable, so measurable subskills win politically regardless of comprehension outcomes). Which frame is doing the classificatory work?',
    'Statute-text analysis (do mandates reference comprehension outcomes or subskill proxies?) plus outcome auditing of comprehension versus word-recognition trajectories under mandates.',
    'Under the accountability frame the arrangement''s effective extractiveness rises (test-prep substitution) and its type leans snare-ward on the teacher seat; under the pedagogical frame the tangled-rope reading stands. Signals guiding the current choice: statute texts foreground instruction; score politics foreground subskills.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_framing_underdetermination, conceptual, 'CS-framing under-determination between pedagogy-frame and accountability-frame.').

omega_variable(
    universality_of_necessity_claim,
    'Does the necessity claim bind all beginning readers, or chiefly the minority with weak phonological awareness, with the remainder paying opportunity cost for insurance?',
    'Subgroup outcome analyses separating baseline phonological-awareness tiers under mandated versus professionally discretionary instruction.',
    'If necessity is subpopulation-bound, the effective victim set widens (median classrooms pay for a minority''s insurance) and effective extraction on the median classroom rises; if broadly binding, the current authorship stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_of_necessity_claim, empirical, 'Scope of the necessity axiom across the learner distribution.').

omega_variable(
    pendulum_persistence,
    'Will the current revival consolidate into a durable regime or swing back as prior waves did (mid-century crest, rival-paradigm displacement, present reconstruction)?',
    'Longitudinal tracking of statutory durability, funding continuity, and teacher-preparation accreditation across election cycles.',
    'Consolidation locks the current tangled-rope profile; another swing would date a type transition and re-date the interval''s terminal state; consolidation with falling theater_ratio would signal maturation toward rope-like settling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pendulum_persistence, empirical, 'Durability of the revival phase against historical cyclicality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__phonics_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__phonics_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(lite_tr_t50, literacy_acquisition_kernel__phonics_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(lite_tr_t60, literacy_acquisition_kernel__phonics_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(lite_tr_t70, literacy_acquisition_kernel__phonics_reading, theater_ratio, 70, 0.36).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(lite_be_t50, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(lite_be_t60, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(lite_be_t70, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(lite_su_t50, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(lite_su_t60, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 60, 0.56).
narrative_ontology:measurement(lite_su_t70, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 70, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% 'How children learn to read' decomposes into four structurally distinct claims (necessity of explicit code instruction; sequencing relative to connected text; breadth of component coverage; scope of applicability) that no single epsilon can span - hence four linked stories. Edges: this reading's evidence base upstream-feeds structured_literacy_reading's extension; the mandate wave exerts resource-and-legitimacy pressure on balanced_literacy_reading (publisher line revisions, district abandonments); whole_language_reading stands in direct logical contradiction to this reading's necessity axiom. Member epsilon values differ, so the family is modeled as separate stories rather than one constraint measurable from any angle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
