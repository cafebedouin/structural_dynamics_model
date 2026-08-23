% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Mandated Simulation Regime as Competence Maintenance (Simulation-Sufficiency Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Across safety-critical industries, regulators mandate recurring
 *   catastrophe rehearsal and accept simulator performance as the evidence of
 *   maintained competence. The standing arrangement under examination is that
 *   regime as this reading endorses it: simulated catastrophe treated as
 *   genuine exercise of the response competence, with fidelity of simulation
 *   governing how much of that exercise survives contact with a real event.
 *   The regime genuinely solves a collective-action problem — no single
 *   operator can sustain multi-agency rehearsal alone, and skills decay
 *   without scheduled use — while a compliance-market layer has grown around
 *   the mandate: compulsory purchase anchoring vendor pricing,
 *   recertification lock-in, and documented-drill artifacts functioning as
 *   liability cover partly decoupled from field readiness. KEY AGENTS (by
 *   structural relationship): - safety_regulators: Agenda-setter and
 *   beneficiary (institutional/constrained) — mandate the regime and collect
 *   demonstrable oversight; - regulated_operators: dual-positioned
 *   beneficiary/payer (powerful/constrained) — fund the apparatus, collect
 *   compliance cover and capability; - simulator_vendors: primary collecting
 *   seat (organized/arbitrage) — own the mandate-created market; -
 *   drill_participants: cost-bearing participants with incidental personal
 *   gain (moderate/constrained); - fidelity_gap_casualties: primary targets
 *   under this reading's narrowed victim set (powerless/trapped) — bear harm
 *   when fidelity proves inadequate; - accreditation_auditors: analytical
 *   observers measuring what the mandate defines.
 *
 * KEY AGENTS:
 *   - safety_regulators: agenda-setter and beneficiary (institutional/constrained) — set drill mandates, collect demonstrable oversight
 *   - regulated_operators: dual-positioned beneficiary/payer (powerful/constrained) — buy mandated simulation, collect compliance cover and genuine fluency
 *   - simulator_vendors: primary collecting seat (organized/arbitrage) — sell into demand created by compulsion
 *   - drill_participants: cost-bearing participants (moderate/constrained) — spend duty hours, carry the skill gains
 *   - fidelity_gap_casualties: primary targets under this reading (powerless/trapped) — harmed when real events exceed the trained envelope
 *   - accreditation_auditors: analytical observers (institutional/analytical) — certify what the mandate defines as competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.5).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Mandated Simulation Regime as Competence Maintenance (Simulation-Sufficiency Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '5a76fb59-1218-447c-81f1-6700ba972da7').
narrative_ontology:cs_kernel_codification('5a76fb59-1218-447c-81f1-6700ba972da7', formalized).
narrative_ontology:cs_authority_grounding('5a76fb59-1218-447c-81f1-6700ba972da7', expertise).
narrative_ontology:cs_interpretation_layer_present('5a76fb59-1218-447c-81f1-6700ba972da7').
narrative_ontology:cs_reading_relation('5a76fb59-1218-447c-81f1-6700ba972da7', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('5a76fb59-1218-447c-81f1-6700ba972da7', exercise_as_competence_maintenance__hybrid_decay_reading, forecloses).
narrative_ontology:cs_axiom('5a76fb59-1218-447c-81f1-6700ba972da7', foundational, simulated_exercise_is_genuine_activation).
narrative_ontology:cs_axiom_status(simulated_exercise_is_genuine_activation, holdable).
narrative_ontology:cs_axiom_grounding('5a76fb59-1218-447c-81f1-6700ba972da7', simulated_exercise_is_genuine_activation, empirically_contingent).
narrative_ontology:cs_axiom('5a76fb59-1218-447c-81f1-6700ba972da7', secondary, fidelity_gradient_determines_retention).
narrative_ontology:cs_axiom_status(fidelity_gradient_determines_retention, holdable).
narrative_ontology:cs_axiom_grounding('5a76fb59-1218-447c-81f1-6700ba972da7', fidelity_gradient_determines_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('5a76fb59-1218-447c-81f1-6700ba972da7', simulated_exercise_parity).
narrative_ontology:cs_drift_state('5a76fb59-1218-447c-81f1-6700ba972da7', contemporary_post_incident_inquiry_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5a76fb59-1218-447c-81f1-6700ba972da7', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_vendors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, fidelity_gap_casualties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_participants).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_participants).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, transfer_of_training_hypothesis).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_certification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets drill-frequency and fidelity requirements across covered industries, audits simulator scorecards and exercise logs as evidence of readiness, and publishes compliance statistics. Oversight budgets and staffing are justified by the demonstrability of this evidence stream. Abandoning or radically shrinking the mandate exposes the agency to blame attribution after any future incident, so stepping away from the regime carries career-institutional risk even where its value is doubted internally.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators, beneficiary).

% Airlines, hospital systems, chemical plants, and utilities that must purchase mandated simulation hours and document exercise completion. They receive certified-ready status, insurer premium treatment, a litigation defense of record ('the crew was drilled'), and genuine procedural fluency among their crews. They also fund the entire apparatus: device purchase, scenario licensing, instructor time, and crew-hours diverted from production. Leaving the regime is not available while they operate licensed assets.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators, payer).

% Manufacture simulation devices, license scenario libraries, sell instructor certification and recurring recertification cycles into demand that exists only because purchase is compulsory. Pricing anchors to the mandate rather than to willingness-to-pay, and product refresh cycles track regulatory revision calendars. The same product lines port across aviation, medicine, energy, and maritime clients, so any single jurisdiction's rule changes pose little commercial risk.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Frontline crews, nursing staff, control-room operators, and shift supervisors who spend scheduled duty hours inside scenarios and are scored on simulator performance. They bear fatigue, schedule disruption, and evaluation stress, and they individually carry the procedural fluency gained. They cannot opt out while employed in covered roles, and their qualitative sense of whether a scenario felt real enters the record only indirectly, filtered through pass/fail scoring.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_participants, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_participants, beneficiary).

% Patients, passengers, plant neighbors, and residents who are physically present when a real event exceeds what the trained-and-certified organization can execute. They inherit the difference between competence as measured in the simulator and competence demanded by the actual event: degraded communications, improvised decisions, delayed evacuation. They arrive after all preparation is complete and have no seat in the design of what prepared them.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, fidelity_gap_casualties, payer,
    powerless, immediate, trapped, global).

% Inspect exercise logs, witness scheduled drills, and sign off on simulator-scored competence evidence for accreditation purposes. They see pass rates and completion certificates; they almost never observe the same organization's behavior during an unscheduled real event. Their instruments measure exactly what the mandate defines as competence, no more.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, accreditation_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_vendors).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a standing population of organizations capable of executing coordinated emergency response: scheduled rehearsals synchronize multi-shift and multi-agency procedure, surface equipment and protocol defects before real events, and preserve perishable procedural skills across the long quiet intervals between rare catastrophes.
% TRANSFER_FUNCTION: Moves crew time and operating budget from production into rehearsal; moves mandate-driven spending from regulated operators to simulator vendors, scenario licensors, and training providers; moves documented compliance artifacts from operators to regulators, insurers, and litigation defense.
% ABSENT_VOICES: Those harmed when fidelity proved inadequate are dead, injured, or dispersed after the fact and are almost never seated on standards committees that define next cycle's scenarios. Independent human-factors researchers studying the limits of skill transfer sit outside the mandate-setting process. Frontline staff assessments of scenario realism reach the record only as scored outcomes, not as testimony about what the exercises failed to reproduce.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished overnight, drill cadence would fall to voluntary and budgetary whim, the mandated simulator market would evaporate along with its pricing anchor, insurers would lose the documented-readiness signal they price against, and regulators would lose their principal evidence of having overseen anything. Competence maintenance would become discretionary and wildly uneven across organizations until the next disaster re-politicized it.
% FOUNDING_PROBLEM: Recurrent disasters showed organizations that had never rehearsed coordinated response failing within the first minutes to hours, with learning purchased afterward at the price of lives; the arrangement was built to force the rehearsal to happen before the event rather than after it.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-incident investigation boards, public inquiries, and accident reports — produced entirely outside the benefiting parties — attest both directions of the record: organizations without recent relevant rehearsal execute badly, and organizations with recent rehearsal execute measurably better initial response. The same investigation literature is the primary external source of the fidelity-gap critique, which corroborates that the founding problem (unrehearsed response) remains live while documenting where the current answer falls short.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).
:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42: the core rehearsal function delivers real capability, so the reading's own lights discount the arrangement heavily toward coordination, but a rent layer demonstrably accumulates — vendor pricing anchored to compulsory purchase, recertification cycles tracking regulatory calendars, and compliance artifacts priced above their readiness content. Suppression is 0.50 and structural throughout: mandates bind through audit schedules, penalty schedules, and insurance premium consequences, not through internalized belief; nobody involved thinks drills are unnecessary because they have been made unable to imagine otherwise. Theater ratio 0.37 and climbing: scenario reuse, teaching-to-the-checklist, and announced-exercise choreography substitute measurable proxy performance for field transfer, but remain below the proxy-domination threshold. Accessibility_collapse 0.45: alternatives persist (unannounced drills, joint field exercises with response agencies, cross-industry scenario exchanges) but the compliant-minimum format crowds them out once the audit logic is understood. Resistance 0.32: operators lobby against mandate expansion and unions negotiate drill compensation, but normative endorsement of rehearsal broadly blunts opposition — nobody campaigns against being prepared. Enforcement history traces one visible half-cycle: aggressive build-up after founding disasters (suppression_requirement rising to 0.66 by t8), then normalization and softening as paperwork compliance became culturally self-sustaining (declining to 0.50). The oscillation driver is external — disaster salience — not an intermittent-reinforcement mechanism owned by the constraint. All three tracked series run on one shared grid (points 0–24 at step 4) so no metric row is sampled against a substituted end-state value.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the collecting seats should compute differently. From the vendor seat the arrangement is a durable revenue architecture built on statutory demand. From the operator seat it is an annoying-but-rational purchase: compliance cover plus capability, worth the invoice. From the participant seat it is taxed time repaid in personally-held skill. From the fidelity-gap casualty seat it is the reason competent-seeming people failed them — the same scorecard that certified the crew is the record of what was never rehearsed. Scope interacts with verification: operators span global footprints while auditors inspect nationally, so larger scope makes the extraction layer harder to verify and the engine scales effective extraction upward accordingly; casualties, trapped and powerless, sit at the full-target end with no dampening exit. Suppression here is structural, not internalized — no internalization component is claimed, and no suppression-mechanism ambiguity omega is required on that axis.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: regulators and vendors declare as beneficiaries and sit near the subsidy end; fidelity_gap_casualties declare as victims with trapped exit and powerless position, placing them at the full-target end with amplified effective extraction. Two corrections were needed because the derivation would otherwise misread dual-positioned agents. First, regulated_operators appear in the beneficiaries array, which pulls the derivation strongly toward the beneficiary pole (~0.15-0.25); but they also fund the entire apparatus — device purchase, licensing, crew-hours — so their true position is near-symmetric, corrected to 0.45 via the powerful-atom override. Second, drill_participants carry no victim declaration (this reading deliberately narrows the victim set to fidelity-gap casualties, per the reading's structural signature), which would seat them near-neutral-or-beneficiary; but they bear real time, fatigue, and evaluation costs against genuine personal skill gains, corrected to 0.58 via the moderate-atom override. The overrides encode cost-bearing that the narrowed victim declaration cannot express.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: catastrophic-risk exposure persists, and the external investigation record attests that unrehearsed organizations still fail. Status live crossed with verdict world_rearranges yields no zombie flag — the arrangement is not surviving past its function. Mandatrophy resolution is therefore not declared, and no sunset clause is authored: nothing in the regime's own structure contemplates its retirement. The classification work this story performs is preventing two symmetrical mislabels. Reading the arrangement as pure coordination ignores that the same statute that schedules rehearsal also manufactures a captive market and a liability artifact — asymmetric extraction with named payers. Reading it as pure extraction ignores that rehearsal demonstrably preserves perishable skill and surfaces protocol defects before events — a coordination function with corroborated external evidence. Tangled_rope holds both facts without letting either erase the other; the temporal series then tracks which side is gaining (rent and theater rising slowly, enforcement softening), which is the drift signal the corpus exists to take. Coalition note: the victim seat here is composed of post-hoc, geographically dispersed casualties who share no organizing identity before the event that harms them, so coalition-power prospects for the powerless seat are weak, and the analysis does not lean on rescue-by-coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the exercise_as_competence_maintenance kernel — which reading is instantiated here, and what would the sibling readings change structurally?',
    'Framework choice resolves at the point of institutional adoption: an oversight body that certifies readiness from simulator scorecards is holding this reading; one that weights field-performance history over drill logs is holding a sibling. The disagreement locates at a single structural element — whether any attainable simulation fidelity reaches judgment-under-stakes — and each sibling answers it differently.',
    'Adopting the lived-catastrophe sibling expands the victim set to everyone whose competence atrophied without real-stakes activation and raises epsilon sharply; adopting the hybrid sibling splits the kernel into separately classified procedural and judgment constraints with distinct victim sets. This file''s classification holds only within the sufficiency reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is the simulation-sufficiency reading of a contested kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    fidelity_adequacy_at_scale,
    'Is fidelity sufficient for retention transfer attainable at deployable scale and cost, or does the attainable-fidelity frontier fall short of what the sufficiency claim requires?',
    'Transfer-effectiveness studies correlating simulator tier with subsequent field performance across industries, paired with cost curves for high-fidelity simulation at fleet-wide scale.',
    'If the fidelity the claim requires cannot be bought at the scale the mandate covers, sufficiency collapses toward the hybrid reading, the effective victim set widens past fidelity-gap casualties, and epsilon rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_adequacy_at_scale, empirical, 'Whether the reading''s own success condition (adequate fidelity, everywhere, affordably) is met in fact.').

omega_variable(
    simulator_metric_predictive_validity,
    'Do simulator performance metrics predict field performance, or do they reward simulator-specific fluency that decays under real stakes?',
    'Linkage datasets matching individual and crew simulator scores to performance in subsequent unscheduled real incidents, controlling for case mix.',
    'Weak predictive validity means the theater ratio is understated at 0.37 and the arrangement drifts toward theatrical maintenance of a credential rather than maintenance of capability — a classification movement toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_metric_predictive_validity, empirical, 'Whether the competence measure the mandate relies on measures the competence the mandate exists to protect.').

omega_variable(
    coordination_extraction_boundary,
    'Where does genuine coordination end and mandate-created rent begin — is the compelled simulator market a necessary cost of the coordination, or rent riding on compulsion?',
    'Compare readiness outcomes across jurisdictions that mandate outcome-audited field exercises versus jurisdictions that mandate simulator-hour counts, at matched spend levels.',
    'If outcomes track audit type rather than spend, the vendor-side share of measured extraction is rent removable by mandate redesign without losing the rehearsal function; if outcomes track spend, most measured extraction is nearer true coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Separability of the rehearsal function from the compliance-market apparatus attached to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.37).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'exercise maintains crisis competence.' The label conflates structurally distinct claims about what exercises the competence kernel; each claim is authored as its own story with its own epsilon, beneficiary/victim structure, and victim set. This file instantiates the sufficiency claim: a comparatively narrow victim set (only those harmed when simulation fidelity proved inadequate) and an epsilon reflecting endorsement of the core rehearsal function plus compliance-layer accumulation. The sibling stories instantiate the lived-catastrophe necessity claim and the hybrid decay claim, with different victim sets and materially different epsilon. Family linkage runs through network.affects_constraints here and through typed edges in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, powerful, 0.45).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
