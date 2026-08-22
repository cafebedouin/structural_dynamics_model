% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Mandatory Simulation-Based Competence Maintenance Regime (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   A standing arrangement across safety-critical sectors mandates recurring
 *   simulation exercises — tabletop drills, full-scale mock events, evaluated
 *   scenarios — as the primary mechanism for maintaining organizational
 *   crisis competence between real events. This story instantiates the
 *   hybrid_decay_reading of the kernel exercise_as_competence_maintenance:
 *   the arrangement genuinely exercises a procedural component of competence
 *   (scripted steps, radio discipline, handoffs, equipment sequencing —
 *   fluency that survives years without real activation) while a second
 *   component, judgment-under-stakes — open-ended triage, improvisation when
 *   the script breaks, willingness to deviate under consequence — finds no
 *   analogue in evaluated rehearsal and decays. The epsilon referent is the
 *   standing simulation-reliance arrangement itself, assessed by this
 *   reading's lights: it delivers real procedural retention while
 *   externalizing the cost of the unexercised component onto responders and
 *   the publics who depend on them. Family epsilon deltas over the same
 *   referent: the simulation_sufficiency_reading authors low epsilon (residue
 *   is under-fidelity); the lived_catastrophe_necessity_reading authors high
 *   epsilon (nothing constitutive is retained); this reading authors
 *   intermediate epsilon 0.60 because one component is genuinely served and
 *   the other is not. KEY AGENTS (by structural relationship): -
 *   exercise_vendors_and_consultants: primary beneficiary (organized/mobile)
 *   — collects the exercise spend; revenue scales with mandated hours -
 *   organizational_compliance_functions: agenda-setter
 *   (institutional/constrained) — administers the internal exercise calendar
 *   and evidence trail - executive_risk_owners: secondary beneficiary
 *   (powerful/arbitrage) — converts documented drills into board assurance
 *   and liability defense - insurance_underwriters: secondary beneficiary
 *   (institutional/arbitrage) — prices risk off completion records -
 *   regulators_and_accreditors: agenda-setter (institutional/generational) —
 *   mandates frequency and inspects artifacts; collects legitimacy, not fees
 *   - frontline_incident_responders: primary target
 *   (moderate/identity_locked) — carries rehearsed procedure and unexercised
 *   judgment into real events - crisis_affected_publics: terminal target
 *   (powerless/trapped) — absorbs the shortfall when response quality lags
 *   documented readiness - transfer_of_training_researchers: excluded
 *   analytical voice — holds the differential-retention evidence with no seat
 *   in mandate design
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.6).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.55).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Mandatory Simulation-Based Competence Maintenance Regime (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '5bfcf128-d5aa-41dd-b791-86e761e4250d').
narrative_ontology:cs_kernel_codification('5bfcf128-d5aa-41dd-b791-86e761e4250d', formalized).
narrative_ontology:cs_authority_grounding('5bfcf128-d5aa-41dd-b791-86e761e4250d', expertise).
narrative_ontology:cs_interpretation_layer_present('5bfcf128-d5aa-41dd-b791-86e761e4250d').
narrative_ontology:cs_reading_relation('5bfcf128-d5aa-41dd-b791-86e761e4250d', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('5bfcf128-d5aa-41dd-b791-86e761e4250d', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_axiom('5bfcf128-d5aa-41dd-b791-86e761e4250d', foundational, competence_kernel_is_componentially_heterogeneous).
narrative_ontology:cs_axiom_status(competence_kernel_is_componentially_heterogeneous, holdable).
narrative_ontology:cs_axiom_grounding('5bfcf128-d5aa-41dd-b791-86e761e4250d', competence_kernel_is_componentially_heterogeneous, empirically_contingent).
narrative_ontology:cs_axiom('5bfcf128-d5aa-41dd-b791-86e761e4250d', foundational, maintenance_regimes_must_match_component_requirements).
narrative_ontology:cs_axiom_status(maintenance_regimes_must_match_component_requirements, holdable).
narrative_ontology:cs_axiom_grounding('5bfcf128-d5aa-41dd-b791-86e761e4250d', maintenance_regimes_must_match_component_requirements, instrumental).
narrative_ontology:cs_reference_frame('5bfcf128-d5aa-41dd-b791-86e761e4250d', two_component_competence_stewardship).
narrative_ontology:cs_drift_state('5bfcf128-d5aa-41dd-b791-86e761e4250d', contemporary_compliance_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5bfcf128-d5aa-41dd-b791-86e761e4250d', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_vendors_and_consultants).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_compliance_functions).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, executive_risk_owners).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, insurance_underwriters).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, regulators_and_accreditors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_incident_responders).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_affected_publics).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_transfer_doctrine).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, documented_exercise_equals_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design scenarios, build simulator platforms, and staff evaluator pools for regulated sectors. Revenue scales with mandated exercise hours and audit cycles, so growth tracks the expansion of exercise requirements rather than demonstrated transfer to real-event performance. If mandates lapsed, they could retool toward adjacent training markets.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_vendors_and_consultants, beneficiary,
    organized, biographical, mobile, global).

% Own the internal exercise calendar, scenario library, and after-action evidence trail. Schedule drills, select scenarios, manage evaluator relationships, and file the documentation regulators and insurers consume. Headcount and internal standing scale with the audit burden; pivoting to non-exercise preparation modes would strand accumulated documentation capital and reporting relationships.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_compliance_functions, agenda_setter,
    institutional, biographical, constrained, national).

% Sign preparedness attestations, present exercise results to boards and regulators, and negotiate insurance terms using documented drill compliance. The completion record functions as liability defense after incidents. They can restructure operations, outsource hazardous functions, or shift activity across jurisdictions if the liability calculus changes.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, executive_risk_owners, beneficiary,
    powerful, immediate, arbitrage, global).

% Price premiums and set coverage terms using exercise-completion records as a proxy for operational risk. The documented-compliance artifact lets them underwrite exposures they could not otherwise price. They can withdraw from lines or reprice portfolios across markets.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, insurance_underwriters, beneficiary,
    institutional, biographical, arbitrage, global).

% Mandate minimum exercise frequencies, accredit exercise programs, and inspect after-action documentation. The artifact stream gives them inspectable evidence of sector preparedness and a defensible account of their own oversight. Career civil servants operate inside statutory frameworks that already name exercises as the compliance instrument, so redirecting toward other preparation modes requires legislative movement rather than administrative choice.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, regulators_and_accreditors, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, regulators_and_accreditors, beneficiary).

% Staff the response roles the exercises rehearse. They accumulate drill fluency — scripted steps, radio calls, checklist timing — while occasions to practice open-ended decisions under genuine consequence remain rare. When real events arrive they carry both the rehearsed procedures and the unexercised judgment. Vocational identity is constituted through the responder role, so leaving the profession means abandoning a self-concept, not just changing jobs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_incident_responders, payer,
    moderate, biographical, identity_locked, regional).

% Patients, plant neighbors, passengers, residents — whoever depends on the organization's crisis performance at the moment of the event. They did not choose the operator, cannot contract around its preparation choices, and absorb the outcome when delivered response falls short of what the documented readiness implied.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_affected_publics, payer,
    powerless, immediate, trapped, local).

% Study how skills acquired in simulation transfer to performance under real consequence, including evidence that retention differs sharply across skill components. Their findings circulate in journals and conferences but have no seat in accreditation design or mandate-setting, which proceed on exercise-hour counts and completion rates.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, transfer_of_training_researchers, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_vendors_and_consultants).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Joint exercises solve a real collective-action problem: dispersed teams, shifts, and partner agencies rarely act together between real events, and exercises give them a shared repertoire — common terminology, handoffs, radio discipline, equipment sequencing — so their first coordinated action is not their first-ever joint action.
% TRANSFER_FUNCTION: Moves training budgets, staff hours, and operational downtime into exercise provision and documentation; moves assurance upward to boards, regulators, and insurers in the form of completion records; and moves the cost of the unexercised component outward and downward — onto responders who must improvise with decayed judgment and onto publics who absorb the shortfall when real events test it.
% ABSENT_VOICES: Transfer-of-training researchers holding the differential-retention evidence sit outside mandate design; veterans of the real incidents the exercises rehearse are consulted episodically after failures rather than seated in accreditation; crisis-affected publics have no representation anywhere in exercise governance.
% DISAPPEARANCE_RATIONALE: If the exercise regime vanished overnight, multi-team coordination practice would stop, the compliance and insurance architectures built on completion records would lose their evidentiary basis, vendor and consultancy revenue would evaporate, and organizations would face an unfilled choice among alternative preparation modes — the surrounding institutional economy is arranged around the regime.
% FOUNDING_PROBLEM: Real crises are rare relative to workforce turnover, so between events teams had no occasion to practice together; first real activation doubled as first rehearsal, and coordination failures — unclear command, unfamiliar handoffs, frozen communication — compounded the underlying hazard.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards and survivor advocacy groups corroborate both halves from outside the benefiting parties: historical reviews confirm the original coordination-practice deficit was real and lethal, and post-incident investigations repeatedly document crews performing rehearsed procedures fluently while failing at unrehearsed judgment. No beneficiary-independent source attests that current exercise regimes close the judgment gap.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60: the regime draws real resources (budgets, hours, downtime) and returns genuine procedural retention, but the component it does not exercise is precisely the one real events test hardest, and that cost lands outside the paying organization. Suppression is authored at 0.55 as a raw structural property — unscaled by power or scope in the engine's computation: mandates, audit gates, accreditation conditions, and insurance terms make simulation the path of least institutional resistance while leaving alternatives (stress-exposure programs, live-incident rotations) legally possible but organizationally penalized. Theater ratio 0.50: a large share of exercise activity is scripted for evaluators — scenarios chosen to be passable, injects softened, after-actions written to close findings — while a real residue builds transferable fluency. Accessibility collapse 0.40: once the two-component structure is understood, the alternatives remain visible and practicable; nothing about the regime makes stakes-bearing practice impossible, merely expensive and career-risky. Resistance 0.50: post-incident investigations repeatedly find fluent-procedure/poor-judgment signatures, fueling internal reform movements that the compliance cycle absorbs. The claimed type (tangled_rope) is authored from the reading's structural analysis — genuine coordination function plus asymmetric externalization requiring active enforcement — independently of these metric values; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the measurement the corpus exists to take. The temporal series share one grid (points 0 through 24, step 4) and are monotonic rather than cyclical: enforcement capacity built up through successive post-incident mandate ratchets and plateaued, extraction rose as credential value outgrew training value, and theatrical scripting grew with evaluator-facing accountability. Boltzmann coordination type is enforcement_mechanism: the regime's binding form is a regulatory-accreditation framework whose product is synchronized multi-team response; the type default floor applies, with no override justified.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor, compliance, executive, and insurer seats the regime is functioning infrastructure: it generates billable work, inspectable evidence, defensible attestations, and priceable risk. From the responder seat the same regime is a promise that decays exactly where real events bite — the drills rehearse the parts of the job that rarely kill anyone and omit the parts that do. From the public seat there is no regime at all, only the gap between documented readiness and delivered response. The engine computes these divergent per-seat classifications from power, exit, and role data; the divergence between the agenda-setter seats (a world of artifacts) and the payer seats (a world of consequences) is the measurement this story exists to take. Coalition potential is thin: responders are fragmented across employers and shifts, and crisis-affected publics are unorganized by definition — each ad hoc coalition forms around a specific disaster and dissolves into it, so neither payer seat converts numbers into leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the collecting seats: vendors (fee receipt, mobile exit), executives and insurers (assurance and pricing gains, arbitrage-grade exit) derive near-beneficiary d. Compliance functions administer rather than merely collect, but their budget and standing scale with the audit burden, keeping them near the subsidized end. Responders derive near-full-target d: they bear the unexercised component, and their exit is identity_locked — the identity-fusion mechanism is vocational and relational, a self-concept constituted through the responder role, so exit means abandoning identity rather than changing employers; identity lock pushes them toward the full-target end beyond what their moderate formal power alone would suggest, and if that vocational frame broke (mass attrition, professional redefinition), their effective extraction would fall toward the constrained-mobile band. Crisis-affected publics are trapped and powerless — they never chose the operator and cannot contract around its preparation choices — placing them at the terminal-target end. Regulators and accreditor bodies are the one seat a raw beneficiary label would misdescribe: they collect legitimacy artifacts rather than fees, and their generational horizon is staked on sector safety outcomes; the role declaration (agenda_setter with secondary_role beneficiary) carries this distinction without needing a directionality override. No overrides were used: the derivation chain from declared roles, power atoms, and exit options reproduces the intended directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-event practice scarcity for dispersed teams — remains live for the procedural component, so this is not a resolved mandate wearing its old clothes: abolishing the regime would forfeit real retention. Mislabeling risks run both ways. Calling the arrangement a rope erases the externalized decay cost and licenses indefinite compliance spending on the exercised component; calling it a snare erases the genuine coordination it delivers and points remediation at abolition rather than supplementation. Tangled_rope keeps both halves visible and aims correction at the asymmetry: exercise the second component by its own requirement (consequence-bearing practice) instead of expanding hours of the first. The R5 interview records the founding problem as contested — live for procedures, unresolved for judgment — with corroboration from accident investigation boards and survivor groups, sources outside the benefiting parties; the founding_problem_status x disappearance_verdict pair (contested x world_rearranges) raises no zombie-mandate flag, correctly, because the arrangement still performs part of its founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_component_decomposition,
    'Does crisis competence decompose into a simulation-exercisable procedural component and a stakes-dependent judgment component with genuinely different exercise requirements?',
    'Longitudinal designs correlating exercise dose and fidelity with real-incident performance, scored separately for procedural execution and novel-decision quality; natural experiments where organizations added stress-exposure or live-incident rotation to simulation-only regimes.',
    'If no stable decomposition appears, this reading collapses toward simulation_sufficiency (epsilon falls toward rope) or lived_catastrophe_necessity (epsilon rises toward snare); confirming the decomposition fixes the tangled_rope structure and locates remediation at the unexercised component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_component_decomposition, empirical, 'Whether the two-component premise distinguishing this reading is empirically grounded.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of kernel exercise_as_competence_maintenance — what would each sibling reading change structurally if adopted?',
    'Compare the three family stories'' victim sets and epsilon over the same referent: simulation_sufficiency shrinks the victim set toward none (harms reflect under-fidelity, not the regime); lived_catastrophe_necessity expands it to nearly everyone credentialed-ready (all simulation-retained competence is illusory); this reading holds the intermediate set — those harmed specifically by judgment-component failure.',
    'Family membership determines which remediation path is coherent: raise fidelity (sufficiency), replace simulation with live exposure (necessity), or supplement simulation with consequence-bearing practice for the judgment component only (this reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: reading membership and sibling deltas within the kernel.').

omega_variable(
    judgment_failure_attribution,
    'Are real-incident failures of improvisation attributable to decay under this regime, or would they occur under any preparation arrangement given the rarity of true catastrophes?',
    'Matched comparison of incident outcomes across organizations differing in preparation mode (simulation-only versus mixed-mode with live exposure), controlling for event severity and crew experience.',
    'If failures are counterfactually equivalent, the victim set thins and epsilon falls toward rope; if regime-attributable, the externalized-cost structure holds and the tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_failure_attribution, empirical, 'Counterfactual attribution of the harms defining this reading''s victim set.').

omega_variable(
    fidelity_threshold_for_judgment,
    'Is there a simulation fidelity or stress threshold above which judgment-under-stakes IS exercised, making observed decay an investment artifact rather than a structural limit?',
    'Dose-response studies across fidelity tiers: tabletop, full-scale immersive, embedded live players with real consequence for participants.',
    'A reachable threshold would convert this reading''s structural claim into a funding complaint and reduce the extraction attributed to the arrangement itself; an unreachable threshold confirms the two-component structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_threshold_for_judgment, empirical, 'Whether the judgment component is structurally or merely financially outside simulation''s reach.').

omega_variable(
    residual_risk_tradeoff_preference,
    'Do societies accept cheaper partial preparedness (simulation-only) over costlier full-component preparation as a deliberate values choice rather than an imposed extraction?',
    'Revealed preference in mandate-setting debates, insurance pricing structures, and public inquiry recommendations following major failures.',
    'If accepted deliberately, part of the measured extraction is priced-in social choice, softening the asymmetry without dissolving it; if rejected once made explicit, the extraction is unambiguous.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_risk_tradeoff_preference, preference, 'Whether the partial-preparation bargain reflects consented trade-off or imposed cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_decay_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.26).
narrative_ontology:measurement(hybrid_decay_tr_t4, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(hybrid_decay_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(hybrid_decay_tr_t12, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(hybrid_decay_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(hybrid_decay_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(hybrid_decay_tr_t24, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 24, 0.5).

% Extraction over time
narrative_ontology:measurement(hybrid_decay_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hybrid_decay_be_t4, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(hybrid_decay_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(hybrid_decay_be_t12, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(hybrid_decay_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(hybrid_decay_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hybrid_decay_be_t24, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 24, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_decay_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(hybrid_decay_su_t4, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(hybrid_decay_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(hybrid_decay_su_t12, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(hybrid_decay_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(hybrid_decay_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(hybrid_decay_su_t24, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'simulation maintains crisis competence' decomposes per the epsilon-invariance principle into three readings of kernel exercise_as_competence_maintenance, each authoring a distinct epsilon over the same referent (the standing mandatory simulation-based exercise regime). simulation_sufficiency_reading authors low epsilon (the regime genuinely exercises the kernel; residue is under-fidelity, remediable by investment); this hybrid_decay_reading authors intermediate epsilon 0.60 (procedural retention is real; judgment decay is externalized); lived_catastrophe_necessity_reading authors high epsilon (nothing constitutive is retained; the regime is credentialing). Upstream/downstream structure: fidelity and transfer-of-training evidence feed this reading's decomposition premise; this reading's two-component claim is in turn cited by necessity advocates against sufficiency. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
