% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Continuous Multi-Mechanism Competence Occupation Requirement (Hybrid Reading)
 *   domain: institutional/safety-regulation/professional-competence
 *
 * SUMMARY:
 *   Across high-reliability industries — aviation, nuclear operations,
 *   surgery, emergency response — a standing requirement holds that licensed
 *   practitioners maintain competence through continuous engagement with
 *   multiple distinct exercise mechanisms: full-fidelity simulation, periodic
 *   refresher instruction, procedural reinforcement drilling, and line audits
 *   of real-work performance. Regulators enforce it through proficiency
 *   checking and certificate action; operators fund it; a specialized
 *   training industry delivers it; insurers condition coverage on it. This
 *   story instantiates the hybrid_occupation reading of the
 *   competence_occupation kernel: no single mechanism suffices, each
 *   exercises and observes a different slice of the competence surface, and
 *   the optimal configuration among mechanisms is unsettled — making training
 *   optimization a standing research problem. Per the epsilon-invariance
 *   principle this file authors ONE reading; the sibling readings are
 *   separate constraints linked through network.affects_constraints.
 *   Epsilon's referent is the standing multi-mechanism arrangement as it
 *   actually operates, assessed by this reading's lights — never a
 *   hypothetical leaner regime. The claimed_type (tangled_rope) is authored
 *   independently of the metrics: the regime coordinates genuine assurance
 *   AND carries asymmetric extraction; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is the measurement the corpus exists to take. KEY
 *   AGENTS (by structural relationship): - licensed_practitioners: Primary
 *   target (organized/identity_locked) — bears the regime's time, stress, and
 *   career-risk costs - training_industry_vendors: Primary beneficiary
 *   (organized/arbitrage) — collects the largest direct monetary flow under
 *   mandate-guaranteed demand - safety_regulators: Agenda setter
 *   (institutional/identity_locked) — writes, enforces, and ratchets the
 *   requirement; scope scales with it - hro_operators: Dual-positioned
 *   payer-beneficiary (powerful/constrained) — funds the apparatus, receives
 *   liability shielding and insurability - insurers_underwriters: Beneficiary
 *   (institutional/arbitrage) — prices the regime's records, reinforces
 *   enforcement through coverage conditions - service_users_public: Diffuse
 *   beneficiary-payer (powerless/constrained) — receives safety assurance,
 *   pays embedded costs, holds no configuration seat -
 *   training_research_community: Analytical observer (analytical/analytical)
 *   — supplies the decay curves and transfer studies all seats cite -
 *   evidence_based_training_advocates: Excluded voice (moderate/mobile) —
 *   argues for leaner outcome-based configurations; published but never
 *   agenda-setting
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.46).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.57).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.46).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Continuous Multi-Mechanism Competence Occupation Requirement (Hybrid Reading)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "institutional/safety-regulation/professional-competence").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '70ff1419-4763-4d7b-88fc-a69bc5986a7d').
narrative_ontology:cs_kernel_codification('70ff1419-4763-4d7b-88fc-a69bc5986a7d', distributed).
narrative_ontology:cs_authority_grounding('70ff1419-4763-4d7b-88fc-a69bc5986a7d', expertise).
narrative_ontology:cs_interpretation_layer_present('70ff1419-4763-4d7b-88fc-a69bc5986a7d').
narrative_ontology:cs_reading_relation('70ff1419-4763-4d7b-88fc-a69bc5986a7d', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('70ff1419-4763-4d7b-88fc-a69bc5986a7d', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_axiom('70ff1419-4763-4d7b-88fc-a69bc5986a7d', foundational, plurality_of_mechanisms_necessary).
narrative_ontology:cs_axiom_status(plurality_of_mechanisms_necessary, holdable).
narrative_ontology:cs_axiom_grounding('70ff1419-4763-4d7b-88fc-a69bc5986a7d', plurality_of_mechanisms_necessary, empirically_contingent).
narrative_ontology:cs_axiom('70ff1419-4763-4d7b-88fc-a69bc5986a7d', foundational, synthetic_exercise_genuinely_occupies_kernel).
narrative_ontology:cs_axiom_status(synthetic_exercise_genuinely_occupies_kernel, holdable).
narrative_ontology:cs_axiom_grounding('70ff1419-4763-4d7b-88fc-a69bc5986a7d', synthetic_exercise_genuinely_occupies_kernel, empirically_contingent).
narrative_ontology:cs_reference_frame('70ff1419-4763-4d7b-88fc-a69bc5986a7d', plural_mechanism_occupation_baseline).
narrative_ontology:cs_drift_state('70ff1419-4763-4d7b-88fc-a69bc5986a7d', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('70ff1419-4763-4d7b-88fc-a69bc5986a7d', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_industry_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, insurers_underwriters).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, service_users_public).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, licensed_practitioners).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, hro_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, hro_operators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_research_community).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, service_users_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufactures and operates full-fidelity simulators, delivers recurrent courseware, and staffs instructor and examiner services. Demand is set by regulatory mandate rather than discretionary purchase, in a concentrated supplier market with pricing power. Each added mechanism — a new device class, a new audit program, a new refresher cycle — expands the addressable market; device utilization is contracted years ahead.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_industry_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Writes and enforces the recurrent-training requirement, approves curricula and training devices, and administers proficiency checking through designated examiners. Budget and staffing scale with program scope, and post-incident ratchets expand the mandate. The agency's public legitimacy rests on visibly maintaining the regime; career civil servants spend entire working lives inside the certification apparatus.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, safety_regulators, beneficiary).

% Attends scheduled simulator sessions, refresher ground school, and procedural drills, and is subject to line audits and periodic proficiency checks. Bears duty time, study load, evaluation stress, and in some professions direct fees. Failure at a check suspends currency and livelihood pending remediation. Unions negotiate scheduling and pay protections but not the existence of the requirement; leaving the profession forfeits accumulated career capital and professional identity.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, licensed_practitioners, payer,
    organized, biographical, identity_locked, global).

% Funds and schedules the entire training apparatus, absorbs fleet and theatre downtime for training cycles, and passes much of the cost into prices. In exchange receives documented-compliance liability shielding, insurability, and public legitimacy. Cannot exit the regime while operating under license; jurisdiction shopping at the regulatory margins is possible but does not remove the obligation.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, hro_operators, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, hro_operators, beneficiary).

% Conditions coverage on documented compliance with the training regime and feeds audit and check records into actuarial models. The regime gives them a legible risk object to price; they reinforce enforcement by writing training standards into coverage conditions without operating any of it, and can reprice or withdraw from lines if the regime loosens.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, insurers_underwriters, beneficiary,
    institutional, biographical, arbitrage, global).

% Receives the safety assurance the regime promises — flights, surgeries, and plant operations conducted by currently qualified crews. Pays embedded training costs in fares, bills, and premiums. Holds no seat in configuration decisions and has no practical alternative service mode that escapes the regime's cost structure.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, service_users_public, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, service_users_public, payer).

% Produces the skill-decay curves, transfer-of-training studies, and simulator-fidelity evaluations that every seat cites in configuration debates. Research funding and publication careers depend on the optimization question remaining open; the community supplies evidentiary raw material without administering or purchasing anything.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_research_community, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, training_research_community, beneficiary).

% Researchers, former check airmen, and operator training managers who argue for leaner, competency-outcome-based configurations in place of mandated mechanism menus. They publish, testify, and staff advisory committees, but hold no agenda-setting vote; their proposals enter the process and exit diluted.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, evidence_based_training_advocates, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_industry_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the assurance problem: how a profession guarantees to regulators, insurers, and the public that rare-event-critical skills remain occupied despite long quiescent periods between real demands. The multi-mechanism design responds to heterogeneous decay rates and observability limits — simulation exercises rare and dangerous procedures safely, refreshers rebuild declarative knowledge, procedural drilling maintains motor sequences, and line audits observe behavior that only appears in real work.
% TRANSFER_FUNCTION: Moves time, money, and attention: from practitioners (duty time, off-duty study, evaluation stress) and operators (training budgets, equipment downtime) to training vendors (device hours, courseware, instruction), regulators (program fees funding oversight scope), and research institutions (study populations and data access); moves assurance, currency, and employability back to practitioners.
% ABSENT_VOICES: Evidence-based minimalists and competency-assessment advocates hold no agenda-setting seat — they advise and publish but do not decide. Practitioners who exited under check failure are absent from configuration debates entirely. Service users are present only through proxy representation. The unanimity behind mechanism multiplication partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism requirement vanished overnight, insurers would reprice or withdraw coverage pending new risk evidence, regulators would rebuild oversight instruments after the first decay-attributed incident, vendors would lose mandate-guaranteed demand, practitioner currency would lapse into private arrangements, and the public assurance signal would collapse — the HRO labor market and its insurance and regulatory surround would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Post-war jet-age accident clusters and early nuclear incidents showed that critical skills decay during long quiescent periods, that one-time licensing did not maintain competence, and that recurring failures traced to rustiness and unrehearsed rare procedures. The arrangement was built to solve continuous assurance of rare-event readiness.
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation bodies outside the beneficiary set continue to attribute residual incidents partly to skill lapse and unrehearsed conditions, and the peer-reviewed human-factors literature independently documents decay curves and transfer gaps; neither collects training rents. Corroboration for the problem's liveness is therefore external and reasonably strong; corroboration for any particular configuration is not, which is precisely the open question this reading carries.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).
:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: the majority of regime spend purchases real service delivery (device operation, instruction, audit labor), but extraction enters through concentrated-market pricing on mandate-guaranteed demand, uncompensated practitioner time, and chronic over-provision sustained by the unresolved configuration question — when the optimal dose is unknown, the dose drifts upward and stays there. Suppression 0.57: enforcement is sanction-backed (failed checks suspend currency and livelihood; certificates are revocable) and opting out while practicing is not permitted, but broad practitioner consent, union negotiation, and professional buy-in temper the coercive surface. Theater_ratio 0.38: a substantial minority of activity is teaching-to-the-profile and credit-hour ritualism, against a functional core of genuine skill exercise; the ratio rises over the interval as standardized high-stakes evaluation matures. Accessibility_collapse 0.45: alternatives do not fully collapse — data-driven qualification programs, competency-based models, and the sim-sufficiency proposal remain live and partially implemented, which is itself evidence against mountain status. Resistance 0.5: union campaigns over unpaid training time, professional revolt against certification-maintenance mandates, and operator cost lobbying are real and recurring. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. The three measurement series run on one shared time grid ({0,10,20,30,40,50}) so every metric is authored at every examined point; the historical dynamic is a post-incident ratchet (step increases after visible accidents, partial relaxation in budget cycles) that decade-scale sampling renders as a smooth rise — the steps, not the slope, are the real enforcement history.
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute the same structure differently. From the practitioner seat (identity_locked payer) the regime is enforced burden with existential stakes — a failed check is a career event, and the requirement's necessity is experienced as imposed. From the vendor seat (arbitrage beneficiary) it is a legitimate, durable market serving safety. From the regulator seat (identity_locked agenda_setter) it is the mission itself — the agency has become its oversight function, and questioning the configuration questions the institution. From the insurer seat (arbitrage beneficiary) it is a legible risk object that makes underwriting possible. The engine derives these divergent classifications from power, exit, and directional position; the divergence, not any single seat's verdict, is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: vendors, regulators, insurers, and the public derive low directionality (subsidized or near-symmetric); practitioners derive high directionality (full-target side, amplified by identity_locked exit). Two overrides correct derivations the structural arrays cannot express. First, powerful -> 0.45: hro_operators are listed among victims because training budgets are a direct cost they bear, but the derivation from victim-listing alone would push them toward the full-target end; their liability shielding, insurability, and cost pass-through make them genuinely dual-positioned, sitting well short of full target. Second, powerless -> 0.30: service_users_public are listed as beneficiaries, but they also pay embedded costs in every fare, bill, and premium; the override places them slightly beneficiary-side of symmetric rather than at the subsidized pole. Practitioners (organized, identity_locked, victim-listed) need no override — the derivation already places them near the full-target end, which matches their structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — continuous assurance of rare-event readiness — is live and externally corroborated, so this is not a resolved mandate wearing an old uniform; the R5 mismatch consumer should find status=live crossed with verdict=world_rearranges and raise no zombie flag. The classification still earns its keep at the component level: the regime's theater concentrates in its evaluation mechanisms (teaching-to-the-checkride, credit-hour accumulation), and if line audits decay into scored ritual — the checkride_validity_decay omega — the audit component becomes piton-flavored while the ensemble persists on the strength of its other legs. Authoring tangled_rope rather than rope prevents the opposite mislabel: reading the vendor receipts and practitioner burden as pure extraction would erase the genuine assurance function that accident investigations keep vindicating; authoring rope would erase the mandate-guaranteed demand structure that keeps the dose above any defensible optimum. The perpetual-optimization structure is the hinge: unresolved configuration questions sustain over-provision, and over-provision is where the extraction lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the hybrid_occupation reading of the competence_occupation kernel; would instantiating simulation_sufficiency or real_incident_necessity instead change the beneficiary/victim structure and epsilon?',
    'Author the sibling stories as separate constraints and compare computed classifications. The disagreement is located in whether synthetic and plural exercise genuinely occupies the competence kernel or merely rehearses its measurable proxies.',
    'Under simulation_sufficiency the refresher and audit mechanisms become removable overhead (lower epsilon, rope-leaning); under real_incident_necessity the entire synthetic apparatus becomes proxy performance (higher theater_ratio, piton/snare-leaning). This story''s epsilon is indexed to the hybrid frame over the fixed referent of the standing arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of the competence_occupation kernel; sibling readings would restructure beneficiaries and epsilon over the same referent.').

omega_variable(
    marginal_mechanism_dose_response,
    'Does each additional mandated mechanism add independent competence coverage, or do the mechanisms overlap enough that the marginal mandate is redundant over-provision?',
    'Dose-response meta-analysis across mechanism combinations: fit skill-decay curves against varying doses of simulator hours, refresher cycles, procedural drilling, and audit frequency, testing whether each mechanism shifts the decay intercept independently.',
    'Redundancy at the margin converts the excess from coordination cost to extractive overhead, raising effective extraction and pushing the regime toward the snare-leaning edge of tangled_rope; demonstrated additivity supports treating the surplus as the price of the assurance function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_mechanism_dose_response, empirical, 'Whether the multi-mechanism menu is additive in coverage or partially redundant over-provision.').

omega_variable(
    checkride_validity_decay,
    'Has high-stakes standardized evaluation (proficiency checks, scored line audits) decoupled from the competence it certifies — do audit and check findings actually predict subsequent event involvement?',
    'Correlate line-audit and proficiency-check outcomes with subsequent operator-level event rates; compare predictive validity before and after data-driven qualification programs were introduced.',
    'If decoupled, the audit mechanism''s theater_ratio is understated and the enforcement seat''s coordination claim weakens toward ritual; if predictive, line audits are the regime''s highest-value mechanism and the measured theater is concentrated elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(checkride_validity_decay, empirical, 'Goodhart risk on the evaluation mechanisms: whether checking still measures the competence it gates.').

omega_variable(
    authority_grounding_underdetermination,
    'Is the regime''s authority grounded in expertise (technical bodies interpreting decay evidence and revising curricula accordingly) or in extraction (a regulator-vendor complex whose budgets and markets depend on kernel stability)?',
    'Trace revision history: when decay or transfer evidence contradicted a mandated configuration, did mandates revise toward the evidence (expertise behavior) or persist with interpretive absorption inside approved-program reviews (extraction behavior)?',
    'Expertise grounding supports the coordination half of the tangled_rope reading; extraction grounding recasts the agenda_setter seat as captured, strengthens the snare reading, and would reclassify the authority structure''s drift as denial-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_underdetermination, conceptual, 'CS-framing under-determination: the same institutional surface supports an expertise reading and an extraction reading with different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.26).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.31).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__hybrid_occupation, theater_ratio, 30, 0.35).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__hybrid_occupation, theater_ratio, 40, 0.37).
narrative_ontology:measurement(comp_tr_t50, competence_occupation__hybrid_occupation, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(comp_be_t30, competence_occupation__hybrid_occupation, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(comp_be_t40, competence_occupation__hybrid_occupation, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(comp_be_t50, competence_occupation__hybrid_occupation, base_extractiveness, 50, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(comp_su_t30, competence_occupation__hybrid_occupation, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(comp_su_t40, competence_occupation__hybrid_occupation, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(comp_su_t50, competence_occupation__hybrid_occupation, suppression_requirement, 50, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'competence maintenance training' (epsilon-invariance principle). The label conflates three structurally distinct claims: (1) hybrid_occupation — this file — plural mechanisms jointly necessary, configuration optimally indeterminate; (2) simulation_sufficiency — simulator drills alone prevent decay, making refresher/audit mechanisms removable overhead; (3) real_incident_necessity — only actual catastrophic incidents provide authentic occupation conditions, making the synthetic apparatus proxy performance. Each carries its own epsilon, beneficiary structure, and classification; this upstream reading (highest institutional entrenchment) influences both siblings because regulators cite hybrid necessity when defending budgets against sim-sufficiency simplification and against incident-necessity skepticism. Sibling files must link back via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, powerful, 0.45).
constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, powerless, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
