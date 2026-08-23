% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Distributed Near-Miss Learning Regime for Catastrophic-Failure Industries
 *   domain: safety-engineering/organizational-learning
 *
 * SUMMARY:
 *   A standing apparatus in high-hazard industries — mandatory incident and
 *   near-miss reporting into shared pools, cross-organizational dissemination
 *   of foreign incidents, just-culture immunity protections, and mandated
 *   high-fidelity recurrent rehearsal — maintains operational competence
 *   against failures too rare for any single organization to learn from
 *   directly. Aviation is the exemplar: ASRS, ASAP, ICAO dissemination,
 *   LOFT/CRM recurrent training. The reading's structural delta: industries
 *   with strong cross-organizational learning networks retain competence;
 *   those without it (surgery is the standing candidate) do not. This story
 *   is one reading of the kernel catastrophe_avoidance_retention (see
 *   kernel_context); the epsilon referent is the standing
 *   distributed-learning arrangement itself as this reading assesses it — not
 *   the simulation-only or catastrophe-only arrangements its siblings would
 *   build. The claimed type (tangled_rope) is authored from structure: a
 *   genuine pooling function that solves a real collective-action problem,
 *   carrying a real transparency burden — residual reporter exposure,
 *   regressive compliance costs, information rents — that requires continuous
 *   enforcement and immunity maintenance to keep from corroding the candor
 *   the network runs on. Metrics are authored from observed operation over
 *   the interval t=0..50 (calendar 1974–2024) and are not reconciled to the
 *   claim.
 *
 * KEY AGENTS:
 *   - aviation_safety_regulators: agenda-setter (institutional/constrained) — sets the mandates, operates the immunity machinery, collects the information stream its oversight runs on
 *   - accident_investigation_bodies: agenda-setter/beneficiary (institutional/constrained) — converts the pooled record into findings and the next round of standards
 *   - airline_operators: coordinated payer (powerful/constrained) — bears drill downtime, compliance overhead, and disclosure exposure; collects the pooled-learning dividend no internal program could replicate
 *   - frontline_crew: transparency taxpayer (organized/identity_locked) — files the near-miss reports the network runs on; bears residual criminal and career exposure the immunity regime only partly removes
 *   - commercial_passengers: diffuse beneficiary (powerless/constrained) — receives the accident-rate dividend; no lever on the apparatus
 *   - insurers_reinsurers: data-rent beneficiary and quiet enforcement arm (institutional/arbitrage) — prices candor into premiums; can reprice or withdraw
 *   - aircraft_manufacturers: design-feedback beneficiary (powerful/mobile) — converts fleet-wide incident data into service bulletins and design changes
 *   - small_regional_carriers: regressive-burden payer (moderate/constrained) — same mandates on thinner margins; paper compliance is their discount
 *   - unnetworked_hazard_industries: excluded counterfactual (institutional/constrained) — high-hazard fields without the network; their error rates are the control case
 *   - safety_science_researchers: analytical observer (analytical/generational) — tests whether the apparatus actually maintains competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.34).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.47).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.34).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Distributed Near-Miss Learning Regime for Catastrophic-Failure Industries").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety-engineering/organizational-learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '614409f7-cbac-4deb-b489-960a56dfcac0').
narrative_ontology:cs_kernel_codification('614409f7-cbac-4deb-b489-960a56dfcac0', distributed).
narrative_ontology:cs_authority_grounding('614409f7-cbac-4deb-b489-960a56dfcac0', expertise).
narrative_ontology:cs_interpretation_layer_present('614409f7-cbac-4deb-b489-960a56dfcac0').
narrative_ontology:cs_reading_relation('614409f7-cbac-4deb-b489-960a56dfcac0', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('614409f7-cbac-4deb-b489-960a56dfcac0', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_axiom('614409f7-cbac-4deb-b489-960a56dfcac0', foundational, rare_event_competence_requires_cross_org_pooling).
narrative_ontology:cs_axiom_status(rare_event_competence_requires_cross_org_pooling, holdable).
narrative_ontology:cs_axiom_grounding('614409f7-cbac-4deb-b489-960a56dfcac0', rare_event_competence_requires_cross_org_pooling, empirically_contingent).
narrative_ontology:cs_axiom('614409f7-cbac-4deb-b489-960a56dfcac0', secondary, awaiting_catastrophe_as_teacher_impermissible).
narrative_ontology:cs_axiom_status(awaiting_catastrophe_as_teacher_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('614409f7-cbac-4deb-b489-960a56dfcac0', awaiting_catastrophe_as_teacher_impermissible, deontological).
narrative_ontology:cs_reference_frame('614409f7-cbac-4deb-b489-960a56dfcac0', pooled_incident_learning_regime).
narrative_ontology:cs_drift_state('614409f7-cbac-4deb-b489-960a56dfcac0', contemporary_post_prosecution_trend, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('614409f7-cbac-4deb-b489-960a56dfcac0', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, commercial_passengers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, accident_investigation_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, insurers_reinsurers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, aircraft_manufacturers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crew).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, small_regional_carriers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crew).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_theory).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, reason_latent_failure_model).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, just_culture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the binding reporting, dissemination, and recurrent-training standards that carriers and states adopt (ICAO standards transposed into national rules; FAA/EASA regulations). Operate the de-identification and immunity machinery that makes candor legally survivable, and receive the incident stream that constitutes their oversight capacity — agency budgets, rulemaking agendas, and international standing all scale with reporting volume. Abandoning the mandate would dismantle their own information base, so their exit from the arrangement is not real.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_safety_regulators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_safety_regulators, beneficiary).

% Draw on the shared accident and incident record to publish findings and safety recommendations that shape the next round of standards and training requirements. Their caseload, relevance, and persuasive force depend on a steady flow of candid reports, and they are among the loudest institutional voices pressing back when prosecution trends chill reporting.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, accident_investigation_bodies, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, accident_investigation_bodies, agenda_setter).

% Operate the mandated recurrent drill regimes, file the required reports, absorb the training downtime and administrative overhead, and face civil discovery of their internal safety documents in litigation. In exchange they draw on a failure library no single carrier could generate internally and operate under accident rates that keep their social license stable. Holding an operating certificate is incompatible with leaving the arrangement; re-registering across jurisdictions is the only partial exit and it does not escape the international standards.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_operators, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_operators, beneficiary).

% Pilots, controllers, and cabin crew file the near-miss reports the whole network runs on. Confidentiality and immunity protections reduce but do not eliminate exposure: safety reports can surface in criminal investigations after accidents, and prosecutions of crews following crashes have become more common in some jurisdictions over the past decade. Their working identity is fused with the safety role and their type-specific training is sunk cost; leaving the profession means forfeiting the career. Union representation gives them the strongest collective voice among the parties bearing reporting exposure, and they spend it defending just-culture protections.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crew, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crew, beneficiary).

% Fly under a safety regime they cannot inspect and rarely think about. They receive the accident-rate dividend and supply the fare revenue and political tolerance that make the apparatus fundable. Their practical lever — choosing carriers or routes on safety grounds — is nearly inert at baseline accident rates this low.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, commercial_passengers, beneficiary,
    powerless, immediate, constrained, global).

% Price hull, liability, and reinsurance coverage against the pooled loss and incident record, and make participation in reporting and training regimes an effective condition of affordable coverage. Because they can reprice portfolios or withdraw from aviation lines altogether, they act as a quiet second enforcement layer behind the regulator, with none of its public accountability.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, insurers_reinsurers, beneficiary,
    institutional, generational, arbitrage, global).

% Receive de-identified fleet-wide incident and failure data that feeds design changes, service bulletins, and fleet-level airworthiness directives. The same documentation stream bears on their liability exposure, giving them a dual interest in both the flow and the framing of the record. They can shift product lines and regulatory strategy across jurisdictions.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, aircraft_manufacturers, beneficiary,
    powerful, generational, mobile, global).

% Carry the same training, reporting, and audit mandates as major carriers on much thinner margins, so simulator hours, safety-office staffing, and reporting overhead weigh proportionally heavier. Some respond with paper compliance — the documents are in order and the practice is thin. They genuinely benefit from the pooled record but lack the internal safety staff to metabolize it fully.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, small_regional_carriers, payer,
    moderate, biographical, constrained, regional).

% High-hazard fields such as surgery and parts of chemical processing operate without an equivalent cross-organizational near-miss network: error data stays inside institutions, insurers, or courtrooms. They hold no seat in the standard-setting conversation this apparatus runs on, and their persistently higher error rates are the standing counterexample its advocates argue against rather than with.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, unnetworked_hazard_industries, excluded,
    institutional, generational, constrained, global).

% Study whether pooled incident learning and high-fidelity rehearsal actually preserve operational competence — reporting climates, just culture, drill transfer, the medicine contrast. They hold no operational stake, publish findings that cut in both directions, and are the nearest thing the arrangement has to an external auditor.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools statistically rare failure evidence across competing organizations so that learning accumulates faster than any single organization's own accident history could support, and standardizes recurrent high-fidelity rehearsal so crews practice failure modes they will rarely meet in service.
% TRANSFER_FUNCTION: Moves failure and near-miss information upward from frontline crew and operators into shared institutional pools (regulators, investigators, insurers, manufacturers), and moves the costs of candor — legal exposure, competitive disclosure, drill downtime, administrative overhead — onto the reporting parties, while the resulting accident-rate dividend spreads across the entire system including parties that report nothing.
% ABSENT_VOICES: Crew in jurisdictions without functioning immunity protections, whose near-miss reports carry prosecution risk and who are therefore silent in the pool; small carriers experiencing the mandate as a regressive cost; patients and practitioners in high-hazard fields that never built the network, whose error data never enters any pool; and crews prosecuted after accidents, whose cases are the apparatus's strongest counter-evidence and who are structurally absent from the standard-setting conversation.
% DISAPPEARANCE_RATIONALE: If the reporting pools, immunity frameworks, and mandated rehearsal vanished overnight, nothing visible breaks for years — which is the trap. Rare failure modes would re-learn themselves one organization at a time at full price; accident rates in complex operations would drift upward over a decade or more as latent failure patterns re-emerge unshared; the training industry would reorganize around unverifiable fidelity claims; insurers would reprice or withdraw from lines they can no longer model. The rearrangement is slow, latent, and large.
% FOUNDING_PROBLEM: In the early jet age, regulators and operators confronted a problem no single organization could solve alone: the failure modes of complex systems are too rare for any one carrier or crew to learn from directly, and waiting for each organization to buy its own catastrophes as tuition was unacceptable after a string of midair collisions and controlled-flight-into-terrain losses. The founding problem was how to make one organization's near-miss teachable to all of them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the operator set: accident-investigation findings (NTSB, BEA, AAIB) repeatedly trace losses to failure patterns already documented in other operators' reports; ICAO safety reports and insurance loss curves document the divergence between networked and non-networked operations; the academic safety-science literature (Reason's latent-failure work, Weick and Sutcliffe's high-reliability studies) independently attests both the rarity problem and the pooling remedy; and the medicine contrast — persistently high error rates in a high-hazard field without an equivalent network — serves as the external control case. No party inside the benefiting operator set is needed to attest the founding problem or its live status.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.34: the transparency burden is real (the post-2004 prosecution trend, civil discovery of safety documents, regressive compliance costs on small carriers) but was deliberately engineered down by the immunity and de-identification architecture — hence the U-shaped series, falling from the punitive 1974 reporting climate (0.38) to the just-culture high-water mark around 2004 (0.28), then creeping back up as criminalization and discovery exposure grow (0.34 at t=50). Suppression 0.47: participation is mandated (ICAO standards binding on states, national regulations binding on carriers, insurer conditions binding on both), and the alternatives — own-experience-only learning, simulation-only regimes — are closed off inside certified operations; suppression is authored as a raw structural property of the arrangement and is not scaled by power or scope. Theater_ratio 0.31 and rising: the core function (real pooled learning, real rehearsal) remains dominant, but checkbox drills and defensive paperwork grow with the compliance bureaucracy; the series stays below the Goodhart substitution threshold across the interval. Accessibility_collapse 0.45: alternatives persist (under-investment is common outside aviation, and non-networked fields operate without the apparatus), but within certified aviation the exits are largely collapsed by regulator and insurer pressure. Resistance 0.40: reporting chill, competitive-disclosure reluctance, drill fatigue, and paper compliance — persistent and managed rather than broken. All three tracked metrics are authored on one shared six-point grid (t=0,10,20,30,40,50) so no metric's end-state is silently backfilled into earlier periods.
 *
 * PERSPECTIVAL GAP:
 *   The same structure reads differently by seat: from the regulator and investigator seats the apparatus is their eyes — a coordination achievement they administer and staff; from the crew seat it is a candor bargain whose protections have been thinning since the mid-2000s; from the operator seat it is simultaneously a cost center and a subsidy no internal program could replace. The engine computes these per-seat classifications from the structural data; the divergence between the administrator seats (rope-flavored experience) and the reporter seat (residual extraction at the margin) is exactly the measurement this story exists to take. The authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (passengers, regulators, investigators, insurers, manufacturers) derive low directionality — the arrangement subsidizes them. The victim declarations (operators, crew, small carriers) derive high directionality — the costs route through them. Two exit modulations matter: the crew's identity_locked exit places them near the full-target end of the range at the margin (they cannot leave the profession without forfeiting the career their training built, so the reporting exposure they carry cannot be escaped by walking away), while the insurers' arbitrage exit places them at the beneficiary end despite their enforcement role. Operators should compute near symmetric: declared payers, but their situation describes a net dividend, and the engine's per-seat computation from payer role plus powerful power plus constrained exit landing mid-range is the honest reading of that dual position. No directionality overrides are authored: the derivation from beneficiary/victim data plus exit options captures the structure, and the interesting divergence is between seats, not a correction to any single atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rare-event learning — is live and permanent; nothing here has outlived its function, so there is no mandatrophy to resolve, and the R5 fields record status=live with world_rearranges (no mismatch flag). The classification's work is preventive in both directions: reading the apparatus as a snare would miss that its participants are net beneficiaries and that the accident-rate dividend is the largest transfer in the system; reading it as a pure rope would miss the transparency burden and regressive costs that must be continuously managed down (immunity statutes, de-identification, discovery protections) or the network starves for candor. The tangled_rope classification tracks exactly that maintenance requirement — and the post-2004 upward creep in the extraction series is the early signature of what happens when the maintenance lapses. Receipt analysis: the gains of the arrangement are genuinely split — information rents accrue to regulators, investigators, insurers, and manufacturers, and the safety dividend diffuses to passengers — so gain_flow is authored as the affirmative claim 'diffuse' after checking every named seat; fixing_cost is 'cheap' because the agenda-setters could harden reporter protections by ordinary legislation at cost well below the benefit of restored candor, and the burden's persistence despite a cheap available fix is the signature of neglect rather than capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aviation_medicine_contrast_causality,
    'Is the safety divergence between networked aviation and non-networked high-hazard fields (surgery is the standing candidate) caused by the incident-sharing apparatus itself, or by confounds — regulatory maturity, equipment standardization, market incentives, traffic volume?',
    'Interrupted time-series and natural experiments where reporting networks are introduced into previously non-networked fields (anesthesia closed-claims systems, WHO surgical safety programs, chemical-process sharing initiatives), compared against matched non-adopting fields.',
    'If confounds explain the divergence, this reading''s core premise weakens toward its siblings and epsilon should be re-authored as coordination overhead without demonstrated competence return; if causal, the medicine contrast stands as the reading''s strongest external evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aviation_medicine_contrast_causality, empirical, 'Whether cross-industry safety divergence is attributable to the learning network itself.').

omega_variable(
    transparency_tax_ratchet_direction,
    'Is the post-2004 rise in reporter exposure (criminal prosecution of crews after accidents, civil discovery of safety documents) a transient litigation-cycle artifact or a structural ratchet that keeps pushing the candor bargain toward collapse?',
    'Longitudinal reporting-volume series per jurisdiction against prosecution and discovery events; confidential reporting-program submission trends before and after high-profile prosecutions.',
    'A continuing ratchet dates a drift from managed hybrid operation toward snare-like operation (candor taken while protections decay, network starving for input); a reversal would confirm the engineered-down equilibrium is stable and current epsilon sits near its floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_tax_ratchet_direction, empirical, 'Direction and persistence of the reporting-chill trend.').

omega_variable(
    kernel_sufficiency_disagreement_location,
    'This constraint is one reading of kernel catastrophe_avoidance_retention; the kernel contest is located on the sufficiency question — whether high-fidelity simulation, actual catastrophe, or the distributed near-miss mix is necessary and sufficient for competence retention. Which structural claim does the evidence support?',
    'Comparative competence studies across industries and periods operating under different regime mixes; drill-transfer studies testing whether simulated practice survives contact with novel real failures; pooled-record studies showing near-misses teaching at scale without catastrophe.',
    'Adopting simulation_as_proxy_catastrophe would concentrate standing in the training industry and re-author beneficiaries around drill vendors; adopting catastrophe_as_necessary_selector would license under-investment in learning infrastructure and shift its costs onto future victims; either adoption dissolves this story''s beneficiary/victim structure and re-authors epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sufficiency_disagreement_location, conceptual, 'Committer-frame omega: the kernel''s sufficiency contest and what each sibling resolution would restructure.').

omega_variable(
    candor_adverse_selection,
    'Does the residual transparency burden fall disproportionately on the most candid reporters and operators, producing adverse selection in which the shared pool over-represents the cautious and under-represents the risky?',
    'Compare reporting rates, report quality, and subsequent event rates across operators with different reporting climates at matched safety performance; within-operator reporting deltas before and after exposure events such as prosecutions.',
    'Strong adverse selection means the network quietly takes candor from the honest while free-riders collect the dividend — pushing the marginal seat from tangled_rope toward snare and arguing for hardened immunity design; weak selection supports the current engineered equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(candor_adverse_selection, empirical, 'Whether the transparency burden selects against candor within the network.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(cata_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 10, 0.33).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 20, 0.3).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 50, 0.34).
narrative_ontology:measurement_basis(cata_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 50, 0.47).
narrative_ontology:measurement_basis(cata_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel catastrophe_avoidance_retention decomposes into three readings with distinct epsilon and distinct beneficiary/victim structures — simulation_as_proxy_catastrophe (drills as genuine practice; extraction concentrated in the training industry's fidelity claims), catastrophe_as_necessary_selector (real catastrophes as the only teacher; minimal institutional apparatus, maximal human cost, licenses prevention under-investment), and this story, hybrid_near_miss_learning (distributed pooling apparatus; moderate epsilon carried by a transparency burden that must be continuously engineered down). The dependency structure runs both ways: this reading's incident record is what makes the other two testable — simulation fidelity claims are checked against reported real events, and the catastrophe-only claim is falsified by the pooled record showing near-misses teaching at scale without catastrophe. Each family file links the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
