% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation-as-Sufficient-Catastrophe-Practice Doctrine (Proxy Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Across high-hazard industries — nuclear operations, commercial aviation,
 *   emergency medicine, petrochemical processing — the standing arrangement
 *   treats recurring simulation exercises (full-mission simulators, tabletop
 *   drills, mock codes, scenario suites) as catastrophe-equivalent practice:
 *   sufficient on its own, and indefinitely, to maintain the operational
 *   competence that real catastrophes would otherwise be invoked to teach.
 *   Regulators certify readiness against documented drill hours; operators
 *   book compliance records; vendors supply the scenario economy. The
 *   arrangement presents itself as pure coordination — a solution to the
 *   problem that real catastrophes cannot be scheduled for rehearsal — and
 *   this story authors it from that seat: claimed_type rope, with metrics
 *   authored independently to describe the modest extraction and enforcement
 *   overhead the arrangement actually carries. KEY AGENTS (by structural
 *   relationship): - regulatory_agencies: Agenda-setter
 *   (institutional/constrained) — sets drill requirements, converts
 *   documented simulation into certifiable readiness - licensed_operators:
 *   Primary beneficiary (powerful/constrained) — books the compliance record,
 *   captures the liability shield, avoids costlier verification -
 *   simulator_vendors: Secondary beneficiary (organized/arbitrage) — sells
 *   the scenario economy the doctrine sustains - frontline_response_teams:
 *   Dual-positioned participant (organized/constrained) — receives safe
 *   rehearsal, pays in duty time and residual risk -
 *   incident_aftermath_populations: Excluded seat (powerless/trapped) — bears
 *   consequences only when a real event tests the doctrine - hro_researchers:
 *   Analytical observer (moderate/analytical) — measures the gap between
 *   exercised and actual performance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.18).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation-as-Sufficient-Catastrophe-Practice Doctrine (Proxy Reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75').
narrative_ontology:cs_kernel_codification('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', formalized).
narrative_ontology:cs_authority_grounding('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', expertise).
narrative_ontology:cs_interpretation_layer_present('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75').
narrative_ontology:cs_reading_relation('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', catastrophe_proxy_sufficiency__hybrid_degradation_reading, forecloses).
narrative_ontology:cs_reading_relation('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', foundational, simulation_exercises_constitute_equivalent_practice).
narrative_ontology:cs_axiom_status(simulation_exercises_constitute_equivalent_practice, holdable).
narrative_ontology:cs_axiom_grounding('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', simulation_exercises_constitute_equivalent_practice, empirically_contingent).
narrative_ontology:cs_axiom('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', secondary, drill_documentation_certifies_readiness).
narrative_ontology:cs_axiom_status(drill_documentation_certifies_readiness, holdable).
narrative_ontology:cs_axiom_grounding('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', drill_documentation_certifies_readiness, conventional).
narrative_ontology:cs_reference_frame('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', certified_simulation_sufficiency_baseline).
narrative_ontology:cs_drift_state('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', post_incident_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a6b18e5b-5430-4ce5-a7a0-2d9c0b381c75', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulator_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_response_teams).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_response_teams).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_transfer_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, compliance_certifiability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the frequency and format of required emergency exercises, audit the documentation, and accept completed drill hours as evidence that licensees stand ready for real events. After incidents, the drill record is the first exhibit entered in the agency's own defense. Abandoning this verification practice would mean rebuilding the readiness apparatus from scratch under statutory deadlines.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Run nuclear plants, airlines, hospital systems, and chemical facilities under licenses that require recurring exercises. They purchase simulator capacity, release crews for drills, and accumulate the compliance file that answers investigators and courts. Full-scale live rehearsal of worst cases, independent adversarial verification, or formal admission of uncertainty would each cost more and expose more than the current regimen; leaving the licensing system is not available to them.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_operators, beneficiary,
    powerful, biographical, constrained, national).

% Build and sell full-mission simulators, scenario libraries, and exercise-debrief software to regulated industries worldwide. Demand follows the doctrine that simulator hours count toward readiness; a regime demanding higher fidelity or live components would force re-engineering, while a regime discounting simulation outright would shrink the market. Products move freely across sectors and borders.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulator_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Reactor crews, flight decks, code teams, and incident commanders spend scheduled hours in scenarios, rehearsing rare-failure sequences in conditions where mistakes cost nothing. They pay in duty time diverted to exercises and in carrying whatever gap remains between the drilled version of an event and the real one. Professional standing ties to drill performance records; stepping outside the exercise system is not a career option.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_response_teams, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_response_teams, payer).

% Live downwind of reactors, in flight paths, downstream of plants, and in the catchments of hospitals. Readiness on their behalf is certified through documents they never see, in proceedings they are not party to. Their exposure becomes concrete only when a real event arrives and tests what the exercises built; until then they hold no seat and no exit from the geography.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, incident_aftermath_populations, excluded,
    powerless, generational, trapped, regional).

% Study how drilled performance compares with performance under real events, publish on surprise, skill fade, and the limits of rehearsal, and testify occasionally in rulemaking. Findings enter the record but carry no vote on exercise requirements; access to incident data depends on the cooperation of the organizations studied.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hro_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_operators).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives distributed teams a schedulable, repeatable, low-consequence rehearsal of rare high-consequence events, synchronizes multi-agency response on shared scenarios, and gives regulators a uniform observable for readiness across thousands of licensees.
% TRANSFER_FUNCTION: Moves operating budgets and crew duty-hours from licensees into simulator acquisition and exercise execution; moves documented assurance from operators to regulators; moves post-incident liability exposure away from operators and agencies onto the deferred contingency of real-event performance, with vendor margins drawn from the mandated training stream.
% ABSENT_VOICES: Incident-aftermath populations are absent from every standard-setting table; their interests appear only as actuarial aggregates. Dissenting safety engineers inside operator organizations are present but filtered: fidelity concerns compete against compliance calendars and are resolved before reaching rulemaking. Future crews who will inherit whatever gap the current regimen leaves have no representative.
% DISAPPEARANCE_RATIONALE: Certification regimes would lose their readiness observable overnight: operators could no longer demonstrate diligence in the accepted currency, vendors would lose the mandated-demand floor under their market, and regulators would face immediate pressure to substitute live-drill components or formal admissions of uncertainty. The rehearsal capacity itself would not vanish, but the arrangement that schedules, funds, and credits it would dissolve into ad hoc practice.
% FOUNDING_PROBLEM: Real catastrophes are too rare, dangerous, and costly to serve as routine practice; early high-hazard industries needed a repeatable way to rehearse rare failures and demonstrate diligence to insurers, courts, and publics.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident investigation boards (which repeatedly credit prior drilling with saved lives while documenting shortfalls), the academic high-reliability-organization literature, and insurer loss data all attest that the problem remains real. None of these sources attests that simulation fully solves it; the corroboration covers the problem, not the sufficiency answer.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits low (0.22 at interval end): the arrangement's costs — crew hours, simulator capital, exercise design — track its rehearsal function closely, with a thin rent layer where mandated demand supports vendor margins and where certification substitutes for costlier verification. Suppression is low (0.18): participation is compulsory only in the sense that licensure is, and dissent operates through publication and rulemaking comment rather than against barriers. Theater ratio 0.30: most exercises still train, but the documentation-and-audit share grows steadily, visible in the rising series. Accessibility collapse 0.35: hybrid regimens, fidelity upgrades, and live-drill components remain live, argued-for alternatives rather than collapsed options. Resistance 0.40: investigation boards and organizational scholarship press on the gap between drilled and actual performance without displacing the standard. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: mandatory hour floors, audit intensity, and documentation burden hardened monotonically across the interval — an enforcement ratchet laid over otherwise stable practice. All three series share one time grid (0-36 by 6) so no metric row is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and primary-beneficiary seats should compute the arrangement as diligence made legible: from those chairs, documented exercises are the whole of the story. The dual-positioned frontline seat computes rehearsal gained against residual risk carried — near symmetric. The excluded seat computes nothing until an event forces computation; its position exists only in the omega set. Among equal-standing beneficiaries, exit asymmetry (vendor arbitrage versus licensee constraint) should produce visibly different directionalities despite identical declared roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Four declared beneficiaries pull the derivation toward the subsidy end: agencies (agenda-setting plus shield), operators (shield plus avoided verification cost), vendors (mandated-demand rents with arbitrage exit placing them nearest zero), and frontline teams, whose payer secondary role holds them near symmetric. No victim group is declared: within this reading competence is maintained, so the structure has no target pole and effective extraction stays low at every seat. The contingent-target possibility — populations exposed if transfer fails — is deliberately carried in the omega variables rather than authored into the victim array, because writing victims here would import sibling readings' structure into this reading's epsilon and break the one-reading-one-constraint discipline.
 *
 * MANDATROPHY ANALYSIS:
 *   Declaring the founding problem live blocks the zombie path: the arrangement has not outlived its function, because unschedulable catastrophes still need rehearsal. The rope claim keeps the genuine rehearsal function visible so the arrangement is not misread as pure extraction; the omega set holds the deferral question open so it is not misread as costless coordination either. If cohort data ever confirms generational degradation, the correct move is decomposition into the hybrid sibling reading as its own constraint — not retroactive relabeling of this file.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is the simulation_as_proxy_catastrophe_reading of the catastrophe_proxy_sufficiency kernel — how would instantiating a sibling reading change the structural surface?',
    'Author the three sibling stories and diff their beneficiary/victim sets and epsilon values against this file.',
    'catastrophe_necessity_reading introduces a required-real-event input and an untrained-cohort victim set; hybrid_degradation_reading adds a generational victim set; simulation_fidelity_threshold replaces categorical sufficiency with a technology-contingent boundary — each changes the directionality map and likely the computed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Committer-frame index: one reading of a four-reading kernel.').

omega_variable(
    sufficiency_verifiable_only_by_event,
    'Can the sufficiency claim be verified except by the very catastrophes the arrangement exists to avoid — and if not, who bears the cost of the unverifiable interval?',
    'Longitudinal cohort comparison of simulation-trained versus event-experienced teams; natural experiments where real events strike heavily drilled facilities; cross-jurisdiction outcome data.',
    'Confirmed transfer gaps materialize a deferred victim set and push the arrangement toward enforced extraction riding a real rehearsal function; confirmed sufficiency stabilizes the low-extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_verifiable_only_by_event, empirical, 'The epistemic trap: verification requires the event the doctrine claims to make unnecessary.').

omega_variable(
    liability_shield_function_weight,
    'Is regulatory liability protection an incidental byproduct of certification or the arrangement''s operative function?',
    'Compare jurisdictions where drill certification carries different evidentiary weight in negligence proceedings; trace rulemaking histories for liability-driven amendments.',
    'If the shield is operative, extraction concentrates on the regulator/operator axis and exceeds the measured 0.22; if incidental, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_shield_function_weight, conceptual, 'Whether the liability shield is byproduct or payload.').

omega_variable(
    authority_framing_expertise_vs_extraction,
    'Is the certification authority''s legitimacy grounded in demonstrated expertise, or in the benefit it collects from preventing revision of the sufficiency kernel?',
    'Test whether the authority updates standards on disconfirming evidence at the rate its expertise claim predicts; examine who funds the fidelity research the authority relies upon.',
    'Under the alternative framing, authority_grounding shifts and the interpretive layer reads as drift-absorbing rather than truth-tracking, altering the commitment-system pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_expertise_vs_extraction, conceptual, 'Framing under-determination on the authority axis of the certification system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t6, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(cata_tr_t12, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(cata_tr_t18, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(cata_tr_t36, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 36, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t6, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 6, 0.17).
narrative_ontology:measurement(cata_be_t12, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 12, 0.19).
narrative_ontology:measurement(cata_be_t18, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 18, 0.2).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 24, 0.21).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(cata_be_t36, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 36, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t6, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 6, 0.12).
narrative_ontology:measurement(cata_su_t12, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 12, 0.14).
narrative_ontology:measurement(cata_su_t18, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 18, 0.16).
narrative_ontology:measurement(cata_su_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 24, 0.17).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(cata_su_t36, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 36, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The colloquial label 'simulation maintains catastrophe-readiness' decomposes, per the epsilon-invariance principle, into four structurally distinct claims held by different parties: categorical sufficiency (this file), real-event necessity, hybrid degradation, and fidelity-threshold conditionality. Each carries its own epsilon, beneficiary structure, and failure mode; this family links them via affects_constraints so evidence and contamination propagate across the family rather than being averaged inside one story. This reading sits upstream of the fidelity-threshold sibling: regulatory adoption of categorical sufficiency redirects funding and legitimacy away from fidelity-improvement programs without logically eliminating them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
