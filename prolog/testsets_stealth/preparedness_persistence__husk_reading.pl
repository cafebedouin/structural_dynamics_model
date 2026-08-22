% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Memorial Drill-and-Inspection Regime (Husk Reading)
 *   domain: civic/institutional/disaster-preparedness
 *
 * SUMMARY:
 *   This file instantiates the husk_reading of the preparedness_persistence
 *   kernel: a regional flood-preparedness regime — annual evacuation drills,
 *   levee and shelter inspections, readiness certification — whose forms have
 *   run uninterrupted for four decades while the operational substance they
 *   were chartered to maintain quietly emptied out. Boats without trained
 *   crews, caches with expired equipment, evacuation routes calibrated to a
 *   channel that has since moved: the artifacts of readiness accumulate on
 *   schedule, and the capability they certify does not. The regime is not
 *   defended by anyone against reform; it is simply never worth any
 *   officeholder's while to replace, because the cost of rebuilding real
 *   capability lands on present tenures while the cost of its absence lands
 *   on future flood victims. KEY AGENTS (by structural relationship): -
 *   emergency_management_agency: agenda_setter
 *   (institutional/identity_locked) — administers the drill calendar and
 *   inspection cycle; collects legitimation it consumes internally; could
 *   redirect the appropriation but inherits the ritual as the institution's
 *   self-definition - population_at_flood_risk: primary target
 *   (powerless/trapped) — receives the regime's reassurance outputs and bears
 *   realized flood losses - municipal_taxpayers: payer (moderate/constrained)
 *   — fund the ceremonial apparatus through general revenue -
 *   volunteer_rescue_networks: excluded (organized/mobile) — witness the gap
 *   in deployment, hold no seat in the record-producing process -
 *   flood_insurance_carriers: observer with arbitrage exit (institutional) —
 *   register the gap actuarially and exit via repricing rather than protest -
 *   disaster_research_community: analytical observer — documents
 *   form/substance divergence; no lever on operations
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.6).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.38).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Memorial Drill-and-Inspection Regime (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "civic/institutional/disaster-preparedness").

domain_priors:requires_active_enforcement(preparedness_persistence__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, '9cea0dbc-d26f-468f-a6fe-7c779c77c935').
narrative_ontology:cs_kernel_codification('9cea0dbc-d26f-468f-a6fe-7c779c77c935', fixed_text).
narrative_ontology:cs_authority_grounding('9cea0dbc-d26f-468f-a6fe-7c779c77c935', lineage).
narrative_ontology:cs_interpretation_layer_present('9cea0dbc-d26f-468f-a6fe-7c779c77c935').
narrative_ontology:cs_reading_relation('9cea0dbc-d26f-468f-a6fe-7c779c77c935', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('9cea0dbc-d26f-468f-a6fe-7c779c77c935', preparedness_persistence__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('9cea0dbc-d26f-468f-a6fe-7c779c77c935', foundational, ritual_form_displaces_operational_substance).
narrative_ontology:cs_axiom_status(ritual_form_displaces_operational_substance, holdable).
narrative_ontology:cs_axiom_grounding('9cea0dbc-d26f-468f-a6fe-7c779c77c935', ritual_form_displaces_operational_substance, empirically_contingent).
narrative_ontology:cs_axiom('9cea0dbc-d26f-468f-a6fe-7c779c77c935', foundational, certified_assurance_outruns_verified_capability).
narrative_ontology:cs_axiom_status(certified_assurance_outruns_verified_capability, holdable).
narrative_ontology:cs_axiom_grounding('9cea0dbc-d26f-468f-a6fe-7c779c77c935', certified_assurance_outruns_verified_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('9cea0dbc-d26f-468f-a6fe-7c779c77c935', substantive_readiness_regime).
narrative_ontology:cs_drift_state('9cea0dbc-d26f-468f-a6fe-7c779c77c935', contemporary_audit_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9cea0dbc-d26f-468f-a6fe-7c779c77c935', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, population_at_flood_risk).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, municipal_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_agency).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, performed_readiness_confers_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Charters and schedules the annual drill cycle, convenes the inspections, and issues the readiness certificates that travel to the legislature and the press. Successive budget cycles traded boat-hours, crew certifications, and shelter stock for ceremony that scales more cheaply. It could re-task its appropriation toward live capability, but each leadership cohort inherits the ritual calendar as the definition of the job, and ending it would require the institution to repudiate forty years of its own public record.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_agency, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, emergency_management_agency, beneficiary).

% Households on the floodplain receive the regime's outputs as reassurance — route cards, brochures, televised drills, certification banners — and have no means to verify the gap between the certified picture and the cache contents. When water arrives they bear the evacuation failures, delayed rescues, and property losses that working capability would have reduced. Leaving means abandoning homes, jobs, and kin networks.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, population_at_flood_risk, payer,
    powerless, generational, trapped, local).

% Fund the drill-and-inspection line item through general revenue and receive ceremony and paperwork where the appropriation's stated purpose was capability. Objection channels exist — council meetings, budget comments — but are slow and individually ineffective, and relocation to another jurisdiction is costly.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, municipal_taxpayers, payer,
    moderate, biographical, constrained, regional).

% High-water clubs and mutual-aid crews that launch when floods come. Their after-action notes document the gap directly — gauge assumptions that no longer match the channel, locked equipment caches, coordinators unreachable on the drilled frequencies — but they hold no seat in the planning cycle that produces the official readiness record.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, volunteer_rescue_networks, excluded,
    organized, biographical, mobile, regional).

% Price the regional portfolio actuarially; their loss curves register the difference between certified readiness and realized response more precisely than any hearing. They do not participate in preparedness planning. They express the finding through premiums, deductibles, and withdrawal from the worst-exposed zip codes — exiting the risk rather than reforming the regime.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_insurance_carriers, observer,
    institutional, biographical, arbitrage, continental).

% Studies the regime as an instance of symbolic preparedness: documents form/substance divergence across comparable agencies, publishes the pattern, and occasionally testifies at post-flood hearings. Holds analytic leverage but no lever on the drill calendar itself.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, disaster_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives dispersed response organizations a shared annual rhythm: a common drill calendar, standardized inspection checklists, inter-agency contact protocols, and a public-signaling schedule. Whatever else it does, it synchronizes paperwork and personnel across fire, police, medical, public works, and volunteer squads.
% TRANSFER_FUNCTION: Moves appropriated preparedness funds and staff hours into the production of compliance artifacts — drill records, sign-off sheets, readiness certificates — and moves reassurance from the agency to the public and the legislature; it moves realized flood losses, when they arrive, onto the residents of the protected plain.
% ABSENT_VOICES: Volunteer rescue crews see the form/substance gap on every deployment and hold no seat in the process that writes the official record; floodplain residents are consulted through brochures, not hearings; frontline responders' after-action memoranda are filed by the same office whose performance they critique. Insurers hold quantified evidence of the gap but communicate only through pricing.
% DISAPPEARANCE_RATIONALE: The reassurance economy, the compliance calendar, the certification traffic to the legislature, and the preparedness appropriation line all reorganize within a budget cycle. Nothing about the river changes and no lost competence returns — that is the reading's point — but every arrangement organized around the form (reporting duties, drill-season staffing, displayed certificates, liaison routines) would need immediate replacement or open admission.
% FOUNDING_PROBLEM: After the founding flood, the region had no standing interface between civil government, rescue services, and the public: improvised command, unknown evacuation routes, uninspected levees. The charter created a permanent drill-and-inspection regime to build rehearsed coordination and demonstrate it continuously.
% FOUNDING_PROBLEM_CORROBORATION: The founding commission report and enabling statute attest the original problem from outside today's operating offices. Its current status is attested divergently: the agency cites recurring near-miss activations as proof the problem is live and managed, while insurer loss curves, university disaster-research centers, and volunteer after-action testimony — sources outside the agency's legitimacy economy — attest that the regime ceased addressing the problem and that exposure has grown with floodplain development. No neutral body currently adjudicates between these attestations.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is authored high (0.82) because the regime's observable output is overwhelmingly artifact: scripted drills with pre-briefed participants, inspections announced in advance and routed past known deficiencies, certificates issued on documentation rather than demonstration. Extractiveness (0.60) runs through two channels — quiet-year diversion of capability funding into ceremony, and the capitalized expectation of realized flood losses that working capability would have reduced — and the series rises as the form/substance gap widens. Suppression (0.38) is a raw structural property, unscaled by power or scope: mandates compel attendance and audit trails compel paperwork, but nothing blocks exit or bans alternatives — reform proposals are simply never funded. Accessibility collapse is low (0.30): the alternative (no-notice live exercises, capability line items) remains fully imaginable and is understood by every seat; it is untaken because its costs and benefits fall on different tenures. Resistance (0.30) is episodic — post-flood hearings, auditor memos, volunteer testimony — and decays between events. The measurement series is the secular trend beneath a sawtooth: each flood briefly spikes resistance and dips theater, and each spike relapses within two to three budget cycles, so the oscillation is not itself the extraction mechanism — the ratchet is the quiet-year relapse. Claimed type (piton) is authored from the reading's structural logic — atrophied function, intact form, no capturer, prohibitive fix — independently of these metric values; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda_setter seat should compute different types from the same underlying structure. From the agency's interior, the drill calendar is not a husk but the job itself: the year is structured by it, careers advance through it, and its artifacts are the institution's visible product — a seat experiencing coordination plus legitimation, damped by the knowledge that it could not survive openly admitting the gap. From the floodplain, the same calendar is assurance purchased with real exposure. Insurers experience the structure as a pricing input and exit through arbitrage rather than opposition; the research community sees it entire but holds no lever. The engine computes this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. population_at_flood_risk and municipal_taxpayers are declared victims with trapped and constrained exit respectively, placing both near the full-target end — amplified for residents by the trap and by the regime's regional scope, which makes verification of the gap expensive for outsiders and cheap to simulate for insiders. The agency is deliberately NOT declared in base_properties.beneficiaries: its gain (legitimation) is not an actor-collected rent but a vindicated proposition's yield; on the stakeholder surface it carries secondary_role beneficiary, and its derived directionality sits mid-range — partial beneficiary on the legitimacy channel, pushed back toward target by identity_lock, since it bears the reputational tail risk of the gap it administers. Insurers, as observers with arbitrage exit, derive near the beneficiary end: the regime's information asymmetry subsidizes their pricing position while they bear none of its costs. Volunteers, excluded from the transfer, derive near symmetric with a secondary burden at flood time. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading is itself a mandatrophy finding: the mandate — build and continuously demonstrate readiness — has outlived its function, which atrophied behind the demonstrating. Classification discipline prevents the two standard mislabels. Read as rope (the agency's self-description: 'we coordinate and practice'), the dead function disappears into the living form. Read as snare (the post-disaster prosecutorial framing: 'they knew and collected anyway'), the analysis demands a capturer — and the record shows none: receipts are consumed as ceremony, no seat banks the difference, and no one suppresses reform so much as no one is positioned to profit from it. Piton is the structure the record supports: the administrator could change the arrangement, the cost of fixing exceeds what the administrator bears, and what remains is mostly performance. The receipt surface records this affirmatively: gain_flow is 'diffuse' after checking every named seat (the agency's appropriations are burned as staging, not banked; suppliers are competitive fee-for-service and are not seated), and fixing_cost is 'prohibitive' — a multi-budget-cycle rebuild whose benefits mature after the fixer's tenure ends. The founding-problem interview corroborates: status contested, attestation split between the agency's self-certification and outside loss data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separation,
    'This constraint is the husk_reading of kernel preparedness_persistence. What observation separates it from competence_reading and hybrid_reading?',
    'A no-notice, independently evaluated full-scale exercise scored against the founding charter''s own benchmarks: competence_reading predicts benchmark-meeting performance, hybrid_reading predicts live components passing while ritual components fail, husk_reading predicts broad failure across components with paperwork fully intact.',
    'Resolution toward competence dissolves this file into the sibling (epsilon collapses toward coordination cost); toward hybrid splits it; confirmation entrenches the piton classification and strengthens the case for sunset-or-substantiate review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separation, empirical, 'Which reading of the preparedness kernel the operational record actually supports.').

omega_variable(
    latent_loss_capitalization,
    'How much of the regime''s measured extraction is quiet-year budget diversion into ceremony versus capitalized expectation of realized flood losses?',
    'Actuarial comparison of annualized flood losses under observed response capability versus charter-standard capability, layered over the drill-appropriation record.',
    'If capitalized latent loss dominates, effective extraction for the resident seat is far higher than quiet-year measures suggest and the classification hardens; if diversion dominates, the regime sits nearer the transient-neglect boundary of the piton cell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latent_loss_capitalization, empirical, 'Split of extraction between visible waste and capitalized disaster loss.').

omega_variable(
    institutional_identity_fusion,
    'Does the form persist through inertial budgeting alone, or through identity fusion — administrators whose professional selves are constituted by the ritual calendar?',
    'Leadership-cohort natural experiment: track whether an incoming administration that replaces ceremony with live exercises sustains the change past its own tenure, plus structured interviews on what the drill season means to career incumbents.',
    'Identity fusion places the agenda_setter nearer the full-target end (it bears the gap it administers) and lengthens the husk''s expected lifetime; pure inertia lowers both and predicts faster decay once attention arrives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_fusion, empirical, 'Inertia versus identity lock as the persistence mechanism.').

omega_variable(
    competence_benchmark_frame,
    'Atrophied relative to what — the founding charter''s capability standard, or contemporary best practice among comparably exposed regions?',
    'Cross-regional benchmarking of demonstrated response times and equipment readiness against peer agencies, controlling for hydrological exposure.',
    'Against the charter baseline the husk reading is severe; against contemporary best practice the atrophy may be partial, shifting weight toward hybrid_reading and lowering epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_benchmark_frame, conceptual, 'Framing choice that sets the atrophy denominator.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__husk_reading, theater_ratio, 8, 0.63).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__husk_reading, theater_ratio, 16, 0.69).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.75).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__husk_reading, theater_ratio, 32, 0.79).
narrative_ontology:measurement_basis(prep_tr_t32, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__husk_reading, theater_ratio, 40, 0.82).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__husk_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__husk_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__husk_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement_basis(prep_be_t32, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__husk_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__husk_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__husk_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement_basis(prep_su_t8, observed).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__husk_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement_basis(prep_su_t16, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__husk_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t32, preparedness_persistence__husk_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement_basis(prep_su_t32, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__husk_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drills and inspections maintain readiness' carries three structurally distinct claims, decomposed per the epsilon-invariance principle: competence_reading authors epsilon near coordination-cost levels (live practiced knowledge); this husk_reading authors epsilon 0.60 with theater_ratio 0.82 (memorial form over atrophied substance); hybrid_reading authors a stratified profile (some components live, some ritual). The competence claim operates rhetorically upstream — it is cited in budget hearings as evidence against the husk claim — so this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
