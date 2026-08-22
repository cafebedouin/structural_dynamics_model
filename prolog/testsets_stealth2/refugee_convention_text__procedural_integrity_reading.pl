% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention — Procedural Integrity Reading (Fair Individualized Assessment Safeguard)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Convention Relating to the Status of Refugees and its 1967
 *   Protocol form the standing arrangement this story is about: a
 *   near-universal treaty regime under which states operate individualized
 *   asylum determination. This file instantiates ONE reading of that
 *   contested text — the procedural_integrity_reading — under which the
 *   Convention functions as a procedural safeguard: protection thresholds may
 *   flex within a state-defined band, but fair individualized assessment is
 *   non-negotiable, and the correctness of outcomes is subordinate to the
 *   integrity of the process producing them. Assessed by that reading's own
 *   lights, the standing arrangement delivers genuine individualized
 *   assessment through most interior systems while its accelerating edges —
 *   maritime interdiction, externalized processing, border-procedure
 *   deadlines, offshore transfer — deny the promised hearing to a growing
 *   share of claimants. Constraint-family note: the colloquial label 'the
 *   Refugee Convention' decomposes into three structurally distinct stories
 *   (this reading, restrictive_sovereignty_reading,
 *   expansive_humanitarian_reading) with different epsilon values, victim
 *   sets, and classifications; the siblings are separate files linked through
 *   network.affects_constraints, and this file's epsilon judges the standing
 *   arrangement by process lights only — it neither adopts the expansive
 *   reading's humanitarian referent nor the restrictive reading's sovereignty
 *   referent. The claimed type and the metrics are independent authored
 *   facts: tangled_rope is claimed from structure (a real coordination
 *   function, identifiable payers, active enforcement), while the metric
 *   values describe observed operation.
 *
 * KEY AGENTS:
 *   - state_parties: Primary agenda-setter and net beneficiary (institutional/constrained) — administers the system, retains threshold flexibility, funds the machinery
 *   - national_asylum_agencies: Operational administrator (institutional/constrained) — absorbs caseload strain, identity fused with throughput targets
 *   - individually_assessed_claimants: Secondary beneficiary (powerless/trapped) — receives the safeguard where process functions
 *   - degraded_process_claimants: Primary target (powerless/trapped) — bears accelerated, externalized, or summary handling
 *   - interdicted_high_seas_claimants: Excluded target (powerless/trapped) — removed before any hearing exists
 *   - unhcr_supervision: Supervisory observer (institutional/analytical) — monitors, standard-sets, reports
 *   - regional_human_rights_courts: Enforcement arm (institutional/analytical) — adjudicates the procedural floor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.62).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.64).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention — Procedural Integrity Reading (Fair Individualized Assessment Safeguard)").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'c4f9bf34-ccf6-4b7c-b527-aea40faf36d4').
narrative_ontology:cs_kernel_codification('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', fixed_text).
narrative_ontology:cs_authority_grounding('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', lineage).
narrative_ontology:cs_interpretation_layer_present('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4').
narrative_ontology:cs_reading_relation('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', refugee_convention_text__restrictive_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', foundational, process_integrity_non_negotiable).
narrative_ontology:cs_axiom_status(process_integrity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', process_integrity_non_negotiable, deontological).
narrative_ontology:cs_axiom('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', secondary, individualized_assessment_required).
narrative_ontology:cs_axiom_status(individualized_assessment_required, holdable).
narrative_ontology:cs_axiom_grounding('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', individualized_assessment_required, conventional).
narrative_ontology:cs_reference_frame('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', fair_individualized_determination_baseline).
narrative_ontology:cs_drift_state('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c4f9bf34-ccf6-4b7c-b527-aea40faf36d4', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, state_parties).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, individually_assessed_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, degraded_process_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, interdicted_high_seas_claimants).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, non_refoulement_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, individualized_status_determination).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, effective_remedy_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the Convention and its Protocol and run the asylum systems that give them effect. They draft and amend national procedures, set the subsidiary categories and evidentiary thresholds that sit inside the treaty's flexible band, and answer to treaty-monitoring bodies and domestic courts for how claims are handled. They collect legitimacy from operating an individualized determination system and retain wide latitude over definitions and evidentiary standards; the treaty text and supervisory machinery bar abolishing substantive review altogether. Leaving the treaty framework carries reputational and diplomatic costs, and several states pursue added flexibility through externalization partnerships instead.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, state_parties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, state_parties, beneficiary).

% Operate the determination machinery day to day: scheduling interviews, commissioning interpreters and country-of-origin information, drafting reasoned decisions, supporting appeal stages. They face caseload surges, political pressure for faster clearance, and staffing ceilings, and their institutional identity has fused with throughput and backlog-reduction targets. They cannot leave the system they administer; adaptation happens inside it, through procedure design, prioritization rules, and simplified decision tracks.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, national_asylum_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Reach the territory of a state party and lodge a claim; attend a personal interview with interpretation; receive a written, reasoned decision that is appealable in many jurisdictions. Where the machinery works they obtain an individualized verdict on their fear of persecution and, if recognized, residence documents, family reunification, and a path toward durable status. Leaving the process means returning to the country they fled or drifting into irregularity; onward movement is blocked by visa requirements and transfer rules, and no private channel sells what the hearing provides.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, individually_assessed_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Encounter the system through its accelerated and externalized edges: border-procedure deadlines measured in days, interviews conducted without adequate interpretation, transfer to offshore sites where legal advice is scarce, or summary triage into categories that skip full examination. Their claims are decided, deferred, or deflected without the individualized hearing the treaty promises; they bear prolonged detention, family separation, and return to danger. Their exit options mirror the assessed claimants' but with weaker procedural footholds at every step.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, degraded_process_claimants, payer,
    powerless, biographical, trapped, global).

% Are intercepted before reaching any territory where a claim could be lodged — turned back at sea, transferred to third-country facilities, or held in transit zones. No individualized hearing precedes their removal; they would contest the legality of interdiction if any forum would hear them, but the interception arrangements are built precisely to avoid creating such a forum. Their situation does not appear in the determination statistics through which the system accounts for itself.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, interdicted_high_seas_claimants, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, interdicted_high_seas_claimants, excluded).

% Supervises application of the treaty, issues guidelines on procedure and exclusion, audits state systems, and intervenes in strategic litigation. It documents where practice departs from fair-hearing standards and brokers responsibility-sharing arrangements. It holds no enforcement power of its own; its leverage is convening, standard-setting, and public reporting.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr_supervision, observer,
    institutional, generational, analytical, global).

% Adjudicate whether national asylum procedures satisfy fundamental-rights guarantees: access to territory, effective remedy, conditions of detention, protection against return to harm. Their rulings have halted maritime pushbacks, forced reopening of closed files, and shaped border-procedure design. They enforce the procedural floor but cannot legislate substantive categories; their docket grows with each new externalization arrangement.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, regional_human_rights_courts, agenda_setter,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, state_parties).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, reviewable standard for deciding protection claims: every claimant within a state party's jurisdiction can lodge a claim, be interviewed individually, and receive a reasoned decision, so that protection decisions are comparable across jurisdictions and no state can quietly shift its protection duties to neighbors by simply declining to decide.
% TRANSFER_FUNCTION: Moves decision authority over life-determining status questions from unilateral executive discretion into a supervised procedural channel; moves the costs of running that channel — adjudication, interpretation, legal aid, detention capacity — onto state administrations; and, where access fails, moves the risks of error and return-to-danger onto the claimants least able to bear them.
% ABSENT_VOICES: Claimants intercepted before reaching any forum — at sea, in transit zones, in offshore facilities — are absent from every table where procedures are designed; transit-state municipalities and host communities bearing warehoused populations rarely participate in treaty diplomacy; UNHCR attends as supervisor, but states draft, ratify, and enter reservations.
% DISAPPEARANCE_RATIONALE: If the procedural safeguard vanished overnight, states would revert to discretionary, unreviewable protection decisions; the supervisory ecosystem of courts and UNHCR guidelines would lose its textual anchor; claimant outcomes would track bilateral politics and administrative caprice; and a market for removal-without-hearing would price itself instantly. Arrangements demonstrably depend on the safeguard continuing.
% FOUNDING_PROBLEM: Post-war mass displacement had no agreed mechanism for deciding who merited protection: ad hoc group judgments were arbitrary, generated interstate friction over burdens, and left individual fates to diplomatic bargaining. The arrangement was built to individualize and legitimize that decision through a fair, reviewable hearing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: regional human-rights courts attest from an enforcement seat that individualized determination remains the operative problem — their dockets fill with procedural-failure cases; UNHCR Global Trends records displacement at record highs; academic legal scholarship and NGO field documentation independently describe the founding problem as unresolved. No source, inside or outside the beneficiary set, attests that the problem is solved.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the referent is the standing arrangement — state determination practice under the treaty — judged by this reading's own process lights; interior systems in most state parties deliver real individualized assessment, but interdiction, externalization, and acceleration deny or degrade the hearing for a substantial and growing minority, so epsilon is substantial without approaching pure-extraction levels. Suppression 0.64 is a raw structural property, unscaled by power or scope: the arrangement is held up by interdiction operations, carrier-sanction regimes, detention capacity, safe-third-country designations, and readmission chains — coercive machinery aimed at keeping claimants away from the hearing itself. Theater_ratio 0.50: roughly half of observable procedural activity now signals compliance (template credibility findings, summary triage categories, rubber-stamped appeals) rather than deciding claims. Accessibility_collapse 0.50 mixes across seats: claimants have effectively no alternative to the state channel once inside a jurisdiction, while state parties retain open alternatives inside the flexible threshold band — the reading's own concession keeps state-side alternatives alive. Resistance 0.52: sustained state resistance (externalization partnerships, litigation against procedural directives, non-cooperation) against court and NGO defense of the floor. All three tracked series share one eight-point grid (1951–2025); the trajectories are monotonic ratchets rather than cycles — enforcement builds on prior enforcement — so no intermittent-reinforcement analysis applies. The claimed type and the metrics were authored independently.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. Degraded-process and interdicted claimants are powerless and trapped: no alternative channel exists for them, so effective extraction concentrates at the full-target end. Individually assessed claimants sit near the beneficiary end: the same machinery subsidizes them with a hearing and its protections. State parties are declared beneficiaries yet also fund and staff the machinery — their derived directionality sits low but not at zero, reflecting flexibility rents partly spent on compliance costs. National asylum agencies occupy an ambivalent middle: they absorb caseload strain (target-side) while accruing mandate and budget from enforcement intensity (beneficiary-side); no directionality override was authored because overrides key on the power atom and would also strike the institutional observers, so the agencies' ambivalence is left to the canonical fallback and flagged here. Regional courts and UNHCR hold analytical exits and compute near-neutral. Same-tier differentiation: state parties and national agencies share the institutional power atom but diverge in exposure — states capture the flexibility rents, agencies internalize the throughput pressure; courts and agencies share institutional standing but sit on opposite sides of procedural strictness, courts enforcing the floor the agencies are pressured to shave.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low end: state_parties (flexibility, legitimacy, avoided protection burdens) and individually_assessed_claimants (the hearing itself, and protection where thresholds are met) derive low directionality, pushed toward subsidy by the claimants' trapped exit — they cannot purchase a hearing anywhere else. Victim declarations drive the high end: degraded_process_claimants and interdicted_high_seas_claimants are powerless and trapped, placing them near the full-target end; the interdicted seat is the extreme case, removed before any hearing exists to fail them. Scope amplifies: the regime is near-global, so verification of process quality is difficult and effective extraction scales up modestly for the target seats. The vindicated propositions (non-refoulement doctrine, individualized determination, effective remedy) collect nothing and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (record displacement; individualized determination still the operative question) and the world rearranges without the safeguard, so no dead-mandate mismatch arises and no mandatrophy resolution is declared. The classification work this reading performs is boundary-keeping in both directions: against a pure-coordination label, which would erase the payer seats whose process is denied — victims are named and enforcement is active; against a pure-extraction label, which would erase the genuine collective-action function — a common reviewable standard prevents beggar-thy-neighbor denial races, and state-side alternatives remain open inside the flexible band, so exits are not suppressed for the parties the reading defers to. The tangled-rope claim holds both facts in one structure; the temporal series shows which way the mixture is drifting (extraction and theater rising together), which is the datum the corpus exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the procedural_integrity_reading of the refugee_convention_text kernel; would the restrictive_sovereignty_reading or the expansive_humanitarian_reading of the same text produce a different victim set, epsilon, or classification?',
    'Generate the sibling stories and compare computed classifications; empirically, observe whether states exercising threshold flexibility behave as good-faith margin-holders or as minimum-floor seekers converging on the restrictive reading.',
    'If threshold flexibility operates as a ratchet toward the restrictive reading''s minimum floor, this reading''s victim set expands toward the restrictive reading''s and its classification drifts toward the snare end; if flexibility stays bounded by process guarantees, the reading holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel contest: sibling readings of the Convention text would restructure the victim set and epsilon.').

omega_variable(
    procedural_access_victim_boundary,
    'Is the victim set limited to claimants formally denied procedural access (interdiction, offshore transfer without guarantees), or does it extend to claimants processed through nominally open but substantively degraded channels?',
    'Compare decision quality across procedure types: grant-rate divergence between comparable cohorts, appellate overturn rates, interview-duration and interpretation-adequacy audits.',
    'A broader victim set raises effective extraction for the payer seats and pushes the computed classification toward the snare end; a narrowly bounded set supports the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_access_victim_boundary, empirical, 'Where the boundary of the victim set sits relative to procedural access.').

omega_variable(
    offshore_guarantee_possibility,
    'Can offshore or extraterritorial processing ever deliver the full procedural guarantees this reading requires, or is territorial presence constitutive of fair individualized assessment?',
    'Matched comparison of onshore and offshore determination outcomes controlling for resources and cohort composition; litigation record on extraterritorial procedure.',
    'If guarantees are territorially impossible, the reading''s allowance for offshore processing licenses systematic extraction and the victim set widens to all offshore-processed claimants; if achievable, the coordination function extends beyond territory and the reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_guarantee_possibility, conceptual, 'Whether the reading''s offshore-permissibility clause is operable or self-defeating.').

omega_variable(
    enforcement_ratchet_vs_demand,
    'Does the rising suppression_requirement series reflect a deliberate enforcement ratchet (externalization infrastructure compounding on itself) or proportional capacity response to record displacement demand?',
    'Normalize enforcement indicators (interceptions, detention bed-days, carrier penalties) per claimant across demand cycles; test the lag structure between demand spikes and enforcement buildout.',
    'A ratchet supports continued drift toward the snare end and validates the extraction-accumulation hypothesis; demand-proportionality supports a stable coordination reading with cyclical noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_vs_demand, empirical, 'Driver of the enforcement intensification visible in the temporal series.').

omega_variable(
    procedural_theater_diagnosis,
    'Is the rising theater_ratio performative maintenance of legitimacy (compliance signaling without decision-quality change) or transitional overload that recedes with capacity investment?',
    'Track whether theater indicators (template credibility findings, rubber-stamped appeal rates) fall when adjudication capacity rises, or persist independently of capacity.',
    'Persistent theater under adequate capacity would mark degeneration toward inertial performance; capacity-correlated theater marks strain rather than decay and leaves the coordination function intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_theater_diagnosis, empirical, 'What the measured procedural theater is symptomatic of.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_integrity_rc_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t1951, observed).
narrative_ontology:measurement(proc_integrity_rc_tr_t1967, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t1967, observed).
narrative_ontology:measurement(proc_integrity_rc_tr_t1985, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1985, 0.24).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t1985, observed).
narrative_ontology:measurement(proc_integrity_rc_tr_t1999, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1999, 0.3).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t1999, observed).
narrative_ontology:measurement(proc_integrity_rc_tr_t2011, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2011, 0.36).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t2011, observed).
narrative_ontology:measurement(proc_integrity_rc_tr_t2015, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t2015, observed).
narrative_ontology:measurement(proc_integrity_rc_tr_t2020, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2020, 0.47).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t2020, observed).
narrative_ontology:measurement(proc_integrity_rc_tr_t2025, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2025, 0.5).
narrative_ontology:measurement_basis(proc_integrity_rc_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(proc_integrity_rc_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.28).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t1951, observed).
narrative_ontology:measurement(proc_integrity_rc_be_t1967, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1967, 0.31).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t1967, observed).
narrative_ontology:measurement(proc_integrity_rc_be_t1985, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1985, 0.37).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t1985, observed).
narrative_ontology:measurement(proc_integrity_rc_be_t1999, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1999, 0.44).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t1999, observed).
narrative_ontology:measurement(proc_integrity_rc_be_t2011, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2011, 0.5).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t2011, observed).
narrative_ontology:measurement(proc_integrity_rc_be_t2015, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t2015, observed).
narrative_ontology:measurement(proc_integrity_rc_be_t2020, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t2020, observed).
narrative_ontology:measurement(proc_integrity_rc_be_t2025, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(proc_integrity_rc_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(proc_integrity_rc_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.2).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t1951, observed).
narrative_ontology:measurement(proc_integrity_rc_su_t1967, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1967, 0.22).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t1967, observed).
narrative_ontology:measurement(proc_integrity_rc_su_t1985, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1985, 0.32).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t1985, observed).
narrative_ontology:measurement(proc_integrity_rc_su_t1999, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1999, 0.4).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t1999, observed).
narrative_ontology:measurement(proc_integrity_rc_su_t2011, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2011, 0.46).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t2011, observed).
narrative_ontology:measurement(proc_integrity_rc_su_t2015, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t2015, observed).
narrative_ontology:measurement(proc_integrity_rc_su_t2020, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t2020, observed).
narrative_ontology:measurement(proc_integrity_rc_su_t2025, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(proc_integrity_rc_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Refugee Convention' covers three structurally distinct claims that the ε-invariance principle separates into a constraint family: the expansive_humanitarian_reading (threshold breadth — contested, advocacy careers ride on it, high epsilon against narrow-threshold practice), this procedural_integrity_reading (process integrity — the floor courts actually enforce, moderate-high epsilon against degraded-access practice), and the restrictive_sovereignty_reading (sovereign discretion — low epsilon from its own seat, high from the others'). The upstream story is the treaty text itself as fixed kernel; each reading is a downstream instantiation. This file links both siblings; the family is complete only when all three files cross-link.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
