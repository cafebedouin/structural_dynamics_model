% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence (Drill-and-Inspection Regime)
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   A statutory preparedness regime operates on a standing calendar: tabletop
 *   exercises escalating to full-scale field drills, scheduled and no-notice
 *   equipment inspections, joint inter-agency exercises, and after-action
 *   reviews whose findings feed repairs and protocol revisions. On the
 *   competence reading, this machinery constitutes live exercised knowledge:
 *   rehearsal counters the skill decay that sets in between rare events,
 *   inspections catch physical and procedural degradation before it
 *   compounds, and repeated joint operation pre-builds the inter-agency trust
 *   that disasters otherwise demand instantly. Costs are real but reciprocal
 *   — duty hours, levy-funded budgets, administrative overhead — and the
 *   product is a public readiness stock that no participant privately
 *   captures. The claim and the metrics are authored independently: the
 *   claimed type states what this reading holds the arrangement to be; the
 *   metrics state what it observably does.
 *
 * KEY AGENTS:
 *   - regional_emergency_responders: principal beneficiary seat (organized/constrained) — surrenders duty hours to rehearsal, collects retained proficiency and pre-built mutual aid
 *   - protected_residents: principal beneficiary seat (moderate/constrained) — funds and complies, receives practiced response
 *   - specialized_water_defense_engineers: beneficiary and technical administrator (institutional/identity_locked) — the corps whose exercised judgment is the asset being kept alive
 *   - national_emergency_management_agency: agenda setter (institutional/constrained) — owns the calendar, the audits, and the after-action loop
 *   - unexposed_taxpayers: cost-bearing seat (moderate/constrained) — pays without direct exposure; the margin where over-investment would bite
 *   - unmodeled_hazard_communities: excluded seat (powerless/trapped) — inside the jurisdiction, outside the scenario library
 *   - comptroller_audit_office: analytical observer (institutional/analytical) — measures readiness value against spend
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence (Drill-and-Inspection Regime)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/institutional_memory/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '964ab4b2-f65a-4b39-920c-a3fd6feddf81').
narrative_ontology:cs_kernel_codification('964ab4b2-f65a-4b39-920c-a3fd6feddf81', formalized).
narrative_ontology:cs_authority_grounding('964ab4b2-f65a-4b39-920c-a3fd6feddf81', practice).
narrative_ontology:cs_interpretation_layer_present('964ab4b2-f65a-4b39-920c-a3fd6feddf81').
narrative_ontology:cs_reading_relation('964ab4b2-f65a-4b39-920c-a3fd6feddf81', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('964ab4b2-f65a-4b39-920c-a3fd6feddf81', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('964ab4b2-f65a-4b39-920c-a3fd6feddf81', foundational, drills_preserve_live_operational_competence).
narrative_ontology:cs_axiom_status(drills_preserve_live_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('964ab4b2-f65a-4b39-920c-a3fd6feddf81', drills_preserve_live_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('964ab4b2-f65a-4b39-920c-a3fd6feddf81', secondary, inspection_findings_compel_corrective_action).
narrative_ontology:cs_axiom_status(inspection_findings_compel_corrective_action, holdable).
narrative_ontology:cs_axiom_grounding('964ab4b2-f65a-4b39-920c-a3fd6feddf81', inspection_findings_compel_corrective_action, empirically_contingent).
narrative_ontology:cs_reference_frame('964ab4b2-f65a-4b39-920c-a3fd6feddf81', exercised_competence_maintenance).
narrative_ontology:cs_drift_state('964ab4b2-f65a-4b39-920c-a3fd6feddf81', contemporary_climate_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('964ab4b2-f65a-4b39-920c-a3fd6feddf81', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, regional_emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, protected_residents).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, specialized_water_defense_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, regional_emergency_responders).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, protected_residents).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, unexposed_taxpayers).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, spaced_rehearsal_retention_doctrine).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, inspection_feedback_loop_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Career and volunteer firefighters, flood-fight crews, and emergency medical teams in hazard-prone regions. They surrender scheduled duty hours to scenario exercises, equipment checks, and joint drills with neighboring services; in return their skills stay current and their inter-agency working relationships are pre-built before an event forces them. Leaving the service means abandoning a vocation most entered for life; transferring to another region means restarting accreditation from zero.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, regional_emergency_responders, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, regional_emergency_responders, payer).

% Households and businesses in flood plains, wildfire interfaces, and seismic zones. They fund the exercise system through local levies and the national budget, comply with evacuation drills and building inspections, and receive the product: response organizations that arrive practiced rather than improvising. Relocation away from hazard zones is possible for some but means abandoning homes, jobs, and family land.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, protected_residents, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, protected_residents, payer).

% The career technical corps that designs, inspects, and stress-tests dams, dikes, and storm-surge barriers. Its members spend decades inside the same institutions, run full-scale failure-mode exercises on physical infrastructure, and apprentice successors through supervised inspection tours. The corps' judgment exists almost nowhere else; an individual who leaves takes a slice of it, and the institution absorbs the loss slowly through the apprenticeship chain.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, specialized_water_defense_engineers, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, specialized_water_defense_engineers, agenda_setter).

% The statutory body that sets the national exercise calendar, accredits local drill programs, conducts audits and inspections, and compiles after-action findings into revised protocols. Its staff careers are built inside the system it administers, and its budget depends on demonstrating continued readiness value to the legislature each cycle.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, national_emergency_management_agency, agenda_setter,
    institutional, generational, constrained, national).

% Households and firms outside declared hazard zones who fund the exercise and inspection system through general taxation without living behind the dikes or in the fire interface. They see the line-item costs annually and the payoff only when a disaster elsewhere is contained quickly; their main lever is fiscal criticism of exercise spending.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, unexposed_taxpayers, payer,
    moderate, biographical, constrained, national).

% Settlements exposed to compound or novel hazards — cascading heat-power-water failures, flood regimes outside historical return periods — that fall outside the scenario library the exercise system rehearses. They live inside the protection system's jurisdiction but their risks are not on the drill schedule; they lack the standing to add scenarios, and few can afford to relocate.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, unmodeled_hazard_communities, excluded,
    powerless, biographical, trapped, regional).

% The independent fiscal office that audits exercise spending against measured readiness outcomes, flags diminishing returns, and publishes cost-effectiveness comparisons across regions. It runs on data the operating agencies supply and holds no operational role in the drills themselves.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, comptroller_audit_office, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts individually perishable skills and scattered institutional experience into a maintained, shared readiness stock: scheduled rehearsal counters skill decay between rare events; joint exercises pre-build inter-agency trust and communication lines that cannot be improvised mid-disaster; inspections convert findings into physical repairs and plan revisions before failure.
% TRANSFER_FUNCTION: Moves tax revenue and personnel duty-hours into rehearsal, equipment testing, and corrective maintenance; moves tacit operational knowledge from veterans to successors through supervised exercise; moves inspection findings into infrastructure repair orders. Nothing in the flow terminates in a private purse — outputs land in public capacity and repaired infrastructure.
% ABSENT_VOICES: Communities facing hazards outside the rehearsed scenario library would object that the calendar rehearses the last disaster rather than the next one; volunteer and informal responders hold ground-level knowledge that rarely enters formal after-action loops; fiscal critics outside the audit office's remit have no seat when exercise budgets are set. They are absent because scenario selection and budget setting sit inside the agencies that run the system.
% DISAPPEARANCE_RATIONALE: Response organizations would begin losing proficiency immediately: equipment checks lapse, joint procedures rust, and veteran knowledge exits with retirements without being replaced. The first major flood or wildfire under an unrehearsed system would produce slower mobilization, failed inter-agency handoffs, and avoidable losses, and governments would rebuild the exercise machinery under post-disaster pressure — the arrangement would reassemble because the underlying decay problem never left.
% FOUNDING_PROBLEM: Recurring disasters exposed that response capacity evaporates between events: personnel turn over, equipment degrades unnoticed, written plans ossify, and organizations that performed adequately a decade earlier fail at the next event. The drill-and-inspection regime was built to defeat that inter-event decay.
% FOUNDING_PROBLEM_CORROBORATION: Independent skill-decay research in emergency medicine and firefighting documents measurable proficiency loss without rehearsal; insurer loss-adjustment data show faster, cheaper outcomes in jurisdictions with recent full-scale exercises; post-disaster inquiry commissions staffed from outside the exercising agencies repeatedly cite lapsed inspection and drill gaps as aggravating factors; and the unmodeled-hazard communities themselves attest the problem is live by pointing at the scenarios that exclude them. Attestation does not rest on the benefiting parties alone.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because the arrangement's flows terminate in public capacity rather than any seat's pocket, and its costs are roughly reciprocal to the safety product; the residual is administrative overhead and compliance burden. Suppression (0.22) reflects mandated participation and compulsory inspection access — real coercion in service of the schedule, not closure of alternatives, since households and firms remain free to prepare beyond the mandate and to criticize it. Theater_ratio (0.12) is this reading's signature: announced exercises occasionally stage for cameras, but the after-action loop ties most activity to correctable findings. Accessibility_collapse (0.28) is low because alternatives — private insurance, community self-organization, individual stockpiling — persist alongside the regime; it prevails by performing the collective function better, not by closing exits. Resistance (0.20) is limited to routine friction over drill hours and inspection disruption. Coordination type is authored as resource_allocation: the regime's core function is allocating scarce rehearsal hours and budget against skill decay; the statutory mandate is the instrument, not the function. The temporal series share one grid: extractiveness dips through the post-Cold-War drawdown and creeps back with audit bureaucratization; theater spikes briefly when budget cuts trim real exercises faster than ceremonial ones, then falls as after-action professionalization matures; suppression_requirement traces the enforcement arc — statutory mandate strength decaying after the Cold War and partially rebuilding in the climate-adaptation era.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is deliberately muted in this reading, and the muting is informative. From the responder and resident seats the arrangement computes as a net gain purchased with time and levies; from the taxpayer seat it computes as a mild standing cost justified as tail-risk insurance; from the agency seat it is the administrative object the institution has become. Because no seat captures the gains, the usual sharp payer/beneficiary split collapses toward symmetry — which is precisely what the sibling husk reading denies: under that reading the same calendar would split sharply between ceremony-sustaining officials and a public paying for performance. The engine computes per-seat classifications from the structural data; this file supplies the data of a regime whose seats genuinely converge.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: responders, residents, and the engineer corps sit near the beneficiary end — the regime subsidizes their safety and capability, and their exits (leaving the service, abandoning protected land, dissolving the corps' vocation) are costly enough to bind them to it without making them targets. The management agency derives near-symmetric: it bears administrative cost and collects capability, budget relevance, and institutional continuity. Unexposed taxpayers lean target-side — they fund without direct exposure — but the flow lands in diffuse capacity, not in another seat's gain, so their position stays a mild drag rather than extraction. No directionality overrides are needed: the beneficiary declarations plus exit options already place every seated agent correctly, and the excluded community sits outside the derivation entirely, which is the honest record of its position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-event competence decay — remains live: turnover continues, equipment ages, and the hazard frontier moves. Status live paired with verdict world_rearranges is the consistent cell: nothing here flags as a zombie mandate. The classification discipline cuts both ways. Honest low theater_ratio keeps this regime from being misread as the husk pattern (performance mistaken for retention), and the falling theater series argues against inertial drift; conversely, if the over-investment omega resolved adversely, the taxpayer seat would harden and the clean reading claimed here would acquire an extraction shadow — the omegas mark exactly where this classification is falsifiable rather than decorative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the competence_reading of the preparedness_retention kernel — that drills and inspections are competence-preserving practices maintaining live operational capacity. What would the sibling readings change structurally?',
    'Cross-reading comparison on the shared observables: ceremony-to-competence ratio, beneficiary structure, and stratification of retained skill. The husk_reading would move theater_ratio above 0.5 and relocate effective beneficiaries to ceremony-sustaining officials; the hybrid_reading would split the beneficiary structure between specialized technical corps and a ceremonial public layer.',
    'Classification is reading-indexed: under the husk_reading the same drill calendar computes as performance sustained past function; under the hybrid_reading it decomposes into a live segment and a ceremonial remainder. This file''s low theater_ratio and diffuse gain flow are commitments of the competence seat, not neutral facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the preparedness_retention kernel; siblings would restructure beneficiaries and theater.').

omega_variable(
    ceremony_competence_ratio_observability,
    'Can the ceremony-to-competence ratio — the exact locus of disagreement between this reading and its siblings — be measured cleanly enough to settle the contest?',
    'Unannounced no-notice drills, cross-jurisdiction exchange evaluations, and outcome-linked studies comparing exercised versus lapsed units on real-event performance.',
    'If no-notice performance diverges sharply from announced-drill performance, this reading''s low theater_ratio is overstated and the story converges toward the husk structure; if they track, the competence reading is vindicated on its own observable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_competence_ratio_observability, empirical, 'Measurability of the ceremony-versus-competence distinction separating this reading from its siblings.').

omega_variable(
    overinvestment_fiscal_drag,
    'Does exercise and inspection spending exceed the marginal competence it purchases — making fiscal efficiency the arrangement''s first casualty?',
    'Marginal-return analysis across exercise-intensity gradients: compare regions and periods differing in drill frequency against audited readiness outcomes and real-event results.',
    'If returns flatten well below current intensity, unexposed_taxpayers harden from incidental cost-bearers into genuine losers, the arrangement acquires an extraction shadow, and the clean beneficiary structure claimed here narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overinvestment_fiscal_drag, empirical, 'Whether the regime is over-invested past the point where added rehearsal buys added capacity.').

omega_variable(
    scenario_library_novelty_gap,
    'Does the rehearsed scenario library cover the hazard distribution the protected population will actually face, as climate-driven compound events outrun historical return periods?',
    'Audit of the exercise scenario library against forward-looking hazard assessment, weighted by exposed population.',
    'If coverage is narrow, maintained competence is competence for the wrong future: the beneficiary claim contracts to known-hazard populations and the excluded seat''s position hardens from a gap into structural exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scenario_library_novelty_gap, empirical, 'Whether drilled-for hazards match the hazards coming.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t6, preparedness_retention__competence_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(prep_tr_t6, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_retention__competence_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t18, preparedness_retention__competence_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement_basis(prep_tr_t18, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__competence_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t36, preparedness_retention__competence_reading, theater_ratio, 36, 0.12).
narrative_ontology:measurement_basis(prep_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t6, preparedness_retention__competence_reading, base_extractiveness, 6, 0.19).
narrative_ontology:measurement_basis(prep_be_t6, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_retention__competence_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t18, preparedness_retention__competence_reading, base_extractiveness, 18, 0.16).
narrative_ontology:measurement_basis(prep_be_t18, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__competence_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t36, preparedness_retention__competence_reading, base_extractiveness, 36, 0.18).
narrative_ontology:measurement_basis(prep_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t6, preparedness_retention__competence_reading, suppression_requirement, 6, 0.27).
narrative_ontology:measurement_basis(prep_su_t6, observed).
narrative_ontology:measurement(prep_su_t12, preparedness_retention__competence_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement_basis(prep_su_t12, observed).
narrative_ontology:measurement(prep_su_t18, preparedness_retention__competence_reading, suppression_requirement, 18, 0.18).
narrative_ontology:measurement_basis(prep_su_t18, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__competence_reading, suppression_requirement, 24, 0.22).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__competence_reading, suppression_requirement, 30, 0.23).
narrative_ontology:measurement_basis(prep_su_t30, observed).
narrative_ontology:measurement(prep_su_t36, preparedness_retention__competence_reading, suppression_requirement, 36, 0.22).
narrative_ontology:measurement_basis(prep_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness drills' conflates three structurally distinct arrangements, decomposed per the epsilon-invariance principle into a three-story family sharing the preparedness_retention kernel: competence_reading (this file — low theater, diffuse gains, rope-shaped), husk_reading (high theater, ceremony sustained past function, performance without retained skill), and hybrid_reading (stratified: live technical core, ceremonial periphery). Each carries its own epsilon, beneficiaries, and theater profile; the edges here record family membership, and the ordering runs competence (the affirmative claim) to husk (its negation) to hybrid (the stratified synthesis both cite).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
