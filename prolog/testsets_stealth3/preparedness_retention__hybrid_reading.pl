% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Flood Preparedness Retention (Hybrid Reading)
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   Since the 1953 North Sea Flood, the Netherlands has concentrated
 *   flood-defense competence in permanent specialist institutions —
 *   Rijkswaterstaat for national defenses, barriers, and storm-surge
 *   forecasting; the regional water boards for local works and dedicated
 *   taxation — while the broad societal preparedness that once accompanied
 *   them (household flood plans, community drills, walking wardens with
 *   enforcement duties) has thinned into commemoration: anniversary
 *   processions, symbolic national drill days, tabletop exercises with
 *   pre-printed outcomes. The standing arrangement under contest — the
 *   referent for this story's extractiveness — is that stratified system as
 *   it operates today: a live technical core surrounded by a ceremonial
 *   periphery, financed by the population whose independent response capacity
 *   has meanwhile atrophied. The claim and the metrics are authored
 *   independently: the claimed type states the structure this reading
 *   believes true (a genuine coordination core joined to an asymmetric burden
 *   shift); the metrics describe the arrangement's observed operation. The
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - delta_floodplain_residents: Primary target (powerless/trapped) — finance the system, bear the residual risk, hold no practical exit from the protected zone
 *   - rijkswaterstaat_technical_cadre: Primary beneficiary-administrator (institutional/identity_locked) — retains live competence, collects statutory mandate, budget, and continuity
 *   - water_board_establishment: Secondary beneficiary (institutional/constrained) — collects regional tax base and national standing
 *   - infrastructure_ministry: Agenda-setter (powerful/arbitrage) — writes statutes and budgets, absorbs political accountability
 *   - municipal_first_responders: Peripheral target (organized/constrained) — formal response role persists while heavy capability migrated to the center
 *   - volunteer_dike_wardens: Ceremonial-track remnant (moderate/constrained) — honored titles attached to retired duties
 *   - citizen_preparedness_advocates: Excluded voice (organized/constrained) — campaigns for distributed readiness, holds no standard-setting seat
 *   - dutch_safety_board: Analytical observer (institutional/analytical) — investigates gaps between plan and capability, no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.42).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Flood Preparedness Retention (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/institutional_memory/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '5dee4aae-0ad5-46ad-94e9-042f8d353d9a').
narrative_ontology:cs_kernel_codification('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', formalized).
narrative_ontology:cs_authority_grounding('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', expertise).
narrative_ontology:cs_interpretation_layer_present('5dee4aae-0ad5-46ad-94e9-042f8d353d9a').
narrative_ontology:cs_reading_relation('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_axiom('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', foundational, competence_stratifies_by_proximity_to_infrastructure).
narrative_ontology:cs_axiom_status(competence_stratifies_by_proximity_to_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', competence_stratifies_by_proximity_to_infrastructure, empirically_contingent).
narrative_ontology:cs_axiom('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', secondary, professional_substitution_degrades_distributed_resilience).
narrative_ontology:cs_axiom_status(professional_substitution_degrades_distributed_resilience, holdable).
narrative_ontology:cs_axiom_grounding('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', professional_substitution_degrades_distributed_resilience, empirically_contingent).
narrative_ontology:cs_reference_frame('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', dual_track_stratified_readiness).
narrative_ontology:cs_drift_state('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', contemporary_post_safety_board_audits, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5dee4aae-0ad5-46ad-94e9-042f8d353d9a', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, rijkswaterstaat_technical_cadre).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, water_board_establishment).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, infrastructure_ministry).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, delta_floodplain_residents).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_first_responders).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, volunteer_dike_wardens).
narrative_ontology:constraint_vindicates(preparedness_retention__hybrid_reading, technocratic_protection_substitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Career hydraulic engineers and program managers at the national water agency. They design and maintain primary flood defenses, operate the storm-surge barriers, run the national early-warning chain, and certify dike strength. Statutory mandate, multi-year budget lines, and a recruitment pipeline flow to them; public gratitude and blame alike route through their office. Leaving the agency means leaving the profession its senior staff trained into — the institution and the role of keeping the water out have become the same thing, so exit is not a practical option for the body as such.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat_technical_cadre, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, rijkswaterstaat_technical_cadre, beneficiary).

% Elected regional water authorities predating the modern state. They levy a dedicated water tax, administer regional dikes and pumping stations, and sit on the national committees that allocate protection standards. Continuity of the board system, its tax base, and its seat at the national table flow to them, while day-to-day technical work increasingly routes through or defers to the national agency. Dissolving a water board is legally and politically fraught, and no successor arrangement has been built, so the boards persist in their present form.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, water_board_establishment, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, water_board_establishment, agenda_setter).

% The national ministry responsible for water-management policy. It writes the water-safety statutes, sets protection standards, approves the agencies' budgets, and answers parliament whenever flood risk makes headlines. Political credit for visible safety flows to it, along with budget authority; accountability arrives whenever a threat materializes. It can restructure the specialist agencies by statute, spending political capital with each reorganization, and it can shift burdens between national and regional levels as coalitions change.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, infrastructure_ministry, agenda_setter,
    powerful, biographical, arbitrage, national).

% Households living behind the dikes, much of the country below sea level. They pay water-board levies and national contributions that finance the specialist system; in return they are told the water is handled. Few hold personal flood plans, know evacuation routes beyond the highway, or have practiced a response; the last generation with living flood memory is aging out. Relocation would mean abandoning homes, jobs, and a landscape that constitutes the country itself, so leaving the protected zone is not realistically on the table for the population as a class.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, delta_floodplain_residents, payer,
    powerless, generational, trapped, national).

% Safety regions, fire brigades, and municipal emergency planners. Formally they execute evacuation and crisis response; in practice their flood scenarios are annual tabletop exercises with pre-printed conclusions, and the heavy capabilities — barrier operation, surge forecasting, dike reinforcement — sit with national agencies. They request joint exercises and resource-sharing and receive scheduling memoranda. Their leverage is procedural: they cannot self-fund heavy capability, and their mandates depend on the national framework they operate under.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, municipal_first_responders, payer,
    organized, biographical, constrained, local).

% Successors of the historic dike reeves who once held legal duty to walk and inspect assigned stretches of dike. Today most walks are commemorative — anniversary processions, school visits, badge ceremonies — while actual inspection runs on sensors and contractors. The honorific and the community role flow to them; enforcement authority moved away decades ago. Individuals may step down freely, but stepping down means surrendering a hereditary community position, so the tradition continues through whoever accepts the next ceremony.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, volunteer_dike_wardens, payer,
    moderate, biographical, constrained, local).

% Community organizers, aid societies, and researchers campaigning for household flood kits, neighborhood drills, and local response training. They argue distributed readiness is cheaper and faster than waiting for national services during the first hours of a surge event. They appear in consultations as invited witnesses, hold no seat in the standard-setting committees, and depend on the agencies they criticize for access and pilot funding.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, citizen_preparedness_advocates, excluded,
    organized, biographical, constrained, national).

% The independent statutory board that investigates disasters and near-misses. After flood-threat episodes it publishes findings on evacuation feasibility, communication failures, and gaps between written plans and actual capability. Its reports feed parliamentary debate; it commands no enforcement power and relies on the agencies' cooperation for data access.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, dutch_safety_board, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, rijkswaterstaat_technical_cadre).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates hydraulic engineering, storm-surge forecasting, and infrastructure maintenance in permanent specialist institutions that operate across political cycles and generational turnover, so flood-defense capability does not depend on episodic local effort or on living disaster memory.
% TRANSFER_FUNCTION: Moves operational responsibility, budget, and the burden of vigilance from diffuse residents and municipalities to the specialist agencies; moves residual risk and first-hours vulnerability back onto a population whose own response capacity has meanwhile atrophied.
% ABSENT_VOICES: Neighborhood-level preparedness organizers and the aging cohort with living flood memory would object that household and community readiness was traded away without their consent; they sit outside the standard-setting committees, consulted as witnesses at most. Future residents who inherit the concentrated exposure have no seat at all.
% DISAPPEARANCE_RATIONALE: The specialist agencies maintain thousands of kilometres of primary defenses, operate the storm-surge barriers, run the warning chain, and certify dike strength; overnight removal would halt maintenance, freeze barrier operation, and expose the atrophied municipal and household capacity immediately — the water-management order of the delta would have to be rebuilt from nothing, under time pressure.
% FOUNDING_PROBLEM: After the 1953 North Sea Flood killed over eighteen hundred people in the southwest of the country, the founding problem was to make flood-defense competence and infrastructure independent of generational forgetting: permanent institutions that would hold the knowledge even after living memory of the disaster had faded.
% FOUNDING_PROBLEM_CORROBORATION: The post-1953 Delta Commission report and the parliamentary inquiry records corroborate the founding problem from outside today's benefiting parties. The independent safety board's post-episode investigations corroborate the disputed half — that societal readiness, not technical defense, is where capacity now runs thin. No source independent of the benefiting parties attests that the ceremonial societal track solves a still-live problem; that silence is itself signal.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62 reflects the stratified structure's asymmetry: the core delivers real protection (damping epsilon below pure-extraction territory), but the same arrangement drains distributed resilience — responsibility migrates inward, periphery capacity atrophies, and residual risk lands on residents who no longer hold response skills. Suppression 0.42 is structural rather than prohibitive: nothing bans household drills, but statutory consolidation (Water Act 2009, Delta Act 2012), dedicated taxation, and official assurance crowd out and defund local alternatives; roughly four-fifths of the measured suppression is this budgetary-statutory absorption and one-fifth internalized reliance ('the experts handle it') that persists in residents' expectations after barriers to acting are removed. Theater 0.52 weighs the whole system: core operations (barrier control, dike certification, surge forecasting) remain functional while the societal-facing layer — commemorations, ceremonial wardenry, paper evacuation exercises — is predominantly performative; the balance tips just past even as the ceremonial share grew across the interval. Accessibility collapse 0.45: rebuilding distributed readiness remains possible (precedents exist, knowledge recoverable) but compounds in difficulty as atrophy deepens. Resistance 0.40: municipal safety regions petition for joint resourcing, the independent safety board publishes critical findings, decentralization proposals recur — persistent friction that has never restructured the arrangement. All three series share one time grid; values are measured at interval end (2025). The trajectories are monotonic, not oscillatory, so no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the ministry and the specialist agencies, the arrangement is responsible specialization: competence housed where it is used daily, funded predictably, audited professionally. From floodplain households and municipal responders, the same arrangement reads as abandonment dressed as protection — a system that collects their taxes, retires their capacities, and returns reassurance instead of skills. The volunteer wardens occupy a third position: honored titles attached to duties that were taken away. The engine computes these per-seat types from the power, exit, and role data; the gap between the administrator's experience and the payer's experience is the measurement this story exists to register, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   The technical cadre and the water boards sit nearest the beneficiary pole: continuity, statutory mandate, tax bases, and staffing pipelines flow to them. Their exit is locked not because anyone forbids leaving but because the institutions have fused with their function — the national agency cannot stop guarding the delta without ceasing to be itself; if that fusion ever broke (privatization, or a catastrophic failure that delegitimized the cadre), the seat's exit would loosen and its computed position would move. Floodplain residents sit nearest the target pole: they finance the system, bear the residual risk, and hold no practical exit from below-sea-level ground. Municipal responders and volunteer wardens occupy the middle-high band: their formal roles persist while their substance migrated to the center. The ministry is listed among beneficiaries because it collects credit and risk-transfer, but it is not a passive collector — it writes the budgets and answers parliament when threats materialize. The structural derivation alone would place it near the beneficiary pole, so a directionality override lifts the powerful-atom seat to 0.25 to encode that partial cost-bearing. Excluded advocates stand outside the enforcement object; the safety board's seat is analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this arrangement as a hybrid of coordination and extraction keeps both halves on the books. Reading it as pure coordination would erase the burden shift: the atrophying periphery and the single-point-of-failure exposure would vanish from the ledger. Reading it as pure extraction would erase the service: a functioning defense core that genuinely protects millions. The gate requirements for the claimed type — declared beneficiaries, declared victims, active enforcement — force both facts to be authored together. On the genealogy interview, the founding problem (make flood competence survive generational forgetting, post-1953) is live for the technical track and disputed for the societal track, hence status 'contested'; paired with the disappearance verdict 'world_rearranges', the status-by-verdict combination does not assert a dead-mandate zombie flag — the core still earns its keep — but the contested status marks precisely where the arrangement's mandate is thinning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_kernel_assignment,
    'Which reading of the preparedness-retention kernel correctly characterizes the system: stratified (live core, ceremonial periphery), fully ceremonial, or fully competence-preserving?',
    'Outcome-correlated audits: regress measured response performance (evacuation feasibility trials, maintenance defect rates, barrier-operation timing) against drill and inspection records disaggregated by institutional tier.',
    'If the competence view is right, extractiveness falls toward the coordination-cost floor and the arrangement drifts toward pure coordination; if the husk view is right, the core is hollow too and the constraint collapses toward enforced extraction with no service side; confirmation of this hybrid reading stabilizes the dual-track classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_of_kernel_assignment, conceptual, 'This story is one reading of kernel preparedness_retention; sibling readings would change the victim set and epsilon substantially.').

omega_variable(
    centralization_optimum_question,
    'Is centralized specialist retention genuinely superior for delta-scale hazards (modern sensing and warning compensating for local atrophy), or does distributed resilience add irreplaceable value the center cannot replicate?',
    'Comparative outcome studies against more decentralized preparedness systems, plus stress tests that simulate center failure (barrier malfunction, forecast outage) and measure whether any peripheral capacity remains to absorb the first hours.',
    'If centralization is optimal, part of the measured extraction re-describes as efficient division of labor and epsilon drops; if distributed capacity is irreplaceable, the single-point-of-failure cost is understated by the current score and extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(centralization_optimum_question, empirical, 'Whether the burden shift is a genuine trade or a pure loss.').

omega_variable(
    stratification_trajectory,
    'Is the dual-track equilibrium stable, or does the ceremonial periphery eventually contaminate the core through budget contagion, narrowed recruitment pipelines, and audit cultures that reward paperwork?',
    'Longitudinal tracking of recruitment depth, budget allocation between operational and ceremonial activity, and the correlation between inspection volume and realized defense condition.',
    'A contamination path models a transition in which the arrangement sheds its live function and drifts toward inertial persistence; a regenerative path (core anchoring renewed distributed capacity) models stabilization closer to pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_trajectory, empirical, 'Whether the stratified equilibrium persists or decays.').

omega_variable(
    cs_framing_underdetermination,
    'Should the commitment structure be framed as the institutional arrangement itself (statutes, budget lines, agencies) or as the legitimacy doctrine layered above it (the claim that professional protection substitutes for societal memory)?',
    'Trace what actually adjudicates disputes about preparedness adequacy: if statutory criteria and budget decisions settle them, the institutional frame holds; if appeals to the founding promise of total protection settle them, the doctrinal frame governs.',
    'Under the doctrinal frame, the performative share of the arrangement is attributed to the doctrine rather than the agencies, raising the ceremonial weight of the authority structure and shifting the commitment-system pattern; the institutional frame was chosen because codified statutes and budgetary reality do the day-to-day adjudicating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the same commitment system produce different classification signals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1965, preparedness_retention__hybrid_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement_basis(prep_tr_t1965, observed).
narrative_ontology:measurement(prep_tr_t1975, preparedness_retention__hybrid_reading, theater_ratio, 1975, 0.24).
narrative_ontology:measurement_basis(prep_tr_t1975, observed).
narrative_ontology:measurement(prep_tr_t1985, preparedness_retention__hybrid_reading, theater_ratio, 1985, 0.31).
narrative_ontology:measurement_basis(prep_tr_t1985, observed).
narrative_ontology:measurement(prep_tr_t1995, preparedness_retention__hybrid_reading, theater_ratio, 1995, 0.39).
narrative_ontology:measurement_basis(prep_tr_t1995, observed).
narrative_ontology:measurement(prep_tr_t2005, preparedness_retention__hybrid_reading, theater_ratio, 2005, 0.46).
narrative_ontology:measurement_basis(prep_tr_t2005, observed).
narrative_ontology:measurement(prep_tr_t2015, preparedness_retention__hybrid_reading, theater_ratio, 2015, 0.5).
narrative_ontology:measurement_basis(prep_tr_t2015, observed).
narrative_ontology:measurement(prep_tr_t2025, preparedness_retention__hybrid_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(prep_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t1965, preparedness_retention__hybrid_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement_basis(prep_be_t1965, observed).
narrative_ontology:measurement(prep_be_t1975, preparedness_retention__hybrid_reading, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement_basis(prep_be_t1975, observed).
narrative_ontology:measurement(prep_be_t1985, preparedness_retention__hybrid_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement_basis(prep_be_t1985, observed).
narrative_ontology:measurement(prep_be_t1995, preparedness_retention__hybrid_reading, base_extractiveness, 1995, 0.54).
narrative_ontology:measurement_basis(prep_be_t1995, observed).
narrative_ontology:measurement(prep_be_t2005, preparedness_retention__hybrid_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement_basis(prep_be_t2005, observed).
narrative_ontology:measurement(prep_be_t2015, preparedness_retention__hybrid_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(prep_be_t2015, observed).
narrative_ontology:measurement(prep_be_t2025, preparedness_retention__hybrid_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(prep_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1965, preparedness_retention__hybrid_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement_basis(prep_su_t1965, observed).
narrative_ontology:measurement(prep_su_t1975, preparedness_retention__hybrid_reading, suppression_requirement, 1975, 0.33).
narrative_ontology:measurement_basis(prep_su_t1975, observed).
narrative_ontology:measurement(prep_su_t1985, preparedness_retention__hybrid_reading, suppression_requirement, 1985, 0.36).
narrative_ontology:measurement_basis(prep_su_t1985, observed).
narrative_ontology:measurement(prep_su_t1995, preparedness_retention__hybrid_reading, suppression_requirement, 1995, 0.39).
narrative_ontology:measurement_basis(prep_su_t1995, observed).
narrative_ontology:measurement(prep_su_t2005, preparedness_retention__hybrid_reading, suppression_requirement, 2005, 0.41).
narrative_ontology:measurement_basis(prep_su_t2005, observed).
narrative_ontology:measurement(prep_su_t2015, preparedness_retention__hybrid_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement_basis(prep_su_t2015, observed).
narrative_ontology:measurement(prep_su_t2025, preparedness_retention__hybrid_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(prep_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'preparedness retention' decomposes into three structurally distinct readings of one kernel. The competence reading is the established upstream claim (cited as evidence the system works); the husk reading is the maximal critical claim; this hybrid reading occupies the middle with an intermediate epsilon — its referent is the standing arrangement, not any reading's endorsed alternative. Family members are linked pairwise via affects_constraints so purity degradation propagates visibly across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
