% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause - Narrow Originalist Reading (Trade-Crossing-Lines Limit)
 *   domain: legal/constitutional_federalism
 *
 * SUMMARY:
 *   The narrow originalist reading instantiates the commerce_clause_scope
 *   kernel as a strict jurisdictional boundary: federal commercial power
 *   reaches only trade that crosses state lines, exercised to make such trade
 *   regular - clearing state-imposed barriers and securing uniform rules for
 *   interstate dealing - and nothing else. Manufacturing, agriculture,
 *   mining, labor conditions, and social life inside a single state lie
 *   beyond national reach however large their aggregate footprint. This file
 *   authors that reading alone as an epsilon-invariant constraint; the
 *   sibling readings (broad_effects_test, intermediate_channels) are separate
 *   files with different victim sets and different epsilon values. Family
 *   note: the colloquial label 'the Commerce Clause' decomposes into these
 *   three structurally distinct allocations of power, linked by
 *   network.affects_constraints. The narrow reading is historically upstream
 *   - its ascendant span (roughly 1895 through 1937, from E.C. Knight through
 *   Carter Coal) collapsed in the 1937 reversal, enabling the broad reading's
 *   subsequent reign; the upstream claim's textual conservatism was cited as
 *   evidence against the downstream expansions. The metrics below describe
 *   the narrow reading's operation during its ascendant span: its
 *   extractiveness reflects what the boundary cost those denied national
 *   protection, not the states it shields. The claim/metric relationship is
 *   deliberately unreconciled: the type is authored from structural analysis,
 *   the metrics from the historical record of operation.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/trapped) - retains exclusive jurisdiction over intrastate economic and social life; cannot exit the union
 *   - intrastate_local_businesses: Secondary beneficiary (moderate/mobile) - shielded from national wage, licensing, and environmental regimes; relocatable at cost
 *   - interstate_carriers_and_traders: Secondary beneficiary (powerful/arbitrage) - the clause's original constituency; barrier clearance and uniform rules are their market access
 *   - federal_judiciary: Agenda-setter (institutional/constrained) - administers the boundary through judicial review; draws the line, strikes the statutes
 *   - federal_legislature: Payer (institutional/constrained) - passes national statutes and watches the local-activity portions struck; Article V escape nearly closed
 *   - national_civil_rights_claimants: Primary victim (powerless/constrained) - denied national statutory remedy inside recalcitrant states
 *   - intrastate_workers: Victim (powerless/constrained) - no national floor for hours, wages, child labor, or organizing in local enterprises
 *   - pollution_exposed_communities: Victim (powerless/trapped) - anchored towns facing intrastate sources beyond national environmental standards
 *   - state_residents_preferring_national_standards: Excluded voice (organized/constrained) - favor national floors that appear on no ballot
 *   - constitutional_scholars: Analytical observer (analytical/analytical) - reconstruct founding usage and argue the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.72).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.62).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause - Narrow Originalist Reading (Trade-Crossing-Lines Limit)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "legal/constitutional_federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '4690e87f-6f1f-41ac-84e6-773791bc3ddc').
narrative_ontology:cs_kernel_codification('4690e87f-6f1f-41ac-84e6-773791bc3ddc', fixed_text).
narrative_ontology:cs_authority_grounding('4690e87f-6f1f-41ac-84e6-773791bc3ddc', lineage).
narrative_ontology:cs_interpretation_layer_present('4690e87f-6f1f-41ac-84e6-773791bc3ddc').
narrative_ontology:cs_reading_relation('4690e87f-6f1f-41ac-84e6-773791bc3ddc', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('4690e87f-6f1f-41ac-84e6-773791bc3ddc', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('4690e87f-6f1f-41ac-84e6-773791bc3ddc', foundational, regulate_means_make_regular).
narrative_ontology:cs_axiom_status(regulate_means_make_regular, holdable).
narrative_ontology:cs_axiom_grounding('4690e87f-6f1f-41ac-84e6-773791bc3ddc', regulate_means_make_regular, empirically_contingent).
narrative_ontology:cs_axiom('4690e87f-6f1f-41ac-84e6-773791bc3ddc', foundational, commerce_means_crossing_state_lines).
narrative_ontology:cs_axiom_status(commerce_means_crossing_state_lines, holdable).
narrative_ontology:cs_axiom_grounding('4690e87f-6f1f-41ac-84e6-773791bc3ddc', commerce_means_crossing_state_lines, empirically_contingent).
narrative_ontology:cs_axiom('4690e87f-6f1f-41ac-84e6-773791bc3ddc', secondary, no_general_federal_police_power).
narrative_ontology:cs_axiom_status(no_general_federal_police_power, holdable).
narrative_ontology:cs_axiom_grounding('4690e87f-6f1f-41ac-84e6-773791bc3ddc', no_general_federal_police_power, conventional).
narrative_ontology:cs_reference_frame('4690e87f-6f1f-41ac-84e6-773791bc3ddc', founding_era_enumerated_powers_compact).
narrative_ontology:cs_drift_state('4690e87f-6f1f-41ac-84e6-773791bc3ddc', new_deal_constitutional_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4690e87f-6f1f-41ac-84e6-773791bc3ddc', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, intrastate_local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, interstate_carriers_and_traders).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_civil_rights_claimants).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, intrastate_workers).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, pollution_exposed_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_legislature).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, dual_federalism).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, state_police_power_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sovereign polities retaining exclusive jurisdiction over manufacturing, agriculture, labor, morals, and land use inside their borders. The arrangement guarantees that national legislation cannot reach these domains; they answer politically only to their own electorates. They cannot leave the union - secession was foreclosed by war and doctrine - so their protection is total and their exit nil. They remain subject to the arrangement's other face: state taxes and rules that discriminate against goods merely passing through to other states can be struck down.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, trapped, regional).

% Firms operating wholly within one state - local manufacturers, farms, neighborhood lenders. They stand outside national wage-hour, licensing, and environmental regimes that attach only to interstate activity. If their home state regulates them heavily they can relocate across a state line at real cost, which gives them leverage with state legislators and keeps state regimes competitively light.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, intrastate_local_businesses, beneficiary,
    moderate, biographical, mobile, local).

% Railroads, national merchants, and commodity dealers whose business is moving goods across state lines. The arrangement's barrier-clearing function is theirs: no state may tariff, delay, or discriminate against their traffic, and the rules they deal under are uniform. They were the clause's original constituency and remain its most reliable defenders; their operations span enough jurisdictions that no single state can hold them up.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, interstate_carriers_and_traders, beneficiary,
    powerful, generational, arbitrage, continental).

% The courts administer the boundary: defining what counts as trade crossing state lines, drawing the direct/indirect line, and invalidating national statutes that overshoot. Life tenure insulates the line-drawers from the electoral backlash their rulings provoke, but appointments are political and each invalidated statute spends legitimacy the institution cannot mint cheaply. They cannot delegate the drawing or decline the cases.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Passes national statutes addressing labor, civil rights, and industrial conditions, then watches the courts strike the portions resting on local activity. Its responses are limited: redraft narrower versions, propose constitutional amendments that almost never clear Article V, or wait for appointments to change the bench. Each cycle consumes a session's agenda and returns the problem unsolved.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_legislature, payer,
    institutional, biographical, constrained, national).

% People facing exclusion, segregation, or violence in lodging, entertainment, and employment inside a single state. National statutes forbidding such treatment exceed the federal reach under this arrangement, so their remedies run through the very state courts and legislatures that sustain the treatment. Leaving means abandoning home, kin, and livelihood; staying means petitioning authorities aligned with the harm.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_civil_rights_claimants, payer,
    powerless, generational, constrained, national).

% Workers in purely local enterprises - mills, mines, canneries serving one state's market. Hours, wages, child labor, and organizing rights have no national floor; protection depends entirely on each state's willingness. Skilled workers can move toward protective states; the youngest and poorest, whose labor the arrangement leaves most exposed, bear the highest moving costs and the least mobility.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, intrastate_workers, payer,
    powerless, biographical, constrained, regional).

% Towns downstream, downwind, or downgradient of industrial operations whose market is local. Because the source's activity counts as local manufacture, national environmental standards cannot attach; relief must come from state agencies that share budget and personnel with the industry. Property, family, and congregation roots anchor residents who might otherwise leave.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, pollution_exposed_communities, payer,
    powerless, generational, trapped, local).

% Majorities in opinion surveys repeatedly favor national minimum standards on wages, discrimination, and air quality, but the boundary that withholds those standards appears on no ballot anywhere. Their preference can influence who sits in Congress, yet Congress cannot act on the withheld subjects; the objection has no forum, so it surfaces only in briefs and correspondence.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_residents_preferring_national_standards, excluded,
    organized, biographical, constrained, national).

% Historians and law professors who reconstruct founding-era usage, trace the doctrine's arc, and argue the competing readings before audiences with no vote. Their stake is reputational and intellectual; they observe the boundary's operation without standing inside it.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the Articles-of-Confederation collective-action problem: states left free to tax, tariff, and burden one another's goods had crippled the national market. The arrangement commits one authority - the national government - to keeping trade crossing state lines regular: striking state-imposed barriers, preempting conflicting state commercial rules, and guaranteeing uniform dealing rules for interstate traffic. It also fixes a stable boundary between two sovereignties so neither swallows the other.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction over intrastate economic and social life from the national government to the states; moves the cost of foregone national protection onto people subject to hostile or lax state regimes; moves compliance savings to intrastate firms; and moves boundary-drawing prestige to whichever bench holds the pen.
% ABSENT_VOICES: Those denied national protection - civil-rights claimants, child laborers, pollution-exposed towns - enter the record only after harm, as litigants attacking the boundary they live under. State residents who favor national standards have no forum: the boundary appears on no ballot. Future generations inherit the allocation without voice. The unanimity behind the founding settlement arose in rooms - Philadelphia and the ratifying conventions - from which enslaved people, women, and the unpropertied were absent.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, Congress would legislate national wage, hour, civil-rights, and environmental floors within months; state regulatory autonomy would contract to administering national baselines; intrastate firms would absorb federal compliance costs; and the courts would lose their boundary-drawing docket. The federal system would reorganize around plenary national power - the precise outcome the arrangement exists to prevent.
% FOUNDING_PROBLEM: Under the Articles of Confederation each state erected tariffs, duties, and discriminatory burdens against its neighbors' goods; thirteen mercantilisms strangled the common market. The clause was written to give one authority power to keep commerce among the states regular - trade flowing without internal tolls - and to speak with uniform rules where dealing crosses lines.
% FOUNDING_PROBLEM_CORROBORATION: External attestation for the problem's historicity is strong: Federalist Nos. 22 and 42 describe state commercial warfare as a chief failure of the Articles; the ratification debates return to it repeatedly; Gibbons v. Ogden (1824) reads the clause as securing free interchange and uniform regulation. None of these voices belongs to today's beneficiary set. Whether the problem remains live is disputed: originalist scholars attest a permanent underlying problem (holding national power bounded), while legal historians writing outside the state-autonomy camp contend the trade-barrier problem died with the national market and the arrangement now chiefly shelters state establishments.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72 at interval end) tracks the widening gap between national problems and the tools the boundary permits: in 1895 confining the antitrust law cost little; by 1936 voiding the national coal and recovery statutes left depression conditions beyond federal touch. Suppression (0.62) measures the foreclosure of statutory alternatives plus the counter-majoritarian force needed to hold the line against successive Congresses - the series rises as each invalidated statute is more popular than the last, ending at the court-packing confrontation. Theater (0.48) traces the direct/indirect distinction's decay from a good-faith (if confused) instrument into formalist performance - the stream-of-commerce and direct/indirect contortions of the mid-1930s - approaching the Goodhart threshold at interval end. Accessibility collapse is low (0.35): the rival readings never vanished - the broad construction was articulated from ratification onward and stayed intellectually available, which is why the boundary fell when politics turned. Resistance is high (0.68): Progressive-era campaigns, the child-labor amendment drive, and the New Deal confrontation. Coalition note: the powerless victim seats were not individually mobile, but the New Deal coalition assembled them into a bloc strong enough to break the boundary - coalition power, not individual exit, is what moved it. All three series share one seven-point grid (t=0..30, indexing the ascendant era 1895-1937, compressed non-linearly: t0 approximates E.C. Knight, t10 Hammer v. Dagenhart, t25 Schechter and Carter Coal, t30 the 1937 reversal); every tracked metric is authored at every shared point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From state capitols the boundary is protective coordination: a guarantee that distant majorities cannot rewrite local life. From the segregated lodging house and the cannery floor it is abandonment: the same line that guards state autonomy strands the unprotected inside hostile state regimes. The bench experiences the arrangement as neutral line-drawing without a material stake; the legislature experiences it as a straitjacket on its agenda. Same nominal institutional level, different exits: interstate traders hold arbitrage-grade exit (operations spanning many jurisdictions, none decisive against them), intrastate workers hold constrained exit (mobility priced beyond the poorest), anchored mill towns hold effectively none. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared roles drive the derivation. State governments, intrastate firms, and interstate traders sit at the beneficiary pole (d near 0.0): the arrangement subsidizes their autonomy and market access, and their declarations feed low derived directionality. Civil-rights claimants, intrastate workers, and pollution-exposed communities sit at the target pole (d near 1.0): the arrangement's entire operation consists of withholding national remedy from them, and their exit options (constrained down to trapped) pin them near the full-target end. The judiciary, as agenda-setter, derives near-symmetric with a slight institutional gain - striking statutes enlarges judicial power relative to deference, but the seat collects no material rent. No directionality overrides are used: the declared beneficiary/victim structure plus differentiated exit options already separate the seats, and the two institutional actors (legislature as payer, judiciary as agenda-setter) occupy different roles despite sharing a power atom, so a power-atom-keyed override would misfire on one of them. Suppression is authored as a raw structural property and enters the computation unscaled; only extractiveness is scaled, by directionality and spatial scope, in the engine's arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - state trade barriers - is largely dead: states no longer tariff one another, and the barrier-clearing function survives mainly as dormant-commerce review that every reading of the kernel accepts. What persists is a second function the founders did not primarily intend: shielding state regulatory autonomy from national revision. Classifying the arrangement as tangled_rope keeps both faces visible - calling it pure coordination erases the stranded victim seats; calling it pure extraction erases the real common-market function that made the union economically viable. It is not a piton: enforcement is real rather than theatrical maintenance, and the beneficiary seats profit enough to defend the arrangement actively. The rising theater_ratio marks the danger this classification watches: if the barrier function finishes dying while formalist distinction-drawing continues, the structure drifts toward inertial maintenance - the temporal series exists to date that transition if it arrives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the commerce_clause_scope kernel - what changes structurally if a sibling reading (broad_effects_test or intermediate_channels) governs instead?',
    'Adoption of a sibling reading by the interpreting authority (court-composition shift, Article V amendment, doctrinal overruling); compare victim and beneficiary sets across the linked family files.',
    'Under broad_effects_test the victim set expands to everyone subject to aggregate-effects regulation and state autonomy contracts sharply; under intermediate_channels victims shrink to non-economic activity lacking a jurisdictional element; the epsilon and victim structure authored here apply only to the narrow reading''s operative span.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story instantiates the narrow_originalist reading of a three-reading kernel.').

omega_variable(
    original_meaning_or_constructed_shield,
    'Is the narrow scope a discovery of the Constitution''s fixed public meaning, or a constructed rule whose practical effect concentrates regulatory autonomy in state political establishments?',
    'Founding-era corpus linguistics and ratification-debate analysis, combined with a comparative study of who historically invoked the narrow reading - states defending local hierarchies versus traders seeking open national markets.',
    'If primarily a constructed shield, the coordination story weakens and the structure drifts toward pure extraction; if genuine discovered meaning, the coordination function is authentic and the hybrid classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_or_constructed_shield, conceptual, 'Whether the reading''s scope is textual discovery or interest-serving construction.').

omega_variable(
    recalcitrant_state_variance,
    'How much the narrow reading costs the unprotected classes depends on how far state regimes actually diverge - do states voluntarily converge on protective labor, civil-rights, and environmental standards?',
    'Cross-state comparison of regulatory floors during the reading''s operative span; natural experiments where states adopted protections ahead of any national law.',
    'High convergence shrinks the victim seats'' effective burden and pulls the structure toward pure coordination; persistent divergence (the documented record of the ascendant era) sustains high extraction and supports the hybrid or worse classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recalcitrant_state_variance, empirical, 'State-regime divergence as the determinant of the victim set''s realized costs.').

omega_variable(
    revival_viability,
    'Can the narrow reading regain operative control of the doctrine, or is its ascendant era permanently closed?',
    'Track Supreme Court composition and commerce-clause holdings for partial-revival signals (the post-1995 line of cases), and monitor Article V activity.',
    'Partial revival reopens the victim and beneficiary structure modeled here as live law; permanent closure converts this story into historical record and settles the drift_state toward completed repudiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_viability, empirical, 'Whether the reading''s reference frame can be reconstructed or remains a minority position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commerce_narrow_originalist_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(commerce_narrow_originalist_tr_t0, observed).
narrative_ontology:measurement(commerce_narrow_originalist_tr_t5, commerce_clause_scope__narrow_originalist, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(commerce_narrow_originalist_tr_t5, observed).
narrative_ontology:measurement(commerce_narrow_originalist_tr_t10, commerce_clause_scope__narrow_originalist, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(commerce_narrow_originalist_tr_t10, observed).
narrative_ontology:measurement(commerce_narrow_originalist_tr_t15, commerce_clause_scope__narrow_originalist, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(commerce_narrow_originalist_tr_t15, observed).
narrative_ontology:measurement(commerce_narrow_originalist_tr_t20, commerce_clause_scope__narrow_originalist, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(commerce_narrow_originalist_tr_t20, observed).
narrative_ontology:measurement(commerce_narrow_originalist_tr_t25, commerce_clause_scope__narrow_originalist, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(commerce_narrow_originalist_tr_t25, observed).
narrative_ontology:measurement(commerce_narrow_originalist_tr_t30, commerce_clause_scope__narrow_originalist, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(commerce_narrow_originalist_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(commerce_narrow_originalist_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(commerce_narrow_originalist_be_t0, observed).
narrative_ontology:measurement(commerce_narrow_originalist_be_t5, commerce_clause_scope__narrow_originalist, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(commerce_narrow_originalist_be_t5, observed).
narrative_ontology:measurement(commerce_narrow_originalist_be_t10, commerce_clause_scope__narrow_originalist, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(commerce_narrow_originalist_be_t10, observed).
narrative_ontology:measurement(commerce_narrow_originalist_be_t15, commerce_clause_scope__narrow_originalist, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(commerce_narrow_originalist_be_t15, observed).
narrative_ontology:measurement(commerce_narrow_originalist_be_t20, commerce_clause_scope__narrow_originalist, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(commerce_narrow_originalist_be_t20, observed).
narrative_ontology:measurement(commerce_narrow_originalist_be_t25, commerce_clause_scope__narrow_originalist, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(commerce_narrow_originalist_be_t25, observed).
narrative_ontology:measurement(commerce_narrow_originalist_be_t30, commerce_clause_scope__narrow_originalist, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(commerce_narrow_originalist_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(commerce_narrow_originalist_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(commerce_narrow_originalist_su_t0, observed).
narrative_ontology:measurement(commerce_narrow_originalist_su_t5, commerce_clause_scope__narrow_originalist, suppression_requirement, 5, 0.34).
narrative_ontology:measurement_basis(commerce_narrow_originalist_su_t5, observed).
narrative_ontology:measurement(commerce_narrow_originalist_su_t10, commerce_clause_scope__narrow_originalist, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(commerce_narrow_originalist_su_t10, observed).
narrative_ontology:measurement(commerce_narrow_originalist_su_t15, commerce_clause_scope__narrow_originalist, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(commerce_narrow_originalist_su_t15, observed).
narrative_ontology:measurement(commerce_narrow_originalist_su_t20, commerce_clause_scope__narrow_originalist, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(commerce_narrow_originalist_su_t20, observed).
narrative_ontology:measurement(commerce_narrow_originalist_su_t25, commerce_clause_scope__narrow_originalist, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(commerce_narrow_originalist_su_t25, observed).
narrative_ontology:measurement(commerce_narrow_originalist_su_t30, commerce_clause_scope__narrow_originalist, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(commerce_narrow_originalist_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Commerce Clause' covers three structurally distinct allocations of federal power and is decomposed per the epsilon-invariance principle into three linked stories. This file (narrow_originalist) is the historically upstream member: its textual conservatism was cited as evidence against the downstream expansions, and its 1937 collapse is the event that admitted the broad reading's reign. Each member carries its own epsilon, victim set, and classification; the edges propagate contamination analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
