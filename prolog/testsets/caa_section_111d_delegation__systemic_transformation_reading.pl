% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: EPA Section 111(d) 'Best System' Authority for Systemic Grid Decarbonization
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act authorizes EPA to establish 'best
 *   system of emission reduction' standards for existing stationary sources.
 *   The statute is contested between two coherent but logically incompatible
 *   readings: one interprets 'best system' as grid-wide, generation-shifting
 *   strategies permitting coal retirement and renewable substitution
 *   (systemic transformation reading); the other interprets 'best system' as
 *   facility-level, pollutant-reduction measures within generator control
 *   only (facility constraint reading). This constraint story instantiates
 *   the systemic transformation reading — the interpretation under which EPA
 *   authority reaches generation portfolio composition, fuel switching, and
 *   state-level decarbonization pathways. Under this reading, the constraint
 *   creates a tangled rope structure: genuine coordination function (federal
 *   authority solves interstate carbon spillover problem and collective
 *   action failure), combined with asymmetric extraction (fossil-locked
 *   sectors bear disproportionate costs, stranded asset liability, and
 *   suppressed exit options). The extraction is real and significant but not
 *   total — organized fossil fuel sectors retain substantial negotiating
 *   power, can challenge the authority in court, and can lobby for
 *   exemptions. The constraint exhibits rising extractiveness and suppression
 *   over the measurement interval (2020–2026), as EPA interpretation hardens
 *   and litigation challenges fail, moving the equilibrium from ambiguous
 *   authority to enforced mandate.
 *
 * KEY AGENTS:
 *   - EPA Administrative Authority (institutional/arbitrage) — interprets and enforces 'best system' standard; benefits from expanded delegated authority; primary beneficiary
 *   - Renewable Energy Sectors (institutional/arbitrage) — benefit from regulatory demand creation and subsidized substitution pathways; primary beneficiary
 *   - Coal Generators and Fossil Fuel Sector (organized/constrained) — face regulatory mandate to decarbonize; bear stranded asset costs; primary victim with substantial agency
 *   - Coal-Dependent Communities (powerless/trapped) — structurally dependent on coal employment and revenue; face economic collapse without transition assistance; powerless victims with no exit
 *   - Fossil-Locked States (powerful/constrained) — constitutionally obligated to implement federal standards; can negotiate implementation pathways but cannot exit; constrained powerful actors
 *   - Analytical Observer (analytical/analytical) — civilizational perspective risks naturalizing contested legal interpretation as immutable regulatory requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.58).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.68).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "EPA Section 111(d) 'Best System' Authority for Systemic Grid Decarbonization").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '05579cb9-2b40-4010-99ae-2b7d67aa4be4').
narrative_ontology:cs_kernel_codification('05579cb9-2b40-4010-99ae-2b7d67aa4be4', fixed_text).
narrative_ontology:cs_authority_grounding('05579cb9-2b40-4010-99ae-2b7d67aa4be4', extraction).
narrative_ontology:cs_interpretation_layer_present('05579cb9-2b40-4010-99ae-2b7d67aa4be4').
narrative_ontology:cs_reading_relation('05579cb9-2b40-4010-99ae-2b7d67aa4be4', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('05579cb9-2b40-4010-99ae-2b7d67aa4be4', foundational, generation_portfolio_within_best_system_authority).
narrative_ontology:cs_axiom_status(generation_portfolio_within_best_system_authority, holdable).
narrative_ontology:cs_axiom_grounding('05579cb9-2b40-4010-99ae-2b7d67aa4be4', generation_portfolio_within_best_system_authority, empirically_contingent).
narrative_ontology:cs_axiom('05579cb9-2b40-4010-99ae-2b7d67aa4be4', foundational, interstate_carbon_spillover_coordination_necessity).
narrative_ontology:cs_axiom_status(interstate_carbon_spillover_coordination_necessity, holdable).
narrative_ontology:cs_axiom_grounding('05579cb9-2b40-4010-99ae-2b7d67aa4be4', interstate_carbon_spillover_coordination_necessity, deontological).
narrative_ontology:cs_reference_frame('05579cb9-2b40-4010-99ae-2b7d67aa4be4', federal_environmental_authority_for_systemic_grid_transformation).
narrative_ontology:cs_drift_state('05579cb9-2b40-4010-99ae-2b7d67aa4be4', contemporary_major_questions_doctrine_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('05579cb9-2b40-4010-99ae-2b7d67aa4be4', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sectors).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, climate_mitigation_constituencies).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, epa_institutional_authority).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_generators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, stranded_asset_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL-DEPENDENT COMMUNITIES (SNARE) — Structurally trapped by decades of infrastructure investment, workforce specialization, and revenue dependence. Exit from coal economy carries massive unemployment, pension liability, and economic collapse risk with no coordinated transition pathway. Regulation forces rapid asset devaluation without compensation. Maximum experienced extraction.
constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FOSSIL FUEL GENERATORS (TANGLED ROPE) — Organized, institutional actors with substantial resources and political leverage. Benefit from existing grid infrastructure and regulatory capture positions; face regulatory pressure to decarbonize and stranded asset costs. High extraction but also substantial agency — can negotiate timelines, invest in gas transition, lobby for exemptions. Suppression is high (regulatory mandate) but not absolute (legal challenge pathways, state-level resistance).
constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY SECTORS AND CLIMATE CONSTITUENCIES (ROPE) — Primary beneficiaries. Regulation creates demand for renewable substitution, opens subsidized deployment pathways, and establishes regulatory authorization for investment. Low or negative extraction because the constraint routes benefits toward these actors. Arbitrage exit options (can shift from one compliance pathway to another, geographic arbitrage across state lines). Genuine coordination function: the 'best system' mandate solves a collective action problem — states and utilities cannot credibly commit to decarbonization without federal enforcement.
constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EPA ADMINISTRATIVE APPARATUS (PITON) — The regulatory authority sees Section 111(d) as a tool for solving a market failure (carbon externality). The institutional frame is that EPA is coordinating rational decarbonization via technical standards. However, the theater ratio (0.42) reflects that much EPA activity is procedural performance: rule-making theater, comment periods, environmental review rituals that delay but do not prevent the underlying mandate. The constraint persists through institutional inertia and statutory obligation, not because it solves the climate problem more effectively than alternatives (carbon tax, cap-and-trade with lower administrative overhead). Degraded because the primary coordination mechanism (federal standard-setting) is less efficient than price-based approaches.
constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FOSSIL-LOCKED STATES (TANGLED ROPE) — Powerful actors (state governments) with institutional capacity and political legitimacy but constrained by constitutional limits on intrastate commerce power and federal preemption. Benefit from existing coal-to-electricity system; bear costs of rapid decarbonization mandate. Can negotiate compliance pathways, delay through litigation, or coordinate to change federal law, but face suppression mechanism (EPA authority under the Clean Air Act) that prevents exit. Moderate experienced extraction because states have agency and can partially accommodate preferences within federal framework.
constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long-term structural perspective, federal environmental regulation is an immutable response to market failure and interstate spillovers (pollution does not respect state boundaries). The 'best system' authority is a natural institutional response to collective action problems that states cannot solve unilaterally. This perspective risks naturalizing what is actually a contested legal interpretation. The engine will flag this as a false summit: the mountain classification treats the delegation doctrine as a law of nature, when in fact it is a coherent but contested constitutional reading.
constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(caa_section_111d_delegation__systemic_transformation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, TR),
    TR >= 0.70.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint redistributes substantial value from fossil fuel generators toward renewable energy sectors and climate mitigation constituencies. The EPA interpretation expands agency authority beyond what the facility-constraint reading would permit, unlocking new regulatory pathways. However, extractiveness is not maximal (0.70+) because the constraint has genuine coordination function — interstate carbon spillovers create authentic collective action failure that unilateral state action cannot solve. The coordinate rises from 0.32 (initial ambiguity about EPA authority) to 0.58 (enforced interpretation) as litigation challenges fail and the mandate hardens. Suppression (0.68): High. The mechanism is statutory delegation, administrative procedure, and federal preemption doctrine — fossil fuel sectors cannot legally exit federal regulation, can only negotiate implementation details. Coal-dependent communities face exit costs in the billions (workforce retraining, pension liability, regional economic collapse). The suppression is enforced through regulatory authority, not physical coercion, but the barriers are nearly absolute. Theater ratio (0.42): Moderate-low. The constraint has lower theater than many regulatory regimes because the underlying mechanism is straightforward: EPA sets standards, states/utilities implement, generation portfolios shift. Environmental review processes and administrative procedures do create some theater (comment periods, impact assessments), but the core mandate is directly functional — it causally reshapes generation investment. Theater increases slightly over the interval as litigation challenges extend timelines and procedural requirements accumulate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies a core mandatrophy problem: the two readings of Section 111(d) produce opposite classifications from the same structural position. Under the facility-constraint reading, EPA authority is narrowly bounded to facility-level efficiency — the constraint appears as rope (coordination within clear statutory limits) or even mountain (immutable facility physics). Under the systemic-transformation reading, EPA authority extends to grid-wide generation portfolios — the constraint appears as tangled rope (coordination + extraction) or snare (from coal community perspective). The perspectival gap is not between agents at different power positions; it is between coherent but incompatible legal readings. Resolving this gap requires Supreme Court interpretation of the statute, not interpersonal negotiation. The constraint's classification is therefore radically contingent on a single institutional decision point.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) follows from beneficiary/victim declarations and exit options. Renewable energy sectors and EPA derive low d (beneficiaries with arbitrage options) → negative or near-zero effective extraction. Fossil fuel generators derive moderate d (victims with substantial but finite exit costs and negotiating power) → moderate effective extraction. Coal-dependent communities derive high d (victims trapped by economic dependency) → maximum effective extraction. States derive high-moderate d (victims with constitutional constraints limiting exit) → high-moderate extraction. The powerless coal communities perspective shows maximum d (trapped exit + victim status) yielding snare classification. The organized fossil sector perspective shows moderate d (constrained exit + victim status) yielding tangled rope. The renewable/EPA perspective shows low d (arbitrage exit + beneficiary status) yielding rope. The piton classification for EPA reflects not low extraction chi but high theater ratio relative to functional necessity — the administrative apparatus sees itself as executing a mandate, not recognizing that alternative price-based mechanisms might achieve the same outcome with lower theater.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates the mandatrophy precisely: the systemic transformation interpretation of Section 111(d) must satisfy both gates simultaneously. (1) GENUINE COORDINATION FUNCTION: Federal authority does solve a real collective action problem — individual states cannot tax carbon credibly because firms relocate; unilateral decarbonization by one state increases grid costs for all consumers. The constraint coordinates a solution that states acting alone cannot achieve. (2) ASYMMETRIC EXTRACTION: The costs of decarbonization are concentrated on coal-dependent regions and stranded asset holders, while benefits (climate risk reduction, renewable investment opportunities) are geographically dispersed and temporally distant. No compensatory mechanism ensures that extraction is symmetric. The constraint satisfies BOTH conditions, confirming tangled rope classification. The false summit risk is the analytical perspective that naturalizes this constraint as an immutable response to physical climate limits, when in fact it is a contested legal interpretation of delegated administrative authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegated_authority_scope_ambiguity,
    'Does ''best system of emission reduction'' authorize EPA to mandate grid-wide generation substitution (coal→gas→renewables), or only generation-unit-level efficiency improvements within facility control?',
    'Supreme Court interpretation of ''system'' in statutory context; analysis of prior EPA interpretations under Section 111 for other sectors; legislative history and statutory purpose examination',
    'Systemic interpretation: tantalized_rope classification confirmed, high extraction visible, coal sector becomes victim. Unit-level interpretation: constraint becomes rope (coordination only), extraction becomes negligible, coal sector not a victim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegated_authority_scope_ambiguity, conceptual, 'Scope of ''best system'' authority: systemic generation substitution vs facility-level efficiency only').

omega_variable(
    delegation_doctrine_constitutional_vulnerability,
    'Is the delegated authority constitutionally sound under major questions doctrine (Dobbs, NIFLA, Navajo Nation precedent), or does the breadth of EPA discretion in defining ''best system'' exceed nondelegation limits?',
    'Supreme Court ruling on the specific delegation; major questions doctrine application; analysis of statutory standards and EPA interpretive guidelines',
    'If constitutional: constraint holds as classified. If major questions doctrine invalidates: constraint collapses to rope (coordination via clearer statutory boundaries) or vanishes entirely (replaced by legislative mandate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_doctrine_constitutional_vulnerability, conceptual, 'Constitutional validity of EPA''s delegated authority under major questions doctrine').

omega_variable(
    stranded_asset_compensation_gap,
    'Does the ''best system'' mandate trigger any compensatory obligation to coal-dependent communities and stranded asset holders, or is extraction one-sided?',
    'Legislative amendments providing transition assistance; regulatory mitigation requirements; case law on property rights and regulatory takings; state-level just transition programs',
    'If compensation exists: suppression decreases (exit cost lowered), extraction decreases, classification toward rope. If uncompensated: suppression holds, victims bear full cost, snare classification for victims confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_compensation_gap, empirical, 'Presence or absence of compensatory mechanisms for stranded coal assets').

omega_variable(
    alternative_grid_decarbonization_pathways,
    'Could grid decarbonization be achieved via price-based mechanisms (carbon tax, cap-and-trade) with lower administrative overhead and more even cost distribution?',
    'Comparative analysis of regulatory vs market-based decarbonization costs; implementation timelines; distributional impacts across regions and sectors; learning from EU ETS and other price-based regimes',
    'If price mechanisms superior: current regulation is theater (piton classification confirmed). If price mechanisms blocked by political economy: regulation is necessary coordination (rope reclassification). If hybrid optimal: tangled_rope confirmed with lower theater than administrative-only approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_grid_decarbonization_pathways, empirical, 'Relative efficiency and distributional impact of price-based vs regulatory decarbonization pathways').

omega_variable(
    reading_vs_facility_constraint_distinction,
    'This reading (systemic transformation authority) forecloses or coexists with the facility-constraint reading (EPA limited to unit-level efficiency mandates)?',
    'Conceptual clarification: the two readings instantiate different interpretations of the same statutory text. Systemic reading: ''best system'' includes grid-wide generation substitution pathways. Facility reading: ''best system'' limited to equipment-level, pollutant-reduction measures within the facility itself. These are NOT compatible within a single framework — they assign opposite meaning to the same statutory language.',
    'If systemic reading is judicially adopted: facility reading is foreclosed (same-statute interpretation excludes it). If facility reading prevails: systemic reading is foreclosed. Coexistence would require the statute to mean both things simultaneously, which is incoherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_facility_constraint_distinction, conceptual, 'Logical relationship between systemic transformation reading and facility-constraint reading of Section 111(d)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa111d_systrans_tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(caa111d_systrans_tr_t3, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement(caa111d_systrans_tr_t6, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 6, 0.42).

% Extraction over time
narrative_ontology:measurement(caa111d_systrans_be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(caa111d_systrans_be_t3, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(caa111d_systrans_be_t6, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(caa111d_systrans_su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(caa111d_systrans_su_t3, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(caa111d_systrans_su_t6, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, coal_stranded_assets_and_transition_costs).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, state_climate_authority_preemption).

% DUAL FORMULATION NOTE:
% The systemic transformation reading affects the facility constraint reading by foreclosing it (same statutory text cannot mean both simultaneously). The constraint network also affects stranded asset compensation regimes (structural interdependence: if compensation pathways are created, suppression decreases and classification shifts toward rope) and state climate authority questions (federal preemption reduces state alternatives, constraining powerful actors).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
