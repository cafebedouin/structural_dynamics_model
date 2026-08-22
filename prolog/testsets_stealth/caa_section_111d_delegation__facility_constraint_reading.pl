% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: Clean Air Act Section 111(d) 'Best System' — Facility-Bound Reading (Fenceline Constraint)
 *   domain: administrative law/environmental regulation/constitutional interpretation
 *
 * SUMMARY:
 *   Section 111(d) directs EPA to set existing-source performance standards
 *   based on the 'best system of emission reduction' that has been
 *   'adequately demonstrated.' Two readings contest the term's reach. This
 *   story instantiates the facility-bound reading as a standing arrangement:
 *   EPA's authority is confined to measures implementable at individual
 *   covered facilities — heat-rate improvement, carbon capture retrofit — and
 *   cannot reach grid-wide generation-shifting or forced fleet retirement.
 *   The arrangement is enforced judicially (the major-questions settlement
 *   entrenched it), and its distributive work is real: the coal fleet is
 *   shielded, states keep energy-mix control, and the ceiling's costs land on
 *   emissions-exposed populations, downwind states, a renewables sector
 *   barred from the compliance calculus, and future generations. EPS
 *   REFERENT: the standing facility-bound arrangement itself — the regime
 *   this reading defends — with values authored from this reading's seat
 *   while describing the arrangement's actual operation, including ceiling
 *   costs this reading's own lights would prefer to describe as mere
 *   restraint. CLAIM/METRIC INDEPENDENCE: claimed_type (tangled_rope) is my
 *   structural judgment — genuine federal-state coordination function plus
 *   asymmetric ceiling extraction requiring active judicial enforcement; the
 *   metrics are my descriptive judgment of how the arrangement actually
 *   operates. Where the engine's per-seat computations diverge from either,
 *   that divergence is the datum. FAMILY NOTE: this file is one member of the
 *   caa_section_111d_delegation kernel family; the sibling story
 *   (systemic_transformation_reading) instantiates the competing reading with
 *   its own epsilon, victim set, and enforcement history.
 *
 * KEY AGENTS:
 *   - - environmental_protection_agency: Regulated agenda-setter (institutional/constrained) — administers Section 111(d) inside a fenceline boundary it did not choose
 *   - - federal_appellate_courts: Enforcing agenda-setter (institutional/analytical) — police the ceiling through major-questions review
 *   - - coal_plant_operators: Primary beneficiary (powerful/constrained) — shielded from retirement-forcing standards; bear retrofit costs when standards reach them
 *   - - coal_dependent_state_governments: Secondary beneficiary (institutional/constrained) — retain energy-mix authority; fiscally tied to coal
 *   - - fossil_generation_investors: Beneficiary (powerful/arbitrage) — avoid stranded-asset losses via the ceiling
 *   - - climate_vulnerable_communities: Primary target (powerless/trapped) — bear foregone-abatement costs with no seat in the contest
 *   - - downwind_pollution_bearing_states: Organized target (organized/constrained) — bear continued upwind emissions; partial litigation remedies only
 *   - - renewable_energy_developers: Excluded target (organized/mobile) — cheapest-abatement option barred from the remedy menu
 *   - - future_generations: Diffuse target (powerless/trapped) — inherit the accumulated emissions the ceiling permits
 *   - - us_congress: Dormant corrector (institutional/analytical) — could settle the scope by amendment; has not
 *   - - administrative_law_scholars: Analytical observers (moderate/analytical) — map the delegation problem from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.58).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.58).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Clean Air Act Section 111(d) 'Best System' — Facility-Bound Reading (Fenceline Constraint)").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative law/environmental regulation/constitutional interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'f23bf3ba-05dd-4b72-8380-f59fa2005286').
narrative_ontology:cs_kernel_codification('f23bf3ba-05dd-4b72-8380-f59fa2005286', fixed_text).
narrative_ontology:cs_authority_grounding('f23bf3ba-05dd-4b72-8380-f59fa2005286', lineage).
narrative_ontology:cs_interpretation_layer_present('f23bf3ba-05dd-4b72-8380-f59fa2005286').
narrative_ontology:cs_reading_relation('f23bf3ba-05dd-4b72-8380-f59fa2005286', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('f23bf3ba-05dd-4b72-8380-f59fa2005286', foundational, best_system_confined_to_fenceline_measures).
narrative_ontology:cs_axiom_status(best_system_confined_to_fenceline_measures, holdable).
narrative_ontology:cs_axiom_grounding('f23bf3ba-05dd-4b72-8380-f59fa2005286', best_system_confined_to_fenceline_measures, conventional).
narrative_ontology:cs_axiom('f23bf3ba-05dd-4b72-8380-f59fa2005286', secondary, economic_transformation_requires_clear_congressional_authorization).
narrative_ontology:cs_axiom_status(economic_transformation_requires_clear_congressional_authorization, holdable).
narrative_ontology:cs_axiom_grounding('f23bf3ba-05dd-4b72-8380-f59fa2005286', economic_transformation_requires_clear_congressional_authorization, conventional).
narrative_ontology:cs_reference_frame('f23bf3ba-05dd-4b72-8380-f59fa2005286', fenceline_performance_standard_framework).
narrative_ontology:cs_drift_state('f23bf3ba-05dd-4b72-8380-f59fa2005286', post_major_questions_settlement, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f23bf3ba-05dd-4b72-8380-f59fa2005286', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_generation_investors).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_vulnerable_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, downwind_pollution_bearing_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, cooperative_federalism).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, clear_statement_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes performance standards for existing power plants under Section 111(d). After the settled interpretation, every standard it issues must rest on measures a plant can implement on site — heat-rate upgrades, carbon capture retrofits — and it may not credit shifting generation across the grid. It retains rulemaking staff and enforcement tools but lost its preferred policy instrument; its routes out are persuading Congress to amend the statute or leaning on other air-quality programs.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency, agenda_setter,
    institutional, biographical, constrained, national).

% Review EPA rulemaking and police the boundary between source-level standards and economy-wide energy planning. Since the major-questions settlement they strike or stay rules that reach beyond the fenceline, and lower courts follow the precedent. They hold the interpretive pen; their exit is doctrinal evolution, which moves slowly.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_appellate_courts, agenda_setter,
    institutional, generational, analytical, national).

% Run existing coal-fired units whose retirement schedules would accelerate under a grid-wide reading. The settled interpretation keeps their units compliant-or-retrofit rather than obsolete; they finance heat-rate improvements and, where standards require, capture equipment. Early retirement remains available but means writing off sunk capital, so they contest standards at the margin instead.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators, payer).

% Collect severance revenue, payroll tax, and employment from coal generation and retain sole authority over their states' energy mixes — they may build renewables or keep coal, and the federal standard cannot dictate the answer. Several are diversifying slowly; none can leave the federal framework that guarantees the autonomy.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments, beneficiary,
    institutional, biographical, constrained, regional).

% Hold debt and equity in existing fossil fleets. The settled interpretation preserves the cash flows and terminal values of those assets by removing regulatory retirement pressure; their capital can move freely, and some hedge by divesting, but the assets they keep are worth more under the ceiling than under a transformation mandate.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, fossil_generation_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Live with flood, heat, and storm exposure that compounds with every ton of foreseen-but-unrequired carbon. They hold no procedural seat in the interpretive contest, cannot relocate out of climate exposure, and experience the ceiling as years added to their risk horizon.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_vulnerable_communities, payer,
    powerless, generational, trapped, global).

% Receive particulate, ozone, and mercury burdens from upwind coal units that the ceiling keeps running longer. They litigate under good-neighbor provisions and win partial relief, but the interpretive boundary keeps the underlying units online; their remedy set is narrower than their grievance.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, downwind_pollution_bearing_states, payer,
    organized, biographical, constrained, regional).

% Sell the cheapest marginal abatement available, yet the compliance calculus cannot count their output because the standard must be implementable at the emitting plant itself. They build anyway under state mandates and corporate demand, and their capital moves easily across markets — but the federal pathway that would have paid them for displaced generation is closed.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, excluded,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer).

% Inherit the atmospheric stock the ceiling permits to accumulate. They have no vote, no standing, and no exit; every year the boundary holds adds to the stock they will manage.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Wrote the ambiguous phrase in 1970 and 1990 and retains power to settle its scope by amendment. Comprehensive climate legislation has repeatedly failed, leaving the interpretive contest to the courts; members face constituents on both sides of the boundary.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, us_congress, observer,
    institutional, generational, analytical, national).

% Track the delegation problem across administrations: how much transformative authority a generalist statute carries when Congress stays silent. They publish from outside the benefiting industries and supply the corroboration record for the founding problem's continued liveness.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, administrative_law_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a stable division of regulatory labor: EPA benchmarks what individual plants can achieve; states decide what their grids are made of. Investors get a fixed compliance perimeter around which to plan retirements and retrofits. The boundary solves a real federal-state overlap problem that a grid-wide mandate would reopen.
% TRANSFER_FUNCTION: Moves regulatory discretion over decarbonization strategy away from EPA and toward preservation for incumbent fossil generators and their host states; moves the cost of foregone abatement onto emissions-exposed populations, downwind states, and future generations; moves compliance-cost predictability to utilities and lenders.
% ABSENT_VOICES: Climate-vulnerable communities and future generations would object most and are procedurally nowhere — the contest is fought among EPA, industry, coal states, and the courts. Renewable developers appear only as amici offering cost data the standard cannot use. Downwind states get partial standing through separate good-neighbor provisions.
% DISAPPEARANCE_RATIONALE: If the fenceline boundary vanished overnight, EPA could credit generation-shifting in the next round of standards, coal retirement schedules would accelerate, state energy-mix autonomy would contract to implementation detail, renewable procurement would scale against a federal compliance market, and the protected cash flows of the existing fossil fleet would reprice — the regulatory architecture of the power sector would reorganize around the broader reading.
% FOUNDING_PROBLEM: How to regulate emissions from thousands of existing sources under a generalist 1970 statute without either displacing state control over energy systems or handing an agency open-ended authority to restructure a trillion-dollar economy — the 'best system of emission reduction ... adequately demonstrated' formula was the compromise that made existing-source regulation passable at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: administrative-law scholarship across ideological lines treats the agency-authority-in-congressional-silence problem as unresolved; the Supreme Court's own opinions acknowledge that Congress has not spoken clearly on grid transformation; and the repeated failure of comprehensive climate bills is a public record no beneficiary manufactured. No serious party disputes that the underlying question remains open.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58: the ceiling's costs are real and concentrate on seats with no procedural voice, but part of the arrangement is ordinary boundary-drawing with a legitimate enacted-text grounding, keeping epsilon below pure-extraction range. Suppression 0.58: persistence rests on judicial veto over the principal regulatory alternative inside the operative statute, though alternatives outside Section 111(d) survive. Theater 0.36: the boundary functions, but a growing share of activity is rhetorical — 'state flexibility' and 'affordability' framing performing beneficence over what is substantively an incumbent shield. Accessibility collapse 0.60: once the settlement is understood, within-statute alternatives collapse; cross-statute routes persist. Resistance 0.65: continuous — litigation, alternative-authority workarounds, repeated legislative attempts. TEMPORAL GRID: all three series run on one shared grid (t=0 maps to 1995, t=30 to 2025, points every 6 units); the suppression_requirement series is authored because the story specifically tracks an enforcement ratchet — each renewed attempt at interpretive breadth met faster, higher-level judicial force (stays granted earlier, doctrine sharpened) — not a static enforcement picture. COALITION NOTE: the target set is numerous but mostly unorganizable (diffuse, future, unrepresented); only downwind states hold organized leverage, which is why resistance stays high without flipping the arrangement. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute opposites from one legal order: operator and coal-state seats stand on a protective floor (the boundary shields them); EPA stands under an instrument ceiling (the same boundary removed its preferred tool); climate-facing seats stand under a mortality-relevant delay; courts administer settled doctrine. Same-nominal-power divergence: fossil investors (arbitrage exit) experience the ceiling as portfolio protection they can walk away from, while plant operators (sunk assets) experience it as survival — identical beneficiary declaration, different exit grades, different computed positions. The engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. coal_plant_operators sit nearest the beneficiary pole — direct rent receipt (continued operation of units a broader reading would retire) with constrained exit amplifying the gain. fossil_generation_investors sit nearby but their arbitrage-grade exit damps the seat's exposure. coal_dependent_state_governments sit low-d with a mixed constituency (constituents also breathe the emissions). On the target side, future_generations and climate_vulnerable_communities sit nearest the full-target pole — trapped, powerless, and global scope amplifies effective extraction through verification difficulty; downwind_pollution_bearing_states sit slightly damped by partial good-neighbor remedies; renewable_energy_developers sit mid-high — a real imposed cost (closed federal pathway) softened by mobile capital. EPA is deliberately left undeclared in the beneficiary/victim arrays: it administers a boundary it did not choose, losing instruments while gaining litigation certainty, and takes the canonical fallback rather than a derived pole. Courts and Congress occupy administrative and observer seats near symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. A pure-coordination label would erase the ceiling's concentrated costs on seats with no procedural voice; a pure-extraction label would erase the genuine federal-state boundary and investment-certainty functions that predate the carbon contest and would survive a carbon-free grid. MANDATROPHY WATCHPOINT: the founding problem (bounding agency authority amid congressional silence) is live today, so no zombie flag fires. But if Congress enacts comprehensive climate legislation, the founding problem dies while the interpretive ceiling persists by inertia — at that point the arrangement trends toward theatrical maintenance of a superseded boundary, and the founding_problem_status x disappearance_verdict mismatch consumer should catch the transition to a degraded, inertial form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_unresolved,
    'Does the Section 111(d) delegation kernel correctly instantiate as this facility-bound constraint or as the systemic-transformation sibling — i.e., is ''best system of emission reduction'' confined to source-implementable measures, or does it reach grid-wide strategies?',
    'Congressional amendment settling the scope of Section 111(d), or a Supreme Court merits decision revisiting the major-questions settlement with a changed Court composition.',
    'If the systemic reading prevails, this constraint dissolves into the sibling: coal operators flip from beneficiaries to targets, the climate-facing seats lose victim status, and epsilon re-authors for the replaced arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_unresolved, conceptual, 'Committer-frame uncertainty: which reading instantiates the delegation kernel.').

omega_variable(
    ccs_adequate_demonstration_trajectory,
    'Will carbon capture and storage become ''adequately demonstrated'' and cost-reasonable at fleet scale, and on what timeline?',
    'Deployment and cost data from CCS retrofit projects, Department of Energy tracking, and the compliance record under source-level standards.',
    'If CCS scales, the ceiling''s bite shrinks — deep cuts remain available inside the frame and epsilon falls; if it stalls, the ceiling locks in emissions and epsilon rises toward pure-extraction levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ccs_adequate_demonstration_trajectory, empirical, 'Whether the ceiling''s extractiveness grows or shrinks with retrofit technology maturity.').

omega_variable(
    state_autonomy_genuine_or_cover,
    'Is the preserved state autonomy over energy mix a genuine federalism coordination good, or cover language for incumbent-fossil protection?',
    'Revealed-preference analysis of how states exercise the autonomy: states pursuing aggressive clean-energy standards despite the freedom versus states using the freedom to block renewables; comparative outcomes across coal-dependent and diversified states.',
    'If predominantly cover, the coordination half of the hybrid classification weakens and the arrangement trends toward pure extraction; if genuine, the coordination function stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_genuine_or_cover, conceptual, 'Tests whether the reading''s coordination justification survives revealed preference.').

omega_variable(
    foregone_abatement_counterfactual,
    'How much cumulative abatement and avoided climate damage is causally attributable to the facility-bound ceiling specifically, as opposed to other constraints (political, economic, technological)?',
    'Counterfactual modeling comparing realized emissions trajectories against modeled trajectories under a systemic-reading regime, controlling for fuel prices and state policy.',
    'Large attributable foregone abatement raises effective extraction on the climate-vulnerable and future-generation seats; small attribution lowers it and recasts the ceiling as redundant rather than extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foregone_abatement_counterfactual, empirical, 'Attribution of climate harm to the interpretive ceiling specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa111d_facility_read_tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(caa111d_facility_read_tr_t0, observed).
narrative_ontology:measurement(caa111d_facility_read_tr_t6, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(caa111d_facility_read_tr_t6, observed).
narrative_ontology:measurement(caa111d_facility_read_tr_t12, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(caa111d_facility_read_tr_t12, observed).
narrative_ontology:measurement(caa111d_facility_read_tr_t18, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(caa111d_facility_read_tr_t18, observed).
narrative_ontology:measurement(caa111d_facility_read_tr_t24, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(caa111d_facility_read_tr_t24, observed).
narrative_ontology:measurement(caa111d_facility_read_tr_t30, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(caa111d_facility_read_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(caa111d_facility_read_be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(caa111d_facility_read_be_t0, observed).
narrative_ontology:measurement(caa111d_facility_read_be_t6, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(caa111d_facility_read_be_t6, observed).
narrative_ontology:measurement(caa111d_facility_read_be_t12, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(caa111d_facility_read_be_t12, observed).
narrative_ontology:measurement(caa111d_facility_read_be_t18, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement_basis(caa111d_facility_read_be_t18, observed).
narrative_ontology:measurement(caa111d_facility_read_be_t24, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement_basis(caa111d_facility_read_be_t24, observed).
narrative_ontology:measurement(caa111d_facility_read_be_t30, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(caa111d_facility_read_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(caa111d_facility_read_su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(caa111d_facility_read_su_t0, observed).
narrative_ontology:measurement(caa111d_facility_read_su_t6, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 6, 0.34).
narrative_ontology:measurement_basis(caa111d_facility_read_su_t6, observed).
narrative_ontology:measurement(caa111d_facility_read_su_t12, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(caa111d_facility_read_su_t12, observed).
narrative_ontology:measurement(caa111d_facility_read_su_t18, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 18, 0.49).
narrative_ontology:measurement_basis(caa111d_facility_read_su_t18, observed).
narrative_ontology:measurement(caa111d_facility_read_su_t24, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(caa111d_facility_read_su_t24, observed).
narrative_ontology:measurement(caa111d_facility_read_su_t30, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(caa111d_facility_read_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what EPA can require under Section 111(d)' decomposes into two structurally distinct constraints per the epsilon-invariance principle: the facility-bound reading (this file) and the systemic-transformation reading (sibling file). They differ in epsilon, victim sets, enforcement history, and coordination function — measuring one with the other's observables produces unstable epsilon, which is the signature of two constraints sharing a label. The upstream/downstream structure is unusual: the facility reading currently governs (post-settlement) and thereby constrains the sibling's operating environment, but as readings of a single statutory term they stand in foreclosure rather than mere influence — no single authoritative framework can hold both scopes at once. Both files carry this note and link each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
