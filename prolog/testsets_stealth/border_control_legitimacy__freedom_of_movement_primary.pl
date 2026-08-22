% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Global Border-Closure Regime (Freedom-of-Movement-Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This story instantiates the freedom_of_movement_primary reading of the
 *   border_control_legitimacy kernel: freedom of movement is a fundamental
 *   human right, and territorial sovereignty does not carry border closure
 *   authority. The epsilon referent is the standing arrangement under
 *   contest, the global border-closure regime of visa codes, externalized
 *   processing, interception, detention, and deportation, assessed by this
 *   reading's own lights, never the open-movement arrangement this reading
 *   endorses (authoring epsilon for the endorsed alternative would drive it
 *   to zero by construction). The colloquial label 'border control
 *   legitimacy' decomposes into three structurally distinct constraints, this
 *   reading, sovereignty_primary, and jurisdictional_sovereignty, linked
 *   through network.affects_constraints; each authors its own epsilon over
 *   the same referent arrangement, and the values differ widely because the
 *   readings locate exclusion power differently within sovereignty. Claim and
 *   metrics are independent authored facts: the claimed type records this
 *   reading's structural assessment of the arrangement; the metrics record
 *   the regime's observed operation; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed output is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - sovereignty_performing_governments: agenda-setter and beneficiary (institutional/constrained) — writes and enforces the closure rules, collects electoral legitimacy from performing control
 *   - destination_country_labor_insiders: primary beneficiary (powerful/constrained) — captures the wage-premium and queue-insulation margin of restricted entry
 *   - border_security_industry: secondary beneficiary (organized/arbitrage) — receives appropriations that scale with enforcement intensity
 *   - asylum_seekers_interdicted_in_transit: primary target (powerless/trapped) — bears interception, detention, and route mortality directly
 *   - would_be_labor_migrants: primary target (powerless/trapped) — bears smuggler payments, debt bondage, and decades of removal exposure
 *   - migrant_sending_states: excluded voice (moderate/constrained) — absorbs citizen losses and returnee crises with weak consultative standing
 *   - human_rights_treaty_bodies: analytical observer (institutional/analytical) — sees the full structure across jurisdictions, holds no enforcement arm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.85).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.88).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Global Border-Closure Regime (Freedom-of-Movement-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '9cb84c20-5239-49c0-a310-315a6c4b8928').
narrative_ontology:cs_kernel_codification('9cb84c20-5239-49c0-a310-315a6c4b8928', formalized).
narrative_ontology:cs_authority_grounding('9cb84c20-5239-49c0-a310-315a6c4b8928', lineage).
narrative_ontology:cs_interpretation_layer_present('9cb84c20-5239-49c0-a310-315a6c4b8928').
narrative_ontology:cs_reading_relation('9cb84c20-5239-49c0-a310-315a6c4b8928', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('9cb84c20-5239-49c0-a310-315a6c4b8928', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('9cb84c20-5239-49c0-a310-315a6c4b8928', foundational, movement_is_fundamental_human_right).
narrative_ontology:cs_axiom_status(movement_is_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('9cb84c20-5239-49c0-a310-315a6c4b8928', movement_is_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('9cb84c20-5239-49c0-a310-315a6c4b8928', foundational, closure_authority_outside_sovereign_competence).
narrative_ontology:cs_axiom_status(closure_authority_outside_sovereign_competence, holdable).
narrative_ontology:cs_axiom_grounding('9cb84c20-5239-49c0-a310-315a6c4b8928', closure_authority_outside_sovereign_competence, deontological).
narrative_ontology:cs_axiom('9cb84c20-5239-49c0-a310-315a6c4b8928', secondary, jurisdictional_regulation_within_sovereign_competence).
narrative_ontology:cs_axiom_status(jurisdictional_regulation_within_sovereign_competence, holdable).
narrative_ontology:cs_axiom_grounding('9cb84c20-5239-49c0-a310-315a6c4b8928', jurisdictional_regulation_within_sovereign_competence, conventional).
narrative_ontology:cs_reference_frame('9cb84c20-5239-49c0-a310-315a6c4b8928', universal_freedom_of_movement_presumption).
narrative_ontology:cs_drift_state('9cb84c20-5239-49c0-a310-315a6c4b8928', contemporary_externalization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9cb84c20-5239-49c0-a310-315a6c4b8928', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, destination_country_labor_insiders).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_security_industry).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, sovereignty_performing_governments).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers_interdicted_in_transit).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, would_be_labor_migrants).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, state_plenary_power_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, westphalian_mobility_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate visa categories, fund patrol and detention capacity, and sign externalization agreements with transit and origin states. Collect electoral legitimacy from visible acts of control: wall openings, deportation counts, take-back-control campaigns. They hold the pen on the arrangement and could legalize channels or dismantle externalized processing by statute, but face electoral punishment from insider voters for doing so. Their realistic choice set runs from tightening to managed relaxation, not exit.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, sovereignty_performing_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, sovereignty_performing_governments, beneficiary).

% Citizen workers and established residents of wealthy destination states. Restricted entry keeps labor supply below what open movement would deliver, supporting wage floors in exposed sectors, shorter job queues, and lower congestion in public services. Most did not design the arrangement and rarely think about it, but they capture its economic margin continuously and vote periodically on its maintenance. Leaving would mean forfeiting the premium; staying costs them nothing under the current design.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, destination_country_labor_insiders, beneficiary,
    powerful, biographical, constrained, national).

% Contractors building surveillance infrastructure, operators running detention facilities, vendors supplying biometric databases, consultancies designing externalization schemes. Appropriations scale with enforcement intensity, and contract renewals depend on continued perceived threat rather than on reduced crossings. Revenue streams diversify across jurisdictions, so a slowdown in one corridor shifts sales to another.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_security_industry, beneficiary,
    organized, immediate, arbitrage, continental).

% Fleeing persecution, war, or state collapse, they reach a land border or coastline and meet interception, offshore processing, pushback, or years of waiting in a transit state with no lawful path forward. Detention, family separation, and route mortality are direct experiences rather than abstractions. Every legal door is the thing that is closed; onward movement means another militarized frontier.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers_interdicted_in_transit, payer,
    powerless, immediate, trapped, regional).

% Workers in low-wage economies seeking income multiples of home wages. Legal channels for their skill class are narrow or nonexistent, so the price of movement is paid to smugglers, in debt bondage, and along deadly routes. Some save for years for a journey with high mortality odds; others conclude the risk is unbearable and stay, which the enforcement design records as success. Those who make it through live for decades under threat of removal.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, would_be_labor_migrants, payer,
    powerless, biographical, trapped, global).

% Governments of origin countries watch working-age citizens depart through dangerous channels, manage consular crises and returnee reception, and negotiate readmission agreements from weakness. They would redesign the arrangement toward expanded legal channels if they had a seat at the table; their formal participation is limited to consultative dialogues and bilateral bargains struck under aid-and-trade pressure. Remittance inflows partially offset the losses.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migrant_sending_states, excluded,
    moderate, generational, constrained, national).

% UN treaty bodies, regional human rights courts, and special rapporteurs review state practice against movement and non-refoulement obligations, issue judgments and findings, and document pushbacks across jurisdictions. They hold no enforcement arm; compliance depends on state consent. Their seat sees the full structure and records the gap between obligation and operation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, human_rights_treaty_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, destination_country_labor_insiders).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates membership boundaries: determines which people may enter a state's labor market, territory, and protection systems, allowing public provision, infrastructure, and political representation to be planned for a bounded population. Entry-point screening also performs a narrower function, interdicting individuals carrying specific threats, that is logically distinct from categorical exclusion by nationality or class.
% TRANSFER_FUNCTION: Moves access, to labor markets, territory, physical safety, and family life, away from would-be entrants and their origin communities and toward incumbent residents of destination states; separately moves large fiscal appropriations from destination-state treasuries to enforcement contractors and detention operators; and concentrates mortality risk onto the migrants themselves along externalized routes.
% ABSENT_VOICES: The migrants subject to the arrangement have no seat anywhere it is designed: no interdicted asylum seeker votes on the visa code, and transit-state populations absorb the backlog without consent. Origin-state governments hold weak consultative seats. Future cohorts who will age into the demographic shortfall the arrangement produces are unrepresented everywhere.
% DISAPPEARANCE_RATIONALE: If border closure authority vanished overnight, destination labor markets would reprice as new workers arrived, enforcement budgets and detention capacity would stand down, smuggling markets would collapse for lack of the scarcity they sell, sending regions would lose remittance dependence and gain returning households, and citizenship law would become the sole determinant of rights. The enforcement-industrial complex and its fiscal flows would dissolve within years.
% FOUNDING_PROBLEM: Mass displacement after the World Wars and through the Cold War's close, combined with post-1970s economic divergence, confronted states with population movements they could neither filter nor plan for; the passport-visa-detention architecture was built to restore state capacity to decide who arrives, protecting wages, welfare systems, and security from unmanaged inflows.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the passport system, notably Torpey's account of the state's monopolization of legitimate means of movement, and UNHCR displacement statistics corroborate that the founding problem, unmanageable displacement meeting unplannable arrival, was real. On status: destination-state governments attest the problem is live, citing security and integration strain; labor economists and demographers writing outside the enforcement apparatus attest the framing is substantially outdated, pointing to shrinking workforces and sectoral shortage evidence. Nobody outside the benefiting parties attests that the problem remains live in its original form.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.85) because the arrangement's burden, denied access to safety and labor markets, falls on people who never consented to it and cannot relocate around it, while the margin accrues continuously to insiders. Suppression is higher still (0.88) because persistence depends on active machinery: interception, detention, carrier sanctions, externalized processing, and the criminalization of rescue; the arrangement does not persist by participant preference. Theater ratio (0.55) reflects the widening gap between enforcement performance and stated outcomes: walls that reroute rather than reduce, deportation counts staged as policy, pandemic closures retained as signal after their epidemiological rationale faded; by the interval's end a majority of visible enforcement activity functions as reassurance for insiders rather than as flow management. Accessibility collapse is moderate (0.40): lawful channels have collapsed for the poor, but irregular routes, onward movement, and intra-bloc mobility keep alternatives partly alive, and what the arrangement suppresses specifically is the lawful alternative. Resistance (0.65) is substantial: litigation wins before regional courts, rescue fleets, sanctuary networks, and migrant-led organizing impose real costs. Coordination type is declared identity_coordination, membership-boundary maintenance, with the standing caveat that identity framing is the most common cover story for asymmetric extraction; the conservative floor ensures the coupling test still registers the power-by-scope concentration (insider voting power, global scope, trapped targets). The three temporal series share one eight-point grid (1974-2026); trajectories are monotonic ratchets rather than cycles, because each crisis (post-Cold War displacement, 2001, 2015-16, the pandemic) left enforcement machinery permanently larger, which is why no oscillation appears in the record. Both target classes are individually powerless; the coalition question carried in omega victim_coalition_feasibility is the live path by which resistance could exceed the authored 0.65.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergent types from identical structure. From the agenda-setter seat the arrangement presents as legitimate self-government, a coordination structure the state built and may adjust; from the insider-beneficiary seat it presents as ordinary background policy delivering a quiet premium; from either target seat the same structure operates as coerced exclusion with no exit. The engine derives these per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them. The divergence is the finding: a structure that reads as coordination from the seats that wrote it and as extraction from the seats trapped inside it is the signature this corpus exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: governments sit near the subsidized end, collecting legitimacy and appropriations while bearing only enforcement costs they themselves choose; insiders capture the economic margin with zero administrative burden; the industry converts appropriations to revenue with arbitrage-grade portfolio exit. Victim declarations map to the target end: both migrant classes sit at or near full-target, and trapped exit amplifies their effective burden beyond the base rate. Sending states sit near-symmetric with a slight target lean, citizen losses against remittance inflows; treaty bodies take the analytical seat and feed no directional arithmetic. The regime's spatial scope is global, which raises verification difficulty and amplifies effective extraction for the trapped seats. Receipt surface: the durable economic margin lands principally on destination-country labor insiders (wage premiums, queue insulation); the security industry receives direct appropriations and governments receive political capital, but the compounding rent accrues to insiders, hence gain_flow names that seat. Fixing is classified prohibitive: the agenda-setter could dismantle the arrangement by statute at trivial administrative cost, but the electoral price inside an office horizon exceeds any benefit it could bank, so the cost-to-fix is political rather than technical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, restoring state capacity to manage displacement, was real and is documented from outside the benefiting parties, but its status is contested: the enforcement apparatus attests it live, while demographic and labor-market evidence from outside suggests the original framing no longer matches conditions. Because status is contested rather than dead, the mismatch consumer finds no zombie flag here. The mandatrophy risk runs the other way: the arrangement's genuine screening function could license reading the whole structure as coordination. This reading's classification blocks that move: the screening function is separable (see omega screening_exclusion_separability), and what persists is the exclusion apparatus, maintained by coercion and theatrical reassurance. If the founding problem were later shown dead while the apparatus persists, the theater-heavy profile would route the structure toward the degraded-inertial type; the authored theater trajectory (0.18 rising to 0.55) is the leading indicator of exactly that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading, freedom_of_movement_primary, of the border_control_legitimacy kernel; what would the classification look like from the sibling readings'' seats?',
    'Generate the sibling stories (sovereignty_primary, jurisdictional_sovereignty) over the identical referent arrangement and compare per-seat classifications; the disagreement is located in a single structural element, whether exclusion power sits inside or outside the sovereign competence.',
    'Under sovereignty_primary the victim ledger empties (exclusion becomes legitimate self-definition) and epsilon collapses toward coordination cost; under jurisdictional_sovereignty the victim set shrinks to those excluded without sufficient balancing justification. This story''s high epsilon is reading-indexed over a fixed referent, not a property of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Reading-indexed classification of a shared kernel; sibling readings would restructure the beneficiary and victim sets.').

omega_variable(
    sibling_reading_structural_delta,
    'If the jurisdictional_sovereignty reading prevailed, which elements of this story''s structure survive?',
    'Compare the sibling story''s victim set and enforcement-legitimacy verdicts against this one; the delta concentrates on whether public-consent and labor-needs balancing can ever justify exclusion, and on whether the enforcement apparatus retains legitimacy.',
    'Jurisdictional_sovereignty preserves a regulated-subset victim ledger (those excluded without adequate balancing grounds) and keeps screening legitimate while condemning categorical exclusion; this reading condemns the exclusion apparatus wholesale and relocates all state authority to post-entry regulation of rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Expected structural delta between this reading and the balancing sibling.').

omega_variable(
    movement_right_foundation,
    'Is freedom of movement a pre-political fundamental right that sovereignty cannot override, or a conventional entitlement that positive law grants and may withdraw?',
    'Cross-jurisdictional convergence analysis together with philosophical argumentation: if the right''s force depends entirely on enactment, the reading reduces to a policy preference and loses its trumping power over sovereignty claims.',
    'If merely conventional, this reading converges toward jurisdictional_sovereignty (balancing replaces trumping) and the forecloses relation to sovereignty_primary weakens toward coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_right_foundation, conceptual, 'Whether the reading''s foundational right is deontological or conventional in force.').

omega_variable(
    screening_exclusion_separability,
    'Can the genuine screening function, interdicting specific threats and verifying identity, be institutionally separated from categorical exclusion by origin?',
    'Natural experiment: intra-bloc mobility arrangements maintain screening without nationality-based exclusion among members; extend the comparison to visa-free corridors and labor-mobility agreements.',
    'If separable, the measured extraction is rent riding on a real function and the snare reading strengthens; if inseparable, part of the burden attributed to exclusion is irreducible coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(screening_exclusion_separability, empirical, 'Whether the arrangement''s coordination and exclusion components are structurally separable.').

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the measured suppression is structural (patrols, visa gates, detention, externalized processing) versus internalized (would-be migrants ruling themselves out, families discouraging journeys, learned expectation of death at sea)?',
    'Post-liberalization trajectory: if attempted movement surges immediately after legal channels open, prior suppression was substantially structural; if expectations overshoot revealed desire, an internalized component persisted beyond the machinery.',
    'Internalized suppression travels with the target after any reform, meaning formal channel-opening under-delivers and the deterrent effect outlives the enforcement apparatus that produced it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized composition of the suppression scalar.').

omega_variable(
    victim_coalition_feasibility,
    'Can the arrangement''s targets, interdicted asylum seekers, undocumented workers, and would-be migrants, form an effective coalition despite dispersion, legal precarity, and disenfranchisement?',
    'Track organizational density: migrant-led movements, collective action by undocumented workers, transnational advocacy networks; measure whether litigation and mobilization convert into channel-opening.',
    'An effective coalition would raise resistance above the authored 0.65 and destabilize the arrangement toward reform; persistent dispersion keeps each target individually powerless and the arrangement stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_feasibility, empirical, 'Coalition potential of a dispersed, legally precarious target class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1974, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fom_primary_tr_t1974, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1974, 0.18).
narrative_ontology:measurement_basis(fom_primary_tr_t1974, observed).
narrative_ontology:measurement(fom_primary_tr_t1985, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(fom_primary_tr_t1985, observed).
narrative_ontology:measurement(fom_primary_tr_t1992, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1992, 0.27).
narrative_ontology:measurement_basis(fom_primary_tr_t1992, observed).
narrative_ontology:measurement(fom_primary_tr_t2001, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2001, 0.33).
narrative_ontology:measurement_basis(fom_primary_tr_t2001, observed).
narrative_ontology:measurement(fom_primary_tr_t2011, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2011, 0.4).
narrative_ontology:measurement_basis(fom_primary_tr_t2011, observed).
narrative_ontology:measurement(fom_primary_tr_t2016, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2016, 0.46).
narrative_ontology:measurement_basis(fom_primary_tr_t2016, observed).
narrative_ontology:measurement(fom_primary_tr_t2020, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2020, 0.51).
narrative_ontology:measurement_basis(fom_primary_tr_t2020, observed).
narrative_ontology:measurement(fom_primary_tr_t2026, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2026, 0.55).
narrative_ontology:measurement_basis(fom_primary_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(fom_primary_be_t1974, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1974, 0.55).
narrative_ontology:measurement_basis(fom_primary_be_t1974, observed).
narrative_ontology:measurement(fom_primary_be_t1985, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement_basis(fom_primary_be_t1985, observed).
narrative_ontology:measurement(fom_primary_be_t1992, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1992, 0.64).
narrative_ontology:measurement_basis(fom_primary_be_t1992, observed).
narrative_ontology:measurement(fom_primary_be_t2001, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2001, 0.69).
narrative_ontology:measurement_basis(fom_primary_be_t2001, observed).
narrative_ontology:measurement(fom_primary_be_t2011, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2011, 0.75).
narrative_ontology:measurement_basis(fom_primary_be_t2011, observed).
narrative_ontology:measurement(fom_primary_be_t2016, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2016, 0.79).
narrative_ontology:measurement_basis(fom_primary_be_t2016, observed).
narrative_ontology:measurement(fom_primary_be_t2020, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement_basis(fom_primary_be_t2020, observed).
narrative_ontology:measurement(fom_primary_be_t2026, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2026, 0.85).
narrative_ontology:measurement_basis(fom_primary_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(fom_primary_su_t1974, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1974, 0.48).
narrative_ontology:measurement_basis(fom_primary_su_t1974, observed).
narrative_ontology:measurement(fom_primary_su_t1985, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement_basis(fom_primary_su_t1985, observed).
narrative_ontology:measurement(fom_primary_su_t1992, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1992, 0.62).
narrative_ontology:measurement_basis(fom_primary_su_t1992, observed).
narrative_ontology:measurement(fom_primary_su_t2001, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2001, 0.71).
narrative_ontology:measurement_basis(fom_primary_su_t2001, observed).
narrative_ontology:measurement(fom_primary_su_t2011, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2011, 0.77).
narrative_ontology:measurement_basis(fom_primary_su_t2011, observed).
narrative_ontology:measurement(fom_primary_su_t2016, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2016, 0.82).
narrative_ontology:measurement_basis(fom_primary_su_t2016, observed).
narrative_ontology:measurement(fom_primary_su_t2020, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2020, 0.86).
narrative_ontology:measurement_basis(fom_primary_su_t2020, observed).
narrative_ontology:measurement(fom_primary_su_t2026, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2026, 0.88).
narrative_ontology:measurement_basis(fom_primary_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, identity_coordination).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% 'Border control legitimacy' is a colloquial label covering three structurally distinct constraints over one shared referent arrangement, the global border-closure regime. sovereignty_primary authors low epsilon from its seat (exclusion as legitimate self-definition); jurisdictional_sovereignty authors intermediate epsilon (exclusion legitimate only within balancing limits); this story authors high epsilon (exclusion categorically illegitimate). The readings disagree on a single structural element, whether exclusion power sits inside the sovereign competence, and that disagreement relocates the victim set. sovereignty_primary is historically upstream: its doctrine is cited by enforcement structures as warrant, and this reading attacks its premise directly (forecloses), while jurisdictional_sovereignty competes as a live alternative (coexists_with). Each family member links to the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
