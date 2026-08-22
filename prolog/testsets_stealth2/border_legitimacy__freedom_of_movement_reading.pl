% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Exclusion Regime — Freedom-of-Movement Reading
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This file instantiates the freedom_of_movement_reading of the
 *   border_legitimacy kernel. The constraint under classification is the
 *   standing border-exclusion regime — visa architectures, interdiction,
 *   detention, removal, externalized processing — assessed by this reading's
 *   own lights: movement as a human right, borders as presumptively
 *   illegitimate restrictions. Per the epsilon-referent rule, extractiveness
 *   is authored over the standing arrangement the story is about, never over
 *   the open-borders alternative this reading endorses. The structural delta
 *   against the sibling readings: current citizens enter the victim set
 *   (displaced domestic workers via the legally-silenced-labor mechanism;
 *   welfare recipients via fiscal crowding-out and conditionality erosion),
 *   border enforcement reads as extractive rather than protective, and
 *   epsilon over the restriction is high. The sibling readings are separate
 *   constraint files linked through network.affects_constraints; their
 *   contest is routed to omega variables, not folded into this
 *   classification.
 *
 * KEY AGENTS:
 *   - national_border_agencies: Agenda-setter (institutional/arbitrage) — sets visa rules and enforcement posture; administers patrols, detention, removal; budget compounds with intensity
 *   - border_enforcement_industry: Beneficiary (organized/mobile) — sells surveillance, barriers, detention beds, deportation logistics across many states
 *   - employers_of_undocumented_labor: Primary beneficiary (powerful/mobile) — harvests the legally-silenced workforce that restriction manufactures
 *   - smuggling_networks: Beneficiary (organized/mobile) — prices the danger premium that enforcement intensity creates
 *   - prospective_migrants_denied_entry: Primary target (powerless/trapped) — bears the extraction of movement itself
 *   - asylum_seekers_interdicted: Target (powerless/trapped) — intercepted, processed offshore, warehoused indefinitely
 *   - undocumented_underclass_workers: Target (powerless/trapped) — present without status; complaint equals deportation
 *   - displaced_domestic_workers: Secondary target (moderate/constrained) — citizens undercut by the silenced labor pool
 *   - welfare_recipients_in_destination_states: Secondary target (powerless/trapped) — bear crowding-out and conditionality erosion
 *   - sending_country_communities: Excluded voice (powerless/trapped) — absorb the deaths and the dependency, hold no seat
 *   - migrant_rights_movements: Resisting observer (organized/analytical) — documents, litigates, shelters
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical) — records the gap between codified right and enforced exclusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.86).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.88).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Exclusion Regime — Freedom-of-Movement Reading").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '5ef34470-1076-4453-9f45-5c6531fbfb28').
narrative_ontology:cs_kernel_codification('5ef34470-1076-4453-9f45-5c6531fbfb28', distributed).
narrative_ontology:cs_authority_grounding('5ef34470-1076-4453-9f45-5c6531fbfb28', distributed).
narrative_ontology:cs_reading_relation('5ef34470-1076-4453-9f45-5c6531fbfb28', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ef34470-1076-4453-9f45-5c6531fbfb28', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('5ef34470-1076-4453-9f45-5c6531fbfb28', foundational, cross_border_movement_is_fundamental_human_right).
narrative_ontology:cs_axiom_status(cross_border_movement_is_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('5ef34470-1076-4453-9f45-5c6531fbfb28', cross_border_movement_is_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('5ef34470-1076-4453-9f45-5c6531fbfb28', foundational, birthplace_is_morally_arbitrary_for_life_chances).
narrative_ontology:cs_axiom_status(birthplace_is_morally_arbitrary_for_life_chances, holdable).
narrative_ontology:cs_axiom_grounding('5ef34470-1076-4453-9f45-5c6531fbfb28', birthplace_is_morally_arbitrary_for_life_chances, deontological).
narrative_ontology:cs_axiom('5ef34470-1076-4453-9f45-5c6531fbfb28', secondary, origin_based_exclusion_presumptively_unjust).
narrative_ontology:cs_axiom_status(origin_based_exclusion_presumptively_unjust, holdable).
narrative_ontology:cs_axiom_grounding('5ef34470-1076-4453-9f45-5c6531fbfb28', origin_based_exclusion_presumptively_unjust, deontological).
narrative_ontology:cs_reference_frame('5ef34470-1076-4453-9f45-5c6531fbfb28', movement_as_default_liberty).
narrative_ontology:cs_drift_state('5ef34470-1076-4453-9f45-5c6531fbfb28', contemporary_wall_building_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5ef34470-1076-4453-9f45-5c6531fbfb28', '2026-06-20T09:30:00Z').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, employers_of_undocumented_labor).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, smuggling_networks).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, prospective_migrants_denied_entry).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, asylum_seekers_interdicted).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, undocumented_underclass_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, national_border_agencies).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, state_monopoly_on_authorized_movement).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, positive_law_exclusion_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures and interior ministries set visa categories, admission quotas, and enforcement posture; agencies administer patrols, detention, and removal. They justify the apparatus through sovereignty, security, and labor-market protection. They can expand or contract legal channels at will, and their budgets and staffing compound with enforcement intensity regardless of whether crossings fall.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, national_border_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, national_border_agencies, beneficiary).

% Surveillance vendors, barrier contractors, private detention operators, and deportation-logistics firms sell the apparatus. Revenue scales with enforcement intensity, not with outcomes; the industry markets its products across multiple states and lobbies for expansion. Exit is easy: the same technology and contracts sell to any government.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry, beneficiary,
    organized, biographical, mobile, continental).

% Agricultural, construction, care, and processing employers hire workers whose status bars them from complaining, unionizing, or litigating. Restriction is what manufactures this silence: legalization would raise their labor costs toward market terms. They price occasional audit and raid risk as an ordinary operating cost and can shift sectors or contractors freely.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, employers_of_undocumented_labor, beneficiary,
    powerful, biographical, mobile, national).

% Close the gap between demand for movement and legal supply. Fees scale with the danger of the route, and enforcement intensity is what makes routes dangerous; every corridor closed raises the premium on the next one. Networks relocate instantly when any single route is suppressed.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, smuggling_networks, beneficiary,
    organized, immediate, mobile, global).

% People whose life prospects depend on moving and who face closed legal channels: they may stay and forgo the move, take debt-financed irregular routes carrying lethal risk, or join queues they rarely qualify for. They bear the full brunt of the regime's deterrent design — the deaths in deserts and seas are the mechanism working as built.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, prospective_migrants_denied_entry, payer,
    powerless, biographical, trapped, global).

% Fleeing persecution or state collapse, they encounter interception at sea, pushback at land borders, offshore processing, and indefinite waiting in encampments. They cannot choose destination freely, cannot work while waiting in most jurisdictions, and bear family separation imposed by the processing architecture.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, asylum_seekers_interdicted, payer,
    powerless, immediate, trapped, regional).

% Present without status inside the destination state. They work below market terms because invoking labor law means deportation. Exit is blocked in both directions: regularizing is procedurally near-impossible, and returning means forfeiting everything the journey cost. They are the load-bearing tier of the exploited labor market the restriction creates.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, undocumented_underclass_workers, payer,
    powerless, biographical, trapped, national).

% Citizens in low-barrier, tradeable occupations competing against a workforce that cannot invoke labor protections. Their wages and conditions are dragged down not by migrants' presence as such but by the legal silencing of that workforce. Switching sectors, regions, or into retraining is possible but costly, and they are addressed politically as beneficiaries of the regime they are harmed by.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Bear the fiscal crowding-out of enforcement budgets and the political conditionality that ties benefit access to immigration politics. The existence of a super-exploitable underclass beneath them is used to justify workfare discipline and benefit retrenchment. They have no individual exit from the national fiscal-political settlement that spends their programs on walls and detention beds.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_destination_states, payer,
    powerless, immediate, trapped, national).

% Villages and families that lose members to the desert and the sea and to remittance-dependent economies shaped by enforcement. They hold no seat in the destination-state fora where the regime is designed; their objection — the right to leave and to live — is registered nowhere in the process that kills their sons and daughters at rates that would be intolerable if applied to citizens.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, sending_country_communities, excluded,
    powerless, generational, trapped, regional).

% NGOs, sanctuary networks, rescue flotillas, and abolitionist scholars. They document deaths, litigate against removals, shelter the undocumented, and name the structure as a rights violation. They see the whole apparatus but hold no agenda-setting power; their resistance is continuous and largely defensive.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migrant_rights_movements, observer,
    organized, generational, analytical, global).

% Treaty bodies and special rapporteurs review state practice against the codified movement guarantees (UDHR Article 13, ICCPR Article 12). They record, year over year, the widening gap between the right to leave and move that states have signed and the exclusion machinery those same states operate. Their findings carry documentation weight and no enforcement force.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, employers_of_undocumented_labor).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A genuine residue exists: identity documentation, customs, and epidemiological screening at ports of entry — tasks that do not require excluding anyone by national origin. The apparatus's principal actual output is different: it coordinates a political equilibrium inside destination states, reconciling electorates to labor-market composition by guaranteeing that the foreign workers admitted are legally controllable. On this reading, equilibrium-management rather than screening is what the bulk of the machine coordinates.
% TRANSFER_FUNCTION: Subtracts freedom of movement from everyone born outside a shrinking club of wealthy states and transfers it to no one. Moves legally-silent labor power from undocumented workers to their employers; moves enforcement contracts from destination-state treasuries to the enforcement industry; moves passage premiums from desperate movers to smuggling networks; and, via the underclass mechanism and enforcement budgets, moves bargaining power from domestic workers and budget share from welfare programs to the enforcement coalition.
% ABSENT_VOICES: The governed themselves: would-be migrants and sending communities have no vote, no lobby seat, and no standing in the destination-state fora where the regime is set. Their consent is never solicited and their deaths are recorded as statistics. Domestic workers and welfare recipients are spoken for by politicians claiming to protect them, ordinarily without their participation in the design of what protects whom.
% DISAPPEARANCE_RATIONALE: Labor markets in rich states would reprice as millions of legally-silent workers gained standing; the enforcement industry would lose its revenue base overnight; smuggling premiums would collapse toward zero as legal channels opened; remittance economies and sending-community demographics would reorganize; and welfare politics would lose the scapegoat that disciplines benefit claimants. Nothing physical requires the regime; everything social rearranges around its absence.
% FOUNDING_PROBLEM: After 1914 and again in the 1970s, territorial states confronted mass mobility they read as threatening: wage depression, welfare solvency, cultural cohesion, and after 2001 security. The modern passport, visa, interdiction, and detention apparatus was built to reconcile the state's claim to control membership with the reality of people who need to move.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: migration economists (Clemens, Pritchett) attest the magnitude of the original mobility problem from the demand side; Torpey's historical work attests the regime's recent, constructed genealogy rather than any natural permanence; Card-line and subsequent meta-analytic labor evidence undercuts the native wage-threat premise at current scales. No source outside the enforcement-industrial and nationalist beneficiary set attests that the exclusion problem as currently framed remains live; the reading's own tradition holds the founding problem was misdiagnosed, with a genuine screening residue requiring a small fraction of the apparatus.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.86) because the regime subtracts a basic liberty by birthplace lottery from billions of people and additionally manufactures an exploitable underclass whose surplus flows to employers. Suppression is higher still (0.88) and is a raw structural property, unscaled by power or scope: the regime's persistence depends on continuous coercive force — walls, patrols, detention capacity, removal flights, criminalization of movement, closure of legal channels — not on participant preference. Theater ratio (0.52) reflects a split apparatus: roughly half of enforcement activity is spectacle calibrated to electorate anxiety (barrier milestones, deportation-flight coverage, surge announcements timed to elections, walls that reroute rather than stop flows), while the functional half — detention, interdiction, removal — is real coercion. Accessibility collapse (0.58) is snare-typical: legal alternatives collapse almost completely for would-be movers once the regime is understood, but irregular routes persist and the historical existence of open-movement zones (EU internal) keeps alternatives from vanishing entirely. Resistance (0.62) is sustained and real: rights movements, sanctuary networks, rescue operations, and litigation meet the regime continuously. The claimed type (snare) is authored from structural analysis; the metrics are authored independently from descriptive observation — the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the measurement the corpus exists to take. All three temporal series run on one shared seven-point grid (1973–2025) so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the agenda-setter seat the same structure is legitimate statecraft the agency administers and staffs; from the trapped payer seats it is extraction in the full sense; from the beneficiary seats it is a revenue line. Same-level lateral divergence is sharpest between displaced_domestic_workers and undocumented_underclass_workers: adjacent positions in the same labor market, nominally exposed to the same forces, yet computing differently because legal status modulates exit (constrained versus trapped) and therefore effective extraction. Identity-lock enters on the destination-electorate side: for a decisive voting bloc, border control is fused with national self-conception rather than held as policy, which is why fixing is prohibitive for the agenda-setter despite large distributive gains — breaking the identity frame, not the budget arithmetic, is the binding constraint. Coalition potential among the powerless victims exists on paper (movers, underclass workers, domestic workers, and welfare recipients share interests against the underclass mechanism) but is structurally suppressed: the regime's status competition narratives are part of its persistence machinery.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the low-d pole: border_enforcement_industry, employers_of_undocumented_labor, and smuggling_networks sit near the beneficiary end (their revenue and surplus are the regime's product), with employers capturing the largest aggregate share via wage suppression across the silenced workforce. The agenda-setting agencies sit slightly above pure beneficiary: they spend treasuries but their budgets, staffing, and mandate grow with enforcement intensity. Direct victims — prospective migrants, asylum seekers, the undocumented underclass — sit near the full-target pole, amplified by trapped exit. The citizen victims are the reading's distinctive contribution: displaced_domestic_workers bear mediated harm (via the silenced-labor mechanism, not direct confiscation) and welfare_recipients bear fiscal-political harm; both are declared victims and derive high d accordingly. One directionality override is authored: the story's only moderate-power seat (displaced_domestic_workers) would derive full-target d from its victim declaration, but its harm channel is mediated rather than direct, so d is set to 0.78 — below the direct victims, above symmetry. Suppression, again, is unscaled structure; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The regime's cover story is rope-shaped — orderly migration, secure borders, managed numbers — and a classifier that took the cover at face value would misread enforced exclusion as coordination. The snare classification forces the two questions the cover forecloses: who is actually coordinated (anxious electorates and enforcement bureaucracies, not movers), and who pays (movers, the underclass, and — on this reading — the very domestic workers and welfare recipients the cover claims to protect). The mandatrophy direction cuts the other way too: the founding problem had a live core (mass mobility did demand some administrative answer after 1914 and again after 1973), but the apparatus has grown orders of magnitude beyond any screening need while its evidentiary premises eroded; the mandate has outlived its support even as its budget compounds. Reading the regime as a rope would launder that accumulation; reading it as inertial theater would miss the functioning detention and removal machinery. The snare verdict holds both facts: functional coercion in service of extraction, spectacle wrapped around it. The R5 interview records the founding problem as contested rather than dead — the mismatch consumer should read status=contested x verdict=world_rearranges as a live-function dispute, not a zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the border_legitimacy kernel; the sovereignty_reading and humanitarian_obligation_reading siblings instantiate different constraints over the same standing arrangement — which structural elements do the readings actually disagree about?',
    'Comparative classification across the three linked stories: locate the divergence in (a) the victim set (whether current citizens sit inside it), (b) the legitimacy presumption attached to exclusion, and (c) the epsilon authored over the identical referent.',
    'Under sovereignty_reading the same regime computes with citizens as beneficiaries and substantially lower epsilon; under humanitarian_obligation_reading a partial-legitimacy band appears around persecution cases. The kernel contest is carried by cross-reading deltas, not by any within-story metric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of the border_legitimacy kernel; sibling readings are separate constraints.').

omega_variable(
    native_harm_empirics,
    'Do displaced domestic workers and welfare recipients belong in the victim set — are the wage-undercutting and fiscal-crowding-out channels empirically real at policy-relevant magnitudes?',
    'Meta-analysis of native wage effects (Card-line versus Borjas-line literatures), fiscal incidence studies of enforcement budgets, and natural experiments from legalization episodes that remove the silenced-labor mechanism.',
    'If native harms are negligible, the victim set contracts to migrants and the undocumented underclass; epsilon over the restriction stays high, but the coalition structure, the transfer_function description, and the reading''s distinctive delta against its siblings change materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_harm_empirics, empirical, 'Empirical status of the citizen-victim channels unique to this reading.').

omega_variable(
    screening_exclusion_separability,
    'Is the genuine coordination residue (identity documentation, customs, epidemiological screening) structurally separable from exclusion by national origin?',
    'Compare throughput, safety, and health outcomes in regimes that screen without origin-exclusion (EU internal free-movement zone) against exclusionary regimes of comparable traffic.',
    'If separable, the regime is near-pure extraction wearing an administrative costume and the snare reading hardens; if inseparable, a slice of measured extraction is irreducible coordination cost and the classification softens toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(screening_exclusion_separability, conceptual, 'Whether the regime''s coordination and exclusion components can be prised apart.').

omega_variable(
    internalized_mobility_norms,
    'How much of the regime''s stability rests on internalized acceptance of the state''s monopoly on authorized movement rather than on active enforcement force?',
    'Compare attempted-movement and compliance behavior when enforcement capacity suddenly recedes at specific corridors (patrol withdrawals, state collapse) versus sustained low-enforcement equilibria.',
    'Higher internalization means the suppression_requirement series overstates the force needed going forward and predicts slow decay if enforcement is defunded; lower internalization predicts an immediate movement surge. The structural-versus-internalized split changes the drift trajectory, not the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_mobility_norms, empirical, 'Share of the regime''s suppression that is structural force versus internalized acceptance of immobility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1973, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1973, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement_basis(bord_tr_t1973, observed).
narrative_ontology:measurement(bord_tr_t1985, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement_basis(bord_tr_t1985, observed).
narrative_ontology:measurement(bord_tr_t1993, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1993, 0.34).
narrative_ontology:measurement_basis(bord_tr_t1993, observed).
narrative_ontology:measurement(bord_tr_t2001, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement_basis(bord_tr_t2001, observed).
narrative_ontology:measurement(bord_tr_t2011, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2011, 0.44).
narrative_ontology:measurement_basis(bord_tr_t2011, observed).
narrative_ontology:measurement(bord_tr_t2019, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2019, 0.5).
narrative_ontology:measurement_basis(bord_tr_t2019, observed).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(bord_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1973, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1973, 0.62).
narrative_ontology:measurement_basis(bord_be_t1973, observed).
narrative_ontology:measurement(bord_be_t1985, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement_basis(bord_be_t1985, observed).
narrative_ontology:measurement(bord_be_t1993, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1993, 0.74).
narrative_ontology:measurement_basis(bord_be_t1993, observed).
narrative_ontology:measurement(bord_be_t2001, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2001, 0.78).
narrative_ontology:measurement_basis(bord_be_t2001, observed).
narrative_ontology:measurement(bord_be_t2011, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2011, 0.82).
narrative_ontology:measurement_basis(bord_be_t2011, observed).
narrative_ontology:measurement(bord_be_t2019, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2019, 0.85).
narrative_ontology:measurement_basis(bord_be_t2019, observed).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2025, 0.87).
narrative_ontology:measurement_basis(bord_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1973, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement_basis(bord_su_t1973, observed).
narrative_ontology:measurement(bord_su_t1985, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement_basis(bord_su_t1985, observed).
narrative_ontology:measurement(bord_su_t1993, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement_basis(bord_su_t1993, observed).
narrative_ontology:measurement(bord_su_t2001, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2001, 0.72).
narrative_ontology:measurement_basis(bord_su_t2001, observed).
narrative_ontology:measurement(bord_su_t2011, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2011, 0.8).
narrative_ontology:measurement_basis(bord_su_t2011, observed).
narrative_ontology:measurement(bord_su_t2019, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2019, 0.86).
narrative_ontology:measurement_basis(bord_su_t2019, observed).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2025, 0.88).
narrative_ontology:measurement_basis(bord_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'border legitimacy' decomposes into three structurally distinct constraints — one per reading of the kernel. All three fix epsilon's referent to the same standing border-control arrangement; they differ in the epsilon authored over it and in victim-set membership (this reading alone places current citizens inside the victim set via the underclass and crowding-out mechanisms). Upstream/downstream: the freedom_of_movement_reading's rights jurisprudence exerts structural pressure on the humanitarian_obligation_reading's boundary-drawing, while the sovereignty_reading supplies the positive-law doctrine the regime's operation vindicates. Every family member links the others via affects_constraints; orphaning any one would hide the cross-reading deltas that carry the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
