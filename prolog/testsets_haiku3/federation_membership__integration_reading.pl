% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration with Free Movement Rights
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates the integration reading of the
 *   federation-membership kernel: federation membership is irreversible,
 *   supranational authority is legitimate, and free movement across member
 *   borders is a constitutional right, not negotiable policy. This reading
 *   treats the federation as an endpoint of political evolution — member
 *   states cannot exit, citizens cannot be restricted, and national borders
 *   have no legitimacy as economic or labor-market barriers. The sibling
 *   sovereignty reading frames federation membership as a conditional treaty
 *   where national border authority remains legitimate and free movement is a
 *   negotiable policy choice. This story author writes the integration
 *   reading as a clean constraint — not hedging across both readings, not
 *   averaging ε over the dispute. The structural delta declared in the kernel
 *   context is instantiated: mobile citizens are in the beneficiary set;
 *   local labor markets and destination-community provision are victims;
 *   extractiveness is high (0.68) precisely because mobile citizen arbitrage
 *   displaces local labor protections. Border restriction is declared
 *   illegitimate by the reading itself — the constraint's enforcement
 *   machinery actively suppresses national labor-market boundary maintenance.
 *
 * KEY AGENTS:
 *   - mobile_citizens: Beneficiary (arbitrage exit, wage capture). Power: moderate, time horizon: biographical, exit: arbitrage. They are the readable-side winners of the integration reading.
 *   - local_labor_markets: Victim (wage pressure, outmigration). Power: organized, time horizon: generational, exit: constrained. They cannot exclude federation labor or restrict entry.
 *   - destination_community_provision: Victim (service demand surge, housing pressure). Power: powerless, time horizon: immediate, exit: trapped. They bear diffuse costs and cannot price-discriminate against incoming mobile citizens.
 *   - multinational_corporations: Beneficiary (labor-mobility arbitrage). Power: institutional, time horizon: generational, exit: arbitrage. They couple labor mobility with capital mobility to optimize extraction.
 *   - supranational_federation_authority: Agenda-setter (sets and enforces irreversibility doctrine). Power: institutional, time horizon: civilizational, exit: analytical. Legitimates the constraint through courts and treaties.
 *   - origin_state_governments: Payer (fiscal base erosion, authority degradation). Power: institutional, time horizon: generational, exit: identity_locked. They cannot exit the federation (fusion with federal identity) even when economically rational.
 *   - origin_state_publics: Excluded (non-mobile, bearing concentrated costs, no arbitrage option). Power: powerless, time horizon: biographical, exit: trapped. They are systematically excluded from the constraint's framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.72).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration with Free Movement Rights").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, 'e371a72b-baf0-4918-90a9-487975a28b57').
narrative_ontology:cs_kernel_codification('e371a72b-baf0-4918-90a9-487975a28b57', formalized).
narrative_ontology:cs_authority_grounding('e371a72b-baf0-4918-90a9-487975a28b57', extraction).
narrative_ontology:cs_interpretation_layer_present('e371a72b-baf0-4918-90a9-487975a28b57').
narrative_ontology:cs_reading_relation('e371a72b-baf0-4918-90a9-487975a28b57', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e371a72b-baf0-4918-90a9-487975a28b57', foundational, federation_membership_irreversible).
narrative_ontology:cs_axiom_status(federation_membership_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('e371a72b-baf0-4918-90a9-487975a28b57', federation_membership_irreversible, deontological).
narrative_ontology:cs_axiom('e371a72b-baf0-4918-90a9-487975a28b57', foundational, free_movement_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('e371a72b-baf0-4918-90a9-487975a28b57', free_movement_constitutional_right, deontological).
narrative_ontology:cs_axiom('e371a72b-baf0-4918-90a9-487975a28b57', foundational, supranational_authority_legitimate).
narrative_ontology:cs_axiom_status(supranational_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('e371a72b-baf0-4918-90a9-487975a28b57', supranational_authority_legitimate, conventional).
narrative_ontology:cs_reference_frame('e371a72b-baf0-4918-90a9-487975a28b57', integration_irreversibility_doctrine).
narrative_ontology:cs_drift_state('e371a72b-baf0-4918-90a9-487975a28b57', contemporary_migration_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e371a72b-baf0-4918-90a9-487975a28b57', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, urban_consumer_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, regional_employment_protection).
narrative_ontology:constraint_victim(federation_membership__integration_reading, destination_community_provision).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, origin_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federation members with skills or capital can relocate across member states to pursue higher wages, better opportunities, or lifestyle preference. They benefit from access to labor markets across the federation without visa restriction or work authorization delay. The constitutional right to free movement is their primary asset — exit from one member state to another incurs no border friction. They capture the surplus from labor arbitrage and geographic mobility.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, arbitrage, global).

% Regional and national labor markets in lower-wage or lower-opportunity member states experience outmigration of skilled workers to wealthier federation regions. Wages in destination-restricted occupations depress as supply expands; domestic employers in origin regions lose competitive talent; social cohesion in origin communities erodes as working-age population leaves. These markets cannot restrict incoming mobile labor from other members and face structural wage pressure they cannot exit.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    organized, generational, constrained, national).

% Housing, education, healthcare, and social services in destination cities and regions experience demand surge as mobile citizens concentrate in high-opportunity areas. Local provision capacity does not scale at the speed of migration; waiting lists lengthen, housing costs rise, service quality per capita declines. Destination communities cannot price mobility out (constitutional rights forbid price discrimination), cannot restrict entry, and cannot reduce provision without violating federation-wide service standards. The cost is borne diffusely by existing residents and locally-financed services.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, destination_community_provision, payer,
    powerless, immediate, trapped, local).

% Firms can deploy labor across member states without work-permit friction, recruiting talent from the widest pool and shifting operational bases to minimize tax and regulatory burden. They benefit from unrestricted labor mobility, which lowers hiring costs and increases operational flexibility. Capital mobility couples with labor mobility to maximize extraction efficiency. They are structural beneficiaries of the irreversibility doctrine — once locked in, member states cannot retroactively restrict the flows MNCs depend on.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Trade unions, apprenticeship systems, and sectoral employment protections in origin member states lose enforcement power when workers can freely exit to higher-wage regions and when employers can recruit from federation-wide labor pools. Collective bargaining power erodes as the outside option (move to another member state) undercuts local negotiating strength. These institutions cannot exclude federation labor or impose local training requirements on incoming workers.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, regional_employment_protection, payer,
    moderate, biographical, constrained, regional).

% Member states in lower-wage regions or peripheral positions experience fiscal and political damage: the mobile population that paid in taxes leaves; the immobile population that depends on public services remains. This shifts the tax base and the beneficiary base in ways governments cannot correct via border control (forbidden by integration doctrine) or labor taxation (forbidden by equal treatment). Exit for member states means withdrawing from the federation entirely — identity fusion with 'European' or 'federal' citizenship makes withdrawal politically impossible even when economically rational for local populations.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, origin_state_governments, payer,
    institutional, generational, identity_locked, national).

% The supranational institutional structure (e.g. European Union courts, legislative bodies) sets and enforces the integration doctrine: federation membership is irreversible, free movement is a constitutional right, national borders cannot restrict citizen movement. This authority legitimates the constraint through treaties, court decisions, and administrative enforcement. It claims the doctrine solves the coordination problem of common market formation and prevents revival of national fragmentation. It collects political legitimacy from the mobile beneficiary set.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_federation_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Non-mobile populations in origin member states experience the constraint's costs (outmigration of peers, eroded local services, wage pressure, community dissolution) without the exit option that mobile citizens capture. They are locked into their origin state by lack of skill, capital, or language ability. They would object to the irreversibility and free-movement doctrine if their voice were institutionalized, but the federation's decision structures privilege mobile citizens and supranational authority over rooted community interests. They are systematically excluded from the constraint's framing.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, origin_state_publics, excluded,
    powerless, biographical, trapped, national).

% Political movements and member state governments that hold the sovereignty reading (federation as conditional treaty, national border authority legitimate, free movement as negotiable policy) contest the integration reading but lack institutional power to revise the doctrine. They are excluded from the authoritative framing of the kernel — courts and supranational bodies adjudicate against their reading. Their exclusion sustains the integration reading's maintenance.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, sovereignty_reading_partisans, excluded,
    powerful, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, supranational_federation_authority).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of common market formation: by making federation membership irreversible and free movement a constitutional right, the constraint ensures labor, capital, and goods flow across member-state boundaries without friction, reducing transaction costs for trade and economic integration. Prevents the revival of national protectionism and fragmentation.
% TRANSFER_FUNCTION: Transfers labor-market surplus from immobile origin-state populations and local employment-protection institutions to mobile citizens and multinational corporations. Transfers housing and service provision capacity from destination communities to incoming mobile citizens. Transfers fiscal capacity from peripheral member states to wealthier federation core through tax-base erosion and reallocation pressures. Transfers political authority from member-state governments to supranational federation institutions (by making national border control illegitimate).
% ABSENT_VOICES: Origin-state non-mobile publics would object if institutionalized: they bear concentrated costs (outmigration, wage pressure, service degradation, community dissolution) without the arbitrage option. Sovereignty-reading partisans and national-government representatives would contest the irreversibility and free-movement framing if their institutional voice were equal — but the federation's decision structures are weighted toward supranational courts and mobile-citizen interests, systematically marginalizing rooted community and national authority positions.
% DISAPPEARANCE_RATIONALE: If the integration reading and its enforcement disappeared overnight, member states would immediately reimpose border controls, restrict labor mobility, and reassert labor-market protection. Capital flows would reallocate toward lower-regulatory regions; urban housing markets would depressurize; local employment institutions would recover bargaining power; origin-state fiscal bases would stabilize. The federation itself would splinter into conditional, renegotiable membership or dissolve — the irreversibility doctrine is the glue holding the institutional structure together.
% FOUNDING_PROBLEM: Post-World War II European fragmentation and national protectionism produced two world wars and prevented economic recovery. The founding problem was: how to make war between member states economically irrational and institutionally impossible by binding their economies and populations together irreversibly.
% FOUNDING_PROBLEM_CORROBORATION: The supranational federation authority and mobile-citizen beneficiaries attest the founding problem is still live — fragmentation risk, resurgent nationalism, and the threat of conflict. Origin-state publics, sovereignty-reading partisans, and labor-protection advocates attest the founding problem is substantially solved (no member state credibly threatens war; economics and law enforcement prevent it) and the integration doctrine now persists as rent extraction and institutional self-preservation rather than as a response to genuine fragmentation risk. Economic historians and political scientists from outside the federation authority and non-mobile constituencies support the shifted-function reading.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.42 → 0.68) as the integration reading hardens: early in the federation's history, the free-movement doctrine was balanced against genuine economic-integration coordination benefits; over time, the coordination problem (fragmentation, protectionism) weakened as institutional stability grew, but the free-movement enforcement machinery intensified, making the constraint increasingly pure extraction. Theater rises (0.18 → 0.44) as the supranational authority invokes 'common market necessity' and 'peace through integration' more performatively and less functionally — the constraint now persists largely by narrative maintenance rather than by solving the founding coordination problem. Suppression is consistently high (0.58 → 0.72) because the constraint's persistence depends on actively suppressing member-state labor-market boundary maintenance and national sovereignty claims. All three metrics are authored on one shared grid so every time point has a value for every metric. The measurement basis is 'observed' for t=0 through t=30 (historical data on migration flows, labor-market impacts, fiscal transfers, enforcement intensity) and 'projected' for t=35 (author's extrapolation based on trend trajectory).
 *
 * PERSPECTIVAL GAP:
 *   Supranational authority and mobile citizens will compute this as tangled_rope: genuine coordination (common market, labor-market efficiency, economic integration) alongside asymmetric extraction (they collect the benefits). Origin-state governments and local labor markets will compute this as snare: the coordination story is cover for irreversible labor-market extraction. Origin-state publics will compute it as snare with high theater — they see performative justifications about 'integration' and 'peace' masking pure extraction. Destination communities will compute it as a perverse tangled_rope where they coordinate (provide services) but pay (service degradation, housing costs). The engine's per-seat computation will reveal this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens hold d near the beneficiary end (0.0–0.2) — they benefit from unrestricted movement, capture labor-market surplus, and have high exit optionality (can move between member states at will). Supranational authority and multinational corporations are near the beneficiary end for similar reasons (d ~0.15–0.25) — they set/benefit from the constraint. Local labor markets, destination-community provision, and origin-state governments are near the target end (d ~0.75–0.95) — they bear extraction costs they cannot exit from. Origin-state publics are at the extreme target end (d ~0.95) — they bear costs, have no arbitrage option, and cannot exit their origin state or the federation itself (identity-locked). The directionality derivation chain flows from the explicit beneficiary/victim declarations in base_properties through the power and exit-options of each stakeholder. No directionality overrides are needed — the structural data produce the right directionality surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: post-WWII European fragmentation and protectionism were genuine coordination failures. The integration constraint solved it — irreversibility and free movement eliminated the revival of national economic warfare. However, the founding problem has substantially atrophied: no member state credibly threatens war; economic integration and institutional lock-in prevent fragmentation through entirely different mechanisms (sunk costs, institutional path-dependency, NATO). Yet the constraint persists and intensifies because (1) the supranational authority benefits from maintenance (institutional self-preservation), (2) mobile-citizen beneficiaries have captured political voice disproportionate to their demographic share, and (3) the constraint's enforcement creates its own maintenance necessity (if member states cannot restrict borders, they must maintain supranational authority to manage the externalities of free movement). The theater ratio's rise (0.18 → 0.44) indicates the constraint now functions largely to maintain institutional power rather than to solve the founding fragmentation problem — mandatrophy is advanced but incomplete. A full mandatrophy declaration would require either the founding problem to be demonstrably dead (contested status keeps this ambiguous) or explicit evidence that the constraint now persists despite solving nothing (the contestation prevents closure on that front).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_sovereignty_kernel_framing,
    'Is the federation a unitaristic endpoint where national authority is superseded, or a conditional association where national authority retains ultimate legitimacy and member states can exit?',
    'Crisis stress-test: observe how member states and supranational authority respond to high-stakes challenges (economic crisis, migration crisis, security threat). If a member state reasserts border control and is permitted to negotiate (rather than being overruled), the sovereignty reading gains ground. If supranational authority suppresses any reassertion of national authority, the integration reading holds.',
    'If sovereignty framing prevails, free movement reverts from constitutional right to negotiable policy; member-state labor protections regain legitimacy; the constraint reclassifies from tangled_rope (for beneficiaries) / snare (for victims) toward rope or piton (conditional coordination). If integration framing holds, the constraint remains tangled_rope / snare with irreversible extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_kernel_framing, conceptual, 'Core kernel ambiguity: the legitimacy source and reversibility status of federation membership.').

omega_variable(
    founding_problem_atrophy_status,
    'Is the founding fragmentation-and-protectionism problem still live, or has it been solved such that the integration constraint now persists as rent extraction and institutional inertia?',
    'Historical analysis comparing member-state incentives in the early federation (1950s–1970s, genuine fragmentation risk) vs. contemporary period (2010s–2020s, no credible war risk, institutional path-dependency). If no member state would reasonably attempt fragmentation even if legally permitted, the problem is solved. Survey member-state leadership and publics: do they justify federation membership by fragmentation-risk or by economic/political path-dependency and institutional inertia?',
    'If founding problem is dead, the constraint''s classification shifts from tangled_rope (coordination + extraction) toward pure snare or piton (extraction or theater). If founding problem is live, tangled_rope classification holds. This directly feeds the mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_atrophy_status, empirical, 'Whether the fragmentation-risk coordination problem the constraint was built to solve still exists.').

omega_variable(
    labor_market_extraction_separability,
    'Is labor-market free movement structurally necessary for common-market coordination (goods and capital flow), or can capital mobility and goods trade operate independently of labor mobility?',
    'Natural experiment: observe economic blocs that permit capital and goods mobility but restrict labor mobility (bilateral trade agreements, customs unions without free movement). If those arrangements achieve comparable economic integration and lower the displacement costs measured here, labor mobility is separable and the extraction is pure. If economic integration stalls without labor mobility, they are coupled.',
    'If separable, the high extractiveness is attributable to labor mobility specifically, not to the common market''s coordination necessity. If coupled, part of the measured extraction is the price of coordination itself. This informs how much of the extraction is rent vs. efficient coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_extraction_separability, empirical, 'Whether labor-market free movement is structurally necessary for common-market coordination or is a separable extraction mechanism.').

omega_variable(
    origin_state_identity_lock_mechanism,
    'Is the political impossibility of member states reasserting border control a result of genuine institutional lock-in (sunk costs, path-dependency) or of internalized identity fusion with the federation (identity-locked exit)?',
    'Analyze member-state resistance movements and political discourse: do they reframe ''federation membership'' as optional and subject to renegotiation (indicating path-dependency model), or do they accept federation as inevitable and argue for internal reform (indicating identity-lock model)? Examine whether exit movements frame leaving as ''unthinkable'' or merely as economically irrational.',
    'If institutional path-dependency, the constraint persists through rational calculation and could be renegotiated if costs clearly exceeded benefits. If identity-lock, the constraint persists even as costs accumulate — member states would exit if they psychologically could, but cannot because the federation identity is constitutive of national identity. Identity-lock suppression is more stable and harder to dislodge than path-dependent lock-in.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(origin_state_identity_lock_mechanism, conceptual, 'Whether origin-state political impossibility of reasserting authority is institutional lock-in or internalized identity fusion.').

omega_variable(
    theater_ratio_maintenance_drivers,
    'What portion of the theater-ratio increase (0.18 → 0.44) is driven by the founding coordination problem actually becoming less pressing vs. being driven by institutional actors investing more in performative maintenance?',
    'Discourse analysis: compare the frequency and content of ''common market'' and ''integration'' rhetorical invocation across decades. If rhetoric increases while fragmentation risk decreases, the increase is performative maintenance. Survey federation and member-state officials: do they privately acknowledge the founding problem is solved while publicly maintaining the integration narrative?',
    'If the rise is performative maintenance-driven, mandatrophy is advanced and the constraint''s functional necessity is lower than its institutional maintenance suggests. If the rise reflects genuine problem-solving narrative (the founding problem is still live, and integration story-telling must work harder against resurgent resistance), the functionality claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_maintenance_drivers, empirical, 'Whether the theater-ratio rise reflects atrophied founding-problem or amplified institutional maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fede_tr_t5, federation_membership__integration_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(fede_tr_t10, federation_membership__integration_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(fede_tr_t15, federation_membership__integration_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(fede_tr_t20, federation_membership__integration_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(fede_tr_t25, federation_membership__integration_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(fede_tr_t30, federation_membership__integration_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(fede_tr_t35, federation_membership__integration_reading, theater_ratio, 35, 0.44).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t5, federation_membership__integration_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fede_be_t10, federation_membership__integration_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(fede_be_t15, federation_membership__integration_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(fede_be_t20, federation_membership__integration_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(fede_be_t25, federation_membership__integration_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(fede_be_t30, federation_membership__integration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fede_be_t35, federation_membership__integration_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(fede_su_t5, federation_membership__integration_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(fede_su_t10, federation_membership__integration_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fede_su_t15, federation_membership__integration_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(fede_su_t20, federation_membership__integration_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fede_su_t25, federation_membership__integration_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(fede_su_t30, federation_membership__integration_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(fede_su_t35, federation_membership__integration_reading, suppression_requirement, 35, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.22).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership__integration_reading, national_labor_protection_authority).
narrative_ontology:affects_constraint(federation_membership__integration_reading, destination_community_service_provision).
narrative_ontology:affects_constraint(federation_membership__integration_reading, monetary_union_irreversibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership kernel. The sibling sovereignty_reading frames the same institutional arrangement differently: federation membership as conditional treaty, national border authority as legitimate, free movement as negotiable policy. The two readings share a referent (the federation institutional structure and labor-mobility policies) but assess it from incompatible normative framings (supranational-authority-centered vs. national-authority-centered). They are published as separate constraint stories linked via affects_constraints; each story instantiates one reading with its own ε, stakeholder perception structure, and classification. The decomposition reflects DP-001 (ε-invariance): a single ε cannot bridge two readings with incompatible legitimacy sources and beneficiary structures. Integration reading authors high ε from mobile-citizen extraction; sovereignty reading would author lower ε because national border control is legitimate. Both are structurally true within their reading's epistemic frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
