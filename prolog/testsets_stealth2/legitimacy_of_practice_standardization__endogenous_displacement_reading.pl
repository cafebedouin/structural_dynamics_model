% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Voluntarism Gate on Practice Standardization (Endogenous Displacement Reading)
 *   domain: political_history/institutional_change
 *
 * SUMMARY:
 *   Across the modernization record - calendar reform, dress codes, naming
 *   conventions, measurement standards - societies have argued about what
 *   makes a practice change real rather than merely proclaimed. This story
 *   authors one answer, the endogenous displacement reading: change is
 *   legitimate when it diffuses voluntarily, carried by perceived utility and
 *   cultural evolution, showing gradual adoption curves, regional variation,
 *   elite-to-mass sequencing, temporary friction, and a transitional double
 *   life. The arrangement under contest is the voluntarism gate itself: the
 *   standing rule that imposed standardization lacks legitimacy and tends to
 *   produce hollow compliance, while adopted standardization binds. Custodial
 *   authorities administer the authenticity judgment and collect the
 *   resulting veto power; communities hold the adoption decision; modernizing
 *   states must cultivate rather than command; private early adopters pay in
 *   concealment; cross-jurisdictional commerce pays in mismatch costs and
 *   collects brokerage profits; treaty partners demand alignment from outside
 *   the conversation. Claim and metrics are authored independently: the
 *   tangled_rope claim states the structure this reading believes true (a
 *   real coordination function bound to a real asymmetric burden), while the
 *   metric values describe the arrangement's actual operation as the record
 *   shows it. Epsilon's referent is the voluntarism-gated order as this
 *   reading holds it, assessed by this reading's own lights; decree-route
 *   outcomes belong to the sibling story, not to a second observable inside
 *   this one. KEY AGENTS (by structural relationship): -
 *   traditional_authorities: primary beneficiary and gate administrator
 *   (organized/identity_locked) - preside over the pace and license of change
 *   - local_practice_communities: collective beneficiary
 *   (moderate/constrained) - hold the adoption decision as bodies -
 *   modernizing_state_administrators: primary payer with secondary
 *   beneficiary position (institutional/constrained) - must cultivate rather
 *   than command - private_early_adopters: concentrated payer
 *   (moderate/trapped) - bear the double life -
 *   transnational_commerce_networks: payer with arbitrage offset
 *   (organized/arbitrage) - pay mismatches, harvest brokerage -
 *   international_treaty_partners: excluded demandant
 *   (institutional/constrained) - bear alignment costs with no seat -
 *   modernization_scholars: analytical observer - hold the comparative
 *   evidence the readings dispute
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.42).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.48).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Voluntarism Gate on Practice Standardization (Endogenous Displacement Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '98e2c310-ca6a-47ef-8dce-6e9de512de9d').
narrative_ontology:cs_kernel_codification('98e2c310-ca6a-47ef-8dce-6e9de512de9d', distributed).
narrative_ontology:cs_authority_grounding('98e2c310-ca6a-47ef-8dce-6e9de512de9d', practice).
narrative_ontology:cs_interpretation_layer_present('98e2c310-ca6a-47ef-8dce-6e9de512de9d').
narrative_ontology:cs_reading_relation('98e2c310-ca6a-47ef-8dce-6e9de512de9d', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('98e2c310-ca6a-47ef-8dce-6e9de512de9d', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('98e2c310-ca6a-47ef-8dce-6e9de512de9d', foundational, legitimacy_requires_voluntary_uptake).
narrative_ontology:cs_axiom_status(legitimacy_requires_voluntary_uptake, holdable).
narrative_ontology:cs_axiom_grounding('98e2c310-ca6a-47ef-8dce-6e9de512de9d', legitimacy_requires_voluntary_uptake, deontological).
narrative_ontology:cs_axiom('98e2c310-ca6a-47ef-8dce-6e9de512de9d', secondary, imposed_practice_yields_hollow_compliance).
narrative_ontology:cs_axiom_status(imposed_practice_yields_hollow_compliance, holdable).
narrative_ontology:cs_axiom_grounding('98e2c310-ca6a-47ef-8dce-6e9de512de9d', imposed_practice_yields_hollow_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('98e2c310-ca6a-47ef-8dce-6e9de512de9d', endogenous_diffusion_order).
narrative_ontology:cs_drift_state('98e2c310-ca6a-47ef-8dce-6e9de512de9d', contemporary_global_integration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('98e2c310-ca6a-47ef-8dce-6e9de512de9d', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, local_practice_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_administrators).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, private_early_adopters).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, transnational_commerce_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_administrators).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, transnational_commerce_networks).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffusion_of_innovations_model).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, endogenous_institutional_change_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clerics, elders, guild heads, and lineage heads who administer communal practice: they fix ritual calendars, certify proper dress, and judge which novelties count as organic growth versus foreign imitation. Their standing rests on being custodians of inherited forms; when adoption proceeds on their timetable they preside over change and keep their office. Stepping outside the custodial role would dissolve the basis of their authority, so resignation is not a live option from where they stand.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authorities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authorities, agenda_setter).

% Villages, congregations, and urban neighborhoods whose shared routines - feast days, wedding dress, mourning periods - mark belonging. They adopt or decline novelties as bodies, usually after neighboring communities have tested them. Leaving the community's practice frame entirely means losing the social fabric that organizes marriage, inheritance, and mutual aid, so exit is rarely taken even where individual members disagree with the majority's pace.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, local_practice_communities, beneficiary,
    moderate, generational, constrained, local).

% Ministries and reform cabinets that want unified calendars, standardized weights, uniform professional dress, and legible populations for taxation and administration. Under the prevailing legitimacy standard they may propose, subsidize, demonstrate, and wait, but not compel; official careers rise and fall faster than diffusion completes, so ministers often leave office before the change they championed matures. When they respect the standard their reforms outlast them; when they defy it they invite backlash that undoes their successors' work as well.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_administrators, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_administrators, beneficiary).

% Individuals - clerks, merchants' wives, students, younger sons - who find the incoming practice useful or fashionable and take it up before their surroundings do. They wear the new coat to town and the old robe at home, keep two calendars in one ledger, celebrate quietly. Open deviation draws ridicule, matchmaking penalties, or worse, so adoption runs ahead of acknowledgment; they carry both systems indefinitely and cannot fully join the new world without abandoning the old one's people.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, private_early_adopters, payer,
    moderate, biographical, trapped, regional).

% Merchant houses, shipping lines, and banking correspondents working across jurisdictions that keep different calendars, dress expectations, and measurement habits. Every mismatch is a cost - duplicated books, missed feasts, re-translated contracts - yet the same mismatches pay those who broker between systems: interpreters, conversion specialists, warehousemen who store goods across the date line of two new years. They press for uniformity and profit from the transition in the same breath.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, transnational_commerce_networks, payer,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, transnational_commerce_networks, beneficiary).

% Foreign ministries, standard-setting bodies, and treaty organizations that require aligned calendars, formats, and professional presentation for diplomacy, trade, and technical cooperation. They can offer loans, treaties, and prestige conditioned on alignment but hold no seat in the domestic argument over whether alignment is wanted; their demands arrive as external pressure, which the prevailing standard treats as precisely the kind of influence adoption must be free of.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, international_treaty_partners, excluded,
    institutional, generational, constrained, global).

% Historians and social scientists who compile adoption curves, map regional variation, and compare decree regimes with diffusion regimes across centuries. They publish the evidence the contending legitimacy doctrines argue over and hold no stake in any practice community's outcome.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authorities).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the durability problem of practice change: gating legitimacy on voluntary uptake ensures that changed practices are actually lived rather than merely professed, aggregates dispersed information about what works (uptake reveals utility), and spares communities the backlash cycle that follows imposition. Stated without evaluation of whether the gate is worth its costs.
% TRANSFER_FUNCTION: Moves discretion over practice change from decree-issuing authorities to adopting publics and their custodial gatekeepers; moves veto power over novelties to traditional authorities; moves transition costs onto early adopters (concealment, doubled bookkeeping) and onto cross-system brokers and treaty partners (mismatch costs, delayed alignment).
% ABSENT_VOICES: Private early adopters are present but muted - their testimony reads as unrepresentative taste, and open advocacy invites the very sanctions they avoid by concealing. Members of tightly surveilled communities who prefer change cannot voice it at all. International treaty partners hold no seat in the domestic legitimacy argument; their alignment demands arrive as external pressure, which the gate's own standard classifies as exactly the influence adoption must be free of.
% DISAPPEARANCE_RATIONALE: If the voluntarism gate vanished overnight, decree-based standardization would become presumptively legitimate: states would impose calendars, dress codes, and administrative formats wholesale, custodial veto power would evaporate, the double life would be replaced by compliance-or-punishment, and the adoption record would shift from gradual curves to step functions with whatever reversion followed. Every named seat's situation depends on the gate's existence.
% FOUNDING_PROBLEM: Modernizing states repeatedly discovered that decreed standardization failed: revolutionary France's decimal calendar was abandoned within twelve years, calendar decrees met riot and mockery, sumptuary and dress edicts produced evasion, and anti-ritual campaigns left practice intact beneath professed conformity. The gate was articulated as the remedy: route change through consent so that what changes stays changed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: diffusion-of-innovations research and comparative historical studies - disciplines with no stake in custodial jurisdiction - document higher persistence and lower reversion for voluntarily adopted practices, and the decree regimes' own archives corroborate it: each backtrack, each tolerated informal practice after formal decree, and each quiet re-legalization of what prohibition failed to displace is an admission filed by the imposing side.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.42 is assessed on the standing arrangement this reading holds up: a practice-change order gated on voluntary uptake. By this reading's own lights most of the order's burden is either the price of durability or filtering of fragile novelties, but three cost centers are real and acknowledged: custodial veto rents, the foregone coordination gains of slow alignment, and the double-life burden carried by private adopters. Suppression 0.48 records the sanction machinery - ridicule, matchmaking penalties, delegitimation of artificial reform, backlash against defying governments - while noting that adoption itself is never compelled; the gate suppresses imposers and public deviants, not private choice. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope. Theater 0.25 and rising: as lived observance thins, custodial authorities stage heritage performance to substantiate authenticity claims, so a growing share of maintenance activity is demonstrative rather than functional. Accessibility collapse 0.35: the rival legitimacy routes do not collapse under scrutiny - decree regimes kept working at cost, and domain partition kept describing much of the record - which is why the sibling readings survive as live constraints. Resistance 0.55: modernizing administrators, treaty partners, and chafing early adopters push against the gate continuously, and states periodically defy it and absorb the backlash. Coordination type identity_coordination: the gate's primary function is boundary maintenance - what counts as authentic practice, and who may change it - which is exactly the type's domain; no floor override is declared. All three temporal series run on one shared grid (t=0,10,20,30,40,50,60) so every metric is authored at every examined point; trajectories are monotone, not cyclical - accumulation, not oscillation - so no intermittent-reinforcement dynamic is claimed. suppression_requirement is tracked because the narrative specifically traces enforcement-capacity change: the sanction apparatus hardened as modernization pressure mounted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the custodial seat the gate is stewardship: change they preside over is change that lasts, and their office depends on the distinction between organic and imposed. From the modernizing administrator's seat the same gate is a gag rule on collective benefit: fiscal unification and administrative legibility wait on diffusion that outlasts careers. From the private adopter's seat it is a tax paid in concealment - two calendars, two wardrobes, two selves. Commerce sits astride the line, paying and profiting simultaneously. The scholar seat sees only the curves. The engine derives these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: custodial authorities sit nearest the beneficiary pole (the gate subsidizes their jurisdiction and veto), and local practice communities sit near-symmetric with a beneficiary lean (they hold the adoption decision and bear little imposed cost). Victim declarations drive high d: private early adopters are the most trapped targets (concealment costs with no exit), modernizing administrators are strong targets damped by their secondary beneficiary position (durable legitimacy when they comply), and transnational commerce networks sit near the middle - real mismatch costs offset by brokerage profits and arbitrage-grade exit. Treaty partners, excluded from the conversation, derive near-full-target d: they bear alignment costs with no seat. No directionality overrides were declared: the beneficiary/victim declarations plus exit options already separate the seats, and the available override granularity (per power atom) is too coarse to adjust one seat without distorting another at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - imposed standardization producing riot, hollow compliance, and reversion - is live, so there is no resolved mandate to retire and no sunset to declare. Classification discipline cuts both ways: reading the gate as pure coordination ignores that its gains accrue to a capturable seat (custodial authorities collect the veto rents, hence gain_flow names them) and that fixing is prohibitive; reading it as pure extraction ignores the documented filtering and durability functions that even hostile seats rely on when their own reforms need to stick. The tangled_rope claim holds both halves. The receipt surface flags the drift risk to monitor: a captured gain flow with prohibitive fixing cost is the signature along which hybrids decay toward extraction, and the gatekeeper_rent_share omega is the instrument that would detect it. Enforcement is distributed social sanction rather than a single administrator, which blocks the degraded-inertia reading: there is no office that could quietly retire the gate, only a coalition that would have to stop enforcing it. Coalition potential among the payer seats exists on paper - modernizers, early adopters, and commerce all chafe - but the seats are dispersed, differently timed, and split by the very identity commitments the gate maintains, so coalition power remains latent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_legitimacy_criterion,
    'This story instantiates the endogenous_displacement_reading of the kernel legitimacy_of_practice_standardization; would the observed record of calendar and dress change - gradual curves, regional variation, transitional double lives - be equally generated by the sibling readings (exogenous_override_reading, dual_practice_equilibrium_reading), and what evidence discriminates between them?',
    'Compare jurisdictions matched on practice content but varying decree enforcement intensity: strict-decree regimes converging without reversion support the exogenous reading; persistent parallel practice under both decree and liberty supports the dual-equilibrium reading; S-curve displacement tracking voluntary-first adoption supports this reading.',
    'If the record under-determines the readings, the gate''s necessity claim weakens and part of the coordination function attributed here may belong to the dual-equilibrium arrangement instead; the family may need re-partitioning and this reading''s classification could shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_legitimacy_criterion, conceptual, 'Which reading of the practice-legitimacy kernel the adoption record actually supports.').

omega_variable(
    gatekeeper_rent_share,
    'How much of the burden this arrangement imposes is protective filtering of fragile novelties versus veto rent collected by custodial authorities defending jurisdiction?',
    'Trace the later fate of changes custodians blocked: novelties that failed everywhere regardless of adoption route indicate filtering; novelties that succeeded once adopted elsewhere, or after custodial turnover, indicate rent.',
    'A dominant rent share pushes the arrangement toward extraction with coordination cover and confirms the captured gain_flow as the operative structure; a dominant filtering share supports the hybrid reading with modest excess burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_rent_share, empirical, 'Protective filtering versus custodial veto rent as the source of the gate''s burden.').

omega_variable(
    double_life_transience,
    'Is the double life of private adopters a transitional phase that closes as diffusion completes, as this reading predicts, or a stable equilibrium of public conformity and private divergence?',
    'Longitudinal cohort tracking of public/private practice divergence across generations in matched communities, comparing communities at different stages of diffusion.',
    'If stable, the reading''s core prediction fails, the voluntarism gate loses its durability justification, and weight shifts to the dual-practice-equilibrium reading; if transient, the gate''s coordination claim is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_life_transience, empirical, 'Whether the double life is transitional, as this reading''s structural delta asserts, or a persistent equilibrium.').

omega_variable(
    invented_tradition_framing,
    'How much of what custodians defend as inherited organic practice is itself a recent construction, such that the gate protects curated heritage rather than continuous transmission?',
    'Genealogical audit of defended practices: dated origins, documentary breaks, and revival moments versus continuous attestation across the defended lineage.',
    'If much defended practice is invented, the gate''s protective function thins toward interest defense, effective burden rises, and the arrangement drifts toward extraction administered under authenticity cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invented_tradition_framing, conceptual, 'Whether the tradition the gate defends is transmitted or constructed.').

omega_variable(
    internalized_authenticity_norms,
    'Is the sanction keeping public practice aligned with communal expectation primarily external (community penalty) or internalized (an authenticity conscience that persists where custodial power has faded)?',
    'Compare adoption latency and open-deviation rates between communities with intact custodial authority and communities where custodial offices have lapsed.',
    'If internalized, the arrangement''s coercive footprint is understated by external-sanction measures and persists after custodial decline; the suppression attributable to the arrangement is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_authenticity_norms, empirical, 'Structural versus internalized suppression mechanism sustaining the gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lops_edr_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lops_edr_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(lops_edr_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(lops_edr_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(lops_edr_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(lops_edr_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(lops_edr_tr_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(lops_edr_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lops_edr_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(lops_edr_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(lops_edr_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(lops_edr_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(lops_edr_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(lops_edr_be_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(lops_edr_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lops_edr_su_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(lops_edr_su_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(lops_edr_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(lops_edr_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(lops_edr_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.47).
narrative_ontology:measurement(lops_edr_su_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when is practice standardization legitimate?' decomposes into three structurally distinct constraints per the epsilon-invariance rule: this endogenous_displacement_reading (legitimacy from voluntary uptake; moderate epsilon; victims are gate-bound modernizers, concealed early adopters, and mismatch-paying commerce), the exogenous_override_reading (legitimacy from decree for collective benefit; different victim set - practice-holding populations facing compulsion), and the dual_practice_equilibrium_reading (domain partition between state and traditional authority; different beneficiary structure entirely). Each reading gets its own epsilon, its own stakeholders, and its own classification; the family is linked through affects_constraints. Upstream/downstream: the documented failures of decree regimes are the evidence this reading cites, so the exogenous story sits upstream of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
