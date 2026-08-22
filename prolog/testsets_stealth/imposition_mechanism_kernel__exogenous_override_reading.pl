% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: Coercive Norm Imposition Regime (Exogenous Override Reading)
 *   domain: historical sociology / state formation / cultural authority
 *
 * SUMMARY:
 *   Between 1868 and 1894 the Meiji state imposed a package of new norms on a
 *   heterogeneous territory of former domains: conscription (1873), a cash
 *   land tax (1873), compulsory schooling, the household registry, the
 *   Gregorian calendar (1873), uniform local government, dress and hairstyle
 *   edicts, and the disestablishment of the temples and status corporations
 *   that had previously organized cultural life. This story instantiates ONE
 *   reading of the imposition_mechanism_kernel: the
 *   exogenous_override_reading, under which these norms were imposed by state
 *   coercion and their legitimacy derived from the state's monopoly on
 *   violence rather than from prior cultural acceptance. Compliance under
 *   this reading tracks enforcement presence; where monitoring thinned,
 *   compliance was performed rather than lived. The epsilon referent is the
 *   standing coercive arrangement itself, the edict-and-enforcement regime,
 *   assessed by this reading's own lights; the sibling readings
 *   (endogenous_climb, hybrid_legitimation) are separate constraints with
 *   their own epsilon and beneficiary structures, linked through
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope (a genuine coordination function,
 *   meaning uniform law, tax, schooling, and time discipline solved real
 *   collective-action problems, fused with asymmetric extraction through the
 *   same structure), while the authored metrics describe substantially
 *   extractive, actively enforced operation. The engine measures the
 *   divergence per seat; the claim is not reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - meiji_central_state: agenda-setter (institutional/mobile) — drafts the edicts, builds the police-court-school enforcement apparatus, collects the taxes and conscripts; can adjust individual edicts but has staked international legitimacy on the core program
 *   - ex_samurai_class: primary payer with secondary beneficiary position (organized/identity_locked) — stipends commuted, swords banned, status order dismantled; a fraction absorbed into the army, police, and ministries that enforces the same edicts; armed resistance (Satsuma 1877) crushed
 *   - peasant_communities: primary payer (powerless/trapped) — cash land tax regardless of harvest, conscription quotas, school costs, registry-bound; thousands of village uprisings in the 1870s suppressed
 *   - urban_merchants_new_professionals: primary beneficiary (moderate/mobile) — unified national market, standardized currency and contracts, credentialed labor pool
 *   - local_administrative_elites: local enforcers with a payer position (moderate/constrained) — administer tax and conscription lists for the center, collect office and salary, absorb village anger, have lost autonomous authority
 *   - traditional_cultural_authorities: excluded (moderate/identity_locked) — temple networks, Confucian academies, village ritual specialists displaced by the new system; not consulted; objection enters as petition and passive noncompliance
 *   - historical_sociologists: analytical observer — reconstruct the legitimation mechanism from enforcement records and reception data; hold competing readings of the same record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.72).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "Coercive Norm Imposition Regime (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical sociology / state formation / cultural authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '4ce5031f-076b-4c55-9117-2da5651b255a').
narrative_ontology:cs_kernel_codification('4ce5031f-076b-4c55-9117-2da5651b255a', distributed).
narrative_ontology:cs_authority_grounding('4ce5031f-076b-4c55-9117-2da5651b255a', distributed).
narrative_ontology:cs_reading_relation('4ce5031f-076b-4c55-9117-2da5651b255a', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ce5031f-076b-4c55-9117-2da5651b255a', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('4ce5031f-076b-4c55-9117-2da5651b255a', foundational, legitimacy_from_violence_monopoly).
narrative_ontology:cs_axiom_status(legitimacy_from_violence_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('4ce5031f-076b-4c55-9117-2da5651b255a', legitimacy_from_violence_monopoly, empirically_contingent).
narrative_ontology:cs_axiom('4ce5031f-076b-4c55-9117-2da5651b255a', secondary, coercion_precedes_acceptance).
narrative_ontology:cs_axiom_status(coercion_precedes_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('4ce5031f-076b-4c55-9117-2da5651b255a', coercion_precedes_acceptance, empirically_contingent).
narrative_ontology:cs_reference_frame('4ce5031f-076b-4c55-9117-2da5651b255a', violence_monopoly_norm_regime).
narrative_ontology:cs_drift_state('4ce5031f-076b-4c55-9117-2da5651b255a', contemporary_reception_studies_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4ce5031f-076b-4c55-9117-2da5651b255a', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, meiji_central_state).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, urban_merchants_new_professionals).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, local_administrative_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, ex_samurai_class).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, peasant_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_cultural_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ex_samurai_class).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, local_administrative_elites).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__exogenous_override_reading, top_down_modernization_doctrine).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__exogenous_override_reading, violence_monopoly_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and promulgates the reform edicts (conscription, the cash land tax, compulsory schooling, household registry, calendar, dress codes), builds the police, courts, school inspectorate, and garrison network that enforce them, and receives the taxes, conscripts, and administrative legibility the system yields. It can repeal or soften individual edicts, and did relax several sumptuary and status rules, but has staked its international standing on the core program, so wholesale abandonment is not a live option from where it stands.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, meiji_central_state, agenda_setter,
    institutional, generational, mobile, national).

% Has its stipends commuted to bonds, its swords banned, and the status order its identity was built around dismantled by edict. A substantial fraction is recruited into the new army, police, and ministries, the very machinery administering the edicts, which gives part of the class a stake in the arrangement it is simultaneously losing under. Its organized resistance, the 1877 Satsuma rising, is defeated; after that, exit means shedding the identity itself, joining the bureaucracy, or poverty.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ex_samurai_class, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, ex_samurai_class, beneficiary).

% Pay a land tax fixed in cash regardless of harvest, supply conscript sons, fund and sometimes build the compulsory schools, and reorganize village life around the registry and the Western calendar. Thousands of village uprisings in the 1870s are put down by garrisons and police. Exit means flight, and the household registry is designed to catch exactly that.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, peasant_communities, payer,
    powerless, biographical, trapped, regional).

% Gain a unified national market with standardized currency, weights, contracts, and a single legal code, plus a credentialed labor pool from the school system and the new professions it staffs. They pay the same taxes as everyone else, but standardization is the precondition of their business, and their mobility lets them concentrate in the cities where both enforcement and opportunity are densest.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, urban_merchants_new_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Former village headmen and domain officials who now collect the tax, maintain conscription lists, and enforce school attendance on behalf of the center, drawing salaries and holding local office for doing it. They enforce rules they did not write, absorb the village anger aimed at the center, and have lost the autonomous authority their predecessors exercised; stepping out of the administrative role means losing office, income, and standing in one motion.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, local_administrative_elites, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, local_administrative_elites, payer).

% Temple networks, Confucian academies, and village ritual specialists whose control over learning, calendars, funerals, and life-cycle rites is taken over by the new state system. They were not consulted in drafting the reforms; temple lands and legal privileges are stripped in the early 1870s. Their objection enters the record as petitions and passive noncompliance, and their authority has nowhere institutional to go.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_cultural_authorities, excluded,
    moderate, generational, identity_locked, regional).

% Reconstruct how the new norms gained legitimacy from enforcement records, rebellion statistics, school attendance data, tax arrears, and studies of everyday reception. They hold competing readings of the same archive, publish against each other, and decide nothing.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, meiji_central_state).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes law, taxation, schooling, timekeeping, military obligation, and civil status across a territory of former domains with incompatible local regimes, solving the coordination problem of governing and defending a newly unified nation with one administrative language.
% TRANSFER_FUNCTION: Moves cash (a land tax fixed in money and collected regardless of harvest), labor (conscription and school service), and cultural and juridical authority (from temples, status corporations, and village institutions to the central state) from rural communities and the former warrior class to the central state and the commercial-professional classes aligned with it.
% ABSENT_VOICES: Traditional cultural authorities, including temple networks, Confucian academies, and village ritual specialists, were not consulted; their subsidies and legal privileges were simply stripped. Village communities learned of conscription and the cash tax by edict. Ex-samurai objections entered the record chiefly as armed rebellion, which was then criminalized, converting political voice into enforcement targets.
% DISAPPEARANCE_RATIONALE: Under this reading, if the edict-and-enforcement regime vanished overnight, the norm package would largely collapse rather than persist: cash tax collection, conscription, school attendance, and registry-bound status all depended on the state's monitoring and coercive capacity, and the suppressed alternatives (village ritual time, the samurai status order, temple authority) would resurface. The fiscal-military state would lose its extractive base within a harvest cycle, and the national market built on standardized currency and contracts would fragment back toward regional arrangements.
% FOUNDING_PROBLEM: A semi-colonial threat environment, meaning unequal treaties, gunboat diplomacy, and the visible colonization of neighboring territories, combined with a fragmented feudal territory: the new government needed rapid fiscal-military centralization and a legible, uniform population to negotiate treaty revision and avoid the fate it watched befall its neighbors.
% FOUNDING_PROBLEM_CORROBORATION: The threat itself is corroborated from outside the benefiting parties: Western diplomatic archives, the unequal-treaty record, and contemporaneous foreign observers (advisors, journalists, naval officers) all attest the semi-colonial pressure was real and acute. No source outside the benefiting parties, however, attests that this coercive package, rather than a gradualist or negotiated path, was necessary to meet it; the necessity claim rests on the state's own retrospective narrative and is disputed in the historiography, which is why the status is authored as live for the interval rather than dead.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.72 at interval end because the package moved cash (a land tax fixed in money and collected regardless of harvest), labor (conscription), and cultural authority from villages and status groups to the center, with the rate of transfer set unilaterally by edict. Suppression is 0.78 and is authored as a raw structural property, NOT scaled by power or scope (the engine owns any scaling), because persistence depended on criminalizing alternatives: sword bans, press and assembly restrictions, registry surveillance, and the military defeat of the largest rebellion. Theater is 0.28: the enforcement was real, but a growing share of compliance was performative where monitoring lapsed (Western dress at official ceremonies, attendance registers kept for inspectors), which the rising theater series tracks. Accessibility_collapse is 0.55: old practices were criminalized, not erased; they persisted underground and in village life, and a minority exited by migration, so alternatives were suppressed rather than annihilated. Resistance is 0.72: the 1870s saw thousands of village uprisings, conscription riots, and school burnings, and in 1877 an organized samurai army; coalition power among powerless villages was real but insufficient against the state's centralized violence, which is why resistance is high without being effective. All three metric series run on one shared time grid (1868, 1873, 1877, 1881, 1885, 1890, 1894) so no metric is sampled against another's end-state, and the final values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute different types from the same structure. From the central state's position the arrangement is a survival program it built under existential external pressure: the edicts are the price of treaty revision and of not being colonized, and enforcement is simply administration. From the ex-samurai and village positions the same edicts are dispossession, meaning wealth, sons, and cultural authority taken by a center that never asked, with compliance purchased by garrisons and policemen. The excluded cultural authorities never entered the conversation at all; their absence is part of the structure, not a gap in it. The engine computes this per-seat divergence from power, exit, and role; this commentary only names it.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiaries (central state, urban commercial-professional classes, local administrative elites) derive low d, meaning the arrangement subsidizes them: the state receives the taxes, conscripts, and legibility; merchants receive the national market the standardization creates; local elites collect office and salary for administering it. The declared victims (ex-samurai, peasant communities, traditional cultural authorities) derive high d: they bear the transfer with trapped or identity-locked exit. The two dual-positioned agents sit mid-structure: ex-samurai are payers whose secondary beneficiary role (state offices) partially offsets, and local elites are enforcers who also lost their autonomous authority. No directionality overrides are authored: the derivation from declared roles plus exit options already captures these relationships, and the dual positions are declared as secondary roles rather than overridden.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Calling the arrangement a pure snare would erase the genuine coordination it performed: uniform law, a national market, mass literacy, and a single calendar solved collective-action problems no village coalition could solve alone. Calling it a rope would erase the identifiable victims, meaning the crushed rebellions, the disestablished temples, the stipend-less samurai, and the fact that compliance tracked enforcement rather than acceptance. Tangled rope holds both halves. On mandatrophy: the founding problem (fiscal-military survival under semi-colonial pressure) was live throughout the interval and substantially resolved by its end, so mandatrophy_resolved is NOT declared; the mandate still functioned within the window. Whether the arrangement atrophies toward performance or hardens into pure rent collection once the emergency passes is a post-1894 question outside this interval; the R5 mismatch consumer (status x verdict) is the hook that would flag it if the arrangement persists after the founding problem dies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (exogenous_override) of the imposition_mechanism_kernel; what would the sibling readings change about the constraint''s structure?',
    'Adjudication by comparative historiography: enforcement records, rebellion data, and post-enforcement compliance persistence would shift the balance among the exogenous_override, endogenous_climb, and hybrid_legitimation readings of the same kernel.',
    'Under endogenous_climb, the same norm package would carry low epsilon (voluntary adoption, no imposition victims); under hybrid_legitimation, epsilon sits mid-range with a partially offset extraction structure. This file authors only the exogenous reading''s structure; the siblings are separate constraints, not measurement parameters of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer frame: this story instantiates one reading of the imposition-mechanism kernel; sibling readings are separate constraints.').

omega_variable(
    compliance_disposition_ambiguity,
    'Was compliance under the coercive regime genuinely conditional on state monitoring (this reading''s premise), or did internalized acceptance accumulate within the interval?',
    'Compliance trajectories in districts where enforcement capacity thinned (budget cuts, garrison redeployments): persistence without monitoring supports internalization; lapse supports the monitoring-conditional premise.',
    'If acceptance accumulated, effective suppression falls, epsilon falls, and the reading drifts toward hybrid_legitimation; if compliance lapsed wherever monitoring lapsed, the exogenous reading''s epsilon is confirmed or raised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_disposition_ambiguity, empirical, 'Whether compliance was monitoring-conditional or increasingly internalized over the interval.').

omega_variable(
    coercion_necessity_counterfactual,
    'Was the coercive package necessary to the founding problem (fiscal-military survival under semi-colonial pressure), or would gradualist or negotiated paths have sufficed?',
    'Comparative analysis of other late modernizers that centralized with less coercion and their success or failure records, plus counterfactual fiscal modeling of the 1870s state budget.',
    'Necessity would shift weight toward a rope-like coordination framing of the same structure; dispensability would push toward a snare framing and raise the extraction verdict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_necessity_counterfactual, conceptual, 'Counterfactual necessity of the coercive path to the founding problem.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (police, criminal law, registry surveillance, garrisons) or internalized (habituated deference and fear persisting after patrols withdraw)?',
    'Post-enforcement-decay trajectory in specific localities: if suppression effects persist after the enforcement mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests; targets carry it after leaving the enforcement zone, and the payer seats'' classification shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the coercive norm regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 1868, 1894).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exogenous_override_reading_tr_t1868, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(exogenous_override_reading_tr_t1873, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1873, 0.18).
narrative_ontology:measurement(exogenous_override_reading_tr_t1877, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1877, 0.2).
narrative_ontology:measurement(exogenous_override_reading_tr_t1881, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1881, 0.24).
narrative_ontology:measurement(exogenous_override_reading_tr_t1885, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1885, 0.26).
narrative_ontology:measurement(exogenous_override_reading_tr_t1890, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement(exogenous_override_reading_tr_t1894, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1894, 0.28).

% Extraction over time
narrative_ontology:measurement(exogenous_override_reading_be_t1868, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1868, 0.55).
narrative_ontology:measurement(exogenous_override_reading_be_t1873, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1873, 0.66).
narrative_ontology:measurement(exogenous_override_reading_be_t1877, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1877, 0.71).
narrative_ontology:measurement(exogenous_override_reading_be_t1881, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1881, 0.73).
narrative_ontology:measurement(exogenous_override_reading_be_t1885, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1885, 0.74).
narrative_ontology:measurement(exogenous_override_reading_be_t1890, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1890, 0.73).
narrative_ontology:measurement(exogenous_override_reading_be_t1894, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1894, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(exogenous_override_reading_su_t1868, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1868, 0.45).
narrative_ontology:measurement(exogenous_override_reading_su_t1873, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1873, 0.62).
narrative_ontology:measurement(exogenous_override_reading_su_t1877, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1877, 0.75).
narrative_ontology:measurement(exogenous_override_reading_su_t1881, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1881, 0.78).
narrative_ontology:measurement(exogenous_override_reading_su_t1885, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1885, 0.8).
narrative_ontology:measurement(exogenous_override_reading_su_t1890, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1890, 0.77).
narrative_ontology:measurement(exogenous_override_reading_su_t1894, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1894, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'how did the Meiji norms gain legitimacy' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that cannot share one constraint: the endogenous_climb_reading (low epsilon; voluntary adoption, no imposition victims), this exogenous_override_reading (high epsilon; coercive transfer with identifiable victims), and the hybrid_legitimation_reading (mid epsilon; symbolic transfer plus incentives, partially offset extraction). Each is authored as its own file with its own beneficiaries, victims, and metrics; the family is linked through affects_constraints so evidence and contamination propagate across the readings. The downstream structure runs from the shared enforcement record (this reading's data) into the hybrid reading, which must price in the coercion this file documents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
