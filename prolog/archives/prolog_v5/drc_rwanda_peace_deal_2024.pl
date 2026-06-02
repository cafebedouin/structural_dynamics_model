% ============================================================================
% CONSTRAINT STORY: drc_rwanda_peace_deal_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_drc_rwanda_peace_deal_2024, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: drc_rwanda_peace_deal_2024
 *   human_readable: US-Brokered DRC-Rwanda De-escalation Framework (2024)
 *   domain: geopolitical/conflict_resolution
 *
 * SUMMARY:
 *   The 2024 US-brokered DRC-Rwanda de-escalation framework represents a
 *   constraint that simultaneously functions as coordination mechanism for
 *   conflict reduction and as extraction mechanism for mineral resources and
 *   territorial control. The framework was negotiated to address years of
 *   proxy warfare in eastern DRC, with Rwanda supporting the M23 rebel group
 *   while denying involvement. The agreement nominally commits Rwanda to
 *   military withdrawal, pledges US security guarantees for DRC sovereignty,
 *   and establishes monitoring mechanisms through UN and regional bodies.
 *   However, the structural incentives embedded in the framework create a
 *   Tangled Rope dynamic: Rwanda gains international legitimacy and sanctions
 *   relief while maintaining de facto control over mineral-rich eastern
 *   territories through proxy forces; the US gains a diplomatic win and
 *   regional stability narrative without military commitment; the DRC
 *   government gains temporary protection but loses agency in determining
 *   terms; DRC civilians remain trapped in occupation. The theater ratio
 *   (0.68) reflects the performative elements: frequent international
 *   monitoring visits, ceremonial compliance gestures, and narrative
 *   management by all parties, while underlying power dynamics and resource
 *   extraction persist.
 *
 * KEY AGENTS:
 *   - United States: Institutional broker (institutional/arbitrage) — benefits from conflict resolution narrative and regional influence; can shift commitment with minimal cost
 *   - Rwanda Armed Forces: Organized extractor (organized/constrained) — captures resource control and buffer zone while appearing compliant; faces international pressure but maintains strategic depth through proxy structures
 *   - DRC Government: Trapped sovereign (moderate/constrained) — nominally gains security guarantees but surrenders agency to external enforcement; depends on US commitment for territorial integrity
 *   - DRC Civilian Population: Primary victim (powerless/trapped) — remains in conflict zone with no exit; subject to occupation, displacement, and resource extraction; framework provides no direct protection mechanisms
 *   - M23 Rebel Group and Proxy Forces: Armed non-state actor (powerful/mobile) — status ambiguous under framework; unclear whether disarmament or integration into national forces occurs; maintains territorial control
 *   - African Union / SADC: Regional institutional observer (institutional/constrained) — nominal mediation role; real authority displaced by US brokerage; maintain missions through inertia despite limited enforcement capacity
 *   - International Humanitarian Organizations: Monitoring networks (organized/constrained) — operate under framework's security guarantees; theater ratio high due to performative verification missions
 *   - Extractive Industry Interests: Economic beneficiaries (powerful/arbitrage) — international mining companies gain resource access under framework; benefit from stability and diminished competition from war profiteers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(drc_rwanda_peace_deal_2024, 0.58).
domain_priors:suppression_score(drc_rwanda_peace_deal_2024, 0.62).
domain_priors:theater_ratio(drc_rwanda_peace_deal_2024, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, extractiveness, 0.58).
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(drc_rwanda_peace_deal_2024, tangled_rope).
narrative_ontology:human_readable(drc_rwanda_peace_deal_2024, "US-Brokered DRC-Rwanda De-escalation Framework (2024)").
narrative_ontology:topic_domain(drc_rwanda_peace_deal_2024, "geopolitical/conflict_resolution").

domain_priors:requires_active_enforcement(drc_rwanda_peace_deal_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, united_states_regional_influence).
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, rwanda_armed_forces).
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, multinational_extractive_interests).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, drc_civilian_population).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, drc_territorial_integrity).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, m23_disarmed_combatants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DRC CIVILIANS (SNARE) — Powerless, trapped in conflict zone with no exit. Bear full extraction through displacement, resource predation, and loss of territorial sovereignty. Framework provides no mechanism for civilian voice or protection. Maximum coercion, zero coordination benefit.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DRC GOVERNMENT (TANGLED ROPE) — Constrained power: nominally sovereign but unable to enforce territorial control without external support. Framework offers coordination benefit (ceasefire terms) but enforces asymmetric extraction: Rwanda retains military advantages and resource access while DRC receives promises of protection dependent on US commitment. Active enforcement by US presence required to maintain constraint.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNITED STATES (ROPE) — Institutional power with arbitrage exit (can withdraw support, shift regional strategy, or broker alternative arrangements). Framework provides coordination benefit for US: establishes regional stability narrative, legitimizes US mediation authority, creates diplomatic win without military commitment. Experiences constraint as pure coordination mechanism.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RWANDA (TANGLED ROPE) — Organized military power with constrained exit (faces international pressure but retains strategic depth through M23 proxy and resource control). Framework offers coordination (ceasefire terms, legitimacy) AND extraction (maintains de facto control of eastern DRC resources and buffer zone while appearing to comply with international norms). Suppression enforced through threat of sanctions or US policy shift.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: AFRICAN UNION / SADC (PITON) — Nominal institutional role but degraded enforcement capacity. Regional conflict resolution mechanisms persist (SADC peacekeepers, AU mediation) largely through inertia despite weak enforcement power. Theater ratio high: ceremonial mediation and monitoring missions perform regional responsibility while practical power lies with external brokers (US, France). Limited real coordination function; maintenance through institutional ritual.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: HUMANITARIAN NGOS / UN MISSIONS (SCAFFOLD) — Organized agents with constrained exit (mandated to operate in conflict zones; political dependency on major powers). Framework creates temporary coordination structure for humanitarian access and monitoring. Suppression tolerable if it declines over time — sunset presumed when conflict genuinely de-escalates. Theater ratio moderate: NGOs perform verification and monitoring functions but depend on government permission and security guarantees that the framework provides only temporarily.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — From civilizational perspective on interstate dynamics, the framework appears to reflect immutable structural facts: great powers mediate regional conflicts; smaller states must accept external authority to gain stability; resource competition and territorial ambiguity are inherent to African geopolitics. However, this naturalizes contingent institutional arrangements — the specific choice of US brokerage, the terms favoring Rwanda, the absence of DRC agency are not natural laws but political decisions. Engine detects as false natural law.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(drc_rwanda_peace_deal_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(drc_rwanda_peace_deal_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(drc_rwanda_peace_deal_2024, TR),
    TR >= 0.70.

:- end_tests(drc_rwanda_peace_deal_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The framework is not maximally extractive because it provides genuine coordination function (ceasefire, reduced direct conflict, humanitarian access) that benefits multiple parties including some DRC actors. However, extraction is substantial because the coordination benefits accrue unevenly — Rwanda retains military advantage and resource control while DRC surrenders agency, and the framework legitimizes this asymmetry through international certification. Initial extractiveness (0.48) reflects pre-framework baseline conflict with uncoordinated violence; post-framework extractiveness (0.58) reflects organized extraction hidden within coordination machinery. Suppression (0.62): Moderate-high. DRC government and civilians have severely limited alternatives — exit options are constrained by geography (landlocked), military weakness, and lack of patron powers willing to offer better terms. Rwanda faces international pressure but retains enough strategic depth (proxy forces, resource wealth, regional allies) to maintain options. Theater ratio (0.68) reflects significant performative content: ceremonial compliance gestures, monitoring missions that verify compliance without enforcing consequences, diplomatic announcements of progress that obscure unchanged power dynamics. Theater has increased over the 12-month interval as the initial framework excitement has plateaued and routine performance (meetings, reports, patrols) has become theatrical rather than functional.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence reveals the constraint's hybrid nature. The US broker experiences coordination (Rope) because they are solving the problem they defined (regional stability). Rwanda experiences mixed coordination-extraction (Tangled Rope) because the framework simultaneously constrains their military ambitions and legitimizes their territorial gains. The DRC government experiences extraction-with-coordination (Tangled Rope) because they gain nominal protection while losing agency. DRC civilians experience pure extraction (Snare) because the framework's benefits never reach them — they remain displaced, occupied, and subject to resource predation. Regional institutions see degraded ceremony (Piton) because their mediation role is performed for legitimacy while real power lies with the US. The analytical observer risks false naturalization (Mountain) — seeing great-power mediation of regional conflicts as inevitable structural law — when the specific US role, the terms favoring Rwanda, and the exclusion of DRC agency are contingent political choices. The perspectival gap models how the same structural arrangement can be coordination, extraction, theater, and natural law depending on where the observer sits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural position — their power level, exit options, and relationship to the extraction flow. The US as institutional actor with arbitrage exit options (can redirect attention, withdraw support, shift mediation to others) experiences low derived d (~0.10), yielding negative or minimal χ. Rwanda as organized actor with constrained exit (faces sanctions risk, international isolation if framework fails) but beneficiary status (retains resource access and buffer zone) experiences moderate d (~0.35-0.40), yielding moderate χ. DRC government as moderate power with constrained exit (no patron alternative, landlocked geography, military weakness) and victim status experiences high d (~0.65-0.70), yielding high χ. DRC civilians as powerless with trapped exit experience maximum d (~0.95), yielding maximum χ. The engine's directionality derivation automatically accounts for this variation through the sigmoid mapping of d to f(d). No overrides are required — the structural data (beneficiary/victim + power + exit) determines d accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The DRC-Rwanda framework exhibits classic mandatrophy between the coordination-function narrative (peace deal) and the extraction-function reality (resource and territorial control maintained under new legitimacy). Mandatrophy is NOT resolved because the framework's future remains genuinely uncertain — it could evolve into Scaffold (temporary coordination transitioning to genuine peace) or degrade into Snare (extraction machinery hardened through international certification). The five omega variables capture the critical uncertainties: Rwanda's compliance (Will proxies disarm?), US enforcement duration (When does attention shift?), DRC agency recovery (Can sovereignty be rebuilt?), resource benefit flows (Who profits from minerals?), and civilian protection (Does violence actually decline?). If Rwanda genuinely withdraws and DRC rebuilds capacity, the constraint transitions toward Scaffold or multi-party Rope. If Rwanda maintains de facto control and US attention wanes, the constraint transitions toward Snare with high theater. The analytical system cannot resolve mandatrophy yet — it marks the story as ACTIVE because the structural ambiguity is empirically resolvable through ground-truth verification (military movements, mineral flows, casualty data, displacement trends). The framework is not a false summit (Mountain) but a genuine Tangled Rope in unstable equilibrium: its classification depends on which structural facts dominate over the next 24 months.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rwanda_compliance_verification,
    'Will Rwanda genuinely withdraw M23 proxy forces and cease resource extraction, or maintain de facto control through non-state actors while appearing compliant?',
    'Ground verification of military movements; mineral supply chain tracking; independent assessment of M23 command structure and funding sources; satellite imagery of troop positions',
    'If Rwanda complies: constraint reclassifies toward Scaffold or Rope. If Rwanda maintains control through proxies: constraint remains Tangled Rope or becomes Snare. Resolves mandate of framework credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rwanda_compliance_verification, empirical, 'Rwanda''s genuine commitment to military withdrawal vs proxy maintenance').

omega_variable(
    us_enforcement_duration,
    'How long will US diplomatic and security presence remain committed to enforcing the framework before shifting attention to other strategic priorities?',
    'Tracking US security aid disbursement, diplomatic engagement levels, military advisors deployed; correlation with US domestic political cycles and other regional crises',
    'If sustained: framework maintains enforcement capability and constraint remains Tangled Rope. If withdrawn within 2 years: suppression increases and constraint transitions toward Snare or degrades into Piton. Determines viability of sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_enforcement_duration, empirical, 'Duration of US enforcement commitment').

omega_variable(
    drc_agency_recovery,
    'Can DRC government build sufficient military capacity and territorial control to become credible independent actor, or remains permanently dependent on external guarantors?',
    'Assessment of DRC military modernization, officer training completion, mineral revenue capture; longitudinal analysis of territorial control expansion independent of external support',
    'If DRC recovers agency: constraint could transition to Rope (mutual coordination). If dependency persists: constraint remains Tangled Rope or becomes permanent Snare. Determines whether framework enables genuine sovereignty recovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drc_agency_recovery, empirical, 'DRC government capacity for independent agency and territorial control').

omega_variable(
    extractive_resource_flows,
    'Who actually benefits from eastern DRC mineral wealth (cobalt, coltan, cassiterite) under the framework: international companies, Rwanda, DRC government, or local warlords?',
    'Commodity chain tracing; ownership analysis of mining concessions; benefit-sharing agreement audits; comparison of pre- and post-framework mining revenue allocation',
    'If DRC retains majority: framework functions as coordination mechanism for resource distribution (Rope). If Rwanda or external actors maintain control: framework is extraction mechanism (Snare or Tangled Rope). Determines whether resource predation is constraint''s core function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_resource_flows, empirical, 'Actual beneficiaries of mineral wealth extraction').

omega_variable(
    civilian_protection_implementation,
    'Does the framework produce genuine reduction in civilian deaths, displacement, and sexual violence, or are protection mechanisms performative while violence persists?',
    'Longitudinal casualty data; displacement statistics; sexual violence reporting; comparison with baseline conflict periods; correlation between framework implementation phases and civilian impact',
    'If genuine protection: constraint reclassifies toward Scaffold or mixed Rope/Scaffold from civilian perspective. If performative: constraint remains Snare for civilian populations; theater_ratio confirmed high. Determines whether constraint benefits primary victim group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_protection_implementation, empirical, 'Effectiveness of civilian protection mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(drc_rwanda_peace_deal_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drc_rwa_tr_t0, drc_rwanda_peace_deal_2024, theater_ratio, 0, 0.52).
narrative_ontology:measurement(drc_rwa_tr_t6, drc_rwanda_peace_deal_2024, theater_ratio, 6, 0.68).
narrative_ontology:measurement(drc_rwa_tr_t12, drc_rwanda_peace_deal_2024, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(drc_rwa_be_t0, drc_rwanda_peace_deal_2024, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(drc_rwa_be_t6, drc_rwanda_peace_deal_2024, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(drc_rwa_be_t12, drc_rwanda_peace_deal_2024, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(drc_rwanda_peace_deal_2024, enforcement_mechanism).
narrative_ontology:affects_constraint(drc_rwanda_peace_deal_2024, m23_armed_group_dynamics).
narrative_ontology:affects_constraint(drc_rwanda_peace_deal_2024, congolese_mineral_supply_chain).
narrative_ontology:affects_constraint(drc_rwanda_peace_deal_2024, regional_patron_state_competition).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the underlying Rwanda-DRC conflict (which has structural properties including historical grievance, resource competition, and ethnic dimensions) and upstream of specific implementation dynamics (M23 disarmament, mineral extraction patterns, civilian protection outcomes). The framework itself is a distinct constraint: it is not the conflict but the structure imposing order on the conflict. Its extractiveness (0.58) reflects the asymmetry of the imposed order, not the raw severity of underlying conflict. Decomposition into separate stories would be appropriate only if measuring the conflict vs the framework produced ε values differing by >0.25; current analysis treats the framework as the primary constraint with the conflict as context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(drc_rwanda_peace_deal_2024, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
