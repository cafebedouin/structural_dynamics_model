% ============================================================================
% CONSTRAINT STORY: frontex_pushback_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_frontex_pushback_coordination, []).

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
 *   constraint_id: frontex_pushback_coordination
 *   human_readable: Frontex-Coordinated Asylum Suppression at EU Maritime Borders
 *   domain: political/migration/border_control
 *
 * SUMMARY:
 *   The European Union's external maritime border management system,
 *   operationalized through Frontex (European Border and Coast Guard Agency)
 *   in coordination with member state authorities, represents a constraint
 *   that operates at the intersection of migration control, humanitarian law,
 *   and institutional coordination. Since 2014, this system has evolved from
 *   a burden-sharing coordination mechanism (distributing surveillance and
 *   interdiction costs) into an apparatus optimized for asylum access
 *   suppression through maritime interception and informal pushbacks. The
 *   constraint exhibits the structural signatures of a Snare: high
 *   extractiveness (blocking asylum access), high suppression (limited legal
 *   alternatives for asylum seekers, no procedural protections for
 *   intercepted vessels), and increasing theater (formal invocations of
 *   international maritime law and humanitarian commitments coexisting with
 *   systematic violations). The base extractiveness has increased from 0.42
 *   (2014, when Frontex mandate emphasized rescue coordination) to 0.68
 *   (2024, when interception-focused operations dominate), indicating
 *   institutional drift toward pure suppression. Theater ratio has similarly
 *   increased from 0.45 to 0.65, reflecting the growing disjuncture between
 *   stated humanitarian commitments and operational reality. The constraint's
 *   coordination dimension persists — member states genuinely benefit from
 *   Frontex surveillance and information-sharing infrastructure — but this
 *   coordination function has been subordinated to the extraction function
 *   (preventing asylum access). From the asylum seeker's perspective, this is
 *   a Snare with no coordination benefit; from the member state's
 *   perspective, it is a Tangled Rope (mixing coordination and sovereignty
 *   consolidation); from Frontex's institutional perspective, it appears as
 *   Rope (burden-sharing coordination). The mandatrophy is resolved by
 *   recognizing that all three perspectives are structurally valid readings
 *   of different agent positions relative to the constraint. The snare
 *   classification reflects the empirically dominant function: preventing
 *   asylum access through coordinated maritime interdiction with high
 *   coercion and minimal procedural protections.
 *
 * KEY AGENTS:
 *   - Asylum seekers (maritime): Primary victims (powerless/trapped) — interdicted at sea, facing pushback to origin or transit countries, no legal access to asylum procedures
 *   - EU interior ministries (Greece, Spain, Italy, Cyprus, Malta): Primary beneficiaries (powerful/arbitrage) — coordinate border enforcement while outsourcing deniability to Frontex; preserve national sovereignty while leveraging EU coordination infrastructure
 *   - Frontex (institutional mandate): Secondary beneficiary (institutional/arbitrage) — expands operational scope and institutional autonomy through coordination role; insulated from legal accountability for pushback operations
 *   - Third country coastguards (Libya, Turkey, Morocco): Coerced partners (powerless/trapped) — receive Frontex equipment and training conditional on accepting returned asylum seekers; trapped in agreements through resource dependency
 *   - International humanitarian law framework (UNCLOS, 1951 Refugee Convention): Victim/framework (analytical/trapped) — non-refoulement principle systematically violated; international legal obligations persist as theater while operations circumvent them
 *   - Analytical observer: Civilizational position (analytical/analytical) — observes the full apparatus from outside EU institutional interest; sees coordination as instrumental to suppression, not primary function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frontex_pushback_coordination, 0.68).
domain_priors:suppression_score(frontex_pushback_coordination, 0.78).
domain_priors:theater_ratio(frontex_pushback_coordination, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frontex_pushback_coordination, extractiveness, 0.68).
narrative_ontology:constraint_metric(frontex_pushback_coordination, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(frontex_pushback_coordination, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(frontex_pushback_coordination, snare).
narrative_ontology:human_readable(frontex_pushback_coordination, "Frontex-Coordinated Asylum Suppression at EU Maritime Borders").
narrative_ontology:topic_domain(frontex_pushback_coordination, "political/migration/border_control").

domain_priors:requires_active_enforcement(frontex_pushback_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(frontex_pushback_coordination, eu_interior_ministries).
narrative_ontology:constraint_beneficiary(frontex_pushback_coordination, frontex_institutional_mandate).
narrative_ontology:constraint_victim(frontex_pushback_coordination, asylum_seekers_maritime).
narrative_ontology:constraint_victim(frontex_pushback_coordination, humanitarian_obligation_framework).
narrative_ontology:constraint_victim(frontex_pushback_coordination, third_country_coastguards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARITIME ASYLUM SEEKER (SNARE) — Trapped between source country persecution and coordinated maritime interdiction. No legal exit option from waters; Frontex-coordinated pushbacks prevent landfall. Faces physical coercion (naval interception), legal suppression (non-arrival = non-filing of asylum claim), and exclusion from EU territory via coordination between Frontex and national authorities. d≈0.96, f(d)≈1.41, σ=1.2 → χ≈0.73. Pure extraction with maximal coercion.
constraint_indexing:constraint_classification(frontex_pushback_coordination, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THIRD COUNTRY COASTGUARD AS COERCED PARTNER (SNARE) — Trapped into enforcing EU border exclusion through Frontex coordination agreements, equipment provision, and training. Receives EU maritime equipment and funding conditional on pushback compliance. No credible exit from agreements without losing resources. d≈0.92, f(d)≈1.37, σ=0.9 → χ≈0.59. High extraction via institutional conditioning.
constraint_indexing:constraint_classification(frontex_pushback_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: EU MEMBER STATE BORDER AUTHORITY (TANGLED ROPE) — Coordination function: Frontex provides maritime surveillance, coordination infrastructure, and burden-sharing with other member states. Extraction function: National sovereignty over borders is preserved and reinforced; outsourcing to Frontex creates asymmetric power (EU agency can claim deniability for pushback operations). d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.19. Both coordination (surveillance tech, resource sharing) and asymmetric extraction (sovereignty consolidation) present.
constraint_indexing:constraint_classification(frontex_pushback_coordination, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FRONTEX INSTITUTIONAL MANDATE (ROPE) — Coordination function: Frontex coordinates disparate national maritime operations into a unified border management system, standardizing surveillance, interception, and information-sharing protocols. Reduces transaction costs of border coordination among 27 member states. Extraction function is structured as secondary: Frontex's autonomy is justified by coordination necessity. d≈0.25, f(d)≈0.05, σ=1.1 → χ≈0.04. Appears as pure coordination from institutional perspective; deniability and institutional insulation are the real benefits.
constraint_indexing:constraint_classification(frontex_pushback_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN LAW (PITON) — The 1951 Refugee Convention and UNCLOS maritime protections exist as a legal framework but are theatrically maintained while operationally undermined. Frontex pushbacks violate non-refoulement principles, yet are framed as 'irregular migration enforcement' — the humanitarian law persists as institutional backdrop while its enforcement has atrophied. theater_ratio≈0.65 reflects high performative content: formal legal commitments coexist with systematic violations. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.32. Piton gate fires: theater ≥ 0.65 and the constraint persists through institutional inertia despite functional atrophy.
constraint_indexing:constraint_classification(frontex_pushback_coordination, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (SNARE) — From a universal/civilizational perspective, this constraint represents pure extraction with maximal suppression justified by framing asylum seekers as 'irregular migrants' and pushbacks as 'border enforcement.' The entire coordination apparatus serves a single function: preventing asylum access through maritime interdiction. The 'coordination' benefit (unified EU border management) is subordinate to the extraction benefit (population control). ε=0.68, suppression=0.78, theater=0.65 yield χ=0.68 (at organized/constrained level). The analytical observer sees no false summit — the structural data confirms snare classification across all measurement bases.
constraint_indexing:constraint_classification(frontex_pushback_coordination, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frontex_pushback_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(frontex_pushback_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(frontex_pushback_coordination, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(frontex_pushback_coordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(frontex_pushback_coordination, TR),
    TR >= 0.70.

:- end_tests(frontex_pushback_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The primary function of the Frontex system is preventing asylum access through maritime interdiction. This is not a collateral effect of coordination — it is the central operational objective. Asylum seekers are systematically excluded from EU territory through coordinated pushback operations; this exclusion imposes costs (lack of asylum access, exposure to return to persecution) that directly benefit EU interior ministries (reduced asylum flows, maintained sovereignty over border policy). The 2014-2024 trajectory (0.42→0.68) reflects operational evolution toward suppression as the primary goal. Suppression (0.78): High. Multiple mechanisms: (1) Physical coercion — naval vessels, armed personnel, forced turnarounds; (2) Legal suppression — asylum seekers never reach territorial waters where asylum claims would be processed, rendering their claims legally invisible; (3) Third-country conditioning — agreements forcing Libya and Turkey to accept returns create institutional barriers to alternative routes; (4) Informational suppression — pushback operations are partially covert, with limited documentation of destination outcomes. Theater ratio (0.65): Moderate-high. The constraint maintains formal invocation of international maritime law (UNCLOS search-and-rescue obligations), humanitarian principles (Frontex mandate emphasizes 'life-saving'), and refugee law frameworks (EU asylum regulations), while operationally circumventing these through coordination agreements that enable plausible deniability. Frontex can frame pushbacks as member state operations; member states can claim Frontex coordination shields them from individual liability. The theater has increased from 0.45 (2014) to 0.65 (2024) as the disjuncture between stated commitments and operational reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme — spanning from pure snare (asylum seeker) to pure rope (Frontex institutional view). Maritime asylum seekers experience maximum extraction with zero coordination benefit: they are subject to coordinated interception with no legal recourse, no procedural protection, and high likelihood of return to persecution. EU member states experience tangled rope: they benefit from Frontex coordination infrastructure (unified surveillance, burden-sharing of interception costs) while also benefiting from extraction (reduced asylum pressure on national asylum systems). Frontex itself sees pure rope: its coordination function is genuinely useful (member states save resources through centralized maritime operations), and this allows the organization to frame itself as a facilitator rather than an enforcer. The analytical observer sees snare: the coordination function is secondary to the suppression function; the entire apparatus is optimized for preventing asylum access, and coordination infrastructure is instrumental to that goal. The humanitarian law framework sees a piton — its formal commitments persist as theater while functional enforcement has atrophied. This perspectival gap reveals the constraint's true structure: what appears as benign coordination from institutional perspectives is experienced as pure coercive exclusion from the perspective of those it governs.
 *
 * DIRECTIONALITY LOGIC:
 *   Maritime asylum seekers: Victim + trapped → d≈0.96, f(d)≈1.41. Maximal extraction. No exit option (cannot sail backward to origin safely; cannot legally land in EU); subject to coordinated physical coercion; excluded from asylum procedures through non-arrival. EU member states: Beneficiary + arbitrage → d≈0.35, f(d)≈0.28. Moderate extraction from institutional perspective (benefit from coordination infrastructure and reduced asylum burden). However, some member states (especially island states like Cyprus, Malta) bear higher costs and should be reclassified toward constrained exit (d→0.55, f(d)→0.75). Third-country coastguards: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction. Conditional relationships (equipment, training, funding) create constrained exit; trapped into enforcement role through resource dependency. Frontex institutional mandate: Beneficiary + arbitrage → d≈0.22, f(d)≈-0.03. Negative effective extraction; net beneficiary. Expansion of institutional scope justified by coordination role. International humanitarian law framework: Victim + trapped → d≈0.90, f(d)≈1.35. Framework cannot exit or adapt; binding commitments (UNCLOS, Refugee Convention) create structural trap; violations are systematic but institutional momentum prevents enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint is legitimately classified as Snare (ε=0.68, suppression=0.78, χ=0.68 at primary target perspective; χ ≥ 0.66 gate satisfied). The mandatrophy resolution involves recognizing that Frontex coordination infrastructure genuinely serves a coordination function (member state burden-sharing, unified surveillance, reduced transaction costs) but this coordination function is now subordinated to and instrumentalized for the extraction function (preventing asylum access). The constraint was originally conceived as Rope (pure coordination) in early Frontex mandate documents (2004-2007); it evolved into Tangled Rope (mixed coordination and burden-shifting) by 2010-2014; it became Snare (coordinated suppression apparatus) by 2015-2024. The mandatrophy is not 'whether coordination exists' — it does. The mandatrophy is 'whether coordination justifies the extraction.' The answer is no: EU member states could achieve equivalent coordination benefits through information-sharing and voluntary burden-sharing mechanisms that do not require maritime interception systems optimized for asylum suppression. The fact that they have instead built an apparatus that couples coordination with maximum suppression indicates that the suppression is the primary goal, with coordination as instrumental justification. The operative classification is therefore Snare, with coordination as theater that enables deniability. The mandatrophy is resolved at the analytical/civilizational perspective, where the structural prioritization becomes visible: coordination mechanisms were built to achieve suppression, not the reverse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pushback_legal_status,
    'Do Frontex-coordinated maritime interdictions and turnarounds constitute illegal refoulement under UNCLOS and the 1951 Refugee Convention, or are they lawful border enforcement?',
    'International Court of Justice rulings, EU Court of Justice precedent, UNHCR formal position, documented pushback incidents with destination tracing (do interdicted vessels return to persecution?)',
    'If illegal refoulement: Snare classification confirmed (ε→0.72, suppression→0.82, χ→0.75). If lawful enforcement: Classification downgrades to Tangled Rope (coordination benefit of burden-sharing outweighs extraction cost). This omega determines whether the constraint is per-se violation or trade-off.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pushback_legal_status, empirical, 'Whether maritime pushbacks constitute illegal refoulement').

omega_variable(
    coordination_alternative_feasibility,
    'Could EU member states achieve equivalent border management coordination and resource-sharing without maritime interception systems designed to prevent asylum access?',
    'Comparative analysis of EU internal border coordination (Schengen) vs external maritime coordination; counterfactual scenario modeling (what if Frontex focused on smuggling interdiction without asylum prevention?); member state capability to coordinate independently',
    'If feasible alternative exists: Snare classification confirmed (coordination could happen without extraction). If no alternative: Tangled Rope (coordination genuinely requires extraction mechanism as side effect). This omega determines whether extraction is necessary or gratuitous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_alternative_feasibility, empirical, 'Whether effective EU border coordination requires asylum suppression mechanism').

omega_variable(
    member_state_exit_capacity,
    'Can individual EU member states credibly exit Frontex coordination while maintaining border security and humanitarian obligations?',
    'Documented costs of national-only border management for Mediterranean states; EU pressure mechanisms on defecting member states; asylum processing capacity if maritime pushbacks cease',
    'If no credible exit: Member states are also victims (snare perspective valid). If credible exit exists but is costly: Member states occupy tangled_rope position (not powerless/trapped as currently assumed). This omega determines whether member states should be reclassified from beneficiary to constrained victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_exit_capacity, empirical, 'Whether EU member states can exit Frontex coordination without cost').

omega_variable(
    pushback_destination_outcomes,
    'What are the documented outcomes for asylum seekers returned via maritime pushbacks? Do they face persecution, detention, trafficking, or successful secondary asylum attempts?',
    'Longitudinal tracking of pushback destinations (Libya, Turkey, origin countries); documented persecution of returned individuals; secondary asylum filing rates and success rates',
    'If returned to persecution: ε→0.75 (extraction includes imminent harm risk), suppression→0.85 (systemic coercion). If returned to safe third countries: ε→0.62 (extraction is access denial only), suppression→0.70. This omega determines severity magnitude within snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pushback_destination_outcomes, empirical, 'Documented outcomes for asylum seekers returned via maritime pushbacks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(frontex_pushback_coordination, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frontex_tr_t0, frontex_pushback_coordination, theater_ratio, 0, 0.45).
narrative_ontology:measurement(frontex_tr_t5, frontex_pushback_coordination, theater_ratio, 5, 0.55).
narrative_ontology:measurement(frontex_tr_t10, frontex_pushback_coordination, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(frontex_be_t0, frontex_pushback_coordination, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(frontex_be_t5, frontex_pushback_coordination, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(frontex_be_t10, frontex_pushback_coordination, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(frontex_pushback_coordination, enforcement_mechanism).
narrative_ontology:affects_constraint(frontex_pushback_coordination, eu_asylum_reception_capacity).
narrative_ontology:affects_constraint(frontex_pushback_coordination, libyan_coastguard_sovereignty).
narrative_ontology:affects_constraint(frontex_pushback_coordination, mediterranean_smuggling_market).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story family examining EU asylum policy structure. (1) EU asylum reception capacity (ε=0.35, Rope) — coordination among member states on asylum processing standards; (2) Frontex pushback coordination (ε=0.68, Snare) — maritime suppression apparatus; (3) Libyan coastguard sovereignty (ε=0.72, Snare) — coerced return operations. The Frontex constraint is downstream of EU member state decisions to prioritize suppression over reception capacity, and upstream of outcomes in third-country partners.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(frontex_pushback_coordination, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
