% ============================================================================
% CONSTRAINT STORY: litchfield_sensitive_locations_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_litchfield_sensitive_locations_2026, []).

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
 *   constraint_id: litchfield_sensitive_locations_2026
 *   human_readable: Litchfield School Perimeter Crisis
 *   domain: political/social
 *
 * SUMMARY:
 *   On February 5, 2026, federal agents were reported within a block of the
 *   School of St. Litchfield, a primary private institution serving affluent
 *   families and a prominent civic anchor in the community. The incident
 *   triggered immediate concern about the intersection of law enforcement
 *   operations and child-safe spaces. The Litchfield perimeter crisis
 *   exemplifies a structural constraint: the legal and institutional
 *   arrangement governing federal surveillance of schools operates as a
 *   **snare for school communities** (powerless/trapped), extracting
 *   psychological burden and institutional disruption without compensating
 *   benefit, while operating as **rope or arbitrage** for federal agencies
 *   (institutional/mobile), who gain surveillance capability and information
 *   advantage. The constraint is simultaneously a **tangled rope for
 *   organized community groups** who can coordinate resistance but face real
 *   costs, a **scaffold for legislative oversight bodies** who see a path to
 *   policy change, and a **piton for post-9/11 security institutions** that
 *   continue performative surveillance through inertia. The core structural
 *   feature is the **informational and legal asymmetry**: federal agencies
 *   can operate within the perimeter with minimal transparency; schools and
 *   families bear costs (psychological, operational, institutional) without
 *   knowing scope, methods, or justification.
 *
 * KEY AGENTS:
 *   - School Families: Primary victims (powerless/trapped) — geographic commitment to school location creates forced exposure to federal enforcement operations
 *   - School Institution: Primary victim (powerless/trapped) — fiduciary duty to protect students while lacking power to exclude or regulate federal activity
 *   - Federal Enforcement Agencies: Primary beneficiary (institutional/arbitrage) — gain surveillance position and intelligence advantage; can exit by changing operations
 *   - Parent Advocacy Groups: Secondary actor (organized/constrained) — can coordinate community response but face costs of escalation and institutional resistance
 *   - Legislative Oversight Bodies: Potential path to resolution (powerful/mobile) — have authority to clarify legal bounds and mandate transparency
 *   - Post-9/11 Security Infrastructure: Institutional context (institutional/arbitrage) — maintains surveillance apparatus through funding and operational inertia despite questionable effectiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, 0.68).
domain_priors:suppression_score(litchfield_sensitive_locations_2026, 0.72).
domain_priors:theater_ratio(litchfield_sensitive_locations_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(litchfield_sensitive_locations_2026, snare).
narrative_ontology:human_readable(litchfield_sensitive_locations_2026, "Litchfield School Perimeter Crisis").
narrative_ontology:topic_domain(litchfield_sensitive_locations_2026, "political/social").

domain_priors:requires_active_enforcement(litchfield_sensitive_locations_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(litchfield_sensitive_locations_2026, federal_enforcement_agencies).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, school_families).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, educational_institution).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, civic_trust_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCHOOL FAMILIES AND STAFF (SNARE) — Residents have no exit from the school's location, which is also their community anchor. Trapped by both geographic necessity and institutional commitment. Bears full extraction cost: threat perception, disruption to educational environment, psychological burden of federal presence, without ability to relocate institutions or control enforcement operations. Maximum experienced extraction.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION (SNARE) — School has fiduciary duty to students and cannot exit. Forced to manage federal enforcement operations' proximity impact on educational operations, student safety protocols, parent confidence, and institutional credibility. High suppression: cannot refuse or negotiate federal activity in its jurisdiction. No alternative to bearing these costs.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL ENFORCEMENT AGENCIES (ROPE) — Experiences the constraint as coordination necessity: surveillance of sensitive locations requires proximity to operational targets. School perimeter location is instrumentally valuable for law enforcement mission. Can exit by changing operational scope or jurisdiction. Net beneficiary — gains surveillance capability and information advantage over targets.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARENT ADVOCACY AND COMMUNITY (TANGLED ROPE) — Organized agents see mixed extraction and coordination. The constraint creates coordination challenge (information sharing about security, school protocols, legal resources) but also extracts through enforcement of compliance with federal monitoring, implicit pressure to cooperate with investigations, and risk of reputational association. Have some agency through collective action but exit is costly (changing schools, community withdrawal).
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: LEGISLATIVE AND OVERSIGHT (SCAFFOLD) — Sees the constraint as a temporary coordination failure with potential sunset: expanding federal oversight near schools without legal authorization to do so, with pressure for legislative or judicial clarification of bounds. This perspective recognizes the institutional pathway to resolution (statute, court ruling, operational guidelines) and has mobility to shift policies. Experiences lower extraction because the problem is seen as solvable through institutional mechanisms.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-9/11 SECURITY INFRASTRUCTURE (PITON) — From the civilizational view of institutional security arrangements, the perimeter surveillance of schools is a largely performative legacy of expanded security theater. The original function (detecting external threats) has degraded into routine monitoring; the practice persists through operational inertia and legislative entrenchment rather than demonstrated effectiveness. Theater ratio reflects that much of the enforcement activity is visible-to-public signaling rather than functional intelligence gathering.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, tension between state security apparatus and protection of civilian institutions (schools) in democratic states appears immutable: governments must balance surveillance capacity against constitutional limits on interference. However, the structural data contradicts a pure mountain classification — the legal and constitutional frameworks governing sensitive location surveillance are contingent institutional arrangements, not laws of nature. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litchfield_sensitive_locations_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(litchfield_sensitive_locations_2026, TR),
    TR >= 0.70.

:- end_tests(litchfield_sensitive_locations_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Federal agencies extract surveillance position and information advantage within a school perimeter zone without equivalent transparency to or negotiation with affected families and institutions. The extraction is not total (agencies are pursuing legitimate law enforcement functions, not pure predation), but the asymmetry of benefit and burden is severe. Measurement trajectory shows increasing extractiveness as operational presence becomes routine — from 0.52 at discovery to 0.68 at current state — indicating the constraint is normalizing. Suppression (0.72): High. School families and institutions face severe barriers to contesting federal operations: legal authority is ambiguous (federal agencies claim domestic security mandate; schools claim constitutional protection), and political cost of explicit refusal is high (risk of appearing uncooperative with law enforcement). Suppression reflects lack of meaningful alternatives — families cannot move schools easily, and institutions cannot exclude federal operations through normal channels. Theater ratio (0.58): Moderate-high. Federal presence near schools serves both functional (actual intelligence gathering) and performative (public visibility of security, deterrence messaging, institutional legitimacy) purposes. The ratio is increasing (0.42 to 0.58 over interval) because much of the visible enforcement activity appears to be visible-to-public signaling rather than covert intelligence collection, consistent with post-9/11 security theater patterns.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximal perspectival divergence. Federal agencies see coordination necessity (Rope) — school perimeter surveillance is instrumentally valuable for law enforcement mission. School communities see pure extraction (Snare) — they bear costs without understanding function or receiving benefit. Legislative observers see a temporary institutional failure (Scaffold) — one legal clarification or policy mandate could resolve it. Post-9/11 security institutions see a degraded ritual (Piton) — the apparatus persists through inertia despite questionable effectiveness. Organized community groups see mixed extraction and coordination (Tangled Rope) — they can mobilize but at cost. The analytical observer risks naturalizing the constraint as an immutable property of state security (Mountain), but the structural data reveals it as a contingent arrangement: the legal and institutional bounds are not laws of nature but design choices subject to change.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by structural position: federal agencies (beneficiary + arbitrage exit) derive low d → negative effective extraction; school families (victim + trapped exit) derive high d → high effective extraction; parent groups (mixed + constrained exit) derive moderate-high d → moderate effective extraction. The beneficiary/victim declaration is direct: federal agencies benefit from the surveillance position (gain intelligence, operational advantage); school families and institutions bear costs (psychological burden, operational disruption, institutional trust erosion, without compensating benefit). The exit asymmetry is structural: federal agencies can change operations easily; schools cannot relocate; families cannot exit education. This asymmetry drives the high d values for trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The Litchfield constraint resolves mandatrophy by revealing how different institutional actors legitimately perceive the same arrangement through different lenses. Federal agencies see coordination (Rope) because the constraint solves their operational problem (surveillance access). School communities see extraction (Snare) because the constraint imposes costs without benefit to them. Legislative observers see a temporary failure (Scaffold) because they have authority to change the rules. No single classification captures all structural truth — the presheaf over the observation site (federal perspective, community perspective, legislative perspective, institutional history perspective) IS the complete picture. The false mountain appears when analysts naturalize the arrangement as inherent to state security — the structural data reveals it as contingent: legal bounds could shift, transparency could reduce asymmetry, operational alternatives could replace school-perimeter surveillance. The constraint is not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_authorization_boundary,
    'What legal and constitutional limits govern federal enforcement operations within a school perimeter zone?',
    'Judicial clarification through precedent (FISA court rulings, constitutional test cases, DOJ policy guidance); legislative specification of sensitive location protection zones',
    'If bounds are narrow: current operations constitute unauthorized suppression (snare classification reinforced). If bounds are wide: operations may be lawful coordination (rope reclassification). Current ambiguity enables extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_authorization_boundary, conceptual, 'Legal and constitutional bounds for enforcement near schools').

omega_variable(
    informational_asymmetry_closure,
    'Can families and institutions gain equivalent surveillance awareness (know when monitoring occurs, what protocols govern it) or is the asymmetry structural?',
    'Implementation of transparency mechanisms (disclosure logs, notice requirements, community oversight boards); assessment of whether reciprocal surveillance is possible in democratic framework',
    'If closure possible: constraint becomes more symmetric (Tangled Rope from more perspectives). If structural: suppression floor remains high (Snare confirmed across perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informational_asymmetry_closure, empirical, 'Whether informational asymmetry between enforcers and communities can be reduced').

omega_variable(
    alternative_target_methodology,
    'Do surveillance operations near schools actually provide law enforcement intelligence advantage over alternative methodologies (warrant-based investigation, source intelligence, electronic surveillance at other locations)?',
    'Comparative effectiveness analysis (conviction rates, intelligence quality) for operations using school-perimeter surveillance vs. alternative methods; operational reports and case outcome data',
    'If alternatives are equally effective: school perimeter surveillance is extractive theater with no coordination benefit (confirms Snare). If perimeter operations are uniquely effective: constraint has genuine coordination function (supports Rope/Tangled Rope reclassification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_target_methodology, empirical, 'Comparative effectiveness of school-perimeter surveillance vs. alternative methods').

omega_variable(
    democratic_consensus_stability,
    'Is the current arrangement (federal enforcement near schools with minimal transparency) sustainable as a political settlement, or does public pressure create inevitable path to policy change?',
    'Longitudinal tracking of public opinion, legislative pressure, community organizing; assessment of whether political coalitions will demand policy change within 5-10 year interval',
    'If unsustainable: scaffold sunset logic applies (policies will change through political pressure). If stable: enforcement apparatus is entrenched snare. Current trajectory suggests sunset is likely but not certain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_consensus_stability, preference, 'Political sustainability of current enforcement arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(litchfield_sensitive_locations_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litch_tr_t0, litchfield_sensitive_locations_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(litch_tr_t3, litchfield_sensitive_locations_2026, theater_ratio, 3, 0.5).
narrative_ontology:measurement(litch_tr_t6, litchfield_sensitive_locations_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(litch_be_t0, litchfield_sensitive_locations_2026, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(litch_be_t3, litchfield_sensitive_locations_2026, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(litch_be_t6, litchfield_sensitive_locations_2026, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(litchfield_sensitive_locations_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, warrantless_surveillance_expansion).
narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, school_safety_zone_definition).

% DUAL FORMULATION NOTE:
% The Litchfield perimeter crisis is downstream of broader warrantless surveillance expansion (which has higher ε and Mountain characteristics at the civilizational level). This constraint story focuses on the specific institutional arrangement governing federal enforcement near schools, with extractiveness driven by informational asymmetry and legal ambiguity rather than by the underlying surveillance architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(litchfield_sensitive_locations_2026, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
