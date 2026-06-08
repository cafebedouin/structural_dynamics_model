% ============================================================================
% CONSTRAINT STORY: orthographic_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel_flat_control, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: orthographic_kernel_flat_control
 *   human_readable: Orthographic Standard for Written Turkish
 *   domain: political_linguistics/state_formation/cultural_transmission
 *
 * SUMMARY:
 *   The orthographic standard for written Turkish represents a foundational
 *   commitment system that grounds legitimate literacy, state documentation,
 *   and cultural transmission. Established through the 1928 script reform
 *   (replacing Ottoman/Arabic-based script with Latin alphabet), the
 *   constraint exhibits the full structural complexity of a commitment
 *   system: it coordinates genuine administrative and educational functions
 *   while simultaneously extracting from non-standard script communities and
 *   erasing prior orthographic traditions. The constraint's evolution over a
 *   century shows declining extractiveness (0.72 → 0.32) and suppression
 *   (0.85 → 0.55) as the standard became naturalized and enforcement costs
 *   decreased, but rising theater ratio (0.25 → 0.52) as the constraint's
 *   performative maintenance increased relative to its functional necessity.
 *   The analytical observer risks naturalizing this contingent political
 *   choice as an inevitable feature of modern literacy, triggering false
 *   summit detection. The constraint demonstrates how commitment systems
 *   ground legitimacy in fixed kernels (the orthographic standard) while
 *   benefiting identifiable institutional actors (state apparatus, education
 *   system, cultural gatekeepers) at the cost of linguistic minorities and
 *   historical traditions.
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures administrative efficiency and bureaucratic control through unified script
 *   - Standardized Education System: Secondary beneficiary (institutional/arbitrage) — benefits from simplified curriculum and textbook standardization
 *   - Cultural Gatekeepers: Tertiary beneficiary (powerful/constrained) — maintain authority over legitimate literacy and cultural standards
 *   - Non-Standard Script Communities: Primary victim (powerless/trapped) — forced to adopt standard script for state participation; no exit option
 *   - Linguistic Minorities: Secondary victim (moderate/constrained) — face assimilation pressure and erosion of minority language traditions
 *   - Historical Script Tradition: Tertiary victim (institutional/arbitrage) — prior orthographic system persists only as cultural performance (piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political choice as inevitable feature of modernity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel_flat_control, 0.38).
domain_priors:suppression_score(orthographic_kernel_flat_control, 0.62).
domain_priors:theater_ratio(orthographic_kernel_flat_control, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel_flat_control, "Orthographic Standard for Written Turkish").
narrative_ontology:topic_domain(orthographic_kernel_flat_control, "political_linguistics/state_formation/cultural_transmission").

domain_priors:requires_active_enforcement(orthographic_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(orthographic_kernel_flat_control, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, standardized_education_system).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, cultural_gatekeepers).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, non_standard_script_communities).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, linguistic_minorities).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, historical_orthographic_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the orthographic standard through education policy, documentation requirements, and bureaucratic practice. Controls the apparatus that legitimizes literacy and validates state documents. Benefits from unified script through administrative efficiency and bureaucratic control. Can theoretically change the standard but rarely exercises this option due to institutional inertia and the costs of transition.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, state_administrative_apparatus, agenda_setter,
    institutional, immediate, arbitrage, national).

% Implements the orthographic standard through curriculum design, textbook production, and teacher training. Benefits from unified script through simplified pedagogical infrastructure and standardized assessment. Could theoretically teach multiple orthographies but institutional structure prevents this. Collects rents from standardization through simplified operations and economies of scale.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, standardized_education_system, beneficiary,
    institutional, biographical, arbitrage, national).

% Intellectuals, scholars, and cultural authorities who maintain and defend the orthographic standard. Control the definition of legitimate literacy and cultural coherence. Benefit from gatekeeping authority over who is recognized as 'educated' and 'cultured.' Constrained by dependence on the standard for their own institutional authority and prestige. Maintain the standard partly through genuine coordination (preserving cultural coherence) and partly through authority assertion.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, cultural_gatekeepers, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel_flat_control, cultural_gatekeepers, beneficiary).

% Communities that historically used non-standard orthographies (Arabic-based script, minority language scripts). Forced to adopt the standard script for state participation (education, documentation, legal proceedings). Cannot maintain non-standard scripts in formal domains without severe penalties. No exit option: participation in state institutions requires standard script adoption. Bear the full cost of script assimilation and linguistic erosion.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, non_standard_script_communities, payer,
    powerless, biographical, trapped, national).

% Communities whose primary language is not Turkish or whose Turkish dialect differs significantly from the standard. Face pressure to adopt standard orthography for education and economic opportunity. Can maintain minority language scripts in private/cultural domains but face assimilation pressure in formal institutions. Constrained by economic incentives for standard literacy and social prestige attached to standard script. Bear costs of linguistic assimilation while gaining access to state services.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, linguistic_minorities, payer,
    moderate, generational, constrained, national).

% The prior Ottoman/Arabic-based orthographic system that was displaced by the 1928 script reform. Persists in limited domains (calligraphy, historical documents, religious texts, academic study) but has lost its primary function as a literacy system. Maintained through cultural institutions and scholarly practice rather than through active use. Represents a non-agent entity (a tradition, not an actor) kept for narrative completeness.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, historical_script_tradition, payer,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_non_agent(orthographic_kernel_flat_control, historical_script_tradition).

% Civilizational-level perspective that risks naturalizing the orthographic standard as an inevitable feature of modern literacy. Sees the standard as a natural law of literate civilization rather than as a contingent political choice. Does not benefit from or bear costs of the constraint directly but provides the analytical frame that can either naturalize or denaturalize the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified orthographic standard enables state administration, bureaucratic record-keeping, and educational standardization across a geographically dispersed and linguistically diverse population. The standard solves the genuine coordination problem of how to maintain administrative coherence and enable literacy instruction at scale.
% TRANSFER_FUNCTION: The constraint transfers linguistic authority from non-standard script communities to the state apparatus and cultural gatekeepers. It transfers educational opportunity from non-standard speakers to those who adopt the standard. It transfers cultural prestige from historical script traditions to the modern standard. It transfers administrative efficiency gains to the state apparatus.
% ABSENT_VOICES: Communities that historically used non-standard orthographies (Ottoman script users, minority language speakers) are largely absent from the decision-making process that established and maintains the standard. Their objections to script displacement are not represented in the institutional structures that enforce the standard. Religious communities that use Arabic script for sacred texts are partially excluded from formal literacy domains.
% DISAPPEARANCE_RATIONALE: If the orthographic standard disappeared overnight, the state would face immediate administrative chaos: documentation systems would become incoherent, educational curricula would fragment, bureaucratic records would become unreadable. The state would be forced to either re-establish a unified standard or accept significant administrative inefficiency. Minority language communities would gain capacity to maintain non-standard scripts, but the economic incentives for standard literacy would persist. The constraint's disappearance would rearrange the linguistic landscape, not leave it unchanged.
% FOUNDING_PROBLEM: The founding problem was the need to modernize Turkish literacy and align with Western script systems as part of the broader Kemalist modernization project (1928 script reform). The prior Ottoman/Arabic-based script was perceived as incompatible with modern administration and Western alignment. The founding mandate was to replace the historical script with a more 'modern' and 'efficient' system.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (need for modernization and Western alignment) was attested by the Kemalist state apparatus and cultural reformers who drove the 1928 script reform. However, the problem's status is now contested: some argue the modernization mandate has been achieved and the standard is now self-sustaining through institutional inertia; others argue the modernization mandate was always a cover story for political control and cultural assimilation. Corroboration comes from historical analysis of the reform decision and from the declining enforcement intensity over time (suggesting the original mandate has been achieved).
narrative_ontology:disappearance_verdict(orthographic_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-STANDARD SCRIPT SPEAKER (SNARE) — Trapped by state literacy requirements and documentation mandates. Cannot participate in formal education, legal proceedings, or state services without adopting the standard orthography. No exit option: the constraint is enforced through institutional gatekeeping (schools, courts, bureaucracy). Experiences maximum extraction — forced script adoption with no alternative pathway.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY LANGUAGE COMMUNITY (TANGLED ROPE) — Constrained by educational and economic barriers to maintaining non-standard orthographies. Benefits from standardized script through access to state services and economic opportunity, but at the cost of linguistic assimilation. Genuine coordination function (unified literacy enables state administration and commerce) paired with asymmetric extraction (minority script traditions erode). Significant agency but high cost of exit.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — Primary beneficiary. Experiences the orthographic standard as pure coordination: unified script enables efficient documentation, record-keeping, and bureaucratic function. Net beneficiary with high exit options (can change the standard if needed, though rarely exercises this option). Extraction runs toward this agent through standardization rents and administrative efficiency gains.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STANDARDIZED EDUCATION SYSTEM (ROPE) — Secondary beneficiary. Experiences the orthographic standard as enabling coordination: unified script allows curriculum standardization, textbook production, and teacher training across the nation. Benefits from the constraint through simplified pedagogical infrastructure. High exit options (could theoretically teach multiple orthographies, but institutional inertia prevents this).
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CULTURAL GATEKEEPERS (TANGLED ROPE) — Intellectuals, scholars, and cultural authorities who maintain the standard. Experience genuine coordination function (preserving cultural coherence and literary tradition) alongside extraction (gatekeeping authority over legitimate literacy, controlling who is recognized as 'educated'). Constrained by institutional dependence on the standard for their own authority. Moderate extraction with significant agency.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL SCRIPT TRADITION (PITON) — The prior orthographic system (Ottoman script, Arabic-based writing) persists in limited domains (calligraphy, historical documents, religious texts) but is largely maintained as performance and cultural heritage rather than functional literacy. Theater ratio high: the historical script is preserved through institutional ritual (museums, cultural centers, academic study) but has lost its primary coordination function. Classified as piton because the function has atrophied while the constraint persists through inertia and cultural nostalgia.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some orthographic standardization appears inherent to literate civilization: complex societies require unified writing systems for administration and knowledge transmission. This perspective risks naturalizing the Turkish orthographic standard as an inevitable feature of modernity. However, the structural data contradicts this: the standard was deliberately chosen (Latin script adoption in 1928), enforced through state power, and benefits identifiable institutional actors. The engine will compute this as a false summit, revealing that 'necessary for civilization' naturalizes what is actually a contingent political choice.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(orthographic_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orthographic_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(orthographic_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(orthographic_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The orthographic standard extracts from non-standard script communities through forced adoption and assimilation pressure, but the extraction is not maximal because: (1) the standard provides genuine coordination benefits (unified literacy enables state administration and commerce), (2) enforcement has declined over time as the standard became naturalized, (3) minority communities retain some capacity to maintain non-standard scripts in private/cultural domains. The declining trajectory (0.72 → 0.38 over 100 years) reflects that initial coercive enforcement has been replaced by normalized institutional practice. Suppression (0.62): Moderate-high. Significant barriers to maintaining non-standard scripts include: state education mandates, documentation requirements, economic incentives for standard literacy, social prestige attached to standard script. But suppression is not total — some communities maintain non-standard scripts despite pressure, and digital technologies enable script switching. The declining trajectory (0.85 → 0.55) reflects that enforcement infrastructure has weakened as the standard became self-sustaining through institutional inertia. Theater ratio (0.48): Moderate. The orthographic standard exhibits moderate performative content: (1) the standard is genuinely functional for administration and education (low theater), (2) but cultural gatekeepers maintain the standard partly through ritual and authority assertion (moderate theater), (3) the historical script tradition is preserved almost entirely through performance (high theater in that domain). The rising trajectory (0.25 → 0.52) reflects that as the standard became naturalized, its performative maintenance increased relative to its functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement produces radically different classifications across observer positions. The state apparatus sees pure coordination (Rope) — unified script enables efficient administration. The minority language community sees mixed coordination and extraction (Tangled Rope) — genuine benefits paired with assimilation pressure. The non-standard script speaker sees pure extraction (Snare) — forced adoption with no exit option. The cultural gatekeepers see mixed coordination and gatekeeping extraction (Tangled Rope) — maintaining cultural coherence while controlling legitimate literacy. The historical script tradition sees degraded performance (Piton) — preserved through ritual but functionally obsolete. The analytical observer risks seeing natural law (Mountain) — orthographic standardization as inevitable feature of modernity — but the structural data reveals this as a false summit: the standard was deliberately chosen, enforced through state power, and benefits identifiable institutional actors. The perspectival gap reveals that 'necessity for civilization' naturalizes what is actually a contingent political choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to the extraction flow. The state apparatus and education system are beneficiaries with high exit options (arbitrage) — they experience low or negative effective extraction (d ≈ 0.1-0.2). The cultural gatekeepers are beneficiaries with constrained exit (they depend on the standard for their authority) — they experience moderate extraction (d ≈ 0.4-0.5). The minority language community is partly victim, partly beneficiary (they gain access to state services but lose linguistic autonomy) — they experience moderate-high extraction (d ≈ 0.6-0.7). The non-standard script speaker is a pure victim with no exit — they experience maximum extraction (d ≈ 0.9-1.0). The historical script tradition is a victim with institutional status but no functional role — it experiences moderate extraction (d ≈ 0.5-0.6). The analytical observer's mountain classification is perspectival — the engine's false summit detector will identify it as naturalization of a contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthographic standard exhibits potential mandatrophy: the founding mandate was to modernize Turkish literacy and align with Western script systems (1928 reform). This mandate has been achieved — Turkish literacy is now standardized and aligned with Latin script. However, the constraint persists beyond its founding mandate through institutional inertia and cultural gatekeeping. The constraint's function has shifted from active modernization (1928-1950s) to normalized institutional practice (1950s-present). The rising theater ratio (0.25 → 0.52) indicates that performative maintenance has increased relative to functional necessity, suggesting the constraint is approaching piton status. However, the constraint has not fully resolved into piton because: (1) the standard still provides genuine coordination benefits for administration and education, (2) enforcement infrastructure persists (though weakened), (3) the constraint's mandate has been reframed from 'modernization' to 'cultural preservation' and 'national identity.' The mandatrophy is not fully resolved — the constraint exhibits both functional coordination and performative maintenance, with the balance shifting toward performance over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is the orthographic standard a natural law of literate civilization or a constructed constraint that benefits identifiable actors?',
    'Historical analysis of script adoption decisions; comparison with alternative orthographic systems in other languages; examination of whether the standard was chosen for technical superiority or political control',
    'If natural law: mountain classification confirmed, suppression is coordination cost. If constructed: false summit detected, tangled_rope or snare classification more accurate, suppression is coercive enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Whether orthographic standard is natural law or constructed political choice').

omega_variable(
    script_switching_capacity,
    'Could a modern state maintain administrative coherence and educational function with multiple orthographic standards (e.g., Latin script for official documents, Arabic script for religious/cultural texts)?',
    'Comparative analysis of multilingual states with multiple orthographies (India, Switzerland); technical feasibility studies of dual-script administration; historical precedent analysis',
    'If yes: the suppression of alternative scripts is political choice, not technical necessity. Snare classification strengthens. If no: coordination function is genuine, tangled_rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_switching_capacity, empirical, 'Whether multiple orthographies could coexist in state administration').

omega_variable(
    linguistic_assimilation_mechanism,
    'Does the orthographic standard directly cause linguistic assimilation of minority languages, or is it a marker/enabler of assimilation driven by other factors (economic incentives, social prestige)?',
    'Longitudinal study of minority language retention in regions with strong vs weak orthographic enforcement; analysis of communities that maintain non-standard scripts despite state pressure; comparison of assimilation rates across different enforcement intensities',
    'If direct cause: the constraint is a primary extraction mechanism. If marker/enabler: extraction is secondary to economic and social factors. Affects whether victims classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_assimilation_mechanism, empirical, 'Whether orthographic standard directly causes linguistic assimilation').

omega_variable(
    historical_script_functionality,
    'Was the prior Ottoman/Arabic-based script genuinely less functional for modern administration, or was the switch to Latin script primarily a political assertion of national identity and Western alignment?',
    'Technical comparison of script efficiency for Turkish language representation; historical analysis of the 1928 script reform decision; examination of whether technical arguments were primary or post-hoc justification',
    'If technical: the switch was coordination improvement, tangled_rope classification confirmed. If political: the switch was assertion of state power, snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_script_functionality, empirical, 'Whether script switch was technical improvement or political assertion').

omega_variable(
    enforcement_intensity_variation,
    'Does enforcement intensity of the orthographic standard vary across regions, time periods, or social classes? If so, what explains the variation?',
    'Historical documentation of enforcement policies; analysis of literacy rates and script adoption across regions; examination of whether enforcement was stronger in minority-language areas',
    'If enforcement varies: the constraint is political choice, not natural law. If uniform: suggests coordination function is primary. Affects suppression metric interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_intensity_variation, empirical, 'Variation in enforcement intensity of orthographic standard').

omega_variable(
    cultural_gatekeeping_extraction,
    'Do cultural gatekeepers (scholars, intellectuals, literary authorities) extract rents from controlling the standard orthography through gatekeeping authority over legitimate literacy?',
    'Analysis of who controls orthographic policy; examination of career incentives for scholars who maintain the standard; study of how orthographic authority translates to institutional power and prestige',
    'If yes: cultural gatekeepers are beneficiaries, not neutral arbiters. Tangled_rope classification confirmed. If no: gatekeepers are primarily coordinators, rope classification more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_gatekeeping_extraction, empirical, 'Whether cultural gatekeepers extract rents from orthographic control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel_flat_control, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ortho_theater_1928, orthographic_kernel_flat_control, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ortho_theater_1948, orthographic_kernel_flat_control, theater_ratio, 20, 0.35).
narrative_ontology:measurement(ortho_theater_1978, orthographic_kernel_flat_control, theater_ratio, 50, 0.48).
narrative_ontology:measurement(ortho_theater_2028, orthographic_kernel_flat_control, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(ortho_extractiveness_1928, orthographic_kernel_flat_control, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(ortho_extractiveness_1948, orthographic_kernel_flat_control, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(ortho_extractiveness_1978, orthographic_kernel_flat_control, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(ortho_extractiveness_2028, orthographic_kernel_flat_control, base_extractiveness, 100, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(ortho_suppression_1928, orthographic_kernel_flat_control, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(ortho_suppression_1948, orthographic_kernel_flat_control, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(ortho_suppression_1978, orthographic_kernel_flat_control, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(ortho_suppression_2028, orthographic_kernel_flat_control, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel_flat_control, information_standard).
narrative_ontology:boltzmann_floor_override(orthographic_kernel_flat_control, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel_flat_control, linguistic_assimilation_pressure).
narrative_ontology:affects_constraint(orthographic_kernel_flat_control, state_documentation_legitimacy).
narrative_ontology:affects_constraint(orthographic_kernel_flat_control, cultural_gatekeeping_authority).

% DUAL FORMULATION NOTE:
% The orthographic standard is a foundational constraint that affects multiple downstream constraints in the domains of linguistic assimilation, state legitimacy, and cultural authority. The standard's extractiveness and suppression metrics are measured at the constraint level; downstream constraints have their own metrics reflecting how the standard's enforcement cascades into specific domains (education, documentation, cultural transmission).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel_flat_control, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
