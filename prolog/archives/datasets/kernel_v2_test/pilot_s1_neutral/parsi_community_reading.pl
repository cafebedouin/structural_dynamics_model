% ============================================================================
% CONSTRAINT STORY: parsi_community_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parsi_community_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: parsi_community_reading
 *   human_readable: Parsi Marriage Authority: Community Codification Reading
 *   domain: comparative_law/legal_pluralism/personal_status_law
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the marriage authority
 *   kernel: the Parsi community reading, which holds that marriage authority
 *   derives legitimacy from codification in community-specific statute
 *   (primarily the Parsi Marriage and Divorce Act 1936) administered by
 *   religious authorities (Parsi priests and community councils) within a
 *   bounded religious community. This reading is live in contemporary Parsi
 *   discourse but increasingly contested by reformist movements, younger
 *   generations, and secular state institutions. The constraint exhibits the
 *   characteristic structure of legal pluralism: a minority community
 *   maintaining distinctiveness through separate personal status law,
 *   achieving real coordination (predictable rules, inheritance clarity,
 *   ritual legitimation) while simultaneously extracting compliance through
 *   boundary enforcement (endogamy norms, exclusion of intermarried
 *   individuals, identity-locking mechanisms). The reading differs
 *   structurally from sibling readings in the same kernel: the Hindu codified
 *   reading (Sharia Act provisions for Hindu marriage) operates under state
 *   codification with secular adjudication; the Muslim shariat reading
 *   appeals to religious law interpreted by Islamic jurists; the Christian
 *   colonial reading (Anglo-Indian Personal Law) inherits British statutory
 *   frameworks; the secular contractual reading treats marriage as individual
 *   choice under state law without community mediation. Each reading has a
 *   different source of authority legitimacy, a different beneficiary/victim
 *   structure, and a different exit cost landscape for individuals and
 *   communities.
 *
 * KEY AGENTS:
 *   - Parsi Religious Authority: Institutional actor (institutional/arbitrage) — priests, Parsi community councils, statute-maintaining bodies. Benefits from administering marriage law (institutional role, fee collection, preservation of religious function). Experiences the constraint as rope (genuine coordination function aligned with their institutional interests).
 *   - Endogamy-Sustaining Families: Powerful network (powerful/constrained) — family lineages invested in kinship networks, property transmission, ritual participation. Benefits from endogamy enforcement (maintains family economic/social structures) while bearing enforcement costs (monitoring children's choices, perpetuating marriage arrangement norms). Experiences mixed tangled rope.
 *   - Parsi Individual Seeking Intermarriage: Powerless agent (powerless/identity_locked) — young Parsis whose romantic choice falls outside faith boundary. Bears full extraction (community exclusion, identity severing, ritual exclusion). Identity locked — could structurally exit (relocation, secular marriage) but cannot exercise that exit because identity is constituted through Parsi community membership. Experiences snare.
 *   - Non-Parsi Spouse: Moderate power (moderate/constrained) — persons marrying into the Parsi community. Faces asymmetric legal disabilities (exclusion from ritual, permanent outsider status, inheritance complications). Experiences tangled rope — legitimate coordination exists (marriage recognition, legal status within community) at asymmetric cost.
 *   - Parsi Reform Movements: Organized agents (organized/mobile) — modernization networks, women's organizations, young Parsis advocating statute reform. Have institutional platforms and exit paths (can influence community discourse, create alternative recognition structures, exit to secular regime). Experience the constraint as scaffold — see temporary function with sunset path as younger generations intermarry and cultural transmission replaces boundary enforcement.
 *   - Secular State Institutions: Institutional observer (institutional/arbitrage) — Indian state legal system, constitutional courts, secular civil code advocates. Increasingly displace community statute as primary marriage authority through constitutional precedent (secular state supremacy) and secular civil registration. This is not modeled as a stakeholder in this reading but represents downstream institutional pressure contextualizing the scaffold sunset.
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing the community reading's boundary maintenance as an unchangeable property of religious community persistence, when comparative data (other minority communities maintaining identity without strict endogamy) suggests boundary enforcement is a specific institutional choice, not a necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parsi_community_reading, 0.35).
domain_priors:suppression_score(parsi_community_reading, 0.42).
domain_priors:theater_ratio(parsi_community_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parsi_community_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(parsi_community_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(parsi_community_reading, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parsi_community_reading, tangled_rope).
narrative_ontology:human_readable(parsi_community_reading, "Parsi Marriage Authority: Community Codification Reading").
narrative_ontology:topic_domain(parsi_community_reading, "comparative_law/legal_pluralism/personal_status_law").

domain_priors:requires_active_enforcement(parsi_community_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parsi_community_reading, 'e1fcc0a5-838f-474b-823f-57b4a3385348').
narrative_ontology:cs_kernel_codification('e1fcc0a5-838f-474b-823f-57b4a3385348', formalized).
narrative_ontology:cs_authority_grounding('e1fcc0a5-838f-474b-823f-57b4a3385348', lineage).
narrative_ontology:cs_interpretation_layer_present('e1fcc0a5-838f-474b-823f-57b4a3385348').
narrative_ontology:cs_reading_relation('e1fcc0a5-838f-474b-823f-57b4a3385348', parsi_community_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1fcc0a5-838f-474b-823f-57b4a3385348', parsi_community_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1fcc0a5-838f-474b-823f-57b4a3385348', parsi_community_reading__christian_colonial_reading, influences).
narrative_ontology:cs_reading_relation('e1fcc0a5-838f-474b-823f-57b4a3385348', parsi_community_reading__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('e1fcc0a5-838f-474b-823f-57b4a3385348', foundational, community_authority_legitimacy_via_self_determination).
narrative_ontology:cs_axiom_status(community_authority_legitimacy_via_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('e1fcc0a5-838f-474b-823f-57b4a3385348', community_authority_legitimacy_via_self_determination, conventional).
narrative_ontology:cs_axiom('e1fcc0a5-838f-474b-823f-57b4a3385348', secondary, endogamy_necessity_for_community_continuity).
narrative_ontology:cs_axiom_status(endogamy_necessity_for_community_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e1fcc0a5-838f-474b-823f-57b4a3385348', endogamy_necessity_for_community_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('e1fcc0a5-838f-474b-823f-57b4a3385348', community_self_determination_under_plural_law).
narrative_ontology:cs_drift_state('e1fcc0a5-838f-474b-823f-57b4a3385348', contemporary_secular_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1fcc0a5-838f-474b-823f-57b4a3385348', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(parsi_community_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parsi_community_reading, parsi_religious_authority).
narrative_ontology:constraint_beneficiary(parsi_community_reading, endogamy_sustaining_families).
narrative_ontology:constraint_victim(parsi_community_reading, intermarried_individuals).
narrative_ontology:constraint_victim(parsi_community_reading, non_parsi_spouses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a Parsi individual who wishes to marry outside the faith community: the constraint appears as a snare. Identity lock is binding — to exit (marry a non-Parsi spouse) requires abandoning Parsi identity within the community, severing kinship ties, losing ritual participation rights, and accepting permanent social exclusion. The individual has structural mobility (could physically relocate, could marry) but cognitive capture through identity fusion makes this mobility inaccessible. The constraint extracts community compliance through identity threat.
constraint_indexing:constraint_classification(parsi_community_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% From the perspective of a non-Parsi spouse: the constraint is tangled rope. Real coordination function exists — the marriage is legitimated by community recognition, which enables legal status, inheritance rights, and social standing within the community. But this comes at asymmetric cost: the non-Parsi spouse is excluded from core ritual participation, cannot achieve full community membership regardless of conversion, and bears the permanent status of outsider. The extraction is structural (codified legal disabilities) and partially enforced through community social sanctions.
constraint_indexing:constraint_classification(parsi_community_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% From the perspective of the Parsi religious authority (priests, community councils, statute-maintaining institutions): the constraint is rope. The coordination function is genuine — community marriage law provides clear rules for legitimate union, inheritance succession, ritual obligations, and communal standing. This enables order and predictability. The authority benefits from administering this system (maintains institutional role, collects fees for solemnization, preserves religious function). But the classification as rope reflects that the authority experiences the constraint as functional coordination, not as extraction — the authority's interest in sustaining the boundary is aligned with the community's interest in maintaining distinctiveness.
constraint_indexing:constraint_classification(parsi_community_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% From the perspective of endogamy-sustaining family networks: the constraint is tangled rope. Real coordination function: the marriage rules maintain kinship networks, property transmission patterns, and ritual participation structures that families rely on for economic and social stability. But the constraint also extracts through subordinating women's choice (arranged marriage norms embedded in community law), restricting younger generations' marriage options, and requiring continuous enforcement of boundary maintenance. Families experience moderate extraction costs (pressure to monitor children's romantic choices, social enforcement of endogamy norms) alongside coordination benefits.
constraint_indexing:constraint_classification(parsi_community_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From the perspective of Parsi reform and modernization movements: the constraint appears as scaffold. The codified marriage law is understood as a transitional structure — it served legitimate functions in maintaining community distinctiveness under colonial and post-colonial conditions, but its function is atrophying as younger Parsis increasingly intermarry and the community's survival depends on cultural transmission rather than endogamy enforcement. Reformers see a sunset path: gradual integration of intermarried individuals, recognition of mixed-faith families, eventual deprioritization of endogamy in favor of voluntary cultural participation. Exit path exists (reformers have institutional platforms, can influence statute, can create parallel recognition structures).
constraint_indexing:constraint_classification(parsi_community_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From the perspective of the colonial-era statutory framework itself: the constraint is piton. The Parsi Marriage and Divorce Act (1936) and associated community statutes were designed to preserve Parsi legal distinctiveness during British colonial administration — a real coordination function for a minority community seeking autonomy. But the function has largely atrophied in post-colonial India where secular codification and pluralist recognition are ascendant. The statutory framework persists through institutional inertia: community councils maintain the machinery, priests perform solemnizations under its authority, courts occasionally reference its rules. But the theater ratio (0.28) reflects that much of the enforcement is now performative — younger Parsis increasingly view the statute as an artifact, and secular alternatives are available. The piton persists because the community has not formally renounced it, not because it functions.
constraint_indexing:constraint_classification(parsi_community_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational/universal analytical perspective viewing religious community boundaries as fixed social features: the constraint appears as mountain — endogamy enforcement is an unchangeable property of how religious communities maintain identity over generations. Boundary maintenance requires some form of marriage regulation; that regulation inevitably constrains individual choice. This view naturalizes the endogamy rule as an inherent necessity of community survival. However, the structural data (identified beneficiaries, enforcement machinery, contestation among community members) suggests this is a false summit: the constraint is a specific institutional choice (codified statute), not an immutable law of social organization. Alternative readings (secular contractual, Hindu codified, Muslim shariat) show that marriage authority can be organized along different principles.
constraint_indexing:constraint_classification(parsi_community_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parsi_community_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parsi_community_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parsi_community_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(parsi_community_reading, TR),
    TR >= 0.70.

:- end_tests(parsi_community_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts through three mechanisms: (1) identity-locking (individuals cannot exercise exit options because marriage outside the community triggers identity severing); (2) asymmetric legal disabilities (non-Parsi spouses bear permanent outsider status despite legitimate marriage); (3) behavioral control (families must monitor and enforce endogamy norms, constraining romantic choice). But extractiveness is moderate rather than high because: the coordination function is real (the statute does provide clear marriage rules, inheritance clarity, ritual legitimation), the authority operates under community consent (it maintains legitimacy through community recognition, not coercion alone), and exit to the secular state remains formally available (identity-locking is psychological/social, not legal). A higher extractiveness value (0.50+) would apply if endogamy could be shown to be unnecessary for community function or if enforcement were purely coercive with no coordination benefit. Suppression (0.42): Moderate. The suppression mechanism operates through: (1) identity threat (exclusion from community if exit occurs); (2) social sanctions (ostracism, ritual exclusion); (3) family pressure (generational enforcement of endogamy norms); (4) legal disabilities (asymmetric property/inheritance rules). But suppression is not total because alternatives exist (secular marriage is legally available, reformist movements provide counter-authority, younger generations increasingly ignore endogamy norms). The temporal decline in suppression (0.55 → 0.35 over 75 years) reflects decreasing enforcement capacity as generational compliance erodes and secular state institutions provide exit routes. Theater ratio (0.28): Low-moderate. The constraint has genuine coordination function (the statute really does provide marriage rules, inheritance clarity, ritual legitimation) and real beneficiaries genuinely collect from it (priests administer, families maintain kinship networks). But performance content is increasing (0.15 → 0.32) as younger generations perform compliance formally while practicing intermarriage and reformist discourse challenges the statute's legitimacy. The low initial value reflects that the constraint was functionally embedded in community practice mid-20th century; the rising trajectory reflects increasing theatrical maintenance as actual practice diverges from statutory rules.
 *
 * PERSPECTIVAL GAP:
 *   The most dramatic perspectival gap is between the institutional authority (rope) and the powerless agent seeking intermarriage (snare). The authority experiences the constraint as functional coordination — administering clear rules that enable order and community distinctiveness. The powerless agent experiences identity-locking and total extraction — they cannot exercise their formal right to marry secularly because their identity is constituted through community membership, which marriage outside the community severs. This gap reveals that the same institutional structure provides genuine coordination function for actors whose identity interests align with boundary maintenance, while providing extraction mechanism for actors whose identity interests diverge. A secondary gap exists between endogamy-sustaining families (tangled rope) and reformers (scaffold). Families experience the constraint as real but mixed — coordination (kinship networks) with extraction (monitoring enforcement). Reformers experience it as transient and solvable — a temporary structure with a visible sunset as younger generations intermarry and cultural transmission replaces boundary enforcement. The piton perspective (the statute viewed as a degraded institution) reveals that the constraint's functional role has atrophied — it persists through institutional inertia and formal rule maintenance rather than through live functional coordination. The mountain perspective risks naturalizing this contingent institutional choice as an immutable feature of religious community identity itself, when comparative data show communities maintaining distinctiveness through other mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's structural position: power level, exit options, and relationship to the extraction flow. Parsi religious authority (institutional/arbitrage): d ≈ 0.15 (beneficiary with escape options) — collects from administering the system, can exit to secular adjudication if community demands it, but arbitrage option is weak because institutional role depends on community statute persistence. Non-Parsi spouse (moderate/constrained): d ≈ 0.65 (partially targeted, but exit costs are high, not total) — bears asymmetric legal disabilities, but marriage recognition and community membership offer real benefits that constrain exit option value. Parsi individual seeking intermarriage (powerless/identity_locked): d ≈ 0.85 (maximum target, identity-locked prevents exercise of formal exit) — bears full cost of boundary enforcement, and identity fusion makes exit inaccessible from inside the identity frame, so effective extraction is amplified by identity lock. Endogamy-sustaining families (powerful/constrained): d ≈ 0.50 (mixed coordinated and extracted) — benefit from kinship network maintenance, but constrained by need to enforce norms on younger generations, bearing coordination costs. Reform movements (organized/mobile): d ≈ 0.25 (net beneficiaries with exit paths) — have institutional platforms and can influence statute, can create alternatives, can exit to secular regime if necessary. The engine applies f(d) to each d value to compute experienced extractiveness chi, accounting for directionality modulation and scope amplification. The identity_locked exit option amplifies chi for the powerless agent because cognitive entrapment prevents exercise of formal exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandate: To preserve Parsi religious community distinctiveness and maintain clear rules for legitimate marriage, inheritance, and ritual participation under minority status in a plural legal system. Mandatrophy signal: The founding mandate has partially outlived its functional necessity. Parsi community distinctiveness is increasingly maintained through voluntary cultural participation, diaspora networks, and Zoroastrian religious practice — not through endogamy enforcement via statute. Younger generations intermarry at increasing rates, and cultural transmission has not collapsed despite this. The statute persists through institutional inertia (community councils maintain the machinery, priests perform ceremonies, some families enforce endogamy norms) rather than through live functional necessity. However, mandatrophy is NOT complete because some real coordination function remains: the statute does provide clarity for inheritance, property succession, and ritual solemnization for those who choose community-mediated marriage. The tangled-rope classification correctly reflects this partial mandatrophy: the coordination function is real but declining, the extraction mechanism is real and persistent, and the constraint is held together by institutional habit more than by live necessity. The scaffold perspective (reformist movements seeing the structure as temporary) and the piton perspective (statutory framework as degraded institution) both recognize the partial mandatrophy. A full mandatrophy resolution would require either: (1) abandonment of the statute by the community (formal disestablishment), or (2) reformation of the statute to remove endogamy enforcement and identity-locking mechanisms while preserving voluntary coordination functions (e.g., marriage registration service without boundary exclusions). Neither has occurred, so mandatrophy is recognized but not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_delimitation_vs_naturalization,
    'Is this the Parsi community''s authentic reading of marriage authority as it self-understands it, or am I authoring the elite-institutional reading (statutory framework) while suppressing minority and reformist Parsi readings that contest the statute''s legitimacy?',
    'Qualitative survey of Parsi community discourse: whose voices are centered in this reading (priests and statute-maintaining councils vs younger generations, women, intermarried individuals, reform organizations)? The absence of countervoice is itself the signal.',
    'If this reading over-represents institutional authority and under-represents internal contestation, the classification should shift toward snare (extraction is more severe when internal voices contesting it are excluded from the reading itself). If the reading accurately reflects the community''s self-understanding, the tangled-rope classification is defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_delimitation_vs_naturalization, conceptual, 'Whether this is an authentic community reading or an elite-institutional reading').

omega_variable(
    endogamy_coordination_vs_extraction_line,
    'Where is the boundary between legitimate coordination (community distinctiveness maintenance) and extractive enforcement (exclusion of intermarried individuals)? Is the endogamy boundary necessary for community coherence, or is it a mechanism for controlling member behavior?',
    'Comparison with other minority communities: do communities that do not enforce endogamy maintain cultural distinctiveness and intergenerational transmission? (Jewish diaspora communities offer comparison: high intermarriage rates coexist with cultural identity maintenance.) If coordination can occur without boundary enforcement, the statute''s extraction component is higher than claimed.',
    'If endogamy is unnecessary for community function, extractiveness rises to 0.50+, classification shifts toward snare. If endogamy is necessary, extractiveness stays at 0.35, tangled-rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endogamy_coordination_vs_extraction_line, empirical, 'Necessity of endogamy enforcement for community continuity').

omega_variable(
    statute_vs_community_practice_divergence,
    'Does the codified statute (Parsi Marriage and Divorce Act) match actual Parsi marriage practices, or has practice already diverged such that the statute is now primarily a formal rule disconnected from lived community norms?',
    'Comparison of statutory provisions (e.g., marriage solemnization requirements, inheritance rules for intermarried children) with actual practice data: what percentage of Parsi marriages follow statutory procedures? What percentage of Parsi families accept intermarried children despite statutory disabilities? The divergence magnitude is the piton signal.',
    'High divergence (statute > 30% out of sync with practice) indicates piton classification is correct and theater_ratio should rise. Low divergence indicates the statute still functions operationally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statute_vs_community_practice_divergence, empirical, 'Divergence between codified statute and actual marriage practices').

omega_variable(
    reading_foreclosure_mechanics,
    'Does the Parsi community reading''s core premise (that Parsi identity is constituted through religious community membership under codified statute) logically foreclose the secular contractual reading (marriage as individual choice under state law without community mediation)? Or do these readings coexist as live options for different Parsis?',
    'Examine whether younger Parsis who choose secular marriage without community solemnization view themselves as having exited the Parsi reading entirely or as having chosen a different pathway within Parsi identity. Can both be ''Parsi'' simultaneously, or does each reading exclude the other?',
    'If they coexist (both live for different community members): relation = coexists_with. If one forecloses the other (community authority + secular choice are incompatible): relation = forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanics, conceptual, 'Whether community reading and secular reading logically foreclose each other').

omega_variable(
    kernel_identity_stability,
    'Is the marriage authority kernel (what constitutes a legitimate Parsi marriage) stable and shared, or is it itself the site of deep contestation within the community? Is there a single kernel with multiple readings, or multiple competing kernels?',
    'Examine whether different Parsi constituencies (traditionalists, reformers, secular Parsis, women''s groups) agree on what the kernel question is. If constituencies disagree on the question itself (not just its answer), there may be multiple kernels rather than multiple readings of one kernel.',
    'Single kernel + multiple readings: constraint story is valid. Multiple kernels: this story conflates different reading sites and should decompose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_stability, conceptual, 'Whether the marriage authority kernel is unified or fractured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parsi_community_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_tr_t0, parsi_community_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(parsi_tr_t25, parsi_community_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(parsi_tr_t50, parsi_community_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(parsi_tr_t75, parsi_community_reading, theater_ratio, 75, 0.32).

% Extraction over time
narrative_ontology:measurement(parsi_be_t0, parsi_community_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(parsi_be_t25, parsi_community_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(parsi_be_t50, parsi_community_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(parsi_be_t75, parsi_community_reading, base_extractiveness, 75, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(parsi_su_t0, parsi_community_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(parsi_su_t25, parsi_community_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(parsi_su_t50, parsi_community_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(parsi_su_t75, parsi_community_reading, suppression_requirement, 75, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parsi_community_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(parsi_community_reading, 0.12).
narrative_ontology:affects_constraint(parsi_community_reading, marriage_authority_kernel_hindu_codified_reading).
narrative_ontology:affects_constraint(parsi_community_reading, marriage_authority_kernel_muslim_shariat_reading).
narrative_ontology:affects_constraint(parsi_community_reading, marriage_authority_kernel_secular_contractual_reading).
narrative_ontology:affects_constraint(parsi_community_reading, parsi_endogamy_enforcement).
narrative_ontology:affects_constraint(parsi_community_reading, parsi_community_boundaries).

% DUAL FORMULATION NOTE:
% The Parsi community reading is part of a constraint family under the marriage_authority_kernel. Four structurally distinct constraints emerge from the kernel's four readings, linked by network.affects_constraints. Each reading has its own ε value reflecting different beneficiary structures and extraction mechanisms. The Hindu codified reading (state-administered) has lower extraction than the Parsi reading (community-administered with identity-locking). The secular contractual reading (individual choice) has negligible extraction. These are different constraints because their ε values differ fundamentally — they solve different coordination problems and extract from different agents. The family structure is: marriage_authority_kernel (abstract, not modeled as constraint) → {parsi_community_reading, hindu_codified_reading, muslim_shariat_reading, secular_contractual_reading}. Upstream constraints (parsi_endogamy_enforcement, parsi_community_boundaries) feed into this reading's specification of who qualifies for community-mediated marriage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parsi_community_reading, powerless, 0.85).
constraint_indexing:directionality_override(parsi_community_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
