% ============================================================================
% CONSTRAINT STORY: untouchability_abolition_article_17__structural_persistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_untouchability_abolition_article_17__structural_persistence_reading, []).

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
 *   constraint_id: untouchability_abolition_article_17__structural_persistence_reading
 *   human_readable: Untouchability Abolition Article 17 — Structural Persistence Reading
 *   domain: legal/constitutional/labor_caste
 *
 * SUMMARY:
 *   Article 17 of the Indian Constitution formally abolished untouchability
 *   in 1950, declaring it illegal and unenforceable. Yet seventy years later,
 *   manual scavenging remains a segregated occupation, hereditary
 *   occupational segregation persists in rural labor markets, and caste-based
 *   wage suppression continues. This constraint captures a core paradox of
 *   formal legal abolition: the named practice is eliminated in statute, but
 *   the economic and occupational structure it inhabited survives intact. The
 *   structural_persistence_reading locates the core extraction mechanism in
 *   occupational segregation and labor market sorting, not in the explicit
 *   enforcement of caste hierarchy. Where enforcement_gap_reading emphasizes
 *   prosecutorial failure and village-level intimidation of caste-violence
 *   victims, structural_persistence_reading emphasizes that occupations
 *   themselves remain caste-bound through inheritance, skill gatekeeping,
 *   land relations, and wage discrimination — mechanisms that operate
 *   independently of whether explicit caste-based legal enforcement persists.
 *   The beneficiary from this reading's perspective is not primarily
 *   individuals or organized groups, but the formal prohibition itself:
 *   Article 17 succeeds at what it declares (the illegality of the named
 *   practice) while leaving the economic structure intact.
 *
 * KEY AGENTS:
 *   - Manual Scavenger Workers (powerless/trapped): Primary victims; formal abolition has not opened occupational alternatives; wage segregation and hereditary occupational closure persist
 *   - Rural Dalit Communities (moderate/constrained): Secondary victims; land relations and village social structure enforce occupational segregation; generational poverty and social exclusion perpetuate caste labor sorting
 *   - Formal Constitutional Prohibition (institutional/arbitrage): Primary beneficiary of the structural reading; Article 17 succeeds at declaring untouchability unlawful and eliminating the legal caste category, though economic structure persists
 *   - Urban Organized Labor and Occupational Mobility (powerful/mobile): Secondary beneficiary; Article 17 enabled caste mobility in organized sectors (factory work, civil service, education) for those with education and mobility capacity
 *   - Enforcement Apparatus (institutional/arbitrage): Maintains the piton perspective; Civil Rights Act prosecutions and Atrocities Act case law are visible institutional commitments that continue despite low effectiveness at dismantling occupational structure
 *   - Analytical Observer (analytical/analytical): Risks naturalizing structural occupational segregation as an inevitable law of labor market organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(untouchability_abolition_article_17__structural_persistence_reading, 0.58).
domain_priors:suppression_score(untouchability_abolition_article_17__structural_persistence_reading, 0.72).
domain_priors:theater_ratio(untouchability_abolition_article_17__structural_persistence_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(untouchability_abolition_article_17__structural_persistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(untouchability_abolition_article_17__structural_persistence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(untouchability_abolition_article_17__structural_persistence_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(untouchability_abolition_article_17__structural_persistence_reading, tangled_rope).
narrative_ontology:human_readable(untouchability_abolition_article_17__structural_persistence_reading, "Untouchability Abolition Article 17 — Structural Persistence Reading").
narrative_ontology:topic_domain(untouchability_abolition_article_17__structural_persistence_reading, "legal/constitutional/labor_caste").

domain_priors:requires_active_enforcement(untouchability_abolition_article_17__structural_persistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(untouchability_abolition_article_17__structural_persistence_reading, '9a17259f-c1a6-497e-b531-b2cf951c10e1').
narrative_ontology:cs_kernel_codification('9a17259f-c1a6-497e-b531-b2cf951c10e1', formalized).
narrative_ontology:cs_authority_grounding('9a17259f-c1a6-497e-b531-b2cf951c10e1', lineage).
narrative_ontology:cs_interpretation_layer_present('9a17259f-c1a6-497e-b531-b2cf951c10e1').
narrative_ontology:cs_reading_relation('9a17259f-c1a6-497e-b531-b2cf951c10e1', untouchability_abolition_article_17__enforcement_gap_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a17259f-c1a6-497e-b531-b2cf951c10e1', untouchability_abolition_article_17__horizontal_application_reading, coexists_with).
narrative_ontology:cs_axiom('9a17259f-c1a6-497e-b531-b2cf951c10e1', foundational, occupational_structure_persists_independent_of_legal_caste_category).
narrative_ontology:cs_axiom_status(occupational_structure_persists_independent_of_legal_caste_category, holdable).
narrative_ontology:cs_axiom_grounding('9a17259f-c1a6-497e-b531-b2cf951c10e1', occupational_structure_persists_independent_of_legal_caste_category, empirically_contingent).
narrative_ontology:cs_axiom('9a17259f-c1a6-497e-b531-b2cf951c10e1', foundational, formal_abolition_succeeds_at_doctrinal_elimination).
narrative_ontology:cs_axiom_status(formal_abolition_succeeds_at_doctrinal_elimination, holdable).
narrative_ontology:cs_axiom_grounding('9a17259f-c1a6-497e-b531-b2cf951c10e1', formal_abolition_succeeds_at_doctrinal_elimination, conventional).
narrative_ontology:cs_reference_frame('9a17259f-c1a6-497e-b531-b2cf951c10e1', formal_caste_abolition_framework).
narrative_ontology:cs_drift_state('9a17259f-c1a6-497e-b531-b2cf951c10e1', contemporary_labor_market, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a17259f-c1a6-497e-b531-b2cf951c10e1', '').
narrative_ontology:cs_kernel_id(untouchability_abolition_article_17__structural_persistence_reading, untouchability_abolition_article_17).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(untouchability_abolition_article_17__structural_persistence_reading, formal_constitutional_prohibition).
narrative_ontology:constraint_beneficiary(untouchability_abolition_article_17__structural_persistence_reading, urban_organized_labor).
narrative_ontology:constraint_victim(untouchability_abolition_article_17__structural_persistence_reading, manual_scavenger_workers).
narrative_ontology:constraint_victim(untouchability_abolition_article_17__structural_persistence_reading, segregated_occupational_castes).
narrative_ontology:constraint_victim(untouchability_abolition_article_17__structural_persistence_reading, rural_dalit_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANUAL SCAVENGER (SNARE) — Formally no longer classified as 'untouchable' by statute, but the labor market sorts them identically to pre-abolition caste segregation. Exit from manual scavenging is economically impossible (no alternative occupation open; caste-based occupational closure persists). Suppression is complete — the constraint is enforced not by law but by occupational segregation and economic necessity. No coordination benefit; pure extraction through structural sorting.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL DALIT COMMUNITY (TANGLED ROPE) — Land relations and village social structures create some coordination function (mutual aid, occupational specialization) but are deeply extractive along caste lines. Exit from the village community is costly (migration risks, loss of social support, relocation barriers). The constraint coordinates village economics but systematically extracts from lower castes through occupational segregation and wage suppression. Biographical horizon makes the constraint feel unshakeable; generational view would show some mobility.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FORMAL CONSTITUTIONAL PROHIBITION (ROPE) — Article 17's abolition succeeds perfectly at what it attempts: the *naming* of untouchability as a crime is accomplished, the legal category is eliminated. From the perspective of constitutional doctrine, the prohibition is pure coordination — it settles what untouchability *is* and declares it unlawful. This perspective experiences no extraction because its goal (doctrinal clarity) is achieved. The beneficiary from this perspective is the state's own authority to declare and enforce constitutional principles.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: URBAN ORGANIZED LABOR (TANGLED ROPE) — Article 17's abolition succeeded in opening occupational channels for caste mobility in urban organized sectors (factory work, civil service, education). Genuine coordination of occupational access via merit-based systems benefits organized workers. But the constraint also extracts through wage compression (lower wages justified by 'excess supply' from caste mobility) and occupational gatekeeping (traditional castes maintain control of professional certification). Mixed benefit and cost; high agency through unionization and legal advocacy.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ENFORCEMENT APPARATUS (PITON) — Civil Rights Act prosecutions and Atrocities Act case law represent the institutional commitment to abolition, yet conviction rates remain low, intimidation of victims is routine, and villages routinely ignore the law. The enforcement machinery persists (IPC § 501-503, Protection of Civil Rights Act 1955, SC/ST Prevention of Atrocities Act 1989) but is largely performative — the law is cited, cases are filed, but the occupational structure that *is* untouchability continues unchallenged. Theater ratio high because enforcement activity (arrests, trials) is visible but ineffective at dismantling the underlying economic segregation.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, occupational specialization and the persistence of inherited trades might appear to be a natural economic law: societies organize labor by family inheritance and skill transmission, and caste-based occupation is merely a particular instantiation of this universal pattern. The constraint appears immutable because it derives from 'how occupations work.' However, this perspective naturalizes what is a contingent institutional arrangement — the analytical observer risks false summit classification by treating structural economic sorting as a natural law rather than as a persisting extraction mechanism.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(untouchability_abolition_article_17__structural_persistence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(untouchability_abolition_article_17__structural_persistence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(untouchability_abolition_article_17__structural_persistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(untouchability_abolition_article_17__structural_persistence_reading, TR),
    TR >= 0.70.

:- end_tests(untouchability_abolition_article_17__structural_persistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining over the measurement interval (0.78 → 0.58). The constraint extracts through occupational segregation and wage suppression, but extraction has declined as urbanization, education, and legal mobility for some castes has created partial exit routes. The decline is not uniform: manual scavengers show minimal exit despite legal abolition, while upper-castes maintain occupational and educational closure in many sectors. Suppression (0.72): High, declining (0.85 → 0.72). The measured suppression reflects occupational closure, wage gaps, educational barriers, and lack of alternative occupations for hereditary manual scavengers. Suppression has declined somewhat due to affirmative action, public sector employment, and urban labor market opening, but remains severe in rural areas and unorganized sectors. Theater ratio (0.68): Moderate-high, rising (0.55 → 0.68). The increase in theater reflects the growth of visible enforcement activity (prosecutions, atrocities act cases, constitutional litigation) that does not translate into dismantling the underlying occupational structure. The enforcement apparatus has become more performative over time as cases proliferate but convictions remain low and occupational closure persists.
 *
 * PERSPECTIVAL GAP:
 *   The structural_persistence_reading creates a perspectival gap between those who achieved mobility through Article 17 (urban organized labor, upper-caste professionals, civil service beneficiaries) and those trapped in hereditary manual occupations despite formal abolition. The gap reflects that the constraint simultaneously enables and extracts: Article 17 opened pathways for some while leaving others structurally locked. The formal constitutional prohibition sees itself as successful (Rope perspective) because it achieved what it declared. The manual scavenger sees structural entrapment (Snare perspective) because occupational closure persists. The rural Dalit community sees mixed coordination and extraction (Tangled Rope perspective) because village structure provides some mutual aid and occupational specialization while systematically extracting value along caste lines. The piton perspective on the enforcement apparatus reflects that visible legal activity (prosecutions, convictions, atrocities act cases) has increased while the underlying occupational structure remains largely unaffected — enforcement is performative rather than transformative. The analytical observer at civilizational scope risks the false summit by treating inherited occupational segregation as an immutable feature of labor market structure rather than as a persisting extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural_persistence_reading derives directionality from the constraint's occupational sorting mechanism. Manual scavengers are trapped with no legal exit route (d ≈ 0.95, high f(d) → high χ). Rural Dalit communities are constrained by land relations and village structure (d ≈ 0.80, high f(d)). Urban organized labor that achieved mobility through Article 17 experiences moderate extraction through wage compression (d ≈ 0.55, moderate f(d)). The formal constitutional prohibition itself is the beneficiary from this reading: it succeeds at declaring untouchability unlawful (d ≈ 0.05, low/negative f(d)). The enforcement apparatus experiences arbitrage status — it can deploy enforcement selectively and maintain institutional visibility without dismantling the structure (d ≈ 0.15). The analytical observer at civilizational scope risks d ≈ 0.72 (high χ) if adopting a false-summit view that treats occupational structure as a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The structural_persistence_reading resolves the mandatrophy by showing that formal legal abolition and structural occupational persistence are NOT contradictory — they are orthogonal mechanisms. Article 17 succeeded at the doctrinal task (eliminating the legal category of untouchability and the explicit caste-based legal rules). But doctrinal abolition did not address the economic structure that implemented untouchability: occupational inheritance, wage segregation, land relations, skill gatekeeping, and lack of alternative occupations. The reading diagnoses the constraint as Tangled Rope (0.40 ≤ χ ≤ 0.90, beneficiaries + victims + enforcement present) because the structure has both coordination functions (village occupational specialization, some urban occupational mobility) and extraction (occupational closure, wage suppression, inherited occupational segregation). The mandatrophy is resolved by recognizing that the constitutional prohibition is genuinely successful as doctrinal abolition while the extraction mechanism persists through economic structure that the prohibition does not touch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_legal,
    'Is the suppression of manual scavenger exit enforced by legal prohibition, occupational closure, economic dependency, or internalized caste hierarchy?',
    'Empirical analysis of exit barriers: Can a manual scavenger transition to a different occupation if legal barriers are removed? If removal of legal caste bars still produces occupational segregation, suppression is structural rather than legal.',
    'If suppression is primarily legal: removing caste-based occupation bars (already done in statute) should enable exit. If primarily structural: suppression persists through occupational closure and wage gaps independent of stated law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_legal, empirical, 'Whether suppression is legal or structural').

omega_variable(
    beneficiary_identification_constitutional_scope,
    'Who truly benefits from Article 17''s prohibition? Is the beneficiary the formal legal system''s authority, or specific castes who benefit from occupational mobility while others remain trapped?',
    'Distribution analysis of occupational mobility post-abolition: Which caste groups achieved exit from segregated occupations, and which remain concentrated in manual scavenging? Wage premium analysis for those who achieved mobility.',
    'If abstract constitutional benefit is the primary beneficiary: constraint classifies as Rope (pure coordination). If specific castes benefit while others remain trapped: constraint is Snare or Tangled Rope with asymmetric distribution of gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_constitutional_scope, empirical, 'Identity of beneficiary: formal system vs. differential caste mobility').

omega_variable(
    extractiveness_continuation_mechanism,
    'How does extraction continue in the absence of explicit legal caste enforcement? Is it through wage suppression, occupational closure, land relations, credit markets, or internalized hierarchy?',
    'Comparative wage analysis: manual scavengers vs. other low-wage occupations with no caste history; occupational mobility rates by caste cohort; credit access barriers for scavenging castes; land ownership concentration by caste.',
    'If extractiveness is entirely structural (no legal enforcement needed): constraint persists indefinitely unless economic structure changes. If extractiveness depends on specific institutional mechanisms: policy intervention can reduce χ without changing formal law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_continuation_mechanism, empirical, 'Mechanism sustaining extractiveness in absence of explicit legal caste').

omega_variable(
    reading_contest_structural_vs_enforcement,
    'This reading claims the structural persistence of untouchability survives abolition. How does this reading''s claim differ from the enforcement_gap_reading (which emphasizes low conviction rates and village intimidation)?',
    'Doctrinal and empirical analysis: enforcement_gap_reading focuses on the failure of prosecution and punishment of caste-based violence; structural_persistence_reading focuses on occupational sorting and wage segregation that continue independent of whether enforcement succeeds. These are independent mechanisms.',
    'If enforcement gap is the primary mechanism: fixing prosecution rates and strengthening atrocities act enforcement could reduce extractiveness. If structural persistence is primary: better enforcement alone cannot eliminate occupational segregation without labor market intervention. Both mechanisms likely operate simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_structural_vs_enforcement, conceptual, 'Distinction between enforcement failure and structural occupational persistence').

omega_variable(
    horizontal_application_independence,
    'Does the horizontal_application_reading (Article 17 binds all citizens, not just the state) provide a distinct doctrinal pathway that would address the structural persistence claim, or do both readings leave structural occupational segregation unaddressed?',
    'Constitutional case law analysis: Has horizontal application of Article 17 to private occupational practices been tested? Can it reach occupational closure in unorganized sectors? Does constitutional doctrine alone constrain market-mediated occupational segregation?',
    'If horizontal application reaches private markets: structural persistence could be addressed through direct legal constraint on occupational discrimination. If it does not: all readings leave the structural mechanism intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_application_independence, conceptual, 'Whether horizontal Article 17 addresses structural occupational segregation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(untouchability_abolition_article_17__structural_persistence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(untouchability_struct_theater_t0, untouchability_abolition_article_17__structural_persistence_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(untouchability_struct_theater_t20, untouchability_abolition_article_17__structural_persistence_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(untouchability_struct_theater_t40, untouchability_abolition_article_17__structural_persistence_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(untouchability_struct_extract_t0, untouchability_abolition_article_17__structural_persistence_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(untouchability_struct_extract_t20, untouchability_abolition_article_17__structural_persistence_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(untouchability_struct_extract_t40, untouchability_abolition_article_17__structural_persistence_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(untouchability_struct_suppress_t0, untouchability_abolition_article_17__structural_persistence_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(untouchability_struct_suppress_t20, untouchability_abolition_article_17__structural_persistence_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(untouchability_struct_suppress_t40, untouchability_abolition_article_17__structural_persistence_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(untouchability_abolition_article_17__structural_persistence_reading, resource_allocation).
narrative_ontology:affects_constraint(untouchability_abolition_article_17__structural_persistence_reading, manual_scavenging_occupational_closure).
narrative_ontology:affects_constraint(untouchability_abolition_article_17__structural_persistence_reading, caste_wage_suppression_labor_markets).
narrative_ontology:affects_constraint(untouchability_abolition_article_17__structural_persistence_reading, land_relations_rural_dalit).

% DUAL FORMULATION NOTE:
% The untouchability abolition kernel decomposes into at least three structurally distinct constraints with different ε values: (1) enforcement_gap_reading (ε ≈ 0.45, focus on prosecutorial failure and Atrocities Act efficacy); (2) horizontal_application_reading (ε ≈ 0.52, focus on doctrinal scope and private discrimination); (3) structural_persistence_reading (ε ≈ 0.58, focus on occupational segregation and labor market sorting). Each reading's ε captures a different measurable phenomenon. The structural_persistence_reading measures the economic structure's persistence despite legal abolition; enforcement_gap measures prosecutorial efficacy; horizontal_application measures doctrinal reach. Decomposition is necessary to avoid collapsing these distinct mechanisms into a single 'abolition failure' narrative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
