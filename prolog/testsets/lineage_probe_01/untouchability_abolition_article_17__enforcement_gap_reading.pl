% ============================================================================
% CONSTRAINT STORY: untouchability_abolition_article_17__enforcement_gap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_untouchability_abolition_article_17__enforcement_gap_reading, []).

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
 *   constraint_id: untouchability_abolition_article_17__enforcement_gap_reading
 *   human_readable: Article 17 Abolition: Enforcement Gap Between Statute and Village Practice
 *   domain: legal/doctrinal/constitutional_enforcement
 *
 * SUMMARY:
 *   Article 17 of the Indian Constitution formally abolished untouchability
 *   in 1950. The enforcement_gap_reading instantiates one structural reading
 *   of this contested kernel: the abolition succeeded as formal declaration
 *   but failed in delivery, creating an enforcement gap where the statute
 *   exists on paper but protection is absent in practice. This reading
 *   focuses on the suppression mechanism — victims face hostile or negligent
 *   local enforcement machinery — and the beneficiary, which is the formal
 *   equality record itself (the constitutional order's prestige and national
 *   legitimacy). The constraint exhibits the mixed coordination-extraction
 *   profile of Tangled Rope: the statute coordinates national discourse
 *   around formal equality while extracting a cost from local victims who
 *   face retaliation without reliable state protection. Over the 50-year
 *   interval (1950-2000), extractiveness decreases from 0.72 to 0.58 as
 *   enforcement capacity marginalizes builds (though remains incomplete),
 *   while suppression decreases from 0.78 to 0.68 as awareness and advocacy
 *   increase. Theater ratio increases from 0.55 to 0.65, indicating that
 *   enforcement machinery becomes increasingly ritualistic — FIRs are filed
 *   and prosecutions begun, but conviction rates remain low and social
 *   enforcement of untouchability continues, revealing the machinery as
 *   performative. The constraint is one reading of a contested kernel that
 *   also admits horizontal_application_reading (untouchability persists
 *   because Article 17 binds only the state, not private actors) and
 *   structural_persistence_reading (untouchability persists as
 *   occupation-based economic segregation despite legal abolition). This
 *   reading emphasizes enforcement failure; the siblings emphasize horizontal
 *   application failure and structural economic persistence, respectively.
 *
 * KEY AGENTS:
 *   - Dalit litigants and caste-oppressed populations (local, powerless/trapped): Primary victims. Facing untouchability practices in villages without reliable state enforcement. Maximum experienced extraction.
 *   - Formal equality record (national, institutional/arbitrage): Primary beneficiary. Constitutional order's prestige and legitimacy derive from Article 17's existence. Benefits from the statute without requiring complete enforcement.
 *   - Regional Dalit political organizations (regional, moderate/constrained): Secondary beneficiary and secondary victim. Benefit from the statute as a focal point for mobilization; bear extraction through having to replicate enforcement work.
 *   - Upper-caste dominant factions (local, powerful/constrained): Constrained by the statute's formal prohibition but benefit from the statute's coordination function (allows them to participate in national discourse without defending caste hierarchy). Experience mixed coordination and constraint.
 *   - Enforcement machinery: police, courts, district administration (institutional, institutional/arbitrage): Maintain enforcement as ritual. Theater ratio high, functional verification low. Piton classification — institutional inertia, not active extraction from the constraint itself.
 *   - Analytical observer (civilizational, analytical/analytical): Risks reading the enforcement gap as a natural law of caste systems rather than a contingent institutional failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(untouchability_abolition_article_17__enforcement_gap_reading, 0.58).
domain_priors:suppression_score(untouchability_abolition_article_17__enforcement_gap_reading, 0.68).
domain_priors:theater_ratio(untouchability_abolition_article_17__enforcement_gap_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(untouchability_abolition_article_17__enforcement_gap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(untouchability_abolition_article_17__enforcement_gap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(untouchability_abolition_article_17__enforcement_gap_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(untouchability_abolition_article_17__enforcement_gap_reading, tangled_rope).
narrative_ontology:human_readable(untouchability_abolition_article_17__enforcement_gap_reading, "Article 17 Abolition: Enforcement Gap Between Statute and Village Practice").
narrative_ontology:topic_domain(untouchability_abolition_article_17__enforcement_gap_reading, "legal/doctrinal/constitutional_enforcement").

domain_priors:requires_active_enforcement(untouchability_abolition_article_17__enforcement_gap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(untouchability_abolition_article_17__enforcement_gap_reading, '56a3f356-ca04-4f44-a90b-7897de13e45a').
narrative_ontology:cs_kernel_codification('56a3f356-ca04-4f44-a90b-7897de13e45a', formalized).
narrative_ontology:cs_authority_grounding('56a3f356-ca04-4f44-a90b-7897de13e45a', lineage).
narrative_ontology:cs_interpretation_layer_present('56a3f356-ca04-4f44-a90b-7897de13e45a').
narrative_ontology:cs_reading_relation('56a3f356-ca04-4f44-a90b-7897de13e45a', untouchability_abolition_article_17__horizontal_application_reading, coexists_with).
narrative_ontology:cs_reading_relation('56a3f356-ca04-4f44-a90b-7897de13e45a', untouchability_abolition_article_17__structural_persistence_reading, coexists_with).
narrative_ontology:cs_axiom('56a3f356-ca04-4f44-a90b-7897de13e45a', foundational, statute_sufficient_for_formal_abolition).
narrative_ontology:cs_axiom_status(statute_sufficient_for_formal_abolition, holdable).
narrative_ontology:cs_axiom_grounding('56a3f356-ca04-4f44-a90b-7897de13e45a', statute_sufficient_for_formal_abolition, deontological).
narrative_ontology:cs_axiom('56a3f356-ca04-4f44-a90b-7897de13e45a', foundational, enforcement_gap_as_contingent_failure).
narrative_ontology:cs_axiom_status(enforcement_gap_as_contingent_failure, holdable).
narrative_ontology:cs_axiom_grounding('56a3f356-ca04-4f44-a90b-7897de13e45a', enforcement_gap_as_contingent_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('56a3f356-ca04-4f44-a90b-7897de13e45a', constitutional_abolition_as_formal_status).
narrative_ontology:cs_drift_state('56a3f356-ca04-4f44-a90b-7897de13e45a', contemporary_village_practice_2000, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('56a3f356-ca04-4f44-a90b-7897de13e45a', '').
narrative_ontology:cs_kernel_id(untouchability_abolition_article_17__enforcement_gap_reading, untouchability_abolition_article_17).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(untouchability_abolition_article_17__enforcement_gap_reading, formal_equality_record).
narrative_ontology:constraint_beneficiary(untouchability_abolition_article_17__enforcement_gap_reading, constitutional_prestige).
narrative_ontology:constraint_victim(untouchability_abolition_article_17__enforcement_gap_reading, dalit_litigants).
narrative_ontology:constraint_victim(untouchability_abolition_article_17__enforcement_gap_reading, caste_oppressed_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DALIT LITIGANT (SNARE) — A person facing untouchability practices in the village must rely on enforcement machinery that is locally hostile, under-resourced, and socially contravened by dominant castes. Exit is impossible: they cannot leave their village economy, cannot access alternative dispute resolution outside the caste-controlled local structure. The constraint extracts maximum cost — they bear full risk of retaliation without reliable state protection. The enforcement gap means the statute protects them on paper only.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL DALIT POLITICAL ORGANIZATION (TANGLED ROPE) — Dalit advocacy groups benefit from the Article 17 statute as a coordination tool for collective action (litigation, public campaigns, legislative pressure). The statute provides a focal point for mobilization. However, they also bear extraction: enforcement bottlenecks force them to replicate the state's work (investigative documentation, witness protection, local coordination). The constraint both enables and constrains — coordination without symmetry.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FORMAL EQUALITY RECORD (ROPE) — From the perspective of the constitutional order as a system, Article 17 succeeds in pure coordination: it establishes the legal status of untouchability as abolished, creating a unified frame for national discourse. The statute benefits the constitutional order's legitimacy through formal equality declaration, without requiring enforcement to be uniform or complete. The constitutional system experiences this as successful coordination — the formal record is clean.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UPPER-CASTE DOMINANT FACTION (TANGLED ROPE) — Local dominant castes benefit from the coordination function of Article 17 (formal abolition allows them to participate in national/constitutional discourse without defending caste hierarchy explicitly). However, they are constrained by the statute's formal prohibition, which prevents open enforcement of untouchability and creates litigation risk. They experience mixed coordination and constraint — the statute coordinates them into the national order while constraining their local practices.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ENFORCEMENT MACHINERY (PITON) — The police, courts, and district administration maintain the enforcement process as ritual. FIRs are filed, investigations occur (often perfunctorily), some prosecutions proceed but with low conviction rates. The machinery persists through institutional inertia — it is seen as necessary to legitimacy but functionally degraded. Theater ratio is high: the machinery performs enforcement without reliably preventing untouchability practices. Piton arises from the theater gate, not from experienced extraction by this agent (they are not extracting from the constraint; they maintain it performatively).
constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the enforcement gap appears as a natural law of caste systems: deep social structures persist regardless of formal prohibition. The gap between statute and practice is seen as an immutable feature of how entrenched hierarchies resist legal reform. This perspective risks naturalizing what is actually a contingent enforcement failure, institutional underfunding, and political choice.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(untouchability_abolition_article_17__enforcement_gap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(untouchability_abolition_article_17__enforcement_gap_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(untouchability_abolition_article_17__enforcement_gap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(untouchability_abolition_article_17__enforcement_gap_reading, TR),
    TR >= 0.70.

:- end_tests(untouchability_abolition_article_17__enforcement_gap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The statute provides formal abolition but enforcement failure means victims bear extraction costs (retaliation without state protection). Extractiveness is not maximal (0.72 snare level) because: (a) some enforcement capacity exists and improves over time, (b) regional Dalit organizations provide partial substitution for state enforcement, (c) the statute serves as a focal point for mobilization that constrains dominant-caste actors' ability to enforce untouchability openly. The measurement trajectory (0.72 → 0.65 → 0.58) captures this improvement. Suppression (0.68): High. Multiple barriers prevent victims from exiting the constraint: they cannot leave the village economy, cannot reliably access alternative dispute resolution, face social retaliation for invoking the statute, and lack enforcement protection. However, suppression is not maximal (0.85 mountain level) because: (a) some victims do escape through migration or urban employment, (b) educational access increases exit options for subsequent generations, (c) litigation, though risky, is possible. The measurement trajectory (0.78 → 0.72 → 0.68) captures the gradual reduction of suppression as alternative structures emerge. Theater ratio (0.65): Moderate-high. Enforcement machinery files FIRs, conducts investigations, initiates prosecutions — the rituals of enforcement are performed. Conviction rates, however, are low (often < 10% in documented cases), and many prosecutions are abandoned at district magistrate stage. The machinery's function (preventing untouchability) is substantially decoupled from its performance (conducting enforcement rituals). Theater increases over time (0.55 → 0.65) as the machinery becomes more formalized and ritualistic.
 *
 * PERSPECTIVAL GAP:
 *   The enforcement gap creates a perspectival chasm. The constitutional order (institutional/arbitrage) sees Article 17 as successful — formal equality is declared, the statute exists, enforcement machinery performs its rituals. The Dalit litigant (powerless/trapped) sees pure extraction — the statute is a trap, invoking it brings retaliation without protection. The regional organization (moderate/constrained) sees mixed coordination and constraint — the statute enables mobilization but forces them to replicate enforcement work. The dominant faction (powerful/constrained) sees constraint — the statute limits their open enforcement capacity while preserving their substantive caste control. The enforcement machinery (institutional/arbitrage) performs its role as degraded ritual (Piton) — it maintains legitimacy through procedure while failing in function. The analytical observer risks naturalizing this gap as inherent to social change rather than recognizing it as a contingent institutional failure. The classification across perspectives ranges from Rope (constitutional order) through Tangled Rope (organizations, dominant factions) to Snare (litigants) to Piton (machinery) to Mountain (analytical risk) — the full spectrum of constraint types, revealing that the enforcement gap is not a single constraint but a relational structure that produces different constraints depending on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations plus each perspective's power level, time horizon, and exit options. Formal equality record (institutional/arbitrage) derives low d (~0.15): beneficiary with exit capacity, experiences negative or minimal χ. Dalit litigants (powerless/trapped) derive high d (~0.95): victims with no exit, experience maximum χ via high f(d) (1.42 approx). Regional organizations (moderate/constrained) derive moderate d (~0.55): both victims (bearing extraction) and beneficiaries (using statute for mobilization), moderate experienced extraction. Dominant factions (powerful/constrained) derive d (~0.40): beneficiaries in some ways (statute coordinates them into national order), constrained in others (statute limits open enforcement), net slightly beneficiary. Enforcement machinery (institutional/arbitrage) is not a victim or beneficiary of the constraint itself but a performer of it — analytical perspective, d ~0.73. The χ formula then scales these d values by f(d) and scope modifiers: powerless agents at local scope experience maximum χ; institutional beneficiaries experience negative or minimal χ; moderate agents with regional scope experience intermediate χ. This generates the perspectival gap: same constraint, vastly different experienced extractiveness depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is averted by recognizing that Tangled Rope classification is correct: the constraint is both coordination (the statute coordinates national discourse, provides focal point for mobilization, constrains open enforcement) and extraction (suppression is high, enforcement is hostile/negligent, victims bear maximum cost). The classification does not collapse into mislabeled Rope because victims are identified and asymmetric extraction is documented. The classification does not collapse into mislabeled Snare because a genuine coordination function exists (the statute is used for mobilization, does constrain dominant-caste actors, does provide national focal point). The constraint is a hybrid. Mandatrophy is also resolved by clarifying that different perspectives produce different types (Mountain, Piton, Snare, Rope, Tangled Rope across the six perspectives), not because the base_properties are ambiguous but because the constraint is genuinely relational — it exists differently in different structural positions. The analytical observer's temptation to classify the enforcement gap as Mountain (immutable law of caste systems) is revealed as a false summit: the gap is a contingent institutional failure, not a natural law. The formal equality record's Rope classification (pure coordination) is revealed as beneficiary capture of the narrative — the rope conceals the snare experienced by those at the local level. The Piton classification of enforcement machinery reveals the ritualization: the machinery persists through institutional inertia, not because it functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_resource_allocation,
    'Is the enforcement gap primarily a failure of resource allocation to untouchability cases, or a failure of political will in state enforcement machinery?',
    'Comparative analysis of resource allocation across crime categories; FIR filing rates vs conviction rates for untouchability vs other social crimes; interview data from police and prosecutors on priority assignment',
    'If resource allocation: the constraint is a coordination problem solvable by budget reallocation (Rope emphasis). If political will: the constraint is extractive, with enforcement machinery complicit in suppression (Snare emphasis). Current classification assumes mixed: extraction occurs through both under-resourcing AND hostile/negligent enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_resource_allocation, empirical, 'Resource allocation vs political will in enforcement failure').

omega_variable(
    village_enforcement_autonomy,
    'Can local enforcement machinery (police at village level) act independently of dominant-caste social pressure, or is enforcement capacity structurally captured by the local power structure?',
    'Institutional analysis of police recruitment, promotion, transfer patterns in villages with high untouchability incidents; correlation between dominant-caste land ownership and police action rates; ethnographic documentation of police-dominant-caste relationships',
    'If autonomous: enforcement is a coordination problem (Rope/Scaffold perspectives strengthen). If captured: enforcement is subordinated to caste extraction (Snare/Tangled Rope emphasize asymmetry). This omega targets the structural difference between the enforcement gap as coordination failure vs enforcement gap as complicit extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(village_enforcement_autonomy, empirical, 'Whether local enforcement machinery is structurally captured by dominant-caste interests').

omega_variable(
    statute_as_focal_point_sufficiency,
    'Does Article 17 function as a coordination focal point for mobilization, or does the enforcement gap delegitimize the statute and prevent collective action?',
    'Documentation of Dalit organizational strategies: cases where Article 17 is used as mobilization tool vs cases abandoned due to enforcement failure; quantitative analysis of litigation volume and settlement rates; interviews with advocacy groups on statute utility',
    'If focal point works: the constraint is a genuine Tangled Rope (coordination present alongside extraction). If delegitimized: the constraint is closer to Snare (formal equality record exists; protection does not). This targets the question of whether coordination function persists despite enforcement failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statute_as_focal_point_sufficiency, empirical, 'Whether Article 17 functions as a sufficient focal point for Dalit collective action').

omega_variable(
    alternative_enforcement_mechanisms,
    'Do non-state enforcement mechanisms (community accountability, Dalit institutions, civil society monitoring) partially substitute for state enforcement?',
    'Ethnographic documentation of alternative accountability structures; analysis of cases resolved outside formal courts; mapping of community-based remedies and their effectiveness',
    'If substitution occurs: the constraint''s suppression is lower than measured (enforcement gap is partially bridged). If substitution fails: suppression remains at measured levels. This affects the baseline extractiveness value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_enforcement_mechanisms, empirical, 'Extent of substitution of non-state enforcement mechanisms for state enforcement').

omega_variable(
    kernel_reading_question__enforcement_gap_vs_others,
    'Is untouchability''s persistence defined by the enforcement gap (this reading), by the failure of horizontal application between private actors (sibling: horizontal_application_reading), or by structural economic persistence despite formal abolition (sibling: structural_persistence_reading)?',
    'Causal analysis distinguishing which mechanism dominates empirical cases: (a) victims could organize if enforcement existed but cannot due to enforcement absence; (b) dominant castes can enforce untouchability through private action because Article 17 binds only the state; (c) occupation-based segregation persists as economic structure regardless of legal status. Documentation of dominant-caste enforcement mechanisms, untouchability incident types, and victim organizational capacity across cases.',
    'If enforcement gap dominates: this reading''s classification (Tangled Rope with suppression incomplete at delivery point) holds. If horizontal-application gap dominates: extractiveness shifts upward (state not bounding private caste enforcement); sibling reading is primary. If structural-economic persistence dominates: extractiveness is sustained by economic interdependency, not legal enforcement — constraint is different (sibling reading is primary). The three readings coexist structurally but may have different empirical dominance in different villages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_question__enforcement_gap_vs_others, empirical, 'Which enforcement mechanism dominates the persistence of untouchability: state enforcement gap, horizontal application gap, or structural economic persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(untouchability_abolition_article_17__enforcement_gap_reading, 1950, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1950_early_statute, untouchability_abolition_article_17__enforcement_gap_reading, theater_ratio, 1950, 0.55).
narrative_ontology:measurement(theater_1975_formalization, untouchability_abolition_article_17__enforcement_gap_reading, theater_ratio, 1975, 0.62).
narrative_ontology:measurement(theater_2000_ritualization, untouchability_abolition_article_17__enforcement_gap_reading, theater_ratio, 2000, 0.65).

% Extraction over time
narrative_ontology:measurement(extractiveness_1950_statute_baseline, untouchability_abolition_article_17__enforcement_gap_reading, base_extractiveness, 1950, 0.72).
narrative_ontology:measurement(extractiveness_1975_early_enforcement, untouchability_abolition_article_17__enforcement_gap_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement(extractiveness_2000_contemporary, untouchability_abolition_article_17__enforcement_gap_reading, base_extractiveness, 2000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1950_statute_baseline, untouchability_abolition_article_17__enforcement_gap_reading, suppression_requirement, 1950, 0.78).
narrative_ontology:measurement(suppression_1975_early_enforcement, untouchability_abolition_article_17__enforcement_gap_reading, suppression_requirement, 1975, 0.72).
narrative_ontology:measurement(suppression_2000_contemporary, untouchability_abolition_article_17__enforcement_gap_reading, suppression_requirement, 2000, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(untouchability_abolition_article_17__enforcement_gap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(untouchability_abolition_article_17__enforcement_gap_reading, untouchability_abolition_article_17__horizontal_application_reading).
narrative_ontology:affects_constraint(untouchability_abolition_article_17__enforcement_gap_reading, untouchability_abolition_article_17__structural_persistence_reading).

% DUAL FORMULATION NOTE:
% The enforcement_gap_reading decomposes from the contested kernel untouchability_abolition_article_17 as one structural reading. The sibling readings (horizontal_application_reading, structural_persistence_reading) emphasize different mechanisms of untouchability's persistence: horizontal application failure vs economic structural persistence. These three stories are NOT alternatives to be arbitrated — they are complementary structural readings of a contested kernel that admits multiple coherent framings. Each reading has its own ε value reflecting the mechanism it emphasizes. The enforcement_gap_reading (ε=0.58) emphasizes incomplete enforcement delivery; the horizontal_application_reading (expected higher ε) emphasizes state non-responsibility; the structural_persistence_reading (expected different ε) emphasizes occupation-based economic segregation. They coexist in village reality and in the jurisprudential literature. Link all three via network.affects_constraints to document the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
