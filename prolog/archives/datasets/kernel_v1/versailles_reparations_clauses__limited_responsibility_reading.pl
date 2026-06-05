% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Clauses (Limited Responsibility Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles Treaty's reparations clauses contain an irreducible
 *   ambiguity: are payments bounded by moral responsibility for war (punitive
 *   reading), by economic capacity to pay (limited responsibility reading),
 *   or by the treaty's alleged illegitimacy (repudiation reading)? This
 *   constraint instantiates the limited responsibility reading, which claims
 *   that Article 231's responsibility clause is a legal formality, not a
 *   moral judgment, and that reparations must align with German economic
 *   capacity rather than Allied maximalist demands. This reading emerged from
 *   Weimar negotiators' economic arguments and was partially codified in the
 *   Dawes Plan (1924), which revised payment schedules downward and
 *   established the capacity principle. The constraint exhibits tangled
 *   coordination-extraction dynamics: it stabilizes the interstate system by
 *   preventing German economic collapse (coordination function) while
 *   simultaneously constraining Allied claims and protecting German elite
 *   wealth preservation (extractive function). The theater ratio rises over
 *   the interval as the 'economic viability' framing becomes increasingly
 *   performative—used as a shield against payment demands while German
 *   capital accumulates and labor suppression increases.
 *
 * KEY AGENTS:
 *   - German Economic Elites: Primary beneficiary (institutional/arbitrage) — capacity argument protects capital from seizure and enables wealth preservation through payment negotiation
 *   - Weimar State Negotiators: Secondary beneficiary (institutional/arbitrage) — limited responsibility reading gives them leverage to revise terms downward and stabilize state creditworthiness
 *   - Allied Creditor Nations: Primary victim-beneficiary (powerful/constrained) — constrained by need for German solvency, yet extract through reparations; capacity bound suppresses their maximalist claims
 *   - Occupied Territories (Belgium, Northern France): Secondary victim (moderate/constrained) — legitimate reconstruction claims suppressed relative to reparations demands; constrained by military occupation
 *   - Future German Labor Force: Tertiary victim (powerless/trapped) — bears intergenerational debt servicing through wage suppression and public investment foregone; trapped by prior commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.35).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.48).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Clauses (Limited Responsibility Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '57fb14fa-d539-430c-835f-5fae132a247c').
narrative_ontology:cs_kernel_codification('57fb14fa-d539-430c-835f-5fae132a247c', formalized).
narrative_ontology:cs_authority_grounding('57fb14fa-d539-430c-835f-5fae132a247c', extraction).
narrative_ontology:cs_interpretation_layer_present('57fb14fa-d539-430c-835f-5fae132a247c').
narrative_ontology:cs_reading_relation('57fb14fa-d539-430c-835f-5fae132a247c', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('57fb14fa-d539-430c-835f-5fae132a247c', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('57fb14fa-d539-430c-835f-5fae132a247c', foundational, payments_must_align_with_capacity).
narrative_ontology:cs_axiom_status(payments_must_align_with_capacity, holdable).
narrative_ontology:cs_axiom_grounding('57fb14fa-d539-430c-835f-5fae132a247c', payments_must_align_with_capacity, empirically_contingent).
narrative_ontology:cs_axiom('57fb14fa-d539-430c-835f-5fae132a247c', foundational, article_231_is_legal_formality_not_moral_judgment).
narrative_ontology:cs_axiom_status(article_231_is_legal_formality_not_moral_judgment, holdable).
narrative_ontology:cs_axiom_grounding('57fb14fa-d539-430c-835f-5fae132a247c', article_231_is_legal_formality_not_moral_judgment, conventional).
narrative_ontology:cs_reference_frame('57fb14fa-d539-430c-835f-5fae132a247c', capacity_bounded_reparations_regime).
narrative_ontology:cs_drift_state('57fb14fa-d539-430c-835f-5fae132a247c', contemporaneous_1926, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('57fb14fa-d539-430c-835f-5fae132a247c', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_economic_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, weimar_state_negotiators).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_nations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, future_german_labor_force).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GERMAN LABOR FORCE (SNARE) — Powerless and trapped by intergenerational debt; bears extraction through wage suppression, austerity, and foregone investment in public goods. Cannot negotiate terms of their own servitude. The reparations constraint, framed as economic viability, actually preserves extractive debt servicing across generations.
constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OCCUPIED TERRITORIES (SNARE) — Moderate but constrained. Victims of war destruction with legitimate claims for reconstruction. The limited responsibility reading suppresses their compensation relative to reparations demands on Germany — the constraint trades occupied territory compensation for German economic viability, creating asymmetric extraction. Their exit option is constrained by military occupation and allied power dynamics.
constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED CREDITOR NATIONS (TANGLED ROPE) — Powerful but constrained by economic necessity to maintain German solvency. The limited responsibility reading serves as coordination mechanism (preventing total German collapse that would default all reparations) AND extraction mechanism (channeling recovered capital to victors while limiting occupied territory compensation). Exit is constrained by the requirement that Germany remain economically viable to pay. Asymmetric: victors extract while bearing minimal ongoing cost.
constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: WEIMAR STATE NEGOTIATORS (ROPE) — Institutional actors with arbitrage options. The limited responsibility reading serves as their coordination solution: accepting bounded reparations enables economic planning and international credit access. They experience this constraint as coordination (stabilizing the state) rather than pure extraction. The 'viability' framing gives them negotiating leverage to revise terms downward while maintaining legitimacy.
constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GERMAN ECONOMIC ELITES (ROPE) — Primary beneficiaries. The limited responsibility reading constrains Allied maximalism and protects German capital accumulation. Elites maintain arbitrage options: they can negotiate payment schedules, defer obligations, and use 'economic viability' as a shield against asset seizure or nationalization. They experience this constraint as coordination that enables their survival and wealth preservation.
constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The limited responsibility reading stabilizes an institutional framework (international law, state creditworthiness, enforceable contracts) while embedding extractive asymmetries inside the legitimacy apparatus itself. Article 231 as 'legal formality not moral judgment' is the precise mechanism: it bounds the normative critique of German responsibility while justifying the payment constraint. The constraint coordinates the interstate system AND extracts from those trapped in the economic consequences.
constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(versailles_reparations_clauses__limited_responsibility_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The limited responsibility reading reduces extraction relative to pure punitive scenarios (ε would approach 0.65+) by constraining payments to 'capacity.' However, it remains extractive because the capacity definition itself is contested and favorable to elites—they control the calculation of what Germany can 'viably' pay. The extraction is partial rather than total because the coordination function (preventing state collapse) is genuine; the interstate system does stabilize. The measurement trajectory shows increasing extractiveness from 1919 (0.28, initial reparations optimism) to 1929 (0.42, as the Dawes Plan's downward revisions become extractive in practice—protecting German elites while channeling payments to victors). Suppression (0.48): Moderate. The constraint suppresses alternative framings (repudiation, international debt restructuring, wealth redistribution) and constrains agents' negotiating positions. German labor faces wage suppression and public investment cuts; occupied territories face reduced compensation; future generations face debt servicing. However, suppression is not extreme (would be 0.65+) because agents retain nominal agency—negotiation, partial defaults, and eventual renunciation (1932) remain structurally possible, though costly. Theater ratio (0.62): Rising. The 'economic viability' framing is initially substantive (1919: 0.55, genuine debates about capacity) but becomes increasingly performative (1929: 0.68) as it is used as a shield against payment demands while German capital accumulates and labor conditions deteriorate. The constraint is enforced not primarily through force but through institutional theater—the Reparations Commission, periodic review conferences, and the rhetoric of 'scientific' economic assessment.
 *
 * PERSPECTIVAL GAP:
 *   The limited responsibility reading produces sharp perspectival gaps. German elites see a coordination mechanism (Rope) that stabilizes the state and enables economic planning. Weimar negotiators see an asset-protection mechanism that gives them leverage to revise terms downward. Allied creditors see a mixed constraint: it coordinates the interstate system and ensures some reparations flow, but it suppresses their maximalist claims. Occupied territories see extraction (Snare) because their reconstruction claims are subordinated to German capacity. Future German labor sees intergenerational debt servicing (Snare). The analytical observer sees the full tangled structure: genuine coordination (preventing collapse) embedded with asymmetric extraction (capacity bound protects elites, suppresses alternatives, redistributes wealth upward). The false summit risk: framing 'economic viability' as a natural economic law rather than as a contestable institutional choice that benefits elites.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (German economic elites, Weimar negotiators) derive d ≈ 0.20 (low extraction from the constraint): they have institutional power, arbitrage options (can negotiate terms, defer, restructure), and the constraint directly protects their interests by bounding Allied claims. The sigmoid f(d) produces low effective extraction for these agents. Victims (occupied territories, future labor force) derive d ≈ 0.85 (high extraction): they are trapped or powerless, have no arbitrage, and bear the constraint's costs directly. Allied creditors have mixed directionality (d ≈ 0.55): they benefit from reparations extraction but are constrained by German solvency requirements—they experience the constraint as imposing bounds on their preferred maximalism. The analytical observer (d ≈ 0.72) sees the full structure: coordination function + asymmetric extraction embedded in the legitimacy apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   The limited responsibility reading resolves mandatrophy by clarifying what function the reparations constraint serves. It is NOT pure extraction (Snare) because it does coordinate the interstate system and prevent German collapse, producing genuine coordination benefits (some alignment of incentives, stabilized expectations). It is NOT pure coordination (Rope) because it protects German elite wealth at the expense of occupied territories and future labor—asymmetric extraction persists. It IS tangled because both functions are structurally necessary: the coordination function (capacity bound) is what makes the extraction function (protection of elites) politically sustainable. Eliminating the capacity principle would trigger German default, destabilizing Allied finance; eliminating the elite protection would trigger German political collapse. The tangled structure is the actual equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_viability_threshold,
    'What payment schedule constitutes ''economic viability'' for Germany? Is the threshold determined by German elites'' preferred accumulation rate, by minimum subsistence for the population, or by something between?',
    'Comparative analysis of Weimar payment schedules under different reparations regimes (Dawes Plan, Young Plan, actual default trajectory). Correlation with German wages, public investment, and capital flight patterns.',
    'If threshold is elite accumulation: viability framing is explicit protection of wealth concentration. If threshold is subsistence: reparations remain extractive but legitimately bounded. This resolves whether the limited responsibility reading is genuinely coordination or elite-protecting extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_viability_threshold, empirical, 'Definition of economic viability threshold for reparations payment').

omega_variable(
    article_231_causality_ambiguity,
    'Does Article 231 establish German causal responsibility for war damage (moral judgment), or merely state a legal fiction to ground reparations claims (legal formality)? The limited responsibility reading claims the latter, but the punitive reading claims the former.',
    'Textual analysis of Article 231''s legislative history; comparison with other treaty language on responsibility; examination of how different signatories interpreted its binding force (German delegation vs Allied powers).',
    'If Article 231 IS causal responsibility judgment: limited responsibility reading suppresses this and enables reduced payments. If Article 231 is formality only: limited responsibility reading correctly identifies it, and payment bounds follow legitimately from economic constraints rather than moral judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_causality_ambiguity, conceptual, 'Whether Article 231 establishes moral judgment or legal formality').

omega_variable(
    competing_kernel_framings,
    'This reading is one interpretation of the versailles_reparations_clauses kernel. What makes this reading distinct from the punitive_liability_reading and repudiation_reading?',
    'Committer-axis analysis: this reading''s foundational axiom is that payments must align with capacity, establishing economic viability as the primary constraint. The punitive reading''s axiom is that moral responsibility justifies maximalist extraction regardless of capacity. The repudiation reading''s axiom is that the entire treaty is illegitimate and void.',
    'This omega documents the kernel reading structure itself. The three readings coexist in actual interstate diplomacy (different nations and factions hold each). Recognition that this is ONE reading, not the definitive interpretation, prevents false summit naturalization of this specific framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernel_framings, conceptual, 'Kernel reading identity: limited responsibility vs punitive vs repudiation framings').

omega_variable(
    intergenerational_extraction_mechanism,
    'Does the limited responsibility reading''s ''viability'' constraint actually reduce total German extraction, or does it redistribute extraction across time (reducing immediate burden while extending debt servicing across generations)?',
    'Net present value analysis of reparation schedules under different payment regimes; comparison of total historical extraction (interest + principal) across payment timelines; wage trajectory analysis for German labor force across different reparations scenarios.',
    'If viability reduces total extraction: the tangled rope classification is appropriate. If viability merely postpones extraction across generations: the constraint is more snare-like for future agents, and the limited responsibility reading is intergenerational extraction masquerading as pragmatism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_extraction_mechanism, empirical, 'Whether economic viability bound reduces or redistributes total extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(verep_theater_1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(verep_theater_1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(verep_theater_1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(verep_extractiveness_1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(verep_extractiveness_1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(verep_extractiveness_1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 10, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(verep_suppression_1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(verep_suppression_1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(verep_suppression_1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, dawes_plan_enforcement).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, weimar_hyperinflation_debt_trap).

% DUAL FORMULATION NOTE:
% The three readings of the versailles_reparations_clauses kernel (limited_responsibility, punitive_liability, repudiation) are separate constraint stories with different ε values and different victim/beneficiary structures, instantiating different interpretations of the same historical text. Link all three via network.affects_constraints to show they are readings of the same kernel. The limited_responsibility reading here (ε=0.35, Tangled Rope) differs structurally from punitive_liability_reading (ε would be higher, more Snare-like, maximalist extraction) and repudiation_reading (ε would be lower, more Rope-like, pure coordination of exit). The Dawes Plan and hyperinflation consequences are downstream constraints affected by which reading prevailed institutionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, institutional, 0.18).
constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
