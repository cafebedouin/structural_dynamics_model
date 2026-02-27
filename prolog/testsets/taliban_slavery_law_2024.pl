% ============================================================================
% CONSTRAINT STORY: taliban_slavery_law_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taliban_slavery_law_2024, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: taliban_slavery_law_2024
 *   human_readable: Taliban Criminal Code Re-legalizing Slavery
 *   domain: political/legal/human_rights
 *
 * SUMMARY:
 *   Following the Taliban's return to power in Afghanistan (August 2021), the
 *   regime reinstated a criminal code that effectively re-legalizes slavery
 *   and slave-like coercive practices through provisions permitting forced
 *   labor, debt bondage, and sexual servitude. The constraint operates as
 *   pure extraction: the legal framework formalizes coercive labor relations
 *   primarily targeting women, ethnic minorities, and populations without
 *   Taliban affiliation. The state apparatus uses the code to standardize
 *   extraction across regional commanders, converting informal coercive
 *   practices into legal obligations. The theater ratio is low (0.35) because
 *   the extraction is not performative — the Taliban openly enforces the law
 *   rather than masking it behind rhetorical justification. Suppression is
 *   extreme (0.88) because exit options are nearly nonexistent: victims face
 *   armed enforcement, family-based control, geographic isolation, and legal
 *   prohibition on freedom of movement. International human rights mechanisms
 *   are ineffective (forming only a Piton perspective — institutional theater
 *   without enforcement capacity). The constraint satisfies the snare
 *   classification across all victim perspectives: high extraction, high
 *   suppression, high coercion, minimal coordination benefit.
 *
 * KEY AGENTS:
 *   - Enslaved and Debt-Bonded Populations: Primary victims (powerless/trapped) — no exit options, maximum extraction, subject to armed enforcement and legal prohibition
 *   - Women and Girls: Primary victims (moderate/constrained) — severe legal restrictions on autonomy, forced marriage, sexual slavery, economic dependency constraints
 *   - Ethnic Minorities: Primary victims (moderate/constrained) — targeted for forced labor and conscription; exit constrained by geographic isolation and ethnic identification
 *   - Taliban State Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from standardized extraction framework; coordinates coercive labor across regional commanders
 *   - Connected Military Commanders: Secondary beneficiaries (organized/arbitrage) — acquire forced labor directly through legal framework; can access debt-bonded populations
 *   - Clerical Elite: Secondary beneficiaries (institutional/arbitrage) — benefit from religiously justified legal framework; legitimize extraction through theological authority
 *   - International Human Rights Organizations: Secondary observers (organized/mobile) — limited enforcement capacity; experience mixed coordination and ineffective advocacy
 *   - International Legal Order: Institutional observer (institutional/arbitrage) — maintains condemnation but lacks enforcement; theater-based response
 *   - Analytical Observer: Structural witness (analytical/analytical) — can identify extraction without rationalization as coordination or natural constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taliban_slavery_law_2024, 0.78).
domain_priors:suppression_score(taliban_slavery_law_2024, 0.88).
domain_priors:theater_ratio(taliban_slavery_law_2024, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taliban_slavery_law_2024, extractiveness, 0.78).
narrative_ontology:constraint_metric(taliban_slavery_law_2024, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(taliban_slavery_law_2024, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taliban_slavery_law_2024, snare).
narrative_ontology:human_readable(taliban_slavery_law_2024, "Taliban Criminal Code Re-legalizing Slavery").
narrative_ontology:topic_domain(taliban_slavery_law_2024, "political/legal/human_rights").

domain_priors:requires_active_enforcement(taliban_slavery_law_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taliban_slavery_law_2024, taliban_state_apparatus).
narrative_ontology:constraint_beneficiary(taliban_slavery_law_2024, connected_military_commanders).
narrative_ontology:constraint_beneficiary(taliban_slavery_law_2024, clerical_elite).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, vulnerable_populations).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, women_and_girls).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, ethnic_minorities).
narrative_ontology:constraint_victim(taliban_slavery_law_2024, debt_bonded_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED POPULATIONS (SNARE) — Those subject to forced labor, debt bondage, and sexual servitude under Taliban rule have no meaningful exit. Physical coercion, armed enforcement, family dependency, and legal prohibition of escape create total suppression. No alternatives exist within Afghan borders. Maximum extraction with zero escape options.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN AND GIRLS (SNARE) — Subject to forced marriage (including to combatants), sexual slavery, and severe labor restrictions. Legal framework strips autonomy. Exit options are constrained by family authority, legal prohibition on female autonomy, economic dependency, and threat of violence. Experienced extraction is severe though slightly less than complete powerlessness due to some social coordination and informal mutual aid.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL HUMAN RIGHTS ORGANIZATIONS (TANGLED ROPE) — Can document violations and advocate for sanctions/intervention, but face coordination problems with state actors and resource constraints. Experience both coordination (international monitoring networks) and extraction (pressured to legitimize Taliban through diplomacy). Have exit options through geographic mobility and institutional independence, but constrained by geopolitical leverage asymmetry.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: TALIBAN STATE APPARATUS (ROPE) — Codification serves coordination function: standardizing extraction mechanisms across regional commanders, reducing internal competition for resources and labor, enabling hierarchical control. Benefits from formalized legal framework. Has maximum exit options (can reinterpret or revoke law). Experiences constraint as coordination benefit with no extraction cost — benefits from the legal standardization that makes extraction systematic rather than chaotic.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL ORDER (PITON) — The constraint represents a formal rejection of international norms (1948 Slavery Convention, UN protocols) but international enforcement is largely theatrical: sanctions are inconsistently applied, Taliban state maintains territorial control despite formal condemnation, and the international order has limited capacity to enforce abolition. Theater ratio reflects performative diplomatic protests and ineffective enforcement mechanisms. The constraint persists through institutional inertia of Taliban rule despite global legal consensus against slavery.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the Taliban slavery code is unambiguous extraction with no coordination function. The legal formalization increases measurable coercion without solving genuine collective action problems. No legitimate coordination rationale exists. This perspective confirms the snare classification as structural rather than perspectival.
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taliban_slavery_law_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taliban_slavery_law_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taliban_slavery_law_2024, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taliban_slavery_law_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taliban_slavery_law_2024, TR),
    TR >= 0.70.

:- end_tests(taliban_slavery_law_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The Taliban slavery code is explicitly extractive with minimal pretense of coordination benefits. Forced labor, debt bondage, and sexual servitude are direct extraction mechanisms. The state captures labor value without providing corresponding protection or benefit. The extractiveness has increased from 0.62 (initial declaration) to 0.78 (full implementation) as enforcement mechanisms have been institutionalized and legal interpretations have expanded the scope of coercible populations. Suppression (0.88): Very high. Exit options are nearly nonexistent: victims face armed enforcement, family-level control mechanisms, legal prohibition on autonomous movement, geographic isolation, economic dependency, and threat of violence. International borders provide some exit routes but at extreme cost (migration risks, family separation, detection and punishment). Some variation exists (women have slightly more constrained rather than fully trapped options due to informal family networks, but legal suppression remains severe). Theater ratio (0.35): Low. The Taliban does not disguise the extraction as legitimate governance — it openly enforces slave labor as religious and legal obligation. Rhetoric is theological rather than performative. The low theater reflects that the constraint does not rely on hidden mechanisms or false justification; power is applied directly.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap across victim perspectives (all snare), indicating the extraction is severe and unambiguous from all positions of vulnerability. The significant gap emerges between victims and beneficiaries: the Taliban state apparatus sees Rope (coordination mechanism), while enslaved populations see pure Snare. This gap is not about measurement uncertainty but about structural position — the beneficiary genuinely experiences the legal framework as solving coordination problems (standardizing extraction across commanders), while the victim experiences only coercive enforcement. The international observer (Piton) perceives theater where the Taliban perceives function and victims perceive domination. This three-way perspectival divergence reveals that the constraint serves no genuine coordination function — what appears as coordination to the beneficiary is institutional inertia to the international order (Piton) and pure extraction to the victim (Snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow directly from structural position and exit options. Enslaved populations (powerless/trapped) derive d ≈ 0.95 (full target), producing maximum f(d) ≈ 1.42 and maximum experienced extraction. Women and girls (moderate/constrained) derive d ≈ 0.80 (strong target), producing f(d) ≈ 1.25. International organizations (organized/mobile) derive d ≈ 0.55 (victim with some exit), producing f(d) ≈ 0.75. Taliban apparatus (institutional/arbitrage) derives d ≈ 0.05 (full beneficiary with maximum exit), producing f(d) ≈ -0.12. The directionality chain confirms that beneficiaries experience negative χ (coordination benefit) while victims experience maximum positive χ (extraction). No override is necessary — the structural derivation correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by confirming that the Taliban slavery code is unambiguous extraction without genuine coordination function. The Taliban benefits argument ('standardizing extraction across commanders') is a coordination benefit to the extractor, not a legitimate collective action solution. The code does not solve a problem that requires coercive labor to address — it creates a mechanism for extracting surplus without reciprocal obligation. The analytical observer (perspective 6) confirms this by noting that no legitimate coordination rationale exists. The snare classification stands across all perspectives because the extractiveness (0.78) and suppression (0.88) far exceed any plausible coordination benefit. The mandatrophy analysis also flags the international legal order's false mountain perspective (treating slavery prohibition as a 'natural law of international order') as actually a contested institutional norm that the Taliban explicitly rejects. The constraint thus serves as a diagnostic for distinguishing genuine mountains (irreversible limits) from naturalized institutions (contested norms treated as immutable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_vs_declaration,
    'What proportion of the Taliban criminal code is actually implemented vs. declared for legitimacy?',
    'Field documentation of actual enforcement practices vs. written law; comparison of reported incidents with legal framework predictions',
    'If implementation < 30%: constraint is partly theatrical (Piton component). If implementation > 70%: snare classification strengthens. Affects extractiveness estimate by 0.15-0.25.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_vs_declaration, empirical, 'Gap between formal law and actual enforcement of slavery provisions').

omega_variable(
    internal_taliban_consensus,
    'Is the slavery code the result of unified Taliban ideology or factional compromise that could be reversed?',
    'Analysis of Taliban leadership statements; observation of internal disagreement signals; historical precedent of Taliban policy reversals',
    'If unified: constraint is structural and stable. If factional: potential vulnerability point for pressure. Affects stability and timeline estimates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_taliban_consensus, empirical, 'Degree of internal Taliban consensus on slavery legalization').

omega_variable(
    escape_through_migration,
    'Do border countries and diaspora networks provide functional exit for enslaved populations, reducing the ''trapped'' classification?',
    'Documentation of successful escapes and refugee flows; analysis of migration costs, detection risk, and survival rates for escapees',
    'If exit routes are viable: some victims reclassify from ''trapped'' to ''constrained''. If border enforcement is effective: confirmation of total suppression. Affects directionality calculations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escape_through_migration, empirical, 'Viability of escape through cross-border migration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taliban_slavery_law_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_tr_t0, taliban_slavery_law_2024, theater_ratio, 0, 0.28).
narrative_ontology:measurement(tsl_tr_t6, taliban_slavery_law_2024, theater_ratio, 6, 0.32).
narrative_ontology:measurement(tsl_tr_t12, taliban_slavery_law_2024, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(tsl_be_t0, taliban_slavery_law_2024, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(tsl_be_t6, taliban_slavery_law_2024, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(tsl_be_t12, taliban_slavery_law_2024, base_extractiveness, 12, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taliban_slavery_law_2024, enforcement_mechanism).
narrative_ontology:affects_constraint(taliban_slavery_law_2024, afghan_labor_trafficking).
narrative_ontology:affects_constraint(taliban_slavery_law_2024, taliban_womens_legal_subjugation).
narrative_ontology:affects_constraint(taliban_slavery_law_2024, debt_bondage_south_asia).

% DUAL FORMULATION NOTE:
% The Taliban slavery code is downstream of Taliban state consolidation and ideological commitment to pre-modern Islamic law interpretation. It also upstream influences specific coercive labor practices (trafficking, forced conscription, debt bondage) that derive their legal foundation from the code. Related constraints in the family include afghan_labor_trafficking (implementation-level trafficking) and taliban_womens_legal_subjugation (broader legal framework for female subjugation of which slavery is one component).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
