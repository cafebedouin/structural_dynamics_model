% ============================================================================
% CONSTRAINT STORY: monarch_institutional_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monarch_institutional_legitimacy, []).

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
 *   constraint_id: monarch_institutional_legitimacy
 *   human_readable: Monarch Institutional Legitimacy
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Monarchical institutional legitimacy represents a structural constraint
 *   on political participation and resource allocation that combines genuine
 *   coordination functions (focal point for institutional stability, neutral
 *   ceremonial arbiter) with substantial extraction mechanisms (exclusion
 *   from decision-making based on birth, resource concentration in hereditary
 *   elite, suppression of alternative legitimacy claims). The constraint
 *   exhibits the full six-type spectrum depending on observer position:
 *   excluded citizens experience a snare; reformers experience tangled rope
 *   with constrained exit; bureaucratic institutions experience coordination
 *   benefits; regional power centers experience arbitrage mobility;
 *   ceremonial apparatus has degraded into piton; the monarchy itself
 *   navigates tangled rope contradictions; analytical observers risk
 *   naturalizing it as immutable law. The theater ratio has drifted upward
 *   from 0.35 to 0.68 over the measurement interval, indicating increasing
 *   proportion of performative legitimacy work relative to functional
 *   governance. Extractiveness has drifted upward from 0.42 to 0.52, showing
 *   modest accumulation of exclusion mechanisms even as formal powers have
 *   diminished in many constitutional monarchies.
 *
 * KEY AGENTS:
 *   - Excluded Citizens: Primary victims (powerless/trapped) — born without hereditary claim; no structural exit from monarchical authority
 *   - Aspiring Reformers: Secondary victims (moderate/constrained) — face legal barriers and social costs to advocating democratic alternatives; constrained exit through possible emigration
 *   - Governmental Bureaucracy: Beneficiary institution (institutional/arbitrage) — benefits from monarchy as legitimacy focal point and conflict resolution structure
 *   - Regional Power Centers: Beneficiary actors (powerful/mobile) — can arbitrage between monarchical hierarchy and democratic or competitive structures; mobility provides exit option
 *   - Democratic Participation Capacity: Abstract victim (powerless/trapped) — institutional mechanism for collective voice is structurally subordinated to hereditary authority
 *   - Resource Allocation Efficiency: Abstract victim (powerless/trapped) — hereditary principle distributes resources by birth rather than by capability or need
 *   - Ceremonial Apparatus: Institutional mechanism (institutional/arbitrage) — maintains theatrical legitimacy through pageantry as core function degrades
 *   - Monarchy as Institution: Structural actor (institutional/constrained) — must justify extraction mechanism (hereditary succession) through coordination narrative (national unity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monarch_institutional_legitimacy, 0.52).
domain_priors:suppression_score(monarch_institutional_legitimacy, 0.58).
domain_priors:theater_ratio(monarch_institutional_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monarch_institutional_legitimacy, extractiveness, 0.52).
narrative_ontology:constraint_metric(monarch_institutional_legitimacy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(monarch_institutional_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monarch_institutional_legitimacy, tangled_rope).
narrative_ontology:human_readable(monarch_institutional_legitimacy, "Monarch Institutional Legitimacy").
narrative_ontology:topic_domain(monarch_institutional_legitimacy, "political/institutional").

domain_priors:requires_active_enforcement(monarch_institutional_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monarch_institutional_legitimacy, monarch_institution).
narrative_ontology:constraint_beneficiary(monarch_institutional_legitimacy, hereditary_elite).
narrative_ontology:constraint_victim(monarch_institutional_legitimacy, democratic_participation).
narrative_ontology:constraint_victim(monarch_institutional_legitimacy, resource_allocation_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CITIZEN (SNARE) — Citizens born without hereditary claim face insurmountable barriers to institutional voice within the monarchical structure. No exit option; must bear the constraint of monarchical authority as immutable fact of political life. Maximum extraction relative to structural position — power is claimed as birthright rather than earned or delegated.
constraint_indexing:constraint_classification(monarch_institutional_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING REFORMER (TANGLED ROPE) — Constrained by legal prohibition against constitutional change and social costs of republican advocacy, yet also participates in the stability the monarchy provides. The reformer benefits from national continuity and international legitimacy while bearing costs of excluded voice. Exit through emigration is possible but costly.
constraint_indexing:constraint_classification(monarch_institutional_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENTAL BUREAUCRACY (ROPE) — State apparatus coordinates through the monarch as legitimating center. Bureaucrats experience the monarchy as a coordination mechanism: it provides focal point for institutional continuity, neutral arbiter role, and ceremonial legitimacy without requiring their direct participation in governance. Low experienced extraction; they benefit from having a pre-existing legitimacy structure.
constraint_indexing:constraint_classification(monarch_institutional_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL POWER CENTER (ROPE) — Regional actors (local lords, business magnates, provincial leaders) can negotiate with the monarch or with democratic structures. Mobile exit options allow arbitrage between systems. The monarchy provides coordination benefit: a hierarchy with predictable succession and conflict resolution reduces uncertainty relative to purely competitive alternatives.
constraint_indexing:constraint_classification(monarch_institutional_legitimacy, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CEREMONIAL APPARATUS (PITON) — Coronations, pageantry, hereditary honors, and constitutional formalism constitute an increasingly theatrical legitimation ritual. The actual governance function has migrated to elected bodies or bureaucratic institutions in many contexts, yet the monarchical ceremonial persists through institutional inertia. Theater ratio (0.68) reflects that performance of monarchical authority often exceeds its functional decision-making role.
constraint_indexing:constraint_classification(monarch_institutional_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MONARCHY INSTITUTION (TANGLED ROPE) — The institution benefits from coordination loyalty (subjects unite around the symbol) while bearing suppression costs of maintaining the illusion of naturalness and inevitability. Constrained by the need to justify its existence through increasingly complex legitimacy narratives. Must enforce the extraction mechanism (hereditary succession) while claiming it serves coordination (national unity). Cannot exit into a purely democratic system without dissolution.
constraint_indexing:constraint_classification(monarch_institutional_legitimacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational view, some form of institutional hierarchy may appear inherent to human social organization — the monarch as natural apex of authority, succession as inevitable, hereditary principle as immutable law. This perspective risks naturalizing what is actually a contingent political choice. The engine's false summit detector will identify this as misclassification if the structural data shows enforceability requirements and beneficiary declarations.
constraint_indexing:constraint_classification(monarch_institutional_legitimacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monarch_institutional_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monarch_institutional_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monarch_institutional_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monarch_institutional_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monarch_institutional_legitimacy, TR),
    TR >= 0.70.

:- end_tests(monarch_institutional_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The monarch/hereditary elite extract through exclusive decision-making authority, resource concentration, and exclusion of non-hereditary voices from institutional power. However, the extraction is not maximal — many constitutional monarchies have delegated governance to elected bodies while retaining ceremonial role. The 0.52 reflects real but limited extraction in contemporary contexts. Historical value (0.42) shows accumulation over time as democratic norms globally have increased the perceived unfairness of hereditary authority. Suppression (0.58): Moderate-high. Multiple suppression mechanisms exist: legal prohibition of certain succession paths (female exclusion, bastard lines — historically), social costs to republicanism, internalized cultural frames treating monarchy as natural/inevitable, educational systems naturalizing hereditary hierarchy, information control through court privilege. Suppression is not total — critique exists — but substantial barriers prevent exit from the constraint. Theater ratio (0.68): High and increasing. Coronations, state ceremonies, constitutional formalism, hereditary honors, and pageantry constitute increasingly performative legitimacy work. Actual governance has migrated to elected bodies or bureaucratic institutions in many monarchies, yet ceremonial functions persist and expand. The upward drift reflects Goodhart decay: as functional governance role diminishes, performative legitimacy work must expand to compensate.
 *
 * PERSPECTIVAL GAP:
 *   The monarch's institutional legitimacy generates maximum perspectival divergence because the constraint simultaneously provides coordination benefits (focal point for state continuity, ceremonial arbiter role) and enforces extraction (hereditary exclusion from power). Excluded citizens perceive a snare — they cannot exit and bear the cost of excluded voice. Reformers perceive tangled rope — they benefit from stability while bearing suppression costs. Bureaucrats perceive rope — they benefit from the coordination focal point. Regional power centers perceive rope — they have mobility and can arbitrage systems. The ceremonial apparatus perceives itself as piton — theatrical work persisting through institutional inertia. The monarchy perceives itself as tangled rope — it must enforce hereditary extraction while claiming coordination benefits. The analytical observer risks mountain classification — seeing hereditary authority as inherent to human social organization — which the false summit detector should flag as naturalization of contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values range from beneficiary institutions experiencing low extraction (d ≈ 0.15-0.25 for institutional beneficiaries with arbitrage mobility) to excluded citizens experiencing maximum extraction (d ≈ 0.95 for powerless/trapped). The monarchy institution itself occupies middle ground (d ≈ 0.55) — it is both beneficiary (extracts authority and resources) and constrained actor (must justify extraction through coordination narrative). Regional power centers with mobile exit options derive lower effective extraction despite bearing some suppression costs (d ≈ 0.35-0.45). Reformers constrained by legal prohibition but with emigration as exit derive moderate directionality (d ≈ 0.65). The critical distinction: beneficiaries with arbitrage mobility experience negative or near-zero effective extraction; constrained victims with no mobility experience high extraction. The sigmoid f(d) amplifies this differentiation — d=0.15 yields f(d)≈-0.01 (near institutional subsidy), while d=0.95 yields f(d)≈1.42 (near maximal powerless experience).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves by showing that the same structural setup (hereditary authority with centralized decision-making) provides genuine coordination benefits AND enforces asymmetric extraction simultaneously. This is not paradox but the definition of tangled_rope. The coordination function is real — without a focal institutional center, competing power centers require greater transaction costs for conflict resolution. The extraction is also real — hereditary principle excludes non-hereditary agents from voice and concentrates resources. Both are simultaneously true. The mandatrophy is resolved by recognizing that coordination and extraction are not mutually exclusive categories but structural dimensions that can co-exist. The constraint's classification as tangled_rope from institutional and moderate perspectives, snare from powerless perspective, and rope from beneficiary perspective is not inconsistency but the correct perspectival reading of a hybrid mechanism. The false summit at the analytical/civilizational level (mountain classification) should be flagged as naturalization — the constraint's 'appearance' of inevitability is performatively maintained through the very theater ratio (0.68) that indicates functional degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_consent_circularity,
    'Does the monarch''s legitimacy derive from genuine consent or from successful suppression of exit alternatives?',
    'Historical analysis of constitutional moments: did subjects choose monarchy in deliberative settings or was it imposed/inherited? Comparison of societies with and without republican alternatives available.',
    'If derived from genuine consent: reclassify as Rope from powerless perspective. If derived from suppression of alternatives: Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_consent_circularity, empirical, 'Whether monarchical legitimacy rests on consent or suppression of alternatives').

omega_variable(
    functional_vs_ceremonial_boundary,
    'What proportion of monarchical authority is functional governance versus ceremonial performance in contemporary contexts?',
    'Institutional analysis of decision-making loci: which decisions does the monarch actually make versus which does the sovereign only formally approve? Comparison across democracies with and without monarchs.',
    'If mostly ceremonial (theater > 0.75): piton classification strengthens. If substantial governance remains (theater < 0.40): tangled_rope or snare classification applies more directly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_ceremonial_boundary, empirical, 'Functional governance role versus ceremonial function split').

omega_variable(
    alternative_legitimacy_sufficiency,
    'Would democratic or meritocratic legitimacy structures provide equivalent coordination benefits to hereditary monarchy?',
    'Comparative institutional analysis of legitimacy stability in democracies vs monarchies; measurement of institutional resilience under constitutional stress.',
    'If alternatives provide equivalent coordination: monarchy is pure extraction (Snare). If monarchy provides unique coordination: tangled_rope or rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_sufficiency, conceptual, 'Whether democratic alternatives provide equivalent coordination function').

omega_variable(
    hereditary_mechanism_naturalness,
    'Is the appearance of hereditary succession as ''natural law'' the result of explicit suppression mechanisms or internalized cultural frames?',
    'Genealogical and historical analysis of succession moments: were alternatives actively suppressed (laws against female succession, bastard exclusion, contested claims) or passively accepted through cultural naturalization?',
    'If actively suppressed: high explicit suppression score confirmed. If internalized: suppression is cognitive rather than structural (omega for identity_locked dynamics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hereditary_mechanism_naturalness, empirical, 'Active suppression versus cultural naturalization of hereditary succession').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monarch_institutional_legitimacy, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mona_tr_t0, monarch_institutional_legitimacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mona_tr_t2, monarch_institutional_legitimacy, theater_ratio, 2, 0.48).
narrative_ontology:measurement(mona_tr_t4, monarch_institutional_legitimacy, theater_ratio, 4, 0.58).
narrative_ontology:measurement(mona_tr_t6, monarch_institutional_legitimacy, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(mona_be_t0, monarch_institutional_legitimacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mona_be_t2, monarch_institutional_legitimacy, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(mona_be_t4, monarch_institutional_legitimacy, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(mona_be_t6, monarch_institutional_legitimacy, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monarch_institutional_legitimacy, enforcement_mechanism).
narrative_ontology:affects_constraint(monarch_institutional_legitimacy, hereditary_elite_resource_concentration).
narrative_ontology:affects_constraint(monarch_institutional_legitimacy, democratic_participation_bottleneck).
narrative_ontology:affects_constraint(monarch_institutional_legitimacy, succession_legitimacy_crisis).

% DUAL FORMULATION NOTE:
% Monarchical institutional legitimacy is upstream of specific extraction mechanisms (resource concentration, succession disputes, democratic exclusion). This story captures the general constraint structure; domain-specific stories capture how the constraint instantiates in different contexts (succession crisis, elite concentration, participation bottleneck).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monarch_institutional_legitimacy, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
