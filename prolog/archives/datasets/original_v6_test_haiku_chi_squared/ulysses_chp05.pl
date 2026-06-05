% ============================================================================
% CONSTRAINT STORY: ulysses_chp05
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp05, []).

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
 *   constraint_id: ulysses_chp05
 *   human_readable: The Narcotic Social Rope (Lotus Eaters) — 1904 Dublin
 *   domain: social/religious/technological
 *
 * SUMMARY:
 *   Leopold Bloom's June 16, 1904 Dublin traversal enacts a structural
 *   constraint that operates through sensory and institutional narcotics
 *   rather than explicit coercion. The 'Lotus Eaters' episode of Ulysses
 *   documents the constraint: pharmacy lounges offering warm baths and
 *   perfumed air, church rituals with incense and liturgical sedation,
 *   merchant leisure opportunities, and the ambient lethargy of Victorian
 *   Dublin create a social rope of narcotic compliance. This constraint is
 *   neither pure coordination nor pure extraction but a tangled hybrid where
 *   the coordination mechanism (community participation, sensory comfort,
 *   religious belonging) becomes an extraction mechanism precisely through
 *   its enforcement — the suppression of cognitive resistance and the
 *   prevention of alternative consciousness. The constraint's theater_ratio
 *   (0.64) reflects that much of the apparatus maintaining narcotic
 *   compliance is performative: church ritual is as much theater as function,
 *   merchant leisure is as much social performance as genuine relaxation, and
 *   even Leopold's participation is as much aesthetic engagement as actual
 *   numbness. The constraint exhibits the six DR types depending on
 *   perspective: snare for the drifter who cannot escape, rope for the
 *   merchant who benefits, tangled_rope for the family caught between
 *   coordination and extraction, scaffold for the modernist consciousness
 *   that sees a historical sunset, piton for the church maintaining degraded
 *   ritual, and false summit for the naturalizing view that sees this as an
 *   immutable feature of human cognition.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary victim (powerless/trapped) — drifter navigating narcotic compliance without institutional anchor; experiences constraint as lethargy and cognitive numbing
 *   - Merchant Class (Perfumers, Bathhouse Operators, Pharmacists): Primary beneficiaries (institutional/arbitrage) — extract steady low-friction income from leisure consumption loops
 *   - Working-Class Families: Secondary victims (organized/constrained) — families experience coordination benefits (religious community, affordable leisure) but systematic extraction of wages and family continuity
 *   - Catholic Church Authority: Institutional beneficiary (institutional/arbitrage) — maintains compliance through ritual narcosis; benefits from reduced epistemic resistance; theater_ratio 0.64 indicates degraded institutional function
 *   - Literary Modernists (Yeats, Joyce, Synge): Organized agents (organized/mobile) — represent alternative consciousness pathways with partial exit capacity; enable Scaffold perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the engineered narcotic system as immutable human cognitive limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp05, 0.52).
domain_priors:suppression_score(ulysses_chp05, 0.68).
domain_priors:theater_ratio(ulysses_chp05, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp05, extractiveness, 0.52).
narrative_ontology:constraint_metric(ulysses_chp05, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ulysses_chp05, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp05, tangled_rope).
narrative_ontology:human_readable(ulysses_chp05, "The Narcotic Social Rope (Lotus Eaters) — 1904 Dublin").
narrative_ontology:topic_domain(ulysses_chp05, "social/religious/technological").

domain_priors:requires_active_enforcement(ulysses_chp05).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp05, merchant_class_leisure).
narrative_ontology:constraint_beneficiary(ulysses_chp05, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(ulysses_chp05, pharmaceutical_distributors).
narrative_ontology:constraint_victim(ulysses_chp05, working_class_agency).
narrative_ontology:constraint_victim(ulysses_chp05, epistemic_resistance).
narrative_ontology:constraint_victim(ulysses_chp05, family_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Leopold Bloom experiences the lotus constraint as a snare: sensory and cognitive numbness induced by pharmacy lounges, perfumed baths, and church incense. No escape route available without social death. d≈0.92, f(d)≈1.39, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(ulysses_chp05, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perfumers, bathhouse operators, pharmacists, and church authorities benefit from the narcotic rope as coordination mechanism. Customers are drawn into consumption loops that appear voluntary. d≈0.08, f(d)≈-0.11, σ=0.9 → χ≈-0.05. Net beneficiary through low-friction extraction.
constraint_indexing:constraint_classification(ulysses_chp05, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Families benefit from the narcotic coordination (reduced social friction, religious participation, consumption opportunities) but are systematically victimized by income extraction and degradation of family labor/continuity. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.48.
constraint_indexing:constraint_classification(ulysses_chp05, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Intellectual agents (Leopold's inner monologue, artistic consciousness) see the lotus constraint as a temporary historical condition — the narcotics of Victorian leisure are giving way to literary modernism and psychological realism that expose the mechanism. d≈0.48, f(d)≈0.60, σ=0.9 → χ≈0.29. Moderate extraction because exit is possible through cultural/intellectual mobility.
constraint_indexing:constraint_classification(ulysses_chp05, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% Catholic institutional authority maintains narcotic compliance through ritual (mass incense, holy water, prayer as sedative practice). Theater ratio 0.64: much of the institutional apparatus is performative maintenance of compliance rather than genuine spiritual function. d≈0.12, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Degraded: the church sees its own enforcement as theatrical.
constraint_indexing:constraint_classification(ulysses_chp05, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% From civilizational perspective, the lotus constraint might appear as an immutable feature of human cognition — the brain's susceptibility to dopaminergic reward loops and sensory habituation. However, structural data (ε=0.52, suppression=0.68, theater=0.64) contradicts mountain classification. Engine detects false summit: the constraint is not a natural law but an engineered social narcotics system.
constraint_indexing:constraint_classification(ulysses_chp05, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp05_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp05, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp05, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp05, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp05, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp05_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through two channels: (1) direct merchant extraction (leisure consumption spending that exceeds working-class budgets), and (2) institutional extraction (church control over meaning-making and consciousness). The extraction is sustained because it appears as voluntary coordination — the leisure is genuinely pleasurable, the rituals are genuinely comforting — making the suppression of alternatives less visible. Suppression (0.68): High. The constraint suppresses through: absence of alternative consciousness pathways (limited literacy, educational access), geographic concentration of narcotic mechanisms (pharmacies, churches), and institutional authority (church/merchant networks). But suppression is not total — Leopold's stream-of-consciousness and the existence of modernist literature represent cracks in the suppression. Theater ratio (0.64): Moderate-high. Church ritual theater is explicit (performance of piety, ornate incense for aesthetic effect). Merchant leisure has theater (bathhouse as social performance space). Even Leopold's participation has theatrical dimensions — he is performing compliance while internally narrating resistance. The theater increases over the interval (0.42→0.64) as the modernist exposure of the mechanism makes the performance more conscious.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival divergence. Leopold (powerless/trapped) sees a snare: sensory numbness with no exit. Merchants (institutional/arbitrage) see a rope: smooth coordination mechanism generating pleasant commerce. Church (institutional/arbitrage with piton classification) sees its own degraded ritual: theater maintaining institutional authority. Modernist consciousness (organized/mobile) sees a scaffold: historical condition giving way to psychological realism. Working families (organized/constrained) experience the tangled hybrid directly: coordination benefits (community, ritual participation) paired with extraction costs (wage reduction, family labor displacement). The analytical civilizational observer risks the false summit: naturalizing the engineered narcotic system as immutable human susceptibility to dopaminergic reward and sensory habituation. The perspectival gap is not ambiguity but structural: the same constraint appears categorically different depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Leopold Bloom: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction — no exit route without social death. Merchant class: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary — low-friction extraction. Church institutional: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.08. Institution benefits from compliance enforcement; piton classification from theater gate, not from high chi. Working families: Victim + constrained → d≈0.68, f(d)≈1.02. Mixed experience — benefits from coordination (community, ritual) but systematic extraction via wages and family continuity. Modernist consciousness: Moderate + mobile → d≈0.48, f(d)≈0.60. Moderate extraction because exit is available through cultural mobility. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival naturalization; engine detects false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy in the lotus constraint is resolved by recognizing that the six-type distribution is not ambiguity but structural necessity. The constraint genuinely exhibits snare dynamics (for the powerless), rope dynamics (for beneficiaries), tangled_rope dynamics (for mixed agents), scaffold dynamics (for agents with exit pathways), piton dynamics (for degraded institutions), and appears as mountain (to the naturalizing observer). The mandatrophy resolution is NOT 'which type is correct?' but 'the constraint is a multi-perspectival structure that exhibits all six types as legitimate readings.' The analytical observer's temptation to call it a Mountain (natural law of human narcotizability) is the false summit detection: the constraint is NOT immutable but engineered. The high extractiveness (0.52) combined with the high theater ratio (0.64) and the explicit beneficiary/victim structure confirms Tangled Rope as the canonical classification (coordinating merchant leisure with systematic family wage extraction). The Snare perspective (Leopold/powerless) is the primary structural reality — the merchant rope becomes a snare from the perspective of those trapped in it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coerced_consumption,
    'Are the lotus consumption patterns (pharmacy lounges, perfumed baths, church incense) genuinely voluntary social preferences or systematically coerced through suppression of alternatives?',
    'Historical analysis of neighborhood pharmacy density, church attendance incentive structures (employment, social status), and availability of non-narcotic leisure alternatives in 1904 Dublin',
    'If voluntary: constraint downgrades to Rope (pure coordination). If coerced: classification as Tangled Rope or Snare is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_consumption, empirical, 'Degree of voluntary vs coerced participation in narcotic consumption').

omega_variable(
    pharmaceutical_agency_vs_innocence,
    'Do pharmaceutical distributors and merchants actively engineer the narcotic dependency, or do they passively benefit from naturally occurring consumption patterns?',
    'Archival evidence of advertising strategies, intentional product development for dependency, pricing structures targeting low-income populations, and merchant coordination networks',
    'If actively engineered: beneficiary classification is confirmed as intentional extraction. If passive: constraint may degrade to Rope (unintentional coordination). Mandatrophy resolution depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_agency_vs_innocence, empirical, 'Degree of merchant agency in engineering narcotic dependency').

omega_variable(
    escape_via_modernist_consciousness,
    'Does the emergence of literary modernism (Ulysses itself, Yeats, Joyce''s internal monologue) represent a genuine escape mechanism from the lotus constraint, or is it itself a performative pseudo-escape that maintains suppression while appearing to resist?',
    'Comparison of biographical outcomes for intellectuals who ''escape'' into modernist consciousness vs those without such escape; analysis of whether modernist literature actually changes material conditions or merely represents them',
    'If genuine escape: Scaffold and Piton perspectives are validated. If pseudo-escape: modernism is itself part of the theater ratio — the constraint is more severe than ε=0.52 suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escape_via_modernist_consciousness, conceptual, 'Whether modernist consciousness enables genuine exit from narcotic suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp05, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lotus_tr_t0, ulysses_chp05, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lotus_tr_t5, ulysses_chp05, theater_ratio, 5, 0.53).
narrative_ontology:measurement(lotus_tr_t10, ulysses_chp05, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(lotus_be_t0, ulysses_chp05, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(lotus_be_t5, ulysses_chp05, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(lotus_be_t10, ulysses_chp05, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp05, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp05, dublin_labor_extraction).
narrative_ontology:affects_constraint(ulysses_chp05, religious_institutional_compliance).
narrative_ontology:affects_constraint(ulysses_chp05, pharmaceutical_dependency_distribution).

% DUAL FORMULATION NOTE:
% The lotus constraint is upstream of specific labor and religious extraction mechanisms in 1904 Dublin. The narcotic social rope represents the coordination mechanism that enables lower-friction extraction in subsidiary constraints. The modernist consciousness represents an alternative formulation: the constraint may decompose into separate stories for (1) institutional narcosis (church ritual, ε≈0.35, Rope), (2) merchant extraction (pharmacy leisure, ε≈0.48, Snare), and (3) literary resistance (modernism, ε≈0.28, Scaffold). Current story models the hybrid at ε=0.52.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp05, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
