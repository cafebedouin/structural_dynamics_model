% ============================================================================
% CONSTRAINT STORY: ulysses_chp05
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_lotus_1904, []).

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
 *   constraint_id: ulysses_chp05
 *   human_readable: The Narcotic Social Rope (Lotus Eaters) — Dublin 1904
 *   domain: social/religious/technological
 *
 * SUMMARY:
 *   Leopold Bloom and the citizens of Dublin in 1904 navigate a landscape
 *   saturated with narcotic social arrangements that dissolve civic intention
 *   into sensory indulgence. Flowers, perfumes, warm baths, sexual
 *   distraction, sacramental ritual, café languor, and opiate-inflected
 *   pharmaceuticals create an ambient lethargy that Ulysses explicitly
 *   compares to Homer's Lotus Eaters. This is not accidental: the constraint
 *   is structurally enforced by a coalition of pleasure merchants (bath
 *   houses, flower sellers, perfumers, pharmacists, prostitutes, priests) who
 *   profit from narcotization, and by institutional arrangements (Church,
 *   colonial administration, economic extraction) that benefit from a
 *   pacified, ambition-dampened population. The constraint exhibits the full
 *   spectrum of DR types across perspectives. To Bloom, it is a Snare: he is
 *   trapped by geography, income, and the narcotic satisfaction of immediate
 *   comfort, unable to exit toward higher ambition. To the pleasure
 *   merchants, it is Rope: they coordinate a market in sensory gratification
 *   and profit from it. To Bloom as a professional (advertisement man) and
 *   Molly as a performer, it is Tangled Rope: they benefit from the narcotic
 *   economy while being constrained by the same lethargy. To Irish
 *   nationalists, it is Scaffold: the constraint is a temporary tactical
 *   pacification mechanism with a sunset clause in independence. To the
 *   Church, it is Piton: the enforcement apparatus of sacramental theater
 *   persists through institutional inertia despite degraded doctrinal power.
 *   To the analytical observer, there is a temptation to see it as Mountain
 *   (humans naturally prefer comfort to ambition), but the structural data
 *   reveals this as a false summit: the narcotic regime is contingent,
 *   institutional, and extractive.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary citizen victim (powerless/trapped) — navigates narcotic lethargy; desires higher ambitions but lacks exit routes; experiences maximum extraction of civic agency
 *   - Pleasure Merchants Coalition: Primary beneficiaries (institutional/arbitrage) — bath house proprietors, flower sellers, perfumers, pharmacists, prostitutes, café owners; profit from narcotic provision; experience coordination, not extraction
 *   - Catholic Church Apparatus: Institutional enforcer (institutional/arbitrage) — maintains sacramental theater and sexual suppression; benefits from pacification of population; enforcement is degraded (Piton)
 *   - Molly Bloom: Complicit professional (moderate/constrained) — performer/musician; benefits from pleasure economy; constrained by same lethargy; experiences Tangled Rope
 *   - Stephen Dedalus (Shadow): Intellectual resistance (moderate/mobile) — artist seeking exit from narcotic Dublin; constrained but mobile; potential counter-narrative
 *   - Irish Independence Movement: Organized resistance (organized/constrained) — sees constraint as tactical obstacle with political sunset; building alternative consciousness pathway
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional narcotization as human nature; sees false mountain of inevitability
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
narrative_ontology:human_readable(ulysses_chp05, "The Narcotic Social Rope (Lotus Eaters) — Dublin 1904").
narrative_ontology:topic_domain(ulysses_chp05, "social/religious/technological").

domain_priors:requires_active_enforcement(ulysses_chp05).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp05, pleasure_providers).
narrative_ontology:constraint_beneficiary(ulysses_chp05, institutional_leisure_merchants).
narrative_ontology:constraint_beneficiary(ulysses_chp05, narcotic_distributors).
narrative_ontology:constraint_victim(ulysses_chp05, civic_ambition).
narrative_ontology:constraint_victim(ulysses_chp05, temporal_agency).
narrative_ontology:constraint_victim(ulysses_chp05, intellectual_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DROWSED CITIZEN (SNARE) — Bloom and other ordinary Dubliners, caught in the drift of sensory indulgence (flowers, perfumes, opiates, warm baths, sexual distraction, religious ritual theater). Trapped by social geography, economic constraint, and the narcotic satisfaction of immediate comfort. No exit path visible except geographic flight. Maximum experienced extraction: the constraint systematically converts civic intention into somatic pleasure, ambition into languor.
constraint_indexing:constraint_classification(ulysses_chp05, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PLEASURE MERCHANTS (ROPE) — Bath houses, flower sellers, perfumers, pharmacists, priests distributing sacramental stupor, prostitutes, café proprietors. These institutional actors benefit from the constraint by extracting revenue and social power from the manufacture of narcotic experiences. They experience the constraint as pure coordination: the market for lethargy is their market. Low effective extraction from their perspective because they are the beneficiaries.
constraint_indexing:constraint_classification(ulysses_chp05, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE COMPLICIT PROFESSIONAL (TANGLED ROPE) — Bloom as advertisement man, Molly as musician/performer, the doctor, the priest. These professionals are both beneficiaries (income from the narcotic economy) and constrained by it (their own ambitions blunted by the same lethargy they enable in others). They navigate between the pleasure market and the older civic order, experiencing mixed coordination and extraction.
constraint_indexing:constraint_classification(ulysses_chp05, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE IRISH INDEPENDENCE MOVEMENT (SCAFFOLD) — Nationalist intellectuals, republican organizers, and reformers see the lotus-eating constraint as a temporary obstacle to Irish consciousness and self-determination. The constraint is tactical — it keeps the population pacified and diverted from political mobilization — but it has a sunset clause implicit in the independence goal: the awakening of national consciousness would dissolve the narcotic spell. Organized agents see an exit path through collective political action.
constraint_indexing:constraint_classification(ulysses_chp05, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE COLONIAL RELIGIOUS APPARATUS (PITON) — Catholic Church institutional maintenance in Ireland. The Church enforces sexual lethargy, sacramental theater (mass, confession, extreme unction), and spiritual narcotization as 'moral' governance. But the Church's own enforcement has degraded — it maintains compliance through theater and institutional inertia rather than genuine doctrinal power. Confession and ritual become performative placeholders for actual ethical deliberation. The apparatus persists because alternatives haven't fully replaced it, not because it functions.
constraint_indexing:constraint_classification(ulysses_chp05, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, human susceptibility to narcotic pleasure and sensory distraction is an immutable feature of embodied cognition. The lotus-eating constraint might appear as a natural law: humans will always prefer immediate comfort to deferred ambition. However, the structural data reveals this as a false summit. The constraint is not psychological inevitability but a contingent institutional arrangement: the availability of specific narcotics (flowers, perfumes, opiates, sexual services, religious ritual), the economic concentration of pleasure provision, and the suppression of alternative modes of fulfillment. Different cultures, different epochs, different distributions of power produce different narcotic regimes. This is not a mountain.
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
 *   Extractiveness (0.52): Moderate-high. The constraint extracts civic intention, temporal agency, and ambition from the population. However, the extraction is not maximal because some pleasure provision is genuinely voluntary (citizens choose comfort) and some benefits genuinely accrue to merchants and professionals. The value reflects that the narcotic regime blends real coordination benefits (pleasure provision is a genuine good) with extractive capture of civic energy. Suppression (0.68): High. Multiple barriers prevent exit: economic (lack of resources for alternative activities), geographical (Dublin is isolated; emigration is costly), institutional (Church family social censure), and psychological (narcotic habituation). The combination creates a multi-layered suppression. Theater ratio (0.64): Moderate-high. Sacramental ritual (mass, confession, extreme unction) is substantially performative — it maintains institutional power without addressing actual ethical or spiritual development. But theater is not total — genuine community coordination and social bonding occur through ritual, flowers provide real sensory pleasure, and the pleasure economy is not entirely illusory.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a profound perspectival gap between the trapped citizen and the institutional beneficiary. Bloom experiences Snare: the narcotic arrangement appears to dissolve his will and convert ambition into somnolence. The pleasure merchants experience Rope: they are solving a real problem (the desire for sensory gratification) and coordinating a mutually beneficial market. The Church experiences Piton: the apparatus of enforcement has become degraded theater, maintained through inertia. Nationalists experience Scaffold: the constraint is a tactical obstacle with a political sunset. The analytical observer risks a false mountain: naturalizing institutional narcotization as inherent human nature. The perspectival gap is not merely subjective — it reflects real differences in power, exit options, and structural position. The citizen is trapped; the merchant has arbitrage options; the nationalist has a political exit path; the Church maintains surveillance and ritual theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit capacity. Bloom (powerless/trapped) has high d: he is a full target of extraction, with no exit path visible except geographic flight (emigration). His behavioral adaptation to the constraint (finding pleasure in small comforts) is not an indication of low extraction — it is the mechanism of suppression. The pleasure merchants (institutional/arbitrage) have low d: they benefit from the constraint and can exit by simply ceasing to provision narcotic experiences (though they choose not to). Molly and Bloom-as-professional (moderate/constrained) have mid-range d: they both benefit and are constrained. The Church (institutional/arbitrage) has low d as an institution (benefits from pacification) but maintains high suppression of others. The derivation chain shows that trapped exit options automatically produce high d → high f(d) → high χ for the trapped agent, while arbitrage exit produces low d → negative f(d) → low/negative χ for the beneficiary. This creates the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition. The question 'Is this Rope or Snare?' is answered by 'It is both, from different structural positions.' To the merchant, it is Rope — genuine coordination of mutual benefit in the pleasure market. To Bloom, it is Snare — extraction of civic agency without his consent or understanding. The Tangled Rope classification at the moderate level reflects the hybridization: institutions like the Church perform coordination functions (community gathering, ritual bonding) while extracting civic will and sexual agency. The Piton classification captures the degradation of the Church apparatus: it maintains enforced compliance through theater (sacrament) rather than through genuine doctrinal power or spiritual coherence. The mandate is resolved not by forcing all perspectives into a single type but by acknowledging that the constraint operates as different types for different agents — Snare for the powerless, Rope for the beneficiaries, Tangled Rope for the complicit professionals, Scaffold for the politically conscious, Piton for the institutional apparatus, and false Mountain for those who naturalize it as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narcotic_vs_cultural_rhythm,
    'Is the ''lethargy'' of Dublin 1904 a narcotic/institutional extraction mechanism, or is it a cultural adaptation to colonial economic exhaustion—a legitimate response to structural powerlessness?',
    'Historical analysis of civic participation rates, labor conditions, emigration patterns, and institutional constraints in pre-independence Ireland; comparison with post-independence activity and energy levels; ethnographic reconstruction of subjective experience',
    'If narcotic mechanism: the constraint is a Snare extracting civic agency. If cultural adaptation: the constraint is more Rope than Snare — the lethargy may be a rational coordination failure rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narcotic_vs_cultural_rhythm, empirical, 'Whether lethargy is narcotic extraction or adaptive response to powerlessness').

omega_variable(
    merchant_agency_vs_orchestration,
    'Are the pleasure merchants (bath houses, flower sellers, prostitutes, priests) autonomous economic actors operating a market, or are they instrumentalized by a larger institutional apparatus (colonial capital, Church hierarchy) that coordinates narcotization as social control?',
    'Analysis of merchant economic independence, pricing coordination, institutional oversight, regulatory capture; examination of whether narcotic provision serves merchant profit or serves some external actor''s pacification agenda',
    'If autonomous merchants: the constraint is Rope coordinating mutual benefit. If orchestrated apparatus: the constraint is Snare or Tangled Rope with merchants as enforcers rather than beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(merchant_agency_vs_orchestration, empirical, 'Whether pleasure merchants are independent actors or coordinated apparatus').

omega_variable(
    suppression_mechanism_identity,
    'What is the primary suppression mechanism preventing exit? Is it: (a) economic (lack of resources for alternative activities), (b) psychological (narcotic habituation and expectation management), (c) geographical (Dublin isolation and limited exit routes), (d) institutional (Church/family/social censure), or (e) epistemic (lack of awareness of alternatives)?',
    'Analysis of documented attempts to exit the lethargy (emigration, bohemia, underground intellectual movements); interviews or journals showing subjective barriers; comparison of exit rates across different suppression conditions',
    'Different suppression mechanisms suggest different constraint types and different intervention points. Psychological suppression stabilizes Snare. Institutional suppression enables Tangled Rope reformation. Economic suppression alone is closer to Rope (lacks coercion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity, empirical, 'Identity of primary suppression mechanism (economic, psychological, geographical, institutional, epistemic)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp05, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp05, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp05, theater_ratio, 5, 0.56).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp05, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp05, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp05, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp05, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp05, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp05, colonial_economic_extraction).
narrative_ontology:affects_constraint(ulysses_chp05, catholic_institutional_control).
narrative_ontology:affects_constraint(ulysses_chp05, sexual_suppression_regime).

% DUAL FORMULATION NOTE:
% The lotus-eating constraint is downstream of colonial economic extraction (which reduces available resources and exit options), Catholic institutional control (which enforces sacramental theater and sexual suppression), and the broader sexual suppression regime (which channels erotic energy into acceptable commodified forms). These are separate constraints with their own ε values; the lotus constraint represents the narcotic coordination mechanism that these upstream constraints enable. All three are linked in a constraint family modeling Dublin 1904 institutional capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
