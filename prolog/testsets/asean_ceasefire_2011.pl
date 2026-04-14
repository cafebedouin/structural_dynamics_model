% ============================================================================
% CONSTRAINT STORY: asean_ceasefire_2011
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asean_ceasefire_2011, []).

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
 *   constraint_id: asean_ceasefire_2011
 *   human_readable: 2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The 2011 ASEAN-mediated ceasefire agreement between Thailand and Cambodia
 *   represents a constraint operating simultaneously as coordination
 *   mechanism, extractive arrangement, and theatrical institutional ritual.
 *   Following deadly border clashes near the Preah Vihear temple (part of a
 *   larger territorial dispute over Khmer temple ruins in a region claimed by
 *   both states), ASEAN brokered a ceasefire in February 2011 under the ASEAN
 *   Way doctrine of non-interference plus mediation. The agreement created a
 *   structural tension: the ceasefire genuinely solved the bilateral
 *   coordination problem (preventing escalation to full-scale war), but it
 *   also entrenched asymmetric military advantages, suppressed civilian
 *   agency in border regions, and substituted theatrical mediation for actual
 *   dispute resolution. The constraint exhibits Tangled Rope properties at
 *   the institutional level (coordination with active enforcement) but Snare
 *   properties for border civilians (trapped extraction) and Piton properties
 *   at the regional ritual level (theater substituting for resolution).
 *
 * KEY AGENTS:
 *   - ASEAN Institutional Framework: Primary beneficiary (institutional/arbitrage) — demonstrates regional mediation capacity; vindicates ASEAN Way doctrine
 *   - Thai Military Command: Organized beneficiary with mixed extraction (organized/constrained) — gains strategic coordination and territorial claim consolidation; constrained by ASEAN pressure
 *   - Cambodian Military Command: Organized victim (organized/constrained) — achieves escalation prevention but accepts Thai military asymmetry; constrained by ASEAN framework
 *   - Border Civilian Population: Primary victim (powerless/trapped) — bears suppression costs and restriction of movement; no exit option from imposed ceasefire
 *   - Independent Verification Capacity: Institutional victim (institutional/arbitrage) — undermined by ASEAN's non-interference principle; ICJ rulings create verification lag
 *   - International Mediation Partners (UN, ICJ, bilateral actors): Powerful observers (powerful/mobile) — view ceasefire as temporary scaffold pending ICJ adjudication
 *   - Dispute Resolution Finality: Structural victim (analytical/analytical) — ceasefire perpetuates conflict by substituting mediation theater for binding settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asean_ceasefire_2011, 0.52).
domain_priors:suppression_score(asean_ceasefire_2011, 0.68).
domain_priors:theater_ratio(asean_ceasefire_2011, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asean_ceasefire_2011, extractiveness, 0.52).
narrative_ontology:constraint_metric(asean_ceasefire_2011, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(asean_ceasefire_2011, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asean_ceasefire_2011, tangled_rope).
narrative_ontology:human_readable(asean_ceasefire_2011, "2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement").
narrative_ontology:topic_domain(asean_ceasefire_2011, "geopolitical").

domain_priors:requires_active_enforcement(asean_ceasefire_2011).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, asean_institutional_credibility).
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, thai_government_stability).
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, cambodian_government_legitimacy).
narrative_ontology:constraint_victim(asean_ceasefire_2011, civilian_border_populations).
narrative_ontology:constraint_victim(asean_ceasefire_2011, independent_verification_capacity).
narrative_ontology:constraint_victim(asean_ceasefire_2011, dispute_resolution_finality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BORDER CIVILIAN POPULATION (SNARE) — Trapped between Thai and Cambodian military forces with no exit option and no agency in ceasefire negotiation. Ceasefires are imposed upon them; compliance is coerced through fear of military action. They bear the full suppression cost (shelter restrictions, movement barriers, economic disruption) while extraction is captured by military commands securing territorial claims.
constraint_indexing:constraint_classification(asean_ceasefire_2011, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THAI MILITARY COMMAND (TANGLED ROPE) — Benefits from ceasefire as coordination mechanism (prevents escalation, enables strategic repositioning, secures Preah Vihear territorial claims against Cambodia). Also bears extraction costs through ASEAN mediation constraints and international pressure to maintain restraint. Active enforcement required to prevent unilateral escalation; constrained exit (cannot unilaterally withdraw without regional isolation). Mixed coordination and extraction.
constraint_indexing:constraint_classification(asean_ceasefire_2011, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAMBODIAN MILITARY COMMAND (TANGLED ROPE) — Mirrors Thai position. Benefits from ceasefire coordination (prevents catastrophic escalation against larger Thai military). Also constrained by ASEAN mediation and international oversight. Extraction arises from Thai military advantage in conventional capability — ceasefire asymmetrically favors Thai strategic position while constraining Cambodian escalation options. Active enforcement required.
constraint_indexing:constraint_classification(asean_ceasefire_2011, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ASEAN INSTITUTIONAL FRAMEWORK (ROPE) — Primary beneficiary. Ceasefire agreement demonstrates ASEAN's conflict resolution capacity and legitimacy in regional disputes (ASEAN Way coordination mechanism). Arbitrage exit available: ASEAN can claim success and move to other regional issues; the agreement vindicates ASEAN's non-interference + mediation doctrine. No significant extraction from ASEAN's perspective — the coordination function is primary.
constraint_indexing:constraint_classification(asean_ceasefire_2011, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL MEDIATION PARTNERS (SCAFFOLD) — UN, ICJ, and bilateral partners view the ceasefire as temporary scaffolding pending resolution of the underlying Preah Vihear temple dispute through ICJ adjudication. Theater ratio reflects performative elements of ASEAN-brokered agreements that historically lack enforcement mechanisms. Mobile exit available: international actors can escalate through ICJ or disengage if ceasefire breaks down. Sunset clause implicit: ICJ ruling on maritime boundary disputes (2013 phase 1, ongoing phases) will eventually replace the ceasefire.
constraint_indexing:constraint_classification(asean_ceasefire_2011, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL DIPLOMATIC RITUAL (PITON) — The ceasefire perpetuates the performative structure of Thai-Cambodian diplomacy: periodic ceasefires, ASEAN mediation, token enforcement mechanisms, and persistent underlying territorial disputes. Theater ratio 0.65 reflects that the ceasefire is largely theatrical — armed clashes resume repeatedly (2011, 2013, 2014 resurgences), the underlying Preah Vihear claim remains unresolved, and ASEAN enforcement capacity is minimal. The ritual persists through institutional inertia (ASEAN's mediation role generates political utility even when agreements fail) rather than functional dispute resolution.
constraint_indexing:constraint_classification(asean_ceasefire_2011, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / HISTORICAL INEVITABILITY VIEW (MOUNTAIN) — From a civilizational perspective, the Preah Vihear territorial dispute is an irreducible legacy of 19th-century French colonial boundary demarcation errors and Thai-Cambodian national interests, making occasional violence and periodic ceasefires inevitable features of the regional system. This perspective risks naturalizing what is actually a contingent institutional failure (inability to enforce ICJ rulings and implement binding dispute resolution).
constraint_indexing:constraint_classification(asean_ceasefire_2011, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asean_ceasefire_2011_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asean_ceasefire_2011, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asean_ceasefire_2011, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asean_ceasefire_2011, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asean_ceasefire_2011, TR),
    TR >= 0.70.

:- end_tests(asean_ceasefire_2011_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The ceasefire extracts from border civilians (confined movement, restricted economic activity) and from Cambodia (forced acceptance of Thai military asymmetry and territorial claims). Extraction rises from 0.38 to 0.52 over the interval as ASEAN's inability to prevent resurgences (2013, 2014 clashes) becomes apparent, revealing enforcement theater. Suppression (0.68): High. Ceasefire implementation relies on military pressure—civilians cannot freely cross borders, media access to conflict zones is restricted, independent verification of compliance is minimal (ASEAN observers lack capacity). Both sides maintain heightened military presence. Theater ratio (0.65): Moderate-high. ASEAN brokering is substantially performative—the institutional framework of 'mediation' generates political utility (ASEAN demonstrates relevance) independent of whether it achieves actual conflict resolution. Repeated ceasefire-violation cycles (2011, 2013, 2014) reveal that enforcement mechanisms are theatrical rather than structural.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence across the seven perspectives reveals the constraint's hybrid nature. ASEAN institutional framework treats the ceasefire as a coordination success (Rope); Thai military sees mixed coordination-extraction (Tangled Rope); Cambodian military experiences more extraction than coordination (Tangled Rope with victim markers); border civilians experience pure extraction with suppression (Snare); international mediators view it as temporary scaffolding (Scaffold); the regional diplomatic ritual sees its own theater (Piton); the analytical observer risks false naturalization (Mountain). This spectrum demonstrates that indexical classification captures real structural differences in how the same constraint affects different observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to extraction flows and enforcement mechanisms. ASEAN benefits from the mediation role (low d, negative chi from their perspective—the constraint gives them institutional credibility). Thai military gains strategic advantage (d ≈ 0.35: mixed beneficiary-victim status; constrained exit raises effective extraction). Cambodian military accepts strategic disadvantage (d ≈ 0.65: victim status with constrained exit; higher effective extraction). Border civilians are trapped targets (d ≈ 0.95: maximum extraction from their perspective). The engine's derivation chain maps beneficiary/victim declarations + power level + exit options to these d values and produces chi via the sigmoid f(d) and scope modifier σ(S) = 1.1 for continental scope.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false classification as pure coordination (Rope) by explicitly declaring victim groups (civilian populations, dispute finality, verification capacity) and enforcement mechanisms (ASEAN military coordination, border restrictions). The tangled_rope classification holds at the institutional level because the coordination function (preventing bilateral escalation) is genuine and structurally necessary. However, the mandatrophy is complex: the Thai-Cambodian dyad experiences the ceasefire as hybrid coordination-extraction; Cambodia bears disproportionate extraction; civilians bear pure extraction. The piton perspective reveals that ASEAN mediation has become theatrical—the ritual of brokering ceasefires generates institutional utility independent of whether underlying disputes are resolved. The periodic resurgences (2013, 2014) confirm piton structure: theater ratio rises as ASEAN mediation repeats without functional breakthrough. The mandatrophy is NOT resolved by a single classification; it is resolved by recognizing that the constraint exhibits different types from different perspectives, and the perspectival gap IS the answer: institutional mediation theater (piton) masks asymmetric military extraction (tangled rope) imposed on border civilians (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asean_enforcement_capacity,
    'Does ASEAN possess genuine enforcement capacity to prevent bilateral escalation, or are ceasefires conditional solely on the bilateral parties'' calculations of cost-benefit?',
    'Comparative analysis of ASEAN-brokered ceasefires that held vs. those that failed; examination of whether enforcement actions were ever invoked or merely threatened',
    'If genuine capacity: ceasefire is Tangled Rope (coordination with enforcement overlay). If illusory: ceasefire is Piton (theatrical agreement with minimal functional constraint). Evidence suggests illusory — ASEAN has never invoked sanctions against Thai or Cambodian escalation despite 2013, 2014 resurgences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asean_enforcement_capacity, empirical, 'Whether ASEAN has real enforcement capacity or merely coordination theater').

omega_variable(
    icj_preah_vihear_finality,
    'Will ICJ rulings on the Preah Vihear maritime boundary eventually resolve the underlying dispute, or will territorial claims persist as permanent sources of friction?',
    'Historical pattern analysis of ICJ territorial rulings; evidence of whether losing parties accept adverse rulings; implementation timeline for phased ICJ phases on maritime boundaries',
    'If ICJ finality is achieved: ceasefire transitions to scaffolding (sunset via adjudication). If ICJ fails: ceasefire remains permanent piton (theatrical perpetuation). Current trajectory suggests ICJ phase 2-4 rulings will extend through 2030s without comprehensive finality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icj_preah_vihear_finality, empirical, 'Whether ICJ dispute resolution will provide lasting settlement or perpetuate territorial friction').

omega_variable(
    extraction_beneficiary_asymmetry,
    'Does the Thai military gain asymmetric strategic advantage from the ceasefire relative to Cambodian military capacity, making the constraint extractive for Cambodia despite the coordination frame?',
    'Comparative military capability analysis; force posture changes during ceasefire periods; territorial consolidation benefits to each side; interviews with military strategists',
    'If Thai advantage is asymmetric: extraction flows from Cambodia to Thailand, elevating Cambodian perspective to higher d-value (more snare-like). If balanced: coordination frame holds (tangled rope confirmed). Evidence suggests Thai asymmetry is significant — Thai military conventionally dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_asymmetry, empirical, 'Whether ceasefire creates asymmetric military advantage for Thailand over Cambodia').

omega_variable(
    regional_mediation_theater_substitution,
    'Has ASEAN mediation become a substitute for actual dispute resolution—i.e., does the theater of brokering ceasefires allow the underlying conflict to persist indefinitely?',
    'Timeline analysis of dispute resolution attempts; count of ceasefire agreements and resurgences; evidence of progress toward binding settlements vs. perpetuation of ceasefire cycles',
    'If theater substitution confirmed: piton classification elevated in salience; mandatrophy reveals that coordination narrative masks perpetual extraction from border populations. If false: ASEAN mediation is genuine constraint that will eventually enable resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_mediation_theater_substitution, conceptual, 'Whether ASEAN mediation theater substitutes for actual dispute resolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asean_ceasefire_2011, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asean_tr_t0, asean_ceasefire_2011, theater_ratio, 0, 0.48).
narrative_ontology:measurement(asean_tr_t1, asean_ceasefire_2011, theater_ratio, 1, 0.62).
narrative_ontology:measurement(asean_tr_t2, asean_ceasefire_2011, theater_ratio, 2, 0.65).

% Extraction over time
narrative_ontology:measurement(asean_be_t0, asean_ceasefire_2011, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(asean_be_t1, asean_ceasefire_2011, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(asean_be_t2, asean_ceasefire_2011, base_extractiveness, 2, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asean_ceasefire_2011, enforcement_mechanism).
narrative_ontology:affects_constraint(asean_ceasefire_2011, preah_vihear_territorial_claim).
narrative_ontology:affects_constraint(asean_ceasefire_2011, icj_maritime_boundary_dispute).
narrative_ontology:affects_constraint(asean_ceasefire_2011, thai_cambodian_military_asymmetry).

% DUAL FORMULATION NOTE:
% The 2011 ceasefire is downstream of the underlying Preah Vihear territorial dispute (constraint: preah_vihear_territorial_claim) but represents a distinct structural phenomenon: the substitution of mediation theater for dispute resolution finality. The ceasefire's extractiveness (0.52) reflects enforcement mechanisms and asymmetric military advantage; the territorial claim's extractiveness would be higher (0.65+, snare-like, with suppression ≥ 0.75). The two constraints are decomposed because their ε values differ materially and their failure modes are distinct. Upstream constraint (territorial claim) drives the ceasefire; ceasefire enables perpetuation of the claim by substituting theatrical mediation for binding adjudication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asean_ceasefire_2011, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
