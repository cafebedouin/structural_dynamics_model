% ============================================================================
% CONSTRAINT STORY: comitatus_bond
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_comitatus_bond, []).

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
 *   constraint_id: comitatus_bond
 *   human_readable: The Germanic Comitatus Code
 *   domain: social/political
 *
 * SUMMARY:
 *   The comitatus is the foundational socio-political constraint governing
 *   the relationship between a lord (the 'ring-giver') and his thanes in
 *   early Germanic society, documented in Tacitus, exemplified in Beowulf,
 *   and embedded in early medieval Germanic law codes. The constraint solves
 *   a military coordination problem — how to assemble and maintain a reliable
 *   war band in a pre-state, kinship-based society — while simultaneously
 *   creating and perpetuating status asymmetry and economic extraction. From
 *   the lord's perspective, the comitatus is a coordination mechanism that
 *   binds warriors to his service through oath, kinship obligation, and the
 *   prestige economy. From the thane's perspective, it is a snare: the oath
 *   is lifetime, breach results in social death, and alternatives are
 *   suppressed by the honor code. From the commons' perspective, it is pure
 *   extraction: they subsidize the warrior prestige economy through taxation
 *   and resource confiscation, while having no voice in the code's
 *   enforcement. The constraint exhibits the full six-type perspectival gap:
 *   rope (lord), snare (thane and commons), tangled rope (warrior culture and
 *   analytical observer), and piton (late medieval institutional memory as
 *   the literal constraint decays but the narrative persists). The theater
 *   ratio shows a slight rise over the interval as the actual enforcement
 *   mechanisms fade but the performative memory of the bond strengthens in
 *   epic and ceremonial contexts.
 *
 * KEY AGENTS:
 *   - Ring-Giver Lord: Primary beneficiary (institutional/arbitrage) — benefits from coordination function, distributes rings to bind warriors, captures prestige and military advantage
 *   - Oath-Bound Thane: Primary victim (powerless/trapped) — sacrifices autonomy, commits to death before the lord, receives rings but no guaranteed compensation or exit
 *   - Warrior Prestige Economy: Organized enforcer (organized/constrained) — maintains and transmits the code through kinship networks, skalds, and law; benefits from status hierarchy but constrained by generational enforcement
 *   - Non-Warrior Commons: Secondary victim (powerless/trapped) — subsidize the warrior class through taxation and labor, have no voice in code enforcement, bear security costs of constant warfare
 *   - Late Medieval Institutional Memory: Vestigial actor (institutional/constrained) — comitatus survives as epic theme, literary ideal, ceremonial loyalty oath; the actual enforcement mechanisms have decayed but the performative narrative persists
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as hybrid coordination-extraction system where the coordination function legitimizes asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(comitatus_bond, 0.38).
domain_priors:suppression_score(comitatus_bond, 0.68).
domain_priors:theater_ratio(comitatus_bond, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(comitatus_bond, extractiveness, 0.38).
narrative_ontology:constraint_metric(comitatus_bond, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(comitatus_bond, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(comitatus_bond, tangled_rope).
narrative_ontology:human_readable(comitatus_bond, "The Germanic Comitatus Code").
narrative_ontology:topic_domain(comitatus_bond, "social/political").

domain_priors:requires_active_enforcement(comitatus_bond).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(comitatus_bond, ring_giver_lord).
narrative_ontology:constraint_beneficiary(comitatus_bond, warrior_prestige_economy).
narrative_ontology:constraint_victim(comitatus_bond, bound_thanes).
narrative_ontology:constraint_victim(comitatus_bond, non_warrior_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OATH-BOUND THANE (SNARE) — A thane who swears the comitatus bond sacrifices individual choice; death before the lord is not optional but covenantal. Exit through cowardice results in total social death, loss of kinship bonds, and exclusion from the warrior prestige economy. The oath is binding unto death and no alternatives exist within the status hierarchy. Maximum extraction from the thane's perspective: lifetime commitment, martial obligation without compensation guarantee, and absolute subordination.
constraint_indexing:constraint_classification(comitatus_bond, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RING-GIVER LORD (ROPE) — The lord experiences the comitatus as a coordination mechanism: binding thanes to his war band solves the collective action problem of assembling a reliable fighting force. The lord gives rings (wealth distribution) and expects loyalty in return. From the lord's institutional perspective, the constraint solves a genuine coordination problem — how to assemble warriors who will not flee. The extraction runs toward the thane, not the lord. The lord benefits from the coordination function and has exit options (recruit different thanes).
constraint_indexing:constraint_classification(comitatus_bond, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE WARRIOR PRESTIGE ECONOMY (TANGLED ROPE) — The broader warrior culture that sustains and enforces the comitatus sees it as both coordination (the bond creates stable war bands and reduces internal feuding) and extraction (the code concentrates prestige, wealth, and martial authority in the hands of successful ring-givers and their households, excluding commoners and enforcing strict status hierarchy). Organized agents (skalds, thane councils, succession lineages) have partial exit but limited agency — the code is generationally enforced. The prestige economy benefits from stability but bears costs through rigid hierarchy.
constraint_indexing:constraint_classification(comitatus_bond, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE NON-WARRIOR COMMONS (SNARE) — Farmers, craftspeople, and unfree laborers have no access to the prestige economy and bear the costs of constant warfare: taxation, conscription of resources, and lack of security. The comitatus extracts wealth and labor from the commons to support the warrior hierarchy, and they have no exit option or voice in the code's enforcement. Total suppression of alternatives.
constraint_indexing:constraint_classification(comitatus_bond, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE INSTITUTIONAL MEMORY (LATE MEDIEVAL PERSPECTIVE) (PITON) — By the High Middle Ages, the literal comitatus is dead; feudalism has replaced it. Yet the cultural memory of the oath-bound war band persists in epic poetry (Beowulf, Song of the Nibelungs), in vestigial loyalty oaths, and in theatrical re-enactment of ancient Germanic virtue. The institutional structures that enforced the code have decayed, but the performative narrative of the noble bond lingers. Theater ratio rises as the actual enforcement mechanisms (kinship networks, immediate warrior culture) disappear but the ideal persists in literature and ceremonial.
constraint_indexing:constraint_classification(comitatus_bond, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal view, the comitatus is a hybrid constraint: it solves a genuine military coordination problem (how to maintain a war band without central bureaucracy) while simultaneously creating asymmetric extraction (thanes bear disproportionate risk and loss of autonomy; commoners subsidize the warrior prestige economy). The constraint is neither pure coordination nor pure extraction but an integrated system where the coordination function legitimizes and enables the extraction.
constraint_indexing:constraint_classification(comitatus_bond, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(comitatus_bond_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(comitatus_bond, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(comitatus_bond, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(comitatus_bond, TR),
    TR >= 0.70.

:- end_tests(comitatus_bond_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The comitatus extracts thane autonomy, risk-bearing, and lifetime obligation. However, the constraint is not maximum-extraction because the ring distribution provides real material compensation (albeit unequal), and the coordination function addresses a genuine collective problem. The extractiveness reflects a bargain: the thane trades autonomy for prestige, wealth distribution, and military brotherhood. Suppression (0.68): High. Alternatives to the oath are severely suppressed: refusal means social death, exclusion from prestige economy, and loss of kinship bonds. Breach of oath triggers blood feud and clan dishonor. The code is enforced through kinship networks and cultural shame — institutional rather than explicit coercion, but highly effective. Theater ratio (0.55): Moderate. The comitatus has genuine coordination function (assembles fighting forces, reduces internal feuding, creates stable leadership), but also significant performative elements: the ring-giving ceremony, the oath ritual, the war-band identity. As the constraint ages (post-500 CE), the performative element increases relative to function — epics celebrate the bond long after the literal institutional structure has weakened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The ring-giver lord sees rope (coordination, solves the war-band assembly problem). The thane sees snare (lifetime obligation, no exit, death expected). The warrior prestige culture sees tangled rope (both coordination through stable hierarchies and extraction through status concentration). The commons sees snare (pure extraction, no benefit, no voice). The late medieval society sees piton (the old code survives as literary and ceremonial memory, but actual enforcement has decayed). The analytical observer sees tangled rope with false summit danger (the naturalization of the honor code as 'inherent Germanic virtue' rather than contingent institutional arrangement). The perspectival gap reveals that the same constraint distributes costs and benefits radically unequally across the status hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. The ring-giver lord is a beneficiary with arbitrage exit options — he can recruit different thanes, renegotiate terms, and exit the bond with minimal cost. Derived d ≈ 0.15 (beneficiary + arbitrage). The oath-bound thane is a victim with trapped exit — breach triggers social death, and kinship obligation prevents escape. Derived d ≈ 0.95 (victim + trapped). The warrior prestige economy is an organized beneficiary but with generational constraints — they maintain and enforce the code but cannot fully exit without losing status. Derived d ≈ 0.40 (organized + constrained). The commons are trapped victims with no voice — they subsidize the warrior class with no benefit and no exit. Derived d ≈ 0.98 (powerless + trapped). The directionality values track the asymmetry: high d values for those bearing extraction (thanes and commons), low d values for beneficiaries (lords), and moderate d for the organized culture that mediates the code.
 *
 * MANDATROPHY ANALYSIS:
 *   The comitatus resolves mandatrophy by demonstrating that genuine coordination (assembling a war band, creating stable hierarchies) can coexist with asymmetric extraction (thane autonomy loss, commons subsidy). The constraint is not 'really just coordination' or 'really just extraction' — it is both integrated. The mandate is coordination: solve the collective action problem of military organization. The atrophy risk is extraction: the coordination mechanism becomes a vehicle for status concentration and wealth transfer. The constraint avoids pure snare classification because the lord genuinely solves a coordination problem and the thane genuinely receives prestige and material benefit. But it avoids pure rope classification because the thane bears disproportionate risk and sacrifice. The tangled rope classification captures this hybrid: real coordination function + asymmetric extraction + active enforcement. The piton perspective (late medieval) shows the risk — as the actual coordination function weakens (feudalism replaces comitatus), the performative theater increases (epic celebration, ceremonial loyalty oaths) but the underlying extraction mechanism weakens through institutional decay rather than through resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_oath_authenticity,
    'To what extent is the thane''s oath genuinely voluntary vs. coerced by kinship obligation and social exclusion threats?',
    'Historical analysis of thane recruitment patterns, comparison with conscription rates, examination of documented refusals or opt-outs and their social consequences',
    'If purely voluntary: the constraint is rope (coordination with consent). If coerced by social structure: it is snare (extraction with suppressed alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_oath_authenticity, conceptual, 'Degree of genuine voluntariness in oath-taking').

omega_variable(
    ring_giver_capacity_constraint,
    'Are the ring-giver''s distributions genuinely wealth transfers (coordination payment) or performative gifts with no real material value?',
    'Archaeological/documentary analysis of ring quality, market value, distribution frequency, comparison with warrior subsistence costs and compensation expectations',
    'If rings are substantive compensation: tangled rope confirmed (real trade-off between risk and wealth). If rings are symbolic: the system is closer to pure snare (thanes risk death for prestige, not material security).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ring_giver_capacity_constraint, empirical, 'Material value of ring-giver compensation').

omega_variable(
    commons_consent_invisibility,
    'Did non-warriors have any institutional voice in or tacit acceptance of the comitatus code, or was it purely top-down imposition?',
    'Analysis of folk law, oral tradition, craft guild records, evidence of peasant resistance or accommodation, comparison with societies where commons had veto power',
    'If commons had voice: system is more complex tangled rope (multi-stakeholder). If purely imposed: snare classification for commons is solid, and the overall constraint is more purely extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_consent_invisibility, empirical, 'Degree of commons agency in comitatus legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(comitatus_bond, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comit_tr_t0, comitatus_bond, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comit_tr_t500, comitatus_bond, theater_ratio, 500, 0.5).
narrative_ontology:measurement(comit_tr_t1000, comitatus_bond, theater_ratio, 1000, 0.55).

% Extraction over time
narrative_ontology:measurement(comit_be_t0, comitatus_bond, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comit_be_t500, comitatus_bond, base_extractiveness, 500, 0.38).
narrative_ontology:measurement(comit_be_t1000, comitatus_bond, base_extractiveness, 1000, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(comitatus_bond, enforcement_mechanism).
narrative_ontology:affects_constraint(comitatus_bond, feudal_hierarchy).
narrative_ontology:affects_constraint(comitatus_bond, germanic_honor_code).
narrative_ontology:affects_constraint(comitatus_bond, early_medieval_kinship_law).

% DUAL FORMULATION NOTE:
% The comitatus as a historical institution (c. 1-500 CE) has extractiveness ~0.38 with active enforcement. The comitatus as a narrative ideal in late medieval literature and ceremonial (c. 500-1500 CE) has extractiveness ~0.25 with high theater ratio ~0.75 (piton). These are structurally distinct constraints: the historical comitatus is tangled rope; the literary/ceremonial memory is piton. Both are included in this story to show the institutional decay trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
