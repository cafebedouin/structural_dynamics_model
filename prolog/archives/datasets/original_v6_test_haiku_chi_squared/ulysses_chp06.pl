% ============================================================================
% CONSTRAINT STORY: ulysses_chp06
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp06, []).

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
 *   constraint_id: ulysses_chp06
 *   human_readable: The Funerary Mountain (Prospect Cemetery)
 *   domain: social/religious/technological
 *
 * SUMMARY:
 *   Leopold Bloom's attendance at Paddy Dignam's funeral in 'Hades' (Chapter
 *   6 of Ulysses, set June 16, 1904, Dublin) presents a layered constraint
 *   structure. On the surface, the funeral is a straightforward coordination
 *   mechanism: the mourning community assembles to perform communal rituals
 *   that honor the dead and process collective grief. But underneath runs an
 *   immutable fact — death itself, the irreducible constraint from which all
 *   funerary practice emerges. The constraint exhibits both Mountain and Rope
 *   characteristics depending on the observer. For the analytical observer,
 *   mortality is a natural law; for Bloom and the mourning community, the
 *   funeral is a beneficial coordination ritual that solves the problem of
 *   how to behave appropriately in the presence of death. The Church and
 *   cemetery authority derive modest but legitimate benefit from the system.
 *   A modernist or secular perspective sees the ritual as culturally
 *   contingent, with a sunset clause as cremation and state ceremonies become
 *   viable alternatives. The constraint demonstrates how the same social
 *   structure (the funeral) can be simultaneously immutable (death) and
 *   contingent (ritual form).
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Moderate mourner (moderate/mobile) — experiences ritual as meaningful coordination, navigates both social obligation and genuine grief
 *   - Paddy Dignam: The deceased (powerless/trapped) — no agency; funeral performed for living, not dead
 *   - Mourning community: Dublin Catholic networks (institutional/constrained) — benefit from coordination structure; perpetuate ritual norms
 *   - Church and Cemetery Authority: Institutional actors (institutional/constrained) — derive modest revenue and institutional legitimacy; constrained by religious/social norms
 *   - Secular modernizers: Future alternative advocates (organized/constrained) — organized opposition to Catholic monopoly on ceremony; see sunset approaching
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees mortality as irreducible mountain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp06, 0.12).
domain_priors:suppression_score(ulysses_chp06, 0.08).
domain_priors:theater_ratio(ulysses_chp06, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp06, extractiveness, 0.12).
narrative_ontology:constraint_metric(ulysses_chp06, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(ulysses_chp06, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp06, rope).
narrative_ontology:human_readable(ulysses_chp06, "The Funerary Mountain (Prospect Cemetery)").
narrative_ontology:topic_domain(ulysses_chp06, "social/religious/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp06, mourning_community).
narrative_ontology:constraint_beneficiary(ulysses_chp06, ritual_perpetuators).
narrative_ontology:constraint_beneficiary(ulysses_chp06, cemetery_maintenance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From the civilizational vantage, mortality is an immutable physical and biological fact. No agent can exit; no social arrangement can override it. The cemetery is infrastructure for managing an unchangeable constraint. ε≈0.08, accessibility_collapse≈0.92, resistance≈0.08, emerges_naturally confirmed. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.10.
constraint_indexing:constraint_classification(ulysses_chp06, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: LEOPOLD BLOOM / MOURNER (ROPE) — Bloom attends Paddy Dignam's funeral as a coordinated social obligation. The funeral itself is low-extraction coordination: all participants agree on the ritual structure (music, prayers, procession), the burial location (Prospect Cemetery), and the behavioral norms (silence, sobriety, respect). Bloom is mobile — he could skip the funeral but chooses participation. The ritual solves a genuine coordination problem: how do strangers and acquaintances collectively honor the dead without conflict? Theater is moderate (0.35) — genuine emotion mixed with performative elements. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.06.
constraint_indexing:constraint_classification(ulysses_chp06, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: MOURNING COMMUNITY / DUBLIN CATHOLICS (ROPE) — The Catholic funerary ritual is institutional coordination: parish structure, requiem mass procedures, cemetery associations, and burial protocols are all established routines that reduce friction and conflict. The community experiences the constraint as enabling: it tells people how to behave appropriately, maintains social bonds across generations, and prevents chaos at moments of vulnerability. d≈0.40, f(d)≈0.40, σ=0.9 → χ≈0.04.
constraint_indexing:constraint_classification(ulysses_chp06, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHURCH & CEMETERY AUTHORITY (ROPE) — The Church and cemetery authority (plot allocation, maintenance, fee collection) derive modest benefit from the funerary system. Benefits are genuine: steady revenue from burial fees, maintained grounds, institutional perpetuation. But extraction is low — the institutional actors are constrained by religious doctrine (dignity of the dead, community service) and social norms (cannot refuse burial, cannot exploit grief egregiously). Theater is moderate — some performative ceremony, but genuine sacramental function. d≈0.35, f(d)≈0.30, σ=0.9 → χ≈0.03.
constraint_indexing:constraint_classification(ulysses_chp06, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PADDY DIGNAM / THE DEAD (MOUNTAIN) — The deceased has zero degrees of freedom. Mortality is absolute; no exit, no alternatives, no negotiation. The funeral is performed for the living, not the dead. d≈0.99, f(d)≈1.42, σ=1.0 → χ≈0.17. However, this perspective is trivial — the dead experience no constraint because they experience nothing. The mountain classification reflects the observer's structural position (powerless + trapped), not any meaningful relationship to the deceased.
constraint_indexing:constraint_classification(ulysses_chp06, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 6: SECULAR MODERNISM / FUTURE ALTERNATIVES (SCAFFOLD) — From the perspective of early-20th-century secular modernism and future burial alternatives (cremation, state vs. religious ceremony), the Catholic funerary system is a temporary coordination mechanism with a sunset clause. Organized secularists and modernizers view the ritual as valuable but bounded: it will eventually be displaced by more efficient, secular, or technological alternatives (cremation, civil ceremonies). The constraint persists through cultural inertia but faces organized opposition. d≈0.45, f(d)≈0.45, σ=0.9 → χ≈0.14. Theater rises as function declines (0.35 current, trending toward 0.50).
constraint_indexing:constraint_classification(ulysses_chp06, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp06_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp06, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp06, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ulysses_chp06_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The funeral system does generate revenue for the Church and cemetery authority through burial fees, plots, and maintenance. But extraction is minimal — fees are modest relative to service, social norms prevent exploitation of grief, and the system provides genuine coordination benefits to all participants. The trend over 1850-1904 shows slight increase (0.08→0.12) as urbanization and commercialization marginally increase extraction. Suppression (0.08): Very low. Participants are substantially mobile — Bloom could skip the funeral, mourners could choose alternative ceremonies (though options are limited in 1904 Dublin), the community is not coerced. Suppression rises only if one equates social norms with coercion; structural suppression is minimal. Theater ratio (0.35): Moderate. The funeral contains both genuine emotion and performative elements. Mourners genuinely grieve and bond; the ritual meaningfully structures that grief. But ceremony also contains theatrical elements — formal dress, prescribed language, choreographed movements — that follow script rather than spontaneous expression. Theater has increased over 50 years as the ritual has become more standardized and less organic.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is not about disagreement on type (all perspectives except the modernist see Rope or Mountain), but about what the underlying constraint IS. Bloom sees a Rope — functional coordination that helps mourners navigate grief. The Church sees a Rope — institutional mechanism that perpetuates community and generates modest revenue. The analytical observer sees a Mountain — mortality is the real constraint, and funerary ritual is infrastructure for managing the immutable. The modernist sees a Scaffold — the Catholic ritual is a temporary coordination mechanism whose sunset is arriving via cremation and secularization. The deceased (trivially) experiences Mountain — zero degrees of freedom. The perspectival gap reveals that participants are coordinating around different underlying constraints: Bloom is solving 'How do I behave appropriately?' (Rope); the Church is solving 'How do we maintain institutional legitimacy?' (Rope); the modernist is solving 'How do we transition away from religious monopoly on ceremony?' (Scaffold); the analytical observer is noting that all these solutions address a deeper immutable (Mountain). None are wrong; they address different structural levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Mourning community: Beneficiary + constrained → d≈0.40, f(d)≈0.40. The community benefits from the coordination structure (knows how to behave, maintains social bonds) but is constrained by religious authority and cannot easily exit established norms. Church and Cemetery Authority: Beneficiary + constrained → d≈0.35, f(d)≈0.30. Genuine benefit (revenue, institutional role) but constrained by doctrine, social expectations, and inability to overcharge without backlash. Leopold Bloom: Both beneficiary and victim, mobile → d≈0.50, f(d)≈0.65. Benefits from knowing ritual structure (reduces social anxiety) but also victims to extraction indirectly (pays for burial through community tithe, spends emotional labor). Paddy Dignam: Victim + trapped → d≈0.99, but trivial (no experience). Secular modernist: Victim of Catholic monopoly, organized, constrained → d≈0.45, f(d)≈0.45. Sees constraint as extractive (Catholic control of ceremony) but organized and has exit path (alternative ceremonies). Analytical observer: Observer position, d≈0.72, f(d)≈1.15. Sees the constraint from outside; risks naturalizing the ritual as immutable when it is culturally contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by distinguishing the Mountain (death itself) from the Rope (funerary coordination). The funerary ritual addresses the Mountain but is not itself a Mountain. Participants who mistake the ritual FOR the constraint (who think 'the proper burial is necessary to honor the dead') have inverted the logic — the ritual is a solution, not the underlying fact. The analytical observer who sees a Mountain is correct about mortality but must not confuse that with a Mountain classification of the funerary system. The Church and mourning community correctly see Rope — they are solving a genuine coordination problem (how to behave appropriately, how to maintain social bonds). The modernist correctly sees the sunset — cremation and secular ceremonies are alternative solutions to the same underlying constraint (mortality). No mandatrophy exists if the analysis holds this distinction: the Mountain (mortality) is constant; the Rope (ceremonial coordination) is contingent and trending toward Scaffold/Piton as alternatives emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacred_vs_coordination,
    'Is the funerary ritual a sacred mountain (irreducible religious truth) or a coordination rope (culturally contingent solution)?',
    'Historical analysis of burial practice variation across cultures; identification of universal vs. culturally specific elements; theological vs. anthropological interpretation',
    'If sacred: the mountain view is foundational; modernist perspectival gap is invalid. If coordination: the scaffold perspective is correct; the constraint will erode over generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_vs_coordination, conceptual, 'Whether funerary ritual is sacred/immutable or culturally contingent').

omega_variable(
    emotional_reality_of_extraction,
    'Does the Church''s modest extraction (burial fees, institutional perpetuation) constitute genuine asymmetric extraction from grieving families, or is it fair compensation for services rendered?',
    'Fee structure analysis relative to income; comparison with secular cremation/burial alternatives; historical analysis of fee collection practices during grief',
    'If extraction: the constraint contains Snare elements for economically vulnerable mourners. If fair compensation: the Rope classification holds across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_reality_of_extraction, empirical, 'Whether burial fees represent extraction or fair compensation').

omega_variable(
    necessity_of_ritual,
    'Is the funeral ritual (procession, ceremony, cemetery internment) necessary for grief processing and social bonding, or is it performative theater masking the fundamental mountain (death) underneath?',
    'Psychological research on grief outcomes with vs. without ritual; ethnographic comparison of low-ritual vs. high-ritual cultures; analysis of grief satisfaction metrics',
    'If necessary: theater is functional (0.35 is accurate); Rope classification is stable. If performative: theater is masking (theater should be higher, 0.50+); Piton perspective becomes valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_of_ritual, empirical, 'Whether ritual is functionally necessary for grief or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp06, 1850, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulysses_hades_tr_t1850, ulysses_chp06, theater_ratio, 1850, 0.25).
narrative_ontology:measurement(ulysses_hades_tr_t1880, ulysses_chp06, theater_ratio, 1880, 0.3).
narrative_ontology:measurement(ulysses_hades_tr_t1904, ulysses_chp06, theater_ratio, 1904, 0.35).

% Extraction over time
narrative_ontology:measurement(ulysses_hades_be_t1850, ulysses_chp06, base_extractiveness, 1850, 0.08).
narrative_ontology:measurement(ulysses_hades_be_t1880, ulysses_chp06, base_extractiveness, 1880, 0.1).
narrative_ontology:measurement(ulysses_hades_be_t1904, ulysses_chp06, base_extractiveness, 1904, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp06, enforcement_mechanism).
narrative_ontology:affects_constraint(ulysses_chp06, ulysses_wandering_isotropy).
narrative_ontology:affects_constraint(ulysses_chp06, ulysses_consciousness_stream).

% DUAL FORMULATION NOTE:
% The funerary system exhibits dual formulation: as natural law (death, immutable) and as cultural practice (ritual, contingent). The Ulysses narrative uses the funeral to explore how consciousness confronts the mountain (Bloom's internal monologue during the burial) while performing the rope (external ritual compliance). The two constraints are structurally linked — the rope derives its meaning-making power from the presence of the mountain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
