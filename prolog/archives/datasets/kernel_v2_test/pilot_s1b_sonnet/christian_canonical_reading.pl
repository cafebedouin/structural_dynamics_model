% ============================================================================
% CONSTRAINT STORY: christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_christian_canonical_reading, []).

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
 *   constraint_id: christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Catholic/Protestant)
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   Christian ecclesiastical authority over marriage represents one reading
 *   of the family_law_authority kernel. The Catholic Church claims exclusive
 *   jurisdiction over marital validity for baptized persons through
 *   sacramental theology: marriage is an indissoluble sacrament conferring
 *   grace, with annulment (not divorce) as the only dissolution pathway.
 *   Protestant denominations maintain denominational governance with varied
 *   divorce permissibility, ranging from near-Catholic restrictiveness
 *   (Anglican) to liberal acceptance (mainline Protestant). The constraint
 *   coordinates genuine functions—ritual provision, community formation,
 *   pastoral care, theological consistency—while extracting through
 *   jurisdictional monopoly claims, identity-based exit barriers, and
 *   asymmetric dissolution access. The structure is a tangled rope:
 *   coordination and extraction are inseparable. The Catholic identity-lock
 *   is particularly strong: divorce-seeking Catholics are structurally mobile
 *   (secular law is available) but identity-fused with sacramental framework,
 *   making exit psychologically unthinkable from within the frame. The
 *   annulment tribunal system has degraded into substantial theater: formal
 *   investigation of marital validity conducted decades post-facto, with
 *   outcomes often determined by diocesan resources rather than canonical
 *   facts. The 2015 Mitis Iudex reforms streamlining annulment procedures
 *   constitute an implicit institutional admission of prior theater. This
 *   constraint is downstream of core theological doctrines (sacramental
 *   theology, apostolic succession) but is a distinct structural constraint
 *   with its own extraction profile and victim set.
 *
 * KEY AGENTS:
 *   - Divorce-Seeking Catholic: Primary victim (powerless/identity_locked) — structurally mobile but cognitively bound; experiences maximum extraction through mandatory permanence
 *   - Protestant Denominational Member: Mixed victim-beneficiary (moderate/constrained) — receives coordination alongside extraction; can exit at moderate cost
 *   - Ecclesiastical Hierarchy: Primary beneficiary (institutional/arbitrage) — collects from jurisdictional authority, annulment fees, pastoral monopoly
 *   - Interfaith Couple: Secondary victim (moderate/constrained) — faces canonical barriers and dispensation requirements but receives some coordination benefits
 *   - Secular Legal Reform Coalition: Organized agents (organized/mobile) — building alternative secular pathways; sees ecclesiastical authority as transitional with sunset
 *   - Annulment Tribunal System: Institutional actor (institutional/arbitrage) — maintains performative adjudication ritual; recognizes own degraded function (piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees tangled rope structure with genuine coordination and genuine extraction inseparable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(christian_canonical_reading, 0.48).
domain_priors:suppression_score(christian_canonical_reading, 0.62).
domain_priors:theater_ratio(christian_canonical_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(christian_canonical_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(christian_canonical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(christian_canonical_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(christian_canonical_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(christian_canonical_reading, "Christian Canonical Marriage Authority (Catholic/Protestant)").
narrative_ontology:topic_domain(christian_canonical_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(christian_canonical_reading, 'a7fcfaae-9676-4cee-b1b8-ac4d648849e9').
narrative_ontology:cs_kernel_codification('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', formalized).
narrative_ontology:cs_authority_grounding('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', lineage).
narrative_ontology:cs_interpretation_layer_present('a7fcfaae-9676-4cee-b1b8-ac4d648849e9').
narrative_ontology:cs_reading_relation('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', christian_canonical_reading__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', christian_canonical_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', christian_canonical_reading__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', christian_canonical_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', foundational, marriage_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', marriage_indissoluble_sacrament, deontological).
narrative_ontology:cs_axiom('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', secondary, ecclesiastical_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(ecclesiastical_jurisdiction_exclusive, overridden).
narrative_ontology:cs_axiom_grounding('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', ecclesiastical_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', tridentine_sacramental_framework).
narrative_ontology:cs_drift_state('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', post_secularization_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7fcfaae-9676-4cee-b1b8-ac4d648849e9', '').
narrative_ontology:cs_kernel_id(christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(christian_canonical_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(christian_canonical_reading, denominational_governance_bodies).
narrative_ontology:constraint_beneficiary(christian_canonical_reading, conforming_married_couples).
narrative_ontology:constraint_victim(christian_canonical_reading, divorce_seeking_catholics).
narrative_ontology:constraint_victim(christian_canonical_reading, interfaith_couples).
narrative_ontology:constraint_victim(christian_canonical_reading, remarriage_seekers).
narrative_ontology:constraint_victim(christian_canonical_reading, annulment_petitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIVORCE-SEEKING CATHOLIC (SNARE) — Identity-locked rather than structurally trapped: could exit to secular jurisdiction, but Catholic identity is constituted through sacramental framework. Biographical time horizon shows the binding as cognitive rather than material. Experiences maximum extraction through mandatory permanence with no dissolution pathway except annulment (theatrical, expensive, uncertain). The coordination story (sacramental grace) is experienced as cover for extraction.
constraint_indexing:constraint_classification(christian_canonical_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: PROTESTANT DENOMINATIONAL MEMBER (TANGLED ROPE) — Constrained by denominational norms and social costs but not absolutely bound. Experiences genuine coordination (community support, ritual framework, marriage counseling) alongside extraction (denominational discipline, reputation costs of divorce, pastoral gatekeeping). Can exit to another denomination or secular jurisdiction at moderate cost. Mixed beneficiary and victim.
constraint_indexing:constraint_classification(christian_canonical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ECCLESIASTICAL HIERARCHY (ROPE) — Benefits from jurisdictional authority over validity, annulment processes, and pastoral care infrastructure. Experiences the constraint as coordination: maintaining theological consistency, providing ritual framework, adjudicating canonical questions. Arbitrage exit: can engage or withdraw from civil legal systems strategically. Primary beneficiary.
constraint_indexing:constraint_classification(christian_canonical_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERFAITH COUPLE (TANGLED ROPE) — Faces canonical barriers (dispensations required, pastoral gatekeeping, potential invalidity) but also receives coordination benefits (ritual inclusion, community recognition where granted). Constrained by high costs of navigating dual religious requirements. Both coordinated and extracted from.
constraint_indexing:constraint_classification(christian_canonical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: SECULAR LEGAL REFORM COALITION (SCAFFOLD) — Organized advocacy groups, legal reformers, and civil society actors see ecclesiastical marriage authority as a transitional arrangement being superseded by secular family law. The sunset is the gradual separation of religious solemnization from civil recognition, already complete in many jurisdictions. Coordination function: preventing dual-jurisdiction conflicts during the transition.
constraint_indexing:constraint_classification(christian_canonical_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANNULMENT TRIBUNAL SYSTEM (PITON) — The Catholic annulment process is substantially theatrical: formal investigation of marital validity conducted decades post-facto, with outcomes often determined by diocesan resources and petitioner sophistication rather than canonical facts. Function has atrophied from theological adjudication to bureaucratic performance, maintained through institutional inertia. The system itself recognizes the performative nature (streamlined processes introduced 2015, implicit admission of prior theater).
constraint_indexing:constraint_classification(christian_canonical_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global view, ecclesiastical marriage authority coordinates genuine functions (ritual provision, community formation, theological consistency) while extracting through jurisdictional monopoly claims, exit barriers, and asymmetric dissolution access. The constraint persists through both coordination need and institutional enforcement. Not a mountain: the structure is contingent on institutional power, not natural law. Not a rope: identifiable victims exist. The claimed type matches the analytical classification.
constraint_indexing:constraint_classification(christian_canonical_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(christian_canonical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(christian_canonical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(christian_canonical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(christian_canonical_reading, TR),
    TR >= 0.70.

:- end_tests(christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Catholic no-divorce doctrine extracts from identity-locked adherents who cannot exit sacramental framework despite structural availability of secular alternatives. Protestant denominations extract less through more flexible dissolution pathways but still impose denominational discipline costs. Ecclesiastical hierarchy collects through jurisdictional authority, annulment processes (fees, delays, documentation requirements), and pastoral gatekeeping. The extraction is real but not maximal: secular family law provides exit for those who can break identity-lock, and Protestant variance demonstrates theological contingency. Suppression (0.62): Moderate-high. Current suppression is primarily identity-based and social rather than legal: Catholic identity constituted through sacramental participation creates cognitive exit barrier even where legal exit exists. Historical suppression was higher (0.70-0.72 in 1563-1763) when state enforcement of canon law was common and heresy prosecution threatened exit-seekers. Contemporary suppression includes social costs (family rupture, community exclusion), employment discrimination in church-affiliated institutions, and pastoral pressure against remarriage. Theater ratio (0.35): Moderate. Catholic annulment procedures are substantially performative: tribunals investigate marital validity decades post-facto with outcomes poorly correlated to canonical facts and strongly correlated to petitioner resources. The 2015 Mitis Iudex reforms reduced theater from ~0.48 to 0.35 by streamlining procedures, but the reduction itself constitutes institutional admission that prior complexity was non-functional. Protestant marriage discipline has lower theater (counseling and pastoral care retain more genuine adjudicative function).
 *
 * PERSPECTIVAL GAP:
 *   The divorce-seeking Catholic experiences a snare: the coordination story (sacramental grace) is cover for extraction through mandatory permanence with no dissolution pathway except theatrical annulment. Identity-lock rather than structural trap: secular divorce is legally available but psychologically unthinkable from within the sacramental frame. The Protestant member experiences tangled rope: genuine coordination (community, pastoral care) inseparable from extraction (denominational discipline, reputation costs). The ecclesiastical hierarchy experiences rope: they are coordinating theological consistency and ritual provision while collecting from jurisdictional authority. The interfaith couple experiences tangled rope: canonical barriers impose real costs but ritual inclusion (where granted) provides real benefits. The secular reform coalition experiences scaffold: ecclesiastical authority is transitional, being superseded by separation of religious solemnization from civil recognition—the sunset is already complete in many jurisdictions. The annulment tribunal sees piton: its own process has atrophied from genuine adjudication to bureaucratic performance maintained through inertia. The analytical observer sees tangled rope: coordination need is real (ritual framework, theological consistency) but extraction is real (identity-lock, jurisdictional monopoly, asymmetric dissolution), and the two are structurally inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The ecclesiastical hierarchy is a clear beneficiary with arbitrage exit: d approaches 0.0, producing low or negative chi (experiences coordination, minimal extraction). The divorce-seeking Catholic is a victim with identity_locked exit: d approaches 1.0, producing maximum chi through the cognitive binding mechanism—structural mobility exists but is inaccessible from within the identity frame. The Protestant member is both beneficiary (community support) and victim (denominational discipline) with constrained exit: d is moderate (~0.5), producing mixed chi. The interfaith couple is both coordinated and extracted from, also with constrained exit: d is moderate-high (~0.6). The analytical perspective is neither beneficiary nor victim: d is near 0.5 (neutral), chi is determined by base extraction without directional amplification or damping. The identity-lock mechanism is critical: the Catholic who cannot imagine themselves outside the sacramental framework experiences high chi not because exit is materially blocked but because exit requires abandoning the identity through which they understand themselves. A Catholic who breaks the identity-lock and exits to secular jurisdiction retroactively reveals that the suppression was cognitive rather than structural—the same objective barriers produced different experienced extraction depending on identity frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tangled rope is the structurally correct classification when coordination and extraction are genuinely inseparable. The ecclesiastical hierarchy's rope perspective is their genuine experience—they are coordinating theological consistency, not merely extracting. The divorce-seeking Catholic's snare perspective is their genuine experience—the coordination story is experienced as cover. Both are true simultaneously because the coordination function (sacramental framework provision) and the extraction function (identity-lock binding) operate through the same institutional structure. The constraint is not 'really' a rope with imaginary victims or 'really' a snare with imaginary coordination—it is a tangled rope where neither function can be isolated. The analytical observer's tangled_rope classification matches the claimed_type, confirming that the perspectival gap is not an error but the constraint's actual structure. The false summit risk (naturalizing the constraint as mountain through sacramental theology claims) is addressed by the omega variables: if sacramental marriage is genuine natural law, mountain classification becomes defensible; if constructed, ecclesiastical authority is primary beneficiary of the naturalization. The piton observation (annulment theater) is a local degradation within the larger tangled_rope structure, not a replacement of it—the theatrical annulment process is how the extraction mechanism operates in practice for the victim class seeking exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacrament_vs_contract_naturalization,
    'Is sacramental marriage theology a genuine natural law claim (marriage''s essential nature) or a constructed constraint benefiting ecclesiastical authority?',
    'Historical analysis: does sacramental theology predate or postdate institutional church power? Cross-cultural comparison: do societies without sacramental frameworks have stable marriage institutions? Theological genealogy: were early Christian marriage practices sacramental or contractual?',
    'If genuine natural law: mountain classification defensible from theological perspective. If constructed: tangled_rope confirmed, ecclesiastical authority is primary beneficiary of the sacramental claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrament_vs_contract_naturalization, conceptual, 'Whether sacramental marriage is natural law or constructed extraction').

omega_variable(
    protestant_variance_resolution,
    'Does Protestant denominational variance in divorce permissibility reveal that marriage permanence is not theologically essential, or does it represent theological error that Catholic doctrine correctly rejects?',
    'Intra-Christian theological dispute. Resolution depends on interpretive authority: sola scriptura vs. magisterial tradition. No neutral empirical resolution possible.',
    'If variance reveals non-essentiality: Catholic identity-lock is cognitive capture, not theological necessity. If variance is error: Catholic doctrine is coordination on truth, Protestant practice is drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protestant_variance_resolution, conceptual, 'Whether denominational divorce variance reveals theological contingency').

omega_variable(
    kernel_reading_alternative,
    'This constraint is one reading of the family_law_authority kernel. What would change structurally if the hindu_dharmashastra_reading or muslim_shariat_reading were adopted instead?',
    'Compare beneficiary sets, victim sets, and exit options across readings. Hindu reading: caste endogamy rules shift victims to intercaste couples. Muslim reading: unilateral male divorce (talaq) shifts extraction to women. Secular reading: eliminates religious authority as beneficiary, shifts coordination to state.',
    'Different readings preserve similar coordination functions (family formation, inheritance clarity, child custody frameworks) while shifting who collects from and who bears costs of the arrangement. The coordination need is kernel-stable; the extraction pattern is reading-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative, conceptual, 'Structural deltas across family_law_authority kernel readings').

omega_variable(
    annulment_theater_threshold,
    'At what procedural complexity does annulment investigation transition from genuine canonical adjudication to theatrical performance?',
    'Comparative analysis of annulment grant rates across dioceses with similar populations; correlation between petitioner resources (legal representation, documentation quality) and outcomes; tribunal backlog and processing time analysis; post-2015 streamlining impact measurement.',
    'If theater ratio < 0.4: annulment system retains substantial adjudicative function, piton classification weakens. If theater ratio > 0.6: system is primarily performative, piton classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_theater_threshold, empirical, 'Theater threshold in Catholic annulment procedures').

omega_variable(
    secular_sunset_completion,
    'Has the separation of religious solemnization from civil recognition reached completion, or does ecclesiastical authority still extract through informal mechanisms (social pressure, employment discrimination, inheritance custom) even where formal jurisdiction has transferred to state?',
    'Jurisdictional analysis: in how many countries does religious marriage ceremony alone confer legal status? Economic analysis: employment discrimination against divorced Catholics in church-affiliated institutions. Social network analysis: community exclusion patterns post-divorce.',
    'If sunset complete: scaffold classification confirmed, ecclesiastical extraction is residual. If extraction persists informally: scaffold classification is aspirational, tangled_rope persists in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_sunset_completion, empirical, 'Whether secular sunset has eliminated ecclesiastical extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(christian_canonical_reading, 0, 463).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chr_canon_tr_t0, christian_canonical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(chr_canon_tr_t200, christian_canonical_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(chr_canon_tr_t350, christian_canonical_reading, theater_ratio, 350, 0.38).
narrative_ontology:measurement(chr_canon_tr_t400, christian_canonical_reading, theater_ratio, 400, 0.42).
narrative_ontology:measurement(chr_canon_tr_t430, christian_canonical_reading, theater_ratio, 430, 0.48).
narrative_ontology:measurement(chr_canon_tr_t450, christian_canonical_reading, theater_ratio, 450, 0.4).
narrative_ontology:measurement(chr_canon_tr_t452, christian_canonical_reading, theater_ratio, 452, 0.35).
narrative_ontology:measurement(chr_canon_tr_t463, christian_canonical_reading, theater_ratio, 463, 0.35).

% Extraction over time
narrative_ontology:measurement(chr_canon_be_t0, christian_canonical_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(chr_canon_be_t200, christian_canonical_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(chr_canon_be_t350, christian_canonical_reading, base_extractiveness, 350, 0.52).
narrative_ontology:measurement(chr_canon_be_t400, christian_canonical_reading, base_extractiveness, 400, 0.54).
narrative_ontology:measurement(chr_canon_be_t430, christian_canonical_reading, base_extractiveness, 430, 0.5).
narrative_ontology:measurement(chr_canon_be_t450, christian_canonical_reading, base_extractiveness, 450, 0.48).
narrative_ontology:measurement(chr_canon_be_t463, christian_canonical_reading, base_extractiveness, 463, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(chr_canon_su_t0, christian_canonical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(chr_canon_su_t200, christian_canonical_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(chr_canon_su_t350, christian_canonical_reading, suppression_requirement, 350, 0.68).
narrative_ontology:measurement(chr_canon_su_t400, christian_canonical_reading, suppression_requirement, 400, 0.64).
narrative_ontology:measurement(chr_canon_su_t430, christian_canonical_reading, suppression_requirement, 430, 0.62).
narrative_ontology:measurement(chr_canon_su_t450, christian_canonical_reading, suppression_requirement, 450, 0.62).
narrative_ontology:measurement(chr_canon_su_t463, christian_canonical_reading, suppression_requirement, 463, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(christian_canonical_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(christian_canonical_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(christian_canonical_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the family_law_authority kernel. The readings are structurally distinct constraints (different epsilon values, different beneficiary/victim sets) linked through the shared kernel. The Christian canonical reading has higher extraction than the secular contractual reading (state authority with civil divorce) and lower extraction than the Hindu Dharmashastra reading (caste endogamy enforcement) or Parsi Zoroastrian reading (excommunication for interfaith marriage). Network edges represent the fact that these readings are in active contestation in pluralistic legal systems: Indian Special Marriage Act creates secular pathway alongside religious personal laws; European secular family law supersedes ecclesiastical jurisdiction; interfaith couples navigate multiple readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
