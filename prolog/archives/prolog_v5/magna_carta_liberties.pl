% ============================================================================
% CONSTRAINT STORY: magna_carta_liberties
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_liberties, []).

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
 *   constraint_id: magna_carta_liberties
 *   human_readable: The Great Charter of Liberties (Magna Carta, 1215)
 *   domain: political/legal
 *
 * SUMMARY:
 *   Magna Carta (1215) is a peace treaty between King John and a coalition of
 *   rebellious English barons, crystallizing an alternative to unlimited
 *   royal prerogative. The charter is a structural hybrid: it functions as
 *   coordination mechanism (standardizing feudal obligations, creating
 *   predictable legal procedures) while simultaneously formalizing extraction
 *   (baronial courts retain toll authority, merchant guilds gain monopoly
 *   protections, ecclesiastical hierarchy gains tax immunity). The
 *   classification varies sharply by observer position. The peasant majority,
 *   excluded from all protections and bound by reinforced serfdom clauses,
 *   experiences Magna Carta as a snare — a formalization of their
 *   subordination under the guise of universal law. The baronial coalition
 *   experiences it as coordination (solving their collective action problem
 *   against kingship). The crown experiences it as constraint (loss of
 *   prerogative) mixed with coordination (predictable feudal obligations).
 *   The analytical observer over centuries experiences it as piton — the
 *   charter's functional content (feudal property protection, baronial
 *   privilege) gradually atrophies as serfdom declines and franchise expands,
 *   but the mythic reading ('universal rights,' 'rule of law') persists,
 *   reissued 63 times with reinterpretations that would shock the original
 *   drafters. The theater ratio rises across the interval as the charter
 *   becomes a symbol deployed in later conflicts (Peasant Revolt of 1381,
 *   English Civil War, American Revolution) for purposes the original text
 *   does not support. The extractiveness declines modestly as baronial
 *   enforcement power wanes and the feudal system itself degrades, but
 *   suppression remains high throughout — the constraint's enforcement
 *   mechanism relies on baronial military power and royal legitimacy, both of
 *   which depend on preventing alternatives (peasant organization, church
 *   independence, merchant confederations) from crystallizing.
 *
 * KEY AGENTS:
 *   - King John: Institutional actor (institutional/trapped initially, arbitrage post-treaty) — subject to baronial coalition but retains enforcement power and prerogative definition
 *   - Baronial Coalition: Organized actors (organized/constrained) — primary beneficiaries; solve their collective action problem against arbitrary kingship; extract standardized rents and legal protections
 *   - Peasant and Serf Population: Powerless actors (powerless/trapped) — explicitly excluded from charter protections; experience reinforced feudal bonds; no exit option
 *   - Merchant Cities and Guilds: Moderate power (moderate/constrained) — benefit from standardized weights, measures, and toll reductions; constrained by feudal tenure obligations and monopoly requirements
 *   - Church Hierarchy: Institutional actors (institutional/arbitrage) — gain protection from royal seizure and interdict threat; experience charter as sunset mechanism resolving immediate crisis
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the charter's function gradually degraded and mythologized as feudal system decays
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_liberties, 0.38).
domain_priors:suppression_score(magna_carta_liberties, 0.68).
domain_priors:theater_ratio(magna_carta_liberties, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_liberties, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_liberties, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(magna_carta_liberties, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_liberties, tangled_rope).
narrative_ontology:human_readable(magna_carta_liberties, "The Great Charter of Liberties (Magna Carta, 1215)").
narrative_ontology:topic_domain(magna_carta_liberties, "political/legal").

domain_priors:requires_active_enforcement(magna_carta_liberties).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_liberties, baronial_elite).
narrative_ontology:constraint_beneficiary(magna_carta_liberties, merchant_cities).
narrative_ontology:constraint_beneficiary(magna_carta_liberties, church_hierarchy).
narrative_ontology:constraint_victim(magna_carta_liberties, royal_absolutism_constraints).
narrative_ontology:constraint_victim(magna_carta_liberties, peasant_and_serf_population).
narrative_ontology:constraint_victim(magna_carta_liberties, common_law_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANT AND SERF (SNARE) — Magna Carta offers elaborate protections for baronial property and merchant trading rights but explicitly excludes the unfree majority. Serfdom remains legally enforced. The charter is a snare because it crystallizes the feudal extraction hierarchy while creating the illusion of universal legal protections. No exit option; the constraint formalizes feudal bonds.
constraint_indexing:constraint_classification(magna_carta_liberties, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLING FREEHOLDERS AND MERCHANTS (TANGLED ROPE) — Magna Carta provides genuine coordination benefits (standardized weights/measures, removal of arbitrary tolls, stable inheritance rights) alongside modest extraction (baronial courts still tax justice). Exit is constrained by feudal tenure obligations, but the charter creates enforceable rights against arbitrary seizure. Mixed coordination and extraction.
constraint_indexing:constraint_classification(magna_carta_liberties, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BARONIAL COALITION (ROPE) — The immediate beneficiaries. Magna Carta is a coordination mechanism solving the collective action problem of resistance to arbitrary royal prerogative. The barons extract a constraint on kingship itself, converting undefined feudal obligations into specified rents and services. Low overall extraction (for these agents) because the mechanism redistributes power among near-peers.
constraint_indexing:constraint_classification(magna_carta_liberties, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ROYAL CROWN (TANGLED ROPE) — King John is both victim (restrained from arbitrary taxation and military prerogative) and enforcer (Magna Carta requires royal seal and compliance machinery). The crown experiences extraction (loss of absolute prerogative) but also gains coordination benefit (predictable feudal obligations replace arbitrary demands). The charter binds the king to law, converting indefinite extraction capacity into defined rents.
constraint_indexing:constraint_classification(magna_carta_liberties, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE CHURCH AUTHORITY (SCAFFOLD) — The charter explicitly protects church liberties and property from royal seizure. For the ecclesiastical hierarchy, Magna Carta is a sunset mechanism: it solves the immediate crisis (the pope had placed England under interdict; the church needed the charter to end the conflict) while creating a framework for transition from direct royal coercion to negotiated feudal order. The church exits the immediate threat and gains contractual security.
constraint_indexing:constraint_classification(magna_carta_liberties, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL INERTIA VIEW (PITON) — Over centuries, Magna Carta becomes a theatrical symbol of 'English liberties' while the actual feudal extraction it codified gradually atrophies (serfdom declines, peasant revolts succeed, franchise expands). The charter persists as performative cover for institutional change — reissued 63 times with modifications, cited to legitimize reforms it did not originally contain (parliamentary supremacy, religious freedom, representative consent). Theater ratio rises as the original feudal specificity fades and the mythic 'universal rights' reading replaces historical analysis.
constraint_indexing:constraint_classification(magna_carta_liberties, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_liberties_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_liberties, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_liberties, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_liberties, TR),
    TR >= 0.70.

:- end_tests(magna_carta_liberties_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Magna Carta codifies extraction of feudal obligations, toll authority, and property protections into standardized (rather than arbitrary) forms. The reduction from theoretical royal prerogative to specified rents is extraction for the crown but coordination benefit for the barons. The overall value reflects that the charter redistributes power among hierarchical actors (king, barons, church) without directly extracting from the powerless majority — instead, it formalizes their subordination. Suppression (0.68): High. The constraint's enforcement relies entirely on baronial military coalition and royal legitimacy. Alternatives (peasant organization, church autonomy, merchant confederations operating outside feudal hierarchy) are systematically suppressed by force and legal prohibition. The charter itself criminalizes oath-breaking and unauthorized assembly, strengthening the suppression mechanism. Theater ratio (0.65): Moderate-high. The charter begins with specific feudal content (widow remarriage tax, forest hunting rights, Jewish creditor limitations) but is quickly reissued with modified language and broader claims. By the 13th century, the charter is cited as evidence of 'English liberties' far beyond its actual clauses. The performative reading accelerates as the feudal system degrades — the charter persists as symbol precisely because its original function (feudal obligation codification) is no longer operative. Later reissuances (1225, 1297) drop specific feudal language and add more 'universal' framing, increasing theater as the material basis of feudal extraction dissolves.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence from identical base properties. The baronial coalition sees coordination (rope) — they are solving their collective action problem against arbitrary kingship by converting undefined feudal obligations into specified rents and services. The crown sees constraint (tangled rope) — the king is both restrained (loss of prerogative) and gains coordination benefit (predictable obligations replace arbitrary demands). The church sees temporary crisis resolution (scaffold) — the interdict threat ends; the church gains property protection; the mechanism has a sunset in ecclesiastical independence. The peasant sees pure extraction (snare) — the charter explicitly reinforces serfdom, formalizes forced labor, and provides no protection. The middling freeholders see mixed coordination and extraction (tangled rope) — they gain standardized legal procedures and protection from arbitrary seizure, but still owe feudal obligations and face baronial court authority. The analytical observer across centuries sees institutional degradation (piton) — the charter's feudal content gradually becomes inert; the mythic 'universal rights' reading replaces historical analysis; the constraint persists through reissuance and ceremonial citation long after its functional basis atrophies. The perspectival gap is driven by directionality: beneficiaries (barons, church, merchants with property) experience low or negative extraction; the powerless peasant majority experiences high extraction; the crown straddles both sides of the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chains from beneficiary/victim declarations and exit options. The baronial coalition benefits from the charter (low d → low χ, classified as rope); the crown loses prerogative but gains coordination (moderate d, classified as tangled rope for the crown's own perspective); the church benefits and exits the crisis (low d with scaffold dynamics); the peasant majority bears cost and has no exit (high d → high χ, classified as snare). The analytical observer operates at (analytical/analytical) with universal scope, applying civilizational time horizon — this produces a d value (≈0.72-0.73) that would normally indicate snare or tangled rope, but the theater ratio (0.65) and the decaying functional content across the interval produces piton classification. The overrides reflect that the crown's position is genuinely mixed — it is both enforcer and subject of the constraint — and the straightforward derivation would mislabel this. The crown perspective includes an override documenting that while the crown nominally bears cost (high d from victim framing), it also retains effective enforcement power and can renegotiate the constraint after each baronial coalition collapses (arbitrage exit option), producing moderate d rather than high d.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by distinguishing the genuine coordination function (baronial coalition solving collective action) from the pure extraction (peasant subordination formalized). The charter IS both — it is a coordination mechanism for the baronial coalition that solves their coordination problem against arbitrary kingship. Simultaneously, it IS a snare for the peasant majority, who experience only formalized extraction. The mandatrophy is not resolved by collapsing the types but by recognizing that the charter is a DUAL STRUCTURE: two separate constraints operating on different populations through the same formal document. From the baronial/crown perspective, Magna Carta solves a genuine coordination problem (how to convert arbitrary feudal obligations into predictable, enforceable rents) — this is rope/tangled rope. From the peasant perspective, Magna Carta formalizes subordination while creating the illusion of universal protections — this is snare. The analytical observer's piton classification emerges from the long-term degradation: the original dual structure (coordination for elites, formalization for peasants) gradually decays as feudal extraction itself declines and becomes vestigial, leaving the charter as a reissued symbol whose mythic content (universal liberties) has entirely replaced its historical content (feudal obligation codification). The 63 reissuances, each with modified language and broader claims, are the primary evidence that the charter's original function is atrophied and the constraint persists through institutional inertia and ceremonial citation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_intent_vs_mythology,
    'Is Magna Carta primarily a feudal peace treaty codifying existing baronial privileges, or was it a proto-constitutional statement of universal legal rights?',
    'Close reading of the 63 surviving copies and reissues; comparison of original clauses (feudal specifics: widow remarriage taxes, forest usufruct, Jewish loan cancellation) against later reinterpretations (Coke''s 1628 Petition of Right, American constitutional borrowing)',
    'If feudal: constraint is a snare from peasant perspective, rope from baronial perspective, piton from centuries-long analytical view. If proto-constitutional: constraint is rope or scaffold from broader perspectives. The classification hinge depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_intent_vs_mythology, empirical, 'Whether Magna Carta encodes feudal privileges or universal rights').

omega_variable(
    extraction_beneficiary_identification,
    'Who are the primary extractors from Magna Carta''s codification: the baronial class consolidating power against kingship, or the crown retaining effective prerogative despite nominal constraint?',
    'Fiscal analysis of royal revenue post-1215 vs. pre-1215; data on successful baronial enforcement of charter clauses; frequency of reissue as evidence of non-compliance',
    'If barons extract: charter is rope from their perspective, snare from peasant perspective. If crown retains effective prerogative: charter is theater (piton) — the crown renegotiates terms after each rebellion and the constraint is inert. This determines whether beneficiary is baronial_elite or whether the classification degrades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_identification, empirical, 'Who successfully extracts value from Magna Carta''s codification').

omega_variable(
    peasant_exclusion_intentionality,
    'Is the exclusion of serfs and unfree persons from Magna Carta''s protections a deliberate restriction to feudal property-holders, or a limitation of contemporary legal categories?',
    'Textual analysis of charter language; comparison with contemporary legal documents; evidence of deliberate baronial resistance to broader liberties language',
    'If deliberate: constraint is a snare by design, formalizing feudal hierarchy. If categorical limitation: constraint still functions as snare from peasant perspective (same outcome, different intent). Either way, the snare classification holds; the question affects narrative interpretation but not structural classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peasant_exclusion_intentionality, conceptual, 'Whether peasant exclusion from Magna Carta protections is intentional restriction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_liberties, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_tr_t0, magna_carta_liberties, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mc_tr_t30, magna_carta_liberties, theater_ratio, 30, 0.5).
narrative_ontology:measurement(mc_tr_t60, magna_carta_liberties, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(mc_be_t0, magna_carta_liberties, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mc_be_t30, magna_carta_liberties, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(mc_be_t60, magna_carta_liberties, base_extractiveness, 60, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_liberties, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_liberties, feudal_obligation_codification).
narrative_ontology:affects_constraint(magna_carta_liberties, english_common_law_development).
narrative_ontology:affects_constraint(magna_carta_liberties, serfdom_legal_reinforcement).

% DUAL FORMULATION NOTE:
% Magna Carta is a constraint family consisting of three structurally distinct constraints: (1) baronial_coalition_coordination (ε=0.12, rope) — solves the barons' collective action problem, (2) feudal_extraction_formalization (ε=0.42, tangled rope) — codifies and standardizes extraction from the peasant majority while also constraining the crown, (3) magna_carta_mythology (ε=0.65, piton) — emerges over centuries as the original feudal content decays and the mythic 'universal liberties' reading replaces historical analysis. These three constraints share the formal document but operate on different populations through different mechanisms. The main story (magna_carta_liberties) integrates all three through perspectival indexing; the network links capture upstream dependency (the charter depends on feudal obligation codification existing as a constraint) and downstream influence (the charter's mythology drives later constraints like parliamentary supremacy and representative consent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_liberties, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
