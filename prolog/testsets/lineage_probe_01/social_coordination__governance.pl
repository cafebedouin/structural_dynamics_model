% ============================================================================
% CONSTRAINT STORY: social_coordination__governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_coordination__governance, []).

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
 *   constraint_id: social_coordination__governance
 *   human_readable: Governance Authority as Large-Scale Coordination Mechanism
 *   domain: political/social
 *
 * SUMMARY:
 *   Large-scale social coordination through authoritative institutions
 *   represents one contested reading of the social_coordination kernel. This
 *   reading claims that centralizing authority to bind members and adjudicate
 *   disputes is necessary for coordination at population scale. The
 *   constraint exhibits the core tangled_rope signature: genuine coordination
 *   function (resolving conflicts, provisioning public goods, enabling
 *   commerce through property law) paired with asymmetric extraction
 *   (taxation, conscription, enforcement of hierarchy). The extractiveness
 *   has accumulated over the interval (0.32 → 0.48) as bureaucratic apparatus
 *   has grown; suppression has intensified (0.35 → 0.58) as alternative
 *   coordination modes have been systematically subordinated to state
 *   authority; theater has increased slightly (0.38 → 0.52) as the
 *   legitimation ritual has become more elaborate relative to actual
 *   adjudication function. This reading competes with three structural
 *   alternatives: kinship_obligation (coordination through inherited
 *   reciprocity), market_exchange (coordination through voluntary exchange
 *   under price), and ritual_consensus (coordination through sacralized
 *   collective choice). The governance reading forecloses none of them
 *   logically — different populations have coordinated at scale through each
 *   mechanism — but it does influence them by establishing legal hierarchies
 *   that rank governance authority above rival modes. The analytical observer
 *   risks a false summit (mountain classification) by naturalizing governance
 *   authority as immutable at civilizational scale, when historical and
 *   contemporary evidence shows alternative coordination modes can scale if
 *   authority's enforcement burden is reduced.
 *
 * KEY AGENTS:
 *   - Office Holders: Primary beneficiary (institutional/arbitrage) — capture revenue, authority, and prestige through institutional position. Depend on governance structure for their power; arbitrage between formal authority and informal networks (graft, nepotism).
 *   - Institutionalized Access Elite: Secondary beneficiary (institutional/arbitrage) — professionals, merchants, clergy with formal standing; benefit from rule of law, predictable adjudication, and market coordination enabled by governance structure.
 *   - Politically Marginal: Primary victim (powerless/trapped) — unable to exit jurisdiction; excluded from formal institutional access; suppressed from using alternative coordination modes (kinship courts, religious adjudication, voluntary assembly); bear taxation and conscription with minimal input.
 *   - Non-Institutionalized Coordination Modes: Structural victim — kinship law, merchant guilds, religious authority, local consensus mechanisms are subordinated to state law and delegitimized as official authority.
 *   - Ordinary Subject: Mixed experience (moderate/constrained) — benefits from conflict resolution and public goods, but constrained by fixed borders and subordination to hierarchical rules.
 *   - Administrative Bureaucracy: Institutional actor maintaining itself (institutional/arbitrage) — the apparatus has become self-perpetuating; theater of legitimacy persists partly via inertia.
 *   - Transitional Reform Movement: Organized agent seeking alternative (organized/constrained) — civil society, democratic movements working toward distributed governance or subsidiarity-principle alternatives.
 *   - Analytical Observer: Risks naturalizing contingent choice (analytical/analytical) — treats governance authority as inevitable at civilizational scale; obscures the contested kernel structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_coordination__governance, 0.48).
domain_priors:suppression_score(social_coordination__governance, 0.58).
domain_priors:theater_ratio(social_coordination__governance, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_coordination__governance, extractiveness, 0.48).
narrative_ontology:constraint_metric(social_coordination__governance, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(social_coordination__governance, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_coordination__governance, tangled_rope).
narrative_ontology:human_readable(social_coordination__governance, "Governance Authority as Large-Scale Coordination Mechanism").
narrative_ontology:topic_domain(social_coordination__governance, "political/social").

domain_priors:requires_active_enforcement(social_coordination__governance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(social_coordination__governance, '97ce57ef-51fd-47e9-a83f-2d4a800a35a4').
narrative_ontology:cs_kernel_codification('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', formalized).
narrative_ontology:cs_authority_grounding('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', extraction).
narrative_ontology:cs_interpretation_layer_present('97ce57ef-51fd-47e9-a83f-2d4a800a35a4').
narrative_ontology:cs_reading_relation('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', social_coordination__kinship_obligation, coexists_with).
narrative_ontology:cs_reading_relation('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', social_coordination__market_exchange, coexists_with).
narrative_ontology:cs_reading_relation('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', social_coordination__ritual_consensus, coexists_with).
narrative_ontology:cs_axiom('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', foundational, authoritative_institution_necessity).
narrative_ontology:cs_axiom_status(authoritative_institution_necessity, holdable).
narrative_ontology:cs_axiom_grounding('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', authoritative_institution_necessity, empirically_contingent).
narrative_ontology:cs_axiom('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', foundational, hierarchy_efficiency_at_scale).
narrative_ontology:cs_axiom_status(hierarchy_efficiency_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', hierarchy_efficiency_at_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', centralized_authority_legitimacy).
narrative_ontology:cs_drift_state('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', contemporary_post_industrial, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('97ce57ef-51fd-47e9-a83f-2d4a800a35a4', '').
narrative_ontology:cs_kernel_id(social_coordination__governance, social_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_coordination__governance, office_holders).
narrative_ontology:constraint_beneficiary(social_coordination__governance, institutional_access_elite).
narrative_ontology:constraint_victim(social_coordination__governance, politically_marginal).
narrative_ontology:constraint_victim(social_coordination__governance, non_institutionalized_coordination_modes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICALLY MARGINAL — Cannot exit governance structures (territorial residence, legal jurisdiction, birth status confer involuntary membership). Suppressed from using alternative coordination modes (kinship networks, voluntary exchange, ritual consensus) via criminal law, administrative penalty, or institutional exclusion. Bears costs of compliance (taxation, labor obligation, dispute resolution loss) while receiving minimal benefit of dispute adjudication. High experienced extraction.
constraint_indexing:constraint_classification(social_coordination__governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORDINARY SUBJECT — Constrained exit (migration costly, renunciation of citizenship difficult). Benefits from governance as a coordination mechanism: conflict resolution that prevents blood feuds, property law enabling commerce, public goods provisioning. Also bears extraction: taxation, forced conscription, subordination to bureaucratic rules. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(social_coordination__governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OFFICE HOLDER — Primary beneficiary. Achieves power, revenue extraction, and authority through institutional standing. Views governance constraint as coordination mechanism that solves the collective action problem of large-scale coordination without private extraction cost. Can arbitrage between formal authority and informal networks (nepotism, graft, rent-extraction). Experiences constraint as legitimate coordination structure that serves them.
constraint_indexing:constraint_classification(social_coordination__governance, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADMINISTRATIVE BUREAUCRACY — Institutional actor that has become the performance of governance. Procedural compliance, rule-following, and ritual of legitimacy have partly decoupled from actual coordination function. Theater of law and order persists (ceremonies, documentation, hierarchy) even when coordination could be achieved through simpler mechanisms. Bureaucracy maintains itself through institutional inertia and the belief that formal procedures confer legitimacy.
constraint_indexing:constraint_classification(social_coordination__governance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSITIONAL GOVERNANCE REFORM — Organized actors (civil society, reform movements, democratic activists) see authoritative governance as a temporary coordination mechanism that can sunset into distributed consensus structures or subsidiary-principle federalism. Sunset logic: as education, communication infrastructure, and civic participation deepen, need for top-down authority diminishes. Alternative coordination modes (deliberative democracy, participatory budgeting, local consensus) can scale if enforcement burden is reduced. Low effective extraction because the movement has agency and sees an exit path.
constraint_indexing:constraint_classification(social_coordination__governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, large-scale coordination without centralized authority is treated as impossible or prohibitively costly. Hobbesian framing: without a sovereign to bind all members, coordination devolves to kinship, tribe, or anarchy. This perspective treats authority structures as an immutable natural law, an irreducible feature of the human condition at population scale. However, this risks false summitry — treating a specific institutional choice as inevitable when alternative coordination modes have historically scaled and could scale again.
constraint_indexing:constraint_classification(social_coordination__governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_coordination__governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_coordination__governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_coordination__governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_coordination__governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_coordination__governance, TR),
    TR >= 0.70.

:- end_tests(social_coordination__governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. Governance structures do extract substantial resources (taxation, conscription, property control, labor obligation), but the extraction is not maximal because genuine coordination benefits are real — contract enforcement, conflict resolution, public goods provisioning, and infrastructure enable commerce and reduce violence. The 0.48 value reflects this hybrid: meaningful coordination function alongside asymmetric extraction. Suppression (0.58): Moderate-high. Rival coordination modes are substantially suppressed — kinship law is denied official standing, merchant guilds are subordinated to state commerce regulation, religious authority is restricted to spiritual domains, local consensus is overridden by hierarchy. However, suppression is not total in most modern states — kinship networks persist informally, merchant codes operate parallel to commercial law, religious adjudication continues in some contexts. The 0.58 reflects the systematic legal subordination of alternatives. Theater ratio (0.52): Moderate. Governance legitimacy is partly performative (ceremonies, proclamations, formal procedures) but also functional (adjudication does resolve disputes, law does coordinate behavior). The theater component has increased over the interval as states have developed more elaborate bureaucratic ritual. The interval measurements (0 to 400) represent roughly 400 years of institutional development in Europe and its colonial/post-colonial extensions. Over this period, extractiveness has risen as tax bureaucracy developed, suppression has intensified as alternative coordination modes were systematically delegitimized, and theater has increased as legitimate procedure became more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The governance reading creates a wide perspectival gap. The office holder sees legitimate coordination (Rope) — the constraint solves the collective action problem of binding large populations. The ordinary subject sees mixed coordination and extraction (Tangled Rope) — real benefits but real costs. The politically marginal sees pure extraction (Snare) — suppressed from alternatives, forced to comply, minimal benefit. The bureaucracy sees its own degradation (Piton) — procedure has become largely self-perpetuating ritual. The reform movement sees a temporary apparatus with alternatives emerging (Scaffold) — distributed deliberation and subsidiarity could replace centralized authority. The analytical observer risks seeing immutable natural law (Mountain) — treating governance authority as inherent to large-scale coordination rather than one contingent institutional form among alternative reading options. This gap reflects the genuine contest between the governance reading and its siblings (kinship, market, ritual) — different population structures, communication technologies, and institutional developments have favored different coordinate modes at different scales and times.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the extraction flow and exit capacity. Office holders (beneficiary + arbitrage) experience low or negative effective extraction — the constraint exists to serve them. Politically marginal (victim + trapped) experience maximum extraction — they cannot exit and are excluded from institutional voice. Ordinary subjects (mixed + constrained) experience moderate extraction — they benefit from coordination but pay costs. The piton perspective (bureaucracy maintaining itself) emerges from theater_ratio exceeding functional necessity; the scaffold perspective (reform movement with sunset vision) emerges from organized agents with exit paths and alternative models. The mountain perspective (analytical/natural law) risks false summitry by naturalizing what is revealed as a contested kernel reading once the alternative modes are examined.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_coordination_feasibility,
    'Can non-authoritative coordination modes (kinship obligation, market exchange, ritual consensus, digital consensus) actually scale to state or continental population sizes, or is top-down authority inherently necessary above a threshold population?',
    'Historical and contemporary case analysis: Swiss cantons, Icelandic Commonwealth (pre-1262), Acephalous societies (Ibibio, Nuer, Somali), digital consensus systems (blockchain governance, online deliberation at scale). Threshold analysis: at what population does distributed coordination provably fail?',
    'If alternative coordination scales: governance constraint is contingent institutional choice (Tangled Rope from most perspectives). If scaling fails above threshold: governance is closer to immutable (Mountain may be appropriate). If scaling succeeds at some scales but not others: constraint is scope-dependent; separate stories per scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Feasibility and scale limits of non-authoritative coordination modes').

omega_variable(
    reading_kernel_ambiguity,
    'Is this reading one interpretation of an enduring kernel of ''social coordination'' (the kernel remains stable; readings differ), or do the four sibling readings (governance, kinship, market, ritual) actually refer to four distinct kernels that are wrongly grouped under a single label?',
    'Philosophical reconstruction: identify the transhistorical commitment that all four readings share. If no shared commitment exists — if ''kinship obligation'' and ''governance authority'' address structurally incompatible coordination problems — then the kernel is misnamed and the readings are separate constraints.',
    'If single kernel with multiple readings: this story''s cs_structure, reading_relations, and axioms capture the structure correctly. If four separate kernels: this story should be de-linked from siblings; each should declare its own network relationships independently. Affects the entire kernel context frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether four sibling readings share a single kernel or represent separate kernels mislabeled').

omega_variable(
    extraction_mechanism_ambiguity,
    'Is the measured extractiveness (0.48) a genuine feature of authoritative governance, or does it reflect the specific institutional *form* (hierarchy, centralization, bureaucracy) rather than an inherent property of large-scale coordination itself?',
    'Comparative institutional analysis: measure extractiveness of governance structures with different organizational forms (centralized state, federal subsidiarity, multi-tiered delegation, distributed adjudication). If extractiveness correlates with centralization rather than scale, the constraint should be decomposed into (a) large-scale coordination (separate constraint, lower extractiveness), and (b) centralized authority mechanism (separate constraint, higher extractiveness).',
    'If extracted via form, not via scale: ε for ''governance as coordination'' is lower (~0.30); ε for ''centralized authority mechanism'' is higher (~0.60). Decomposition creates separate stories per ε-invariance principle. If extracted inherently at scale: this constraint is correctly classified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Whether extractiveness is inherent to large-scale coordination or attributable to specific institutional form').

omega_variable(
    suppression_subordination_distinction,
    'Is the measured suppression (0.58) an active suppression mechanism (formal prohibition of rival coordination modes), or is it subordination (rival modes permitted but delegitimized / ranked below authority)?',
    'Historical-legal analysis: documentation of formal prohibition vs. hierarchical ranking. Compare societies that actively criminalize kinship-law adjudication vs. those that permit it but rank it below state law. Suppress = criminal penalty; subordinate = legal standing but lower priority. The distinction affects the snare classification for the powerless agent.',
    'If active suppression: measured suppression ≥ 0.58 is accurate; snare classification is robust. If subordination only: suppression should be ~0.40 (lower intensity); powerless agent may experience tangled_rope rather than snare. Affects multiple perspective classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_subordination_distinction, empirical, 'Whether rival coordination modes are actively suppressed or hierarchically subordinated').

omega_variable(
    reading_naturalness_contest,
    'This reading instantiates the governance authority frame, one of four competing readings of the social_coordination kernel. That contest is real, persistent, and unresolved — it is the structure the committer frame is meant to capture. What is the epistemic status of treating this reading as a single constraint rather than an inherently contested site?',
    'Metatheoretical: the Deferential Realism framework''s architectural commitment to kernel/reading structure is designed to handle exactly this case. A contested kernel is a single contested commitment (the social_coordination claim); the readings differ in how that commitment is interpreted. Treating the governance reading as a single constraint is correct IF it represents one coherent interpretation (Axiom: ''Authoritative institutions provide necessary large-scale coordination''). Treating it as inherently fragmented would require abandoning the kernel structure.',
    'If this reading is inherently fragmented: no single constraint story can capture it; the kernel framework fails on this case. If the reading is coherent: this story is correctly authored. The question is philosophical, not empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_naturalness_contest, conceptual, 'Epistemic status of treating the governance reading as a single constraint given its contested nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_coordination__governance, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(socgov_tr_t0, social_coordination__governance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(socgov_tr_t200, social_coordination__governance, theater_ratio, 200, 0.46).
narrative_ontology:measurement(socgov_tr_t400, social_coordination__governance, theater_ratio, 400, 0.52).

% Extraction over time
narrative_ontology:measurement(socgov_be_t0, social_coordination__governance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(socgov_be_t200, social_coordination__governance, base_extractiveness, 200, 0.41).
narrative_ontology:measurement(socgov_be_t400, social_coordination__governance, base_extractiveness, 400, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(socgov_su_t0, social_coordination__governance, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(socgov_su_t200, social_coordination__governance, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(socgov_su_t400, social_coordination__governance, suppression_requirement, 400, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_coordination__governance, enforcement_mechanism).
narrative_ontology:affects_constraint(social_coordination__governance, social_coordination__kinship_obligation).
narrative_ontology:affects_constraint(social_coordination__governance, social_coordination__market_exchange).
narrative_ontology:affects_constraint(social_coordination__governance, social_coordination__ritual_consensus).
narrative_ontology:affects_constraint(social_coordination__governance, bureaucratic_legitimacy_theater).
narrative_ontology:affects_constraint(social_coordination__governance, state_monopoly_on_force).

% DUAL FORMULATION NOTE:
% This is the governance reading of the social_coordination kernel. The upstream kernel is the contested claim about how large-scale coordination is achieved; this story instantiates one interpretation (authority-based). The downstream constraints are specific mechanisms (bureaucratic theater, state force monopoly) that the governance reading depends on. The kinship, market, and ritual siblings are sibling readings of the same kernel, not derivative constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
