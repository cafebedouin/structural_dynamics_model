% ============================================================================
% CONSTRAINT STORY: preah_vihear_territorial_claim
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preah_vihear_territorial_claim, []).

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
 *   constraint_id: preah_vihear_territorial_claim
 *   human_readable: Preah Vihear Territorial Claim and Border Dispute Resolution
 *   domain: geopolitical/territorial_sovereignty
 *
 * SUMMARY:
 *   The Preah Vihear territorial dispute between Thailand and Cambodia
 *   originates in colonial-era boundary ambiguity (French Indochina
 *   demarcation) and has evolved into a structurally mixed constraint
 *   coupling genuine coordination problems (border clarity, international law
 *   precedent) with extraction mechanisms (nationalist mobilization, military
 *   resource capture, population displacement). The 2008 International Court
 *   of Justice ruling nominally resolved the legal claim but has not arrested
 *   the underlying dispute — both states continue military posturing,
 *   periodic armed clashes (2008-2011), and nationalist narratives despite
 *   the judgment. This reveals the constraint's true nature: the legal
 *   question (Who owns Preah Vihear temple?) was coordinable; the underlying
 *   extraction mechanism (using territorial nationalism to mobilize domestic
 *   identity and justify military budgets) is not. The constraint exhibits
 *   all structural properties of Tangled Rope: a genuine coordination
 *   function (international law, border demarcation precedent) coexists with
 *   asymmetric extraction (nationalist coalitions benefit from claim
 *   mobilization while border populations bear costs). The theater ratio
 *   (0.64) reflects high performative content: diplomatic negotiations, UN
 *   statements, ICJ proceedings, ASEAN mediation statements, and nationalist
 *   rhetoric perform the appearance of dispute resolution while the
 *   underlying resource extraction (military spending, population
 *   displacement, identity consolidation) persists. Extractiveness has
 *   increased from 0.32 (when the dispute was primarily a legal question) to
 *   0.58 (current state where legal resolution has not arrested
 *   political-military extraction). Theater has similarly risen as more
 *   institutional actors (ICJ, ASEAN, UN, bilateral negotiations) have
 *   invested performative effort into a dispute that the underlying
 *   nationalist incentives keep alive.
 *
 * KEY AGENTS:
 *   - Border Populations (displaced): Primary victims (powerless/trapped) — lose property, livelihood, and security to military escalation cycles with no exit options or representation
 *   - Thai Nationalist Coalition (government, military, conservative civil society): Primary beneficiary-extractor (organized/constrained) — mobilizes territorial claim for identity consolidation and military budget justification; constrained by ICJ ruling and international opposition
 *   - Cambodian Nationalist Coalition (government, military, civil society): Primary beneficiary-extractor (organized/constrained) — mobilizes counter-claim for identity and military assertion; constrained by military inferiority and legal precedent
 *   - Military Institutional Actors (Thai and Cambodian armed forces): Secondary beneficiaries — benefit from budget allocation, capability expansion, and operational justification; constrained by international peacekeeping norms
 *   - ASEAN Regional Mediators (Indonesia, Malaysia, Vietnam, others): Moderate agents (moderate/constrained) — have genuine coordination interest (regional stability) but constrained by non-interference doctrine and inability to override nationalist coalitions
 *   - International Court of Justice: Institutional beneficiary (institutional/arbitrage) — experiences constraint as coordination problem; benefits from precedent-setting authority; has arbitrage option (ruling was sufficient from legal perspective)
 *   - International Dispute Resolution System: Degraded institutional form (institutional/arbitrage) — maintains performative authority through ICJ, UN statements, bilateral negotiations despite minimal functional dispute resolution
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing nationalist territorial claims as immutable properties of sovereignty rather than contingent extraction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preah_vihear_territorial_claim, 0.58).
domain_priors:suppression_score(preah_vihear_territorial_claim, 0.68).
domain_priors:theater_ratio(preah_vihear_territorial_claim, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preah_vihear_territorial_claim, extractiveness, 0.58).
narrative_ontology:constraint_metric(preah_vihear_territorial_claim, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(preah_vihear_territorial_claim, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preah_vihear_territorial_claim, tangled_rope).
narrative_ontology:human_readable(preah_vihear_territorial_claim, "Preah Vihear Territorial Claim and Border Dispute Resolution").
narrative_ontology:topic_domain(preah_vihear_territorial_claim, "geopolitical/territorial_sovereignty").

domain_priors:requires_active_enforcement(preah_vihear_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preah_vihear_territorial_claim, thai_nationalist_coalition).
narrative_ontology:constraint_beneficiary(preah_vihear_territorial_claim, cambodian_nationalist_coalition).
narrative_ontology:constraint_beneficiary(preah_vihear_territorial_claim, military_institutional_actors).
narrative_ontology:constraint_victim(preah_vihear_territorial_claim, border_population_displaced).
narrative_ontology:constraint_victim(preah_vihear_territorial_claim, international_dispute_resolution_credibility).
narrative_ontology:constraint_victim(preah_vihear_territorial_claim, regional_peace_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BORDER POPULATIONS (SNARE) — Trapped by military escalation cycles and forced displacement. No exit options. Experience maximum extraction through loss of livelihood, property, and security. Cannot organize against either state's nationalist mobilization.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASEAN REGIONAL MEDIATORS (TANGLED ROPE) — Constrained by non-interference doctrine and consensus requirements, but also benefit from maintaining regional stability frameworks. Genuine coordination function (resolving dispute) mixed with extraction (both states resist mediation, forcing mediators to absorb reputational cost). Constrained exit: mediators cannot walk away without losing institutional credibility.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL COURT OF JUSTICE (ROPE) — Experiences the constraint as pure coordination: ICJ ruling (2008) established precedent for border demarcation. Net beneficiary from perspective of institutional prestige (precedent strengthens international law authority). Low extraction experienced because ICJ has arbitrage option (can refuse cases or limit enforcement). Coordination function is genuine — ruling resolved the nominal legal claim.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THAI NATIONALIST COALITION (TANGLED ROPE) — Benefits from territorial claim (coordinating national identity, resource assertion). But constrained by international law precedent (ICJ ruling) and military resource requirements for enforcement. Mixed function: coordinates domestic nationalist sentiment while extracting resources from state budget and border population acceptance. Constrained exit: cannot abandon claim without identity cost; cannot enforce without international opposition.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAMBODIAN NATIONALIST COALITION (TANGLED ROPE) — Symmetric structure to Thai coalition. Benefits from counter-claim (coordinating identity against Thai threat narrative). Constrained by military resource inferiority and ICJ ruling against some territorial assertions. Mixed extraction: mobilizes population identity while extracting resources for military buildout. Constrained exit: cannot abandon counter-claim without conceding sovereignty narrative.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL SYSTEM (PITON) — The ICJ ruling and international law apparatus represent a degraded institutional form. Theater ratio is high because the ruling (2008) has not resolved the dispute — both states continue military posturing, nationalist mobilization, and periodic escalations despite the legal judgment. International law performs authoritative judgment but lacks enforcement capacity. The system persists through institutional inertia (legitimacy of the ICJ process) despite minimal functional resolution. Theater_ratio reflects that legal precedent is maintained performatively while the underlying extraction continues.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, territorial sovereignty is treated as an immutable property: every state has inherent right to territorial integrity and border clarity. The constraint appears as a necessary feature of the international system — states must have boundaries, and disputes over ambiguous colonial demarcations are structurally inevitable. This perspective risks naturalizing what is actually a contingent extraction mechanism: nationalist mobilization that benefits political elites while imposing costs on border populations and peace infrastructure. FALSE SUMMIT CANDIDATE.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preah_vihear_territorial_claim_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preah_vihear_territorial_claim, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preah_vihear_territorial_claim, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(preah_vihear_territorial_claim, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(preah_vihear_territorial_claim, TR),
    TR >= 0.70.

:- end_tests(preah_vihear_territorial_claim_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint has evolved from a primarily legal coordination problem (ε ≈ 0.32 at origin, when French colonial ambiguity required clarification) to a political extraction mechanism (current ε = 0.58). The escalation reflects that nationalist mobilization extracts resources from both states' populations, displaces border communities, and diverts military resources from other uses. The extraction is not total (0.58 < 0.66 snare threshold) because significant coordination remains: the ICJ ruling does clarify international law, ASEAN mediation does provide alternative dispute pathways, and both states do maintain some level of restraint (hot wars punctuated by ceasefire periods rather than continuous conflict). Suppression (0.68): High. Measured through barriers to peaceful coexistence: nationalist narrative dominance, military mobilization, border closure, population displacement, and international law that legitimizes territorial claims without providing enforcement mechanisms. The suppression is active (requires military forces, propaganda, border control) rather than passive (constraints flow from structural immobility). Theater ratio (0.64): High and rising. The diplomatic apparatus (ICJ, ASEAN, bilateral talks, UN statements) produces a performative appearance of dispute resolution while the underlying nationalist extraction continues. Both states perform compliance with international law while maintaining military posturing. The theater reflects that the legal question has been settled (ICJ 2008) but the political extraction remains unsettled. Theater has risen from 0.42 (when the ICJ ruling was new and appeared potentially resolving) to 0.64 (current state, where multiple institutional actors perform dispute resolution efforts without arresting the underlying nationalist mobilization).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the full range of perspectival gap across structural positions. The Thai and Cambodian nationalist coalitions experience the constraint as mixed coordination-extraction (Tangled Rope) — they benefit from territorial claim mobilization while constrained by legal precedent and resource costs. The border populations experience pure extraction (Snare) — no benefit, maximum cost, no exit. ASEAN mediators experience coordination with extraction costs (Tangled Rope) — they genuinely coordinate regional stability but absorb reputational cost when the dispute persists. The ICJ experiences pure coordination (Rope) — the ruling resolved the legal question cleanly from the court's perspective. The international legal system (piton) experiences degraded institutional form — the ruling persists through performative authority despite minimal functional impact. The analytical observer risks mountain classification (territorial sovereignty as immutable) but the structural data reveals this as naturalization of contingent political choices. This perspectival gap is diagnostic: it reveals that the 'coordination' framing (dispute resolution, international law precedent) captures the genuine legal problem but misses the underlying extraction mechanism (nationalist mobilization, military resource capture). A pure coordination framing (Rope) would miss the extraction; a pure extraction framing (Snare) would miss the coordination. Tangled Rope is the accurate classification because both functions are genuinely present and genuinely mixed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position: their power level, exit options, and relationship to the extraction flow. Thai nationalist coalition benefits from the claim (low d, around 0.25-0.35 as organized beneficiary with constrained exit — they benefit but cannot fully walk away without identity cost). Cambodian coalition has similar directionality (beneficiary with constrained exit). Border populations are full targets with no exit (high d, around 0.90-0.95 as trapped victims). ASEAN mediators occupy the balanced position (d ≈ 0.50) — they benefit from regional stability but pay reputational costs when the dispute persists. ICJ experiences the constraint as beneficiary-neutral coordination (d ≈ 0.15 as institutional beneficiary with arbitrage exit — they can walk away from enforcement). The piton perspective derives d from the international legal system's structural position as maintainer of the performative apparatus (institutional beneficiary with arbitrage exit, d ≈ 0.20). The analytical observer has d ≈ 0.72 as observer position seeing the full extraction structure. The perspectival gap reveals the constraint's structure: agents with low d (beneficiaries with arbitrage options) classify as Rope; agents with high d (victims with trapped exit) classify as Snare; agents with mixed positions and genuine benefit-cost tradeoffs classify as Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The Preah Vihear dispute resolves mandatrophy through the Tangled Rope classification's dual-function structure. The coordination function is genuine: the ICJ ruling did establish precedent for resolving territorial claims through international law, and ASEAN mediation has created alternative dispute pathways. The extraction function is also genuine: nationalist mobilization extracts resources from populations, displacement extracts territory from border communities, military budgets extract fiscal resources, and the dispute persists despite legal resolution. Neither function dominates — the constraint is approximately balanced between coordination and extraction, making Tangled Rope structurally appropriate. The mandatrophy is resolved by recognizing that the 'dispute resolution' framing (Rope) would miss the nationalist extraction, while the 'nationalist extraction' framing (Snare) would miss the genuine international law coordination. Both are structural features of the same constraint. The constraint persists in the middle ground: extraction is high enough that legal resolution fails to arrest it; coordination is genuine enough that pure extraction models cannot fully explain why both states maintain restraint rather than continuous hot war. The theater ratio (0.64) further supports this: the constraint has genuine institutional functions (ICJ, ASEAN, bilateral talks) that perform real coordination work, but those functions operate at high theater overhead — the apparatus maintains itself through performative authority without fully arresting the underlying extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nationalist_mobilization_necessity,
    'Is the nationalist mobilization around the territorial claim a necessary feature of state legitimacy in the region, or a contingent choice by political elites?',
    'Historical analysis of periods when the claim was dormant vs. activated; correlation with domestic political instability, military budgeting cycles, and elite power consolidation; comparative analysis of other territorial claims in the region with different escalation patterns',
    'If necessary: the extraction mechanism is structurally unavoidable (constraint is closer to Mountain). If contingent: the mobilization is a political choice, and the constraint is purely Tangled Rope/Snare (extraction can be decoupled from coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationalist_mobilization_necessity, empirical, 'Whether nationalist mobilization is structurally necessary or politically contingent').

omega_variable(
    icj_enforcement_capacity,
    'Does the 2008 ICJ ruling have any binding enforcement mechanism beyond the state parties'' voluntary compliance?',
    'Review of ICJ enforcement protocols; analysis of state compliance with ICJ rulings in similar territorial disputes; assessment of UN Security Council involvement in enforcement',
    'If no enforcement: international law is purely performative for this constraint, and the piton classification is accurate. If enforcement exists: the international system has genuine coordination power, shifting the constraint toward Rope for ICJ perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(icj_enforcement_capacity, empirical, 'Whether ICJ rulings have binding enforcement mechanisms').

omega_variable(
    border_population_extraction_mechanism,
    'Is the border population displacement a byproduct of military escalation, or a deliberate extraction mechanism to clear contested territory?',
    'Analysis of displacement patterns relative to military operations; demographic studies of population return rates post-ceasefire; documentation of state policies toward border populations; comparison with displacement in other territorial conflicts',
    'If byproduct: the snare classification reflects genuine victimhood but may understate agency. If deliberate: the extraction is more severe and systematic, confirming snare classification and adding war crime dimensions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_population_extraction_mechanism, empirical, 'Whether border population displacement is deliberate or incidental').

omega_variable(
    colonial_demarcation_ambiguity_resolution,
    'Is the original colonial boundary ambiguity genuinely unresolvable, or has it been treated as unresolvable to preserve the extraction mechanism?',
    'Archival analysis of French colonial demarcation records; cartographic reconstruction of intended vs. actual boundary; linguistic analysis of treaty language; assessment of technical feasibility of boundary clarification',
    'If genuinely ambiguous: the initial coordination problem is real, justifying the coordination function in Tangled Rope. If technically resolvable but politically preserved: the extraction is more deliberate, moving classification toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_demarcation_ambiguity_resolution, empirical, 'Whether colonial boundary ambiguity is genuinely unresolvable or politically maintained').

omega_variable(
    international_law_precedent_utility,
    'Does the ICJ precedent (Preah Vihear 2008) actually clarify international boundary law, or does it primarily serve to legitimize state positions without resolving the underlying dispute?',
    'Citation analysis of the ruling''s use in subsequent international law scholarship; application of the precedent to other territorial disputes; assessment of whether the ruling changed international boundary settlement practices',
    'If precedent is generative: international law coordination function is real (Rope classification for ICJ accurate). If legitimation theater: the ruling is purely performative, confirming Piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_precedent_utility, empirical, 'Whether ICJ precedent clarifies law or legitimizes positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preah_vihear_territorial_claim, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pvt_tr_t0, preah_vihear_territorial_claim, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pvt_tr_t10, preah_vihear_territorial_claim, theater_ratio, 10, 0.55).
narrative_ontology:measurement(pvt_tr_t20, preah_vihear_territorial_claim, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(pvt_be_t0, preah_vihear_territorial_claim, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pvt_be_t10, preah_vihear_territorial_claim, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pvt_be_t20, preah_vihear_territorial_claim, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pvt_su_t0, preah_vihear_territorial_claim, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(pvt_su_t10, preah_vihear_territorial_claim, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(pvt_su_t20, preah_vihear_territorial_claim, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preah_vihear_territorial_claim, enforcement_mechanism).
narrative_ontology:affects_constraint(preah_vihear_territorial_claim, thai_military_budget_allocation).
narrative_ontology:affects_constraint(preah_vihear_territorial_claim, cambodian_sovereignty_assertion_narrative).
narrative_ontology:affects_constraint(preah_vihear_territorial_claim, asean_regional_stability_infrastructure).

% DUAL FORMULATION NOTE:
% The Preah Vihear territorial claim decomposes into at least three structurally distinct constraints: (1) Colonial boundary demarcation ambiguity (ε ≈ 0.10, Mountain or Rope — genuine coordination problem with natural law properties), (2) International law precedent and ICJ authority (ε ≈ 0.25, Rope — coordination function with low extraction), (3) Nationalist mobilization extraction mechanism (ε ≈ 0.68, Snare or Tangled Rope — pure extraction using territorial claim as framing). The aggregated constraint (ε = 0.58, Tangled Rope) represents the mixed function across all three sub-components. Decomposition into separate stories would be appropriate if the analysis aimed to isolate the legal vs. political dimensions; the current story captures the constraint as experienced: an integrated mechanism where legal ambiguity enables political extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preah_vihear_territorial_claim, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
