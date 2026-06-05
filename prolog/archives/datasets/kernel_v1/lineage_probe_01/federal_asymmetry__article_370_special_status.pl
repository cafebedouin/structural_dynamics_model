% ============================================================================
% CONSTRAINT STORY: federal_asymmetry__article_370_special_status
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_asymmetry__article_370_special_status, []).

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
 *   constraint_id: federal_asymmetry__article_370_special_status
 *   human_readable: Article 370: Jammu and Kashmir Special Status Under the Indian Constitution
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   Article 370 of the Indian Constitution granted Jammu and Kashmir a
 *   special status within the Indian union: a separate constitution, autonomy
 *   over internal affairs, legislative powers limited for the Union, control
 *   over property ownership and permanent residency, and exemption from most
 *   national laws. Enacted as a temporary provision in 1950 via presidential
 *   order, it persisted for seventy years through unstated political
 *   consensus, until revoked via the same mechanism in August 2019. The
 *   constraint exhibits the full structural complexity of federal asymmetry:
 *   it was simultaneously a coordination mechanism that enabled Kashmir's
 *   integration into India (solving the collective action problem of bringing
 *   a distinct polity into a larger federation), an extraction mechanism that
 *   created second-class citizenship for non-Kashmiri Indians within
 *   Kashmir's borders, a protection mechanism for Kashmiri demographic and
 *   political autonomy, and a theatrical fiction maintained by the
 *   performative designation of temporariness. The 2019 abrogation reveals
 *   that the constraint's stability rested on political consensus rather than
 *   structural necessity — when that consensus fractured, the 'temporary'
 *   provision could be eliminated in a single parliamentary session. This
 *   reading instantiates ONE interpretation of the federal asymmetry kernel:
 *   that Article 370 was the apex of constitutional asymmetry, a negotiated
 *   compact grounded in the legitimacy claim of protecting Kashmiri
 *   self-determination within a federal framework. This reading coexists with
 *   (and is influenced by) alternative readings that emphasize linguistic
 *   reorganization as an asymmetry mechanism, or union-centered bias in
 *   federal design itself.
 *
 * KEY AGENTS:
 *   - Kashmiri Political Establishment: Primary beneficiary (institutional/arbitrage, 1950–2019) — gained autonomy over internal administration, control over property/residency, separate constitutional framework. Victim after 2019 (constrained/identity_locked) — the abrogation removed the institutional framework that constituted Kashmiri political identity within the Indian federation.
 *   - Indian Citizen-Outsider: Primary victim (powerless/trapped) — denied property ownership, permanent residency, and electoral participation in Kashmir under Article 370. Beneficiary after 2019 under uniform citizenship (arbitrage) — gained access to previously restricted rights, though gained under conditions of coercive abrogation.
 *   - Indian Union/Federal Center: Constrained beneficiary (institutional/arbitrage) — gained territorial integration and foreign policy control but constrained in legislative reach and uniform state-building in one state. Post-2019: gains uniform federal authority but loses the political legitimacy that asymmetry provided.
 *   - Kashmir Valley Minorities (religious, ethnic): Structurally ambiguous — Article 370 protected them from electoral marginalization within Kashmir's majority-dominated politics, but also prevented their access to pan-Indian legal protections and employment. Post-2019: formal equality but potential vulnerability to majoritarian politics at both state and national levels.
 *   - India's Partition Trauma: Structural driver of the constraint — Article 370 was partially a payment of debt to Kashmir for partition violence and separatist threat. This shapes the legitimacy claim of the reading.
 *   - Analytical Observer: Sees the constraint as either a necessity of federation (mountain) or a false naturalization of political choice (false summit).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_asymmetry__article_370_special_status, 0.58).
domain_priors:suppression_score(federal_asymmetry__article_370_special_status, 0.68).
domain_priors:theater_ratio(federal_asymmetry__article_370_special_status, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_asymmetry__article_370_special_status, extractiveness, 0.58).
narrative_ontology:constraint_metric(federal_asymmetry__article_370_special_status, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federal_asymmetry__article_370_special_status, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_asymmetry__article_370_special_status, tangled_rope).
narrative_ontology:human_readable(federal_asymmetry__article_370_special_status, "Article 370: Jammu and Kashmir Special Status Under the Indian Constitution").
narrative_ontology:topic_domain(federal_asymmetry__article_370_special_status, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(federal_asymmetry__article_370_special_status).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_asymmetry__article_370_special_status, '45358394-086d-4178-8c49-0be337549005').
narrative_ontology:cs_kernel_codification('45358394-086d-4178-8c49-0be337549005', formalized).
narrative_ontology:cs_authority_grounding('45358394-086d-4178-8c49-0be337549005', extraction).
narrative_ontology:cs_interpretation_layer_present('45358394-086d-4178-8c49-0be337549005').
narrative_ontology:cs_reading_relation('45358394-086d-4178-8c49-0be337549005', federal_asymmetry__linguistic_reorganization_reading, coexists_with).
narrative_ontology:cs_reading_relation('45358394-086d-4178-8c49-0be337549005', federal_asymmetry__union_bias_design_reading, coexists_with).
narrative_ontology:cs_axiom('45358394-086d-4178-8c49-0be337549005', foundational, special_status_protects_kashmiri_self_determination).
narrative_ontology:cs_axiom_status(special_status_protects_kashmiri_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('45358394-086d-4178-8c49-0be337549005', special_status_protects_kashmiri_self_determination, deontological).
narrative_ontology:cs_axiom('45358394-086d-4178-8c49-0be337549005', foundational, asymmetry_as_negotiated_federal_compact).
narrative_ontology:cs_axiom_status(asymmetry_as_negotiated_federal_compact, overridden).
narrative_ontology:cs_axiom_grounding('45358394-086d-4178-8c49-0be337549005', asymmetry_as_negotiated_federal_compact, instrumental).
narrative_ontology:cs_reference_frame('45358394-086d-4178-8c49-0be337549005', constitutional_asymmetry_as_legitimated_autonomy).
narrative_ontology:cs_drift_state('45358394-086d-4178-8c49-0be337549005', contemporary_hindu_nationalist_resurgence, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('45358394-086d-4178-8c49-0be337549005', '').
narrative_ontology:cs_kernel_id(federal_asymmetry__article_370_special_status, federal_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_asymmetry__article_370_special_status, kashmiri_political_autonomy).
narrative_ontology:constraint_beneficiary(federal_asymmetry__article_370_special_status, central_union_authority).
narrative_ontology:constraint_victim(federal_asymmetry__article_370_special_status, uniform_federal_citizenship).
narrative_ontology:constraint_victim(federal_asymmetry__article_370_special_status, kashmiri_demographic_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KASHMIRI POLITICAL ESTABLISHMENT (ROPE) — Article 370 provided genuine coordination benefit: autonomy in internal administration, separate constitutional framework, control over permanent residency and land ownership. The special status enabled Kashmiri political actors to negotiate their place in the Indian union while retaining institutional sovereignty over critical domains. Experienced the constraint as coordination, not extraction — the mechanism solved the collective action problem of integrating a distinct polity into a larger federation. Net beneficiary through most of the 70-year period.
constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIAN CITIZEN-OUTSIDER IN KASHMIR (SNARE) — Article 370 created asymmetric citizenship rights: non-Kashmiri Indian citizens could not own property, acquire permanent residency, or participate in state electoral politics within Jammu and Kashmir. Trapped by a constitutional provision that created second-class citizenship within their own nation. Maximum suppression — no exit option except abandoning property claims or relocation. No coordination benefit received — pure extraction of political and property rights for the sake of demographic protection in Kashmir. The abrogation of Article 370 reversed the victim set: now Kashmiri residents face exclusion from pan-Indian job markets and demographic change outside their control.
constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INDIAN UNION / FEDERAL CENTER (TANGLED ROPE) — Article 370 simultaneously constrained and enabled Union authority. Constraint: the Union could not unilaterally alter Kashmir's special status or impose uniform national laws (Parliament had limited legislative powers in Kashmir). Coordination benefit: the special status was a negotiated mechanism that brought Kashmir into the Indian union at all — without it, accession was contingent. Asymmetric extraction: the Union tolerated the constraint as the price of territorial integration, but extracted federal authority over foreign policy, defense, and key domains. Requires active enforcement — the legal fiction of Kashmir's 'semi-autonomous' status had to be continuously maintained through institutional restraint. Beneficiary (territorial integrity, foreign policy control) and victim (limited legislative reach in one state) simultaneously.
constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: KASHMIR VALLEY POLITICAL PARTIES (TANGLED ROPE) — Article 370 enabled local political actors to compete for power within a protected institutional domain (coordination benefit). But the protection was asymmetric: the special status constrained what those actors could do (could not override Union prerogatives in defense/foreign policy) and made them structurally dependent on central approval for critical institutional changes. Exit options constrained by the fact that Kashmir's entire political economy was built around the special status — dismantling it required dismantling their power base. Beneficiaries of autonomy but victims of structural dependency.
constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE 'TEMPORARY PROVISION' FICTION (PITON) — Article 370 was labeled a 'temporary provision' from 1950 onward, yet remained in force for seven decades through institutional inertia and political compromise. The theater of temporariness was performative: the provision could theoretically be abrogated by presidential order, but political consensus made this unthinkable for generations. The amendment of Article 370 in 2019 broke the theatrical equilibrium — the 'temporary' provision was finally revoked, revealing that the theater had been the only thing keeping it stable. Theater ratio high because the constraint persisted despite an institutional pathway to terminate it, maintained by unstated political consensus rather than genuine legal or structural necessity.
constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, federal asymmetry is an inherent feature of any multi-ethnic state integrating distinct polities: some degree of differential autonomy is structurally necessary when combining communities with distinct identities, legal traditions, and historical claims to self-determination. The constraint appears immutable — any federation attempting uniform citizenship and law across radically distinct populations faces identical structural pressures. However, the beneficiary/victim declarations reveal this is a false summit: the 'necessity' of asymmetry naturalizes what is actually a contested political choice about which communities receive protection and which are subjected to majoritarianism.
constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_asymmetry__article_370_special_status_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_asymmetry__article_370_special_status, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_asymmetry__article_370_special_status, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_asymmetry__article_370_special_status, TR),
    TR >= 0.70.

:- end_tests(federal_asymmetry__article_370_special_status_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time. In 1950, Article 370 was primarily a coordination mechanism with modest extraction — Kashmiri autonomy was secured, non-Kashmiri citizens faced restrictions but were not the primary population. By 2019, extractiveness had risen to 0.65 as the constraint became increasingly contested: Kashmiri political actors faced pressure from nationalist movements demanding full sovereignty; the federal center faced pressure from Hindu nationalist movements demanding uniform citizenship; non-Kashmiri residents and investment faced increasing restrictions. The rise reflects increasing asymmetry cost as the political consensus that stabilized the arrangement fractured. Suppression (0.68): Moderate-high, rising over time. The constraint required active institutional suppression of alternatives: suppression of the mountain position (natural law federalism) through performative designation as 'temporary'; suppression of uniform citizenship through legal exception; suppression of Kashmir's independence movements through federal security apparatus; suppression of demands for abrogation from Hindu nationalist coalitions through cross-party consensus (until 2019). The rise reflects increasing difficulty of maintaining the arrangement as the consensus weakened. Theater ratio (0.55): Moderate, the key diagnostic signal for the Piton perspective. The 'temporary provision' fiction is performative — the constitutional mechanism for amendment existed and was straightforward (Article 370(3)), but political consensus prevented its invocation for seven decades. This performative stability distinguishes the constraint from both Scaffold (which has a genuine temporal mechanism) and Rope (which persists through real coordination benefit alone). The rise over time reflects increasing difficulty of maintaining the theatrical consensus.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The Kashmiri political establishment sees coordination (Rope) — Article 370 solved the problem of integrating Kashmir into India while preserving institutional autonomy. The non-Kashmiri Indian citizen sees extraction (Snare) — second-class citizenship with no exit option. The federal center sees mixed coordination and constraint (Tangled Rope) — gained integration but lost legislative reach. Kashmir's political parties see protection and dependency (Tangled Rope) — autonomy within a constrained domain. The constitutional fiction sees its own degradation (Piton) — persists through institutional inertia despite an exit mechanism. The civilizational observer risks naturalizing this as immutable federation law (Mountain) — but the beneficiary declarations reveal it as a political choice. The 2019 abrogation demonstrates that the constraint persisted through unstated consensus, not structural necessity, making the Piton perspective empirically correct — when consensus fractured, the 'temporary' provision could be eliminated overnight.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position and exit capacity. Kashmiri beneficiaries with arbitrage options (institutional power, political capacity to negotiate) experience low d → low extracted effective force. Non-Kashmiri trapped citizens experience maximum d (1.0) → maximum experienced extraction. The federal center experiences moderate d (0.50-0.55) because it is both beneficiary (territorial integrity, foreign policy control) and victim (constrained legislative reach). Moderate Kashmiri political actors face constrained exits — they cannot exit the autonomy framework without dismantling their political base, so d is high for the constraint even though they benefit from it. The analytical observer at civilizational scope experiences d ≈ 0.72 (per canonical analysis fallback), which generates the mountain classification. The false summit detector identifies that this apparent natural law has identifiable beneficiaries (Kashmiri political establishment) and victims (non-Kashmiri citizens, uniform statehood), revealing the 'necessity' of asymmetry as a political choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy is resolved by recognizing that Article 370 WAS both a coordination mechanism AND an extraction mechanism — not one or the other. The tangled_rope classification correctly captures that the constraint provided genuine benefits to Kashmiri political actors (coordination of autonomy, protection from demographic marginalization within India) while simultaneously imposing costs on non-Kashmiri citizens (second-class citizenship). The mandatrophy is not 'which is it really?' but 'for whom and in which domain?' The Rope perspective (Kashmiri beneficiary) and Snare perspective (non-Kashmiri victim) are both accurate structural descriptions. The federal center's Tangled Rope perspective shows that the center was also constrained (legislative reach limited to certain domains) while being beneficiary (territorial integration, foreign policy control). The constraint's claimed_type (tangled_rope) reflects this genuine hybridity — it is neither pure coordination (Rope) nor pure extraction (Snare), but a mixed structure where coordination benefits for one group are purchased by extraction costs for another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_extraction_direction,
    'Was Article 370''s special status a coordination mechanism protecting Kashmiri self-determination, or an extraction mechanism concentrating power asymmetrically in the hands of Kashmir''s elite?',
    'Historical analysis of (a) legislative capacity: what decisions did Article 370 permit Kashmiri bodies to make autonomously vs. which required Union approval? (b) Beneficiary identification: who within Kashmir actually benefited from the special status (political class, landowners, religious authorities) vs. who bore costs (minorities, women, economically excluded groups)? (c) Comparison with other asymmetric federations (Belgium, Spain, Bosnia) to determine whether the extraction pattern is unique or generic.',
    'If coordination: the reading treats Article 370 as a genuine constitutional compact that solved a collective action problem (Rope from Kashmiri perspective). If extraction: the reading becomes Tangled Rope or Snare depending on whether the Kashmiri elite captured the autonomy for themselves. This affects whether the abrogation is injustice (elimination of genuine self-determination) or correction (dismantling a mechanism of elite capture and minority suppression within Kashmir).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_extraction_direction, empirical, 'Direction of asymmetric benefit flow under Article 370').

omega_variable(
    temporariness_structural_or_performative,
    'Was Article 370''s designation as a ''temporary provision'' a genuine constitutional constraint that made abrogation structurally available, or a performative fiction that masked political consensus?',
    'Constitutional text analysis: what mechanisms existed for presidential amendment? (Answer: Article 370(3) made amendment straightforward, requiring only presidential order and simple parliamentary amendment of the Constitution.) Historical political analysis: why did seven successive parliaments and multiple national governments refrain from invoking this mechanism? (Answer: sustained political consensus that abrogation would destabilize the Kashmir settlement.) Institutional comparison: other ''temporary'' provisions that were revoked or made permanent — do they show abrogation depends on political will rather than constitutional pathway?',
    'If structural: Article 370 is better classified as Scaffold (temporary by design, with built-in exit mechanism). Theater ratio drops toward 0.40. If performative: the theater of temporariness was the only stabilizing force; abrogation revealed that the constraint persisted through unstated agreement, making it Piton (maintained by institutional inertia despite an exit mechanism being available).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporariness_structural_or_performative, empirical, 'Whether Article 370 temporariness was structural mechanism or performative fiction').

omega_variable(
    partition_debt_versus_negotiated_compact,
    'Was Article 370 a payment of historical debt to Kashmir for partition trauma and the Hindu-Muslim violence of 1947, or a negotiated compact between equals for federal integration?',
    'Historical source analysis: did the Indian leadership frame Article 370 as (a) reparation for partition (framing it as temporary atonement) or (b) permanent constitutional arrangement (framing it as a binding federal structure)? Comparison with other partition-related constitutional provisions (minorities rights, refugee rehabilitation, property laws) to determine whether Article 370 fits the reparative or structural category. Analysis of Kashmiri leadership''s framing: did they treat it as a debt payment or as a constitutional right?',
    'If debt framing: the constraint is interpreted as Snare with a sunset (reparation that could end) — its termination is justifiable as the debt being paid. If compact framing: termination is breach of federal bargain — the reading becomes Tangled Rope with strong victim status for Kashmir. This fundamentally changes whether abrogation is correction or injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_debt_versus_negotiated_compact, conceptual, 'Whether Article 370 represents historical debt or constitutional compact').

omega_variable(
    committer_frame_kernel_contest,
    'How does the Article 370 reading relate to the sibling readings of the federal asymmetry kernel? Does this reading foreclose, coexist with, or influence the linguistic reorganization and union bias readings?',
    'Structural analysis: (1) Does the logic of Article 370''s special status preclude the linguistic reorganization logic? (Answer: No — the 1956 reorganization occurred while Article 370 was in force, showing two independent asymmetry mechanisms operating simultaneously.) (2) Does the logic of Article 370''s special status preclude the union bias design reading? (Answer: No — Article 370 coexists with union emergency powers, all-India services, and a centralized legislative list. Article 370 is one asymmetry mechanism among many.) (3) Do these readings compete or reinforce? (Answer: They reinforce — linguistic reorganization deepened federal asymmetry; union bias design provided the legal framework within which Article 370 operated.) Therefore, relation is coexists_with (held by different political coalitions) and influences (this reading changes the conditions under which the siblings operate).',
    'The classification of relation types determines whether the three readings of federal asymmetry are alternatives (forecloses) or simultaneous structural mechanisms (coexists_with/influences). This affects how the constraint family is decomposed and whether resolving one reading resolves the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_contest, conceptual, 'Structural relationship between Article 370 reading and sibling readings in federal asymmetry kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_asymmetry__article_370_special_status, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art370_theater_1950, federal_asymmetry__article_370_special_status, theater_ratio, 0, 0.35).
narrative_ontology:measurement(art370_theater_1975, federal_asymmetry__article_370_special_status, theater_ratio, 25, 0.42).
narrative_ontology:measurement(art370_theater_2000, federal_asymmetry__article_370_special_status, theater_ratio, 50, 0.48).
narrative_ontology:measurement(art370_theater_2019_pre, federal_asymmetry__article_370_special_status, theater_ratio, 70, 0.55).

% Extraction over time
narrative_ontology:measurement(art370_extractiveness_1950, federal_asymmetry__article_370_special_status, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(art370_extractiveness_1975, federal_asymmetry__article_370_special_status, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(art370_extractiveness_2000, federal_asymmetry__article_370_special_status, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(art370_extractiveness_2019_pre, federal_asymmetry__article_370_special_status, base_extractiveness, 70, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(art370_suppression_1950, federal_asymmetry__article_370_special_status, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(art370_suppression_1975, federal_asymmetry__article_370_special_status, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(art370_suppression_2000, federal_asymmetry__article_370_special_status, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(art370_suppression_2019_pre, federal_asymmetry__article_370_special_status, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_asymmetry__article_370_special_status, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_asymmetry__article_370_special_status, federal_asymmetry__linguistic_reorganization_reading).
narrative_ontology:affects_constraint(federal_asymmetry__article_370_special_status, federal_asymmetry__union_bias_design_reading).
narrative_ontology:affects_constraint(federal_asymmetry__article_370_special_status, kashmir_identity_lock_post_2019).
narrative_ontology:affects_constraint(federal_asymmetry__article_370_special_status, uniform_citizenship_creation_2019).

% DUAL FORMULATION NOTE:
% Article 370 is one reading of the federal asymmetry kernel. The linguistic reorganization reading and union bias design reading represent alternative mechanisms for creating asymmetry within the same federal system. All three readings should be decomposed into separate constraint stories with distinct epsilon values: Article 370 special status (epsilon=0.58, tangled_rope with false summit), linguistic reorganization (epsilon varies by region, range 0.30-0.55), union bias design (epsilon=0.48-0.65 depending on scope). The three stories are linked via affects_constraints to show how they reinforce and interact with each other. Additionally, Article 370's abrogation created two downstream constraints: (1) Kashmir identity_lock post-2019 (the loss of the constitutional framework that defined Kashmiri political identity), (2) uniform citizenship creation 2019 (the imposition of uniform property and residency rights, with asymmetric benefits/costs by region and community).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_asymmetry__article_370_special_status, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
