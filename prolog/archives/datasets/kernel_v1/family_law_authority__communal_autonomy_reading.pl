% ============================================================================
% CONSTRAINT STORY: family_law_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__communal_autonomy_reading, []).

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
 *   constraint_id: family_law_authority__communal_autonomy_reading
 *   human_readable: Family Law Authority: Religious Community Autonomy Reading
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested
 *   family_law_authority kernel — the communal_autonomy_reading. This reading
 *   grounds family law authority in religious community tradition and assigns
 *   the state a recognition-only role: the state acknowledges communal legal
 *   authority but does not regulate its substantive rules. The reading claims
 *   that legitimate family law authority derives from continuous transmission
 *   within a religious tradition (lineage grounding), not from constitutional
 *   state power. This produces systematic extraction of women and minorities
 *   within communities while appearing as neutral cultural autonomy. The
 *   constraint exhibits tangled rope structure: genuine coordination exists
 *   (communities do resolve disputes, preserve traditions, provide social
 *   support) alongside systematic asymmetric extraction (patriarchal control
 *   encoded as tradition, identity-lock preventing exit, suppression of
 *   alternative readings of tradition). The reading's core tension lies in
 *   naturalization: claiming that male-dominated family structures are
 *   'traditional' and therefore legitimate, while treating modern enforcement
 *   mechanisms (council authority, economic disability, social exclusion) as
 *   natural expressions of tradition rather than constructed power
 *   arrangements. The temporal measurements show rising extractiveness,
 *   suppression, and theater over the 100-year interval: as communities
 *   formalize authority structures (councils, written opinions, dispute
 *   resolution procedures), the constraint becomes more visible and more
 *   enforceable, but the increase in formality is accompanied by rising
 *   theater (codified tradition presented as unchanging rather than as
 *   emerging institutional arrangements). This reading coexists with two
 *   siblings: constitutional_supremacy_reading (which grounds all family law
 *   in constitutional state authority) and hybrid_accommodation_reading
 *   (which attempts to balance communal autonomy with constitutional floors).
 *   The three readings are held by different legal factions, judicial
 *   traditions, and scholarly communities — they are live competitors in
 *   contemporary family law without a unified framework holding all three.
 *
 * KEY AGENTS:
 *   - Religious Community Leadership (institutional/arbitrage): primary beneficiary — controls family law authority, benefits from state non-interference, maintains patriarchal structures encoded as tradition
 *   - Women Under Uncodified Community Law (powerless/identity_locked): primary victim — subject to community councils with no written rights or appeal, identity-locked to religious membership, trapped in patriarchal rules presented as culture
 *   - Religious Minorities Within Community (moderate/constrained): secondary victim — subordinate standing in community authority structures, constrained exit options (can leave but lose community access and identity),
 *   - State Judicial Authority (institutional/arbitrage): secondary beneficiary — low-cost coordination (avoids adjudicating complex religious matters), maintains neutrality pose, arbitrage through multiple legal regimes
 *   - Marginalized Family Structures (powerless/trapped): victim — structures not recognized by community law (same-sex partnerships, non-binary identities, chosen families) have no standing; state deference to community excludes them entirely
 *   - Analytical Observer (analytical/analytical): sees structural asymmetry between coordination function and extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__communal_autonomy_reading, 0.45).
domain_priors:suppression_score(family_law_authority__communal_autonomy_reading, 0.58).
domain_priors:theater_ratio(family_law_authority__communal_autonomy_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__communal_autonomy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(family_law_authority__communal_autonomy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(family_law_authority__communal_autonomy_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__communal_autonomy_reading, "Family Law Authority: Religious Community Autonomy Reading").
narrative_ontology:topic_domain(family_law_authority__communal_autonomy_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(family_law_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__communal_autonomy_reading, 'aaa3ef96-2ce5-4118-90db-f96e52872a83').
narrative_ontology:cs_kernel_codification('aaa3ef96-2ce5-4118-90db-f96e52872a83', fixed_text).
narrative_ontology:cs_authority_grounding('aaa3ef96-2ce5-4118-90db-f96e52872a83', lineage).
narrative_ontology:cs_interpretation_layer_present('aaa3ef96-2ce5-4118-90db-f96e52872a83').
narrative_ontology:cs_reading_relation('aaa3ef96-2ce5-4118-90db-f96e52872a83', family_law_authority__constitutional_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('aaa3ef96-2ce5-4118-90db-f96e52872a83', family_law_authority__hybrid_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('aaa3ef96-2ce5-4118-90db-f96e52872a83', foundational, communal_authority_legitimacy).
narrative_ontology:cs_axiom_status(communal_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('aaa3ef96-2ce5-4118-90db-f96e52872a83', communal_authority_legitimacy, conventional).
narrative_ontology:cs_axiom('aaa3ef96-2ce5-4118-90db-f96e52872a83', foundational, state_recognition_non_interference).
narrative_ontology:cs_axiom_status(state_recognition_non_interference, holdable).
narrative_ontology:cs_axiom_grounding('aaa3ef96-2ce5-4118-90db-f96e52872a83', state_recognition_non_interference, instrumental).
narrative_ontology:cs_reference_frame('aaa3ef96-2ce5-4118-90db-f96e52872a83', continuous_religious_tradition_transmission).
narrative_ontology:cs_drift_state('aaa3ef96-2ce5-4118-90db-f96e52872a83', contemporary_state_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aaa3ef96-2ce5-4118-90db-f96e52872a83', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(family_law_authority__communal_autonomy_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__communal_autonomy_reading, religious_community_leadership).
narrative_ontology:constraint_beneficiary(family_law_authority__communal_autonomy_reading, patriarchal_family_structure_beneficiaries).
narrative_ontology:constraint_victim(family_law_authority__communal_autonomy_reading, women_under_uncodified_regimes).
narrative_ontology:constraint_victim(family_law_authority__communal_autonomy_reading, religious_minorities_within_community).
narrative_ontology:constraint_victim(family_law_authority__communal_autonomy_reading, marginalized_family_structures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN IN UNCODIFIED REGIMES (SNARE) — Identity-locked to religious and kinship identity; structurally mobile (could seek state law jurisdiction) but exit would dissolve their identity within the community. Bears full extraction burden: marriage dissolution, custody, inheritance, guardianship controlled by community councils with minimal written rules or appeal mechanisms. No exit option that preserves community membership; no protection from state law (state defers to community authority).
constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RELIGIOUS MINORITIES WITHIN COMMUNITY (TANGLED ROPE) — Constrained by enforcement mechanisms (social exclusion, economic disability, denial of community benefits). Experience both coordination (community law provides dispute resolution, social support) and extraction (unequal standing, denial of decision-making power). Subordinate status in authority structure; can exit but at significant cost to access and identity.
constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS COMMUNITY LEADERSHIP (ROPE) — Primary beneficiary with arbitrage options (can appeal to state recognition, shift jurisdiction, reinterpret tradition to maintain authority). Experiences the constraint as pure coordination: maintaining family law authority is the mechanism for coordinating community practice, preserving tradition, and resolving disputes. High benefit, minimal perceived extraction — the authority itself is the prize.
constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE JUDICIAL AUTHORITY (ROPE) — From the state's perspective in a recognition regime, the constraint is coordination: deferring family law to communities solves a complex pluralism problem without requiring the state to adjudicate internal religious practice. Low-cost coordination mechanism; state arbitrage lies in maintaining multiple legal regimes (civil + communal) without direct oversight burden. State experiences minimal extraction (administrative efficiency).
constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination (dispute resolution, cultural preservation) and systematic extraction (patriarchal authority, gender subordination, enforcement via identity-lock). The theater ratio reflects that community law often claims 'timeless tradition' while operating modern enforcement (surveillance, economic exclusion). Recognition model obscures asymmetric extraction by treating community authority as purely internal/cultural rather than structural/coercive.
constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE INSTITUTIONAL INERTIA (PITON) — The state's 'recognition' of communal authority often becomes performative abdication: the state maintains a formal stance of neutrality while avoiding accountability for outcomes produced under communal law (denial of custody, forced marriage, inheritance discrimination). The constraint persists through institutional inertia and doctrinal theater ('respect for cultural autonomy', 'non-interference in private sphere') rather than active governance function. State sees itself as neutral but functions as accomplice.
constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__communal_autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_law_authority__communal_autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_law_authority__communal_autonomy_reading, TR),
    TR >= 0.70.

:- end_tests(family_law_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate, reflecting the tangled rope signature — genuine coordination mechanisms coexist with extraction. Community family law does provide dispute resolution, social support, and tradition preservation (legitimate benefits). But these coordination functions are tightly coupled with patriarchal extraction: women bear disproportionate authority loss, inheritance inequality, guardianship subordination. The 0.45 value reflects that extraction is not maximal (some coordination function remains real) but is substantial and structurally embedded. Suppression (0.58): Moderate-high. Enforcement via community social authority (reputation, economic access, identity dissolution threat) is softer than state legal coercion but effectively prevents exit. Women cannot invoke state law to overturn community rulings without losing community membership. Minorities within the community face systematic exclusion from decision-making. Theater ratio (0.65): Moderate-high. 'Timeless tradition' is invoked to justify rules that are actually recent institutional constructs (community councils are often 20th-century inventions formalized as 'traditional'). The presentation of male authority as cultural inevitability rather than as modern enforcement structure constitutes significant theater. Claimed type tangled_rope reflects: requires_active_enforcement (true — community councils actively adjudicate and enforce), beneficiaries (religious leadership, patriarchal structure), victims (women, minorities), and ε ≥ 0.30 with suppression ≥ 0.40.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The community leadership sees pure coordination (rope) — family law authority enables cultural transmission and dispute resolution. The state sees low-cost pluralism (rope) — deferring family law avoids complex adjudication. Women trapped in uncodified regimes see pure extraction with identity-lock (snare) — no exit option preserves their identity, no appeal mechanism protects them. Religious minorities see mixed coordination and extraction (tangled rope) — they benefit from community institutions but with subordinate standing. The analytical observer at civilizational scope sees a tangled rope with rising theater — the constraint is becoming more formalized (institutionalizing extraction) while maintaining tradition-based legitimacy claims. The state's institutional perspective sees institutional inertia (piton) — the 'recognition' stance becomes performative when the state avoids reviewing outcomes and denies alternative jurisdiction to those harmed. All these readings are from the same set of base properties (ε=0.45, suppression=0.58, theater=0.65) but interpreted through different (Power, Time, Exit, Scope) tuples. The gap reveals that 'cultural autonomy' is perspectival: it appears as coordination to those controlling the authority structure and as extraction to those subject to unequal rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Religious leadership (beneficiary + arbitrage) derives d ≈ 0.05–0.15 (full beneficiary with exit capacity — low effective extraction). State authority (beneficiary + arbitrage) derives d ≈ 0.10–0.20 (coordination benefit, low extraction). Women victims (victim + identity_locked) derive d ≈ 0.85–0.95 (high extraction + no meaningful exit — identity dissolution cost exceeds any material cost). Religious minorities (victim + constrained) derive d ≈ 0.55–0.70 (moderate extraction + high but surmountable exit cost). Marginalized family structures (victim + trapped) derive d ≈ 0.90–1.00 (maximal extraction — no standing, no recognition, complete exclusion). The chi formula applies f(d) to these d values: beneficiaries experience negative or near-zero effective extraction despite base extractiveness of 0.45; victims experience much higher effective extraction. The scope modifier σ(S) applies: local scope (0.8) reduces visibility and verification difficulty; as this constraint scales to national/global scope, extractiveness amplification occurs (harder to conceal, harder to justify as 'just culture').
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this reading is whether 'authority derives from religious tradition' is a genuine source of legitimacy or a naturalization cover for patriarchal extraction. This reading claims the former (tradition-based authority is legitimate), but empirical trajectory shows rising extractiveness and suppression over time — patterns inconsistent with natural or timeless tradition. If tradition were truly the source, extractiveness should remain stable; instead, formalization of community authority (councils, codified opinions) increases both extractiveness and theater. The analytical observer's tangled_rope classification resolves the mandatrophy by identifying the tension: the coordination function (tradition preservation, dispute resolution) is real, but the extraction function (patriarchal authority, subordination) is equally real and is not derivable from tradition itself — it is a choice about which traditions to codify and which to abandon. The reading does not resolve this; it naturalizes it by treating patriarchal tradition as unchallengeable rather than as one interpretation among possible alternatives. The rising theater (0.55 → 0.65 over the interval) indicates increasing performativity: as challenges to patriarchal authority mount, the invocation of 'timeless tradition' intensifies despite the tradition being modern and contested. Mandatrophy resolution requires the reading to acknowledge that tradition is being actively selected and enforced, not passively inherited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_identity_lock_boundary,
    'Where does genuine consent to communal authority end and identity-locked coercion begin? Can women authentically consent to patriarchal family law when exit means identity dissolution?',
    'Longitudinal qualitative research: exit trajectories of women who leave communities; analysis of decision-making under conditions of identity-lock vs structural mobility; comparison of reported autonomy across regimes with varying exit costs.',
    'If boundary favors consent: women classified as mobile rather than identity_locked; constraint reclassifies from snare toward tangled_rope. If identity-lock is binding: snare classification confirmed; victims lack meaningful choice. Classification hinge on whether consent requires structural capacity to exit without identity collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_vs_identity_lock_boundary, conceptual, 'Whether consent is possible under identity-lock conditions').

omega_variable(
    community_enforcement_versus_state_enforcement,
    'Is community enforcement of family law ''soft'' (social pressure, reputation) or functionally equivalent to state coercion (economic exclusion, denial of access to essential services)?',
    'Empirical analysis of enforcement mechanisms: frequency and severity of economic penalties, social exclusion, denial of community benefits; comparison with state enforcement practices in same jurisdictions; exit cost measurement for women attempting to invoke state law against community authority.',
    'If soft: suppression overstated (~0.40 is adequate); constraint approaches hybrid coordination model. If coercive: suppression accurate or understated (~0.60+); snare classification for victims confirmed. Mechanism classification changes with empirical finding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_enforcement_versus_state_enforcement, empirical, 'Strength of community enforcement mechanisms').

omega_variable(
    reading_underspecification_natural_law_cover,
    'Is ''authority derives from religious tradition'' a genuine normative claim about legitimate authority sources, or a cover story naturalizing male-dominated inheritance structures as inevitable cultural facts?',
    'Historical analysis: documented evolution of ''tradition'' over time (whether codifications have shifted to accommodate male interests); comparison of how ''tradition'' is invoked in defense of patriarchal rules vs. egalitarian ones; recognition of which historical moments get tagged as ''original'' tradition.',
    'If naturalization cover: axiom ''communal_authority_legitimacy'' should be reclassified from holdable to overridden (the reading has begun acknowledging tradition as malleable); constraint moves toward piton (theater conceals instrumental use of tradition). If genuine normative commitment: axiom remains holdable; tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_underspecification_natural_law_cover, conceptual, 'Whether ''tradition'' in this reading is normative claim or naturalization cover').

omega_variable(
    state_recognition_as_abdication_vs_pluralism,
    'Does state recognition of communal family law represent legitimate legal pluralism (respecting community autonomy) or institutional abdication from constitutional duties (equal protection, due process)?',
    'Doctrinal analysis: comparison with state responsibilities in other domains (criminal law, contract law); review of state court decisions invoking ''cultural respect'' to deny review of communal-law outcomes; examination of whether state provides alternative forums or enforcement of constitutional floor for those harmed by communal law.',
    'If pluralism: state perspective remains rope (genuine coordination); constraint is neutral between readings. If abdication: state perspective reclassifies toward piton or snare (complicit extraction); systemic extraction increases; victims'' exit options degrade further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_recognition_as_abdication_vs_pluralism, empirical, 'Whether state recognition of communal law is pluralism or abdication').

omega_variable(
    axiom_foreclosure_across_readings,
    'Does this reading''s axiom ''communal_authority_legitimacy'' logically foreclose the hybrid_accommodation_reading''s claim that constitutional floors (gender equality) are compatible with communal autonomy?',
    'Analytical: test whether accepting ''communal authority derives from tradition'' (this reading) is logically compatible with ''communities must also comply with constitutional equality norms'' (hybrid reading). If the readings both claim authority over family law but one derives it from tradition and the other from constitution, what happens when they conflict?',
    'If foreclosure: reading_relations to hybrid_accommodation should be forecloses, not coexists_with. If compatible: coexists_with is correct (readings held by different legal schools without internal logical contradiction). Directs engine toward conflict detection or coexistence modeling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_foreclosure_across_readings, conceptual, 'Logical compatibility of communal axiom with constitutional floor axiom').

omega_variable(
    women_exit_capacity_degradation_over_time,
    'As community authority becomes institutionalized (codified councils, formal dispute resolution), does women''s effective exit capacity improve (more transparent rules) or degrade (more formalized suppression)?',
    'Comparative historical analysis: jurisdictions with informal vs. formalized communal family law; measurement of women''s exit trajectories, state court receptivity to appeals, enforcement of alternative jurisdiction; correlation between institutionalization and exit success rates.',
    'If institutionalization improves exits: measurement trajectory shows suppression declining, constraint approaches rope. If institutionalization formalizes suppression: suppression rises, constraint approaches snare. Lifecycle pattern clarifies whether formalization is liberalizing or ossifying.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_exit_capacity_degradation_over_time, empirical, 'Effect of institutionalization on women''s exit capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__communal_autonomy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_comm_theater_t0, family_law_authority__communal_autonomy_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(fla_comm_theater_t50, family_law_authority__communal_autonomy_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(fla_comm_theater_t100, family_law_authority__communal_autonomy_reading, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(fla_comm_extract_t0, family_law_authority__communal_autonomy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fla_comm_extract_t50, family_law_authority__communal_autonomy_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(fla_comm_extract_t100, family_law_authority__communal_autonomy_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fla_comm_suppress_t0, family_law_authority__communal_autonomy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fla_comm_suppress_t50, family_law_authority__communal_autonomy_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(fla_comm_suppress_t100, family_law_authority__communal_autonomy_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__communal_autonomy_reading, family_law_authority__constitutional_supremacy_reading).
narrative_ontology:affects_constraint(family_law_authority__communal_autonomy_reading, family_law_authority__hybrid_accommodation_reading).
narrative_ontology:affects_constraint(family_law_authority__communal_autonomy_reading, gender_equality_enforcement_constraint).
narrative_ontology:affects_constraint(family_law_authority__communal_autonomy_reading, religious_autonomy_vs_state_jurisdiction).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into three structurally distinct constraints, each with its own ε value and legitimacy grounding. This communal_autonomy_reading has ε ≈ 0.45 (tangled rope: genuine coordination + systematic extraction). The constitutional_supremacy_reading has lower ε (rope or mountain: unified state authority with minimal extraction if properly enforced). The hybrid_accommodation_reading has ε ≈ 0.50–0.65 (tangled rope or snare: depends on whether the 'floor' is enforced or merely aspirational). Network links connect the three readings and also link to downstream constraints (gender equality enforcement, religious autonomy vs. state jurisdiction) that are affected by which reading dominates in a jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__communal_autonomy_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
