% ============================================================================
% CONSTRAINT STORY: catholic_institutional_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catholic_institutional_control, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catholic_institutional_control
 *   human_readable: Catholic Institutional Control and Doctrinal Authority
 *   domain: religious/institutional/social
 *
 * SUMMARY:
 *   The Catholic Church's institutional control mechanism coordinates
 *   doctrinal unity and sacramental authority through a hierarchical system
 *   centered on papal infallibility and episcopal succession. This constraint
 *   exhibits a complex perspectival structure: from the Vatican's
 *   perspective, doctrinal control is a coordination mechanism enabling
 *   global unity and doctrinal consistency. From a devout parishioner's
 *   perspective, the constraint operates through identity fusion — exit would
 *   require abandoning a lifetime-constituted self as a Catholic. From a
 *   reform-minded priest's perspective, the constraint is a tangled hybrid:
 *   genuine coordination need (sacramental practice, doctrinal coherence)
 *   combined with extraction (vow of obedience limiting agency, career
 *   advancement tied to doctrinal compliance). From the analytical
 *   perspective at civilizational scope, the constraint risks appearing as an
 *   immutable natural law of religious organization — yet comparative
 *   analysis reveals that other Christian and non-Christian traditions
 *   maintain doctrinal identity through substantially different mechanisms
 *   (synodalism, interpretive pluralism, democratic governance). The theater
 *   ratio has risen over the interval from 0.35 (1900) to 0.62 (2010),
 *   reflecting Vatican II's introduction of the Reform Apparatus (pastoral
 *   councils, lay ministries, synodal processes) which promised institutional
 *   democratization but has largely become performative while real
 *   decision-making remains centralized. The trajectory suggests piton
 *   dynamics: the post-Vatican II reform machinery persists through
 *   institutional commitment to the Vatican II narrative despite functional
 *   atrophy in actual decision-making authority transfer.
 *
 * KEY AGENTS:
 *   - Vatican Hierarchy / Papal Authority: Primary beneficiary (institutional/arbitrage) — captures exclusive doctrinal authority and control over sacramental legitimacy; can reformulate doctrine but chooses not to
 *   - Devout Parishioner / Lay Membership: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with faith community; exit would require identity abandonment
 *   - Reform-Minded Priest: Secondary victim (moderate/constrained) — believes in coordination but constrained by vow of obedience and career advancement tied to compliance
 *   - Progressive Catholic Movement: Organized resistance (organized/constrained) — theologians and advocacy groups with some agency to propose alternative interpretations; face institutional sanction
 *   - Post-Vatican II Apparatus: Institutional apparatus (institutional/arbitrage) — maintains performative reform machinery; sees own process as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent hierarchical control as inherent to religious organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catholic_institutional_control, 0.58).
domain_priors:suppression_score(catholic_institutional_control, 0.68).
domain_priors:theater_ratio(catholic_institutional_control, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catholic_institutional_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(catholic_institutional_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(catholic_institutional_control, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catholic_institutional_control, tangled_rope).
narrative_ontology:human_readable(catholic_institutional_control, "Catholic Institutional Control and Doctrinal Authority").
narrative_ontology:topic_domain(catholic_institutional_control, "religious/institutional/social").

domain_priors:requires_active_enforcement(catholic_institutional_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catholic_institutional_control, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(catholic_institutional_control, doctrinal_consistency_maintenance).
narrative_ontology:constraint_victim(catholic_institutional_control, lay_membership).
narrative_ontology:constraint_victim(catholic_institutional_control, institutional_reform_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVOUT PARISHIONER (SNARE) — Structurally mobile (could leave) but identity-fused with the faith community. Exit would require abandoning a lifetime-constituted identity as a Catholic. High suppression through social isolation from alternative frameworks, fear of damnation internalized as cognitive pattern, relational bonds with faith community. Experiences the constraint as unchangeable despite structural mobility.
constraint_indexing:constraint_classification(catholic_institutional_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: REFORM-MINDED PRIEST (TANGLED ROPE) — Genuinely believes in Catholic mission coordination (doctrinal consistency, sacramental practice, community care) but faces institutional constraints: career advancement tied to doctrinal compliance, vow of obedience, geographic assignment authority. Benefits from institutional legitimacy and access to sacramental authority; bears costs of enforcing doctrines they may privately question. Mixed coordination and extraction.
constraint_indexing:constraint_classification(catholic_institutional_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VATICAN HIERARCHY (ROPE) — Benefits from doctrinal control and institutional unity. Experiences the constraint as coordination: maintaining doctrinal consistency enables the Church to function as a coherent global actor. Exit options are maximal (can reformulate doctrine, dissolve hierarchies) but are not exercised because the hierarchy genuinely sees the control mechanism as functional. Net beneficiary.
constraint_indexing:constraint_classification(catholic_institutional_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE CATHOLIC MOVEMENT (ORGANIZED) — Organized agents (theologians, advocacy groups, reform networks) see the constraint as a hybrid: real coordination need (doctrinal coherence) combined with extractive suppression of theological innovation. Have some agency to organize alternative interpretations but face institutional sanction. Can neither fully exit nor fully comply without tension.
constraint_indexing:constraint_classification(catholic_institutional_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-VATICAN II REFORM APPARATUS (PITON) — Vatican II (1962-1965) promised institutional renewal and lay engagement. The apparatus created to implement this (pastoral councils, lay ministries, ecumenical dialogue) has largely become performative theater. Real decision-making remains centralized; lay councils are advisory; ecumenical progress stalled. The reform machinery persists through institutional commitment to the Vatican II narrative, despite functional atrophy. Theater ratio dominates.
constraint_indexing:constraint_classification(catholic_institutional_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, religious institutions require doctrinal coherence and institutional unity to maintain identity across time — this appears as a natural law of religious organization itself. Doctrinal control is immutable because religion is inherently about shared belief. However, this naturalizes what is actually a contingent institutional choice: other Christian denominations, Jewish movements, Islamic communities, and secular institutions maintain identity through different mechanisms (democratic governance, interpretive pluralism, subsidiary authority). The mountain view risks false summit.
constraint_indexing:constraint_classification(catholic_institutional_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catholic_institutional_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catholic_institutional_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catholic_institutional_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catholic_institutional_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catholic_institutional_control, TR),
    TR >= 0.70.

:- end_tests(catholic_institutional_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Vatican hierarchy captures exclusive authority over doctrine, sacrament validity, and institutional direction. Lay members bear costs (constraints on theological voice, reproductive autonomy on contraception, LGBTQ+ inclusion) while benefiting from community, meaning, and sacramental access. The extraction is not total (coordination benefits are real) but asymmetric. Reform-minded clergy benefit from institutional legitimacy while bearing costs of enforced orthodoxy. Suppression (0.68): High. Suppression operates through multiple channels: institutional gatekeeping (ecclesiastical filtering of theological voice), credentialing barriers (only ordained clergy can teach official theology), social isolation (separation from alternative Christian communities and secular frameworks), identity internalization (deference internalized as spiritual humility), and family/community tie-downs (sacramental access conditional on institutional membership). Theater ratio (0.62): Moderate-high. Vatican II introduced pastoral councils, synodal processes, and lay ministries which promised institutional renewal and lay participation. In practice, these bodies are advisory; real decision-making remains hierarchical; major reforms (clerical celibacy, women's ordination, contraception teaching) remain blocked despite decades of lay and priest advocacy. The apparatus persists through institutional commitment to the Vatican II narrative despite functional atrophy.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the diagnostic signature of this constraint. The hierarchy's rope perception versus the parishioner's snare perception reveals the extraction mechanism: what appears as necessary coordination from above appears as inescapable imprisonment from below. The identity_locked exit option for the parishioner is critical — they are not trapped by external barriers (legal prohibition, economic dependency) but by identity fusion. They could leave but cannot imagine themselves post-Catholic. This is exactly the mechanism that makes suppression effective while appearing non-coercive. The piton reading of the post-Vatican II apparatus is crucial — it shows that the apparatus itself is part of the control mechanism: it performs reform while preventing it, neutralizing organized resistance by channeling it into toothless consultation. The false mountain at civilizational scope reveals the constraint's most powerful defense: naturalizing hierarchy as inherent to religion. This naturalization is the highest order extraction mechanism — making contingent institutional choices appear unchangeable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural relationship to the extraction flow. The Vatican hierarchy, as beneficiaries with arbitrage options, derives low d (~0.05-0.15) producing negative or near-zero effective extraction from their perspective (they experience coordination, not extraction). The devout parishioner, as a victim identity-locked with constrained or trapped practical exit, derives high d (~0.85-0.95) producing maximum experienced extraction. The reform priest, as a victim with constrained exit options, derives moderate-high d (~0.60-0.75) producing moderate extraction. The progressive movement, as organized agents with constrained exit, derives moderate d (~0.45-0.60) producing moderate-high extraction relative to their power level. The post-Vatican II apparatus, as institutional actors with arbitrage options, derives low d despite functional atrophy (they continue to benefit from being the legitimate reform channel even if toothless). The directionality chain correctly separates beneficiaries from victims and captures the extraction asymmetry across power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival decomposition. The hierarchy's rope is genuine coordination — doctrinal consistency does enable institutional coherence. But this coordination function does not justify the extraction measured from the victim perspective. The tangled_rope classification captures this dual reality: the constraint has a real coordination function (preventing schism, maintaining sacramental unity) AND systematic asymmetric extraction (lay voice suppressed, reforms blocked, reproductive autonomy constrained). The mandatrophy is resolved by recognizing that a constraint can be both genuinely functional AND extractive — the distinction is not 'Is this coordination or extraction?' but 'Who benefits from the coordination and who bears the extraction costs?' The comparative analysis omega (alternative coordination mechanisms) is crucial: if the Catholic Church could maintain identity and coherence through synodalism or democratic governance (like some Orthodox and Anglican traditions), then the hierarchical doctrinal control is not mandatorily coupled to coordination — it is a chosen design that extracts benefits to the hierarchy while providing coordination as a secondary benefit. The piton reading of the post-Vatican II apparatus reveals that the most dangerous stage of the constraint is when it introduces performative reform. This caps extractiveness at moderate (0.58) rather than maximum (0.75+) because targets believe change is possible through institutional channels. If the apparatus had remained purely extractive (no reform machinery at all), resistance would be more acute. The performative apparatus is the refined mechanism — it extracts while suppressing the perception of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_vs_structural_mobility,
    'Is the suppression experienced by lay members structural (material barriers to alternative community, economic dependency) or internalized (identity fusion with the faith)?',
    'Post-exit trajectory analysis: if members who leave the Church report persistent internalized shame, identity fragmentation, or social isolation years after material barriers are removed, suppression is substantially internalized.',
    'If purely internalized: the constraint''s effective suppression is carried by the agent; the Church''s institutional control is weaker than structural metrics suggest. If structural: barriers (social community, sacramental access, family ties) are primary; institutional control is durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_structural_mobility, empirical, 'Whether suppression is structural or identity-internalized').

omega_variable(
    doctrinal_consistency_necessity,
    'Is doctrinal control functionally necessary for the Church''s institutional coherence or is it historically contingent enforcement that could be replaced by other coordination mechanisms?',
    'Comparative institutional analysis: examine periods of doctrinal heterodoxy (early Church councils, medieval heresies, Reformation denominations) and assess whether doctrinal control actually prevented schism or whether schism happened despite control. Identify mechanisms that maintained identity in low-control periods.',
    'If necessary: the tangled_rope classification is correct — coordination function is genuine. If contingent: the constraint is primarily extractive (snare or piton); doctrinal control is ideological cover for institutional power consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_consistency_necessity, empirical, 'Whether doctrinal control is functionally necessary or ideologically contingent').

omega_variable(
    reform_apparatus_sunset,
    'Is Vatican II''s reform apparatus (lay councils, pastoral boards, ecumenical dialogue) genuinely moving toward institutional democratization with a sunset on hierarchical control, or is it permanent performative theater?',
    'Temporal measurement: track decision-making authority concentration over 50-year intervals; measure correlation between council recommendations and official policy changes; assess whether lay membership in decision-making bodies has increased decision-making power or been constrained to advisory roles.',
    'If sunset is real: scaffold classification is correct. If theater is permanent: piton classification understates the extraction — the apparatus actively suppresses reform expectations while maintaining the theater of participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_apparatus_sunset, empirical, 'Whether Vatican II apparatus is a genuine sunset mechanism or permanent theater').

omega_variable(
    alternative_coordination_mechanisms,
    'Could the Catholic Church maintain doctrinal identity and institutional coherence through mechanisms other than hierarchical doctrinal control — e.g., distributed theological interpretation, synodal authority, democratic governance?',
    'Examine existing Christian communities (Orthodox synodalism, Anglican dispersed authority, Pentecostal networks) that maintain identity without Vatican-style centralized doctrinal control. Assess whether these alternatives achieve comparable institutional stability, member retention, and theological consistency.',
    'If alternatives work: doctrinal control is not a natural law but a contingent design choice; the constraint''s extraction is not justified by necessity. If alternatives fail: the mountain view gains credibility — centralized control may be inherent to religious institutional durability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, conceptual, 'Whether alternative coordination mechanisms could replace hierarchical doctrinal control').

omega_variable(
    lay_voice_suppression_mechanism,
    'What specific mechanisms suppress lay theological voice: filtering through episcopal hierarchies, credentialing gatekeeping, institutional sanction, or internalized deference?',
    'Archival analysis of lay theological submissions, synodal processes, and reform initiatives; interviews with lay theologians on barriers to institutional voice; measurement of institutional response time and approval rates for lay-initiated reforms.',
    'If mechanisms are primarily external: targets can theoretically organize to demand voice. If primarily internalized: lay members self-suppress, believing voice is inappropriate — constraint is tighter than external barriers suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_voice_suppression_mechanism, empirical, 'Whether lay suppression is external or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catholic_institutional_control, 1900, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cath_tr_t1900, catholic_institutional_control, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(cath_tr_t1962, catholic_institutional_control, theater_ratio, 1962, 0.42).
narrative_ontology:measurement(cath_tr_t1985, catholic_institutional_control, theater_ratio, 1985, 0.58).
narrative_ontology:measurement(cath_tr_t2010, catholic_institutional_control, theater_ratio, 2010, 0.62).

% Extraction over time
narrative_ontology:measurement(cath_be_t1900, catholic_institutional_control, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement(cath_be_t1962, catholic_institutional_control, base_extractiveness, 1962, 0.48).
narrative_ontology:measurement(cath_be_t1985, catholic_institutional_control, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(cath_be_t2010, catholic_institutional_control, base_extractiveness, 2010, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catholic_institutional_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(catholic_institutional_control, 0.12).
narrative_ontology:affects_constraint(catholic_institutional_control, reproductive_autonomy_constraint).
narrative_ontology:affects_constraint(catholic_institutional_control, clerical_celibacy_enforcement).
narrative_ontology:affects_constraint(catholic_institutional_control, lgbtq_inclusion_barrier).

% DUAL FORMULATION NOTE:
% Catholic institutional control is upstream of three specific constraint domains (reproductive autonomy, clerical celibacy, LGBTQ+ inclusion) that inherit its extractive structure. Each downstream constraint has its own ε and perspectival analysis, but all three depend on the doctrinal control mechanism for enforcement. The upstream constraint establishes the authorization framework; the downstream constraints are its specific applications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catholic_institutional_control, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
