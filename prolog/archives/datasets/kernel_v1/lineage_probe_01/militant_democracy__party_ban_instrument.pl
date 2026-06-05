% ============================================================================
% CONSTRAINT STORY: militant_democracy__party_ban_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_militant_democracy__party_ban_instrument, []).

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
 *   constraint_id: militant_democracy__party_ban_instrument
 *   human_readable: Article 21 Party Ban as Militant Democracy Instrument
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the militant democracy
 *   kernel: the party-ban instrument (Article 21) as the constitutive
 *   mechanism for protecting the free democratic order against self-negating
 *   agents. This reading emphasizes that Karlsruhe's exclusive authority to
 *   declare parties unconstitutional constitutes the sharp edge of militant
 *   democracy doctrine — the willingness to suppress anti-system political
 *   competition at the cost of reduced internal pluralism. The reading
 *   differs structurally from sibling readings (basic_rights_forfeiture,
 *   lessons_of_weimar_reading) in that it treats the party-ban as the primary
 *   institutional response to existential threat, rather than as one option
 *   among several or as a secondary implementation of Weimar lessons. The
 *   constraint exhibits tangled coordination-extraction hybrid structure: the
 *   democratic order coordinates on self-defense (suppressing existential
 *   threats) while extracting the cost of reduced political contestation from
 *   banned-party constituencies. Extractiveness has increased over the
 *   measurement interval (0.38 → 0.52) as Karlsruhe's interpretation has
 *   expanded the scope of parties deemed to impair the constitutional order
 *   (most notably in the 2017 NPD case), and theater ratio has risen as the
 *   Weimar historical narrative has become more central to legitimating the
 *   mechanism. Suppression remains high and stable throughout, reflecting the
 *   non-negotiable nature of the enforcement mechanism: banned parties cannot
 *   petition for reconsideration or legislative remedy; only Karlsruhe's
 *   reinterpretation can restore them.
 *
 * KEY AGENTS:
 *   - Banned Parties and Constituencies: Primary victims (powerless/trapped) — face statutory dissolution and political suppression with no democratic remedy; experience pure extraction
 *   - Federal Constitutional Court (Karlsruhe): Primary beneficiary (institutional/arbitrage) — gains legitimacy-granting authority, jurisdictional expansion, and institutional power through exclusive party-ban authority
 *   - Constitutional Order / Democratic System: Mixed beneficiary-victim (organized/constrained) — benefits from threat suppression but pays extraction cost in reduced internal contestation and delegitimization risks
 *   - Mainstream Democratic Parties: Secondary beneficiary (powerful/mobile) — protected from electoral competition by anti-system parties; constrained by own vulnerability to future bans
 *   - Weimar Historical Doctrine and Institutional Memory: Institutional actor (institutional/analytical) — maintains and legitimates the mechanism through historical trauma narrative; benefits from the constraint's persistence as a symbol of constitutional learning
 *   - Anti-System Political Expression: Victim (powerless/trapped) — systematic suppression of ideological alternatives to constitutional democracy; no organizational capacity to contest the suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(militant_democracy__party_ban_instrument, 0.52).
domain_priors:suppression_score(militant_democracy__party_ban_instrument, 0.68).
domain_priors:theater_ratio(militant_democracy__party_ban_instrument, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(militant_democracy__party_ban_instrument, extractiveness, 0.52).
narrative_ontology:constraint_metric(militant_democracy__party_ban_instrument, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(militant_democracy__party_ban_instrument, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(militant_democracy__party_ban_instrument, tangled_rope).
narrative_ontology:human_readable(militant_democracy__party_ban_instrument, "Article 21 Party Ban as Militant Democracy Instrument").
narrative_ontology:topic_domain(militant_democracy__party_ban_instrument, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(militant_democracy__party_ban_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(militant_democracy__party_ban_instrument, '49a67b30-5f9c-40a6-a837-c927a3a96de5').
narrative_ontology:cs_kernel_codification('49a67b30-5f9c-40a6-a837-c927a3a96de5', formalized).
narrative_ontology:cs_authority_grounding('49a67b30-5f9c-40a6-a837-c927a3a96de5', lineage).
narrative_ontology:cs_interpretation_layer_present('49a67b30-5f9c-40a6-a837-c927a3a96de5').
narrative_ontology:cs_reading_relation('49a67b30-5f9c-40a6-a837-c927a3a96de5', militant_democracy__basic_rights_forfeiture, influences).
narrative_ontology:cs_reading_relation('49a67b30-5f9c-40a6-a837-c927a3a96de5', militant_democracy__lessons_of_weimar, coexists_with).
narrative_ontology:cs_axiom('49a67b30-5f9c-40a6-a837-c927a3a96de5', foundational, constitutional_order_self_defense_permissible).
narrative_ontology:cs_axiom_status(constitutional_order_self_defense_permissible, holdable).
narrative_ontology:cs_axiom_grounding('49a67b30-5f9c-40a6-a837-c927a3a96de5', constitutional_order_self_defense_permissible, deontological).
narrative_ontology:cs_axiom('49a67b30-5f9c-40a6-a837-c927a3a96de5', foundational, karlsruhe_sole_authority_for_constitutional_existentialism).
narrative_ontology:cs_axiom_status(karlsruhe_sole_authority_for_constitutional_existentialism, holdable).
narrative_ontology:cs_axiom_grounding('49a67b30-5f9c-40a6-a837-c927a3a96de5', karlsruhe_sole_authority_for_constitutional_existentialism, conventional).
narrative_ontology:cs_reference_frame('49a67b30-5f9c-40a6-a837-c927a3a96de5', weimar_republic_failure_as_constitutional_lesson).
narrative_ontology:cs_drift_state('49a67b30-5f9c-40a6-a837-c927a3a96de5', contemporary_post_cold_war_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('49a67b30-5f9c-40a6-a837-c927a3a96de5', '').
narrative_ontology:cs_kernel_id(militant_democracy__party_ban_instrument, militant_democracy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(militant_democracy__party_ban_instrument, constitutional_order_stability).
narrative_ontology:constraint_beneficiary(militant_democracy__party_ban_instrument, federalist_constitutional_court).
narrative_ontology:constraint_victim(militant_democracy__party_ban_instrument, banned_parties).
narrative_ontology:constraint_victim(militant_democracy__party_ban_instrument, banned_party_voters).
narrative_ontology:constraint_victim(militant_democracy__party_ban_instrument, anti_system_political_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BANNED PARTY VOTERS (SNARE) — Trapped agents experience pure extraction: political voice and organizational capacity are statutorily suppressed with no legal recourse. Exit from the constraint requires abandoning the political commitment itself. The party ban mechanism permits no appeal, no legislative remedy, no democratic reversal — only Karlsruhe's judicial authority. Maximum suppression and no meaningful exit option.
constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BANNED PARTY ORGANIZATION (SNARE) — Banned parties face statutory dissolution, asset confiscation, and prohibition on political activity. Leadership faces criminal liability for attempting to reorganize. The constraint operates as pure extraction with minimal coordination benefit — the party exists to contest the constitutional order, and the ban mechanism is designed to prevent that contestation entirely. High suppression; constrained rather than trapped exit (leadership can flee, liquidate, or submit to exile, but organizational survival is foreclosed).
constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL CONSTITUTIONAL COURT (ROPE) — Karlsruhe's judicial authority benefits from Article 21 as a coordination mechanism: the power to ban parties seeking to impair the free democratic order concentrates legitimacy-granting authority in the court itself. The court experiences the constraint as coordination (defining and defending constitutional order boundaries) with net institutional benefit (expanded jurisdictional scope and political authority). Arbitrage exit: Karlsruhe can modify interpretation or decline to apply the ban instrument. Low experienced extraction; net beneficiary.
constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL ORDER (TANGLED ROPE) — The democratic system benefits from party-ban coordination (suppression of existential threats to the order) but at extractive cost (reduced internal contestation, compressed political space). The order experiences both coordination (protecting itself from self-negation) and extraction (from actors ideologically committed to preserving the system). Constrained exit: the order cannot indefinitely sustain bans without legitimacy erosion; generational renewal requires some expansion of political space or risk revolutionary challenge.
constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MAINSTREAM DEMOCRATIC PARTIES (TANGLED ROPE) — Mainstream parties experience mixed coordination and extraction. Coordination: party-ban mechanism protects their democratic market share by removing existential competitors. Extraction: the mechanism also constrains their own ideological drift — they must remain within constitutional bounds or risk future bans. Mobile exit (parties can migrate policy positions or appeal to different constituencies) but constrained by reputational and legal risk. Medium experienced extraction; both beneficiary and victim of the coordination function.
constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: WEIMAR INSTITUTIONAL MEMORY (PITON) — The party-ban instrument is maintained substantially through historical narrative (Weimar's failure taught us not to tolerate enemies of democracy) rather than through demonstrated functional necessity in the contemporary constitutional order. The theatrical invocation of Weimar risk sustains the ban mechanism even as actual existential threats have receded. Theater ratio reflects that the doctrine's force derives from historical trauma narrative, not from ongoing empirical assessment of threat. The mechanism persists as institutional inertia and legitimacy ritual.
constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, some democracies claim that self-preservation against existential threat is an immutable principle: a system permitting agents dedicated to its destruction would be logically incoherent. The party-ban mechanism instantiates a supposed natural law of democratic self-defense. However, the structural data reveals this as a false summit: the 'immutable' principle is constituted by German constitutional doctrine (lineage, extracted from Weimar trauma), not by universal logical necessity. Comparative evidence shows stable democracies without party-ban mechanisms (US, UK, Canada) managing existential threats through other means.
constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(militant_democracy__party_ban_instrument_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(militant_democracy__party_ban_instrument, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(militant_democracy__party_ban_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(militant_democracy__party_ban_instrument, TR),
    TR >= 0.70.

:- end_tests(militant_democracy__party_ban_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting asymmetric suppression of one side of the political spectrum (anti-system parties) justified through a concentrated institutional authority (Karlsruhe). The mechanism suppresses political organization (a core democratic right) with no compensating benefit to the victims. The 0.52 value reflects that the constraint is not pure extraction (the order does provide coordination benefits to itself, even if these are self-interested), but the extraction component is substantial and irreversible without Karlsruhe's reinterpretation. Rising trajectory (0.38 → 0.52) reflects expanded application scope, most notably the 2017 NPD decision which established that party-ban applies to parties merely opposing constitutional principles, not just those actively working to overthrow the order. Suppression (0.68): High and rising. The constraint permits no legal remedy through legislative action, no administrative appeal process, and no democratic reversal mechanism — only Karlsruhe's discretionary reinterpretation can restore a banned party. The rising trajectory reflects the 2017 NPD decision's expansion of the scope of bannable parties (from those actively seeking to overturn the order to those merely opposing constitutional principles), increasing the risk surface for any party ideologically critical of the existing constitutional framework. Theater ratio (0.45): Moderate but rising. The mechanism's functional necessity is empirically contestable — stable democracies without party-ban mechanisms manage existential threats through other means (US, UK, Canada). The rising theater ratio reflects increasing reliance on the Weimar historical narrative to legitimate continued application, as contemporary existential threats have arguably receded since the Cold War. The 0.45 value reflects that Article 21 is still primarily functional (Karlsruhe does conduct substantive review of party platforms and activities) rather than purely performative, but the rising theater component (30-year trajectory approaching 0.45) suggests the mechanism is increasingly maintained through institutional inertia and historical narrative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range is extreme, spanning from snare (banned parties experience pure extraction with no exit) to rope (Karlsruhe experiences coordination benefit). The banned-party perspective classifies as snare because the victims are trapped: they cannot appeal to any democratic remedy, cannot reorganize under a different name without legal liability, and cannot contest the court's characterization of them as threatening the constitutional order. Karlsruhe's perspective is rope because the court benefits from exclusive authority and experiences the constraint as coordination (defining constitutional boundaries). The constitutional order's perspective is tangled_rope because it gains self-protection but pays extraction cost (reduced contestation, legitimacy erosion). The mainstream parties' perspective is tangled_rope because they benefit from removal of anti-system competitors but are themselves vulnerable to future bans. The Weimar institutional-memory perspective is piton because the mechanism is increasingly maintained through historical trauma narrative rather than contemporary threat assessment. The analytical observer's perspective risks classifying as mountain (self-preservation is a natural law of democracy) but the structural data reveals this as a false summit: other democracies operate without party-ban mechanisms, and the mechanism's scope has expanded beyond existential threats (from active overthrow-seeking to mere ideological opposition) in ways that suggest it is less about self-defense and more about institutionalizing a particular conception of constitutional order.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options for each perspective. Banned parties are both powerless and trapped (no exit option from suppression except abandonment of their political identity), producing high d and high f(d) → high experienced extraction. Karlsruhe is institutional with arbitrage exit (the court can modify interpretation or decline to apply the ban), producing low d and low f(d) → low or negative experienced extraction (institutional benefit). The constitutional order is organized with constrained exit (it cannot indefinitely sustain bans without legitimacy erosion), producing medium d → medium f(d) → medium experienced extraction. Mainstream parties are powerful with mobile exit (they can migrate policy positions or appeal to different constituencies), producing medium-high d → medium-high f(d) → medium experienced extraction. This directionality distribution explains why the constraint classifies as snare for victims, rope for beneficiaries, and tangled_rope for mixed actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not resolve into a single type but rather instantiates a legitimacy conflict between the coordination function (protecting the order from self-negating threats) and the extraction mechanism (suppressing political competition to which alternative democratic theories are committed). Mandatrophy is resolved by recognizing that the constraint is genuinely tangled: it coordinates on self-defense while extracting from anti-system political expression. The false summit perspective (mountain / natural law of democracy) is diagnosed and rejected by the structural data: comparative evidence shows stable democracies without party-ban mechanisms, and the mechanism's application scope has expanded beyond what self-defense logic entails. The piton perspective (institutional inertia via Weimar narrative) is also legitimate — the mechanism is increasingly maintained through historical trauma narrative as contemporary existential threats have receded. The constraint's classification as tangled_rope is justified because Article 21 genuinely serves both coordination (defending the order) and extraction (suppressing anti-system parties), and neither function can be cleanly separated from the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_threat_definition,
    'What empirical or doctrinal standard defines a party as seeking to impair the free democratic order? Does the standard enable principled application or permit ideological suppression of legitimate opposition?',
    'Historical analysis of Karlsruhe party-ban decisions (NPD, 2017); examination of criteria applied; comparison with parties that narrowly escaped bans; assessment of whether criteria track objective anti-system activity or ideological distance from the court''s conception of constitutional order.',
    'If criteria are principled and narrow: snare classification holds; extraction is justified as self-defense. If criteria are ideological or shifting: classification shifts toward snare-as-pure-extraction; the mechanism becomes a weapon for suppressing legitimate opposition cloaked in constitutional language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(existential_threat_definition, empirical, 'Criteria and application pattern for existential threat determination').

omega_variable(
    weimar_autopsy_vs_contemporary_necessity,
    'Is the party-ban instrument grounded in analysis of contemporary democratic fragility, or does it represent institutionalized trauma response to Weimar''s specific failure mode that may not generalize to modern party systems?',
    'Comparative constitutional analysis: do stable democracies without party-ban mechanisms face equivalent existential risks? Do contemporary party-system dynamics show the same institutional weaknesses that enabled Nazi electoral rise? Assessment of whether the mechanism addresses the actual threat vector or performs a ritualized autopsy.',
    'If trauma-driven but functionally unnecessary: the piton classification is confirmed (institutional inertia masquerading as doctrine). If genuinely necessary: the tangled_rope classification is reinforced and suppression is justified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weimar_autopsy_vs_contemporary_necessity, conceptual, 'Whether party-ban necessity is grounded in contemporary analysis or institutionalized historical trauma').

omega_variable(
    karlsruhe_authority_legitimacy_circularity,
    'Does Article 21 concentrate party-ban authority in Karlsruhe through a legitimate delegation of constitutional interpretation, or does it create a self-reinforcing loop where the court defines threats to the order and then eliminates them, increasing its own institutional power?',
    'Structural analysis of Karlsruhe''s power expansion over successive party-ban decisions; examination of whether the court''s conception of the constitutional order has drifted toward protecting its own jurisdictional scope; comparison with separation-of-powers doctrine in other democracies.',
    'If legitimate delegation: rope/tangled_rope classification from Karlsruhe''s perspective is justified institutional coordination. If self-reinforcing loop: the mechanism is partially extractive for Karlsruhe itself — the court benefits from power concentration even as it performs constitutional guardianship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(karlsruhe_authority_legitimacy_circularity, conceptual, 'Whether party-ban authority delegation is legitimacy-grounded or self-reinforcing').

omega_variable(
    counter_reading_basic_rights_forfeiture,
    'Could Article 18 (basic rights forfeiture) accomplish the same constitutional protection without the full party-ban mechanism of Article 21? If so, why has Article 18 never been successfully applied while Article 21 remains the operative instrument?',
    'Doctrinal comparison of Article 18 and 21 application thresholds and procedures; analysis of why Karlsruhe chose party-ban over rights-forfeiture in historical cases; assessment of whether Article 18''s disuse reflects its insufficiency or reflects path dependence and institutional preference for Article 21.',
    'If Article 18 could substitute: Article 21 represents extractive escalation (suppression of entire party rather than individual rights). If Article 18 is genuinely insufficient: Article 21 is necessary coordination mechanism. The sibling reading ''basic_rights_forfeiture'' gains or loses plausibility depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_reading_basic_rights_forfeiture, empirical, 'Doctrinal and functional comparison of Articles 18 and 21').

omega_variable(
    this_reading_vs_sibling_weimar_lessons,
    'Is Article 21 (party-ban instrument) the core institutional response to Weimar''s failure, or is it one implementation among several equally valid doctrinal responses (e.g., basic rights forfeiture, defensive democracy doctrine, institutional checks)? Does the Weimar-lessons reading foreclose the party-ban instrument or coexist with it?',
    'Historical analysis of Bonn constitutional debates (1948–1949); examination of whether party-ban was the intentional centerpiece or one option among several; assessment of whether Weimar autopsy logically entails party-ban or merely permits it.',
    'If party-ban was the intended core response: this reading is foundational to the Weimar-lessons reading and they coexist. If party-ban was one option among several: alternative readings (basic_rights_forfeiture, institutional_checks) remain live and this reading does not uniquely embody Weimar lessons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_sibling_weimar_lessons, empirical, 'Whether Article 21 is the core or one implementation of Weimar-lessons doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(militant_democracy__party_ban_instrument, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdpb_tr_t0, militant_democracy__party_ban_instrument, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mdpb_tr_t15, militant_democracy__party_ban_instrument, theater_ratio, 15, 0.38).
narrative_ontology:measurement(mdpb_tr_t30, militant_democracy__party_ban_instrument, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(mdpb_be_t0, militant_democracy__party_ban_instrument, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mdpb_be_t15, militant_democracy__party_ban_instrument, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(mdpb_be_t30, militant_democracy__party_ban_instrument, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mdpb_su_t0, militant_democracy__party_ban_instrument, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(mdpb_su_t15, militant_democracy__party_ban_instrument, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(mdpb_su_t30, militant_democracy__party_ban_instrument, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(militant_democracy__party_ban_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(militant_democracy__party_ban_instrument, militant_democracy__basic_rights_forfeiture).
narrative_ontology:affects_constraint(militant_democracy__party_ban_instrument, militant_democracy__lessons_of_weimar).

% DUAL FORMULATION NOTE:
% The militant democracy kernel decomposes into three structurally distinct readings: basic_rights_forfeiture (Article 18 approach), lessons_of_weimar_reading (historical-narrative approach), and party_ban_instrument (this story, Article 21 approach). Each reading has different extractiveness values, different beneficiary/victim structures, and different justificatory narratives. The readings share a common kernel (defensive democracy principle) but diverge on the primary institutional mechanism and scope. This story links to its sibling readings via network.affects_constraints. The party_ban_instrument reading influences (but does not foreclose) the basic_rights_forfeiture reading: if party-ban is the primary mechanism, basic rights forfeiture becomes secondary or supplementary, changing the justificatory force of Article 18.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(militant_democracy__party_ban_instrument, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
