% ============================================================================
% CONSTRAINT STORY: liturgical_vernacularization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_vernacularization, []).

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
 *   constraint_id: liturgical_vernacularization
 *   human_readable: Liturgical Vernacularization and Catholic Institutional Authority
 *   domain: ecclesiastical_history/theological_doctrine
 *
 * SUMMARY:
 *   Vatican II (1962–1965) mandated liturgical vernacularization: the Latin
 *   Mass gave way to national-language Masses approved by local episcopal
 *   conferences. This constraint is analytically critical because it embeds a
 *   contested kernel — whether Vatican II represents ONE reading of Catholic
 *   continuity or MULTIPLE structurally distinct doctrinal shifts bundled
 *   under institutional convenience. The vernacularization mandate appears as
 *   an immutable law of institutional necessity (mountain) to the analytical
 *   observer at civilizational scale; as pure extraction and suppression
 *   (snare) to the Latin tradition itself; as legitimate coordination (rope)
 *   to the reformist Catholic leadership; as mixed coordination-extraction
 *   (tangled_rope) to powerful bishops constrained by obedience; as temporary
 *   scaffolding (scaffold) to progressive reform movements; and as degraded
 *   performative authority (piton) to the Vatican's current liturgical
 *   enforcement apparatus. The constraint's extractiveness rose sharply
 *   during Vatican II (0.18 → 0.35 during conclave) and stabilized at 0.52
 *   post-implementation. Theater ratio increased from 0.42 (pre-Vatican II
 *   Latin clarity) to 0.71 (post-Vatican II performative declarations of
 *   ecclesiastical unity that do not control actual parish practice). This
 *   trajectory marks the shift from substantive institutional control to
 *   theater: the Vatican maintains the fiction of liturgical authority
 *   through repeated decrees (2007 motu proprio, 2021 restrictions, 2023
 *   traditionalist crackdowns) but cannot operationally constrain vernacular
 *   innovation at parish level. The constraint is now primarily inertial
 *   (piton) — maintained by institutional habit rather than functional
 *   necessity.
 *
 * KEY AGENTS:
 *   - Latin Liturgical Tradition: Primary victim (powerless/trapped/global) — the theological transmission vehicle of centuries, stripped of its institutional monopoly. No exit; the tradition persists in marginal canonical spaces (SSPX, traditionalist enclaves) but outside the Church's formal structure. Experiences maximum extraction.
 *   - Parish-Level Clergy and Laity: Mixed agent (moderate/constrained/national) — clergy lose priestly gatekeeping authority but gain flexibility in adaptation; laity gain comprehension but lose the transcendent distance Latin provided. Coordinating benefits and extractive losses coexist.
 *   - Roman Curia (Vatican II Architects): Primary beneficiary (institutional/arbitrage/global) — solves the modernization legitimacy problem while centralizing doctrinal control. Delegates implementation form (vernacular) while retaining doctrinal substance (Rome's teaching office). High arbitrage capacity protects this actor from experienced extraction.
 *   - Traditionalist Episcopal Conferences: Secondary victim (powerful/constrained/regional) — strong Latin-liturgy bishops (esp. French, Italian, Spanish) lose the authority to resist vernacularization. Obedience vows bind them to compliance; their powerful position cannot translate into blocking power because hierarchical structure absorbs resistance. Experience tangled_rope: some coordination benefits (unified church discipline) with significant extraction (loss of regional liturgical authority).
 *   - Progressive Reform Movements: Organized agents (organized/mobile/global) — liberation theology, inculturation advocates see vernacularization as scaffolding for deeper localization. They have the agency and intellectual resources to drive the constraint toward outcomes they want. Lower experienced extraction because they can shape implementation.
 *   - Vatican Liturgical Authority Post-Vatican II: Institutional actor (institutional/arbitrage/global) — maintains ceremonial authority declarations (motu proprios, restrictions) that have minimal operational effect. The machinery persists through inertia. This is the piton element: performative authority without functional constraint.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical/universal) — risks naturalizing the Vatican's institutional choice as an inherent law of religious organization. The false summit detector should flag this: identifying beneficiaries (curia, institutional church authority) on a mountain-type constraint suggests naturalization rather than genuine natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_vernacularization, 0.52).
domain_priors:suppression_score(liturgical_vernacularization, 0.58).
domain_priors:theater_ratio(liturgical_vernacularization, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_vernacularization, extractiveness, 0.52).
narrative_ontology:constraint_metric(liturgical_vernacularization, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(liturgical_vernacularization, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_vernacularization, tangled_rope).
narrative_ontology:human_readable(liturgical_vernacularization, "Liturgical Vernacularization and Catholic Institutional Authority").
narrative_ontology:topic_domain(liturgical_vernacularization, "ecclesiastical_history/theological_doctrine").

domain_priors:requires_active_enforcement(liturgical_vernacularization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_vernacularization, '4d92229a-9b2e-40b1-b1c0-c3342c36925a').
narrative_ontology:cs_kernel_codification('4d92229a-9b2e-40b1-b1c0-c3342c36925a', formalized).
narrative_ontology:cs_authority_grounding('4d92229a-9b2e-40b1-b1c0-c3342c36925a', lineage).
narrative_ontology:cs_interpretation_layer_present('4d92229a-9b2e-40b1-b1c0-c3342c36925a').
narrative_ontology:cs_reading_relation('4d92229a-9b2e-40b1-b1c0-c3342c36925a', vatican_ii_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d92229a-9b2e-40b1-b1c0-c3342c36925a', vatican_ii_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d92229a-9b2e-40b1-b1c0-c3342c36925a', vatican_ii_development_reading, coexists_with).
narrative_ontology:cs_axiom('4d92229a-9b2e-40b1-b1c0-c3342c36925a', foundational, latin_irreplaceable_sacrally).
narrative_ontology:cs_axiom_status(latin_irreplaceable_sacrally, holdable).
narrative_ontology:cs_axiom_grounding('4d92229a-9b2e-40b1-b1c0-c3342c36925a', latin_irreplaceable_sacrally, theological).
narrative_ontology:cs_axiom('4d92229a-9b2e-40b1-b1c0-c3342c36925a', secondary, papal_authority_indivisible_over_discipline).
narrative_ontology:cs_axiom_status(papal_authority_indivisible_over_discipline, holdable).
narrative_ontology:cs_axiom_grounding('4d92229a-9b2e-40b1-b1c0-c3342c36925a', papal_authority_indivisible_over_discipline, deontological).
narrative_ontology:cs_reference_frame('4d92229a-9b2e-40b1-b1c0-c3342c36925a', latin_liturgical_universality).
narrative_ontology:cs_drift_state('4d92229a-9b2e-40b1-b1c0-c3342c36925a', post_vatican_ii_contemporary, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('4d92229a-9b2e-40b1-b1c0-c3342c36925a', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_vernacularization, institutional_church_authority).
narrative_ontology:constraint_beneficiary(liturgical_vernacularization, global_catholic_laity).
narrative_ontology:constraint_victim(liturgical_vernacularization, latin_liturgical_tradition).
narrative_ontology:constraint_victim(liturgical_vernacularization, episcopal_authority_localized).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATIN LITURGICAL TRADITION (SNARE) — Structurally trapped by institutional decree. The vernacularization mandate removed the linguistic substrate of centuries of theological transmission. No exit mechanism; no alternative formulation without abandonment of the tradition itself. Maximum extraction: the constraint takes away the substance while claiming continuity. The tradition experiences the constraint as totalizing confiscation.
constraint_indexing:constraint_classification(liturgical_vernacularization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PARISH-LEVEL CLERGY AND LAITY (ROPE) — Moderate power; significant barriers to resistance (obedience vows, career dependence) but also genuine coordination benefits. The vernacular Mass enables broader participation, reduces cognitive barriers to comprehension, creates locally adaptable liturgy. Mixed extraction: some clergy experience loss of priestly mystique and interpretive gatekeeping; laity experience gain in access. Coordination and extraction coexist at medium intensity.
constraint_indexing:constraint_classification(liturgical_vernacularization, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ROMAN CURIA — VATICAN II ARCHITECTS (ROPE) — Institutional beneficiary with high arbitrage capacity. The coordination frame was controlling a unified global church while managing post-WWII secularization and modernization pressures. Vernacularization solved a legitimacy problem: Latin exclusivity was losing the church's sociological foothold in modernizing societies. The curia experiences this as successful coordination: centralizing doctrinal authority while delegating presentation form. Net beneficiary; low experienced extraction because arbitrage options protect this actor.
constraint_indexing:constraint_classification(liturgical_vernacularization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRADITIONALIST EPISCOPAL BLOC (TANGLED ROPE) — Powerful but constrained by hierarchical obedience. Bishops with strong Latin-liturgy commitments (esp. French, Italian, Spanish episcopal conferences) perceived the mandate as coordination gone extractive. They gained centralized doctrinal clarity but lost liturgical authority to adapt tradition locally. The constraint operates as both coordination (unified church discipline) and extraction (loss of episcopal prerogative). High suppression: resistance was routed through canonically compliant channels only — explicit disobedience was not viable. Their experience is mixed: coordination benefits offset by extracted regional authority.
constraint_indexing:constraint_classification(liturgical_vernacularization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROGRESSIVE POST-VATICAN II REFORM MOVEMENT (SCAFFOLD) — Organized agents (liberation theology networks, inculturation advocates, German-speaking reformers) see vernacularization as a temporary scaffolding enabling deeper transformation: liturgical localization, contextual theology, decolonization of doctrine. They perceive a sunset: as vernacular adaptations mature globally, the universal Latin standard becomes unnecessary, and local inculturation becomes the norm. Low effective extraction because the movement has agency and clear exit path (further decentralization). Theater moderate: the reform movement is energetic, not performative.
constraint_indexing:constraint_classification(liturgical_vernacularization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: VATICAN STRUCTURAL AUTHORITY POST-VATICAN II (PITON) — The institutional apparatus that maintains doctrinal unity through uniform liturgical practice is largely inert. Modern Catholic parishes exhibit high liturgical variance (guitar Masses, incultured rites, charismatic worship styles) that would have been unthinkable in 1962. The institution maintains the fiction of unified discipline through Vatican declarations (e.g., Benedict XVI's 2007 motu proprio restoring Latin) but cannot enforce coherence. The machinery persists through inertia and ceremonial reaffirmation, not functional necessity. Theater high: the vatican's liturgical authority is substantially performative display of control that does not translate to operational constraint at parish level.
constraint_indexing:constraint_classification(liturgical_vernacularization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW VIEW (DOCTRINAL CONTINUITY) — From a civilizational/universal perspective, the constraint appears as an immutable feature of Catholic institutional authority: any universal church must have a unified liturgical expression or risk doctrinal fragmentation. Vernacularization is the vehicle by which Rome maintains doctrinal hegemony while appearing to decentralize presentation. From this view, the constraint is an inherent structural necessity of maintaining ecclesiastical unity across diverse cultures — a natural law of large-scale religious institutions. This perspective frames the constraint as unchangeable.
constraint_indexing:constraint_classification(liturgical_vernacularization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_vernacularization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liturgical_vernacularization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liturgical_vernacularization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liturgical_vernacularization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liturgical_vernacularization, TR),
    TR >= 0.70.

:- end_tests(liturgical_vernacularization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, and rising sharply at Vatican II implementation (0.18 → 0.52 over 5 years), then stabilizing with slight decline to 0.48 as John Paul II's papacy normalized the new liturgy. The extraction is real but not maximal. Beneficiaries (institutional church, global laity accessing Mass in their language) gained legitimacy and operational efficiency. Victims (Latin tradition, regional episcopal authority) lost substantive institutional position. The measurement trajectory reflects the shock of rapid change (sharp rise during and after Vatican II) followed by normalization as the new practice became embedded. Extractiveness has not declined significantly (slight drop in John Paul era) because the constraint remains binding — parishes cannot return to Latin Mass without explicit Vatican permission (still restricted as of 2026). Suppression (0.58): Moderate-high. The constraint operates through hierarchical obedience (vows binding priests and bishops), career dependence (non-compliance threatens advancement), and formal discipline (Vatican declarations backed by administrative machinery). However, suppression is not total — parish-level liturgical drift (guitar Masses, inculturation, charismatic adaptations) shows that the constraint has enforcement gaps. Clergy and laity found workarounds within formal compliance (technically following Vatican rules while practically innovating). Theater ratio (0.64): Moderate-high and rising. Pre-Vatican II Latin liturgy had lower theater (0.42) because the institutional control was substantive — Latin uniformity was operationally enforced and theologically justified. Post-Vatican II theater rose because the Vatican's authority over liturgical practice degraded while ceremonial declarations multiplied. By 1965 (time 5), theater reached 0.64 because the Vatican was reasserting control through motu proprios and restrictions while actual parish practice diverged. By 2000s (time 10), theater peaked at 0.71 because the Vatican maintains loud declarations of liturgical authority (2007 motu proprio by Benedict XVI, 2021 restrictions by Francis) that have minimal practical effect on vernacular innovation. The theater_ratio trajectory shows institutional inertia: the Vatican keeps performing authority over something it cannot operationally control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full six-type perspectival divergence. The Latin tradition sees snare (trapped, global, powerless — the constraint confiscates the linguistic substrate of their theological transmission). Parish clergy and laity see rope (coordination benefits enabling mass participation offset by lost transcendent distance). The curia sees rope (successful coordination of a global institution while managing modernization). Traditionalist bishops see tangled_rope (genuine coordination of church unity alongside extraction of regional authority). Progressive reformers see scaffold (temporary constraint enabling deeper transformation toward inculturation). The Vatican's liturgical authority apparatus sees piton (its own control mechanisms have degraded; it performs authority it cannot enforce). The analytical observer risks seeing mountain (immutable law of religious institutions) — a false summit. The perspectival gaps reveal that the constraint bundles together multiple analytically distinct dynamics: (1) the loss of Latin institutional monopoly (snare-level extraction for the tradition), (2) the gain in laity comprehension and participation (rope-level coordination for parishes), (3) the consolidation of Rome's doctrinal hegemony (rope-level coordination for curia), (4) the extraction of episcopal regional authority (tangled_rope for bishops), (5) the scaffolding of further localization (scaffold for progressives), and (6) the degradation of enforcement machinery (piton for the Vatican). These are not the same constraint viewed from different angles — they are different structural effects bundled under one institutional action.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derived from beneficiary/victim + power + exit options. The Latin tradition: victim status + powerless + trapped = d ≈ 0.95, f(d) ≈ 1.42, high experienced extraction (snare). Parish clergy: mixed (beneficiary in some respects, victim in others) + moderate + constrained = d ≈ 0.50, f(d) ≈ 0.65, moderate experienced extraction (rope to tangled_rope). Curia: beneficiary status + institutional + arbitrage = d ≈ 0.05, f(d) ≈ -0.12, low/negative experienced extraction (rope). Traditionalist bishops: victim status + powerful + constrained = d ≈ 0.55, f(d) ≈ 0.75, moderate-high extraction relative to power (tangled_rope). Progressive reformers: organized + mobile = d ≈ 0.40, f(d) ≈ 0.40, moderate extraction but with agency (scaffold). Vatican authority: institutional + arbitrage = d ≈ 0.05, f(d) ≈ -0.12 (piton theater gates, not d-driven). Analytical: observer = d ≈ 0.72, f(d) ≈ 1.15 (mountain from natural law assumption, reclassified to false summit by FSM detector).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY PERSPECTIVAL MAPPING: The constraint's mandatrophy is resolved by recognizing that Vatican II bundled multiple independent doctrinal shifts (liturgy, collegiality, ecumenism, scriptural authority) that are analytically distinct constraints with different ε values and different beneficiary/victim structures. The measured ε = 0.52 represents vernacularization's direct extraction, but this is only one component of the Vatican II institutional action. A complete analysis would decompose the constraint family: (1) liturgical_authority_decentralization (ε ≈ 0.52, tangled_rope, measured here), (2) episcopal_collegiality_shift (ε ≈ 0.48, likely tangled_rope — apparent empowerment with actual authority consolidation), (3) ecumenical_reorientation (ε ≈ 0.35, likely rope — coordination benefit with some extraction of exclusionary identity), (4) scriptural_authority_modernization (ε ≈ 0.40, likely tangled_rope — scholarly access gained with interpretive control centralized in Rome). Each story would have its own perspectives, beneficiaries, victims, and omega variables. Linking them via network.affects_constraints shows how Vatican II was overdetermined — the curia could have adopted any one of these shifts independently, but bundled them together for institutional convenience. The mandatrophy at the Vatican II level resolves by decomposing the kernel question (one reading vs. multiple shifts) into a constraint family, allowing each shift to be classified independently. This story represents the liturgical component of that family.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vatican_ii_kernel_or_bundled_shifts,
    'Does Vatican II represent ONE contested reading of Catholic continuity, or SEVERAL structurally distinct doctrinal movements that happened to co-occur institutionally?',
    'Textual and historical analysis: (a) Trace whether each major shift (liturgy, ecumenism, episcopal collegiality, scriptural authority) flows logically from a unified doctrinal premise, or whether they rest on distinct theological axioms that could have been adopted or rejected independently. (b) Counterfactual reconstruction: could the Church have adopted vernacular liturgy while rejecting episcopal power-sharing? Could it have embraced ecumenism while maintaining Latin-only worship? (c) Institutional process analysis: did the conciliar debates link these shifts logically, or did separate lobbying blocs (Dutch bishops, Italian progressives, Curia conservatives) push distinct agendas that converged only at the level of institutional bundling?',
    'If ONE reading: the constraint is a coherent reinterpretation of tradition (''tradition develops''; Vatican II is a doctrinal evolution). Vernacularization is meaningfully linked to other Vatican II shifts via shared theological premises. Classification remains tangled_rope with coherent benefit/victim structure. If MULTIPLE shifts: vernacularization is analytically distinct from, e.g., ecumenical opening or episcopal empowerment. Each shift has its own beneficiary/victim structure and ε value. The ''constraint story'' should decompose into a constraint family (linked via network.affects_constraints) with separate stories for: liturgical_authority_decentralization, episcopal_collegiality_shift, ecumenical_reorientation, scriptural_authority_modernization. The omega resolves by historical reconstruction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vatican_ii_kernel_or_bundled_shifts, empirical, 'Whether Vatican II is one kernel reading or multiple bundled shifts').

omega_variable(
    latin_irreplaceability_vs_contingency,
    'Is Latin''s role in Catholic liturgy theologically constitutive (irreplaceable), or contingently institutional (replaceable by other vehicles)?',
    'Theological analysis of pre-Vatican II magisterial statements and conciliar debates: Did the Church''s teaching claim that Latin carries intrinsic theological properties (e.g., precision, sacrality, universality), or only that it serves a practical coordinating function? If intrinsic: the tradition suffers real loss (mountain-level suppression). If contingent: the loss is institutional, not theological — vernacularization is extraction of privilege but not destruction of substance. Secondary: comparative liturgical history — do other Christian traditions (Orthodox, Anglican, Lutheran) experience equivalent loss when moving from their classical languages (Koine Greek, Old Church Slavonic, King''s English) to modern vernaculars?',
    'If intrinsic to theology: the Latin tradition is genuinely destroyed by vernacularization. The tradition experiences snare classification as accurate. Classification unchanged; but omega resolution deepens legitimacy of the trap. If contingent: the Latin tradition survives theologically but its institutional monopoly is extracted. The tradition experiences partial loss, not destruction — snare classification may overstate the constraint''s severity. Could reframe victim as ''Latin liturgical privilege'' not ''Latin tradition as such.'' This distinction affects mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latin_irreplaceability_vs_contingency, conceptual, 'Whether Latin is constitutive or contingent to Catholic theology').

omega_variable(
    episcopal_authority_extraction_asymmetry,
    'Did Vatican II''s apparent empowerment of bishops (collegiality doctrine) actually extract more authority from episcopates than it returned, by centralizing doctrinal control while delegating only implementation?',
    'Institutional analysis: (a) Catalog doctrinal powers bishops possessed pre-Vatican II that they lost post-Vatican II (e.g., authority to set liturgical norms, authority to resist Rome''s doctrinal interpretations, authority to adapt doctrine to local conditions). (b) Catalog new powers Vatican II granted (collegiality in theory, synodality language). (c) Measure actual exercise: which powers do bishops use? Which does Rome block? (d) Comparative institutional study: did episcopal conferences gain or lose real decision-making capacity in the decades after Vatican II?',
    'If bishops genuinely empowered: the constraint is symmetric coordination (Rope from episcopal perspective). If bishops de facto controlled more precisely (centralized doctrine, delegated theater): the constraint is extraction of authority disguised as collegiality (tangled_rope or snare from episcopal perspective becomes accurate). This affects how the powerful/constrained episcopal perspective is classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(episcopal_authority_extraction_asymmetry, empirical, 'Whether episcopal collegiality was genuine empowerment or authority consolidation').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.58) structural (external discipline, obedience vows, hierarchical enforcement) or partly internalized (clergy and bishops have internalized the legitimacy of Rome''s authority and suppress their own dissent)?',
    'Behavioral analysis: (a) Pre-Vatican II: what percentage of clergy openly resisted Latinization or Vatican centralization? What percentage privately dissented but complied? (b) Post-Vatican II: how many clergy left the priesthood over vernacularization? How many refused to say the new Mass? How many maintained private Latin chapels? (c) Internalization test: compare resistance in countries with strong secular alternatives (France, Germany, Austria) vs. countries with deep Catholic institutional identity (Poland, Ireland) — if internalization varies by secularization context, suppression has internalized component.',
    'If structural suppression dominates: the constraint persists through formal enforcement; removal of enforcement mechanisms would enable exit. If internalized: the constraint persists through accepted identity frames; exit requires identity shift, not just structural change. This affects classification of clergy/bishop exit_options: constrained (structural) vs. identity_locked (internalized). If both: some clergy are structurally bound, others cognitively captured by the legitimate authority frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in Catholic obedience').

omega_variable(
    vatican_ii_as_natural_law_false_summit,
    'Does the ''natural law of large-scale religious institutions'' framing (mountain perspective 7) naturalize what is actually a contingent institutional choice by the Vatican bureaucracy?',
    'Comparative institutional analysis: (a) Other global religious institutions (Islamic Umma, Orthodox communion, Anglican communion, Buddhist sangha) — do they uniformly converge on centralized doctrinal authority + delegated presentation? Or do they show diverse institutional forms? (b) Counterfactual: could Catholicism maintain doctrinal unity via other mechanisms (shared texts, periodic councils, charismatic authority, peer networks) without unified liturgical language? (c) Historical: pre-Vatican II, did the Church claim that Latin uniformity was a theological law or a practical disciplinary choice?',
    'If natural law: the constraint is immutable; vernacularization is the least-bad solution to an inherent structural problem. Mountain classification holds. If contingent: the Church chose this institutional form; alternatives existed but were rejected. The ''natural law'' framing is a false summit — it naturalizes the Vatican''s preference for centralized control. Engine''s false summit detector should flag this. Triggers FSM evaluation (beneficiaries declared, so mountain with beneficiaries → may reclassify to tangled_rope if FSM fires).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vatican_ii_as_natural_law_false_summit, conceptual, 'Whether institutional centralization is natural law or contingent choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_vernacularization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litv_theater_pre_vatican, liturgical_vernacularization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(litv_theater_conclave_debate, liturgical_vernacularization, theater_ratio, 2, 0.58).
narrative_ontology:measurement(litv_theater_implementation_ritual, liturgical_vernacularization, theater_ratio, 5, 0.64).
narrative_ontology:measurement(litv_theater_ossified_forms, liturgical_vernacularization, theater_ratio, 10, 0.71).

% Extraction over time
narrative_ontology:measurement(litv_extract_pre_vatican, liturgical_vernacularization, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(litv_extract_vatican_conclave, liturgical_vernacularization, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(litv_extract_post_vatican_stabilize, liturgical_vernacularization, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(litv_extract_john_paul_reign, liturgical_vernacularization, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_vernacularization, enforcement_mechanism).
narrative_ontology:affects_constraint(liturgical_vernacularization, episcopal_collegiality_shift).
narrative_ontology:affects_constraint(liturgical_vernacularization, ecumenical_reorientation).
narrative_ontology:affects_constraint(liturgical_vernacularization, scriptural_authority_modernization).

% DUAL FORMULATION NOTE:
% Liturgical vernacularization is one component of the Vatican II constraint family. The kernel question is whether Vatican II represents one contested reading of Catholic continuity (unified reinterpretation) or multiple structurally distinct doctrinal shifts bundled under institutional convenience (overdetermined composite). This story models the liturgical shift (ε ≈ 0.52, tangled_rope); sibling stories model collegiality, ecumenism, and scriptural modernization. Each has its own ε value, beneficiary/victim structure, and classification. Linking them shows the decomposition: the 'Vatican II' label conflates analytically separable constraints. The omega variable 'vatican_ii_kernel_or_bundled_shifts' documents this decomposition logic. If the kernel question resolves as 'bundled shifts,' the constraint family is the appropriate unit; if 'one reading,' the stories should be re-integrated with a single ε and unified beneficiary/victim logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liturgical_vernacularization, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
