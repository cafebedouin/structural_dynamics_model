% ============================================================================
% CONSTRAINT STORY: amendment_history__reunification_amendments_1990
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amendment_history__reunification_amendments_1990, []).

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
 *   constraint_id: amendment_history__reunification_amendments_1990
 *   human_readable: Reunification by Accession: Constitutional Absorption Without Co-authorship
 *   domain: political/constitutional_law/german_reunification
 *
 * SUMMARY:
 *   German reunification in 1990 presented a constitutional choice: Article
 *   23 (accession of East into existing West German Basic Law) or Article 146
 *   (all-German constituent assembly to draft a new constitution jointly).
 *   The first path was chosen. This constraint models the structure of that
 *   choice as a reading of a contested constitutional kernel — the meaning
 *   and mechanism of German constitutional authority after 1949. The
 *   accession reading holds that institutional continuity of West German law
 *   was the foundation that made unity possible; alternative readings
 *   (co-authorship, mutual reconstitution, symmetrical merger) hold that
 *   unity required a jointly authored constitution. The readings coexist as
 *   live positions in German political discourse but foreclose each other at
 *   the axiom level: this reading commits to 'institutional continuity
 *   precedence' (the existing order can absorb without losing legitimacy);
 *   alternatives commit to 'co-authorship necessity' (legitimacy requires
 *   joint authorship). This constraint exhibits the structure of a tangled
 *   rope: genuine coordination (reunification required legal continuity and
 *   institutional framework); genuine extraction (East German citizens bore
 *   the asymmetry of absorption without co-authorship; the promised Article
 *   146 moment never materialized); active enforcement (suppression of the
 *   constituent-assembly path required sustained institutional choice and
 *   institutional inertia). The measurements track rising suppression at the
 *   1990 accession moment (when the binary became final) and rising theater
 *   as Article 146 became a ceremonial symbol rather than operative
 *   mechanism.
 *
 * KEY AGENTS:
 *   - West German Federal Government: Beneficiary (institutional/arbitrage) — retains legal-institutional continuity; solves coordination problem rapidly without constituent-assembly delay; experiences accession as coordination enabling integration
 *   - East German Citizens/GDR Population: Primary victim (powerless/trapped) — offered binary choice (accede or remain separate); no seat at constitutional drafting; no genuine exit; extracted the transformative cost of absorption without co-authorship
 *   - East German State Apparatus (Modrow/de Maizière Governments): Secondary beneficiary-victim (moderate/constrained) — gained access to resources and Western integration; experienced constrained negotiation within time-pressured binary; some agency in compensation terms; suppressed alternative pathways
 *   - All-German Constitutional Vision/Article 146 Proponents: Victim of foreclosure (institutional/constrained) — the deferred constituent-assembly path became ceremonial rather than operative; coordination function (joint authorship as legitimacy) was suppressed; the vision remained textually in the Basic Law but functionally closed
 *   - West German Legal Establishment/Constitutional Court: Beneficiary (institutional/arbitrage) — authority and interpretation capacity reinforced by continuity model; construe Article 23 as valid path; interpret Article 146 as foreclosed by historical events
 *   - Analytical Observer: Sees structural choice masked as necessity (analytical/analytical) — reveals that 'geopolitical necessity' naturalizes an institutional choice with distributional consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amendment_history__reunification_amendments_1990, 0.58).
domain_priors:suppression_score(amendment_history__reunification_amendments_1990, 0.65).
domain_priors:theater_ratio(amendment_history__reunification_amendments_1990, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amendment_history__reunification_amendments_1990, extractiveness, 0.58).
narrative_ontology:constraint_metric(amendment_history__reunification_amendments_1990, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(amendment_history__reunification_amendments_1990, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amendment_history__reunification_amendments_1990, tangled_rope).
narrative_ontology:human_readable(amendment_history__reunification_amendments_1990, "Reunification by Accession: Constitutional Absorption Without Co-authorship").
narrative_ontology:topic_domain(amendment_history__reunification_amendments_1990, "political/constitutional_law/german_reunification").

domain_priors:requires_active_enforcement(amendment_history__reunification_amendments_1990).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amendment_history__reunification_amendments_1990, 'f7a10a82-6d18-4692-b0ae-1bae621080e5').
narrative_ontology:cs_kernel_codification('f7a10a82-6d18-4692-b0ae-1bae621080e5', fixed_text).
narrative_ontology:cs_authority_grounding('f7a10a82-6d18-4692-b0ae-1bae621080e5', extraction).
narrative_ontology:cs_interpretation_layer_present('f7a10a82-6d18-4692-b0ae-1bae621080e5').
narrative_ontology:cs_reading_relation('f7a10a82-6d18-4692-b0ae-1bae621080e5', amendment_history__asylum_compromise_1993, influences).
narrative_ontology:cs_reading_relation('f7a10a82-6d18-4692-b0ae-1bae621080e5', amendment_history__debt_brake_2009, influences).
narrative_ontology:cs_reading_relation('f7a10a82-6d18-4692-b0ae-1bae621080e5', amendment_history__emergency_acts_1968, coexists_with).
narrative_ontology:cs_reading_relation('f7a10a82-6d18-4692-b0ae-1bae621080e5', amendment_history__rearmament_1956, coexists_with).
narrative_ontology:cs_axiom('f7a10a82-6d18-4692-b0ae-1bae621080e5', foundational, institutional_continuity_precedence).
narrative_ontology:cs_axiom_status(institutional_continuity_precedence, holdable).
narrative_ontology:cs_axiom_grounding('f7a10a82-6d18-4692-b0ae-1bae621080e5', institutional_continuity_precedence, conventional).
narrative_ontology:cs_axiom('f7a10a82-6d18-4692-b0ae-1bae621080e5', foundational, temporal_necessity_justifies_procedural_deviation).
narrative_ontology:cs_axiom_status(temporal_necessity_justifies_procedural_deviation, holdable).
narrative_ontology:cs_axiom_grounding('f7a10a82-6d18-4692-b0ae-1bae621080e5', temporal_necessity_justifies_procedural_deviation, empirically_contingent).
narrative_ontology:cs_reference_frame('f7a10a82-6d18-4692-b0ae-1bae621080e5', west_german_constitutional_authority).
narrative_ontology:cs_drift_state('f7a10a82-6d18-4692-b0ae-1bae621080e5', contemporary_post_reunification, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('f7a10a82-6d18-4692-b0ae-1bae621080e5', '').
narrative_ontology:cs_kernel_id(amendment_history__reunification_amendments_1990, amendment_history).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amendment_history__reunification_amendments_1990, west_german_institutional_continuity).
narrative_ontology:constraint_beneficiary(amendment_history__reunification_amendments_1990, federal_republic_legal_framework).
narrative_ontology:constraint_victim(amendment_history__reunification_amendments_1990, east_german_co_authorship_capacity).
narrative_ontology:constraint_victim(amendment_history__reunification_amendments_1990, all_german_constitutional_moment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EAST GERMAN CITIZENS (SNARE) — Absorption via Article 23 meant no co-authorship of the unified constitution. The promised Article 146 moment (constituent assembly) never materialized. East Germans faced a binary: accept the existing Basic Law or remain separate. No genuine exit option; no seat at the drafting table; no veto over the legal order they were bound into. Maximum experienced extraction — institutional continuity extracted from them, not negotiated with them.
constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EAST GERMAN STATE APPARATUS (TANGLED ROPE) — The GDR's dissolution generated coordination needs: legal continuity, property transfers, institutional integration. Article 23 enabled this coordination while simultaneously suppressing the alternative (constituent assembly). The acceded state had some agency in negotiation (2+4 talks, economic compensation) but faced irreversible time pressure and structural asymmetry. Mixed extraction and coordination — constrained rather than trapped because some terms were negotiable, but the binary nature of the choice (accede or remain separate) limited genuine exit.
constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WEST GERMAN FEDERAL GOVERNMENT (ROPE) — Article 23 solved the coordination problem facing unified Germany: it provided an existing legal framework, avoiding the 2-5 year constituent assembly process. The beneficiary sees the constraint as pure coordination — continuity of law, institutional stability, operational governance during transition. The Federal Republic experienced this as coordination enabling rapid integration, not as extraction. Arbitrage exit (could have negotiated alternative articles; chose institutional continuity) produces low or negative f(d).
constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALL-GERMAN CONSTITUTIONAL VISION (TANGLED ROPE) — The Basic Law itself envisioned Article 146: a moment when a unified Germany would author its own constitution jointly. This vision coordinated the hope that reunification would be mutually generative, not absorptive. By choosing Article 23, the constraint suppressed this coordination function while extracting its legitimacy (the promise of co-authorship became the cover story for absorption). Constrained by geopolitical time pressure and structural lock-in; mixed real coordination losses (the 146 moment never happened) and extraction (the vision was used to justify the choice without delivering it).
constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CONSTITUTIONAL KERNEL (PITON) — Article 146 remains in the Basic Law as if it were still operative, but its function has atrophied. It is invoked in ceremonial contexts (founding moments, legitimacy claims) while its actual purpose — triggering a constituent assembly — has been foreclosed. The kernel persists through institutional inertia: embedded in the text, cited in preambles, but no longer performing its original work. Theater ratio is high because the article is maintained as a symbol of future possibility while actual practice has moved past it.
constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOPOLITICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, rapid reunification in 1990 was structurally inevitable: the Soviet Union was collapsing, German unification had narrow historical window, and a 2-5 year constituent assembly was infeasible. Article 23 accession appears as a natural response to immutable geopolitical conditions — not a choice but a necessity. However, the structural data contradicts this classification. The temporal pressure was real but not immutable; other constitutional pathways existed and were consciously rejected; the extraction logic is clear. This mountain is a false summit, revealing that 'geopolitical necessity' naturalizes what was an institutional choice.
constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amendment_history__reunification_amendments_1990_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amendment_history__reunification_amendments_1990, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amendment_history__reunification_amendments_1990, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(amendment_history__reunification_amendments_1990, TR),
    TR >= 0.70.

:- end_tests(amendment_history__reunification_amendments_1990_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts asymmetric transformation costs from East German agents (absorption without co-authorship, institutional discontinuity in practice despite formal continuity of law, 1990s economic shocks disproportionately borne in East). The West benefits from institutional continuity and rapid integration timeline. The extraction is not maximal (snare-level 0.66+) because genuine coordination occurred: property transfers, institutional integration, legal harmonization required negotiation and had real coordination function. But extraction is significant because the alternative path (constituent assembly) was actively suppressed and its suppression was legitimized through Article 146 remaining textually in the Basic Law (the promise made suppression acceptable). The measurements show extraction at moderate-high (0.42 pre-collapse) rising to full tangled-rope level (0.58) at the 1990 accession moment, remaining elevated as institutional lock-in deepened. Suppression (0.65): High. The constituent-assembly path was suppressed through: temporal pressure (narrow window for action); structural asymmetry (West had existing order, East had dissolution); economic leverage (integration conditioned on rapid acceptance); institutional design (Article 23 was technically available, Article 146 was technically available but procedurally difficult). The suppression increased at t1 when the binary became final and remained high (0.60) as institutional practice locked in the accession path. Theater (0.48): Moderate. Article 146 remains in the Basic Law as a ceremonial symbol of future possibility but is not genuinely operative. Its invocation in constitutional preambles and speeches carries legitimizing weight (we remain open to co-authorship) while the institution practices closure (the 2004 constitutional reform reformatted provisions without triggering 146; political discourse treats 146 as historically closed). Theater increased to 0.62 by 2005 as Article 146 became purely symbolic.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximum perspectival divergence because the same structural event (accession via Article 23) appears as coordination to the beneficiary (West German Federal Government sees efficient legal integration), as snare to the powerless agent (East German citizens offered binary with no real exit), as tangled rope to moderate-power agents (East German state experienced mixed coordination needs and suppressed alternatives), as piton to the deferred constitutional vision (Article 146 became ceremonial), and as false summit to the analytical observer (geopolitical necessity masks institutional choice with distributional consequences). This perspectival range exemplifies how indexical classification reveals structural asymmetries: the same institutional choice is beneficial from one position and extractive from another because the extraction itself is structural (the cost of absorption without co-authorship) not phenomenological.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. West German Federal Government: beneficiary + arbitrage exit → low d → negative f(d) → low/negative χ (experienced as coordination, not extraction). East German citizens: victim + trapped exit → high d → high f(d) → high χ (experienced as maximum extraction). East German state apparatus: victim + constrained exit → moderate-high d → elevated f(d) → elevated χ (experienced as mixed coordination-extraction with some negotiation possible but no genuine veto). The perspectival gap reflects these directionality differences. The piton perspective reflects Article 146 becoming purely performative — it persists in the text but no longer functions as an active constitutional mechanism. The mountain perspective reflects the natural-law reading that geopolitical necessity made accession inevitable, which the structural data contradicts: the temporal pressure was real but not immutable; the suppression was institutional choice, not natural fact.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between coordination (reunification required legal continuity) and extraction (the specific path chosen asymmetrically distributed transformation costs). The tangled rope classification captures both: Article 23 accession genuinely coordinated the legal unification that had to happen; it simultaneously extracted from East German agents the asymmetry of absorption. The alternative reading — that 'real' legitimacy would have required Article 146 constituent assembly — is not empirically falsifiable (we cannot rerun history) but is logically coherent as a claim about constitutional legitimacy. The tangled rope type acknowledges both functions coexisting: this is not a dispute about whether coordination occurred (it did) but about whether the suppression of the alternative path (constituent assembly) was justified by necessity or was an institutional choice with distributional consequences. The false summit mountain from the analytical perspective reveals why mandatrophy matters: once the natural-law framing ('geopolitical necessity made it inevitable') is questioned, the institutional choice becomes visible, and the extraction becomes analyzable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_necessity_vs_institutional_choice,
    'Was the 1990 timeframe a geopolitical immutable or a political choice that foreclosed alternatives?',
    'Historical counterfactual analysis: Soviet timeline, European consolidation pressure, Allied negotiating position. Comparison with other post-conflict reunifications that used constituent assemblies (e.g., Vietnam 1975, Korea discussions).',
    'If immutable: Article 23 was the only rational choice; constraint appears natural and necessary. If political choice: Article 23 was one option among constrained set; reveals power asymmetry in which options were considered viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_necessity_vs_institutional_choice, empirical, 'Whether 1990 temporal pressure was geopolitically immutable or politically constructed').

omega_variable(
    article_146_promise_as_legitimacy_cover,
    'Did Article 146 remain in the Basic Law as a genuine future option or as legitimizing theater for Article 23 accession?',
    'Textual analysis of constitutional debates 1989-1991; statements by architects (Kohl, Genscher, Modrow); post-reunification treatment of Article 146 (invocation frequency, legislative efforts to trigger it, abandonment in 2004 constitutional reform).',
    'If genuine option: the kernel remains open, constraint is temporary scaffolding. If theater: Article 146 legitimized absorption while being functionally closed; suppression was structural, not temporal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_146_promise_as_legitimacy_cover, conceptual, 'Whether Article 146 was a deferred constituent path or a legitimacy cover for absorption').

omega_variable(
    east_german_agency_in_accession_negotiations,
    'Did East German representatives have genuine choice in accepting or rejecting Article 23, or was the binary (accede or dissolve) itself extractive coercion?',
    'Analysis of 2+4 negotiations, Modrow government position papers, economic conditionality, timing pressures. Counterfactual: what would have been the structural consequence of rejection at each negotiation point?',
    'If genuine choice: constraint is mixed coordination-extraction (moderate extraction). If binary was coercive: constraint is closer to snare (maximum extraction with no real alternative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(east_german_agency_in_accession_negotiations, empirical, 'Whether East German accession was negotiated choice or coercive binary').

omega_variable(
    reading_kernel_contest_ground,
    'Which foundational commitment distinguishes this reading (accession-based reunification) from alternative constitutional readings of German reunification (constituent assembly, federal compromise, mutual reconstitution)?',
    'Analysis of what each reading holds as non-negotiable: this reading commits to institutional continuity of West German legal order as the basis for unity; alternative readings commit to co-authorship, mutual constitutive power, or symmetrical reconstitution. The readings foreclose each other at the axiom level.',
    'Determines the cs_structure.reading_relations and axioms. This reading forecloses the ''co-authorship'' axiom central to alternative readings; alternative readings foreclose the ''institutional continuity precedence'' axiom central to this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_ground, conceptual, 'Ground of constitutional contest between accession and co-authorship readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amendment_history__reunification_amendments_1990, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reun_theater_t0_pre_accession, amendment_history__reunification_amendments_1990, theater_ratio, 0, 0.35).
narrative_ontology:measurement(reun_theater_t1_146_deferred, amendment_history__reunification_amendments_1990, theater_ratio, 1, 0.48).
narrative_ontology:measurement(reun_theater_t15_2005_constitutional_reform, amendment_history__reunification_amendments_1990, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(reun_extract_t0, amendment_history__reunification_amendments_1990, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(reun_extract_t1_1990_accession, amendment_history__reunification_amendments_1990, base_extractiveness, 1, 0.58).
narrative_ontology:measurement(reun_extract_t5_post_integration, amendment_history__reunification_amendments_1990, base_extractiveness, 5, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(reun_suppress_t0_pre_collapse, amendment_history__reunification_amendments_1990, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(reun_suppress_t1_1990_binary, amendment_history__reunification_amendments_1990, suppression_requirement, 1, 0.65).
narrative_ontology:measurement(reun_suppress_t5_institutional_lock, amendment_history__reunification_amendments_1990, suppression_requirement, 5, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amendment_history__reunification_amendments_1990, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(amendment_history__reunification_amendments_1990, 0.12).
narrative_ontology:affects_constraint(amendment_history__reunification_amendments_1990, amendment_history__asylum_compromise_1993).
narrative_ontology:affects_constraint(amendment_history__reunification_amendments_1990, amendment_history__debt_brake_2009).
narrative_ontology:affects_constraint(amendment_history__reunification_amendments_1990, amendment_history__emergency_acts_1968).
narrative_ontology:affects_constraint(amendment_history__reunification_amendments_1990, amendment_history__rearmament_1956).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the amendment_history kernel. The kernel is the Basic Law as a stabilized commitment that grounds its legitimacy in constitutional succession and interpretation chains. Each sibling reading (asylum_compromise_1993, debt_brake_2009, emergency_acts_1968, rearmament_1956) instantiates a different constitutional moment and different axioms. This reading (reunification_amendments_1990) focuses on the choice between accession (institutional continuity) and constituent assembly (co-authorship). The network links show that constitutional readings affect each other: decisions made during reunification constrained subsequent amendments (debt brake, asylum changes); earlier amendments (emergency acts, rearmament) constrained how reunification could be framed. Each story has its own ε and perspectives; the family decomposition reflects that these are structurally distinct constitutional contests, not variants of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amendment_history__reunification_amendments_1990, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
