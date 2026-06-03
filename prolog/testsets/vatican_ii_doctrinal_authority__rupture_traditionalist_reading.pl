% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority (Rupture-Traditionalist Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint instantiates the traditionalist reading of the Vatican II
 *   doctrinal authority kernel — the claim that the Second Vatican Council
 *   represents a fundamental rupture with preceding Catholic doctrine and
 *   practice, rather than organic development. The reading asserts that the
 *   Council documents contain structural ambiguities and internal
 *   contradictions that enabled their heterodox implementation by reformist
 *   factions, producing real losses in doctrinal clarity, liturgical
 *   tradition, and institutional missionary capacity. The rupture
 *   traditionalist reading coexists with three other readings of the same
 *   kernel: the continuity reading (Council as development of implicit prior
 *   teaching), the rupture progressive reading (Council as necessary break
 *   with rigidity, authorizing ongoing reform), and the composite
 *   overdetermination reading (Council as convergence of distinct structural
 *   changes). This constraint story models the traditionalist reading as a
 *   single ε-invariant structure with specific beneficiaries and victims,
 *   indexed across multiple observational positions. The constraint exhibits
 *   Tangled Rope classification at the strongest analytical position: genuine
 *   coordination function exists (Council does represent theological
 *   engagement with modernity and ecumenical dialogue), but extraction
 *   asymmetry is severe (traditional practice suppressed, doctrinal coherence
 *   compromised, missionary structures disrupted). The theater ratio rises
 *   from 0.38 to 0.65 over the 20-year interval, reflecting increasing
 *   performative maintenance of the 'organic development' narrative as
 *   substantive rupture becomes undeniable.
 *
 * KEY AGENTS:
 *   - Reformist Episcopal Factions: Primary beneficiary (institutional/arbitrage) — gain authority for pastoral innovation and escape from pre-conciliar doctrinal constraints; arbitrage exit enables implementation without cost
 *   - Traditional Liturgical Communities: Primary victim (powerless/trapped) — forbidden to practice Tridentine Mass, denied institutional support, suppressed by ecclesiastical authority with no exit option
 *   - Doctrinal Clarity and Coherence: Abstract victim (powerless/trapped) — pre-conciliar theological formulations abandoned without explicit replacement, generating decades of hermeneutical confusion and competing magisterial interpretations
 *   - Pre-Conciliar Missionary Structures: Institutional victim (institutional/constrained) — seminaries disbanded or reoriented, missionary orders restructured, catechetical methods abandoned, replaced with experimental approaches producing documented institutional decline
 *   - Conservative Diocesan Clergy: Secondary actor (moderate/constrained) — face extraction (enforced liturgical compliance) and benefit from autonomy gains; mixed position produces Tangled Rope experience
 *   - Traditionalist Organizational Resistance: Organized resistance (organized/mobile) — SSPX and traditionalist networks perceive Council as erroneous, resist through schism, maintain alternative institutional structures; mobile exit prevents maximal extraction
 *   - The Post-Conciliar Magisterium: Institutional actor (institutional/constrained) — performs continuity narrative while enforcing rupture implementation; theater ratio high because magisterial authority claims to preserve tradition while substantively replacing it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority (Rupture-Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '7b815be1-3b5a-4e57-9f2b-7fc158bff2cf').
narrative_ontology:cs_kernel_codification('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', formalized).
narrative_ontology:cs_authority_grounding('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', lineage).
narrative_ontology:cs_interpretation_layer_present('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf').
narrative_ontology:cs_reading_relation('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', vatican_ii_doctrinal_authority__rupture_progressive_reading, influences).
narrative_ontology:cs_reading_relation('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', foundational, authentic_tradition_is_juridically_fixed).
narrative_ontology:cs_axiom_status(authentic_tradition_is_juridically_fixed, holdable).
narrative_ontology:cs_axiom_grounding('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', authentic_tradition_is_juridically_fixed, deontological).
narrative_ontology:cs_axiom('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', foundational, conciliar_ambiguity_signals_formulaic_error).
narrative_ontology:cs_axiom_status(conciliar_ambiguity_signals_formulaic_error, holdable).
narrative_ontology:cs_axiom_grounding('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', conciliar_ambiguity_signals_formulaic_error, empirically_contingent).
narrative_ontology:cs_reference_frame('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', pre_conciliar_doctrinal_firmness).
narrative_ontology:cs_drift_state('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', contemporary_post_conciliar_magisterium, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7b815be1-3b5a-4e57-9f2b-7fc158bff2cf', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, reformist_episcopal_factions).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theological_schools).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgical_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity_and_coherence).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_missionary_structures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL LITURGICAL COMMUNITIES (SNARE) — Cannot exit the post-conciliar imposed liturgical reform. Trapped by ecclesiastical authority that has declared the Tridentine Mass and traditional practices incompatible with Vatican II's mandate. Suppression is comprehensive: banned from parishes, denied institutional support, treated as schismatic if asserting pre-conciliar forms. Maximum extraction without genuine benefit — the constraint's entire function is to extract conformity to reformed practice.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSERVATIVE DIOCESAN CLERGY (TANGLED ROPE) — Constrained by episcopal authority and career risk (reassignment, canonical penalty for non-compliance), but also benefit from the post-conciliar shift in some ways: reduced celibacy enforcement in practice, expanded preaching authority, pastoral flexibility. Face extraction (enforced liturgical innovation against their theological conviction) but also gain some coordination function (pastoral autonomy). Neither maximal extraction nor pure benefit.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMIST EPISCOPAL FACTIONS (ROPE) — Primary beneficiary (institutional/arbitrage). Experience Vatican II as coordination: the Council legitimizes their long-desired pastoral liberalization and opens escape from pre-conciliar doctrinal rigidity. Arbitrage exit option because they can implement reform or pursue ecclesiastical advancement without cost. Net beneficiary — the rupture reading assigns them the extraction flow. This perspective sees the Council documents as enablers of legitimate reform despite ambiguities.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN THEOLOGICAL ESTABLISHMENT (TANGLED ROPE) — Powerful actors (mobile exit) with significant benefits from Vatican II's opening to modern biblical scholarship, existentialist philosophy, and post-Enlightenment thought. Constrained by tension between doctrinal innovation and need to maintain ecclesiastical legitimacy. Experience mixed coordination (genuine dialogue with modern thought) and extraction (enforced reinterpretation of traditional formulas to fit contemporary frameworks). Significant agency but also disciplinary pressure.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE POST-CONCILIAR MAGISTERIUM (PITON) — Performs doctrinal continuity while enforcing substantive rupture. Theater ratio high (0.65): the magisterium insists Vatican II is 'development not rupture' while suppressing pre-conciliar theology and banning the Tridentine Mass. The performative claim ('organic development') persists via institutional authority, but the actual function (enforcing compliance with reformed practice) has atrophied beneath the rhetoric. Theater gate triggers here — the magisterium maintains a performative reading of its own authority rather than a functional one.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONALIST ORGANIZATIONAL RESISTANCE (SCAFFOLD) — Society of Saint Pius X (SSPX) and traditionalist networks (organized/mobile) perceive Vatican II's rupture as requiring organized resistance with an implicit sunset: either Rome returns to pre-conciliar doctrine (Council authoritatively reversed), or traditionalism remains in institutional schism. Low effective extraction from this perspective because organized resistance can exit — schism is available, even if costly. The scaffold reading projects eventual resolution: doctrinal clarification will either vindicate traditionalism (Council was erroneous) or formalize the break (explicit authoritarianism, ending ambiguity). χ ≤ 0.30 from this structural position.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / DOCTRINAL INCOMMENSURABILITY (MOUNTAIN) — From civilizational horizon, some ruptures in doctrinal frameworks may be inevitable structural features of theological history: once a doctrinal formulation is challenged by the broader culture, the formulation cannot be simply restored unchanged — it exists now within a different epistemic and social context. This perspective risks naturalizing the Vatican II rupture as an unchangeable consequence of modernity rather than a contingent choice by the magisterium. Engine false-summit detection should flag this naturalization.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The rupture reading frames the post-conciliar shift as imposing doctrinal and liturgical changes on unwilling constituencies (traditional communities, conservative clergy, pre-conciliar institutional structures). The beneficiaries (reformist bishops, progressive theologians) capture the gains: authority for innovation, end to pre-conciliar constraints, alignment with modern thought. The extraction is neither maximal (organized resistance exists, some communities maintain continuity outside magisterial control) nor minimal (the suppression of Tridentine Mass, closure of traditional seminaries, and institutional restructuring impose real costs on victims). The 0.42→0.58 trajectory reflects increasing institutionalization of post-conciliar reforms — initial implementation was contested (lower ε), but by year 20, the rupture becomes structural policy (higher ε). Suppression (0.62): Moderate-high. Comprehensive suppression mechanisms exist: papal suppression of SSPX communities attempting traditional practices, canonical restrictions on Tridentine Mass celebration, institutional pressure on conservative clergy, theological delegitimization of pre-conciliar positions. But suppression is not total — traditionalist resistance persists, some bishops permit continuity practices, underground seminaries maintain formation in pre-conciliar theology. Theater ratio (0.65): Moderate-high. The magisterium's insistence that Vatican II is 'development not rupture' is performative — contradicted by substantive changes (eucharistic theology, religious freedom, episcopal collegiality, liturgical structure). The theater increases over time (0.38→0.65) because the magisterium must invest increasing rhetorical effort to maintain the development narrative as the rupture becomes undeniable. By year 20, most ecclesiology acknowledges significant change, requiring magisterial theater to preserve the legitimacy claim.
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gaps cluster around the extraction/coordination distinction. Reformist bishops (institutional/arbitrage/immediate) see Rope: the Council coordinates engagement with modernity. Traditional communities (powerless/trapped/biographical) see Snare: mandatory rupture with no exit. Conservative clergy (moderate/constrained/generational) see Tangled Rope: gain and loss mixed. The post-conciliar magisterium (institutional/constrained/civilizational) sees Piton: it performs continuity while enforcing rupture, theater ratio rising as the performance strains. Traditionalist resistance (organized/mobile/generational) sees Scaffold: either Rome returns to pre-conciliar doctrine (Council reversed) or schism is formalized (ambiguity ends). The analytical observer at civilizational horizon risks seeing Mountain: doctrinal ruptures are inherent to theological history, Council's changes are inevitable consequences of modernity. This false-summit risk reveals the traditionalist reading's structural assumption — that the rupture was contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from its structural relationship to the extraction mechanism. Beneficiaries (reformist bishops) have high power and arbitrage exit — low d (0.15), negative f(d) ≈ -0.01, minimal experienced extraction. Victims (traditional communities) are powerless and trapped — high d (0.95), high f(d) ≈ 1.42, maximal experienced extraction. The magisterium occupies a hybrid position: acts as enforcer of rupture (extractor from traditionalists) but constrained by its own legitimacy claim (cannot explicitly deny continuity without losing ecclesiastical authority). This produces moderate d (0.55-0.60), moderate experienced extraction chi. Conservative clergy are trapped by authority hierarchy but benefit from some pastoral autonomy — moderate d (0.65), moderate chi. The rupture reading's directionality differs fundamentally from the continuity reading's: in the continuity reading, the 'rupture' is reframed as development, beneficiaries are reattributed as servants of tradition, and d values shift downward. The reading contest is partly a contest over directionality assignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the rupture_traditionalist_reading and the continuity_reading are structurally incommensurable readings of the same kernel. Both claim to interpret Vatican II faithfully; both claim magisterial authority; both claim fidelity to Catholic tradition. They differ on what 'development' means and where rupture lines exist. The rupture reading assigns higher ε to doctrinal change and frames it negatively (loss of clarity, suppression of tradition). The continuity reading assigns the same observable changes lower ε (reinterprets them as development) and frames them positively (vitality, engagement). The mathematical ε values may be identical (e.g., 0.58 for the magnitude of change), but their normative valence differs. The rupture_traditionalist reading resolves this by declaring beneficiaries and victims: reformist factions benefit (lower d, negative extraction from their perspective), traditional communities suffer (higher d, positive extraction from their perspective). This is not a mathematical inconsistency — it is a perspectival difference rooted in which agents' interests the reading privileges. The analytical observer must recognize that no single perspective is canonical; the reading contest itself is the structural truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_intentional_vs_compromised,
    'Are the Vatican II documents'' ambiguities and internal tensions the result of deliberate rhetorical compromise (enabling multiple interpretations) or genuine theological confusion (errors of formulation)?',
    'Conciliar history and personal testimony from periti (theological advisors) and Council fathers; analysis of draft revisions and textual deletion records; comparison of conciliar intent documents with final text',
    'If intentional compromise: ambiguities are features enabling coalition-building, not bugs. Classification may shift toward Rope (legitimate coordination across factions). If errors of formulation: ambiguities are structural weaknesses, confirming the rupture reading. Theater ratio would be justified as masking conflicting premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentional_vs_compromised, empirical, 'Whether Vatican II ambiguities resulted from deliberate compromise or formulaic error').

omega_variable(
    rupture_vs_development_criterion,
    'What objective criterion distinguishes doctrinal ''development'' from doctrinal ''rupture'' in ecclesiastical tradition?',
    'Systematic comparison of continuity reading''s explicit criteria (Newman''s ''development'', Rahner''s ''hermeneutical tradition'') against rupture reading''s observable shifts; analysis of pre-conciliar theological positions on ecumenism, religious freedom, episcopal collegiality to determine whether Vatican II represents explication of prior implicit teaching or reversal of explicit prior positions',
    'If ''development'' criterion applies: continuity reading is structural, rupture reading is interpretive choice. If objective criterion favors rupture: the taxonomy of doctrinal change itself is contested and underdetermined. Omega unresolvable within ecclesiology alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rupture_vs_development_criterion, conceptual, 'Criterion for distinguishing doctrinal development from rupture').

omega_variable(
    spirit_vs_letter_authority_locus,
    'Where does magisterial authority reside post-Vatican II: in the conciliar documents'' literal text, in their discoverable conciliar intent, or in the ''spirit of the Council'' as interpreted by subsequent magisterium?',
    'Papal and magisterial statements on Council interpretation; cases where magisterium has invoked ''spirit'' against ''letter'' or vice versa; resolution mechanisms (synods, papal encyclicals) for conciliar interpretation disputes',
    'If authority in documents: rupture reading is falsifiable by textual analysis. If authority in ''spirit'': ''spirit'' becomes a floating signifier enabling both rupture and continuity readings indefinitely. If authority in magisterium''s interpretation: authority is self-justifying (magisterium defines what Vatican II means), making the rupture/continuity contest unresolvable within magisterial frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spirit_vs_letter_authority_locus, conceptual, 'Locus of magisterial authority post-Vatican II').

omega_variable(
    missionary_effectiveness_causality,
    'Did post-conciliar liturgical and doctrinal changes cause the documented decline in religious vocations, Mass attendance, and catechetical transmission in the West, or is the causality spurious (independent secularization trend)?',
    'Comparison of vocational and attendance trends in: (a) Catholic populations directly reformed post-1965; (b) communities maintaining pre-conciliar practice (SSPX, Benedictine traditionalist houses); (c) non-Catholic Western religions over same period; (d) non-Western Catholic communities; statistical controls for urbanization, educational attainment, cultural factors',
    'If post-conciliar changes causally responsible: victim group (missionary zeal, doctrinal clarity) has measurable empirical burden. Extractiveness ε justified. If spurious causality: victim group assignment may be misattributed. Extractiveness may reflect coordination failure rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(missionary_effectiveness_causality, empirical, 'Causal link between post-conciliar reforms and decline in Western Catholic institutions').

omega_variable(
    vatican_ii_kernel_contest_itself,
    'Is Vatican II a single coherent doctrinal kernel, or a bundle of logically distinct decisions (liturgical, ecumenical, ecclesiological, political) that happen to be labeled as one Council?',
    'Logical dependency analysis: which Vatican II doctrines logically require which others? Which could have been decided independently? Can the Council be decomposed into separate constraint stories with different ε values?',
    'If single kernel: the rupture/continuity/development debate is about one coherent shift. If bundle: different readings apply to different components (liturgy may be rupture, ecclesiology may be development). This omega routes to potential constraint family decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vatican_ii_kernel_contest_itself, conceptual, 'Whether Vatican II is a single kernel or a bundle of structurally distinct decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vii_trad_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(vii_trad_tr_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(vii_trad_tr_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(vii_trad_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vii_trad_be_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(vii_trad_be_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vii_trad_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vii_trad_su_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(vii_trad_su_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_liturgical_imposition).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, episcopal_collegiality_authority_distribution).

% DUAL FORMULATION NOTE:
% The Vatican II doctrinal authority kernel is interpreted through four distinct constraint readings, each with different ε values and structural relationships. This reading (rupture_traditionalist) assigns high ε to doctrinal change, frames it as extraction, and identifies traditional communities as primary victims. Sibling readings assign different ε values, different beneficiary/victim structures, and different normalizations of the same observable changes. All four readings are linked in network.affects_constraints to show the kernel contest structure. Downstream constraints (post_conciliar_liturgical_imposition, episcopal_collegiality_authority_distribution) are empirically dependent on how Vatican II is read.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
