% ============================================================================
% CONSTRAINT STORY: ecumenical_boundary_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecumenical_boundary_shift, []).

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
 *   constraint_id: ecumenical_boundary_shift
 *   human_readable: Vatican II Ecumenical Boundary Shift as Unified Doctrinal Reinterpretation
 *   domain: ecclesiastical_history/theological_doctrine/institutional_change
 *
 * SUMMARY:
 *   Vatican II (1962-1965) presents a paradigm case of contested
 *   institutional reinterpretation: the Council simultaneously claims
 *   doctrinal continuity with pre-conciliar Catholicism and implements
 *   substantive shifts in religious liberty (Dignitatis Humanae), ecumenism
 *   (Unitatis Redintegratio), episcopal collegiality (Lumen Gentium), and
 *   liturgical praxis (Sacrosanctum Concilium). The structural question is
 *   whether these constitute ONE reading of a unified kernel (Catholic
 *   doctrine) or whether Vatican II bundles multiple independent doctrinal
 *   movements that benefit from institutional packaging as a unified
 *   'aggiornamento.' This constraint analysis treats Vatican II as one
 *   unified reading of the doctrinal kernel while documenting in omega
 *   variables the risk that decomposition into separate stories (per the
 *   ε-invariance principle) would reveal heterogeneous extraction mechanisms.
 *   The measured theater_ratio rise (0.35 → 0.65) tracks the increasing
 *   performative content of 'continuity' claims as substantive divergence
 *   between conciliar directives and preconciliar practice became undeniable
 *   in subsequent decades. The suppression_requirement rise (0.25 → 0.48)
 *   reflects growing curial intervention to harmonize regional
 *   interpretations and constrain progressive readings, indicating that the
 *   constraint's suppressive machinery activates gradually as the
 *   coordination fiction of unified implementation breaks down.
 *
 * KEY AGENTS:
 *   - Roman Curia and Papal Magisterium: Primary beneficiary (institutional/arbitrage) — consolidates authority to interpret Vatican II's meaning; exercises control through postconciliar implementation apparatus (Ecclesia Dei, ad hoc doctrinal corrections)
 *   - Pre-Conciliar Doctrine and Coherence: Primary victim (powerless/trapped) — substantive positions shift while institutional narrative preserves continuity; no exit except schism
 *   - Regional Episcopal Conferences: Secondary victim (moderate/constrained) — gain deliberative voice in Council, lose postconciliar doctrinal autonomy; constrained by magisterial harmonization
 *   - Progressive and Conservative Factions: Organized agents (organized/constrained) — both claim Vatican II vindication; compete for interpretive authority; constrained by need to maintain conciliar loyalty
 *   - Pre-Conciliar Tridentine Framework: Inertial institutional structure (institutional/arbitrage) — formal structures persist (diocesan organization, seminary training models) through momentum despite doctrinal supersession
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the Council's institutional legitimacy claims as inevitable logical necessity rather than contingent exercise of authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecumenical_boundary_shift, 0.52).
domain_priors:suppression_score(ecumenical_boundary_shift, 0.48).
domain_priors:theater_ratio(ecumenical_boundary_shift, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecumenical_boundary_shift, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecumenical_boundary_shift, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ecumenical_boundary_shift, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecumenical_boundary_shift, tangled_rope).
narrative_ontology:human_readable(ecumenical_boundary_shift, "Vatican II Ecumenical Boundary Shift as Unified Doctrinal Reinterpretation").
narrative_ontology:topic_domain(ecumenical_boundary_shift, "ecclesiastical_history/theological_doctrine/institutional_change").

domain_priors:requires_active_enforcement(ecumenical_boundary_shift).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecumenical_boundary_shift, '8da00a31-59f2-4e5a-a0c8-7a8197320692').
narrative_ontology:cs_kernel_codification('8da00a31-59f2-4e5a-a0c8-7a8197320692', fixed_text).
narrative_ontology:cs_authority_grounding('8da00a31-59f2-4e5a-a0c8-7a8197320692', lineage).
narrative_ontology:cs_interpretation_layer_present('8da00a31-59f2-4e5a-a0c8-7a8197320692').
narrative_ontology:cs_reading_relation('8da00a31-59f2-4e5a-a0c8-7a8197320692', vatican_ii_rupture_reading, coexists_with).
narrative_ontology:cs_axiom('8da00a31-59f2-4e5a-a0c8-7a8197320692', foundational, continuity_of_magisterial_authority).
narrative_ontology:cs_axiom_status(continuity_of_magisterial_authority, holdable).
narrative_ontology:cs_axiom_grounding('8da00a31-59f2-4e5a-a0c8-7a8197320692', continuity_of_magisterial_authority, deontological).
narrative_ontology:cs_axiom('8da00a31-59f2-4e5a-a0c8-7a8197320692', foundational, development_preserves_prior_truths).
narrative_ontology:cs_axiom_status(development_preserves_prior_truths, holdable).
narrative_ontology:cs_axiom_grounding('8da00a31-59f2-4e5a-a0c8-7a8197320692', development_preserves_prior_truths, deontological).
narrative_ontology:cs_reference_frame('8da00a31-59f2-4e5a-a0c8-7a8197320692', neo_scholastic_doctrinal_stability).
narrative_ontology:cs_drift_state('8da00a31-59f2-4e5a-a0c8-7a8197320692', post_conciliar_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8da00a31-59f2-4e5a-a0c8-7a8197320692', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecumenical_boundary_shift, roman_curia_institutional_authority).
narrative_ontology:constraint_beneficiary(ecumenical_boundary_shift, papal_magisterium_continuity).
narrative_ontology:constraint_victim(ecumenical_boundary_shift, pre_conciliar_doctrine_coherence).
narrative_ontology:constraint_victim(ecumenical_boundary_shift, episcopal_regional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRE-CONCILIAR DOCTRINE (SNARE) — The prior doctrinal framework cannot exit the reinterpretation that overwrites it. Doctrinal coherence is declared preserved while substantive positions shift (religious liberty, ecumenism, episcopal collegiality). The prior framework is absorbed into continuity narratives without meaningful survival. No alternative except repudiation (which carries schism risk). Maximum experienced extraction from the doctrinal commons.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL EPISCOPAL CONFERENCES (TANGLED ROPE) — Bishops gain collegiality (coordination benefit) but lose doctrinal autonomy (extraction cost). Pre-conciliar regional variation is harmonized into magisterial teaching. Constrained exit: bishops accept conciliar authority or face institutional pressure. Mixed experience: genuine agency in conciliar deliberation paired with binding postconciliar implementation.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: ROMAN CURIA / PAPAL MAGISTERIUM (ROPE) — Primary beneficiary. The Council's reinterpretation establishes magisterial continuity ('development of doctrine') while consolidating curial control through implementation apparatus. Arbitrage exit (curial authorities can selectively interpret conciliar directives). Rope classification: the constraint solves a genuine coordination problem (legitimating doctrinal change while maintaining institutional authority) while providing substantial benefit to the beneficiary.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE / CONSERVATIVE FACTIONS (TANGLED ROPE) — Both factions claim Vatican II as vindication. Progressives see liberalization (ecumenism, religious liberty, vernacular liturgy); conservatives see continuity defense against modernism. Both coordinate through Council authority while extracting divergent readings. Generational timeline: factions compete for interpretive authority across decades. Constrained exit: dissent from conciliar authority carries institutional cost for both sides.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PRE-CONCILIAR TRIDENTINE FRAMEWORK (PITON) — Tridentine institutional structures (rigid catechesis, Latin liturgy, anti-Protestant polemic) persist as theatrical performance long after Vatican II's doctrinal shift. Inertial maintenance through communities (FSSP, sedevacantists) sustained by institutional memory rather than functional necessity. Theater ratio high: ritualized preservation without substantive epistemic or organizational role.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / DOCTRINAL CONTINUITY AS NATURAL LAW (MOUNTAIN) — From a civilizational view, some constraints on doctrinal reinterpretation are immutable: an institution cannot simultaneously affirm and deny the same doctrine without contradiction; continuity claims require continuity narratives; authority structures cannot operate without legitimacy frames. This perspective sees Vatican II's boundary shift as inevitable institutional logic. However, the structural data (identified beneficiaries, enforcement machinery, suppression of alternative readings) reveals this as a false summit: the 'natural law' framing naturalizes what is actually a contested institutional arrangement.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecumenical_boundary_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecumenical_boundary_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecumenical_boundary_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecumenical_boundary_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecumenical_boundary_shift, TR),
    TR >= 0.70.

:- end_tests(ecumenical_boundary_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Vatican II generates extraction across multiple dimensions. The shift to religious liberty (Dignitatis Humanae) overturns the prior Syllabus of Errors framework without explicitly repudiating it — a doctrinal boundary shift that benefits institutional flexibility (papal magisterium can now claim religious freedom advocacy) while suppressing the prior framework's coherence. Ecumenical openness (Unitatis Redintegratio) similarly benefits Rome's diplomatic standing while constraining prior anti-Protestant polemic's institutional legitimacy. Extractiveness rises over the interval (0.28 → 0.52) because implementation reveals gaps between conciliar directives and substantive institutional change — the constraint must increasingly enforce the continuity fiction as reality diverges. Suppression (0.48): Moderate. Pre-conciliar doctrinal frameworks cannot exit without breaking from Rome; bishops lose regional autonomy through postconciliar centralization; conservative factions must suppress their doctrinal objections to maintain institutional standing. Suppression is not total because some regional variation persists (German bishops retain greater interpretive latitude than ultraconservative ones) and some dissent occurs (traditionalist movement, conservative theologian protests). Theater ratio (0.65): Moderate-high. The 'development of doctrine' narrative is substantially performative — it allows institutional transformation while maintaining continuity claims. The rise over time (0.35 → 0.65) reflects that the performance must intensify as substantive divergence becomes visible. By the 1980s-2000s, Vatican interventions to correct 'progressive' interpretations reveal that continuity is a frame rather than a structural feature.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal. The Curia sees Vatican II as solving a real coordination problem: how to maintain institutional authority while adapting to modern world. This is a genuine problem with a genuine solution — the Council's deliberative structure was functionally necessary. But the solution extracts substantial benefit (curial consolidation of interpretive authority) from those it governs (bishops, theologians, the faithful). The victim perspective (pre-conciliar doctrine) has no voice: it is overwritten by institutional authority exercising power to reinterpret. The analytical observer perspective risks treating the Curia's perspective as inevitable natural law — 'institutions must maintain legitimacy narratives, therefore continuity claims are necessary' — but this naturalizes what is actually a choice: Rome could have legitimized Vatican II through explicit rupture narratives rather than continuity frames. The choice to frame as continuity is a structural feature of the constraint (maintaining appearance of institutional stability), not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The Curia benefits from Vatican II's reframing: it gains authority to interpret 'development,' consolidates Rome's diplomatic standing, and legitimizes institutional flexibility. Directionality d ≈ 0.15 (beneficiary with arbitrage exit) — they can selectively implement conciliar directives and maintain interpretive authority. Regional bishops experience d ≈ 0.55 (mixed position: moderate power, constrained exit) — they gain collegiality (minor benefit, lowering d) but lose doctrinal autonomy (extraction, raising d). Pre-conciliar doctrine experiences d ≈ 0.95 (victim with trapped exit) — suppressed without alternative except rupture. The powerless victim (doctrinal coherence as abstract good) has d = 1.0 — maximum target status. Factions and traditionalists experience higher d (0.60-0.75) because organized dissent faces institutional pressure (constrained exit), though their organized status provides some leverage compared to powerless victims. The distribution of d values across perspectives confirms tangled_rope classification: genuine coordination (Council deliberation, collegiality) paired with asymmetric extraction (curial authority consolidation, doctrinal suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   Vatican II resolves mandatrophy by revealing that the Council is simultaneously a genuine coordination mechanism AND an asymmetric extraction. The coordination function is real: bishops needed greater collegial voice, the Church needed ecumenical legitimacy, the faithful needed liturgical accessibility. These are genuine problems. But the solution extracts substantial benefit to Rome's institutional authority (curial power to interpret 'development,' consolidated magisterial control) while suppressing alternative frameworks (pre-conciliar doctrine, regional episcopal autonomy). The constraint is therefore tangled_rope: genuine coordination function + asymmetric extraction + active enforcement (magisterial corrections, Vatican interventions in national conferences). The false summit risk (mountain perspective) arises from naturalizing the continuity narrative as inevitable — 'institutions must maintain legitimacy, therefore continuity frames are necessary.' This is true but incomplete: the choice to frame as continuity rather than explicit rupture is contingent and benefits identifiable agents (the Curia). The mandatrophy dissolves when we recognize that the coordination function is real AND the extraction is real — both are structural features of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_unity_vs_plurality,
    'Does Vatican II represent ONE reinterpretation of a single doctrinal kernel (allowing for multiple readings), or does it bundle MULTIPLE independent doctrinal shifts that happened to co-occur institutionally?',
    'Structural decomposition: identify whether each major Vatican II pronouncement (Dignitatis Humanae on religious liberty, Unitatis Redintegratio on ecumenism, Presbyterorum Ordinis on episcopal collegiality, Sacrosanctum Concilium on liturgy) shares a common hermeneutic principle or whether each operates from distinct theological premises. If distinct premises, decompose into separate constraint stories per the ε-invariance principle.',
    'If unified: Vatican II is ONE constrained kernel reading with multiple perspectives. If plural: Vatican II is a constraint FAMILY (4-5 linked stories) with different epsilon values per doctrinal shift. The pluralist decomposition reveals that some shifts have lower extractiveness (genuine coordination) while others have higher extractiveness (doctrinal suppression). The unified account naturalizes this heterogeneity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_unity_vs_plurality, conceptual, 'Whether Vatican II is one doctrinal reinterpretation or bundled independent shifts').

omega_variable(
    continuity_narrative_functionality,
    'Is the ''development of doctrine'' framing functionally necessary to Vatican II''s legitimacy, or could the Council''s actual content be justified through explicit rupture narratives?',
    'Counterfactual: construct a Vatican II justification that explicitly states ''the prior doctrine was wrong and we are replacing it.'' Assess whether this alternative narrative would have achieved the same institutional outcomes (episcopal support, Catholic laity acceptance, ecumenical standing). If yes, continuity is theater (rises theater_ratio toward piton). If no, continuity is functionally necessary (maintains tangled_rope classification).',
    'If theater: Vatican II''s classification shifts toward piton (high theater, sustained by institutional inertia). If necessary: Vatican II retains tangled_rope (genuine mixed coordination and suppression). Theater hypothesis suggests the Council''s binding mechanism is institutional authority maintaining appearance of stability, not substantive doctrinal coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_narrative_functionality, preference, 'Whether continuity framing is functionally necessary or performative').

omega_variable(
    regional_interpretation_variance,
    'How much doctrinal variance across episcopal conferences represents legitimate regional application versus uncontrolled fragmentation that the magisterium must suppress?',
    'Empirical mapping: track Vatican interventions in national episcopal conferences (Ecclesia in America, Ecclesia in Europa, ad hoc doctrinal corrections) correlating with regional doctrinal divergence. If suppression increases with variance, the constraint''s suppression value is understated. If variance persists without intervention, the constraint''s coordination function is understated.',
    'Rising suppression coefficient indicates stronger extraction mechanism (victim perspective''s snare classification is more accurate). Low intervention frequency indicates higher coordination function (institutional perspective''s rope classification is more accurate). The empirical record since Vatican II shows increasing curial intervention, suggesting actual suppression > measured value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_interpretation_variance, empirical, 'Degree of regional interpretation variance and magisterial response').

omega_variable(
    identity_locked_sedevacantism,
    'Do sedevacantist and traditionalist movements represent rational exit options from Vatican II''s framework, or are they identity-locked refusals rooted in constitutive commitment to pre-conciliar Catholicism?',
    'Distinction: sedevacantism claims material/structural grounds (the pope is invalid, conciliar authority is defective). Identity-lock would manifest as inability to assent to conciliar legitimacy even when structural barriers are removed. Survey traditionalist clergy: would they accept post-Vatican II magisterium if structural changes occurred (curial reform, papal authority clarification)? If ''no'' regardless of structural change, identity-lock diagnosis is confirmed.',
    'If rational exit: pre-conciliar framework is structurally viable alternative (tangled_rope classification confirmed; victims have exit option). If identity-locked: dissent is cognitive capture (mountain-like perception of immutability despite structural mobility). This affects whether episcopal perspective shows true constraint or merely cognitive adhesion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_sedevacantism, empirical, 'Whether traditionalist dissent is rational exit or identity-locked refusal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecumenical_boundary_shift, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ebs_tr_t0, ecumenical_boundary_shift, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ebs_tr_t5, ecumenical_boundary_shift, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ebs_tr_t15, ecumenical_boundary_shift, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ebs_be_t0, ecumenical_boundary_shift, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ebs_be_t5, ecumenical_boundary_shift, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ebs_be_t15, ecumenical_boundary_shift, base_extractiveness, 15, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ebs_su_t0, ecumenical_boundary_shift, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ebs_su_t5, ecumenical_boundary_shift, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(ebs_su_t15, ecumenical_boundary_shift, suppression_requirement, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecumenical_boundary_shift, identity_coordination).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, episcopal_collegiality_reform).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, religious_liberty_doctrine).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, liturgical_vernacularization).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, ecumenical_authority_shift).

% DUAL FORMULATION NOTE:
% Vatican II is presented here as a unified constraint (one doctrinal reinterpretation). However, the omega variables document the risk that ε-invariance decomposition would identify 4-5 distinct constraint stories: (1) collegiality shift (lower ε, more coordination), (2) religious liberty reversal (higher ε, more extraction), (3) liturgical change (moderate ε, genuine coordination), (4) ecumenical boundary (higher ε, institutional extraction). Each doctrinal shift has different beneficiary/victim structure and different suppression mechanisms. If downstream analysis supports decomposition, replace this unified story with a constraint family and link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecumenical_boundary_shift, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
