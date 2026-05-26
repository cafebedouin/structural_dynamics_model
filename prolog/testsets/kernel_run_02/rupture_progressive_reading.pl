% ============================================================================
% CONSTRAINT STORY: rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rupture_progressive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rupture_progressive_reading
 *   human_readable: Vatican II Rupture-Progressive Reading: Authorized Doctrinal Renewal Beyond Text
 *   domain: ecclesiology/institutional_authority/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-1965) and its immediate aftermath (1965-1975) represent
 *   a critical juncture in Catholic institutional authority. The
 *   progressive-rupture reading interprets the Council as a deliberate break
 *   with pre-conciliar rigidity and authoritarianism, treating Vatican II's
 *   textual ambiguities as intentional openings for ongoing doctrinal
 *   development. The 'spirit of the Council' formula grants reformers
 *   hermeneutical license to implement changes that go beyond explicit
 *   textual warrant. This reading is ONE of three structurally distinct
 *   interpretations of the same conciliar event. It forecloses and coexists
 *   with competing readings held by traditionalists and continuity advocates.
 *   The rupture-progressive reading has been explicitly rejected by John Paul
 *   II (who coined 'hermeneutics of rupture' as a pejorative) and by Benedict
 *   XVI (who attempted to reframe Vatican II as continuity). Yet the reading
 *   persists in theological education, pastoral practice, and institutional
 *   implementation, sustained partly by institutional inertia and partly by
 *   genuine defenders who see it as the most coherent reading of the Council.
 *   The extractiveness trajectory (0.32→0.52) reflects increasing recognition
 *   that the reading functions as a mechanism for concentrated power among
 *   progressive theological elites and reform-minded bishops, while the
 *   theater trajectory (0.18→0.38) shows that the reading's functional force
 *   has decayed as it has been formally contested and reframed, yet it
 *   continues to organize implementation through institutional weight rather
 *   than active doctrinal defense.
 *
 * KEY AGENTS:
 *   - Progressive Theological Faction: Primary beneficiary (institutional/arbitrage) — gains interpretive authority and reform authorization from the reading; can cite 'spirit' to defend development beyond text
 *   - Doctrinal Stability Commitment: Primary victim (powerless/identity_locked) — the pre-conciliar understanding of doctrinal immutability is structurally undermined; cannot contest the reading from within the institutional framework that the reading reframes
 *   - Traditionalist Interpretation Authority: Secondary victim (organized/constrained) — loses monopoly on authoritative interpretation but retains textual bases for defense; constrained by institutional power imbalances post-1970
 *   - Post-Conciliar Magisterium: Institutional actor (institutional/constrained) — must maintain Vatican II authority while suppressing destabilizing hermeneutical consequences; subject to competing interpretations it cannot definitively resolve
 *   - Reform-Minded Bishops and Clergy: Secondary actor (moderate/mobile) — see the reading as temporary authorization for necessary institutional renewal; can exit or reduce public involvement if renewal proves contentious
 *   - Institutional Church Apparatus: Institutional maintenance actor (institutional/constrained) — sustains the reading through inertia; structures built on it cannot be easily dismantled despite formal rejection by papal authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contested hermeneutical choice as inevitable doctrinal development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rupture_progressive_reading, 0.52).
domain_priors:suppression_score(rupture_progressive_reading, 0.45).
domain_priors:theater_ratio(rupture_progressive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rupture_progressive_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(rupture_progressive_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(rupture_progressive_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(rupture_progressive_reading, "Vatican II Rupture-Progressive Reading: Authorized Doctrinal Renewal Beyond Text").
narrative_ontology:topic_domain(rupture_progressive_reading, "ecclesiology/institutional_authority/hermeneutics").

domain_priors:requires_active_enforcement(rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(rupture_progressive_reading, fixed_text).
narrative_ontology:cs_authority_grounding(rupture_progressive_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(rupture_progressive_reading).
narrative_ontology:cs_kernel_id(rupture_progressive_reading, vatican_ii_doctrinal_authority).
narrative_ontology:cs_reading_relation(rupture_progressive_reading, continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation(rupture_progressive_reading, rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_axiom(rupture_progressive_reading, foundational, vatican_ii_as_necessary_rupture).
narrative_ontology:cs_axiom_status(vatican_ii_as_necessary_rupture, holdable).
narrative_ontology:cs_axiom_grounding(rupture_progressive_reading, vatican_ii_as_necessary_rupture, instrumental).
narrative_ontology:cs_axiom(rupture_progressive_reading, foundational, spirit_exceeds_text_authority).
narrative_ontology:cs_axiom_status(spirit_exceeds_text_authority, overridden).
narrative_ontology:cs_axiom_grounding(rupture_progressive_reading, spirit_exceeds_text_authority, deontological).
narrative_ontology:cs_reference_frame(rupture_progressive_reading, conciliar_authorization_of_institutional_flexibility).
narrative_ontology:cs_drift_state(rupture_progressive_reading, post_john_paul_ii_era, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rupture_progressive_reading, progressive_theological_faction).
narrative_ontology:constraint_beneficiary(rupture_progressive_reading, conciliar_reform_constituency).
narrative_ontology:constraint_victim(rupture_progressive_reading, doctrinal_stability_commitment).
narrative_ontology:constraint_victim(rupture_progressive_reading, traditionalist_interpretation_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINAL STABILITY COMMITMENT (SNARE) — Cannot exit the interpretive instability created by rupture framing. The pre-conciliar understanding of magisterial immutability is structurally undermined by the reading that treats Vatican II as a break, yet doctrinal stability has no independent voice to contest this. Identity-locked because the commitment itself is constituted through the institutional framework that the rupture reading reframes. Bears maximum extraction — the commitment is gutted by the interpretive move without ability to defend itself within the new framework.
constraint_indexing:constraint_classification(rupture_progressive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: TRADITIONALIST INTERPRETATION AUTHORITY (TANGLED ROPE) — Organized traditionalist bishops and theologians benefit from coherent hermeneutical frameworks (continuity reading) but are constrained by institutional power imbalances post-1970. The rupture reading simultaneously forecloses traditionalist authority (treats pre-conciliar doctrine as superseded) while the Church's formal magisterial apparatus maintains both readings in unresolved tension. Mixed extraction: traditionalists lose interpretive monopoly but retain institutional positions and textual bases for defense.
constraint_indexing:constraint_classification(rupture_progressive_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PROGRESSIVE THEOLOGICAL FACTION (ROPE) — Benefits substantially from the rupture reading. The 'spirit of the Council' formula authorizes doctrinal development that textual conservatives must defend in detail. Experiences the constraint as pure coordination: the reading enables reform implementation, eliminates need to establish textual warrant for every development, and provides flexibility for institutional adaptation. Net beneficiary with full arbitrage — can cite the Council when claiming authority, cite 'spirit' when defending specifics.
constraint_indexing:constraint_classification(rupture_progressive_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-CONCILIAR MAGISTERIUM (TANGLED ROPE) — Constrained by dual authority burden. Must maintain Vatican II's formal authority while suppressing its destabilizing hermeneutical consequences (the 'hermeneutics of rupture' that John Paul II criticized). The magisterium benefits from flexibility in doctrinal development (rope function) but is extracted from by the instability created when 'authentic implementation' of the Council proves contested. Active enforcement required to maintain the reading against countervailing interpretations.
constraint_indexing:constraint_classification(rupture_progressive_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM-MINDED BISHOPS AND CLERGY (SCAFFOLD) — Moderate power with mobile exit options (can retire, reduce public role, or migrate between progressive/conservative dioceses). See the rupture reading as a temporary authorization for institutional renewal — a scaffold enabling necessary structural changes before the Church stabilizes on a new form. Theater low because the reading has concrete programmatic force (liturgical change, educational reform, expanded lay roles). Sunset clause implicit: once institutional transformation is complete, the 'spirit' can be institutionalized into stable norms.
constraint_indexing:constraint_classification(rupture_progressive_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: INSTITUTIONAL CHURCH APPARATUS (PITON) — The apparatus maintains rupture-progressive reading through inertia while formal authority figures increasingly distance themselves from it. The reading persists in seminary curricula, diocesan implementation, and pastoral practice despite being formally rejected (or reframed as 'hermeneutics of continuity') by John Paul II and Benedict XVI. Theater high because the reading's functional force has decayed — it no longer organizes doctrine coherently, yet institutional structures built on it cannot be easily dismantled. Piton classification reflects degradation: the reading was a live doctrinal principle (1970s-1980s) but is now sustained primarily through institutional weight and historical inertia rather than active theological defense.
constraint_indexing:constraint_classification(rupture_progressive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL DEVELOPMENT VIEW (MOUNTAIN) — From a civilizational vantage, the reading represents a natural and inevitable development in doctrinal understanding: any living tradition must adjust to new circumstances; rigidity is structurally impossible over civilizational timescales. Vatican II rupture is thus not a contingent institutional choice but an expression of how doctrine necessarily evolves. However, this perspective naturalizes what is actually a contested hermeneutical claim — the reading is one framing of Vatican II among others, not an immutable law of doctrinal development. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(rupture_progressive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rupture_progressive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rupture_progressive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rupture_progressive_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rupture_progressive_reading, TR),
    TR >= 0.70.

:- end_tests(rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The rupture-progressive reading grants concentrated authority to progressive theological elites and reform-minded bishops to implement changes that exceed explicit textual warrant. This is genuine extraction from the pre-conciliar doctrinal stability framework and from traditionalist interpretive authority. However, the reading is not pure snare (0.70+) because it serves a coordination function — Vatican II genuinely required some mechanism for institutional adaptation to post-war Catholic demographic and cultural changes. The 'spirit' formula solved a real coordination problem (how to authorize reform without formal doctrinal rupture). The extractiveness trajectory reflects increasing recognition of the reading's extraction function as papal authority (John Paul II, Benedict XVI) explicitly rejected it, yet institutional implementation continued based on the reading's accumulated momentum. Suppression (0.45): Moderate. The reading is suppressed by formal papal rejection (magisterial countervailing authority) and by textual arguments from continuity advocates, but it is not totally suppressed — it remains live in theological education, diocesan implementation, and pastoral practice. The suppression is institutional (top-down papal authority) rather than structural (no alternative exists). Theater ratio (0.38, rising to 0.38): Moderate-low but rising. The reading initially had genuine programmatic force (organizing liturgical reform, educational restructuring, expanded lay ministry). But by 2005, the reading's functional force had decayed — it no longer coherently organizes doctrine, yet institutional structures built on it persist. The theater increase reflects that the reading is increasingly performative: invoked to justify institutional practices that would be difficult to defend from the Council's actual texts. Claimed type (Tangled Rope): The reading contains both genuine coordination (authorization for needed institutional adaptation) and asymmetric extraction (concentrating interpretive authority among progressive elites, undermining doctrinal stability commitment). Active enforcement required to maintain the reading against papal and traditionalist countervailing claims.
 *
 * PERSPECTIVAL GAP:
 *   The rupture-progressive reading produces maximum perspectival divergence. For progressive theologians (rope), the reading authorizes necessary reform and appears as pure coordination. For traditionalists (tangled rope), it is simultaneously foreclosing and constraining — their interpretive authority is undermined while they retain partial institutional positions. For doctrinal stability (snare), the reading is pure extraction with no exit. For the post-conciliar magisterium (tangled rope), the reading solves one problem (authorizing reform) while creating another (hermeneutical instability that papal authority cannot fully suppress). For reform-minded bishops (scaffold), it is temporary authorization with an implicit sunset — once institutional changes are embedded, the 'spirit' can be institutionalized. For the institutional apparatus (piton), it is degraded performance — originally functional but now sustained by inertia. For the analytical observer (mountain), the reading risks naturalizing what is actually a hermeneutical choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural position relative to the extraction flow. Progressive theologians as beneficiaries with arbitrage options experience low d (~0.20) → negative effective extraction. Traditionalists as organized victims with constrained exit experience moderate d (~0.55). Doctrinal stability as powerless victim with identity-lock experiences high d (~0.85). The post-conciliar magisterium as institutional actor facing constrained coordination experiences moderate-high d (~0.60). The analytical observer's d is derived from the analytical context canonical value (~0.73). The engine computes chi from these d values and f(d) sigmoid; beneficiaries experience low chi, victims experience high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this reading is resolved by recognizing that Vatican II's textual ambiguities genuinely permit multiple coherent interpretations, and the choice between them is not empirically determined but represents a genuine hermeneutical decision. The rupture-progressive reading is one such coherent interpretation; the continuity reading (sibling constraint) is another. Neither is 'correct' in an absolute sense — both are defensible readings of the conciliar texts and the Council's institutional context. The constraint's structure reflects this incommensurability: the same factual base (Vatican II's documents and post-conciliar events) supports multiple classifications depending on the hermeneutical framework applied. The mandatrophy resolves not by discovering which reading is true, but by acknowledging that both readings are live options held by different institutional factions, and the constraint represents the structural instability created by Vatican II's intentional ambiguity — an institution attempting to authorize both continuity and change without specifying which takes precedence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_authority_boundary,
    'Does the ''spirit of the Council'' (as distinguished from its texts) constitute genuine magisterial authority, or does it function as a hermeneutical license that exceeds the Council''s actual competence?',
    'Examination of Vatican II''s formal constitutions and decrees for language authorizing development beyond explicit text; comparison with John Paul II''s rejection of ''hermeneutics of rupture'' and attempt to establish ''hermeneutics of continuity''; analysis of post-conciliar papal statements that invoke vs. constrain the spirit formula',
    'If spirit has independent authority: rupture reading is justified and extractiveness drops to ~0.35 (pure coordination for reform). If spirit is hermeneutical device only: rupture reading is extraction (presumes authority it was not given), and extractiveness rises to ~0.65 (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spirit_authority_boundary, conceptual, 'Whether ''spirit of the Council'' constitutes independent magisterial authority').

omega_variable(
    vatican_ii_textual_intent,
    'Did Vatican II intend its own texts to be superseded by extrapolated ''spirit,'' or did the Council treat the texts themselves as the binding normative form?',
    'Study of Council debates (Acta Synodalia), papal addresses during and immediately after Council, and the conciliar documents'' own language about their normative force; historical reconstruction of what bishops understood themselves to authorize',
    'If texts were treated as binding norms: rupture reading misrepresents conciliar intent, and extractiveness rises to ~0.70 (snare/foreclosed reading). If Council intended texts as openings for further development: rupture reading is structurally vindicated, and extractiveness drops to ~0.30 (rope/scaffold hybrid).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vatican_ii_textual_intent, empirical, 'Vatican II''s own understanding of the normative force of its texts').

omega_variable(
    implementation_fidelity_verification,
    'How much of post-conciliar institutional change (liturgical reform, educational restructuring, lay ministry expansion) represents authentic doctrinal development vs. administrative overreach or cultural capitulation unwarranted by the texts?',
    'Detailed comparison of specific post-conciliar changes (e.g., Latin Mass suppression, altar repositioning, catechesis revision) against explicit conciliar text; documentation of where implementation exceeded or contradicted what Vatican II actually authorized; testimony from bishops and theologians about what was decided vs. what occurred',
    'If fidelity high: rupture reading captures genuine conciliar intent, extractiveness drops to ~0.35. If fidelity low: rupture reading is cover story for institutional drift, extractiveness rises to ~0.75 (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_verification, empirical, 'Fidelity of post-conciliar implementation to conciliar authorization').

omega_variable(
    hermeneutical_framework_committer_choice,
    'Is the choice between rupture reading and continuity reading grounded in the Council''s own textual evidence (and thus empirically resolvable), or is it fundamentally a choice about which interpretive framework to apply to inherently ambiguous conciliar language (and thus a preference/committer decision)?',
    'Analysis of whether Vatican II contains unambiguous passages that settle the rupture vs. continuity question, or whether both readings can claim equal textual support; assessment of whether disagreement is about the facts (what Vatican II said) or about methodology (how to read contested texts)',
    'If empirically resolvable: one reading is correct and the other is constrained (reclassification possible). If methodological choice: both readings remain live (coexist), and the constraint represents genuine structural incommensurability between hermeneutical frameworks rather than factual disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutical_framework_committer_choice, conceptual, 'Whether rupture vs. continuity is empirically resolvable or a hermeneutical choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rupture_progressive_reading, 1965, 2005).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rupt_tr_t0, rupture_progressive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(rupt_tr_t5, rupture_progressive_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(rupt_tr_t10, rupture_progressive_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(rupt_be_t0, rupture_progressive_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rupt_be_t5, rupture_progressive_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(rupt_be_t10, rupture_progressive_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rupture_progressive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rupture_progressive_reading, continuity_reading).
narrative_ontology:affects_constraint(rupture_progressive_reading, rupture_traditionalist_reading).
narrative_ontology:affects_constraint(rupture_progressive_reading, post_conciliar_liturgical_implementation).
narrative_ontology:affects_constraint(rupture_progressive_reading, magisterial_authority_degradation).

% DUAL FORMULATION NOTE:
% The Vatican II kernel decomposes into three structurally distinct constraint stories: continuity_reading (ε~0.25, Rope — treats texts as binding norms, development as hermeneutical elaboration), rupture_progressive_reading (ε~0.52, Tangled Rope — treats texts as ambiguous openings, development as authorized 'spirit'), and rupture_traditionalist_reading (ε~0.72, Snare — treats post-conciliar changes as institutional betrayal). Each story has its own ε, its own beneficiary/victim structure, and its own measurement trajectory. They are linked via network.affects_constraints because doctrinal change in one reading (e.g., progressive development) creates constraints and opportunities for the others (traditionalist resistance, magisterial disambiguation attempts). The extractiveness divergence (0.25 vs 0.52 vs 0.72) reflects different assessments of whether Vatican II's changes represent legitimate doctrinal development (continuity), necessary but limited reform (rupture-progressive), or institutional betrayal (rupture-traditionalist). No single ε value can capture all three — the readings are empirically incommensurable at the level of extractiveness calculation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rupture_progressive_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
