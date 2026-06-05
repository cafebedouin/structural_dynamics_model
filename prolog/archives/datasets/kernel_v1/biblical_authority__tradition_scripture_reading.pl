% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Scripture Requires Tradition for Authoritative Interpretation; Magisterium Guards Deposit of Faith
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint instantiates the Catholic theological position that
 *   authoritative scriptural interpretation requires living tradition
 *   transmitted through the institutional magisterium. The binding claim is
 *   metaphysical: scripture's meaning is not self-evident but requires
 *   authoritative tradition to stabilize doctrine and prevent heretical
 *   fragmentation. The magisterium (pope, bishops, formal councils) guards
 *   the 'deposit of faith' by controlling what counts as legitimate tradition
 *   and therefore legitimate scripture reading. From a structural analysis
 *   perspective, this arrangement creates both genuine coordination (unified
 *   doctrine, sacramental efficacy, prevention of doctrinal collapse) and
 *   significant extraction: lay interpretive agency is subordinated to
 *   clerical authority; access to sacred meaning is mediated through
 *   institutional gatekeeping; salvation (sacramental grace) is conditional
 *   on obedience to magisterial teaching. The constraint exists in dynamic
 *   tension with alternative readings (Sola Scriptura: scripture alone,
 *   without binding tradition; Conciliar reading: ecumenical councils as
 *   primary authority). The measurement trajectory shows declining
 *   extractiveness over 15 historical units (late medieval through
 *   post-Vatican II) as literacy, printing, and institutional reforms
 *   (Vatican II's Dei Verbum) have granted lay believers greater access to
 *   scripture and scriptural study. However, suppression remains high (0.60
 *   at endpoint) because the magisterium continues to claim ultimate
 *   interpretive authority, and theater ratio has risen (0.35→0.50) as the
 *   functional binding mechanism has weakened while institutional performance
 *   persists.
 *
 * KEY AGENTS:
 *   - Institutional Magisterium: Primary beneficiary (institutional/arbitrage) — controls interpretive authority, receives lay obedience, maintains sacramental monopoly, derives institutional power from interpretation gatekeeping
 *   - Lay Believers: Primary victim (powerless/trapped) — cannot access authoritative scriptural meaning without clerical mediation; dependent on confession, sacraments, catechesis; subject to heresy accusations for independent interpretation
 *   - Regional Bishops & Theologians: Secondary agent (moderate/constrained) — benefit from magisterial framework (legitimacy, institutional standing, doctrinal clarity) but constrained by magisterial authority; cannot innovate doctrinally without approval
 *   - Doctrine & Doctrinal Pluralism: Structural victim — the constraint aims to prevent fragmentation, creating artificial doctrinal unity at the cost of suppressing legitimate theological disagreement
 *   - Reformation Movements: Organized challenger (organized/constrained) — explicitly reject tradition-mediated authority; see Sola Scriptura and printing press as alternative verification mechanism; drive constraint into decline over centuries
 *   - Contemporary Post-Vatican II Church: Institutional re-configurer (institutional/arbitrage) — formally acknowledges lay scriptural agency while maintaining ultimate magisterial authority; constraint persists via institutional inertia and theological training requirements rather than functional control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.58).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.65).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Scripture Requires Tradition for Authoritative Interpretation; Magisterium Guards Deposit of Faith").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, 'cc764aa9-84d2-4975-8378-16c7c4ab932b').
narrative_ontology:cs_kernel_codification('cc764aa9-84d2-4975-8378-16c7c4ab932b', formalized).
narrative_ontology:cs_authority_grounding('cc764aa9-84d2-4975-8378-16c7c4ab932b', extraction).
narrative_ontology:cs_interpretation_layer_present('cc764aa9-84d2-4975-8378-16c7c4ab932b').
narrative_ontology:cs_reading_relation('cc764aa9-84d2-4975-8378-16c7c4ab932b', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('cc764aa9-84d2-4975-8378-16c7c4ab932b', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('cc764aa9-84d2-4975-8378-16c7c4ab932b', foundational, living_magisterium_authoritative_interpreter).
narrative_ontology:cs_axiom_status(living_magisterium_authoritative_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('cc764aa9-84d2-4975-8378-16c7c4ab932b', living_magisterium_authoritative_interpreter, conventional).
narrative_ontology:cs_axiom('cc764aa9-84d2-4975-8378-16c7c4ab932b', foundational, tradition_stabilizes_scriptural_meaning).
narrative_ontology:cs_axiom_status(tradition_stabilizes_scriptural_meaning, holdable).
narrative_ontology:cs_axiom_grounding('cc764aa9-84d2-4975-8378-16c7c4ab932b', tradition_stabilizes_scriptural_meaning, empirically_contingent).
narrative_ontology:cs_reference_frame('cc764aa9-84d2-4975-8378-16c7c4ab932b', apostolic_tradition_mediated_by_episcopal_hierarchy).
narrative_ontology:cs_drift_state('cc764aa9-84d2-4975-8378-16c7c4ab932b', contemporary_post_vatican_ii, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('cc764aa9-84d2-4975-8378-16c7c4ab932b', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, institutional_magisterium).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_scriptural_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, doctrinal_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY BELIEVER (SNARE) — Cannot access authoritative scriptural meaning without clerical mediation. Trapped by institutional gatekeeping and cultural dependence on priestly interpretation. No alternatives for sacramental grace. Experiences constraint as pure extraction — authority over text and salvation becomes institutional monopoly.
constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL BISHOP / THEOLOGIAN (TANGLED ROPE) — Constrained by magisterial authority but also benefits from tradition-mediated interpretive stability and institutional standing. Can offer guidance within boundaries; extraction is real (limited interpretive autonomy) but benefits exist (clarity, resources, legitimacy). Significant cost to doctrinal innovation but also protected from error.
constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL MAGISTERIUM (ROPE) — Primary beneficiary. Interprets constraint as coordination mechanism: tradition-mediated authority preserves doctrinal unity, prevents fragmentation, and enables sacramental efficacy. Extraction toward this agent (interpretive monopoly, mandatory obedience, confession requirements) is experienced as legitimate coordination overhead. Net benefit: control over meaning, spiritual authority, institutional power.
constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORMATION MOVEMENTS (SCAFFOLD) — Organized challenge to the constraint with explicit sunset logic: Sola Scriptura and the printing press create alternative verification pathways. Reformers see the tradition-magisterium system as a temporary coordination mechanism that can be replaced by decentralized scriptural access and translation. The constraint's extraction force decays as literacy rises and printed texts proliferate. Sunset: ~200 years for institutional disestablishment in Protestant regions.
constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-VATICAN II MAGISTERIUM (PITON) — The institutional church acknowledges lay agency (Dei Verbum 1965: 'All the faithful share in understanding and expounding sacred scripture') while maintaining ultimate magisterial authority. The original constraint has degraded: lay Bible studies are now normative; vernacular liturgy replaces Latin gatekeeping; tradition-mediated authority persists through inertia and theological training requirements rather than functional control. Theater ratio is high because the constraint's functional purpose (preventing doctrinal fragmentation) has been partially overtaken by catechesis, yet institutional mediation persists. The magisterium still extracts (reserves interpretive authority, controls doctrinal boundaries) but the binding mechanism has weakened.
constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL AUTHORITY VIEW (MOUNTAIN) — From a civilizational perspective, authoritative scriptural interpretation inherently requires a stable reference frame (tradition) because language is degraded by temporal distance and cultural drift. Meaning cannot be 'naked' in text — it requires a community of interpretation to remain coherent across centuries. This perspective sees the magisterium as a natural law of hermeneutics itself: any stable interpretation system requires gatekeeping authority. However, the structural data reveals this as a false summit: the 'necessity' of clerical mediation is contingent on sacramental theology (grace flows through institutional sacraments) and not on textual hermeneutics alone.
constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_authority__tradition_scripture_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, TR),
    TR >= 0.70.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The magisterium benefits substantially from interpretive authority — it controls meaning, receives obedience, and founds its spiritual authority partly on this gatekeeping. However, extractiveness is not extreme (0.72+) because the constraint does serve genuine coordination functions: it prevents doctrinal chaos, provides unified spiritual guidance, and (from the magisterium's perspective) protects grace-conferring sacraments from doctrinal contamination. The extraction is mixed with real coordination benefit. Suppression (0.65): High. Barriers to lay interpretive agency are substantial: illiteracy (historically), Latin gatekeeping, institutional punishment (inquisition, heresy charges), sacramental dependence, cultural authority of the priesthood, and theological training requirements. However, suppression is not total because (historically) lay biblical devotion and mystical interpretation existed within constraints; (contemporary) Vatican II and literacy have substantially reduced barriers. Theater ratio (0.48): Moderate. The constraint's functional purpose (preventing doctrinal fragmentation and unifying sacramental theology) is real, not purely theatrical. But post-Vatican II, lay Bible studies are normative and Vatican II itself acknowledged lay agency; the original binding mechanism has weakened, yet institutional mediation persists through theological training, catechetical authority, and formal doctrinal pronouncements. The rise in theater ratio from 0.35 to 0.50 reflects this degradation: the constraint persists more through institutional inertia than through functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how indexical classification reveals deeply divergent perceptions of the same structural phenomenon. The magisterium (institutional/arbitrage) sees this as pure coordination (rope): tradition prevents doctrinal chaos and enables unified sacramental theology. Lay believers (powerless/trapped) see it as extraction (snare): they are locked out of meaning and dependent on clerical gatekeeping with no alternatives. Regional bishops (moderate/constrained) see it as mixed (tangled rope): they benefit from institutional clarity but are constrained by authority they cannot override. Reformation movements (organized/constrained) see it as temporary and solvable (scaffold): Sola Scriptura and printing press offer exit routes; the constraint is a coordination failure, not a law. The post-Vatican II church (institutional/arbitrage) sees it as degraded (piton): the original binding mechanism has weakened, yet institutional mediation persists through tradition and training. The analytical observer risks seeing it as natural (mountain): 'any interpretation requires tradition; textual meaning is never naked' — but this naturalizes what is functionally contingent on sacramental theology and clerical monopoly. The engine's false summit detector will flag this last perspective as fraudulent naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derivation for each perspective: (1) Lay Believer: powerless agent + trapped exit + victim of interpretive gatekeeping → d ≈ 0.95, f(d) ≈ 1.42 → high experienced extractiveness χ. (2) Regional Bishop: moderate power + constrained exit + mixed beneficiary (gains legitimacy) and victim (limited autonomy) → d ≈ 0.55, f(d) ≈ 0.75 → moderate χ. (3) Magisterium: institutional power + arbitrage exit + primary beneficiary → d ≈ 0.05, f(d) ≈ -0.12 → negative χ (benefits from constraint). (4) Reformation: organized power + constrained exit + challenger to constraint → d ≈ 0.60, f(d) ≈ 0.85 → moderate χ; but exits are available (Sola Scriptura, new churches), so not maximum. (5) Post-Vatican II: institutional power + arbitrage exit + primary beneficiary but partially degraded → d ≈ 0.20, f(d) ≈ 0.02 → near-zero χ (reduced extraction). (6) Analytical: analytical observer + universal scope + risk of naturalizing contingent arrangement → d ≈ 0.72, f(d) ≈ 1.15 → observes high χ but this is driven by the constraint's false naturalization, not by genuine hermeneutic necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   READING SPECIFICATION: This constraint is ONE reading of the contested kernel biblical_authority. The sibling readings are sola_scriptura_reading (scripture alone; tradition advisable but not binding) and conciliar_reading (ecumenical councils as primary authority). This reading holds that LIVING TRADITION mediated through the INSTITUTIONAL MAGISTERIUM is the primary authority for scriptural interpretation. This is not a mere preference or interpretive emphasis — it is a metaphysical claim about where authority resides. The mandatrophy (the paradox that a tangled rope can appear as a mountain, snare, or rope depending on perspective) is resolved by recognizing that perspectives reveal real structural features: the coordination function (unified doctrine, sacramental efficacy) is genuine; the extraction (lay agency suppression, interpretive monopoly) is also genuine. Both are real. The classification as tangled_rope is correct across perspectives because the constraint's defining feature is that it serves BOTH coordination AND extraction simultaneously. The false summit (mountain from analytical view) is not a mandatrophy to resolve — it is a false summit to detect and flag. When the engine computes the analytical observer's 'mountain' classification, the FSM detector will trigger because beneficiaries are declared (magisterium) — revealing that the 'naturalness' of tradition-mediated authority is a constructed institutional interest, not a law of hermeneutics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_necessity_vs_institutional_interest,
    'Is tradition-mediated authority a hermeneutically necessary feature of textual stability, or a functionally contingent institutional arrangement protecting sacramental monopoly?',
    'Comparative analysis: (1) hermeneutic stability in non-sacramental traditions (Jewish rabbinical interpretation, Islamic tafsir, scholarly secular commentaries) versus (2) rate of doctrinal fragmentation in Sola Scriptura communities versus (3) empirical correlation between interpretive stability and magisterial enforcement vs. other coordination mechanisms (printing press, literacy, catechesis, academic standards).',
    'If hermeneutically necessary: mountain classification is correct; tradition-mediated authority is inherent to meaning-stabilization. If institutionally contingent: false summit — the magisterium''s claim to naturalistic necessity masks an extractive institutional interest in controlling sacramental mediation. Entire classification shifts from mountain to tangled_rope or snare depending on empirical outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_necessity_vs_institutional_interest, empirical, 'Whether tradition-mediated authority is hermeneutically necessary or institutionally contingent').

omega_variable(
    sacramental_grace_metaphysics_vs_linguistic_authority,
    'Does sacramental efficacy (grace conferred through institutional sacraments) logically require magisterial interpretive authority, or are sacramental theology and scriptural hermeneutics independent domains?',
    'Theological exegesis: (1) comparison of Early Church sacramental theology (pre-magisterial centralization) with late-medieval systematic theology; (2) analysis of whether Reformation sacramental theologies (Lutheran, Reformed, Anabaptist) require less interpretive authority while maintaining grace claims; (3) empirical study of whether sacramental practice (baptism, eucharist) correlates with doctrinal coherence better than scriptural interpretation does.',
    'If coupled: sacramental theology structurally depends on magisterial authority; the constraint is foundationally tangled rope (genuine coordination for grace + extraction for interpretive control). If decoupled: sacramental efficacy can exist without magisterial scriptural authority; the constraint becomes pure extraction masked as coordination (shifts toward snare). This is a conceptual/metaphysical determination that reframes what ''coordination'' actually coordinates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_grace_metaphysics_vs_linguistic_authority, conceptual, 'Whether sacramental grace requires magisterial interpretive authority').

omega_variable(
    lay_interpretive_agency_under_magisterium,
    'In historical practice, how much interpretive agency did lay believers actually exercise within the tradition-mediated framework? Was constraint truly ''trapped'' or ''constrained''?',
    'Historical analysis: (1) medieval lay biblical commentary traditions (mystical devotion, allegorical piety, lay theology); (2) parish-level catechesis and lay preaching practices; (3) marginal annotations and ownership marks in lay manuscripts; (4) Inquisition records of lay doctrinal knowledge and debate; (5) spiritual autobiography and confessional records showing lay interpretive reasoning.',
    'If lay agency was substantial: the ''powerless'' classification (trapped exit) overstates constraint severity; better classified as ''constrained'' with moderate power. Perspectival gap narrows between lay and institutional views. If lay agency was severely restricted: ''trapped'' classification confirmed; magisterium extractiveness was nearer 0.70+ historically. Contemporary post-Vatican II lay agency represents genuine institutional shift (not fake liberalization masking control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_interpretive_agency_under_magisterium, empirical, 'Historical scope of lay interpretive agency within magisterial framework').

omega_variable(
    sola_scriptura_doctrinal_fragmentation_empirics,
    'Did Sola Scriptura regions (Protestant) exhibit greater doctrinal fragmentation than tradition-mediated regions (Catholic, Orthodox), or did other factors (political, linguistic, institutional) drive denomination splitting?',
    'Quantitative analysis: (1) rate of schism and doctrinal divergence in Protestant vs Catholic regions over same historical periods; (2) multivariate regression: denominational splits vs literacy rates, printing press diffusion, political sovereignty, economic development, language standardization; (3) comparison of doctrinal stability within Sola Scriptura vs Tradition frameworks controlling for institutional centralization (some Lutheran churches highly centralized, some post-Reformation traditions fragmented despite strong magisterium).',
    'If tradition-mediated: tradition reduces fragmentation independent of other factors; rope classification for the coordination function is validated. If other factors dominant: the magisterium''s claim that tradition-mediated authority is necessary for unity is overstated; fragmentation is driven by politics and economics, not epistemology. Extraction becomes harder to justify as coordination overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sola_scriptura_doctrinal_fragmentation_empirics, empirical, 'Whether Sola Scriptura caused greater doctrinal fragmentation than tradition-mediated authority').

omega_variable(
    contested_kernel_reading_distinction,
    'What precisely distinguishes THIS reading (tradition-scripture-magisterium) from the sibling sola_scriptura_reading and conciliar_reading in terms of which agent controls interpretive authority and under what metaphysical warrant?',
    'Axiomatic distinction: (1) this reading holds that TRADITION (continuous transmission through living magisterium) is the primary authority, with scripture subordinate to authoritative tradition-mediated reading; (2) sola_scriptura reading holds that SCRIPTURE ALONE is the authority, with tradition advisable but not binding; (3) conciliar reading holds that ECUMENICAL COUNCILS are the primary authority, with both scripture and tradition subject to conciliar adjudication. These are not mere differences of opinion — they are incompatible epistemic claims about WHERE authority resides.',
    'Resolving this distinction is essential for applying reading_relations correctly. This reading (tradition supremacy via living magisterium) FORECLOSES sola_scriptura at the metaphysical level (if tradition is binding, scripture-alone is logically impossible in the same framework). But it may COEXIST_WITH conciliar reading (both can hold that councils mediate tradition) or INFLUENCE conciliar reading (if tradition-mediated authority is primary, councils are its expression). The engine needs this distinction to compute whether readings genuinely foreclose or merely coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_reading_distinction, conceptual, 'Axiomatic distinguishment of tradition-scripture-magisterium from sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibauth_trad_theater_late_medieval, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bibauth_trad_theater_counter_reformation, biblical_authority__tradition_scripture_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement(bibauth_trad_theater_enlightenment, biblical_authority__tradition_scripture_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(bibauth_trad_theater_modern, biblical_authority__tradition_scripture_reading, theater_ratio, 9, 0.46).
narrative_ontology:measurement(bibauth_trad_theater_contemporary, biblical_authority__tradition_scripture_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(bibauth_trad_theater_post_vatican_ii, biblical_authority__tradition_scripture_reading, theater_ratio, 15, 0.5).

% Extraction over time
narrative_ontology:measurement(bibauth_trad_extractiveness_late_medieval, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(bibauth_trad_extractiveness_counter_reformation, biblical_authority__tradition_scripture_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(bibauth_trad_extractiveness_enlightenment, biblical_authority__tradition_scripture_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(bibauth_trad_extractiveness_modern, biblical_authority__tradition_scripture_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(bibauth_trad_extractiveness_contemporary, biblical_authority__tradition_scripture_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(bibauth_trad_extractiveness_post_vatican_ii, biblical_authority__tradition_scripture_reading, base_extractiveness, 15, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(bibauth_trad_suppression_late_medieval, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(bibauth_trad_suppression_counter_reformation, biblical_authority__tradition_scripture_reading, suppression_requirement, 3, 0.78).
narrative_ontology:measurement(bibauth_trad_suppression_enlightenment, biblical_authority__tradition_scripture_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(bibauth_trad_suppression_modern, biblical_authority__tradition_scripture_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(bibauth_trad_suppression_contemporary, biblical_authority__tradition_scripture_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(bibauth_trad_suppression_post_vatican_ii, biblical_authority__tradition_scripture_reading, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sacramental_efficacy__institutional_mediation).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, doctrinal_fragmentation__unity_coordination).

% DUAL FORMULATION NOTE:
% The biblical_authority kernel decomposes into three structurally distinct constraints: tradition_scripture_reading (magisterium guards deposit via living tradition; ε≈0.58), sola_scriptura_reading (scripture alone sufficient; ε≈0.25 — less extractive because no clerical monopoly required), and conciliar_reading (councils mediate both; ε≈0.42 — intermediate extraction by conciliar authority). Each reading has its own ε because each makes different empirical claims about what coordination mechanism is necessary and who benefits. Network edges link all three; affect_constraints shows which constraint upstream legitimates which. The tradition_scripture reading draws legitimacy partly from sacramental_efficacy__institutional_mediation (sacraments work through proper intention of priest holding magisterial authority) and from doctrinal_fragmentation__unity_coordination (tradition prevents chaos). These are separate constraints in the family; their extractiveness values differ from the kernel reading itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, institutional, 0.08).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
