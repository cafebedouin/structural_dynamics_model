% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Magisterial Authority (Continuity Reading)
 *   domain: ecclesiastical/hermeneutics/institutional_authority
 *
 * SUMMARY:
 *   The continuity reading of Vatican II proposes that the Second Vatican
 *   Council represents organic development within the unbroken tradition of
 *   Catholic teaching, with no rupture in the magisterium's core doctrinal
 *   commitments. This reading claims that conciliar documents should be
 *   interpreted conservatively, constrained by pre-conciliar doctrine, with
 *   the 'spirit of Vatican II' — appeals to council intention beyond literal
 *   text — treated as unauthorized hermeneutic excess. The constraint
 *   manifests as a framework governing how Vatican II texts are implemented:
 *   pastoral adaptations must preserve pre-conciliar doctrine; religious
 *   freedom (Dignitatis Humanae) is reconcilable with the restrictive
 *   teaching of the Syllabus of Errors via the thesis/hypothesis distinction
 *   or development-of-doctrine logic; Latin preservation (Sacrosanctum
 *   Concilium §36) is binding despite widespread post-conciliar abandonment.
 *   This is one of three structurally distinct readings of the same contested
 *   kernel — the magisterial authority and hermeneutic status of Vatican II
 *   itself. The continuity reading serves identifiable institutional
 *   beneficiaries (the Vatican doctrinal magisterium, tradition-affirming
 *   academies) while constraining progressive implementation, creating an
 *   asymmetric extraction dynamic that qualifies the constraint as tangled
 *   rope: genuine coordination function (a framework for claiming both
 *   fidelity to council and doctrinal stability) combined with high
 *   suppression of alternative interpretations and asymmetric beneficiary
 *   structure.
 *
 * KEY AGENTS:
 *   - Vatican Doctrinal Magisterium (institutional/arbitrage): Primary beneficiary — the continuity reading preserves magisterial authority to bind interpretation across time by subordinating council texts to pre-conciliar doctrinal safeguards. High arbitrage capacity to reinterpret as needed.
 *   - Magisterial Conservatives & Pre-Conciliar Continuity Advocates (institutional/arbitrage): Secondary beneficiaries — the continuity reading validates their hermeneutic project and provides institutional backing for resistance to progressive pastoral applications.
 *   - Progressive Pastoral Theologians (moderate/constrained, often identity_locked): Primary victims — their professional identity is fused with 'spirit of Vatican II' interpretation; the continuity reading delegitimizes their entire theological project by declaring it unauthorized.
 *   - Diocesan Bishops (moderate/constrained): Mixed position — experience both coordination (the continuity reading provides a framework for claiming fidelity to both council and tradition) and extraction (pressure to restrain pastoral innovation).
 *   - Global Catholic Reform Movements (organized/constrained): Secondary victims — organized agents (base communities, theological networks) coordinating around Vatican II's liberalizing potential; the continuity reading constrains these frameworks back into pre-conciliar boundaries.
 *   - Tradition-Affirming Academies (institutional/arbitrage): Secondary beneficiaries — the continuity reading validates ressourcement theology and shapes theological education through university departments.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing a particular hermeneutic choice as the only coherent reading possible.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.58).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Magisterial Authority (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiastical/hermeneutics/institutional_authority").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, 'fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd').
narrative_ontology:cs_kernel_codification('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', fixed_text).
narrative_ontology:cs_authority_grounding('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', extraction).
narrative_ontology:cs_interpretation_layer_present('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd').
narrative_ontology:cs_reading_relation('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', vatican_ii_magisterial_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', foundational, magisterial_development_preserves_doctrinal_substance).
narrative_ontology:cs_axiom_status(magisterial_development_preserves_doctrinal_substance, holdable).
narrative_ontology:cs_axiom_grounding('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', magisterial_development_preserves_doctrinal_substance, deontological).
narrative_ontology:cs_axiom('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', foundational, conciliar_texts_constrain_spirit_of_council_hermeneutics).
narrative_ontology:cs_axiom_status(conciliar_texts_constrain_spirit_of_council_hermeneutics, holdable).
narrative_ontology:cs_axiom_grounding('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', conciliar_texts_constrain_spirit_of_council_hermeneutics, conventional).
narrative_ontology:cs_reference_frame('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', pre_conciliar_doctrinal_continuity).
narrative_ontology:cs_drift_state('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', post_conciliar_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc3d1ebb-45cc-48ba-8de4-f3d67b2ee5cd', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, magisterial_conservatives).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, pre_conciliar_doctrinal_continuity_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_implementers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, council_spirit_interpreters).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, contemporary_pastoral_adaptation_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROGRESSIVE PASTORAL THEOLOGIAN (SNARE) — Structurally mobile (constrained exit via career cost and institutional pressure, but mobile in principle) but identity-fused with conciliar aggiornamento project. Their professional identity is constituted through the 'spirit of Vatican II' interpretation. The continuity reading delegitimizes their entire theological project by declaring spirit-of-council reasoning 'unauthorized' and subordinating it to pre-conciliar doctrinal constraints. Exit from this binding would require abandoning their identity as a genuine agent of council implementation. High experienced extraction: they cannot credibly pursue their pastoral vision without violating magisterial constraints imposed retroactively.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DIOCESAN BISHOP (TANGLED ROPE) — Constrained by magisterial authority from above and pastoral necessity from below. Genuine coordination function: the continuity reading enables bishops to claim fidelity to both council and tradition by subordinating implementation to doctrinal preservation. But also extraction: bishops experience pressure to restrain pastoral innovation lest they authorize 'spirit of council' claims the continuity reading has already delegitimized. Moderate power, generational time horizon (institutional survival), constrained exit (career and institutional dependency).
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VATICAN DOCTRINAL MAGISTERIUM (ROPE) — Primary beneficiary. The continuity reading is a coordination mechanism from this perspective: it allows the Church to claim Vatican II as binding development while subordinating council implementation to pre-conciliar doctrinal safeguards. The magisterium experiences the constraint as pure coordination — a framework for resolving ambiguity in favor of doctrinal stability. High arbitrage capacity: the magisterium can reinterpret council texts as needed via the continuity hermeneutic, maintaining authority over both past and present teaching.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL CATHOLIC REFORM MOVEMENT (TANGLED ROPE) — Organized agents (base communities, theological networks, diaspora communities) coordinating around Vatican II's theological openings. Experience both coordination (the council did provide new frameworks for dialogue, ecological concern, laity engagement) and extraction (the continuity reading constrains these frameworks back into pre-conciliar doctrinal boundaries, delegitimizing implementations that go beyond narrow textual reading). Generational time horizon: reform movements are transgenerational; the constraint determines whether their work is magisterially sanctioned or gradually eroded.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITION-AFFIRMING ACADEMIES (ROPE) — Secondary beneficiaries. The continuity reading validates their hermeneutic project: Vatican II becomes a moment of doctrinal recovery ('ressourcement') rather than rupture. Experiences the constraint as enabling coordination — a framework for reintegrating council teaching into perennial doctrine. High arbitrage: these academies shape theological education and doctrinal interpretation through university theology departments.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HERMENEUTIC NECESSITY (MOUNTAIN) — From a universal/civilizational analytical perspective, the continuity reading reflects a structural necessity of any living tradition: texts do not interpret themselves, and every transmission of teaching across historical ruptures (Vatican I → Vatican II → post-conciliar implementation) requires a hermeneutic that mediates between past and present. The constraint appears as an immutable feature of how traditional authority systems maintain coherence. However, this perspective risks naturalizing a particular hermeneutic choice (continuity framing) as the only coherent reading possible.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from progressive implementation agents by delegitimizing their hermeneutic framework, forcing them to defend pastoral innovations within conservative textual bounds. The extraction is not maximal (snare-level) because the constraint is genuinely a coordination mechanism from the magisterium's perspective — it does enable the Church to claim both conciliar authority and doctrinal stability. But it is substantial because the extraction is asymmetric: beneficiaries (magisterium, conservatives) gain legitimacy and implementation authority, while victims (progressives) bear the cost of constrained pastoral options and delegitimized theological projects. Suppression (0.62): High. The continuity reading suppresses alternative hermeneutics by treating them as unauthorized ('spirit of council' excess). It also suppresses empirical reality: widespread post-conciliar Latin abandonment is treated as violation of SC §36 rather than as evidence that continuity-and-change dynamics do not follow the continuity reading's script. The suppression is enforced institutionally (magisterial teaching, doctrinal sanctions against dissenting theologians) rather than violently, but it is real. Theater ratio (0.68): Moderately high. The continuity reading performs a significant amount of hermeneutic work — distinguishing development from rupture, applying thesis/hypothesis distinctions, interpreting silence in texts as constraint. Much of this theater is necessary (all hermeneutics requires interpretive labor), but some is performative: the framework is applied selectively to reach conservative conclusions, and the consistency of application varies across doctrinal domains. The theater has increased over time as the cost of maintaining the continuity fiction against evidence of real pastoral change has required more elaborate justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The Vatican doctrinal magisterium experiences the continuity reading as pure coordination (Rope) — a framework for maintaining authority over both past and present. Beneficiary academies experience it as enabling (Rope). But diocesan bishops, caught between magisterial constraint and pastoral necessity, experience it as mixed (Tangled Rope). Progressive pastoral theologians with identity-fused commitments to 'spirit of Vatican II' experience it as identity-destroying snare — they cannot pursue their professional project without violating magisterial constraints imposed retroactively. The analytical observer risks the mountain classification — treating hermeneutic necessity (all traditions must interpret themselves) as if the continuity framework were the only possible hermeneutic — but the structural data reveals this as a false summit: the constraint's asymmetric beneficiary structure and selective enforcement show that continuity is one institutional choice among others, not an immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: Who benefits from the continuity reading being binding? The magisterium and conservative academies. Who bears the cost of constraint? Progressive implementation agents. The magisterium's arbitrage exit option (they can reinterpret as needed) produces low d; progressive theologians' identity-locked exit (they cannot abandon the 'spirit of council' project without ceasing to be themselves) produces high d. Organized reform movements face constrained exit (they can adapt, but at cost to their pastoral vision). Diocesan bishops face constrained exit but also benefit from having an authoritative framework. The composition of beneficiaries with arbitrage and victims with trapped/identity-locked/constrained exits produces the tangled_rope classification: genuine coordination (the magisterium really does need a hermeneutic framework) combined with asymmetric extraction (the choice of continuity framework over rupture or composite alternatives systematically favors conservatives).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that the continuity reading is neither pure coordination nor pure extraction, but a genuine hybrid. The coordination function is real: the Church needs some framework for relating council to tradition. But the extraction is also real: the choice of continuity framing (rather than rupture or composite alternatives) serves identifiable beneficiaries and constrains identifiable victims. The constraint's theater (the hermeneutic work of distinguishing development from rupture) is partly necessary coordination labor and partly performative rationalization of a conservative outcome. The omega variables identify the empirical and conceptual uncertainties that determine whether the reading is coherent (remains tangled_rope) or post-hoc rationalization (reclassifies toward snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    development_of_doctrine_threshold,
    'At what degree of change does legitimate organic development become structural rupture? Where does the continuity reading place the threshold, and is it coherent across all council documents?',
    'Detailed analysis of Vatican II texts on specific doctrines (religious freedom via Dignitatis Humanae, liturgical change via Sacrosanctum Concilium, ecumenism via Unitatis Redintegratio) to determine whether the continuity reading produces consistent hermeneutic standards or domain-specific thresholds. Comparison with Newman''s Essay on Development and Vatican I teaching on dogmatic development.',
    'If threshold is coherent and applies uniformly: continuity reading is internally consistent (remains rope/tangled_rope). If threshold is inconsistent or applies selectively to protect particular doctrines: the reading is post-hoc rationalization (reclassifies toward snare for beneficiaries who use the reading to constrain implementation). If threshold is so permissive that all council teaching is ''development'': continuity reading becomes empty (reclassifies as piton — theatrical maintenance of nominal continuity with no real constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(development_of_doctrine_threshold, empirical, 'Whether the continuity reading applies consistent development standards across all council doctrines').

omega_variable(
    thesis_hypothesis_distinction_validity,
    'Does the distinction between thesis (what the Church teaches as unchangeable) and hypothesis (temporary contingent applications) provide a coherent framework for reconciling religious freedom (Dignitatis Humanae) with pre-conciliar teaching (Syllabus of Errors, Immortale Dei), or does it collapse under scrutiny?',
    'Examine whether Vatican sources (Magisterium, doctrinal commentaries, official clarifications post-Vatican II) explicitly endorse the thesis/hypothesis distinction as applied to religious freedom. Determine whether the distinction can accommodate both the permissive DH formulation and the restrictive pre-conciliar formulations without treating one as simply false.',
    'If distinction holds: continuity reading''s doctrinal reconciliation is viable (remains tangled_rope). If distinction is rejected by Vatican sources or produces internal contradictions: the continuity reading is forced to either (a) declare pre-conciliar teaching superseded (reclassifies toward rupture_reading) or (b) declare DH as constrained by implicit qualifications (reclassifies toward snare — high suppression of DH''s liberalizing intent). If distinction is deployed selectively: the reading is post-hoc rationalization (snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(thesis_hypothesis_distinction_validity, empirical, 'Coherence of thesis/hypothesis distinction for reconciling religious freedom with pre-conciliar magisterium').

omega_variable(
    spirit_of_council_authorization_boundary,
    'What determines whether a pastoral application counts as authorized interpretation of council texts versus unauthorized ''spirit of Vatican II'' ideology?',
    'Identify the magisterial rule(s) the continuity reading uses to distinguish (a) legitimate development and pastoral application from (b) unauthorized spirit-of-council excess. Apply this rule consistently to major post-conciliar pastoral initiatives (base communities, synodal governance experiments, divorced-and-remarried communion access, women in liturgical roles, interfaith worship). Determine whether the rule produces determinate answers or whether authorization/unauthorized status correlates with doctrinal conservatism vs progressivism.',
    'If rule is coherent and produces determinate answers independent of doctrinal content: the reading is internally consistent (remains tangled_rope/rope). If rule is indeterminate or correlates with conservative preference: the reading is being used selectively to constrain progressive implementation while permitting conservative elaboration (reclassifies toward snare — the constraint serves conservative beneficiaries, not genuine magisterial order). If the rule is simply ''magisterium decides at each moment'': the constraint is pure discretion (reclassifies toward rope or institutional extraction, not continuity-based coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_of_council_authorization_boundary, empirical, 'Determinacy and consistency of the continuity reading''s rule for authorized vs unauthorized council interpretation').

omega_variable(
    latin_preservation_mandate_enforcement_selectivity,
    'Sacrosanctum Concilium §36 mandates preservation of Latin in the liturgy. Does the continuity reading enforce this mandate equally across all post-conciliar liturgical change, or selectively to protect particular doctrinal elements (e.g., Latin preservation for eucharistic theology but not for other sacraments)?',
    'Catalog post-conciliar Latin preservation efforts and Latin abandonment efforts across the Church. Determine whether Vatican enforcement of SC §36 correlates with the mandate''s literal text or with doctrinal priorities (Latin preserved where pre-conciliar theology is at stake, abandoned where progressive pastoral priorities dominate). Compare Vatican statements on Latin with statements on pastoral adaptation in the same period.',
    'If enforcement is uniform and textually consistent: continuity reading is coherent (remains tangled_rope). If enforcement is selective and correlates with doctrinal conservatism: the reading is post-hoc rationalization used to constrain implementation benefiting progressives while permitting implementation benefiting conservatives (snare classification — high suppression of alternative interpretations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latin_preservation_mandate_enforcement_selectivity, empirical, 'Whether SC §36 Latin mandate is enforced uniformly or selectively by continuity reading advocates').

omega_variable(
    kernel_reading_underdetermination,
    'Is the continuity reading determined by the Vatican II texts themselves, or is it one interpretive choice among several coherent hermeneutics equally grounded in the conciliar documents?',
    'Textual analysis: identify passages in council documents that support continuity framing AND passages that support rupture framing. Determine whether the continuity reading requires additional premises beyond the texts (e.g., hermeneutic assumptions about how tradition works, assumptions about the magisterium''s authority to bind implementation) to reach its conclusions. Compare with rupture_reading''s supporting textual claims.',
    'If continuity is determined by the texts: the reading is structural (remains tangled_rope). If continuity requires additional hermeneutic premises not stated in the texts: the reading is committer-dependent, making it a particular choice rather than inevitable reading (reclassifies toward snare — the constraint''s authority derives from institutional power, not textual content). This resolves the fundamental ambiguity of the kernel: whether Vatican II texts are univocal (continuity determined by text) or intentionally ambiguous (readings are committer choices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether continuity reading is textually determined or committer-dependent hermeneutic choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 10, 0.63).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II constraint family: Three structurally distinct readings of the contested kernel vatican_ii_magisterial_authority. Each reading produces a different constraint with different ε values, beneficiary/victim structures, and perspectival gaps. The continuity_reading (ε=0.58) constrains progressive implementation through hermeneutic limitation. The rupture_reading (estimated ε=0.62) authorizes progressive implementation by declaring prior teaching superseded. The composite_overdetermination_reading (estimated ε=0.42) dissolves the authority question by treating the kernel as irreducibly ambiguous. Network links enable analysis of how each reading influences the others: continuity forecloses composite (cannot have both determinate continuity AND overdetermined ambiguity), coexists with rupture (different parties hold both readings simultaneously), influences rupture (continuity's constraint-via-hermeneutics creates pressure for rupture advocates to reject hermeneutic limits entirely).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
