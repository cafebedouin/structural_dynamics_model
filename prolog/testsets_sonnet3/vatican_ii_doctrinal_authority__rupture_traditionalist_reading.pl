% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority — Rupture (Traditionalist) Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story authors the traditionalist rupture reading of the Vatican II
 *   kernel: the Council's documents, reached through compromise among
 *   conflicting theological factions, contain genuine ambiguities and
 *   formulations that departed from prior magisterial clarity, and these
 *   ambiguities were subsequently exploited by an administering hierarchy and
 *   theological establishment to authorize doctrinal and liturgical changes
 *   the texts do not themselves warrant. This is a distinct constraint from
 *   the progressive rupture reading (which shares the rupture premise but
 *   values it positively as liberation from rigidity) and from the continuity
 *   reading (which denies rupture occurred at all). The extraction (ε≈0.72)
 *   tracks the traditionalist reading's own assessment of the cost imposed on
 *   traditional liturgical communities, missionary orders, and catechetical
 *   clarity — it is not a neutral or averaged figure across readings.
 *
 * KEY AGENTS:
 *   - post_conciliar_episcopal_bureaucracy: administers the ambiguity, institutional/arbitrage — primary agenda_setter and structural beneficiary
 *   - progressive_theological_faculties: benefits from expansive interpretive license, organized/mobile
 *   - traditional_latin_mass_communities: bears the liturgical-suppression cost, powerless/trapped — primary victim
 *   - missionary_orders_committed_to_conversion: bears loss of theological warrant and institutional support, moderate/constrained
 *   - traditionalist_societies_and_associations: excluded from the interpretive process that decides how the ambiguity resolves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.72).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority — Rupture (Traditionalist) Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '645d616a-9589-4c9b-8694-5aa6c471558f').
narrative_ontology:cs_kernel_codification('645d616a-9589-4c9b-8694-5aa6c471558f', fixed_text).
narrative_ontology:cs_authority_grounding('645d616a-9589-4c9b-8694-5aa6c471558f', lineage).
narrative_ontology:cs_interpretation_layer_present('645d616a-9589-4c9b-8694-5aa6c471558f').
narrative_ontology:cs_reading_relation('645d616a-9589-4c9b-8694-5aa6c471558f', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('645d616a-9589-4c9b-8694-5aa6c471558f', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('645d616a-9589-4c9b-8694-5aa6c471558f', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('645d616a-9589-4c9b-8694-5aa6c471558f', foundational, conciliar_ambiguity_constitutes_doctrinal_defect).
narrative_ontology:cs_axiom_status(conciliar_ambiguity_constitutes_doctrinal_defect, holdable).
narrative_ontology:cs_axiom_grounding('645d616a-9589-4c9b-8694-5aa6c471558f', conciliar_ambiguity_constitutes_doctrinal_defect, deontological).
narrative_ontology:cs_axiom('645d616a-9589-4c9b-8694-5aa6c471558f', foundational, prior_codified_magisterium_binds_conciliar_interpretation).
narrative_ontology:cs_axiom_status(prior_codified_magisterium_binds_conciliar_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('645d616a-9589-4c9b-8694-5aa6c471558f', prior_codified_magisterium_binds_conciliar_interpretation, conventional).
narrative_ontology:cs_reference_frame('645d616a-9589-4c9b-8694-5aa6c471558f', pre_conciliar_codified_magisterium).
narrative_ontology:cs_drift_state('645d616a-9589-4c9b-8694-5aa6c471558f', post_conciliar_implementation_period, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('645d616a-9589-4c9b-8694-5aa6c471558f', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopal_bureaucracy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theological_faculties).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_dialogue_institutions).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catechetical_clarity_of_the_faithful).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders_committed_to_conversion).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, clergy_formed_in_pre_conciliar_discipline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers liturgical reform, catechetical revision, and diocesan restructuring under the Council's authority, appealing to conciliar texts and 'the spirit of the Council' to justify discretionary departures from prior discipline. Controls seminary formation, imprimatur, and canonical sanction, and is largely insulated from consequences when implementation produces confusion or defection.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopal_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopal_bureaucracy, beneficiary).

% Gained institutional standing, publishing platforms, and curricular dominance by reading conciliar ambiguity expansively — treating unresolved textual compromises as license for doctrinal development the documents themselves do not state. Their academic and ecclesiastical careers are built on this reading holding.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theological_faculties, beneficiary,
    organized, generational, mobile, global).

% Secretariats and dialogue commissions created post-Council draw funding, staff positions, and diplomatic standing from the ecumenical opening. Their continued existence depends on treating the conciliar rupture as settled rather than as an error to be walked back.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_dialogue_institutions, beneficiary,
    organized, generational, mobile, global).

% Lost licit, unimpeded access to the pre-conciliar liturgical patrimony; faced decades of restriction, suppression, and canonical marginalization for continuing prior liturgical practice. Their exit options are schism, quiet noncompliance, or dependence on episcopal indult — all costly and precarious. They bear the doctrinal-clarity cost the reading identifies as the core casualty.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_communities, payer,
    powerless, generational, trapped, global).

% Not an actor but the casualty named by the reading itself: a generation catechized under ambiguous post-conciliar formation experienced doctrinal confusion the pre-conciliar magisterium's clearer formulations would have foreclosed. Included for completeness as the diffuse harm the reading points to, not as an agent capable of exit.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catechetical_clarity_of_the_faithful, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catechetical_clarity_of_the_faithful).

% Orders whose charism was explicit conversion of non-Christians found their theological warrant undercut by conciliar statements on non-Christian religions read expansively; funding, vocations, and institutional support shifted toward dialogue-oriented apostolates. Cannot simply relaunch the pre-conciliar missionary framework without institutional backing they no longer command.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders_committed_to_conversion, payer,
    moderate, generational, constrained, global).

% Ordained under the older discipline, many found their formation and liturgical competence suddenly treated as obsolete or suspect; some were pressured out, others adapted under duress. Their exit options were laicization, quiet accommodation, or affiliation with traditionalist societies of uncertain canonical standing.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, clergy_formed_in_pre_conciliar_discipline, payer,
    moderate, biographical, constrained, national).

% Societies formed to preserve pre-conciliar liturgy and doctrine argue the rupture reading from outside full ecclesiastical communion or under canonical irregularity, and are frequently excluded from official synodal and consultative processes despite representing the constituency the reading centers.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_societies_and_associations, excluded,
    powerless, generational, trapped, global).

% Some bishops sympathetic to traditionalist concerns grant liturgical indults or protective accommodations while remaining within the post-conciliar structure, testifying that the tension the reading identifies is real without themselves adopting a rupture framework in official capacity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, sympathetic_diocesan_bishops, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopal_bureaucracy).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council did solve real coordination problems — updating liturgical language for pastoral accessibility, clarifying the Church's stance toward modern states and other religions, and organizing episcopal collegiality — but, on this reading, the textual compromises reached to secure near-unanimous conciliar votes introduced ambiguity that was then read by the administering hierarchy as license for changes the texts do not actually mandate.
% TRANSFER_FUNCTION: Moves doctrinal and liturgical authority away from a stable, previously-codified magisterial deposit and toward the discretionary interpretive judgment of post-conciliar bureaucratic and academic actors; moves institutional resources (seminary formation, funding, career advancement) toward progressive and ecumenical apparatus and away from traditional liturgical and missionary communities.
% ABSENT_VOICES: Traditionalist theologians, superiors of pre-conciliar religious orders, and laity attached to the older liturgy were present at the Council only as a minority voting bloc and have had little subsequent access to the interpretive and disciplinary apparatus that decides how conciliar ambiguity is resolved in practice; their objections are treated as marginal rather than as evidence of textual defect.
% DISAPPEARANCE_RATIONALE: If the post-conciliar interpretive apparatus lost its authority to read ambiguity expansively — for instance through an authoritative traditionalist-aligned re-promulgation clarifying the disputed texts in continuity with prior magisterium — liturgical practice, seminary formation, and ecumenical structures built on the expansive reading would have to be substantially rebuilt or abandoned; this is not a constraint whose disappearance would leave the institutional landscape unchanged.
% FOUNDING_PROBLEM: The Council was convened to address a perceived pastoral disconnect between the Church and the modern world — declining practice, calls for liturgical intelligibility, and a need to define the Church's posture toward other Christians, other religions, and modern states.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist clergy and lay commentators (including some cardinals and bishops who participated in the Council itself, e.g. minority-bloc figures who later published critical assessments) attest that the pastoral problem was real but that the textual compromises produced defects rather than solutions. This corroboration comes substantially from within the Church's own hierarchy and participant record, not solely from the constraint's declared victims, though it remains an intra-ecclesial minority position rather than a magisterially settled verdict.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from a moderate 0.35 at the Council's close (1965) to 0.72 by 2025, tracking the traditionalist account of a widening gap between the pre-conciliar doctrinal-liturgical deposit and its post-conciliar administration — with a partial retreat around 2007 (Summorum Pontificum liberalizing the older liturgy) before extraction resumes climbing after 2021 restrictions (Traditionis Custodes) reversed that liberalization. Suppression follows the same arc: rising through the immediate post-conciliar decades as the older liturgy was restricted, easing somewhat 2007, then intensifying again. Theater ratio is moderate-high (0.45) because much post-conciliar reform, on this reading, substitutes symbolic gestures of renewal (committees, dialogues, restructured curricula) for the substantive doctrinal clarity the traditionalist account holds was lost.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar episcopal bureaucracy and progressive theological faculties sit near the beneficiary end: they administer or exploit the ambiguity and are largely insulated from its costs by institutional position and mobility. Traditional liturgical communities and missionary orders sit near the full-target end: trapped or constrained exit, bearing the doctrinal and liturgical cost directly, without the institutional standing to contest the interpretation that disadvantages them. Clergy formed under the older discipline occupy an intermediate position — moderate power but constrained exit, since laicization or affiliation with irregular societies are both costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) preserves the reading's own claim that the Council solved genuine pastoral problems — liturgical accessibility, ecumenical clarity, collegial governance — while insisting that the same textual apparatus was subsequently used to extract far more than those problems required, at the direct expense of parties who had no voice in how the ambiguity would be resolved. Classifying this as pure snare would deny the reading's own acknowledgment of a real founding problem; classifying it as rope would erase the victim set the reading is built to name. The founding_problem mismatch check applies here: status is authored as contested rather than dead, because the traditionalist reading holds the pastoral problem was real but the fix was defective — not that the problem never existed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_defect_vs_interpretive_abuse,
    'Are the doctrinal and liturgical changes attributed to Vatican II genuinely licensed by ambiguity/error in the conciliar texts themselves, or are they extra-textual developments wrongly attributed to the Council by an administering hierarchy pursuing its own agenda?',
    'Close textual-critical comparison of conciliar drafts, floor debates, and final promulgated text against subsequent implementing documents, cross-checked against the stated intent of the drafting commissions (e.g. the Sacred Congregation for Rites) and against magisterial statements from popes who sat through the Council.',
    'If the changes are genuinely textually licensed, the rupture is located in the Council itself and this reading is well-founded; if the changes exceed what the text supports, the true fault lies in post-conciliar administration rather than the conciliar documents, which would shift beneficiary/victim attribution toward the administering bureaucracy alone rather than the Council''s authority as such.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_defect_vs_interpretive_abuse, conceptual, 'Whether post-conciliar change is textually licensed by conciliar ambiguity or is an extra-textual imposition.').

omega_variable(
    committer_reading_selection,
    'Given that continuity, progressive-rupture, traditionalist-rupture, and composite-overdetermination readings are all held by identifiable factions within the Church, is there a fact of the matter about which reading is correct, or is the kernel genuinely underdetermined by the text plus available evidence?',
    'This is not straightforwardly empirically resolvable — it depends partly on contested hermeneutical principles (the ''hermeneutic of continuity'' vs. ''hermeneutic of rupture'' debate explicitly named by Benedict XVI in 2005) and partly on ecclesiological premises about how magisterial authority operates over time, which are themselves internal to the traditions in contest.',
    'If a hermeneutic principle could be authoritatively and non-question-beggingly established, one reading would displace the others as the operative constraint; absent that, all four readings persist as parallel, non-adjudicated constraints held by different constituencies, which is the current state modeled by keeping these as separate linked stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_selection, conceptual, 'Whether the Vatican II kernel has a determinable correct reading or is genuinely underdetermined among the four declared readings.').

omega_variable(
    suppression_mechanism_liturgical,
    'Is the suppression experienced by traditional liturgical communities primarily structural (canonical restriction, episcopal denial of permission) or partly internalized (communities that have absorbed a self-understanding as marginal or tolerated-at-best, persisting even where local permission exists)?',
    'Comparative study of traditionalist community behavior in dioceses with liberal indult policies (post-2007) versus restrictive ones (post-2021): if community caution and self-limitation persist even under liberal local policy, that indicates an internalized component.',
    'If substantially internalized, the effective suppression these communities carry is higher than the structural/canonical measure alone suggests, and would not fully reverse even if canonical restrictions were lifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_liturgical, empirical, 'Structural vs. internalized suppression among traditional liturgical communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority_continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority_rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority_composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the vatican_ii_doctrinal_authority kernel, each authored as a separate ε-invariant constraint per the decomposition principle: continuity_reading (denies rupture; low ε), rupture_progressive_reading (affirms rupture, values it positively; high ε but with a different beneficiary/victim polarity from this story), rupture_traditionalist_reading (this story: affirms rupture, values it negatively; high ε with traditional liturgical/missionary/catechetical communities as victims), and composite_overdetermination_reading (denies the single-rupture-or-continuity framing itself, treating the Council as several distinct bundled structural shifts). All four share the same underlying kernel — the conciliar texts and their authoritative status — but diverge on whether rupture occurred, whether it was warranted, and who bears the cost, and are therefore modeled as four distinct constraints rather than one constraint with a hidden observer parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
