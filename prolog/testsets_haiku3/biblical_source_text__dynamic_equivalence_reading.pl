% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Translation Authority (Communicative Effectiveness Primary)
 *   domain: religious/linguistic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'biblical
 *   source text': the dynamic equivalence reading prioritizes communicative
 *   effectiveness in the target language over morphological fidelity to the
 *   source. The sibling readings—formal equivalence (structure preservation)
 *   and critical reconstruction (textual recovery)—are separate constraint
 *   stories linked via network.affects_constraints. This reading instantiates
 *   a specific authority structure: the evangelical publishing apparatus and
 *   missionary institutions enforce dynamic equivalence as the standard
 *   translation paradigm, marginalizing formal equivalence and critical
 *   reconstruction in popular pastoral contexts. The constraint's operation
 *   vindicates a normative proposition: that accessibility and pastoral
 *   mission are more important than scholarly precision. Lay readers and
 *   missionary contexts benefit; scholars and word-study practitioners bear
 *   the precision loss. The claim/metric gap is deliberate: the reading is
 *   CLAIMED as rope (genuine coordination solving the mass-literacy problem)
 *   while authored metrics describe moderate extractiveness and rising
 *   theater (the claim to accessibility increasingly serving institutional
 *   publishing dominance and paradigm control). The engine measures this
 *   divergence.
 *
 * KEY AGENTS:
 *   - lay_christian_readers: Primary beneficiaries (gain comprehension without language training)
 *   - missionary_contexts: Secondary beneficiaries (tool for pastoral effectiveness)
 *   - evangelical_publishing_apparatus: Agenda-setter (enforces paradigm through market and institutional authority)
 *   - scholarly_word_study_practitioners: Primary payers (lose morphological precision and institutional legitimacy)
 *   - formal_equivalence_tradition: Excluded (pushed to academic/liturgical niches)
 *   - critical_reconstructive_tradition: Excluded (marginalized from evangelical standard-setting)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.31).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Translation Authority (Communicative Effectiveness Primary)").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '10af06c9-9e53-4401-95f6-a0f700a401d0').
narrative_ontology:cs_kernel_codification('10af06c9-9e53-4401-95f6-a0f700a401d0', fixed_text).
narrative_ontology:cs_authority_grounding('10af06c9-9e53-4401-95f6-a0f700a401d0', lineage).
narrative_ontology:cs_interpretation_layer_present('10af06c9-9e53-4401-95f6-a0f700a401d0').
narrative_ontology:cs_reading_relation('10af06c9-9e53-4401-95f6-a0f700a401d0', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('10af06c9-9e53-4401-95f6-a0f700a401d0', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('10af06c9-9e53-4401-95f6-a0f700a401d0', foundational, communicative_transparency_primary).
narrative_ontology:cs_axiom_status(communicative_transparency_primary, holdable).
narrative_ontology:cs_axiom_grounding('10af06c9-9e53-4401-95f6-a0f700a401d0', communicative_transparency_primary, instrumental).
narrative_ontology:cs_axiom('10af06c9-9e53-4401-95f6-a0f700a401d0', foundational, pastoral_accessibility_sufficient_justification).
narrative_ontology:cs_axiom_status(pastoral_accessibility_sufficient_justification, holdable).
narrative_ontology:cs_axiom_grounding('10af06c9-9e53-4401-95f6-a0f700a401d0', pastoral_accessibility_sufficient_justification, deontological).
narrative_ontology:cs_reference_frame('10af06c9-9e53-4401-95f6-a0f700a401d0', accessible_scripture_for_mass_readership).
narrative_ontology:cs_drift_state('10af06c9-9e53-4401-95f6-a0f700a401d0', contemporary_academic_publishing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10af06c9-9e53-4401-95f6-a0f700a401d0', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_christian_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_contexts).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, evangelical_publishing_apparatus).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, scholarly_word_study_practitioners).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, philological_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive Bible translations that prioritize meaning over word-for-word structure, enabling comprehension of narrative, theology, and practical guidance without linguistic expertise. Experience the text as accessible, emotionally resonant, and immediately applicable to daily life. Can switch between translation versions or choose a different religious tradition, though such exit carries relational and identity costs within their faith community.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_christian_readers, beneficiary,
    powerless, biographical, mobile, global).

% Deploy dynamic equivalence translations as primary teaching and conversion tools in contexts where target-language readers lack classical language training. The translation's fidelity to communicative intent (rather than source structure) makes sermons, study guides, and conversion narratives coherent in the receiving language and culture. Efficiency of pastoral mission depends on this translation choice. Constrained to the extent that formal equivalence alternatives are seen as requiring supplementary linguistic training they cannot always provide.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_contexts, beneficiary,
    moderate, biographical, constrained, global).

% Publishers and translation committees enforce dynamic equivalence as the dominant translation paradigm through market dominance, church adoption incentives, and doctrinal framing of 'accessibility' as a moral good. Control translation standard-setting, copyright, distribution infrastructure, and pastoral legitimacy narratives. Benefit from translation market growth and institutional authority consolidation. Can arbitrage between missionary field demands and lay reader preferences.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, evangelical_publishing_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Academics, exegetes, and serious students who require precise morphological tracking and source-language structural preservation to conduct rigorous textual analysis. Experience dynamic equivalence translations as information-lossy: idioms smoothed, syntax normalized, grammatical mood/aspect reduced. Must either invest in learning source languages or accept diminished analytical capacity. Their exit option is strong (access to formal equivalence, critical editions, and original-language tools) but adoption of the dynamic paradigm in their institutional and publishing contexts has made those resources harder to find or teach.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, scholarly_word_study_practitioners, payer,
    powerful, generational, arbitrage, global).

% Linguists and textual historians studying ancient Greek/Hebrew via biblical text as primary corpus. Require morphological fidelity and structural preservation for diachronic and comparative linguistic work. Dynamic equivalence translations are unsuitable for their research; they depend on access to critical editions and formal translations. Constrained exit: institutional authority (academic publishing, theology curriculum) increasingly frames dynamic equivalence as the 'standard' translation, pushing formal equivalence and critical editions to specialized academic markets.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, philological_researchers, payer,
    moderate, generational, constrained, regional).

% Institutional and intellectual tradition committed to source-language structural preservation. Would advocate for different translation principles if present in translation standard-setting, but is marginalized in evangelical publishing, pastoral training, and popular Bible market. Trapped in academic and liturgical niches while dynamic equivalence dominates popular distribution and institutional adoption.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_translation_tradition, excluded,
    powerful, generational, trapped, global).

% Scholarly and historical-critical approach to biblical text that prioritizes textual recovery and genealogical reconstruction. Rejects both dynamic and formal equivalence as premature commitment to a fixed text before origins are settled. This reading is structurally excluded from evangelical translation standard-setting and pastoral interpretation contexts, though present in academic and mainline Protestant institutional spaces.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, critical_reconstructive_tradition, excluded,
    powerful, generational, trapped, global).

% Official and semi-official bodies (United Bible Societies, evangelical denominations, academic translation projects) that set translation standards and adjudicate reading authenticity. Internally contested: some committees enforce dynamic equivalence through institutional authority; others maintain formal equivalence or hybrid approaches. Their role is simultaneously agenda-setter (enforcing the reading) and observer (witnessing the contest between paradigms).
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_standard_committees, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, translation_standard_committees, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, evangelical_publishing_apparatus).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified linguistic bridge from ancient source texts to contemporary readers in their native language, enabling theological comprehension and pastoral application without requiring source-language training. Solves the coordination problem of connecting distant historical texts to living faith communities at scale.
% TRANSFER_FUNCTION: Moves precision and morphological fidelity from scholarly and word-study contexts into the lay pastoral economy. Lay readers gain comprehension; scholars lose granular textual data and must invest in source-language training or accept reduced analytical capacity.
% ABSENT_VOICES: Critical-reconstructive scholars who would object that both dynamic and formal equivalence presume a stable source text before historical recovery is complete. Formal equivalence tradition advocates who would argue for structure-preserving fidelity. These traditions are excluded from evangelical pastoral translation standard-setting and framed as inappropriate or overly academic for the lay reading context.
% DISAPPEARANCE_RATIONALE: If dynamic equivalence paradigm disappeared, evangelical publishing would reorganize around formal equivalence or hybrid models; lay Bible study would require either linguistic training or parallel-text methods (formal plus commentary); missionary contextualization strategies would shift to emphasize teaching-alongside-translation rather than translation-as-primary-access; scholarly word study would reclaim legitimacy in pastoral training; academic biblical studies would no longer need to maintain separate 'scholarly' and 'pastoral' translation tracks.
% FOUNDING_PROBLEM: Early mass Bible translations (King James Era onward) faced a choice between readability and precision; as evangelical mission expanded globally in the 20th century, a growing lay readership without classical language training needed intelligible Scripture access. Dynamic equivalence (Nida-Taber functional equivalence theory) emerged as a solution: prioritize meaning-transfer over morphological fidelity to enable broader pastoral effectiveness.
% FOUNDING_PROBLEM_CORROBORATION: Evangelical and missionary institutions attest the founding problem remains live and dynamic equivalence is necessary for pastoral reach. Biblical scholarship and formal-equivalence tradition attest the founding problem is overstated and conflates different reading competencies (comprehension of narrative vs. rigorous exegesis). Publishing market data from the past 50 years, from outside the benefiting parties, shows dynamic equivalence achieving market dominance and formal equivalence receding in popular contexts while holding ground in academic and liturgical spaces — a signal of paradigm shift rather than pure problem-solving.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.38 → 0.58) as dynamic equivalence consolidates market dominance and formal equivalence recedes in popular Bible publishing. The initial lower extractiveness reflects a genuine coordination function: lay readers really do need accessible Scripture. But as the paradigm institutionalizes, extractiveness rises because the constraint begins to serve institutional agenda-setting more than genuine comprehension need — formal equivalence and critical apparatus are pushed out of the pastoral market, not because they serve worse theological functions but because they require training investment the publishing apparatus doesn't want to fund. Theater rises modestly (0.08 → 0.22) as the institutional justification accumulates: early dynamic equivalence was transparent about the trade-off (meaning for structure); later marketing emphasizes 'accuracy' and 'reliability' while eliding the precision loss. Suppression is low (0.31) because the enforcement is institutional and market-based (dominant publishing position, pastoral training curriculum dominance) rather than explicit prohibition. Formal equivalence and critical reconstruction are not banned—they persist in academic and liturgical niches—but are systematically excluded from popular markets and pastoral training, which is a subtle form of suppression: alternatives are not forbidden but are made illegitimate and inaccessible.
 *
 * PERSPECTIVAL GAP:
 *   Lay readers and missionaries see the arrangement as liberating (access without training). Scholarly practitioners see it as domesticating (precision loss, institutional marginalization). The evangelical publishing apparatus sees it as coordination. The engine computes per-seat types from the structural data: from the lay reader seat, this looks like genuine rope (they benefit from coordination); from the scholar seat, it looks more like tangled rope or snare (they lose something real and the loss is defended by institutional authority). The agenda-setter seat sees pure coordination they built and maintain. The formal equivalence tradition and critical scholars see extraction of precision and institutional authority disguised as accessibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay readers (powerless, mobile, biographical horizon): beneficiaries with genuine comprehension gain; their exit options are relatively free (choose a different translation, leave the faith tradition), so directionality d is low (near beneficiary end). Missionaries (moderate power, constrained exit): beneficiaries via pastoral efficiency but increasingly constrained to dynamic equivalence through institutional training and publishing availability; d is moderate-low. Evangelical publishers (institutional power, arbitrage exit): agenda-setter, collects institutional authority and market dominance; d is near beneficiary end (they control the constraint). Scholars and word-study practitioners (powerful/moderate power, arbitrage exit): targets for precision loss; d approaches target end (they lose something, the arrangement is defended against their exit). The formal equivalence tradition (powerful, trapped in academic niche): excluded from enforcement but excluded from benefit too; directionality is ambiguous (neither actively benefiting nor explicitly targeted, but systematically marginalized). Critical scholars (powerful, trapped): same structural position as formal equivalence tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   The dynamic equivalence reading instantiates a genuine coordination function (mass literacy + theological access) but has acquired extractive institutional components (publishing dominance, paradigm enforcement, alternative marginalization). The measurement series shows extractiveness rising as the paradigm consolidates, which is consistent with an initially-valid coordination mechanism becoming a vehicle for institutional authority consolidation. The founding problem (mass lay readership needs accessible Scripture) is CONTESTED: lay readers say it's still live, scholars say it's largely solved and the arrangement now serves publishing agenda. The formal equivalence tradition and critical reconstruction would say the founding problem was never actually 'need for accessibility' but rather 'how to balance multiple legitimate reading competencies' — a framing that the dynamic paradigm suppresses. The theater_ratio rise (0.08 → 0.22) flags Goodhart drift: institutional messaging increasingly claims 'accuracy' and 'scholarly reliability' for dynamic equivalence, which obscures the transparency of the original trade-off (readability for structure). This is consistent with mandatrophy: the founding problem has become partially inert (lay readership is literate enough that some precision could be recovered without losing comprehension), but the institutional apparatus continues to enforce the dynamic paradigm through curriculum and publishing because it serves institutional continuity, not because the founding problem justifies it. The constraint is not yet a piton—it serves real functions—but shows symptoms of mandatrophy onset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accessibility_versus_precision_trade_off,
    'Is the precision loss inherent to dynamic equivalence, or does it reflect engineering choices in specific translation projects rather than a structural necessity?',
    'Comparative analysis of dynamic equivalence translations from different projects: if precision loss is consistent across all projects, it''s structural; if some projects recover precision without sacrificing comprehension, the trade-off is engineered rather than necessary.',
    'If engineered, the constraint becomes more extractive (institutional choice to subordinate precision to other agenda); if structural, extractiveness is justified as an inherent cost of mass literacy. The classification shifts from tangled_rope toward snare if precision loss is shown to be avoidable but prioritized by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accessibility_versus_precision_trade_off, empirical, 'Whether precision loss is structural or engineered in dynamic equivalence.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (mass lay readership needs accessible Scripture) still the primary driver of dynamic equivalence institutional dominance, or has the arrangement become an end in itself (institutional authority, publishing market control, curriculum lock-in)?',
    'Historical tracking of translation project goals and justifications; market analysis of whether formal equivalence and accessibility are actually incompatible; survey of pastoral and lay reader preferences for translation paradigm.',
    'If the founding problem is no longer primary, the constraint exhibits mandatrophy: the function it was built to serve has become inert, but institutional apparatus persists. Classification would shift toward piton if theater_ratio rises further while extractiveness plateaus. If the founding problem remains live, extractiveness is justified and the constraint remains rope or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether institutional persistence serves founding problem or institutional continuity.').

omega_variable(
    reading_foreclosure_versus_coexistence,
    'Do the dynamic equivalence and formal equivalence readings logically foreclose each other (mutually exclusive core premises), or do they coexist as different-but-compatible approaches for different reading competencies?',
    'Philosophical analysis of the core axioms: can a single authority structure hold both readings simultaneously (they serve different functions), or does commitment to one require rejecting the other (they make incompatible claims about what translation fidelity means)?',
    'If they foreclose each other, the reading_relations will declare forecloses (rare); if they coexist, coexists_with is correct. The distinction affects how the engine models the constraint family''s structural stability: foreclosure suggests institutional instability (one reading will eventually dominate completely); coexistence suggests durable division of labor (but with power asymmetry favoring dynamic equivalence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_versus_coexistence, conceptual, 'Logical structure of the reading contest: exclusive or complementary.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of formal equivalence and critical reconstruction structural (institutional barriers, market dominance, curriculum control) or internalized (evangelical readers and institutions believe dynamic equivalence is ''more accurate'' despite contrary evidence)?',
    'Exit tracking: when scholars or readers encounter formal equivalence or critical reconstruction, do they reject them due to institutional unavailability (structural) or due to belief that the alternatives are inferior (internalized)? De-institutionalization experiment: if formal equivalence were given equal market access and curriculum legitimacy, would lay readers and missionaries maintain preference for dynamic equivalence?',
    'If internalized, the suppression is more effective and durable (targets carry it with them after institutional barriers fall); if structural, opening market and curriculum access would quickly restore formal equivalence competition. Affects stability of the constraint and the classification of whether victims are being actively coerced or have internalized the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of alternative readings is structural or internalized.').

omega_variable(
    kernel_reading_contest_structure,
    'Is the dynamic equivalence reading a deliberate choice among available alternatives (the reading chosen because it serves pastoral and institutional needs), or is it a capture of the kernel by institutional actors (the reading enforced to exclude challenge)?',
    'Historical tracking of reading adoption: was dynamic equivalence chosen after deliberation with formal equivalence and critical reconstruction present, or was it imposed by dominant institutions before alternatives were seriously considered? Institutional access analysis: do formal equivalence and critical scholars have meaningful voice in translation standard-setting, or are they structurally excluded?',
    'If chosen, the constraint serves genuine coordination and the extractive components are side-effects; if captured, the constraint is more snare-like (extraction disguised as coordination). The classification shifts based on whether the reading contest is open or closed to alternative voices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, empirical, 'Whether the dynamic equivalence reading is chosen or captured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bibl_tr_t8, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(bibl_tr_t16, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(bibl_tr_t25, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement(bibl_tr_t35, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bibl_be_t8, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(bibl_be_t16, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(bibl_be_t25, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(bibl_be_t35, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(bibl_su_t8, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(bibl_su_t16, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(bibl_su_t25, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(bibl_su_t35, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 35, 0.3).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(biblical_source_text__dynamic_equivalence_reading, 0.12).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the contested kernel 'biblical source text' along the axis of translation philosophy: dynamic equivalence (THIS story) prioritizes communicative meaning; formal equivalence prioritizes source structure; critical reconstruction prioritizes historical recovery. The three readings coexist in different institutional contexts and each carries its own ε (extractiveness), beneficiary/victim structure, and classification. They form a constraint family linked by network.affects_constraints. The upstream story (critical_reconstructive_reading) influences both others because historical-critical authority is invoked by both supporters and critics of the other readings. The dynamic equivalence reading (THIS story) influences formal equivalence by establishing the market and institutional default that formal equivalence must compete against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
