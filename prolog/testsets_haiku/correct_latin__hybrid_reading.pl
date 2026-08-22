% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin: Classical Form Transmitted Through Medieval Practice But Correctable Via Textual Evidence
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   The constraint embodies a Renaissance humanist reading of 'correct Latin'
 *   as Classical form transmitted through medieval practice but correctable
 *   via textual evidence. This is one of three contested readings of the
 *   'correct_latin' kernel. The constraint's operation involves: (1)
 *   institutional authority shifted to humanist textual scholars who
 *   adjudicate correctness by collating ancient manuscripts; (2) medieval
 *   scribal and monastic traditions rendered 'partly legitimate' but subject
 *   to correction; (3) educational reform that prioritizes Classical texts
 *   over medieval usage. The hybrid reading positions itself as a compromise
 *   between pure continuity (medieval Latin is fully legitimate as evolved
 *   Classical) and pure discontinuity (medieval Latin is entirely corrupt,
 *   requiring reconstruction from ancient sources). But the compromise
 *   produces asymmetric extraction: medieval practitioners lose authority and
 *   confidence, while humanist reformers gain institutional legitimacy and
 *   educational control. The measurement series documents the gradual
 *   increase in extractiveness and suppression requirement as the
 *   constraint's enforcement strengthens, while theater ratio (performative
 *   maintenance) remains moderate — the constraint has a genuine coordination
 *   function (shared standard for fragmented medieval practice) but carries
 *   substantial extraction as the price of enforcing that standard.
 *
 * KEY AGENTS:
 *   - humanist_philologists: institutional agenda-setters (powerful, mobile exit) — establish the textual-evidence standard and control what counts as correct Latin
 *   - medieval_practice_preservers: payers (moderate power, identity-locked exit) — their fluent medieval Latin is rendered 'partly legitimate' but subject to correction; identity fusion with scribal tradition makes exit psychologically costly
 *   - monastic_scribal_traditions: organized payers/beneficiaries (organized power, constrained exit) — lose authority over Latin standards but remain responsible for maintaining the ancient manuscripts that provide corrective evidence
 *   - educational_reformers: beneficiaries (powerful, mobile exit) — gain institutional legitimacy and curricular authority from the hybrid reading's framework
 *   - continuity_reading_advocates: excluded (moderate power, constrained exit) — their position (medieval Latin is legitimate evolution) is systematically devalued by the framework
 *   - discontinuity_reading_advocates: excluded but partially aligned (powerful, mobile exit) — their textual-purity position finds some institutional voice but is not hegemonic under the hybrid compromise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.38).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin: Classical Form Transmitted Through Medieval Practice But Correctable Via Textual Evidence").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, '2c4d6103-0d90-44f2-bc3d-66b4cf9431ba').
narrative_ontology:cs_kernel_codification('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', fixed_text).
narrative_ontology:cs_authority_grounding('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', lineage).
narrative_ontology:cs_interpretation_layer_present('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba').
narrative_ontology:cs_reading_relation('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', foundational, medieval_forms_partially_coherent).
narrative_ontology:cs_axiom_status(medieval_forms_partially_coherent, holdable).
narrative_ontology:cs_axiom_grounding('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', medieval_forms_partially_coherent, empirically_contingent).
narrative_ontology:cs_axiom('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', foundational, textual_evidence_authoritative_for_correction).
narrative_ontology:cs_axiom_status(textual_evidence_authoritative_for_correction, holdable).
narrative_ontology:cs_axiom_grounding('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', textual_evidence_authoritative_for_correction, deontological).
narrative_ontology:cs_reference_frame('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', classical_forms_preserved_in_ancient_texts).
narrative_ontology:cs_drift_state('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', contemporary_textual_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2c4d6103-0d90-44f2-bc3d-66b4cf9431ba', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, textual_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, educational_reformers).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medieval_practice_preservers).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, monastic_scribal_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, monastic_scribal_traditions).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, students_and_scribes).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, students_and_scribes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars trained in textual criticism who set the standard for what counts as 'correct' Latin by collating ancient manuscripts and identifying Classical orthography and vocabulary. They adjudicate disputes by appealing to the oldest reliable sources. Their authority rests on claimed mastery of textual evidence and philological method. They benefit from the constraint because it positions their expertise as essential to legitimate Latin use and education.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Monks, cathedral school masters, and scribes whose training embedded medieval Latin forms (Carolingian orthography, medieval vocabulary, evolved syntax). They read and write Latin fluently in the forms they learned, which are rooted in continuous transmission from the Classical period. The hybrid reading treats their forms as 'partly legitimate' but subject to correction, undermining their confidence in their own training and rendering their practice gradually obsolete.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_practice_preservers, payer,
    moderate, biographical, identity_locked, regional).

% Institutional practices of manuscript copying, annotation, and Latin composition embedded in monastic communities. The constraint's enforcement (textual correction, educational reform prioritizing Classical forms) devalues the scribal traditions' accumulated knowledge while simultaneously requiring them to maintain the very manuscripts (ancient texts) that provide the evidence for Classical correction.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, monastic_scribal_traditions, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, monastic_scribal_traditions, beneficiary).

% Renaissance pedagogues and university administrators who use the hybrid reading to justify curriculum change: Latin instruction now prioritizes Classical texts over medieval usage, making the reformers' new educational models and textbooks the legitimate path. They benefit from the authority this reading confers on their curriculum design.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, educational_reformers, beneficiary,
    powerful, generational, mobile, continental).

% The ancient authors (Cicero, Virgil, etc.) as textual authorities. The hybrid reading vindicates their forms as the standard against which all later Latin is judged. This is a vindicated proposition, not an actor, but included here as a structural pole of the constraint.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_manuscript_authors, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, classical_manuscript_authors).

% Scholars and practitioners who believe medieval Latin is legitimate evolved Classical Latin, not corrupt. They are excluded from adjudicating the standard because the hybrid reading's framework privileges textual evidence over living transmission. Their position is present in the historical record but systematically devalued by the institutional authority the hybrid reading grants to textual philologists.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, continuity_reading_advocates, excluded,
    moderate, biographical, constrained, regional).

% Textual purists who argue medieval Latin is entirely corrupt and only the most ancient, carefully reconstructed Classical forms count as correct. They are partially aligned with the humanist agenda-setters but remain excluded from direct authority because the hybrid reading grants legitimacy to medieval forms' 'grammatical core,' which purists reject. Their more radical position finds some voice in the broader movement but is not hegemonic.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, discontinuity_reading_advocates, excluded,
    powerful, biographical, mobile, continental).

% Young people learning Latin under reformed curricula that prioritize Classical forms. They benefit from access to ancient texts and the prestige of 'correct' Classical Latin; they also bear the cost of unlearning medieval forms and the insecurity that their own writing may be marked as 'incorrect' under the new standard.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, students_and_scribes, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, students_and_scribes, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, evidence-based standard for evaluating Latin correctness: what counts as legitimate Latin is no longer purely a matter of received local practice but is subject to arbitration by comparison with ancient textual sources. This solves the coordination problem of teaching Latin across regions without unified usage and enables scholarly communication across medieval communities with divergent orthographies and vocabulary.
% TRANSFER_FUNCTION: Transfers authority from medieval scribal and monastic traditions (who adjudicate correctness through practice) to humanist textual scholars (who adjudicate through manuscript collation). It also transfers educational prestige and institutional legitimacy toward Renaissance pedagogy and away from monastic Latin instruction. The material benefit accrues to reformers and new educational institutions; the cost is borne by practitioners of medieval forms, who lose authority and confidence in their training.
% ABSENT_VOICES: Medieval practitioners whose fluent, evolved Latin is being corrected are partially silenced by the framework — they are 'partly legitimate' but not authoritative. Continuity-reading advocates (who would argue medieval evolution is legitimate) are excluded from the standard-setting process because the hybrid reading privileges textual evidence over living transmission. Discontinuity advocates (who want fuller reconstruction) have partial voice in the movement but are not hegemonic.
% DISAPPEARANCE_RATIONALE: If the hybrid constraint vanished — if medieval forms remained fully authoritative and textual correction were not enforced — Latin pedagogy would remain embedded in monastic and cathedral schools, manuscript standards would be stable in regional traditions, and Renaissance humanist curriculum reform would lose its philological justification. The educational landscape, the institutional basis of authority over Latin, and the definition of scholarly legitimacy would reorganize.
% FOUNDING_PROBLEM: Fragmentation: medieval Latin diverged across regions into distinct orthographies, vocabularies, and grammatical patterns, making it difficult to establish a shared standard for teaching, scholarship, and textual comparison. Ambiguity: is evolved medieval Latin a legitimate continuation of Classical Latin, or a corruption that obscures the ancient forms?
% FOUNDING_PROBLEM_CORROBORATION: Humanist reformers attest the problem of fragmentation and the need for Classical standardization. Medieval practitioners contest that there is a problem — they see coherent evolved forms, not fragmentation. Independent textual scholars (outside the benefiting party) document that medieval manuscripts do show substantial regional variation, but disagree on whether this is corruption or legitimate evolution. The founding problem is real as a pedagogical coordination challenge, but contested as a validity claim about medieval Latin itself.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the constraint carries a genuine coordination function (shared standard for fragmented medieval Latin) alongside institutional extraction (authority shift to textual scholars). The measurement series shows extractiveness rising from 0.22 to 0.42 over the interval as the hybrid reading becomes institutionalized — initially the constraint is a minority scholarly position, gradually it becomes the standard in educational reform and humanist circles. Suppression (0.38) is high because the constraint's enforcement requires actively defending the textual-evidence standard against continuity readings and excluding medieval practitioners from authority, even as it claims to grant them 'partial legitimacy.' The suppression requirement rises from 0.18 to 0.38, tracking the investment needed to enforce the hybrid reading as it becomes institutionalized. Theater ratio (0.28) reflects the gap between the constraint's claimed compromise (respecting medieval forms' grammatical core) and its actual operation (systematic devaluation of medieval practice). The theater rises from 0.08 to 0.28 as the compromise framing becomes more necessary to justify the extraction — as medieval practitioners are increasingly excluded, the rhetoric of partial legitimacy intensifies, even as the practice of correction becomes more aggressive. The accessibility of alternatives collapses (0.65) because once the textual-evidence standard is institutionalized, alternatives (arguing for full medieval legitimacy or full discontinuity-based reconstruction) become professionally risky or educationally marginal. Resistance (0.58) is substantial because medieval practitioners actively resist the standard, continuity advocates defend the legitimacy of evolved Latin, and discontinuity advocates push for even fuller reconstruction — the constraint faces real opposition from multiple directions.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist philologists' seat, the constraint appears as rational, evidence-based coordination: establishing a shared Classical standard makes Latin scholarship reliable and education coherent. From the medieval practitioners' seat, the constraint appears as institutional displacement: their fluent, evolved Latin is being corrected by scholars consulting dead texts, and their authority is being transferred to those scholars. From the monastic traditions' seat, there is a paradox: they lose authority over Latin standards but are still required to maintain the ancient manuscripts (the evidential basis for correction). From the educational reformers' seat, the constraint enables legitimation: the reform is not arbitrary preference but correction toward objective Classical correctness. From the excluded continuity advocates' seat, the constraint is a false compromise: granting 'partial legitimacy' to medieval forms while subordinating them to textual correction is a way of dismissing the core claim (medieval evolution is legitimate). The engine will compute these divergences from the structural data — the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists are near the beneficiary end of the directionality scale (d ~ 0.15–0.25): they set the agenda, collect institutional authority and prestige, have mobile exit options (they can switch scholarly communities), and strong power (institutional position). Medieval practice preservers are near the target end (d ~ 0.75–0.85): they bear the cost of having their practice corrected, lose authority, have identity-locked exit (their training is fused with their professional identity), and moderate power. Monastic scribal traditions are near-target but partially bifurcated (d ~ 0.65–0.75): they bear the cost of devaluation but simultaneously benefit from being the custodians of the ancient texts that provide correction evidence — their role as payer (lose authority) and partial beneficiary (maintain the evidential basis) reflects this bifurcation. Educational reformers are beneficiaries (d ~ 0.1–0.2): they gain curriculum authority and institutional prestige. Excluded parties (continuity and discontinuity advocates) are targets of the constraint's exclusionary enforcement (d ~ 0.7–0.8) even though they have mixed power levels — the constraint actively suppresses their positions and limits their institutional voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids false mandatrophy-as-snare classification by honestly declaring a real coordination function (shared standard for fragmented medieval Latin) alongside the extraction (authority transfer, institutional displacement). A pure snare would hide the coordination function entirely under cover of extraction; the hybrid reading explicitly acknowledges that establishing a shared standard was a genuine problem and that the textual-evidence approach is one way to solve it. The mandatrophy omega variables (partial medieval legitimacy operationalization, textual evidence authority grounding) identify where the constraint's persistence might become decoupled from its founding problem — if the distinction between 'legitimate' and 'correctable' medieval forms becomes merely rhetorical cover for wholesale suppression, or if textual authority rests on normative choice rather than empirical evidence, the constraint risks devolving into pure institutional extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the ''correct_latin'' kernel is structurally defensible: the hybrid (partial medieval legitimacy + textual correction), the continuity (medieval evolution is legitimate), or the discontinuity (only ancient Classical forms are legitimate)?',
    'Historical linguistics empirical analysis: trace the transmission of Latin forms from Classical period through medieval period, establishing whether medieval forms represent coherent grammatical evolution (supports continuity) or corruption away from Classical principles (supports discontinuity). Textual scholarship: assess whether ancient manuscripts are reliably reconstructible and whether reconstructed forms were actually used or are scholarly ideals (bears on the normative weight of textual evidence the hybrid reading privileges).',
    'If continuity is established empirically, the hybrid reading''s asymmetry (partial medieval legitimacy) dissolves — medieval forms become fully legitimate as evolved Classical. If discontinuity is established, the hybrid reading''s compromise becomes untenable — medieval forms require fuller reconstruction to Classical originals. If the empirical picture is genuinely mixed (some forms evolved coherently, others corrupted), the hybrid reading''s framing holds but requires empirical specification of which medieval forms fall into which category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether the three readings'' core claims about medieval Latin''s linguistic status can be resolved by historical-linguistic evidence.').

omega_variable(
    medieval_practice_preservers_identity_lock,
    'Is the identity lock experienced by medieval practice preservers (monastic scribal traditions) structural and cognitive (they cannot imagine Latin other than the forms they learned), institutional (their authority and prestige are constituted through medieval scribal practice), or both?',
    'Historical record analysis: do medieval practitioners resist the hybrid reading as an external imposition, or gradually internalize textual correction as the legitimate standard? If internalization occurs, the lock is at least partly cognitive/institutional identity fusion rather than purely structural. Biographical traces: do individual practitioners who adopt the hybrid reading report a shift in self-conception about what correct Latin is?',
    'If the lock is primarily structural (they are excluded from authority regardless of their own acceptance), the suppression metric reflects institutional exclusion and remedying it requires authority redistribution. If the lock is primarily internalized (they come to doubt their own training), the suppression persists even after institutional authority is challenged — the constraint ''s inversion into internalized doubt extends its effective reach beyond the institutional surface.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medieval_practice_preservers_identity_lock, empirical, 'Whether the suppression of medieval practice preservers is structural, internalized, or both.').

omega_variable(
    textual_evidence_authority_ambiguity,
    'Does the authority granted to textual evidence in the hybrid reading rest on a claim that ancient manuscripts are objectively more reliable (an empirical claim about manuscript preservation and transmission), or on a normative choice to privilege written Classical texts over lived medieval practice?',
    'Historiography of philology: trace how humanist scholars justified their privileging of textual evidence. Did they claim ancient texts were more reliably preserved (empirical), or that Classical forms were inherently more legitimate (normative)? Textual criticism study: assess the actual reliability of ancient manuscripts versus medieval copies — do the empirical facts support the authority granted to textual evidence?',
    'If textual authority rests primarily on an empirical claim about reliability, and that claim is false or overstated, the hybrid reading''s justification weakens and alternative readings gain standing. If textual authority is a normative choice, it remains contestable within the framework of the three readings — the constraint''s persistence depends on institutional enforcement of that normative choice, not on objective evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_evidence_authority_ambiguity, conceptual, 'Whether textual authority in the hybrid reading is empirically grounded or normatively chosen.').

omega_variable(
    partial_medieval_legitimacy_operationalization,
    'What does ''partially legitimate'' mean operationally in the hybrid reading? Which medieval forms are grammatically preserved as legitimate, and which are correctable orthography/vocabulary? How are these distinctions made, and who adjudicates them?',
    'Textual scholarship analysis: examine how humanist philologists actually classified medieval forms — do they follow a systematic principle (e.g., ''core grammar is legitimate, orthography is correctable''), or is the classification ad hoc and driven by reformist goals? Comparative analysis: does the boundary between ''legitimate'' and ''correctable'' correspond to linguistic structure, or does it follow institutional/pedagogical convenience?',
    'If the boundary is systematic and linguistically grounded, the hybrid reading has genuine structural content and can be distinguished from the other readings. If the boundary is ad hoc and institutionally driven, the distinction between ''partial'' legitimacy and full dismissal collapses — the reading becomes a cover for wholesale suppression of medieval forms dressed in a language of compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partial_medieval_legitimacy_operationalization, empirical, 'Whether the hybrid reading''s notion of partial medieval legitimacy is operationally coherent or masks institutional displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(corr_tr_t5, correct_latin__hybrid_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(corr_tr_t10, correct_latin__hybrid_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(corr_tr_t15, correct_latin__hybrid_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(corr_tr_t25, correct_latin__hybrid_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(corr_tr_t35, correct_latin__hybrid_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(corr_be_t5, correct_latin__hybrid_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(corr_be_t10, correct_latin__hybrid_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(corr_be_t15, correct_latin__hybrid_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(corr_be_t25, correct_latin__hybrid_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(corr_be_t35, correct_latin__hybrid_reading, base_extractiveness, 35, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(corr_su_t5, correct_latin__hybrid_reading, suppression_requirement, 5, 0.24).
narrative_ontology:measurement(corr_su_t10, correct_latin__hybrid_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(corr_su_t15, correct_latin__hybrid_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(corr_su_t25, correct_latin__hybrid_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(corr_su_t35, correct_latin__hybrid_reading, suppression_requirement, 35, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The 'correct_latin' kernel admits three readings: continuity (medieval evolution is legitimate), discontinuity (medieval is corrupt, requires reconstruction), and hybrid (partial medieval legitimacy + textual correction). Each reading is a separate constraint story with distinct ε, beneficiary/victim structure, and classification. The hybrid reading influences both siblings by establishing a compromise frame that partially legitimizes medieval forms while subordinating them to textual correction — this constrains the negotiation space for both continuity (which must justify full legitimacy against the textual standard) and discontinuity (which must justify fuller reconstruction against the compromise). The three stories are linked via affects_constraints; each declares its reading_relations and axioms in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__hybrid_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
