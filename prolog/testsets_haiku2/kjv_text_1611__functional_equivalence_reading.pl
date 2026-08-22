% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: KJV Functional Equivalence Reading: Complementary Translations
 *   domain: religious/textual
 *
 * SUMMARY:
 *   The King James Version, published in 1611, is the subject of three
 *   structurally distinct constraint readings. This story instantiates the
 *   functional-equivalence reading: multiple translations serve complementary
 *   purposes; KJV is valuable for its literary and historical properties
 *   while modern versions provide clarity for study and teaching. The
 *   constraint under this reading is a coordination mechanism that enables
 *   different communities (liturgical, scholarly, popular) to maintain shared
 *   biblical reference without demanding textual uniformity. The
 *   exclusive-inspiration reading claims KJV is the inerrant English Bible
 *   and all competitors are corrupted; the revisable-translation reading
 *   claims KJV should be systematically updated against better manuscripts
 *   and modern linguistics. These are three different constraints operating
 *   on the same kernel text, each with its own beneficiaries, victims, and
 *   type classification.
 *
 * KEY AGENTS:
 *   - liturgical_communities: benefit from KJV's continued liturgical authority and aesthetic continuity
 *   - literary_scholars: benefit from KJV's canonical status as an English literary and historical artifact
 *   - evangelical_clarity_advocates: coordinate modern clarity needs with historical reverence; bear coordination costs
 *   - KJV_exclusive_advocates: excluded from mainstream consensus; maintain counter-institutions
 *   - textual_critics: excluded from authority over KJV's canonical status in liturgical contexts
 *   - mainline_protestant_denominations: agenda-setters; institutionalize the functional-equivalence reading through denominational standards
 *   - academic_theology_departments: agenda-setters; enforce the reading through curriculum and publication norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.38).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.42).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "KJV Functional Equivalence Reading: Complementary Translations").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious/textual").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, 'c7452a2d-0d47-44b0-bcd2-4bd9f393823d').
narrative_ontology:cs_kernel_codification('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', fixed_text).
narrative_ontology:cs_authority_grounding('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', lineage).
narrative_ontology:cs_interpretation_layer_present('c7452a2d-0d47-44b0-bcd2-4bd9f393823d').
narrative_ontology:cs_reading_relation('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', kjv_text_1611__exclusive_inspiration_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', foundational, multiple_translations_legitimately_complementary).
narrative_ontology:cs_axiom_status(multiple_translations_legitimately_complementary, holdable).
narrative_ontology:cs_axiom_grounding('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', multiple_translations_legitimately_complementary, conventional).
narrative_ontology:cs_axiom('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', foundational, kjv_authority_domain_specific_not_universal).
narrative_ontology:cs_axiom_status(kjv_authority_domain_specific_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', kjv_authority_domain_specific_not_universal, conventional).
narrative_ontology:cs_reference_frame('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', denominational_multi_version_coordination).
narrative_ontology:cs_drift_state('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', contemporary_digital_translation_expansion, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c7452a2d-0d47-44b0-bcd2-4bd9f393823d', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, literary_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, historical_preservationists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, evangelical_clarity_advocates).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, kjv_exclusive_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain liturgical continuity and aesthetic resonance by continuing to use KJV in worship. The familiar cadence, theological vocabulary, and cultural authority of the 1611 text sustain congregational practice and transmission across generations. They benefit from the norm that KJV remains authoritative for liturgical purposes while modern versions handle clarity needs in study and instruction.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, liturgical_communities, beneficiary,
    organized, generational, constrained, continental).

% Treat the KJV as canonical in English literature and theology, studied for its linguistic and cultural influence independent of translation accuracy. The reading positions KJV as a coherent literary artifact with sustained scholarly utility. They benefit from the norm that KJV remains valued for its historical and linguistic properties, not relegated to error-correction cycles.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, literary_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Document and maintain access to the 1611 text as a historical record of Jacobean English, translation philosophy, and theological interpretation. They benefit from the norm that KJV remains in print and studied as a historical artifact, not abandoned or fundamentally revised.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, historical_preservationists, beneficiary,
    moderate, generational, mobile, global).

% Must negotiate a division of labor: KJV retains authority in some contexts (liturgy, cultural reference, historical study) while modern translations carry clarity authority in others (sermon preparation, youth instruction, academic theology). They bear the coordination cost of managing multiple reference texts in teaching and preaching, and accept secondary status when KJV-preferring communities insist on KJV-only liturgy.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, evangelical_clarity_advocates, payer,
    organized, biographical, constrained, national).

% Hold the reading that KJV is the exclusively inspired, inerrant English Bible. They experience the functional-equivalence reading as a demotion of KJV's authority and a concession to modern secularism. Their position is excluded from the mainstream scholarly and institutional consensus that this reading represents; they maintain separate publications, conferences, and educational institutions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_exclusive_advocates, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, kjv_exclusive_advocates, excluded).

% Would argue that the KJV's translation choices should be evaluated against manuscript evidence and linguistic scholarship, not preserved as fixed by tradition or literary value. They are effectively excluded from authority over KJV's canonical status in liturgical and popular contexts, though their scholarship informs modern translation projects.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, textual_critics, excluded,
    powerful, generational, mobile, global).

% Institutionalize and enforce the functional-equivalence reading through denominational standards, hymnal inclusion, educational curricula, and ecumenical agreements. They define which texts are suitable for official use in different contexts (liturgy vs. study), publish supplementary materials explaining translation choices, and model the coordinated use of multiple versions in worship and teaching.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, mainline_protestant_denominations, agenda_setter,
    institutional, generational, arbitrage, continental).

% Set curriculum standards that treat KJV as a historical and literary text studied alongside modern translations and scholarly resources. They enforce the reading through textbook selection, course design, and publication norms that position KJV as one among multiple valid translation traditions rather than as the sole authoritative or errant form.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, academic_theology_departments, agenda_setter,
    institutional, generational, mobile, global).

% Monitor the coordination of multiple translation traditions as an ecumenical achievement, enabling churches with different historical and linguistic commitments to participate in shared biblical discourse without demanding uniformity. They assess whether functional equivalence supports or hinders theological unity.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, ecumenical_councils, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__functional_equivalence_reading, mainline_protestant_denominations).
narrative_ontology:fixing_cost_class(kjv_text_1611__functional_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables communities with different historical, aesthetic, and scholarly commitments (liturgical continuity, literary appreciation, textual precision) to maintain shared biblical reference without requiring all to use the same text. Solves the problem: How can a single scriptural tradition support multiple legitimate purposes (worship, study, historical research) without that single text being optimized for all of them?
% TRANSFER_FUNCTION: Transfers intellectual labor and institutional coordination cost from translation committees (who no longer bear sole responsibility for final, inerrant English form) to educators, clergy, and scholars (who must now teach multiple valid versions and explain their differences). Transfers authority over 'the true text' from a single tradition to a plurality of traditions, each authorized within its domain.
% ABSENT_VOICES: Textual critics and revisionist translators, who would argue the KJV's text should be evaluated against manuscript evidence and updated accordingly. KJV-exclusive advocates, who would argue that preserving functional equivalence concedes too much to secularization and modern skepticism about inspired texts. Both are excluded from the consensus that enables the reading; they maintain counter-institutions but do not set the mainstream standard.
% DISAPPEARANCE_RATIONALE: If the functional-equivalence reading disappeared overnight and one of its siblings' framings took dominance, liturgical practice would reorganize: either to KJV-exclusive use (major disruption for modern-translation communities) or to wholesale replacement (major disruption for tradition-bound communities). Academic curricula would collapse to single-text or pure-critique modes. The managed coexistence of multiple versions would give way to hierarchical or competitive structures.
% FOUNDING_PROBLEM: Early-to-mid 20th century: rising biblical scholarship exposed manuscript variations and translation choices the KJV had harmonized or obscured. Simultaneously, rapid linguistic change made KJV archaic for ordinary readers. The problem was: How to honor the KJV's historical and liturgical authority while accommodating modern scholarship and accessibility without declaring the KJV simply wrong?
% FOUNDING_PROBLEM_CORROBORATION: Mainline denominational archives and ecumenical statements (National Council of Churches, World Council of Churches, denominational liturgical commissions) attest the founding problem remains live: the need to coordinate historical reverence with contemporary accessibility. Textual scholars confirm manuscript discoveries and linguistic knowledge have changed since 1611, justifying the problem's persistence. Independent historians of American religion document the institutional development of multi-version strategies through the late 20th century.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__functional_equivalence_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the reading coordinates multiple legitimate purposes without declaring any tradition errant or subordinate. Beneficiaries (liturgical communities, literary scholars, preservationists) gain from the norm that KJV retains valued status. Payers (evangelical clarity advocates, institutional coordinators) bear the cost of managing multiple versions and explaining differences. Suppression is moderate (0.42) because the reading requires active institutional enforcement to prevent KJV-exclusive or replacement readings from dominating; without denominational standards and academic curriculum requirements, the functional-equivalence frame would not persist. Theater is low-moderate (0.28): the coordination function is genuine (different texts do serve different needs better), but some enforcement activity is purely performative—defending KJV's status against critics rather than advancing the actual coordination problem. The measurement series shows slight rise in extractiveness through the interval (0.32→0.39 over 25 years, then plateau), reflecting increasing institutional cost of managing the multi-version coordination as digital distribution and evangelical growth create new access points and pressure for simplification.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (mainline denominations, academic departments) and the beneficiary seats (liturgical communities, literary scholars) experience this as genuine coordination—solving the real problem of supporting multiple legitimate uses. The payer seats (evangelical clarity advocates) experience it as managed compromise; they gain some clarity gains from modern versions but must continuously justify multi-version practice against both KJV-exclusive and replacement pressures. The excluded seats (KJV-exclusive advocates, textual critics) experience it as either capitulation (if exclusive) or obstruction (if revisionist). The engine computes this divergence from the beneficiary/payer/excluded declarations; the authored claim (rope) represents the reading's own framing, not a neutral adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical communities and literary scholars are beneficiaries (d toward 0.0) because they directly benefit from the norm that KJV retains authority in its domains. Evangelical clarity advocates and institutional coordinators are payers (d toward 1.0) because they bear the ongoing cost of teaching and defending multi-version practice. Excluded seats (KJV-exclusive, textual critics) experience high directionality toward target (d toward 1.0) because the reading's dominance actively suppresses their preferred frames. The functional-equivalence reading's authority is decentralized across multiple institutions (mainline denominations, academic departments, ecumenical councils) rather than concentrated in a single authority, which moderates extraction: no single party captures the gains exclusively, and competing agenda-setters must negotiate with each other and with beneficiary communities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—coordinating historical reverence with contemporary accessibility—remains live (confirmed by denominational archives and ongoing scholarly work). The constraint persists because it solves that live problem. However, there is a secondary mandatrophy risk: as digital distribution and evangelical expansion accelerate, and as academic scripture study increasingly uses critical apparatus and modern translations, the coordination cost of maintaining KJV's liturgical authority may exceed the actual liturgical demand. If the founding problem were to be solved without the KJV-preservation element (e.g., by moving liturgy entirely to modern inclusive-language translations while archiving KJV as historical study), the constraint would become a zombie—persisting for institutional inertia rather than live function. The theater-ratio plateau (0.28 stable from year 20 onward) suggests institutional maintenance has stabilized: neither rising performative activity (which would signal atrophying function) nor declining enforcement (which would signal weakening adherence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_value_vs_doctrinal_accuracy,
    'To what extent does valuing KJV for its literary and historical properties constitute a concession that it may be doctrinally inaccurate compared to modern scholarly understanding?',
    'Theological analysis of whether ''historical and literary value'' necessarily implies doctrinal deficiency, or whether a translation can be simultaneously historically important and theologically sound.',
    'If literary value is genuine and not a cover for doctrinal concession, the functional-equivalence reading''s coordination gain is real. If literary value is rhetorical, the reading collapses into a disguised preference for KJV despite acknowledged accuracy problems, which would shift it toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_value_vs_doctrinal_accuracy, conceptual, 'Whether aesthetic/historical valuation masks doctrinal subordination of KJV or represents genuinely independent grounds for preservation.').

omega_variable(
    institutional_cost_of_coordination,
    'What is the actual cost to denominations and educational institutions of maintaining the functional-equivalence regime versus simplifying to a single primary version?',
    'Institutional audit of curriculum hours spent teaching translation theory, administrative effort spent selecting and justifying multiple versions, and pastor/teacher time spent navigating multi-version practice.',
    'If coordination cost is substantial relative to coordination benefit (i.e., communities would function fine with a single version), the constraint approaches piton classification—persisting through institutional inertia rather than live function. If cost is modest relative to benefit, rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_cost_of_coordination, empirical, 'Whether the measured suppression/coordination cost reflects genuine functional need or institutional self-perpetuation.').

omega_variable(
    exclusive_reading_vs_functional_reading_foreclosure,
    'Does the functional-equivalence reading logically foreclose the exclusive-inspiration reading, or can a believer hold both simultaneously?',
    'Theological and philosophical analysis of whether ''KJV is the inerrant English Bible AND multiple versions serve complementary purposes'' is internally coherent or self-contradictory within a single framework.',
    'If the readings logically foreclose each other, the relation is forecloses (rare); if both can be held by different parties without contradiction, the relation is coexists_with (more likely). This classification affects which network edges are available for propagating constraint competition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusive_reading_vs_functional_reading_foreclosure, conceptual, 'Whether the reading relations between functional-equivalence and exclusive-inspiration are foreclosing or coexisting.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression measured (0.42) structural (active institutional enforcement preventing exclusive or revisionist readings from dominating) or internalized (believers have internalized the functional-equivalence frame so thoroughly that suppression is no longer needed)?',
    'Behavioral test: if denominational enforcement of multi-version pedagogy were removed, would the functional-equivalence reading persist, or would communities revert to exclusive or revisionist preferences?',
    'If structural, the constraint depends on ongoing institutional suppression and could be rapidly displaced. If internalized, the frame is more stable even without enforcement. This affects the stability forecast and the credibility of the ''rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression reflects active institutional enforcement or internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__functional_equivalence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(kjv__tr_t0, observed).
narrative_ontology:measurement(kjv__tr_t5, kjv_text_1611__functional_equivalence_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(kjv__tr_t5, observed).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__functional_equivalence_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(kjv__tr_t10, observed).
narrative_ontology:measurement(kjv__tr_t15, kjv_text_1611__functional_equivalence_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(kjv__tr_t15, observed).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__functional_equivalence_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(kjv__tr_t20, observed).
narrative_ontology:measurement(kjv__tr_t25, kjv_text_1611__functional_equivalence_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(kjv__tr_t25, observed).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__functional_equivalence_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(kjv__tr_t30, observed).
narrative_ontology:measurement(kjv__tr_t35, kjv_text_1611__functional_equivalence_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(kjv__tr_t35, observed).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__functional_equivalence_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(kjv__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(kjv__be_t0, observed).
narrative_ontology:measurement(kjv__be_t5, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(kjv__be_t5, observed).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(kjv__be_t10, observed).
narrative_ontology:measurement(kjv__be_t15, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(kjv__be_t15, observed).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(kjv__be_t20, observed).
narrative_ontology:measurement(kjv__be_t25, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement_basis(kjv__be_t25, observed).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(kjv__be_t30, observed).
narrative_ontology:measurement(kjv__be_t35, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(kjv__be_t35, observed).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(kjv__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(kjv__su_t0, observed).
narrative_ontology:measurement(kjv__su_t5, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement_basis(kjv__su_t5, observed).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(kjv__su_t10, observed).
narrative_ontology:measurement(kjv__su_t15, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(kjv__su_t15, observed).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(kjv__su_t20, observed).
narrative_ontology:measurement(kjv__su_t25, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(kjv__su_t25, observed).
narrative_ontology:measurement(kjv__su_t30, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(kjv__su_t30, observed).
narrative_ontology:measurement(kjv__su_t35, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 35, 0.42).
narrative_ontology:measurement_basis(kjv__su_t35, observed).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(kjv__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__functional_equivalence_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% The KJV kernel (the 1611 text itself) generates three structurally distinct constraints corresponding to three different readings of what textual authority KJV should hold. The exclusive-inspiration reading claims KJV is inerrant and competitors are corrupted (Mountain-type); the revisable-translation reading claims KJV should be systematically updated against better manuscripts and modern linguistics (Scaffold-type, with sunset as linguistic knowledge accumulates); this reading claims multiple versions serve complementary purposes with decentralized authority (Rope-type, under this authoring). Each reading has its own beneficiaries, victims, suppression structure, and classification. They are linked by network edges because changes in one reading's institutional dominance directly affect the others' operating environment: if exclusive-inspiration gains dominance, functional-equivalence loses institutional support; if revisable-translation gains dominance, KJV's fixed status is eroded. All three remain live positions in contemporary Christianity, held by different communities and traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__functional_equivalence_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
