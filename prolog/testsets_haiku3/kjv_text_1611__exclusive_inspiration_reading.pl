% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration Doctrine
 *   domain: religious/textual
 *
 * SUMMARY:
 *   The KJV Exclusive Inspiration reading asserts that the King James Version
 *   (1611) is the sole divinely inspired English Bible, with all modern
 *   translations representing either corruption or inferior scholarship
 *   unworthy of trust. This reading is instantiated primarily by organized
 *   KJV-Only leadership (Independent Baptist and some fundamentalist
 *   networks) and enforced through pulpit authority, educational control, and
 *   systematic delegitimization of alternative translations. The constraint
 *   coordinates a textual boundary (one canonical English text) while
 *   extracting hermeneutic authority, suppressing academic textual criticism
 *   and modern translations as spiritually dangerous. The metrics reflect
 *   rising extraction: as the doctrine hardens over the measurement interval,
 *   more scholarly voices are excluded, more congregational autonomy is
 *   restricted, and more theatrical 'textual preservation' rhetoric defends
 *   what is structurally gatekeeping authority.
 *
 * KEY AGENTS:
 *   - KJV-Only leadership: Organized institutional actors (churches, publishing houses, seminaries) that teach and enforce exclusive inspiration doctrine; sole arbiter of textual legitimacy
 *   - Modern translation publishers: Face systematic delegitimization and market exclusion within KJV-Only networks despite institutional power outside those networks
 *   - Academic textual critics: Authority is undermined; their empirical findings about manuscript evidence are treated as hostile rather than evidence
 *   - Congregants seeking clarity: Powerless agents trapped by spiritual authority and social ties, taught that clarity-seeking is pride; dependence on leadership's interpretations is enforced
 *   - Manuscript evidence community: Institutional observer whose work contradicts the doctrine; excluded from legitimacy within the reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.78).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.81).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/textual").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, 'b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42').
narrative_ontology:cs_kernel_codification('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', fixed_text).
narrative_ontology:cs_authority_grounding('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', lineage).
narrative_ontology:cs_interpretation_layer_present('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42').
narrative_ontology:cs_reading_relation('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_reading_relation('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_axiom('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', foundational, kjv_sole_divine_inspiration).
narrative_ontology:cs_axiom_status(kjv_sole_divine_inspiration, holdable).
narrative_ontology:cs_axiom_grounding('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', kjv_sole_divine_inspiration, deontological).
narrative_ontology:cs_axiom('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', foundational, modern_translation_corruption).
narrative_ontology:cs_axiom_status(modern_translation_corruption, holdable).
narrative_ontology:cs_axiom_grounding('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', modern_translation_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', kjv_textual_authority_established).
narrative_ontology:cs_drift_state('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', contemporary_textual_scholarship_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('b1e0f1ea-01f7-4ff0-ab5f-884481c6eb42', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_publishers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, academic_textual_critics).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, congregants_seeking_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, congregants_seeking_clarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches and enforces the doctrine that the KJV alone is divinely inspired in English, with all modern translations representing corruption or inferior scholarship. Controls pulpits, publishing, and educational institutions within their networks. Derives authority from claiming direct lineage to apostolic textual tradition and frames themselves as faithful guardians against heretical modernism. The doctrine shields their interpretive monopoly and makes competing translations illegitimate by definition.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    organized, generational, identity_locked, continental).

% Invest substantially in translation projects (ESV, NIV, NASB, NET, etc.) grounded in better manuscript evidence and modern linguistic scholarship. Face systematic delegitimization within KJV-Only networks: their work is declared corrupted, their scholarship dismissed as prideful modernism, their market access restricted to congregations outside KJV-Only communities. Can exit by publishing to other markets, but lose the entire fundamentalist and some evangelical denominations.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_publishers, payer,
    powerful, biographical, mobile, global).

% Conduct scholarly textual criticism on biblical manuscripts, identifying errors in the KJV based on older Greek and Hebrew sources. Their work contradicts the exclusive inspiration claim directly. Face delegitimization and exclusion from KJV-Only educational and publishing spaces. Operate primarily in secular academia and mainstream Christian institutions; the constraint does not apply to them, but it suppresses their voice within fundamentalist communities.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, academic_textual_critics, payer,
    powerful, generational, arbitrage, global).

% Attend KJV-Only churches seeking spiritual guidance. Encounter archaic language (thee, thou, ye) and obsolete word senses (e.g., 'charity' for 'love', 'let' for 'hinder') that obscure meaning. Are taught that asking for clarity or using modern translations for study is spiritually dangerous — pride, compromise with worldly scholarship, rejection of God's preserved word. Their exit options are constrained by social ties, family, and theological conviction that leaving means apostate drift.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, congregants_seeking_clarity, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, congregants_seeking_clarity, beneficiary).

% Custodians and analysts of ancient biblical manuscripts (Dead Sea Scrolls, Egyptian papyri, Greek uncials, etc.). Their empirical findings consistently show the KJV to be based on a narrow, late manuscript tradition (Textus Receptus) and contain readings not found in earlier sources. Their work is treated as hostile evidence by KJV-Only doctrine; their authority is undermined by claims that they have 'lost faith' or fallen under academic pride.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, manuscript_evidence_community, observer,
    institutional, generational, analytical, global).

% Lead congregations and denominations that respect the KJV historically but endorse multiple translations as legitimate. Would affirm the KJV as beautiful and worthy of study, but reject the exclusive inspiration claim as unsupported by textual evidence and pastorally harmful. Their voices are suppressed within KJV-Only spaces as compromise; they are framed as fallen from the faith.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, moderate_evangelical_leaders, excluded,
    powerful, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, canonical English scripture that unifies a reading community around a fixed textual foundation—a coordination problem in communities where textual variation would create doctrinal instability and interpretive fragmentation.
% TRANSFER_FUNCTION: Moves textual authority and hermeneutic power from academic scholars and modern publishers to KJV-Only leadership and gatekeepers. Modern translations are declared illegitimate, forcing congregants into dependence on KJV-Only interpretation and leadership's explanations of the archaic language.
% ABSENT_VOICES: Congregants with genuine comprehension difficulties (especially those not raised in KJV-reading traditions) and moderate evangelical leaders who support the KJV but reject exclusivity. Their testimony would undermine the claim that the doctrine serves the community rather than the leadership. Academic manuscript specialists are excluded because their empirical findings contradict the exclusive inspiration premise.
% DISAPPEARANCE_RATIONALE: If the exclusive inspiration doctrine vanished, modern translations would immediately become legitimate study tools in these communities, congregant comprehension would improve, textual criticism could be engaged without spiritual threat, and leadership's interpretive gatekeeping would lose its doctrinal backing. The constraint persists because it concentrates authority; its removal would redistribute hermeneutic power.
% FOUNDING_PROBLEM: In the 16th–17th centuries, Protestant communities needed a stable English scripture independent from Catholic-controlled Latin Vulgate; the KJV provided that fixed text, establishing textual authority over Rome and internal coherence over variant readings.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the English Reformation and biblical translation (scholars outside the benefiting parties) confirm the KJV solved a real coordination problem in its era. However, they also attest that the founding problem—doctrinal dependence on Rome, unavailable older manuscripts, absence of modern linguistic tools—is substantially resolved. The constraint persists as what KJV-Only leadership claims is faithfulness but what independent scholars identify as institutional inertia and gatekeeping.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78 at interval end) is high because the constraint concentrates hermeneutic authority in KJV-Only leadership by declaring all alternatives illegitimate. The measurement series shows rising extraction from 0.55 to 0.78 over the interval: as the doctrine's institutional reach grows and academic challenges accumulate, more aggressive defense and gatekeeping are required. Suppression (0.81) reflects the active work of excluding modern translations from pulpits, classroom curricula, and study recommendations—alternatives do not simply fail to compete; they are actively suppressed as spiritually dangerous. Theater_ratio rises from 0.20 to 0.42: early in the interval, the doctrine plausibly coordinates textual unity; by the end, increasingly elaborate rhetoric about 'textual preservation' and 'word-for-word fidelity' (claims not empirically sustained) maintains the doctrine as other functions atrophy. The constraint persists not because congregants freely choose KJV study, but because the doctrine restricts what counts as legitimate scripture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (congregants, modern publishers) experience this as enforced gatekeeping—their alternatives are suppressed, their comprehension needs are treated as prideful, their agency is restricted. The beneficiary seat (KJV-Only leadership) experiences it as faithful preservation—defending truth against corruption. The engine's per-seat computation should reveal this asymmetry: beneficiaries compute rope (coordination that protects), payers compute snare or tangled_rope (extraction defended by authority). The authored claim (tangled_rope) reflects the true structure: there is a coordination function (textual stability), but it is asymmetrically captured—some coordinate and benefit while others are coordinated and extracted from.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-Only leadership sits at d near 1.0 (full target in reverse: they extract from the constraint's operation, controlling interpretation). Congregants sit at d near 1.0 (full targets: they bear suppression, restricted alternatives, dependence on interpretation). Modern publishers sit at d mid-range (powerful but with exit options—they serve other markets). Academic critics sit at d low (they operate outside the constraint's frame, though their work is suppressed within it). The structural relationship is asymmetric: leadership benefits, victims pay. Suppression is uniformly high (0.81) because the doctrine's persistence depends on actively preventing congregants from accessing modern translations and academic findings that would undermine the exclusive inspiration claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual dependence on Rome, internal variant readings in the Reformation era) is dead: modern academic consensus on manuscript evidence is established, alternative English translations are widely available and theologically sound, and the coordination function (textual stability) could be served by other canonical texts. The constraint persists as institutional inertia defended by theology: the doctrine was built to solve a real problem but now persists to maintain leadership authority and congregational dependence. This is a classic mandatrophy candidate—the founding problem is resolved but the constraint extracts ongoing authority rents. The measure from the interval (founding_problem_status=dead, disappearance_verdict=world_rearranges, rising theater_ratio) flags this: the doctrine's justification has atrophied, but its gatekeeping function persists because leadership benefits from the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_textual_claim_status,
    'Is the KJV based on older or later Greek manuscripts, and do earlier sources contain the readings the KJV prioritizes?',
    'Examination of the Greek manuscript tradition (Dead Sea Scrolls, Egyptian papyri, uncials like Aleph and B, Majority Text): empirical comparison of which readings appear in which sources and at what historical depth.',
    'If the KJV is based on the LATER Textus Receptus (16th century) and earlier manuscripts systematically differ, the exclusive inspiration claim requires that divine inspiration ''skipped'' the first 1500 years of manuscript tradition—a premise that contradicts the reading''s own scriptural hermeneutic. This would reclassify the constraint as pure snare (the empirical premise collapses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_textual_claim_status, empirical, 'Empirical basis for the exclusive inspiration claim—whether the KJV manuscripts are actually older/purer as claimed.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of modern translations in KJV-Only communities structural (external gatekeeping, access barriers) or internalized (congregants have adopted the belief that clarity-seeking is pride)?',
    'Post-exit trajectories of congregants who leave KJV-Only communities: do they immediately adopt modern translations and find comprehension easier, or do they retain suspicion of modern versions and fear of corruption? Longitudinal study of second-generation KJV-Only congregants raised in linguistically mixed households.',
    'If suppression is primarily internalized (congregants believe modern translations are corrupted), the constraint''s effective suppression is higher than the structural gatekeeping measure—the target carries the suppression with them after exit. This suggests the constraint operates through identity fusion rather than institutional force alone. If suppression is structural (gatekeeping by leadership), it can be reversed by access to alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of modern translations is enforced by external gatekeeping or internalized as belief.').

omega_variable(
    coordination_extraction_boundary,
    'Can textual stability and doctrinal coherence be coordinated through other canonical texts (e.g., endorsing a single modern translation while allowing diversity in study editions) or does the coordination function require the KJV specifically?',
    'Ethnographic study of communities that endorse a single modern translation canonically (e.g., NASB-only or ESV-only churches) but reject others: do they experience the same textual unity and doctrinal stability, or does diversity in study editions produce the fragmentation the exclusive inspiration doctrine claims to prevent?',
    'If other translations can serve the coordination function equally well, the KJV exclusivity is pure extraction riding on a generalizable coordination service. The constraint would reclassify from tangled_rope (coordination + extraction) to snare (extraction with a coordination cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether textual stability is achievable through alternative canonical texts or only through KJV exclusivity.').

omega_variable(
    kernel_contest_sibling_foreclosure,
    'Does the exclusive_inspiration_reading logically foreclose the revisable_translation_reading and functional_equivalence_reading, or do these readings coexist as competing positions held by different parties?',
    'Analysis of the logical premises: if exclusive inspiration is true, are revisable and functional readings internally contradictory, or do they simply rest on different theological premises that could be held by different communities without one premises directly negating the other''s core premise?',
    'If the readings genuinely foreclose each other (exclusive inspiration PRECLUDES the possibility of legitimate revision or functional diversity), then this constraint logically incompatible with the sibling constraints. If they coexist (different communities hold different premises without logical contradiction at the framework level), then the constraint is part of a kernel family where reading_relations are ''coexists_with''. This determines the cs_structure.reading_relations taxonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_sibling_foreclosure, conceptual, 'Logical structure of the kernel contest—whether readings foreclose or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(kjv__tr_t0, observed).
narrative_ontology:measurement(kjv__tr_t5, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(kjv__tr_t5, observed).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(kjv__tr_t10, observed).
narrative_ontology:measurement(kjv__tr_t15, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(kjv__tr_t15, observed).
narrative_ontology:measurement(kjv__tr_t25, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(kjv__tr_t25, observed).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(kjv__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(kjv__be_t0, observed).
narrative_ontology:measurement(kjv__be_t5, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(kjv__be_t5, observed).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(kjv__be_t10, observed).
narrative_ontology:measurement(kjv__be_t15, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(kjv__be_t15, observed).
narrative_ontology:measurement(kjv__be_t25, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(kjv__be_t25, observed).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(kjv__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(kjv__su_t0, observed).
narrative_ontology:measurement(kjv__su_t5, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(kjv__su_t5, observed).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement_basis(kjv__su_t10, observed).
narrative_ontology:measurement(kjv__su_t15, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement_basis(kjv__su_t15, observed).
narrative_ontology:measurement(kjv__su_t25, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement_basis(kjv__su_t25, observed).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(kjv__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel kjv_text_1611. The exclusive_inspiration_reading asserts the KJV as the sole divinely inspired English Bible. Sibling readings (functional_equivalence and revisable_translation) instantiate different structural constraints with different ε, victim sets, and beneficiary structures. All three readings operate on the same contested kernel text; they are linked via network.affects_constraints because the exclusive_inspiration reading's foundational premises directly challenge the legitimacy of the sibling readings' core claims. The epsilon-invariance principle (OQ-DP-001) requires separate stories: the exclusive reading has high extraction (0.78) via gatekeeping; the functional_equivalence reading would have low extraction via complementary-role coordination; the revisable reading would carry moderate extraction via deference to academic authority. The readings are not alternative perspectives on one constraint—they are structurally distinct constraints on the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
