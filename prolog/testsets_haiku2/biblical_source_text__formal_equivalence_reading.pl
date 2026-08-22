% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__formal_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal Equivalence Biblical Translation Constraint
 *   domain: religious/hermeneutical/textual
 *
 * SUMMARY:
 *   The formal-equivalence reading of biblical translation asserts that a
 *   translator's primary obligation is to preserve the structural,
 *   grammatical, and semantic features of the source text, even when doing so
 *   produces awkward, opaque, or ambiguous target-language expressions.
 *   Intelligibility in the target language is treated as a secondary
 *   responsibility — one that the reading community is obligated to assume
 *   through education, not one that should reshape the translation itself.
 *   This reading is one of three contesting interpretations of what 'faithful
 *   biblical translation' means. The formal-equivalence reading benefits
 *   hermeneutically conservative communities and textual-authority
 *   institutions by keeping interpretation dependent on expert mediation and
 *   by making source-language knowledge a requirement for full textual
 *   access. Non-specialist readers bear substantial costs in cognitive labor
 *   and comprehension difficulty. The constraint exhibits tangled-rope
 *   structure: it solves a genuine coordination problem (multilingual
 *   scholarly reference) while simultaneously extracting authority rents from
 *   those who lack language training.
 *
 * KEY AGENTS:
 *   - hermeneutically_conservative_communities: Primary beneficiary; identity anchored in source-structure fidelity
 *   - textual_authority_institutions: Agenda-setter; controls translation standards and language-education apparatus
 *   - non_specialist_readers: Primary victims; bear cost of required expertise and reduced accessibility
 *   - lay_congregations: Secondary payers; experience reduced engagement and require pastoral mediation
 *   - dynamic_equivalence_advocates: Excluded from authority structures; their alternative framing is delegitimized
 *   - critical_textual_scholars: Excluded from many committees; their arguments about textual instability are marginalized
 *   - biblical_language_educators: Secondary beneficiary; benefit from demand for language training
 *   - theological_conservatives: Powerful beneficiaries; institutional investment in source-fidelity narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.68).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.55).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal Equivalence Biblical Translation Constraint").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/hermeneutical/textual").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, '91ad4a5f-fe1d-4277-aa14-ecca140b7ca9').
narrative_ontology:cs_kernel_codification('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', fixed_text).
narrative_ontology:cs_authority_grounding('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', lineage).
narrative_ontology:cs_interpretation_layer_present('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9').
narrative_ontology:cs_reading_relation('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', foundational, source_structure_is_meaning_carrier).
narrative_ontology:cs_axiom_status(source_structure_is_meaning_carrier, holdable).
narrative_ontology:cs_axiom_grounding('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', source_structure_is_meaning_carrier, deontological).
narrative_ontology:cs_axiom('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', foundational, reader_education_not_translation_adjustment).
narrative_ontology:cs_axiom_status(reader_education_not_translation_adjustment, holdable).
narrative_ontology:cs_axiom_grounding('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', reader_education_not_translation_adjustment, conventional).
narrative_ontology:cs_reference_frame('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', reformation_authorized_vernacular_scripture).
narrative_ontology:cs_drift_state('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', contemporary_digital_era_congregations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91ad4a5f-fe1d-4277-aa14-ecca140b7ca9', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, textual_authority_institutions).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, non_specialist_readers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, lay_congregations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, lay_congregations).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, biblical_language_educators).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, theological_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious communities (denominations, seminaries, scholarly guilds) that anchor their authority legitimacy in textual stability and resistance to modernizing reinterpretation. They benefit from formal equivalence because it preserves structural ambiguities, marginal readings, and interpretive layers that their hermeneutical traditions have built authority on. Exit would mean admitting that the text's meaning has been unstable all along — undercutting the foundational legitimacy claim. Their identity as 'people of the text' (as distinct from 'people of the meaning') is constituted through fidelity to source structure.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_communities, beneficiary,
    organized, generational, identity_locked, global).

% Scholarly guilds, Bible translation committees, denominational publishing bodies, and academic theological institutions that set and enforce the formal-equivalence standard. They maintain the apparatus (commentaries, lexica, hermeneutical training) that renders source-structure fidelity intelligible and authoritative. They benefit from controlling the legitimacy of what counts as 'faithful translation' and from maintaining translational work as a specialized, institution-mediated practice requiring years of advanced education.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, textual_authority_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Individual congregants, lay Bible readers, and non-academic communities who lack training in biblical languages and hermeneutical apparatus. They encounter formal-equivalence translations as opaque — awkward syntax, marginal notes requiring expert decoding, ambiguities left unresolved. They must either invest years learning Greek and Hebrew (creating dependency on institutions offering that education), accept reduced intelligibility and spiritual engagement, or exit to dynamic-equivalence translations and invite critique that they are using 'unfaithful' translations. The cost is cognitive labor, mediated access to meaning, and vulnerability to institutional gatekeeping.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, non_specialist_readers, payer,
    powerless, biographical, constrained, global).

% Communities of worship using formal-equivalence translations in congregational settings. They bear the cost of reduced accessibility (pastors must spend additional time explaining textual choices and ambiguities) and experience reduced congregational engagement when texts are difficult. They derive some benefit from connection to 'the actual words' and the symbolic authority of the original-language connection, even when they cannot access it directly. Their exit is constrained by denominational allegiance and the theological narrative that formal equivalence represents greater fidelity.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, lay_congregations, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, lay_congregations, beneficiary).

% Evangelical and pastoral-mission-oriented communities and scholars who have developed alternative translation philosophies prioritizing communicative effectiveness. They are excluded from the formal-equivalence governing apparatus and their translations are often characterized as 'paraphrases' or 'unfaithful' by the authority institutions, despite their own theoretical sophistication. They would argue for intelligibility-centered metrics and pastoral care as primary theological values, but their voices are structurally marginalized in academic and denominational translation committees.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, dynamic_equivalence_advocates, excluded,
    organized, generational, constrained, global).

% Scholars (primarily in historical-critical tradition) who argue that formal equivalence to a source text assumes a false consensus on what 'the source text' is — the original autographs are not extant, textual criticism remains contested, and the earliest recoverable text is itself an interpretation. They would demand that translation prioritize explicit acknowledgment of textual uncertainty over the pretense of fidelity to a stable source. They are excluded from many denominational translation committees and their work is often dismissed as 'too academic' or destructive to faith.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, critical_textual_scholars, excluded,
    institutional, generational, constrained, global).

% Academic and denominational educators teaching Greek and Hebrew. They benefit from formal equivalence because it creates structural demand for their expertise: as translations preserve source-structure complexities and ambiguities, students and pastors must learn the languages to understand them, ensuring enrollments and institutional support for language programs. They participate in agenda-setting through curriculum design and through translation committee membership.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, biblical_language_educators, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, biblical_language_educators, agenda_setter).

% Powerful institutional actors (denominations, wealthy churches, influential theologians) who have invested theological weight in the claim that 'God's Word' is embodied in source-language structure and that translation's purpose is to preserve that structure even at cost to intelligibility. They benefit from formal equivalence by maintaining the theological narrative that closer-to-the-source equals closer-to-God, which underwriting their institutional authority to interpret and teach. They have the institutional power to shape which translations are endorsed, distributed, and recommended.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, theological_conservatives, beneficiary,
    powerful, generational, mobile, global).

% Scholars from outside the religious tradition analyzing the constraint's operation — historians of translation, social theorists, cognitive scientists studying how communities maintain authority through textual practices. They observe the structural dynamics without advocating for any reading.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__formal_equivalence_reading, textual_authority_institutions).
narrative_ontology:fixing_cost_class(biblical_source_text__formal_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, reproducible, source-structure-faithful translation apparatus that allows multilingual scholarly communities to reference the same Hebrew/Aramaic/Greek text without loss of nuance through paraphrase. Solves the collective problem of maintaining textual-critical access across language barriers and across generations, preventing meaning from degrading into pure interpretive drift.
% TRANSFER_FUNCTION: Moves cognitive labor and interpretive authority from the text itself toward institutional authorities (translators, scholars, denominational interpreters). Non-specialist readers must invest time in language study or accept mediated, expert-explained meaning. Publishers, seminaries, and translation committees collect authority rents by controlling which translations are 'faithful' and which are not.
% ABSENT_VOICES: Critical textual scholars (who would assert that there is no stable source text) and dynamic-equivalence advocates (who would prioritize congregational intelligibility and pastoral care) are structurally excluded from translation committee governance and their work is delegitimized as 'paraphrasing' or 'unfaithful,' despite their theological and hermeneutical sophistication.
% DISAPPEARANCE_RATIONALE: If formal equivalence as a translation mandate disappeared — if congregations and scholars were encouraged to use dynamic-equivalence, simplified, or paraphrastic translations without stigma — institutional language education would face enrollment pressure, denominational authority grounded in textual fidelity would require regrounding, and congregational biblical literacy would shift from 'understanding the source structure' toward 'understanding the meaning in contemporary terms.' The landscape of who has authority to interpret would reorganize.
% FOUNDING_PROBLEM: Early Protestant reform required that congregations access Scripture in their own languages, but the theological legitimacy of that access depended on claiming fidelity to the original languages and resisting Roman Catholic claims that only the Vulgate (or the magisterium) was authoritative. Formal equivalence emerged as the theological answer: fidelity to source structure proved the translation's legitimacy and protected Protestant congregations from charges of textual corruption.
% FOUNDING_PROBLEM_CORROBORATION: Conservative Protestant scholars and institutions attest the founding problem remains live: that ongoing resistance to modernizing reinterpretation and to 'cutting the text to suit contemporary taste' requires maintaining formal-equivalence standards. Critical scholars, pastoral practitioners, and historical analysts attest the founding problem has shifted: modern congregations no longer face Reformation-era ecclesiastical monopolies; the problem now is accessibility and spiritual formation, not papal authority. Empirical evidence from congregational literacy studies and from uptake of dynamic-equivalence translations in evangelical settings supports the contested reading.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__formal_equivalence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderately high because the constraint creates asymmetric access costs: to participate in the 'faithful reading' community requires mastering source languages, which is gatekept through institutional education systems. The constraint is claimed as tangled rope (genuine coordination + asymmetric extraction), and the metrics bear this out. Suppression (0.55) is moderate-to-high: the constraint's persistence depends on actively delegitimizing alternative translations and excluding critics from authority structures, not on participant voluntary preference. Theater (0.42) is moderate-high: institutional rhetoric celebrates 'fidelity to the text,' but as extractiveness and suppression measurements increase over the interval, the proportion of institutional activity devoted to defending the source-structure principle against alternatives (as opposed to actual translation work) grows. The measurement series show extraction and theater rising as alternatives gain institutional presence (time 0-30 interval) and then plateauing as a stable equilibrium is reached (time 30-50). Suppression rises steadily, indicating intensifying need for active exclusion and delegitimization as critical and dynamic-equivalence scholarship becomes more sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   The formal-equivalence reading's authority institutions would describe this as a rope: genuine coordination (multilingual scholarship, textual stability, resistance to corruption). Payer seats would describe it as a snare: the coordination is real but incidental; what persists is the extraction of authority rents and the gatekeeping of access. Lay congregations might describe it as a scaffold: they use formal-equivalence translations while church leaders educate them, but they would prefer to transition to dynamic equivalence once intelligibility improves. The engine's per-seat computation should resolve this perspectival gap without reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Hermeneutically conservative communities are structural beneficiaries: they benefit from fidelity to source structure that preserves the interpretive layers their hermeneutical traditions are built on. They have moderate power (organized but not institutional) and identity_locked exit (their identity as 'people of the text' is constituted through the source-structure commitment). This produces directionality near the beneficiary end (d ≈ 0.1-0.2). Textual-authority institutions are agenda-setters and beneficiaries: they control the translation standard and the education apparatus, and they have institutional power and mobile exit (they could change standards if they chose). This produces d near beneficiary but with high power amplification (d ≈ 0.15-0.25). Non-specialist readers are clear victims: they bear cognitive costs and are constrained (identity_locked in the sense that their religious identity is tied to the text, even though they cannot access it directly without help). This produces directionality near target (d ≈ 0.75-0.85). Lay congregations sit lower (d ≈ 0.65-0.75): they are victims but have some coordination benefit from symbolic connection to the original, and their organizational power (moderate) provides some mitigation. Excluded actors (dynamic-equivalence advocates, critical scholars) don't appear in the directionality calculation because they are excluded, but their structural relationship to the constraint is targeting — they would face high d if included in enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Reformation-era need for authorized vernacular Scripture resisting papal monopoly) is largely resolved in contemporary Protestant contexts — there is no longer genuine threat of textual corruption via Rome, and Protestant denominations govern their own translation standards independently. The formal-equivalence reading maintains that the problem persists (vigilance against reinterpretation, resistance to modern hermeneutical pressure), but this is a shifted problem: it is now about institutional authority maintenance, not about protection from external threat. The reading's classification as tangled rope reflects this: it solves a real coordination problem (scholarly reference, textual stability) while simultaneously extracting authority rents. The mandatrophy signal is the rising theater ratio (time 0 → 0.25, time 50 → 0.42): as alternatives become more sophisticated and as congregational demand for intelligibility grows, the proportion of institutional activity devoted to defending the source-structure principle against alternatives (rather than doing translation work) increases. This indicates that institutional investment is increasingly in performance and gatekeeping rather than in the coordination function itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_text_stability_ambiguity,
    'Is there a stable, recoverable source text that formal equivalence can be faithful to, or is the source itself a contested textual-critical question that cannot be resolved outside of interpretive frameworks?',
    'Critical textual scholarship comparing manuscript families and reconstructing the earliest recoverable text. The resolution lies in whether scholars achieve consensus on an original text or whether textual criticism itself remains fundamentally contested.',
    'If source text is unknowable or fundamentally ambiguous, formal equivalence becomes impossible in principle — the constraint would need reclassification from tangled rope to snare (the fidelity claim becomes a cover story). If source text is recoverable but disputed, the constraint remains tangled rope but with higher extraction costs (non-specialists must learn which textual tradition the translation follows).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(source_text_stability_ambiguity, empirical, 'Whether formal equivalence has a coherent epistemic foundation').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative readings (dynamic equivalence, critical reconstruction) maintained by structural exclusion from authority institutions, by internalized theological commitments (''fidelity is a moral duty''), or by both in what proportions?',
    'Post-exit trajectories: if scholars who leave conservative institutions continue to reject dynamic equivalence due to internalized fidelity commitments, suppression is partly internalized. If they readily adopt alternatives, suppression is primarily structural. Also: comparison of suppression intensity across different cultural contexts (high in institutional seminaries, lower in informal congregations).',
    'If suppression is primarily structural, fixing the constraint is possible by changing institution rules (opening translation committees, legitimizing alternatives). If internalized, the constraint would persist even after structural gates are removed — the classification might shift from tangled rope to snare, indicating identity-locked dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is externally enforced or internally adopted').

omega_variable(
    coordination_function_necessity,
    'Is a formal-equivalence translation standard structurally necessary for multilingual scholarly coordination, or can dynamic-equivalence or critical-reconstructive translations serve the same scholarly reference function with slightly different tradeoffs?',
    'Examination of actual scholarly practice: do scholars working from dynamic-equivalence texts suffer coordination failures or lose reference precision compared to those using formal-equivalence texts? Do critical editions with explicit textual notes serve as adequate reference apparatus?',
    'If coordination can be served equivalently by alternatives, formal equivalence''s tangled-rope classification would shift toward snare — the coordination function becomes incidental and the constraint operates primarily for extraction. If formal equivalence genuinely minimizes coordination costs, the classification stands and the beneficiary/victim structure is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether formal equivalence provides unique coordination value').

omega_variable(
    identity_lock_depth_conservative_communities,
    'Is the identity-lock of hermeneutically conservative communities a commitment to source-structure fidelity per se, or to a broader commitment to textual authority and resistance to modernization that could be satisfied by other translation philosophies?',
    'Ethnographic and historical analysis of actual practices: do communities that shift to dynamic-equivalence translations experience identity disruption and community fragmentation? Or do they integrate the new translation while maintaining their conservative theological identity? Comparative cases: conservative Presbyterian shift to NASB (formal), evangelical shift to NIV (more dynamic) and their impact on community coherence.',
    'If identity-lock is deep and specifically tied to source-structure fidelity, the classification remains tangled rope (beneficiaries are genuinely wedded to the constraint). If identity-lock is superficial (performative, easily renegotiated), the constraint approaches snare: identity-lock serves as a cognitive capture mechanism maintaining dependency on authority institutions rather than as genuine preference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_depth_conservative_communities, conceptual, 'Depth of identity commitment to source-structure fidelity vs. broader textual conservatism').

omega_variable(
    kernel_reading_contest_status,
    'Is the contest between formal equivalence, dynamic equivalence, and critical reconstruction a live, three-way dispute with each reading''s advocates maintaining institutional presence, or has one reading achieved institutional dominance such that the others are marginalized?',
    'Institutional audit: survey of Bible translation committees, seminary curricula, publishing decisions, and scholarly citation patterns to measure which reading dominates at which institutional sites (denominational vs. evangelical vs. academic).',
    'If the contest is genuinely three-way and coexisting, each reading''s classification depends on which institutional seat is occupied. If formal equivalence has achieved dominance in academic/conservative institutions while dynamic equivalence dominates evangelical/mission-oriented institutions, the readings should be understood as structurally coexisting (not foreclosing). If formal equivalence has achieved near-total institutional dominance, the suppression measurement should rise and the constraint might shift toward piton (maintained by institutional inertia rather than active preference).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, empirical, 'Institutional distribution and dominance of competing translation readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__formal_equivalence_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__formal_equivalence_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__formal_equivalence_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__formal_equivalence_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__formal_equivalence_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__formal_equivalence_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__formal_equivalence_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__formal_equivalence_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__formal_equivalence_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__formal_equivalence_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__formal_equivalence_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__formal_equivalence_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__formal_equivalence_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__formal_equivalence_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__formal_equivalence_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(biblical_source_text__formal_equivalence_reading, 0.08).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The biblical_source_text kernel decomposes into three structurally distinct constraints, each instantiating a different reading of what 'faithful translation' means. Formal equivalence prioritizes source-structure fidelity; dynamic equivalence prioritizes target-language communicative effectiveness; critical reconstruction prioritizes textual-critical warrant and honesty about textual uncertainty. These are not perspectives on a single constraint — they are three constraints with different ε values, different beneficiary/victim sets, and different classifications. The formal-equivalence reading exhibits higher extractiveness from non-specialists than the dynamic-equivalence reading does, and both differ from the critical reading's extraction profile (higher on congregations, lower on scholars). Link them via network.affects_constraints to enable the corpus to model how institutional dominance of one reading affects the viability of alternatives (influences relationship), and how doctrinal incompatibilities constrain joint adoption (coexists_with where readings are held by different institutional parties).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
