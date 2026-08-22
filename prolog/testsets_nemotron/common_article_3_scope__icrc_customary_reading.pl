% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: CA3 Scope: ICRC Customary Law Reading
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story captures the ICRC's customary international law
 *   reading of Common Article 3 scope — the interpretive position that CA3's
 *   field of application is determined not by a fixed textual threshold but
 *   by the evolving convergence of state practice and opinio juris as tracked
 *   and restated by the ICRC. The reading functions as a procedural
 *   coordination mechanism: it provides a shared methodology for gradual
 *   scope expansion without requiring formal treaty amendment, allowing the
 *   law to adapt to new conflict forms (NIACs, transnational conflicts,
 *   non-state armed groups) while maintaining the veneer of state consent.
 *   The ICRC's unique institutional role — guardian of the Conventions,
 *   operator of the customary law database, adviser to states — gives its
 *   restatements disproportionate influence. The constraint is claimed as a
 *   rope: a genuine coordination mechanism solving the problem of IHL
 *   adaptation in a treaty system that is practically unamendable. The
 *   metrics reflect low but non-zero extraction (the ICRC's institutional
 *   authority is reinforced), minimal suppression (states can and do
 *   dissent), and low theater (the methodology is genuinely used).
 *
 * KEY AGENTS:
 *   - icrc_institution: Primary agenda_setter (guardian of Conventions, customary law database, state adviser) — institutional/analytical — shapes the constraint's content
 *   - state_legal_advisors: Primary beneficiaries (get authoritative interpretive guidance without legislative burden) — organized/moderate — constrained exit (rejection carries diplomatic cost)
 *   - humanitarian_organizations: Beneficiaries (operational clarity for protection activities) — organized/moderate — mobile exit
 *   - state_centric_states: Excluded/payer (states preferring fixed thresholds find their position marginalized) — powerful/institutional — constrained exit
 *   - expansive_rights_advocates: Excluded (reading doesn't go far enough substantively) — organized/moderate — mobile exit
 *   - international_tribunals: Observer/agenda_setter (apply and thereby legitimize the methodology) — institutional/analytical — analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.18).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.12).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "CA3 Scope: ICRC Customary Law Reading").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '50e97217-9936-4ec5-a98d-def64648234e').
narrative_ontology:cs_kernel_codification('50e97217-9936-4ec5-a98d-def64648234e', fixed_text).
narrative_ontology:cs_authority_grounding('50e97217-9936-4ec5-a98d-def64648234e', lineage).
narrative_ontology:cs_interpretation_layer_present('50e97217-9936-4ec5-a98d-def64648234e').
narrative_ontology:cs_reading_relation('50e97217-9936-4ec5-a98d-def64648234e', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('50e97217-9936-4ec5-a98d-def64648234e', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('50e97217-9936-4ec5-a98d-def64648234e', foundational, customary_law_methodology_authoritative).
narrative_ontology:cs_axiom_status(customary_law_methodology_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('50e97217-9936-4ec5-a98d-def64648234e', customary_law_methodology_authoritative, conventional).
narrative_ontology:cs_axiom('50e97217-9936-4ec5-a98d-def64648234e', foundational, icrc_restatements_presumptive_weight).
narrative_ontology:cs_axiom_status(icrc_restatements_presumptive_weight, holdable).
narrative_ontology:cs_axiom_grounding('50e97217-9936-4ec5-a98d-def64648234e', icrc_restatements_presumptive_weight, conventional).
narrative_ontology:cs_reference_frame('50e97217-9936-4ec5-a98d-def64648234e', id_1949_geneva_conventions_textual_baseline).
narrative_ontology:cs_drift_state('50e97217-9936-4ec5-a98d-def64648234e', post_2005_customary_law_study, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50e97217-9936-4ec5-a98d-def64648234e', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_institution).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, state_legal_advisors).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, state_centric_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ICRC acts as guardian of the Geneva Conventions, maintains the customary IHL database, and advises states on IHL implementation. Its customary law restatements shape the interpretive baseline for CA3 scope. The institution's epistemic authority in IHL is reinforced each time states, tribunals, or militaries cite its studies rather than conducting independent customary law analysis. It does not collect monetary rents but accumulates institutional capital and agenda-setting power.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc_institution, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, icrc_institution, beneficiary).

% Government legal advisers rely on ICRC customary law studies as authoritative guidance for military operations, legislation, and diplomatic positions. Building independent customary law analysis capacity is resource-intensive and diplomatically risky — rejecting ICRC guidance without a credible alternative exposes the state to criticism. They benefit from legal certainty but are constrained in their ability to exit the ICRC's interpretive framework.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, state_legal_advisors, beneficiary,
    organized, biographical, constrained, national).

% NGOs and UN agencies use ICRC customary law restatements as operational guidance for protection activities in conflict zones. They have more exit freedom than states — they can develop independent legal positions or cite alternative sources — but the ICRC's database remains the de facto reference standard in the humanitarian sector.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, humanitarian_organizations, beneficiary,
    organized, biographical, mobile, global).

% States preferring a fixed, narrow CA3 scope (e.g., US, Israel, Russia at various times) bear the cost of incremental scope expansion through customary law. Their preferred reading is structurally marginalized because the ICRC's institutional position gives its restatements presumptive weight. Rejecting the entire customary law methodology carries higher diplomatic and operational costs than accepting marginal expansions they disagree with.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, state_centric_states, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, state_centric_states, excluded).

% Human rights NGOs and advocates arguing for CA3 as a universal floor for any organized violence find this reading procedurally compatible but substantively insufficient. The customary law methodology requires state practice/opinio juris convergence, which is slower and more state-centric than their preferred human rights-based approach. They are not suppressed — they can and do advocate for broader readings — but the constraint's procedural structure does not deliver their substantive goal.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, expansive_rights_advocates, excluded,
    organized, biographical, mobile, global).

% ICTY, ICC, and other tribunals cite ICRC customary law studies when determining CA3 applicability, thereby legitimizing the methodology. They do not control the methodology but their judicial application gives it authoritative weight. They sit at the analytical end of directionality — they observe and apply the constraint but are not structurally subject to its extraction or suppression.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_tribunals, observer,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, international_tribunals, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, authoritative methodology for determining CA3 scope through state practice and opinio juris convergence, allowing IHL to adapt to new conflict forms without the practical impossibility of amending the 1949 Geneva Conventions.
% TRANSFER_FUNCTION: Transfers interpretive authority and institutional capital from states (who would otherwise need independent customary law capacity) to the ICRC, which maintains the database and produces restatements. States save analytical resources; the ICRC gains reinforced epistemic authority.
% ABSENT_VOICES: States that reject the customary law methodology entirely (rare, usually implicit), non-state armed groups who are subject to CA3 but have no voice in customary law formation, and populations in conflict zones whose protection turns on which reading prevails — they are the ultimate excluded, with no seat at the interpretive table.
% DISAPPEARANCE_RATIONALE: If the ICRC customary law methodology vanished, states would lose their shared interpretive baseline for CA3 scope. Some would revert to textualist narrow readings; others would push expansive human rights readings; tribunals would lose a common reference point. The coordination function would collapse into fragmentation, and the Geneva Conventions' unamendability would become a practical crisis for new conflict forms.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions created CA3 as a minimum standard for non-international armed conflicts but left 'armed conflict not of an international character' undefined. The Conventions are practically unamendable (requires universal ratification), yet new conflict forms (transnational NIACs, conflicts with non-state armed groups, 'war on terror' classifications) kept emerging. States needed a way to determine CA3 applicability without treaty amendment.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC attests the problem is live (2016 Commentary on GC I-III, 2020 Challenges Report). States confirm it through continued reliance on customary law methodology in military manuals and diplomatic statements (e.g., 2019-2023 UNGA Sixth Committee debates). Scholars outside the ICRC (Sassòli, Melzer, Akande) corroborate that treaty unamendability makes customary law the primary adaptation mechanism. No major actor claims the adaptation problem is solved.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.18) reflects the ICRC's reinforced institutional authority: states rely on ICRC restatements because the alternative is independent customary law analysis, which is resource-intensive and diplomatically risky. The ICRC does not collect monetary rents, but it collects institutional capital — the constraint sustains its unique epistemic authority in IHL. Suppression (0.12) is low because states can and do reject specific ICRC customary law positions (e.g., several states rejected parts of the 2005 Customary Law Study), but rejection is episodic and issue-specific, not systemic. The theater ratio (0.08) is low because the methodology is genuinely operational — tribunals, militaries, and NGOs actually use the practice/opinio juris tracking framework. Accessibility collapse (0.25) is moderate: the customary law methodology is accessible to any actor willing to do the research, but the ICRC's database creates a de facto standard that is hard to displace. Resistance (0.35) reflects ongoing scholarly and state debate about whether customary law can expand CA3 scope beyond the treaty text's apparent limits.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC's seat, the constraint is pure coordination: a service to states providing legal certainty in an unamendable treaty system. From state_centric_states' seat, it is a slow ratchet: each customary law restatement expands scope incrementally, and the cost of rejecting the whole methodology exceeds the cost of accepting marginal expansions. From expansive_rights_advocates' seat, it is insufficient: the procedural constraint legitimizes a state-centric process that will never reach their substantive floor. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC is the structural agenda_setter and primary beneficiary (institutional authority reinforced, d near 0.0). State legal advisors are beneficiaries (reliable guidance, d ~ 0.2-0.3) but with constrained exit — rejecting ICRC guidance requires building independent customary law capacity. Humanitarian organizations are beneficiaries with mobile exit (they could develop independent guidance). State-centric states are payers/excluded: they bear the cost of scope expansion they oppose, and their preferred reading is structurally marginalized by the ICRC's institutional position. Expansive rights advocates are excluded: their substantive claims find no home in this procedural methodology. Tribunals are observers/secondary agenda_setters: they apply the methodology, legitimizing it, but do not control it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (IHL adaptation without treaty amendment) remains live — the Geneva Conventions have not been amended since 1949/1977, and new conflict forms keep emerging. The ICRC customary law methodology continues to solve a real coordination problem. However, the constraint shows early mandatrophy signals: the ICRC's institutional interest in maintaining its unique epistemic authority may exceed the coordination need. The 2005 Customary Law Study's expansive claims on NIAC scope, and the ICRC's 2015-2020 positions on transnational conflicts and non-state armed groups, go beyond what state practice clearly supports. This reading is not yet a piton — it still solves a live problem — but it risks becoming one if the ICRC's restatements consistently lead state practice rather than follow it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_objectivity,
    'Does the ICRC''s identification of customary rules reflect an objective convergence of state practice and opinio juris, or does it actively shape that convergence through its authoritative interpretive role?',
    'Comparative analysis of ICRC customary law studies against raw state practice records (diplomatic communications, military manuals, national legislation, UN voting patterns) over multiple decades to measure whether ICRC restatements precede or follow state convergence.',
    'If ICRC restatements precede convergence, the constraint operates as an active coordination mechanism that shapes the law it claims to track — making it more rope-like (coordination with institutional steering). If it purely follows, it is closer to a mountain (reflecting an external convergence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_objectivity, conceptual, 'Whether the ICRC''s customary law identification is descriptive or constitutive').

omega_variable(
    kernel_reading_structure,
    'How does the procedural constraint on interpretation in this reading structurally relate to the substantive scope claims of the sibling readings?',
    'Track whether states or tribunals citing the ICRC customary reading adopt its procedural methodology (practice/opinio juris tracking) while reaching different substantive conclusions than either the state-centric or expansive human rights readings.',
    'If the procedural methodology operates independently of substantive outcomes, this reading is a genuine coordination mechanism. If it consistently channels toward one substantive pole, it functions as a disguised version of that sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between this reading''s procedural constraint and sibling readings'' substantive claims').

omega_variable(
    state_buy_in_voluntariness,
    'Is state acceptance of ICRC customary law determinations truly voluntary (states independently converge) or is it extracted through the ICRC''s unique institutional position in IHL?',
    'Analyze instances where states explicitly rejected ICRC customary law positions vs. instances of silent acquiescence; measure whether rejection carries diplomatic/humanitarian costs that make dissent structurally costly.',
    'If dissent is costly, the constraint has suppressed extraction (snare/tangled_rope elements). If states freely converge or diverge, it remains a genuine coordination rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_buy_in_voluntariness, empirical, 'Whether state convergence on ICRC customary readings is voluntary or institutionally coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_icrc_customary_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.02).
narrative_ontology:measurement(ca3_icrc_customary_tr_t1977, common_article_3_scope__icrc_customary_reading, theater_ratio, 1977, 0.03).
narrative_ontology:measurement(ca3_icrc_customary_tr_t1995, common_article_3_scope__icrc_customary_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(ca3_icrc_customary_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.06).
narrative_ontology:measurement(ca3_icrc_customary_tr_t2016, common_article_3_scope__icrc_customary_reading, theater_ratio, 2016, 0.07).
narrative_ontology:measurement(ca3_icrc_customary_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(ca3_icrc_customary_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.05).
narrative_ontology:measurement(ca3_icrc_customary_be_t1977, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1977, 0.08).
narrative_ontology:measurement(ca3_icrc_customary_be_t1995, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(ca3_icrc_customary_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(ca3_icrc_customary_be_t2016, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2016, 0.18).
narrative_ontology:measurement(ca3_icrc_customary_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(ca3_icrc_customary_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.05).
narrative_ontology:measurement(ca3_icrc_customary_su_t1977, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1977, 0.07).
narrative_ontology:measurement(ca3_icrc_customary_su_t1995, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1995, 0.1).
narrative_ontology:measurement(ca3_icrc_customary_su_t2005, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2005, 0.11).
narrative_ontology:measurement(ca3_icrc_customary_su_t2016, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2016, 0.12).
narrative_ontology:measurement(ca3_icrc_customary_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.02).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, niac_threshold_customary_law).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, transnational_conflict_classification).

% DUAL FORMULATION NOTE:
% This reading decomposes the CA3 scope kernel into a procedural coordination mechanism (customary law methodology) distinct from the substantive scope claims of its siblings. The ICRC's methodology is the coordination infrastructure; the state-centric and expansive readings are substantive positions that use or contest this infrastructure. The ε values differ: this reading has low extractiveness (procedural coordination), while the expansive reading would show higher extractiveness (substantive imposition on states) and the state-centric reading would show higher suppression (exclusion of humanitarian protection claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, institutional, 0.05).
constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, organized, 0.25).
constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
