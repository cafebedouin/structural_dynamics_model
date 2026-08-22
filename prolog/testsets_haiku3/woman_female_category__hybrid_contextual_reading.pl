% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Woman/Female Category Membership (Hybrid Contextual Reading)
 *   domain: political/bioethical/legal
 *
 * SUMMARY:
 *   The woman/female category exists at the intersection of medical,
 *   biological, social, and legal domains where the meaning of the term is
 *   contested. The hybrid contextual reading proposes that category
 *   membership should be context-dependent: biological sex for medical
 *   protocols and sports fairness standards (where institutional actors claim
 *   evidence-based necessity), and gender identity for social recognition,
 *   legal documentation, and civil rights (where institutional actors claim
 *   dignity necessity). This reading is one of three structurally distinct
 *   interpretations of the same kernel: the sex-biology reading argues for
 *   uniform sex-based classification; the gender-identity reading argues for
 *   uniform identity-based classification. The hybrid reading navigates the
 *   institutional claim that both criteria serve legitimate,
 *   non-interchangeable functions. The constraint operates through
 *   institutional authority (medical boards, sports governing bodies, legal
 *   systems) that enforces context-dependent rules; the extraction arises
 *   because institutional actors benefit from the ambiguity itself (avoiding
 *   commitment while maintaining authority), and because the rule creates
 *   systematic costs for trans individuals and cis women in contexts where
 *   their criterion is subordinated.
 *
 * KEY AGENTS:
 *   - institutional_conflict_minimizers: Institutional actors (government, healthcare, sports bodies) that set and enforce the hybrid contextual rules; benefit from stability and reduced litigation.
 *   - trans_women: Experience costs when classified by sex in medical/sports contexts despite gender identity recognition elsewhere; identity-locked exit prevents leaving the category system.
 *   - cis_women_in_contested_contexts: Face costs when rules shift or become contested; benefit in medical/biological domains but may face costs in sports depending on inclusion policies.
 *   - trans_men: Systematically invisible under the hybrid model; classified by sex in medical contexts despite legal/social identity recognition elsewhere.
 *   - medical_sports_regulatory_bodies: Institutional beneficiaries; justify context-specific rules as evidence-based.
 *   - civil_rights_advocates_identity_centered: Excluded from medical/sports rule-setting; would object to the sex-based classification in those domains.
 *   - sex_biology_advocates: Excluded from social/legal rule-setting; would object to the identity-based classification in those domains.
 *   - policymakers_seeking_compromise: Analytical seats measuring whether the framework reduces or merely displaces institutional conflict.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.58).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.62).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Woman/Female Category Membership (Hybrid Contextual Reading)").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political/bioethical/legal").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, 'e15512d9-fc9d-4543-82ed-b6f3bc22565a').
narrative_ontology:cs_kernel_codification('e15512d9-fc9d-4543-82ed-b6f3bc22565a', distributed).
narrative_ontology:cs_authority_grounding('e15512d9-fc9d-4543-82ed-b6f3bc22565a', extraction).
narrative_ontology:cs_interpretation_layer_present('e15512d9-fc9d-4543-82ed-b6f3bc22565a').
narrative_ontology:cs_reading_relation('e15512d9-fc9d-4543-82ed-b6f3bc22565a', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('e15512d9-fc9d-4543-82ed-b6f3bc22565a', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('e15512d9-fc9d-4543-82ed-b6f3bc22565a', foundational, legitimate_context_specificity).
narrative_ontology:cs_axiom_status(legitimate_context_specificity, holdable).
narrative_ontology:cs_axiom_grounding('e15512d9-fc9d-4543-82ed-b6f3bc22565a', legitimate_context_specificity, instrumental).
narrative_ontology:cs_axiom('e15512d9-fc9d-4543-82ed-b6f3bc22565a', secondary, institutional_authority_preservation).
narrative_ontology:cs_axiom_status(institutional_authority_preservation, holdable).
narrative_ontology:cs_axiom_grounding('e15512d9-fc9d-4543-82ed-b6f3bc22565a', institutional_authority_preservation, conventional).
narrative_ontology:cs_reference_frame('e15512d9-fc9d-4543-82ed-b6f3bc22565a', institutional_pragmatism_equilibrium).
narrative_ontology:cs_drift_state('e15512d9-fc9d-4543-82ed-b6f3bc22565a', contemporary_litigation_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e15512d9-fc9d-4543-82ed-b6f3bc22565a', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimizers).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, medical_sports_regulatory_bodies).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cis_women_in_contested_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_men).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, trans_women).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, cis_women_in_contested_contexts).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, institutional_pragmatism_doctrine).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, harm_minimization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government agencies, educational institutions, healthcare systems, and sports bodies that adopt context-dependent category rules. They set the rules for which contexts use which criterion (sex for medical records, gender identity for legal recognition) and defend the framework as pragmatic conflict management. Benefit from reduced litigation and institutional stability.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimizers, agenda_setter,
    institutional, generational, mobile, national).

% Bear asymmetric costs across contexts: recognized as women in social/legal domains (benefit to identity alignment) but classified by biological sex in medical/sports contexts (cost of institutional categorization that conflicts with self-identity). Cannot exit the gender category system entirely. Exit from specific contexts (sports, medical) available but carries costs (healthcare access, community participation).
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, trans_women, beneficiary).

% In medical/safety contexts, benefit from category clarity (biological-sex-based medical protocols are their recognized basis for belonging). In sports, some benefit from sex-based classification (competitive fairness standards) and some pay costs (losing sport or resources to trans athletes if trans women are included under gender identity). Costs arise when institutional rules shift between contexts or become contested.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cis_women_in_contested_contexts, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, cis_women_in_contested_contexts, beneficiary).

% Bear structural invisibility costs under the hybrid model: classified as women in medical contexts (based on sex) despite gender identity not aligning, but legally recognized as men in other domains. Trapped between systems that do not acknowledge their boundary-crossing status systematically. Medical access complications; legal documentation mismatches.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_men, payer,
    moderate, biographical, identity_locked, national).

% Justify context-specific rules as evidence-based (biological sex for medical protocols, for sports fairness). Benefit from rule stability that lets them operate with administrative efficiency and reduced challenge. Avoid having to commit to a single definition of woman/female across all functions.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, medical_sports_regulatory_bodies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, medical_sports_regulatory_bodies, agenda_setter).

% Are excluded from institutional rule-setting in medical/sports contexts where the hybrid model applies sex rather than identity. Would argue that gender identity should supersede in all contexts for consistency and dignity. Their exclusion from medical/sports policy-making is a feature of the institutional gatekeeping itself.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, civil_rights_advocates_identity_centered, excluded,
    organized, biographical, constrained, national).

% Are excluded from legal/social recognition policy domains where the hybrid model applies identity. Would argue that biological sex should be the single criterion across all contexts. Their exclusion from social recognition policy-making is maintained by institutional actors who favor context-separation.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_biology_advocates, excluded,
    organized, biographical, constrained, national).

% Observe the institutional actors implementing hybrid rules and measure outcomes. They evaluate whether the framework actually reduces conflict or merely displaces it. Conduct empirical assessment of institutional stability and litigation trends.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, policymakers_seeking_compromise, observer,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimizers).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates medical treatment protocols, sports fairness standards, and legal recognition across multiple institutional domains by allowing each domain to use the criterion (sex or identity) it claims is evidence-based for its function. Solves the institutional problem of harmonizing these domains without forcing a single global definition that would alienate major constituencies.
% TRANSFER_FUNCTION: Transfers legitimacy and institutional stability from the paying groups (trans women, trans men, and cis women in contested contexts) to the institutional actors who set rules. The constraint moves decision-making authority upward to institutions and away from individuals' self-determination in some contexts; moves it toward self-determination in others.
% ABSENT_VOICES: Sex-biology-centered civil rights advocates are largely excluded from legal/social recognition policy; identity-centered civil rights advocates are largely excluded from medical/sports policy. Both groups would object to the domain separation itself if they had equal standing in all rule-setting forums. Their exclusion varies by institutional domain.
% DISAPPEARANCE_RATIONALE: If the hybrid framework vanished, institutions would be forced to commit to a single criterion across all functions (either sex or identity), triggering immediate policy reorganization in medical records, sports eligibility, legal documentation, and social recognition systems. Litigation would spike in the transition period; institutional authority structures would shift as different epistemic communities (biologists, psychologists, civil rights lawyers) competed to define the authoritative criterion.
% FOUNDING_PROBLEM: Medical and sports institutions needed clear, administratively usable criteria for category membership that would support evidence-based protocols. Simultaneously, social/legal institutions needed to recognize individuals' self-identification for dignity and equal participation. The single-criterion approaches (sex-only or identity-only) were seen as solving one problem while creating acute harm in the other domain.
% FOUNDING_PROBLEM_CORROBORATION: Medical and sports bodies attest the founding problem is live: they claim biological sex is evidence-based and operationally necessary. Legal/social recognition advocates and trans groups attest the identity-recognition problem is live. Independent research from outside the institutional rule-making bodies (public health surveys, international law reviews, medical ethics analyses) documents that both problems existed and that the hybrid model was adopted as institutional compromise, not as evidence-based certainty about what woman/female means.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the hybrid framework does coordinate legitimate institutional functions (medical protocols, sports fairness) while simultaneously benefiting institutional actors through ambiguity — they maintain rule-setting authority without forcing a commitment that would alienate major constituencies. Suppression is moderately high (0.62) because the constraint requires active institutional enforcement to maintain the context-separation and to exclude advocates from rule-making in domains where they disagree. Theater ratio is high (0.48) because significant institutional energy goes into defending the framework as evidence-based when the evidence for context-dependence itself is contested — the framework's appearance of pragmatic neutrality requires ongoing performance of institutional expertise. Accessibility of alternatives is moderate (0.45) because both sibling readings remain live as advocacy positions and regulatory alternatives (some jurisdictions adopt sex-only, others identity-only, creating exit options for dissatisfied actors), but institutional gatekeeping prevents easy switching. Resistance is high (0.71) because trans groups, gender-identity advocates, sex-biology advocates, and cis women in contested contexts all mount substantial resistance to elements of the framework. The measurement series show suppression requirement increasing over time (0.58→0.62) as institutional actors must work harder to maintain the domain-separation boundary against advocacy pressure; theater_ratio peaks at midpoint (t=15, value 0.50) as the institutional defense of the framework becomes most intense, then stabilizes as the framework settles into routine. This is one coherent story of the hybrid reading at the level of an institutional constraint; the sibling readings are different constraints with different ε values and different victim sets.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the trans-women seat compute fundamentally different constraint types from the same structural data. The institutional actor sees coordination (legitimate domain-specific rules); the trans woman sees extraction (institutional authority selectively applied by domain). Neither assessment is false — they reflect different structural positions relative to the constraint. The engine captures this divergence via directionality: high d for the trans-women seat (target of institutional categorization) produces higher computed extractiveness; low d for the institutional actor (beneficiary of rule-setting authority) produces lower computed extractiveness. The authored claim (tangled_rope) reflects the structure: real coordination function exists (domain-specific protocols) AND asymmetric extraction (institutional authority benefits from ambiguity, some groups pay for context-dependence).
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and trans men are identity-locked (cannot exit the gender system and face costs in medical domains where sex is the criterion); cis women in contested contexts are constrained (can shift domains but face athletic/resource costs in some). All three occupy the target end of directionality (d near 1.0) in the domains where their reading is subordinated. Institutional actors occupy the beneficiary end (d near 0.0) because they set the rules, collect the authority to define categories, and benefit from the framework's ambiguity which lets them avoid accountability for either criterion. The civil-rights advocates (both identity-centered and sex-centered) are excluded from rule-making but would be targets if they were seated, because the framework subordinates their reading in specific domains. The directionality derivation from the stakeholder situation descriptions yields high d for trans groups and high d for advocates excluded from rule-making, low d for institutional agenda-setters.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid model avoids mandatrophy (the constraint's founding purpose persisting long after its use has degraded) by design: if either the medical/sports function or the social/legal function becomes irrelevant, the framework would simply collapse to the remaining function. However, the framework itself is threatened by a different form of institutional decay: if institutional actors ever genuinely commit to one criterion across all domains (forced by litigation, legislation, or political realignment), the hybrid structure collapses and the ambiguity that benefits the institutional actors vanishes. The measurement showing stable theater_ratio (0.48) despite rising suppression requirement suggests the framework is not yet degrading into pure performance — institutional actors still claim evidence-based necessity for domain-separation. If theater_ratio were to rise above 0.65 while suppression requirement stayed high, that would signal the framework is persisting by performance rather than legitimate function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_dependence_legitimacy,
    'Are the claimed functional distinctions between domains (biological sex for medical protocols, identity for social recognition) evidence-based, or are they institutional constructs that happen to be useful for conflict avoidance?',
    'Comparative institutional analysis: do medical protocols actually require biological sex, or could they adapt to identity-based classification with appropriate protocol adjustments? Do sports fairness standards empirically require sex-based classification, or are they defaults from institutional inertia? Can identity-based legal recognition produce equivalent medical outcomes with protocol adaptation?',
    'If the functional distinctions are genuine, the hybrid model is a defensible coordination solution. If they are institutional constructs, the hybrid model is pure extraction masked as pragmatism — the institutional actors benefit from maintaining both systems without committing to either.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(context_dependence_legitimacy, empirical, 'Whether context-specific criteria are evidence-based or institutionally convenient.').

omega_variable(
    institutional_benefit_quantification,
    'What specific institutional benefit accrues to agenda-setters from maintaining the hybrid framework rather than committing to a single criterion? Is the benefit stability, reduced litigation, maintained authority, or cost avoidance?',
    'Institutional analysis: track litigation costs, administrative burden, and rule-change frequency under hybrid vs. pure frameworks across comparable jurisdictions. Measure whether institutions that adopted single-criterion models experienced greater or lesser institutional stability.',
    'If institutions benefit primarily from reduced litigation and stability, the framework is extraction-light and more rope-like. If they benefit from maintaining decision-making authority and ambiguity itself, the framework is extraction-heavy and more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_quantification, empirical, 'What institutional actors gain from preserving the hybrid framework.').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For trans individuals, is the identity-lock to the gender category system structural (external legal/institutional barriers) or internalized (self-concept fusion with gender identity that persists independent of institutional barriers)?',
    'Post-exit trajectory analysis: if a trans individual exits institutional engagement (doesn''t seek legal recognition, doesn''t pursue medical transition, withdraws from social systems enforcing gender categories), does the experienced lock persist? If so, suppression is partly internalized.',
    'If internalized, the measured suppression (0.62) understates the effective constraint on trans individuals because they carry the lock with them even when institutional barriers are removed. The constraint''s effective suppression in the trans-women and trans-men seats is higher than the institutional measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether identity-lock is structural (institutional) or internalized (psychological/relational).').

omega_variable(
    sibling_reading_foreclosure_test,
    'Is the hybrid reading logically tenable alongside the pure sex-biology and pure identity readings, or does commitment to one reading''s core premise logically foreclose the others?',
    'Logical analysis: the hybrid reading asserts that BOTH sex and identity ground legitimate category membership in different domains. Does accepting this simultaneously require rejecting the pure readings'' assertions that ONLY one criterion grounds legitimate membership? Or can both readings coexist as competing institutional positions without internal logical contradiction?',
    'If logically foreclosed: the three readings are genuine alternatives and the constraint-family is a choice-set. If coexistent: the three readings coexist as live institutional positions and the kernel is genuinely contested without resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Logical compatibility of the hybrid reading with its sibling readings.').

omega_variable(
    victim_set_migration_by_context,
    'Does the victim set under the hybrid reading genuinely shift by context, or is there a constant victim set across contexts with different harm modalities?',
    'Contextual analysis: in medical contexts, who is harmed by the sex-based classification? In sports contexts, who is harmed by the sex-based classification? In social/legal contexts, who would be harmed if sex replaced identity? In each context, identify the specific agents who lose structural position.',
    'If victim set genuinely shifts (trans women lose in medicine, cis women lose in some sports, trans men lose in medicine, gender-identity advocates lose in sports rule-setting), the constraint is truly context-dependent and the extraction is distributed unevenly. If a single group (trans people, or cis women) bears costs in all contexts regardless of the criterion applied, the constraint is simpler than the hybrid model suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_migration_by_context, empirical, 'Whether victim status varies meaningfully by institutional domain or is constant across domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__hybrid_contextual_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement_basis(woma_tr_t5, observed).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__hybrid_contextual_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement_basis(woma_tr_t10, observed).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__hybrid_contextual_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(woma_tr_t15, observed).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(woma_tr_t20, observed).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__hybrid_contextual_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(woma_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t5, woman_female_category__hybrid_contextual_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(woma_be_t5, observed).
narrative_ontology:measurement(woma_be_t10, woman_female_category__hybrid_contextual_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(woma_be_t10, observed).
narrative_ontology:measurement(woma_be_t15, woman_female_category__hybrid_contextual_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(woma_be_t15, observed).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(woma_be_t20, observed).
narrative_ontology:measurement(woma_be_t25, woman_female_category__hybrid_contextual_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(woma_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t5, woman_female_category__hybrid_contextual_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(woma_su_t5, observed).
narrative_ontology:measurement(woma_su_t10, woman_female_category__hybrid_contextual_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(woma_su_t10, observed).
narrative_ontology:measurement(woma_su_t15, woman_female_category__hybrid_contextual_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement_basis(woma_su_t15, observed).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(woma_su_t20, observed).
narrative_ontology:measurement(woma_su_t25, woman_female_category__hybrid_contextual_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(woma_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% The woman/female category kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the same foundational commitment. The sex_biology_reading treats woman/female as biologically defined (XX/XY, reproductive anatomy). The gender_identity_reading treats woman/female as internally self-identified. The hybrid_contextual_reading (this constraint) treats woman/female as context-dependent. The three readings share the same referent (the institutional category) but disagree on the criterion that grounds membership. The hybrid reading influences both siblings by suggesting a middle path, but does not foreclose them — all three remain live institutional positions across different jurisdictions and policy domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, institutional, 0.15).
constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
