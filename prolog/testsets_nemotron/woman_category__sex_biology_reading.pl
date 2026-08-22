% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Sex-Biology Definition of Woman Category
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the sex-biology reading of the
 *   woman_category kernel. The reading defines 'woman' exclusively by
 *   chromosomal (XX), anatomical, and reproductive biology criteria. It
 *   operates as a tangled rope: it coordinates sex-based protections, data
 *   collection, and single-sex spaces (genuine coordination function) while
 *   simultaneously extracting from transgender women (excluded from
 *   protections) and ambiguously classifying intersex individuals (boundary
 *   instability as extraction mechanism). The constraint requires active
 *   enforcement — legal definitions, sports eligibility rules, prison
 *   placement policies, shelter access rules — to maintain the boundary.
 *   Extraction has risen over the interval as gender-identity-based
 *   frameworks have gained institutional traction, making the sex-biology
 *   boundary require more active defense. Theater ratio has risen as
 *   'protecting women's spaces' rhetoric increasingly covers exclusion that
 *   does not materially advance safety. The kernel_context field below
 *   records the committer-frame structure.
 *
 * KEY AGENTS:
 *   - female_biology_rights_advocates: Primary beneficiary (institutional/biographical) — collects the protective and resource allocation benefits of the sex-biology boundary
 *   - transgender_women: Primary victim (moderate/constrained) — excluded from sex-segregated protections, sports, prisons, shelters; bears the cost of the boundary
 *   - intersex_individuals_female_lived: Secondary victim (moderate/identity_locked) — ambiguously included/excluded depending on institutional implementation; boundary instability extracts compliance costs
 *   - sex_based_data_institutions: Beneficiary (institutional/generational) — statistical agencies, public health bodies, crime recording systems that rely on binary sex categories
 *   - women_sports_governing_bodies: Beneficiary/agenda_setter (organized/biographical) — use the boundary to justify sex-segregated competition categories
 *   - gender_identity_advocates: Excluded (powerful/biographical) — would argue for self-ID inclusion but are structurally excluded from this reading's framework
 *   - policy_analysts: Observer (analytical/civilizational) — sees the full constraint family across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.68).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.72).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biology Definition of Woman Category").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'ab269d5a-8112-4fa2-897b-b3be9bc51ec6').
narrative_ontology:cs_kernel_codification('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', formalized).
narrative_ontology:cs_authority_grounding('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', lineage).
narrative_ontology:cs_interpretation_layer_present('ab269d5a-8112-4fa2-897b-b3be9bc51ec6').
narrative_ontology:cs_reading_relation('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', foundational, woman_category_determined_by_gametes_and_anatomy).
narrative_ontology:cs_axiom_status(woman_category_determined_by_gametes_and_anatomy, holdable).
narrative_ontology:cs_axiom_grounding('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', woman_category_determined_by_gametes_and_anatomy, empirically_contingent).
narrative_ontology:cs_axiom('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', foundational, sex_based_protections_require_binary_category).
narrative_ontology:cs_axiom_status(sex_based_protections_require_binary_category, holdable).
narrative_ontology:cs_axiom_grounding('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', sex_based_protections_require_binary_category, conventional).
narrative_ontology:cs_reference_frame('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', binary_sex_class_protection_framework).
narrative_ontology:cs_drift_state('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', contemporary_gender_identity_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab269d5a-8112-4fa2-897b-b3be9bc51ec6', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, female_biology_rights_advocates).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_based_data_institutions).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, women_sports_governing_bodies).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_individuals_female_lived).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, intersex_individuals_female_lived).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_based_protection_framework).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, biological_sex_binary_in_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and advocates who campaign for sex-based rights and protections grounded in reproductive biology. They benefit from the constraint because it secures legal categories, funding streams, and policy frameworks for women-as-a-sex-class. Their exit is mobile — they could shift to gender-identity framing but would lose the specific protections the sex-biology boundary provides. They hold institutional power through legislative access, court precedent, and control of some women's organizations.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, female_biology_rights_advocates, beneficiary,
    institutional, generational, mobile, national).

% Transgender women who are excluded from sex-segregated spaces, sports categories, prison placements, and violence-against-women protections under the sex-biology reading. They bear the costs of the boundary: loss of safety, dignity, and access to resources. Their exit is constrained — they cannot change their biology, and legal gender recognition does not override the sex-biology boundary in this reading's framework. Some pursue litigation or policy advocacy; others accept exclusion. The directionality override reflects their high structural targeting despite moderate nominal power.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    moderate, biographical, constrained, national).

% Intersex individuals raised and living as women who face ambiguous classification under the sex-biology reading — included in some contexts (violence protections) but excluded in others (sports, some legal definitions). Their identity is fused with the 'woman' category as socially lived, making exit identity-locked: rejecting the category means rejecting their social self. They sometimes benefit from sex-based protections, sometimes pay the cost of boundary enforcement. The directionality override reflects their intermediate but structurally targeted position.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_individuals_female_lived, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, intersex_individuals_female_lived, beneficiary).

% Statistical agencies, public health systems, crime recording bodies that rely on binary sex categories for data collection and policy analysis. They benefit from the constraint because it provides a stable, operationalizable category. Their exit is arbitrage-grade — they could adopt gender-identity or multi-category systems (some already do in parallel), but the sex-biology reading remains the default for international comparability and historical continuity.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_based_data_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% International federations (World Athletics, FINA, UCI, etc.) and national Olympic committees that set eligibility rules for women's sport. They administer the constraint by operationalizing the sex-biology boundary (testosterone thresholds, chromosome testing, puberty-based criteria). They are constrained because they face pressure from both sides — human rights bodies demanding inclusion, female athletes demanding exclusion of male-puberty advantage — and cannot easily exit the governance role.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, women_sports_governing_bodies, agenda_setter,
    organized, biographical, constrained, global).

% Advocacy organizations and legal actors pushing for gender-identity-based category membership. They are structurally excluded from this reading's framework — their preferred definition is not a live option within the sex-biology reading. They are trapped because the constraint domain (legal/policy definition of woman) directly governs their constituents' lives, and they cannot opt out of its effects. They contest the constraint externally through litigation, legislation, and institutional pressure.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_advocates, excluded,
    powerful, biographical, trapped, national).

% Researchers, legal scholars, bioethicists who study the constraint family across readings. They neither collect nor pay; they map the structural relationships, track the kernel contest, and analyze classification outcomes. Their analytical exit means they can shift frameworks without personal cost.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, policy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates sex-based protections and resources for a class defined by reproductive vulnerability: violence-against-women data collection, single-sex spaces (shelters, prisons, changing rooms), sports fairness categories, pregnancy/maternity rights, and historical redress for sex discrimination. The binary sex category solves the collective-action problem of identifying the beneficiary class for these protections without individualized assessment.
% TRANSFER_FUNCTION: Moves access to sex-segregated protections, spaces, and resources from transgender women and ambiguously-classified intersex individuals to female-biology-rights beneficiaries. The transfer is not monetary but positional: inclusion/exclusion from legal categories, competitive divisions, institutional safeguards. The sex-biology boundary acts as the gate; those on the female-biology side collect the goods, those on the other side are excluded.
% ABSENT_VOICES: Transgender women and intersex individuals with female-lived experience are the primary absent voices — they are the ones most directly affected by the boundary but have no structural voice within the sex-biology reading's framework. Gender-identity advocates are excluded from the reading's internal logic. In policy venues where the reading is contested, their voices enter as external pressure, not as seated participants.
% DISAPPEARANCE_RATIONALE: If the sex-biology definition vanished overnight, legal frameworks for violence-against-women protections, sports categories, prison placement, shelter access, and anti-discrimination law would lose their operational category. Legislatures and courts would have to adopt a replacement definition (gender identity, gamete-based, cluster concept, or multi-category) — the world would rearrange around a new coordination boundary. The coordination goods (protections, fairness, data) would not disappear but would be recategorized.
% FOUNDING_PROBLEM: Coordinating legal protections and resource allocation for the class of people vulnerable to reproductive exploitation, pregnancy discrimination, and male-pattern violence — historically identified by the binary sex category 'woman' as a proxy for this vulnerability cluster.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by female_biology_rights_advocates (beneficiaries) AND by independent epidemiological data on violence against women, maternal mortality, and pregnancy discrimination (outside the beneficiary set). However, gender_identity_advocates and some bioethicists contest whether the sex-biology proxy still tracks the vulnerability cluster given medical transition and changing social conditions — they argue the founding problem is real but the proxy has drifted. Corroboration is thus partial and contested.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects that the constraint coordinates genuine collective action problems (violence-against-women data, sports fairness, single-sex spaces) while concentrating costs on a defined victim set (transgender women, ambiguously intersex people). Suppression (0.72) is high because the boundary requires active legal and institutional enforcement against self-ID challenges and intersex boundary cases. Theater ratio (0.38) has risen as the 'biological reality' framing increasingly serves to legitimize exclusion that does not functionally protect the coordination goods. Accessibility collapse (0.75) is high because once the sex-biology boundary is accepted as the legitimating framework, alternative categorization schemes (gender identity, gamete-based, cluster concepts) are treated as conceptually incoherent within the framework. Resistance (0.58) is substantial — legal challenges, policy advocacy, and institutional pushback from gender-identity frameworks — but the constraint persists because its beneficiaries control key agenda-setting institutions (sports governance, statistical agencies, some legislatures).
 *
 * PERSPECTIVAL GAP:
 *   From the female_biology_rights_advocates seat: the constraint is a rope — it solves the coordination problem of protecting a biologically defined class. From the transgender_women seat: it is a snare — the coordination story covers exclusion from protections they need. From the intersex_individuals_female_lived seat: it is an unstable tangled rope — sometimes coordinated, sometimes extracted from, depending on which institutional gate they encounter. From the policy_analysts seat: it is one reading in a constraint family where the kernel's referent (who counts as a woman for law/policy) is contested. The engine computes per-seat types from the structural data; this commentary explains the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: female_biology_rights_advocates (collect protections/resources), sex_based_data_institutions (collect clean binary data), women_sports_governing_bodies (collect fair-competition framework). Victims declared: transgender_women (excluded from all sex-segregated goods), intersex_individuals_female_lived (boundary ambiguity imposes compliance costs and arbitrary exclusion). The agenda_setter role is shared between women_sports_governing_bodies (operationalize the boundary in competition) and legislatures/courts (codify it in law). Directionality derivation: beneficiaries → low d (subsidized), victims → high d (targeted), excluded → trapped (no exit from the constraint's domain), observers → analytical. The intersex boundary ambiguity means some intersex individuals experience higher d than others depending on institutional implementation — this is captured by the omega variable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (see six_questions) was coordinating protections for a class defined by reproductive vulnerability. That problem remains live (violence against women, pregnancy discrimination, sports fairness). However, the constraint's mandate has expanded beyond the founding problem: it now governs access to spaces and resources where reproductive biology is not the relevant variable (e.g., domestic violence shelters, prison placement, bathroom access). The extraction from transgender women and intersex individuals is not mandated by the founding problem — it is a boundary-maintenance cost that has become a function. This is classic mandatrophy: the coordination function persists but has accumulated extractive barnacles. The constraint is not resolved mandatrophy because the core coordination problem is still live and the constraint still solves it — but the extraction/exclusion margin has widened beyond what the founding problem justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the woman category a single contested kernel with multiple readings, or are these structurally distinct constraints?',
    'Compare the structural relationships (beneficiaries, victims, enforcement) across the three declared readings. If the same institutional arrangements shift their victim/beneficiary assignments depending on the reading, it is one kernel with multiple readings. If the arrangements themselves differ, they are distinct constraints.',
    'If one kernel: this reading''s extraction is evaluated against a shared referent with sibling readings, and cs_structure fields capture the reading relations. If distinct constraints: each stands alone and network.affects_constraints links them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the three definitions constitute readings of one kernel or separate constraints.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to self-ID, exclusion from sex-segregated spaces) or internalized (transgender women and intersex individuals accepting exclusion as legitimate)?',
    'Post-policy-change trajectory: if suppression persists after legal barriers are removed (e.g., self-ID laws enacted), reclassify as partially internalized. Survey data on whether excluded groups experience the constraint as external or self-imposed.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after legal exit options open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/institutional constraints.').

omega_variable(
    intersex_boundary_ambiguity,
    'Does this reading''s boundary around ''female biology'' structurally include or exclude intersex individuals with female-typical anatomy but non-XX chromosomes?',
    'Legal and policy analysis of how sex-biology definitions are operationalized in practice — e.g., sports eligibility rules, prison placement, violence-against-women shelter access. If intersex individuals are inconsistently classified, the boundary is ambiguous.',
    'If intersex individuals are excluded, the victim set expands and ε increases. If included ambiguously, the constraint''s coordination function is unstable — the boundary itself becomes an extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_boundary_ambiguity, empirical, 'Whether the sex-biology reading''s boundary cleanly resolves intersex cases or produces ambiguous/inconsistent classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(woma_tr_t5, woman_category__sex_biology_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(woma_tr_t10, woman_category__sex_biology_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(woma_tr_t15, woman_category__sex_biology_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(woma_tr_t25, woman_category__sex_biology_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(woma_be_t5, woman_category__sex_biology_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(woma_be_t10, woman_category__sex_biology_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(woma_be_t15, woman_category__sex_biology_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(woma_be_t25, woman_category__sex_biology_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(woma_su_t5, woman_category__sex_biology_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(woma_su_t10, woman_category__sex_biology_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(woma_su_t15, woman_category__sex_biology_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(woma_su_t25, woman_category__sex_biology_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__sex_biology_reading, 0.08).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, sex_segregated_sports_eligibility).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, violence_against_women_data_collection).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, single_sex_space_access_policy).

% DUAL FORMULATION NOTE:
% This constraint family (woman_category kernel) decomposes the colloquial 'definition of woman' into three structurally distinct readings with different beneficiary/victim structures and extraction profiles. The sex_biology_reading coordinates sex-based protections but extracts from transgender women and ambiguously classifies intersex individuals. The gender_identity_reading coordinates inclusion but extracts from female-biology advocates who lose sex-based data and spaces. The intersex_accommodation_reading coordinates biological complexity but extracts from binary legal systems that must accommodate spectrum categories. Each reading instantiates a different constraint with its own ε. They are linked via affects_constraints because they contest the same institutional referents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__sex_biology_reading, moderate, 0.85).
constraint_indexing:directionality_override(woman_category__sex_biology_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
