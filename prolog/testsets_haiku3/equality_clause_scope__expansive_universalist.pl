% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause as Self-Evident Universal Right (Expansive Universalist Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The expansive universalist reading of the equality clause treats equality
 *   as a self-evident, self-executing universal principle that applies to all
 *   humans regardless of whether the historical drafters explicitly included
 *   them. Historical exclusions (of women, enslaved persons, indigenous
 *   peoples, racial minorities) are reframed not as binding constitutional
 *   limits but as hypocrisy and moral contradiction to be corrected through
 *   judicial interpretation. This reading vests interpretive authority in the
 *   progressive wing of constitutional jurisprudence and extends the scope of
 *   equality protections to groups not contemplated at ratification. The
 *   reading generates Tangled Rope structure: it coordinates the aspiration
 *   toward universal equality while extracting from those whose power and
 *   resources derived from the exclusionary settlement, and it requires
 *   active enforcement through judicial doctrine and institutional resistance
 *   to originalist alternatives.
 *
 * KEY AGENTS:
 *   - historically_excluded_groups (beneficiaries; organized/generational/constrained exit)
 *   - constitutional_interpreters_progressive_wing (agenda-setter; institutional/generational/arbitrage exit)
 *   - beneficiaries_of_historical_exclusion (payers; powerful/generational/constrained exit)
 *   - originalist_institutional_authority (payers; institutional/generational/mobile exit)
 *   - supreme_court_as_institution (agenda-setter; institutional/generational/analytical)
 *   - democratic_amendment_proponents (excluded; organized/generational/constrained exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.38).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.62).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause as Self-Evident Universal Right (Expansive Universalist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '968e0ac2-5b62-439a-91c2-ed094310211b').
narrative_ontology:cs_kernel_codification('968e0ac2-5b62-439a-91c2-ed094310211b', fixed_text).
narrative_ontology:cs_authority_grounding('968e0ac2-5b62-439a-91c2-ed094310211b', lineage).
narrative_ontology:cs_interpretation_layer_present('968e0ac2-5b62-439a-91c2-ed094310211b').
narrative_ontology:cs_reading_relation('968e0ac2-5b62-439a-91c2-ed094310211b', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('968e0ac2-5b62-439a-91c2-ed094310211b', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('968e0ac2-5b62-439a-91c2-ed094310211b', foundational, equality_is_self_evident).
narrative_ontology:cs_axiom_status(equality_is_self_evident, holdable).
narrative_ontology:cs_axiom_grounding('968e0ac2-5b62-439a-91c2-ed094310211b', equality_is_self_evident, deontological).
narrative_ontology:cs_axiom('968e0ac2-5b62-439a-91c2-ed094310211b', foundational, historical_exclusion_is_hypocrisy_not_binding).
narrative_ontology:cs_axiom_status(historical_exclusion_is_hypocrisy_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('968e0ac2-5b62-439a-91c2-ed094310211b', historical_exclusion_is_hypocrisy_not_binding, deontological).
narrative_ontology:cs_reference_frame('968e0ac2-5b62-439a-91c2-ed094310211b', self_evident_universal_equality).
narrative_ontology:cs_drift_state('968e0ac2-5b62-439a-91c2-ed094310211b', contemporary_post_civil_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('968e0ac2-5b62-439a-91c2-ed094310211b', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, constitutional_interpreters_progressive_wing).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, beneficiaries_of_historical_exclusion).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, originalist_institutional_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, moral_philosophers_universal_rights_tradition).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, state_governments_with_exclusionary_regimes).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, self_evidence_of_universal_human_equality).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, hypocrisy_of_exclusionary_historical_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women, enslaved persons, indigenous peoples, racial minorities, and other groups formally or functionally excluded from the original constitutional settlement find their claims to equality vindicated by this reading. The reading reframes historical exclusion as a moral contradiction to be corrected, not a binding limit on rights. Exit from this framing would require abandoning their own claims.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Progressive judges, legal scholars, and constitutional advocates who authoritatively interpret the equality clause as self-evident and universal in scope. They set the interpretive standard that equality admits an ever-wider circle of bearers and a thicker set of covered domains. They control the judicial apparatus that operationalizes this reading into binding constitutional law.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_interpreters_progressive_wing, agenda_setter,
    institutional, generational, arbitrage, national).

% Institutions and social structures built on the exclusionary original settlement—traditional gatekeepers, property holders whose wealth derived from exclusionary regimes, institutional hierarchies predicated on differential status. This reading frames their historical advantage as unjust extraction from the excluded, and calls for reallocation and structural reversal. They cannot exit without losing the structural privilege the exclusion secured.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, beneficiaries_of_historical_exclusion, payer,
    powerful, generational, constrained, national).

% Conservative judges and originalist scholars whose authority derives from treating historical constitutional meaning as binding. This reading undermines their interpretive framework by treating the historical boundary (who was meant to be covered) as a hypocrisy to overcome rather than a fixed textual limit. They can contest the reading through their own institutional channels but cannot prevent its expansion absent political/constitutional amendment.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_institutional_authority, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, originalist_institutional_authority, excluded).

% States whose statutory and administrative schemes embedded exclusionary hierarchies now reframed as violations of the self-evident equality principle. Compliance requires dismantling exclusionary laws and practices, incurring enforcement costs and redistributing power and resources. Exit would require formal secession or constitutional amendment.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, state_governments_with_exclusionary_regimes, payer,
    powerful, generational, constrained, regional).

% Those who argue equality rights should be expanded through formal democratic amendment rather than judicial reinterpretation. They are structurally excluded from this reading's legitimacy apparatus: this reading treats judicial expansion as valid even absent democratic endorsement, which forecloses the amendment-proponents' preferred path. They contest the reading through legislative and constitutional channels.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, democratic_amendment_proponents, excluded,
    organized, generational, constrained, national).

% The institutional seat that adjudicates competing readings and renders one binding through canonical constitutional interpretation. Under the expansive universalist reading, the Court acquires interpretive authority to deem new groups and domains covered by equality without textual amendment. This expands institutional power while committing the Court to ongoing controversy.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, supreme_court_as_institution, agenda_setter,
    institutional, generational, analytical, national).

% Academic and theoretical tradition asserting natural, universal, and inalienable human rights grounded in reason or dignity. This reading vindicates that tradition by treating equality as self-evident and expanding its scope against historical contingencies. They benefit from the reading's alignment with their theoretical framework but do not administer or enforce it.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, moral_philosophers_universal_rights_tradition, beneficiary,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, constitutional_interpreters_progressive_wing).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, canonical interpretive frame for what equality means: it applies to all humans, not merely to those historically included, and this frame coordinates legal doctrine, rights claims, and institutional enforcement around the principle of universal human dignity.
% TRANSFER_FUNCTION: Transfers authority to redefine equality's scope from historical/textual boundaries (which excluded certain groups) to interpretive frameworks that expand inclusion. Redistributes institutional power from originalist/conservative interpretive schools toward progressive constitutional jurisprudence. Redistributes social status and resources from beneficiaries of historical exclusion toward historically excluded groups.
% ABSENT_VOICES: Those committed to democratic amendment as the exclusive legitimate path for rights expansion are partially excluded: their objection is heard but treated as subsidiary to the reading's claim that self-evident universal truths do not require democratic ratification. Originalist scholars are formally present in legal debate but treated as defending an incoherent position (claiming universal equality while defending historical exclusion limits).
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the restrictive originalist reading became canonical, legal and social hierarchies based on historical exclusion would stabilize as constitutionally legitimate. Courts would cease recognizing new categories of equality claimants. Movements for LGBTQ+ rights, disability rights, immigrants' rights, and other post-ratification equality claims would lose their primary constitutional anchor. Social reorganization would follow rapidly.
% FOUNDING_PROBLEM: The original constitutional text claimed to establish a universal framework (We the People, all men are created equal in the Declaration) while systematically excluding women, enslaved persons, indigenous peoples, and others. This reading was constructed to resolve that foundational hypocrisy: not by amending the text but by treating the self-evident principle as primary and historical exclusion as a deviation to be corrected.
% FOUNDING_PROBLEM_CORROBORATION: Historians, legal scholars outside the originalist tradition, civil rights advocates, and social movements attest to the founding contradiction. Originalist scholars and conservative jurists contest it, arguing the text was internally coherent given its historical context and should not be reinterpreted against that context. Academic legal historiography (outside originalist schools) supports the contradiction claim; the dispute is whether self-evidence mandates correction or historical intent mandates fidelity.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at end) because the reading genuinely coordinates on a universal principle but does so at the cost of dismantling hierarchies and redistributing authority—the extraction target (beneficiaries of historical exclusion, originalist authority) experiences real loss, but the justification (hypocrisy correction) is structurally legitimate within the reading's own frame. Suppression is elevated (0.62) because maintaining this reading requires actively excluding originalist alternatives from canonical status and defending against textual/originalist challenges to the interpretation. Theater ratio rises over time (0.25→0.41) as the reading's practical enforcement increasingly involves rhetorical defense of expansion (equal protection coverage grows) and justification against the charge that judges are amending, not interpreting. The reading's self-evidence claim is performatively maintained against live contestation. Accessibility of the originalist alternative collapses (0.72) once the self-evident framing is adopted: rejecting self-evident equality is reframed as moral incoherence, not legitimate constitutional disagreement. Resistance remains substantial (0.58) because originalists and democratic-amendment proponents mount sustained institutional and theoretical challenges.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats experience the same constraint through incommensurable lenses. Beneficiaries see universal human dignity finally recognized; payers see illegitimate reallocation of authority and resources justified by an interpretive move (treating historical hypocrisy as correctable) that originalists reject as textually unfounded. The constraint's stability depends on the progressive wing maintaining institutional control of the Supreme Court and constitutional interpretation; loss of that control would shift the constraint's type sharply. The measurement series shows extractiveness rising initially (historical corrections pile up) then plateauing as the major categories (race, gender, disability) reach canonical status and further expansion meets increased originalist resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups are beneficiaries (d→0.0): the reading vindicates their claims without imposing costs on them. Progressive constitutional interpreters are the agenda-setter (d→0.0–0.2): they expand their interpretive authority, face no enforcement cost, and win on their preferred reading. Beneficiaries of historical exclusion are targets (d→0.8–1.0): they lose advantage and resources; exit is constrained (they cannot opt out of being citizens subject to equality law). Originalist institutional authority is a payer (d→0.7–0.9): their interpretive framework is delegitimized and excluded from canonical status; they retain formal presence in debate but are cast as defending incoherence. Democratic amendment proponents are partially excluded (d→0.4–0.6): they are not eliminated but their preferred mechanism (formal amendment) is treated as unnecessary/subordinate when self-evident truths are at stake. The reading itself produces directionality asymmetry by design: it claims self-evidence, which reverses the burden on skeptics.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exhibits mandatrophy symptoms: the founding problem (hypocrisy of exclusion) was addressed primarily through the 14th Amendment (1868) and subsequent explicit amendments and statutes (Civil Rights Act 1964, Voting Rights Act 1965, ADA 1990). The expansive universalist reading, by treating self-evidence as sufficient for interpretation, sidesteps the democratic amendment process and performs the expansion unilaterally through courts. The mandate to correct historical hypocrisy is live (it motivated mid-20th century statutory civil rights law), but the exclusive reliance on judicial reinterpretation rather than democratic amendment represents a drift from the founding problem's original resolution mechanism. The reading is not yet mandatrophic (the founding problem remains contested and live resistance is substantial), but the trajectory shows theater rising as the reading increasingly performs the justification for its own expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_evidence_epistemic_grounding,
    'Is equality self-evident in the Enlightenment sense (knowable through reason alone, independent of historical convention), or is the appearance of self-evidence a product of historical-political contingency that has become sedimented into institutional practice?',
    'Genealogical analysis of the self-evidence claim: trace its adoption in Enlightenment philosophy, its eclipse in 19th-century evolutionism and racial science, and its resurrection in post-WWII human rights discourse. If self-evidence is temporally variable, it is contingent rather than timeless.',
    'If self-evidence is contingent, the reading''s legitimacy depends on current institutional consensus rather than logical necessity, which shifts the ground from ''hypocrisy correction'' to ''institutional power assertion.'' This would reclassify the constraint from Tangled Rope (coordination + extraction) to Snare (pure extraction dressed as coordination) from the originalist seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_evidence_epistemic_grounding, conceptual, 'Whether self-evidence is an epistemic property or an institutional-political status.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Does the expansive reading suppress originalist and amendment-preference voices through external institutional means (doctrinal barriers, academic gatekeeping, judicial selection), or have these frames become internally delegitimized such that even their holders experience the suppression as rational acceptance of defeat?',
    'Examination of originist scholars'' self-reported reasons for doctrinal position change or exit from constitutional law; interviews with judges appointed on originalist criteria who adopt progressive interpretation; analysis of whether suppression persists post-retirement or in non-judicial forums.',
    'If internalized, the suppression is higher-efficiency and more durable; if structural, the suppression is brittle and depends on maintaining institutional control. Internalized suppression suggests the reading has achieved cultural hegemony; structural suppression suggests it remains contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternative readings is structural or internalized within the interpretive community.').

omega_variable(
    self_evidence_vs_constructed_kernel_contest,
    'Is the equality principle self-evident because equality itself is self-evident, or is the appearance of self-evidence a function of the expansive universalist reading''s institutional dominance and the deliberate framing of competing readings as morally incoherent?',
    'Comparative analysis of how each sibling reading justifies its own interpretive frame: does restrictive originalism claim fidelity to text is self-evident, or that historical context is binding? Does progressive textualism claim amendment is self-evidently necessary, or merely procedurally legitimate? Each reading''s claim to necessity versus contingency will clarify whether self-evidence is an intrinsic property or a rhetorical achievement.',
    'This omega documents the kernel contest itself: the competing readings stake their legitimacy on different grounding principles (historical text, democratic process, self-evident reason). The engine''s task is to compute which reading is actually instantiated at each institutional seat, not to adjudicate which grounding is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_evidence_vs_constructed_kernel_contest, conceptual, 'Whether self-evidence is the property of the equality principle or a rhetorical/institutional achievement of the expansive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(equa_tr_t0, projected).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__expansive_universalist, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__expansive_universalist, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__expansive_universalist, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(equa_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(equa_be_t0, projected).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__expansive_universalist, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__expansive_universalist, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__expansive_universalist, base_extractiveness, 40, 0.39).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(equa_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(equa_su_t0, projected).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__expansive_universalist, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__expansive_universalist, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__expansive_universalist, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(equa_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__expansive_universalist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This constraint is part of the equality-clause-scope kernel family. The three constraint stories (expansive_universalist, restrictive_originalist, progressive_textualist) are not alternative measurements of a single constraint but structurally distinct constraints instantiated by three competing readings of the same contested constitutional text. Each reading has a different beneficiary/victim structure, a different ε value, and a different type. The network edges record that this reading's operation (institutional expansion of equality scope through courts) creates downstream pressure on the other readings by establishing that interpretation-without-amendment is viable, which influences how the textualist and originalist readings must defend their own positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
