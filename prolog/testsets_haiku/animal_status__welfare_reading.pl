% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Sentience as Constraint on Instrumental Use (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The welfare reading of animal status establishes that sentient animals
 *   have interests worthy of moral consideration, but these interests do not
 *   prohibit their instrumental use by humans — only constrain it through
 *   welfare requirements. This reading structures the regulatory landscape
 *   governing animal agriculture, research, and use globally. The constraint
 *   is claimed as tangled_rope: it genuinely coordinates a solution to the
 *   inconsistency between sentience recognition and use (the coordination
 *   function); it also operates to extract ongoing value from animals while
 *   exempting users from more stringent moral demands (the extractive
 *   function). The two functions are inseparable — welfare requirements are
 *   the mechanism that permits use to continue. The measurement trajectory
 *   shows the constraint stabilizing around ε ≈ 0.45 after initial
 *   tightening, with theater_ratio rising toward 0.5, suggesting increasing
 *   performative activity relative to functional welfare gains.
 *
 * KEY AGENTS:
 *   - animals_subject_to_instrumental_use — powerless, structurally unable to exit; bear the costs of confinement and use
 *   - animal_agriculture_industry — organized, beneficiary, agenda-setter; commands resources to maintain the use-permitting framework
 *   - research_institutions — institutional, beneficiary, agenda-setter; conduct experiments within welfare constraint
 *   - welfare_advocates — organized payer; push for tighter welfare standards but operate within the constraint's core concession
 *   - abolitionist_activists — excluded; their core claim contradicts the constraint's defining premise
 *   - legal_authority — institutional agenda-setter; enforces welfare standards while preserving use rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.62).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Sentience as Constraint on Instrumental Use (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '69e102e9-f508-4767-ab3a-6b40d485d3f6').
narrative_ontology:cs_kernel_codification('69e102e9-f508-4767-ab3a-6b40d485d3f6', distributed).
narrative_ontology:cs_authority_grounding('69e102e9-f508-4767-ab3a-6b40d485d3f6', lineage).
narrative_ontology:cs_interpretation_layer_present('69e102e9-f508-4767-ab3a-6b40d485d3f6').
narrative_ontology:cs_reading_relation('69e102e9-f508-4767-ab3a-6b40d485d3f6', animal_status__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('69e102e9-f508-4767-ab3a-6b40d485d3f6', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('69e102e9-f508-4767-ab3a-6b40d485d3f6', foundational, animal_sentience_grounds_welfare_constraint).
narrative_ontology:cs_axiom_status(animal_sentience_grounds_welfare_constraint, holdable).
narrative_ontology:cs_axiom_grounding('69e102e9-f508-4767-ab3a-6b40d485d3f6', animal_sentience_grounds_welfare_constraint, empirically_contingent).
narrative_ontology:cs_axiom('69e102e9-f508-4767-ab3a-6b40d485d3f6', foundational, sentience_permits_instrumental_use_with_bounds).
narrative_ontology:cs_axiom_status(sentience_permits_instrumental_use_with_bounds, holdable).
narrative_ontology:cs_axiom_grounding('69e102e9-f508-4767-ab3a-6b40d485d3f6', sentience_permits_instrumental_use_with_bounds, deontological).
narrative_ontology:cs_reference_frame('69e102e9-f508-4767-ab3a-6b40d485d3f6', sentience_recognition_with_use_exemption).
narrative_ontology:cs_drift_state('69e102e9-f508-4767-ab3a-6b40d485d3f6', contemporary_welfare_tightening_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('69e102e9-f508-4767-ab3a-6b40d485d3f6', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, human_users_of_animal_products).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_subject_to_instrumental_use).
narrative_ontology:constraint_victim(animal_status__welfare_reading, welfare_advocates).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, instrumental_use_permissibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of confinement, handling, and terminal procedures in service of human needs. The constraint declares their sentient interests worthy of moral consideration but not sufficient to prevent their use for food, research, or labor. They have no capacity to exit the arrangement or modify its terms. Welfare protections apply, but only within frameworks that maintain their availability for instrumental use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_subject_to_instrumental_use, payer,
    powerless, biographical, trapped, global).

% Operates under a legal and normative framework that permits animal use for production once welfare standards are met. Sets industry standards, lobbies for welfare regulations that remain compatible with production efficiency, and frames high-welfare practices as evidence the constraint is satisfied. Extracts substantial economic value from animals while accepting minimal welfare costs. Commands significant political and economic resources to maintain the framework.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_agriculture_industry, agenda_setter).

% Conduct animal research under animal care protocols that operationalize the welfare constraint. Institutional Review Boards must certify welfare measures, but research is not prohibited by the constraint. Extract scientific and medical knowledge (and research funding) using animals as experimental subjects. Frame compliance with welfare protocols as sufficient legitimation of animal use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, research_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, research_institutions, agenda_setter).

% Access meat, dairy, animal testing outcomes, and other products derived from animal use. The constraint allows them to consume these products without moral prohibition, provided they acknowledge (through labels, certifications) that animals were treated humanely. Exit options exist (vegetarian/vegan alternatives, synthetic substitutes), but these remain marginal. Benefit from a framework that permits use while reducing moral friction.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, human_users_of_animal_products, beneficiary,
    powerful, biographical, mobile, global).

% Push for increasingly stringent welfare standards and enforcement, but operate within the constraint's core concession: that animal use is permitted if welfare is adequate. They bear the cost of advocacy (litigation, lobbying, organizational overhead) while the constraint's architecture preserves the fundamental legitimacy of instrumental use. Their victories become welfare improvements that expand the range of 'acceptable' use rather than closing it.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, welfare_advocates, excluded).

% Reject the constraint's core premise that sentience permits use. They are structurally excluded from the framework because their core claim (no instrumental use at all) contradicts the constraint's defining concession. Their exclusion is built into the constraint's architecture — welfare improvements are viewed by abolitionists as legitimizing cover for exploitation, not moral progress.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_activists, excluded,
    moderate, biographical, identity_locked, global).

% Adjudicates and codifies the boundary between permitted use and impermissible cruelty. Courts interpret animal welfare statutes; legislatures refine them. They maintain the constraint by enforcing welfare standards while preserving use rights. This dual role (enforcer of protection + guarantor of use) is structurally central to the constraint.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, legal_authority_courts_legislation, agenda_setter,
    institutional, generational, analytical, national).

% The empirical grounding for the constraint's core claim: animals possess subjective experience and interests. Advances in neuroscience, ethology, and comparative psychology have strengthened evidence for sentience across many species. This scientific foundation underpins the constraint's legitimacy but does not itself resolve the moral question of what sentience permits.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, scientific_consensus_on_sentience, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(animal_status__welfare_reading, scientific_consensus_on_sentience).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles human use of animals with recognition of their sentient interests by establishing welfare standards as the operative moral floor. Solves the coordination problem: how can a society that acknowledges animal sentience still permit instrumental use? Answer: by making welfare a binding constraint on use rather than a mere preference.
% TRANSFER_FUNCTION: Moves animals' productive capacity (meat, milk, labor, research data) to human users in exchange for welfare protections that fall short of prohibiting use. The constraint transfers moral permission from blanket use (property reading) to use-with-welfare-bounds (welfare reading). The transfer is one-directional: animals provide biological value; humans provide welfare compliance (a cost structure, not a reciprocal benefit).
% ABSENT_VOICES: Abolitionist voices are structurally excluded — the constraint's core premise is that sentience permits use, so any claim that sentience prohibits use is defined out of the framework. They would object to the entire architecture; their exclusion is not an oversight but a feature of the constraint's design. Arguably, the animals themselves are absent — they cannot articulate what welfare level they would choose, only exhibit suffering or thriving.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the legal framework would revert to either abolitionist prohibition (all instrumental use ends) or property-regime deregulation (welfare standards collapse). Either way, the institutional structure of animal agriculture, research, and use would reorganize entirely. The constraint's removal would leave no stable middle ground — it actively constructs the middle ground by its existence.
% FOUNDING_PROBLEM: The foundational problem was moral inconsistency: growing empirical evidence for animal sentience made pre-modern property-regime treatment (pure object, no moral status) indefensible, but societies also depended on animal use. The welfare reading solved the inconsistency by accepting sentience while carving out an exemption for instrumental use, provided welfare standards constrained the use.
% FOUNDING_PROBLEM_CORROBORATION: The constraint's founding narrative is affirmed by the animal agriculture industry and most legislative bodies (the constraint is their preferred reading). Animal welfare advocates affirm that sentience recognition was the founding achievement. Abolitionists and philosophers of animal rights contest whether the founding problem was genuinely solved or merely postponed — they argue the framework's core concession (sentience permits use) was never justified and the 'solution' is theater masking unchanged extraction. Academic philosophy and interdisciplinary animal ethics literature (outside the benefiting parties) split on the verdict: some support welfare as a transitional framework; others treat it as a permanently unstable compromise.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.45 reflects a moderate but substantial transfer: animals yield biological value (meat, research data, labor) in exchange for welfare protections that do not eliminate the use itself. The constraint operates as a regulatory exemption structure — sentience is recognized, but the exemption for instrumental use (with welfare bounds) is what actually operates. Suppression at 0.62 captures the enforcement necessary to maintain this exemption: welfare regulations must be enforced on producers; abolitionist challenges must be marginalized; animals' own resistance to confinement must be managed. Theater_ratio rising toward 0.5 reflects increasing ceremonial activity: welfare certifications, corporate commitments to 'higher-welfare' practices, legislative welfare improvements that expand the scope of 'acceptable' use rather than closing it. The constraint is tangled_rope because the coordination function (solving sentience-use inconsistency) and the extraction function (permitting ongoing use) are locked together — you cannot strengthen welfare without also legitimizing use, and you cannot prohibit use without rejecting the constraint's core premise.
 *
 * PERSPECTIVAL GAP:
 *   From the industry seat, the constraint is genuine coordination: recognition of sentience is the achievement, and welfare compliance is the legitimate operating procedure. From the animal seat (if animals could articulate a position), the constraint is pure extraction — permission continues but under tighter supervision. From the welfare advocate seat, it is a tangled rope: they have achieved sentience recognition but find themselves defending welfare improvements that paradoxically strengthen the justification for continued use (the constraint's beneficiaries cite welfare improvements as proof that use is now ethically acceptable). The abolitionist seat is structurally excluded: the constraint's core concession (sentience permits use) is the abolitionist's core rejection, so the two cannot coexist within one framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals as powerless trapped agents: directionality near 1.0 (full targets). They pay the cost of use with no exit and no voice. The agriculture industry as organized institutional beneficiary: directionality near 0.0. They set the terms, extract the value, and command the resources. Welfare advocates as organized payers: directionality elevated (they bear advocacy costs) but with some leverage (political organization, moral authority). The constraint's structure is asymmetric: beneficiaries control the agenda-setting function; victims and payers have constrained influence. The measurement series shows extractiveness stabilizing rather than rising dramatically, suggesting the constraint has reached an equilibrium rather than intensifying extraction — but suppression and theater remain elevated, indicating ongoing enforcement costs and performative activity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids simple misclassification as pure rope (genuine coordination without extraction) because the evidence shows: (1) beneficiaries actively enforce the exemption structure, not merely coordinate a shared solution; (2) welfare improvements occur alongside unchanged production volumes, suggesting welfare serves legitimation rather than reduction of harm; (3) abolitionist voices are structurally excluded, not persuaded. The constraint similarly avoids simple misclassification as pure snare because: (1) welfare protections are real and enforceable, not theater masking unchanged treatment; (2) the framework genuinely does recognize sentience, which is a normative achievement; (3) the constraint's persistence does not depend on hiding its operation from its beneficiaries. The tangled_rope classification captures the genuine ambiguity: the coordination function is real (solving sentience-use inconsistency); the extraction function is also real (permitting continued use via exemption structures); they are structurally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_requirement_as_legitimation_vs_harm_reduction,
    'Do welfare requirements primarily serve to reduce animal suffering (harm reduction) or to legitimize continued use by performing moral concern (legitimation cover)?',
    'Empirical comparison of welfare improvements to production volume trends, severity of remaining practices, and gap between welfare standards and natural animal behavior capacities. Longitudinal analysis of whether welfare tightening correlates with reduced use or with relabeling of practices as ''humane.''',
    'If primarily harm reduction, the constraint''s extraction component is justified as the price of welfare oversight. If primarily legitimation, the theater_ratio is underestimated and the constraint operates closer to snare. Classification could shift from tangled_rope toward snare if evidence shows welfare serves only to justify unchanged extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_requirement_as_legitimation_vs_harm_reduction, empirical, 'Whether welfare requirements reduce suffering or legitimize use.').

omega_variable(
    sentience_doctrine_empirical_stability,
    'Is the scientific consensus on animal sentience stable, or does the constraint''s entire framework depend on an empirical claim that could be revised by neuroscience advances?',
    'Longitudinal review of neuroscience and ethology research. Scenario testing: if evidence substantially narrowed sentience across some species (e.g., discovering nociception without subjective experience), would the constraint''s justification survive?',
    'If the sentience claim is core to the constraint''s legitimacy and empirically contingent, the constraint carries existential risk from scientific revision. If welfare operates regardless of ultimate sentience verdict, the constraint is more robust. A major empirical revision could trigger mandate obsolescence or force migration to alternative grounds (nature of animal interests, capacity for flourishing, etc.).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_doctrine_empirical_stability, empirical, 'Whether the constraint''s empirical foundation (animal sentience) is stable or subject to revision.').

omega_variable(
    abolitionist_exclusion_as_design_vs_temporary,
    'Is the exclusion of abolitionist voices a permanent feature of the constraint''s design, or a contestable boundary that could be redrawn?',
    'Political analysis of legislative and regulatory moments where abolitionist claims enter institutional discourse. Pressure testing: if abolitionist voices gained sufficient political power, could they force reclassification of the constraint, or is the structure immune?',
    'If exclusion is designed (the constraint only functions by excluding claims that sentience prohibits use), the constraint is fragile at the political boundary and vulnerable to advocacy capture. If redrawn, the constraint could migrate toward scaffold (welfare as transition) or collapse toward abolitionist reading. If exclusion is contingent on power distribution, classification and stability depend on political trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolitionist_exclusion_as_design_vs_temporary, conceptual, 'Whether abolitionist exclusion is structural or contingent.').

omega_variable(
    animal_interests_vs_welfare_standards_alignment,
    'Do the interests of sentient animals (as recognized by the constraint) actually align with the welfare standards used to operationalize the constraint, or do they diverge?',
    'Ethological comparison of natural behaviors and environmental needs against welfare standard specifications. Analysis of gap between what animals exhibit as preference (in choice experiments, space use, etc.) and what standards permit.',
    'High alignment would support the constraint as genuine coordination. High divergence would show the constraint recognizes sentience formally while operationalizing it through standards that ignore actual interests — effectively using sentience recognition as legitimation while welfare standards permit continued violation of interests. This would strengthen reclassification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_interests_vs_welfare_standards_alignment, empirical, 'Whether operationalized welfare standards track animal interests or diverge from them.').

omega_variable(
    reading_kernel_contest_stability,
    'Is the welfare_reading''s position stable as a middle ground, or is it under sustained pressure from both abolitionist and property readings to collapse toward one extreme?',
    'Institutional and political analysis tracking the boundary: do legislatures incrementally tighten welfare toward abolitionist territory, incrementally relax toward property regime, or hold the middle ground? Do courts treat animals as property or as sentient beings? Do moral narratives shift?',
    'If stable, the constraint is an equilibrium. If under sustained pressure, it may be a temporary scaffold. If collapsing toward property reading (welfare standards being rolled back, sentience recognition weakening), the constraint migrates toward snare. If collapsing toward abolitionist reading, the constraint becomes terminal and replaced by prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_stability, conceptual, 'Whether the welfare reading occupies a stable middle ground or is being absorbed by a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t5, animal_status__welfare_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(anim_tr_t5, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t15, animal_status__welfare_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(anim_tr_t15, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t25, animal_status__welfare_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.49).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t5, animal_status__welfare_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(anim_be_t5, observed).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t15, animal_status__welfare_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(anim_be_t15, observed).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t25, animal_status__welfare_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t5, animal_status__welfare_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(anim_su_t5, observed).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t15, animal_status__welfare_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(anim_su_t15, observed).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t25, animal_status__welfare_reading, suppression_requirement, 25, 0.63).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three readings, each with distinct ε values and structural consequences. The welfare_reading (this constraint) sits between the property_reading (legal permissibility without sentience-based constraint, ε ≈ 0.05) and the abolitionist_reading (sentience entails rights, use prohibited, ε ≈ 0.0 or undefined). The welfare reading's ε ≈ 0.45 reflects the extraction via exemption: sentience is recognized but use is permitted. The three readings coexist across different institutional and political contexts, and pressure on one reading (e.g., legislative tightening toward abolitionist or relaxing toward property) propagates through the family. Beneficiary/victim structures diverge: abolitionist reading places all animals in the victim set for any use; property reading places no animals in a victim set for use; welfare reading distinguishes gratuitous harm from instrumental use with welfare protection. Link all three stories via network.affects_constraints and populate reading_relations in each story's cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__welfare_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
