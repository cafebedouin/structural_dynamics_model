% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   The restrictive originalist reading of the equality clause holds that
 *   'all men are created equal' and the constitutional equality guarantee
 *   applied only to propertied white males as political actors within the
 *   18th-century social contract framework. This reading treats the founding
 *   exclusions (enslaved persons, women, propertyless men, indigenous
 *   peoples) not as contradictions to be resolved but as the intended scope
 *   of the compact. The constraint operates by defining the political
 *   community narrowly and requiring formal constitutional amendment — a
 *   high-threshold process controlled by state legislatures dominated by the
 *   beneficiary class — for any expansion. From this reading's perspective,
 *   the standing arrangement under contest is the original 1787 framework;
 *   the extraction is measured against that referent, not against the
 *   universalist alternative.
 *
 * KEY AGENTS:
 *   - propertied_white_male_citizens: Primary beneficiary (institutional/arbitrage) — hold full political rights, control amendment process
 *   - state_legislatures_controlling_franchise: Agenda setter (institutional/generational) — set voting qualifications, ratify/reject amendments
 *   - originalist_judicial_faction: Beneficiary (institutional/generational) — enforces narrow scope through judicial review
 *   - enslaved_persons: Primary victim (powerless/trapped) — legally excluded, subject to chattel regime
 *   - women: Victim (powerless/identity_locked) — excluded from franchise, coverture subsumes legal identity
 *   - propertyless_white_males: Victim (moderate/constrained) — partial inclusion via gradual franchise expansion, but original exclusion
 *   - free_black_persons: Victim (powerless/trapped) — denied citizenship and rights even in free states
 *   - indigenous_persons: Victim (powerless/trapped) — excluded as 'domestic dependent nations', not citizens
 *   - immigrants_without_naturalization: Victim (moderate/constrained) — excluded until naturalization, which states control
 *   - expansive_universalist_advocates: Excluded (organized/mobile) — argue for universal application, structurally locked out of originalist framework
 *   - progressive_textualist_advocates: Observer (institutional/analytical) — argue for amendment-based expansion, operate within system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.88).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.82).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.88).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, snare).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '3335ea0c-cbeb-47c8-b525-8337d6d78063').
narrative_ontology:cs_kernel_codification('3335ea0c-cbeb-47c8-b525-8337d6d78063', fixed_text).
narrative_ontology:cs_authority_grounding('3335ea0c-cbeb-47c8-b525-8337d6d78063', lineage).
narrative_ontology:cs_interpretation_layer_present('3335ea0c-cbeb-47c8-b525-8337d6d78063').
narrative_ontology:cs_reading_relation('3335ea0c-cbeb-47c8-b525-8337d6d78063', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('3335ea0c-cbeb-47c8-b525-8337d6d78063', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('3335ea0c-cbeb-47c8-b525-8337d6d78063', foundational, equality_scope_fixed_at_ratification).
narrative_ontology:cs_axiom_status(equality_scope_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('3335ea0c-cbeb-47c8-b525-8337d6d78063', equality_scope_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('3335ea0c-cbeb-47c8-b525-8337d6d78063', foundational, franchise_expansion_requires_formal_amendment).
narrative_ontology:cs_axiom_status(franchise_expansion_requires_formal_amendment, holdable).
narrative_ontology:cs_axiom_grounding('3335ea0c-cbeb-47c8-b525-8337d6d78063', franchise_expansion_requires_formal_amendment, conventional).
narrative_ontology:cs_axiom('3335ea0c-cbeb-47c8-b525-8337d6d78063', foundational, social_contract_parties_are_propertied_white_males).
narrative_ontology:cs_axiom_status(social_contract_parties_are_propertied_white_males, holdable).
narrative_ontology:cs_axiom_grounding('3335ea0c-cbeb-47c8-b525-8337d6d78063', social_contract_parties_are_propertied_white_males, conventional).
narrative_ontology:cs_reference_frame('3335ea0c-cbeb-47c8-b525-8337d6d78063', founding_era_social_contract).
narrative_ontology:cs_drift_state('3335ea0c-cbeb-47c8-b525-8337d6d78063', post_reconstruction_amendments, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3335ea0c-cbeb-47c8-b525-8337d6d78063', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, state_legislatures_controlling_franchise).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_judicial_faction).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, propertyless_white_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_black_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, immigrants_without_naturalization).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, fixed_constitutional_meaning_at_ratification).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, state_sovereignty_over_franchise_qualifications).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, social_contract_theory_as_founding_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold full political rights (vote, hold office, serve on juries, access courts). Control the political parties and economic institutions. Can move capital and influence across states. The constraint subsidizes their position — they pay no cost for the exclusion of others and collect the full benefits of the political order.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, beneficiary,
    institutional, generational, arbitrage, national).

% Set voting qualifications (property, taxpaying, race, sex, literacy). Ratify or reject constitutional amendments. Control the gateway to political participation. Their power derives from the original constitutional design reserving franchise regulation to states. They administer the constraint and could change it, but are dominated by the beneficiary class.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, state_legislatures_controlling_franchise, agenda_setter,
    institutional, generational, arbitrage, national).

% Enforce the narrow scope through judicial review. Provide interpretive legitimacy to the restrictive reading. Their institutional position (life tenure, appointment by beneficiaries) aligns them with the originalist framework. They collect interpretive authority and institutional prestige from maintaining the constraint.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_judicial_faction, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, originalist_judicial_faction, agenda_setter).

% Legally property, not persons. No rights, no political voice, no exit. The constraint extracts 100% of their labor and personhood. The social contract does not include them — they are its objects, not parties. Suppression is total: slave codes, patrols, fugitive slave laws, Dred Scott decision.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_persons, payer,
    powerless, immediate, trapped, national).

% Excluded from franchise and office. Coverture subsumes legal identity into husband/father. Property rights, contract rights, custody rights denied. The constraint extracts political voice and legal autonomy. Exit is identity-locked: the social role 'woman' is constituted by the exclusion. Resistance requires reconstituting identity (suffrage movement).
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, biographical, identity_locked, national).

% Originally excluded by property qualifications. Gradually included via state-level franchise expansion (1820s-1850s). But the original constraint extracted from them — they paid taxes, served in militias, but had no vote. Exit was constrained: could acquire property, move to more inclusive states, or organize politically. Partial beneficiaries after expansion, but the original constraint targeted them.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertyless_white_males, payer,
    moderate, biographical, constrained, national).

% Denied citizenship (Dred Scott), franchise, and basic rights even in free states. Subject to black codes, fugitive slave risk, colonization pressures. The constraint extracts security, mobility, and political standing. No exit within the system — emigration or rebellion only options.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_black_persons, payer,
    powerless, biographical, trapped, national).

% Classified as 'domestic dependent nations' — not citizens, not foreigners, excluded from the social contract entirely. The constraint extracts land, sovereignty, and cultural continuity via removal, allotment, assimilation. Exit is structurally blocked: the constraint defines them out of the political community that could recognize their rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_persons, payer,
    powerless, generational, trapped, national).

% Excluded until naturalization, which state and federal authorities control. Pay taxes, contribute labor, but denied political voice. The constraint extracts labor and taxes without representation. Exit options: naturalize (if permitted), return, or organize — but the gatekeepers are the beneficiaries.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, immigrants_without_naturalization, payer,
    moderate, biographical, constrained, national).

% Abolitionists, suffragists, civil rights advocates who argue equality is universal. They are structurally excluded from the originalist framework — their arguments are ruled out of bounds by the constraint's own terms. They operate outside the constraint, building parallel movements. Their mobility comes from appealing to natural law, not the constitutional text as originally understood.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, excluded,
    organized, generational, mobile, global).

% Argue that the equality principle is in the text but scope expands through Article V amendment, not judicial reinterpretation. They operate within the system, pushing for formal amendments (13th, 14th, 15th, 19th, 24th, 26th). They are seated at the table but argue for a different reading of the kernel.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, progressive_textualist_advocates, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a political community among propertied white males by defining the boundaries of the social contract, allocating governing authority, and establishing a stable framework for property rights and commercial exchange among the founding class.
% TRANSFER_FUNCTION: Moves political power, legal protection, resource allocation, and citizenship status from the excluded groups (enslaved persons, women, propertyless men, free blacks, indigenous peoples, immigrants) to propertied white male citizens and the state legislatures they control. The transfer is enforced through slave codes, coverture, property qualifications, and the amendment gate.
% ABSENT_VOICES: The excluded groups themselves — enslaved persons (literally silenced by law), women (excluded from the deliberative bodies), indigenous nations (treated as external), propertyless men (no franchise until gradual expansion). They would object to being defined out of the social contract, but the constraint's structure prevents their voices from counting within the framework. The expansive universalist advocates are also excluded from the originalist interpretive community.
% DISAPPEARANCE_RATIONALE: If the restrictive originalist reading vanished overnight, the political community would have to reconstitute its boundaries. The franchise would no longer be limited to the original class; representation, resource allocation, and legal protection would have to extend to all persons. The state legislatures' monopoly on franchise qualification would collapse. The originalist judicial faction would lose its interpretive anchor. The world would rearrange — this is not a natural law but a constructed political order.
% FOUNDING_PROBLEM: Coordinating a viable republican government among propertied white male elites across 13 diverse colonies/states in 1787, balancing state sovereignty with national coordination, protecting property rights and commercial exchange, and avoiding the fragmentation that doomed the Articles of Confederation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (1787 coordination among propertied white male elites) is dead — that specific historical coordination challenge no longer exists. Corroboration: the beneficiaries themselves (originalist judges, state legislatures) no longer claim the 1787 coordination problem is live; they argue the *solution* (the original meaning) is permanently binding. The expansive universalist and progressive textualist readings both attest the founding problem is dead — the former because the principle was always universal, the latter because amendments have solved it. No credible source outside the beneficiary set claims the 1787 coordination problem persists.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The restrictive originalist reading instantiates a snare from the perspective of those excluded: high extraction (0.88 at origin) as the political community's benefits (representation, rights protection, resource allocation) flow exclusively to propertied white males; high suppression (0.82) as the constraint's persistence depends on active exclusion (slave codes, coverture, property qualifications, immigration bars) and the amendment process is controlled by beneficiaries. Theater ratio is low (0.25) because the enforcement is functional, not performative — the exclusion machinery (slave patrols, property tests, literacy tests, poll taxes) does real work. Accessibility collapse is moderate (0.35) because alternatives (universalist readings, natural rights arguments) existed and persisted throughout. Resistance is high (0.72) from the excluded groups and their allies. The claimed type is snare because the coordination story (social contract among equals) is cover for extraction from the excluded.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (propertied white males, state legislatures, originalist judges) experience this as a mountain or rope — a legitimate founding compact that coordinates their political order. The victim seats experience it as a snare — enforced exclusion from the political community. The engine computes this divergence from the structural data: beneficiaries have d near 0.0 (subsidized), victims have d near 1.0 (extracted). The originalist judicial faction sits at d ≈ 0.15 (beneficiary but with some institutional maintenance cost). The expansive universalist advocates are excluded (d undefined — they are not seated within the constraint's operation).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: propertied_white_male_citizens (collect political power, resource allocation, legal protection), state_legislatures_controlling_franchise (control the franchise gateway and amendment ratification), originalist_judicial_faction (institutional legitimacy, interpretive authority). Victims declared: all excluded groups — enslaved persons (total extraction of labor and personhood), women (extraction of political voice and legal autonomy), propertyless white males (partial extraction until franchise expansion), free black persons, indigenous persons, immigrants. Directionality derives from this: beneficiaries → low d, victims → high d. The amendment process is the structural modulation: high threshold, beneficiary-controlled, makes exit from victim status nearly impossible without beneficiary consent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating a political community among propertied white males in 1787) is dead — that specific coordination problem no longer exists. Yet the constraint persists in its restrictive form through judicial doctrine (originalism) and institutional inertia. The mandate has atrophied but the constraint remains, extracting from those the original compact never included. This is not a piton (no theatrical maintenance — the enforcement was and is functional for the beneficiaries) but a snare whose founding justification has expired. The mandatrophy_resolved flag would be false: the constraint's current operation is not justified by its founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''equality_clause_scope'', specifically the ''restrictive_originalist'' reading?',
    'This omega records the committer-frame structure: the kernel_id is equality_clause_scope, this reading_id is restrictive_originalist, sibling readings are expansive_universalist and progressive_textualist. The disagreement is located in the beneficiary set definition, the legitimacy threshold for expansion, and whether franchise extension requires amendment versus judicial interpretation.',
    'If this is a kernel reading, the constraint''s ε refers to the standing arrangement under contest as seen from THIS reading''s lights — the originalist sees the 1787 arrangement as legitimate and the expansion as extractive; the universalist sees the original arrangement as the extraction. Different ε, different constraints. This omega prevents conflation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commiter frame: this is the restrictive_originalist reading of equality_clause_scope kernel').

omega_variable(
    natural_law_vs_constructed_exclusion,
    'Does the restrictive scope reflect a genuine natural law limitation of the 18th-century social contract, or a constructed exclusion benefiting the framers'' class?',
    'Historical analysis of framing-era debates, ratification records, and contemporary political philosophy. If the exclusion was contested even among the framers (e.g., Abigail Adams, early abolitionist arguments), it is constructed, not natural.',
    'If constructed, the constraint is a false summit mountain masquerading as natural law — FSM candidate. The declared beneficiaries (propertied white males) are the extractive class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_exclusion, empirical, 'Whether the restrictive scope is natural law or constructed class privilege').

omega_variable(
    amendment_legitimacy_threshold,
    'Is the ''high legitimacy threshold for expansion'' (requiring formal amendment) a genuine coordination mechanism or a veto point protecting incumbent beneficiaries?',
    'Compare amendment success rates for franchise expansion versus other amendment types. If franchise amendments face structurally higher barriers (supermajority requirements, state legislature veto points controlled by beneficiaries), the threshold is extractive.',
    'If the threshold functions as a beneficiary veto, the constraint''s coordination story is cover for extraction — supports snare classification. If genuinely coordinative, supports rope/tangled_rope elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_legitimacy_threshold, conceptual, 'Whether the amendment barrier coordinates or protects extractive incumbents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eq_orig_restrictive_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(eq_orig_restrictive_tr_t1820, equality_clause_scope__restrictive_originalist, theater_ratio, 1820, 0.15).
narrative_ontology:measurement(eq_orig_restrictive_tr_t1865, equality_clause_scope__restrictive_originalist, theater_ratio, 1865, 0.22).
narrative_ontology:measurement(eq_orig_restrictive_tr_t1870, equality_clause_scope__restrictive_originalist, theater_ratio, 1870, 0.28).
narrative_ontology:measurement(eq_orig_restrictive_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(eq_orig_restrictive_tr_t1965, equality_clause_scope__restrictive_originalist, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(eq_orig_restrictive_tr_t2025, equality_clause_scope__restrictive_originalist, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(eq_orig_restrictive_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.92).
narrative_ontology:measurement(eq_orig_restrictive_be_t1820, equality_clause_scope__restrictive_originalist, base_extractiveness, 1820, 0.89).
narrative_ontology:measurement(eq_orig_restrictive_be_t1865, equality_clause_scope__restrictive_originalist, base_extractiveness, 1865, 0.78).
narrative_ontology:measurement(eq_orig_restrictive_be_t1870, equality_clause_scope__restrictive_originalist, base_extractiveness, 1870, 0.72).
narrative_ontology:measurement(eq_orig_restrictive_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(eq_orig_restrictive_be_t1965, equality_clause_scope__restrictive_originalist, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(eq_orig_restrictive_be_t2025, equality_clause_scope__restrictive_originalist, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(eq_orig_restrictive_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.95).
narrative_ontology:measurement(eq_orig_restrictive_su_t1820, equality_clause_scope__restrictive_originalist, suppression_requirement, 1820, 0.9).
narrative_ontology:measurement(eq_orig_restrictive_su_t1865, equality_clause_scope__restrictive_originalist, suppression_requirement, 1865, 0.75).
narrative_ontology:measurement(eq_orig_restrictive_su_t1870, equality_clause_scope__restrictive_originalist, suppression_requirement, 1870, 0.7).
narrative_ontology:measurement(eq_orig_restrictive_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(eq_orig_restrictive_su_t1965, equality_clause_scope__restrictive_originalist, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(eq_orig_restrictive_su_t2025, equality_clause_scope__restrictive_originalist, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__restrictive_originalist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, thirteenth_amendment).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, fourteenth_amendment).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, fifteenth_amendment).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, nineteenth_amendment).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, voting_rights_act_1965).

% DUAL FORMULATION NOTE:
% This constraint family (equality_clause_scope) decomposes the natural-language concept 'constitutional equality' into three structurally distinct readings with different ε values, beneficiary/victim structures, and classifications. The restrictive_originalist reading has high ε (0.88) and snare classification; the expansive_universalist reading would have low ε from its own referent but high ε from the originalist referent; the progressive_textualist reading sits between. They are linked via affects_constraints because the originalist reading is cited as the original meaning that the other readings must overcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, institutional, 0.15).
constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, powerless, 0.98).
constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, moderate, 0.75).
constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
