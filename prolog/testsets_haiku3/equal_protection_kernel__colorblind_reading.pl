% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Clause — Colorblind Reading
 *   domain: constitutional/civil_rights/education_policy
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause interprets the
 *   Fourteenth Amendment as mandating categorical color-blindness: the state
 *   may not use racial classifications in any context, regardless of purpose.
 *   This reading is one of three competing interpretations of the same kernel
 *   (the Equal Protection Clause). The colorblind reading has become dominant
 *   US constitutional doctrine through Supreme Court decisions (especially
 *   after 2007). The structural delta is stark: historically excluded groups
 *   lose remedial pathways; all race-conscious state action is per se
 *   unconstitutional; the state has no obligation to address effects of past
 *   discrimination. This is authored as TANGLED ROPE: it coordinates a single
 *   uniform principle across jurisdictions (coordination function — formal
 *   equality for all), but it asymmetrically extracts from historically
 *   excluded groups while benefiting majority applicant pools and
 *   standardized testing industries. The coordination and extraction
 *   components are inseparable under this reading's own logic: the extracted
 *   benefit (lower admission competition for majority pools) IS the
 *   coordination mechanism (formal equality applied uniformly). The
 *   constraint's persistence requires active enforcement: universities must
 *   reject or dismantle race-conscious admissions programs through judicial
 *   invalidation and litigation threat, not through participant preference.
 *
 * KEY AGENTS:
 *   - Supreme Court colorblind majority: institutional power, agenda-setter for doctrine, enforces the reading through case law
 *   - Historically excluded racial groups: powerless, trapped, bear generational extractive cost through reduced selective-institution access
 *   - Affirmative action beneficiaries: moderate power, constrained exit, direct victims of the constraint
 *   - Majority applicant pools: organized power, beneficiaries via reduced admission competition
 *   - Remedial and antisubordination reading advocates: institutional power, excluded from doctrine-setting, structurally unable to exit
 *   - University administrators: institutional power, constrained enforcers of the doctrine, pay compliance costs
 *   - Standardized testing industry: organized power, beneficiaries through amplified test-score dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.68).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.72).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Clause — Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional/civil_rights/education_policy").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, 'f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd').
narrative_ontology:cs_kernel_codification('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', fixed_text).
narrative_ontology:cs_authority_grounding('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', lineage).
narrative_ontology:cs_interpretation_layer_present('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd').
narrative_ontology:cs_reading_relation('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', foundational, state_race_classification_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_race_classification_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', state_race_classification_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', foundational, formal_equality_sufficient_for_constitutional_compliance).
narrative_ontology:cs_axiom_status(formal_equality_sufficient_for_constitutional_compliance, holdable).
narrative_ontology:cs_axiom_grounding('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', formal_equality_sufficient_for_constitutional_compliance, deontological).
narrative_ontology:cs_reference_frame('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', constitutional_color_blindness).
narrative_ontology:cs_drift_state('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', contemporary_post_2007, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0d132c3-5a80-4c5c-b42a-1a8289f6dfcd', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, majority_applicant_pools).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, color_blindness_doctrine_advocates).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, affirmative_action_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, university_administrators).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, standardized_testing_industry).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, university_administrators).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, constitutional_color_blindness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The US Supreme Court faction that sets constitutional doctrine through opinions declaring race-conscious admissions violate equal protection per se. They interpret the Fourteenth Amendment's text as mandating formal race-neutrality in all state action. They enforce this doctrine through case decisions that invalidate university admissions programs that consider race.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, supreme_court_colorblind_majority, agenda_setter,
    institutional, generational, analytical, national).

% Applicant pools that benefit from race-neutral admissions criteria where their demographic group holds higher average test scores under standardized measures. They gain admission slots that would have gone to race-conscious affirmative action beneficiaries. They frame the colorblind rule as fairness and meritocracy.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, majority_applicant_pools, beneficiary,
    organized, biographical, mobile, national).

% Black, Latino, Native American, and other historically excluded groups lose access to remedial admissions pathways that had partially compensated for documented educational disparities and historical discrimination. They pay through reduced admission rates to selective institutions, lower lifetime earnings, and foreclosed intergenerational wealth accumulation. Exit from this constraint would require either leaving the jurisdiction or convincing the Court to overturn doctrine — both are structurally unavailable.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups, payer,
    powerless, generational, trapped, national).

% Individual applicants from historically excluded groups who would have been admitted under race-conscious criteria and are now rejected under colorblind criteria. They carry the direct cost of the constraint. They can apply to alternative institutions or retake standardized tests, but these options are constrained by geography, resources, and test-score ceilings.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, affirmative_action_beneficiaries, payer,
    moderate, biographical, constrained, national).

% Legal scholars, civil rights organizations, and minority-party judges and legislators who hold the remedial reading (race-conscious action to remedy documented historical exclusion is constitutionally permitted). They are excluded from the decision-making apparatus that interprets the Constitution: the majority Court has settled doctrine against them. Their only structural exit is a constitutional amendment or a shift in Court composition.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, remedial_reading_advocates, excluded,
    institutional, generational, trapped, national).

% Legal scholars and advocates who hold the antisubordination reading (the clause forbids caste-like subordination, not classification per se; race-conscious action to dismantle hierarchy is permitted). They are similarly excluded from the authoritative doctrine-setting apparatus. Their structural situation mirrors the remedial advocates: no voice in the Court, no exit except through composition change.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, antisubordination_reading_advocates, excluded,
    institutional, generational, trapped, national).

% Public university officials who must enforce the colorblind doctrine or face litigation and loss of accreditation. They comply with the constraint because legal liability makes refusal structurally unavailable. They bear the cost of restructuring admissions machinery; some also benefit if the doctrine aligns with institutional preferences, but most are primarily constrained enforcers.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, university_administrators, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, university_administrators, beneficiary).

% The SAT and ACT testing apparatus benefits from colorblind doctrine because standardized test scores become the dominant admissions criterion. Test-score distributions correlate with race due to socioeconomic and educational disparities, so the colorblind rule amplifies the role of tests without naming race explicitly. The industry gains market share and cultural authority as the 'objective' measure of merit.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, standardized_testing_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Legal scholars, think tanks, and civil rights organizations that endorse colorblind constitutionalism as the correct reading of equal protection. They are vindicated by the doctrine's adoption and hold positions of institutional influence. They collect ideological authority and set the terms of constitutional debate.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, color_blindness_doctrine_advocates, beneficiary,
    organized, generational, mobile, national).

% An analytical observer position that witnesses the constraint's operation across all seats and the divergence between the colorblind reading's formal logic and its distributional effects on historically excluded groups.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, observer_seat, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, majority_applicant_pools).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, race-neutral rule for all admissions decisions across all public institutions under a single constitutional standard. Solves the coordination problem of what uniform principle should govern equal protection in a diverse society — provides a single, formally symmetrical answer applicable regardless of context or history.
% TRANSFER_FUNCTION: Moves selective-institution admission slots from historically excluded racial groups and affirmative action beneficiaries to applicant pools whose demographic groups hold higher average standardized test scores. Transfers ideological authority from remedial and antisubordination readings to colorblind doctrine advocates and their institutional allies.
% ABSENT_VOICES: Historically excluded racial groups have no seat at the Supreme Court; affirmative action beneficiaries are not parties to constitutional interpretation; descendants of enslaved people who would name reparative obligations do not appear in formal proceedings. Remedial and antisubordination reading advocates are institutionally excluded from authoritative doctrine-setting.
% DISAPPEARANCE_RATIONALE: If the colorblind doctrine disappeared (Court reversal, constitutional amendment, or reinterpretation), universities would immediately re-adopt race-conscious admissions criteria where legally permitted; student demographics at selective institutions would shift; the standardized testing industry's dominance would decline; institutional resources would flow differently across racial groups; ideological authority would reallocate from colorblind advocates to remedial advocates. The entire machinery of formal equal protection as currently instantiated would reorganize.
% FOUNDING_PROBLEM: What principle should govern equal protection in a diverse society with competing interpretations of 'equal'? How can the state treat citizens of different races without violating constitutional neutrality? The founding problem names a genuine structural ambiguity: does equal protection require the state to be blind to race, or does it require the state to correct racial subordination?
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court colorblind majority attests the founding problem is answered by the constitutional text: equal protection means formal race-neutrality. Remedial and antisubordination advocates atttest the founding problem remains live and unresolved: formal neutrality can entrench subordination. Empirical evidence from educational attainment disparities, wealth gaps, and educational segregation post-colorblind doctrine adoption supports the 'live problem' attestation from outside the benefiting parties (independent demographers, economists, civil rights organizations not subject to Court authority).
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (when the reading was one contested doctrine among several) to 0.68 (when it became dominant and binding on all public institutions). The rise tracks the doctrine's consolidation: as it moved from contested to settled, the cost to historically excluded groups hardened. Suppression requirement rises even more sharply (0.35 to 0.72) because maintaining the constraint's dominance requires actively suppressing alternative readings through case law precedent, rejecting briefs from remedial advocates, and blocking legislative workarounds. Theater ratio stays moderate (0.29): the doctrine is not pure performance — the coordination function (formal equality) is real and institutional. But a growing share of the constraint's enforcement activity (especially post-2007) targets the suppression of remedial admissions, not the coordination of uniform principle. The measurement series track the constraint's operation across its 35-year interval, with t0-t10 being a period of contestation and increasing consolidation, t10-t25 the period of doctrine-establishment hardening, and t25-t35 plateau under dominant doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court seat experiences the constraint as principled constitutional interpretation yielding uniform fairness; the historically excluded groups' seats experience it as coordinated extraction using formal equality as cover. University administrators sit asymmetrically: they must enforce the doctrine (high d, near target), but they also benefit from reduced litigation risk under a settled constitutional rule (partial d downward). This gap between 'principled interpretation' and 'coordinated extraction' is the core structural divergence: from the agenda-setter's seat the constraint is coordination; from the payer's seat it is extraction. The engine computes per-seat classifications that reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are majority_applicant_pools and color_blindness_doctrine_advocates. Victims are historically_excluded_racial_groups and affirmative_action_beneficiaries. Majority applicants benefit through admission slots freed by colorblind admissions; doctrine advocates benefit through ideological authority and vindication of their constitutional reading. Historically excluded groups lose remedial pathways and suffer generational accumulation of educational/economic disadvantage. Affirmative action beneficiaries lose individual admission chances. The constraint's power derives from institutional authority (Supreme Court) enforcing doctrine through case law, not from voluntary coordination among equal parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading presents itself as transcending race by forbidding racial classification entirely. This framing obscures a core structural fact: the constraint's persistence depends on suppressing alternative readings (remedial and antisubordination) whose advocates would reject the colorblind framing as precisely the problem — they would argue the constraint uses 'colorblindness' as a mandate to ignore subordination. The mandatrophy question: Has the founding problem (what principle governs equal protection?) actually been solved by colorblind doctrine, or has the constraint simply redefined the problem out of existence by forbidding the very category (race-conscious action) that alternative readings argue is necessary to solve it? The R5 mismatch surfaces this: founding_problem_status=contested (the parties dispute whether the problem is solved) + disappearance_verdict=world_rearranges (people organized around race-conscious alternatives would immediately challenge the constraint if they could). This mismatch signals mandatrophy: the constraint persists not because the founding problem is solved, but because the dominant reading suppresses the question of whether it is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_vs_constructed_reading,
    'Is the colorblind reading a natural law reading of the constitutional text (the text mandates color-blindness and the reading discovers it), or a constructed reading that selects one interpretive frame from multiple defensible frames the text admits?',
    'Originalist textual analysis by independent scholars comparing the Fourteenth Amendment''s drafting history and linguistic ambiguity against competing readings'' textual justifications. A constructed reading would show multiple defensible framings in the historical record; a natural law reading would show the text unambiguously mandating color-blindness.',
    'If constructed (which is the likely finding), the constraint is a Tangled Rope with false-summit-mountain risk: it appears as settled constitutional principle but depends on suppressing alternative readings. If natural law (unlikely), the constraint genuinely approaches Mountain status and the extraction profile would be incidental rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_constructed_reading, conceptual, 'Whether the colorblind reading is textually mandated or institutionally chosen.').

omega_variable(
    formal_vs_substantive_equality,
    'Does the Fourteenth Amendment''s ''equal protection'' refer to formal equality (identical treatment) or substantive equality (equivalent opportunity and outcome), and is this question resolvable from the text or does it reflect an irreducible choice?',
    'Comparative constitutional law: examine equal protection analogues in other democracies'' constitutions and their interpretations. If diverse democracies with similar texts adopt different principles, the question is not text-resolvable; if convergence exists, formal equality may be discoverable from text.',
    'If the question is text-resolvable to formal equality, colorblind doctrine is principled and the constraint is pure coordination. If text-irreducible, colorblind doctrine is one choice among defensible alternatives and the constraint is Tangled Rope. If text points toward substantive equality, colorblind doctrine contradicts the text and is false doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality, conceptual, 'Whether equal protection is a question the constitutional text answers or leaves open.').

omega_variable(
    historical_discrimination_persistence,
    'Do documented effects of historical discrimination (wealth gaps, educational attainment disparities, health disparities between racial groups) persist materially into the contemporary period as barriers to contemporary equal opportunity, or have they been substantially remedied?',
    'Empirical analysis of wealth, educational, health, and criminal-justice metrics by racial group over time. If gaps persist and worsen after colorblind doctrine adoption, historical discrimination effects are live. If gaps narrow or close, historical discrimination is materially remedied.',
    'If live and worsening, colorblind doctrine fails to address a material equality problem and functions as Tangled Rope covering extraction. If remedied or stable, colorblind doctrine may be correct as coordinating equal protection in a post-discrimination society.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_discrimination_persistence, empirical, 'Whether historical discrimination''s effects remain material barriers to contemporary opportunity.').

omega_variable(
    remedy_vs_entrenching_hierarchy,
    'Do race-conscious admissions policies at selective institutions materially remedy historical exclusion and reduce group-level inequality, or do they entrench existing hierarchies by admitting token members of excluded groups while leaving systemic barriers intact?',
    'Longitudinal study of post-graduation outcomes, career trajectories, wealth accumulation, and intergenerational mobility for affirmative action beneficiaries versus traditional pathways. Assess whether race-conscious admissions materially improve group outcomes or primarily circulate individuals while leaving group-level structures unchanged.',
    'If remedial policies genuinely improve group outcomes, the colorblind reading prevents a functional remedy, strengthening Tangled Rope classification. If remedial policies are primarily circulating individuals and not changing group structures, colorblind doctrine''s restriction is less clearly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_vs_entrenching_hierarchy, empirical, 'Whether race-conscious admissions policies functionally remedy historical exclusion or entrench hierarchies.').

omega_variable(
    colorblindness_as_subordination_mechanism,
    'Can a state policy that is formally neutral as to race nevertheless function as a mechanism for perpetuating racial subordination by ignoring group-level disparities and refusing to address them?',
    'Structural analysis comparing colorblind policies'' outcomes to antisubordination criteria (caste-like stratification, hierarchy-entrenching effects). If colorblind admissions maintain or increase group-level hierarchy despite formal neutrality, the policy violates antisubordination principles even if it satisfies formal equality.',
    'If colorblindness perpetuates subordination, colorblind doctrine contradicts antisubordination reading and the constraint functions as institutionalized hierarchy protection. If colorblindness avoids subordination, the readings remain genuinely contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblindness_as_subordination_mechanism, conceptual, 'Whether formal race-neutrality can itself function as a subordination mechanism.').

omega_variable(
    suppression_of_alternative_readings_necessity,
    'Does the colorblind reading''s dominance require active institutional suppression of remedial and antisubordination readings through case law precedent and judicial gatekeeping, or would it hold through voluntary adoption if presented fairly?',
    'Counterfactual analysis: if the Supreme Court had not foreclosed remedial and antisubordination readings through precedent (e.g., if it had permitted the Fourteenth Amendment to remain genuinely contestable), would colorblind doctrine persist through natural adoption or would it lose dominance? Historical record of brief-gatekeeping and cert petitions refusing to hear cases from alternative-reading advocates provides structural evidence.',
    'If suppression is necessary, the constraint''s persistence depends on coercive institutional gatekeeping and is Tangled Rope. If colorblindness would win fair competition, it approaches purer coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings_necessity, empirical, 'Whether colorblind doctrine''s dominance is active-suppression-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__colorblind_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(equa_tr_t0, projected).
narrative_ontology:measurement(equa_tr_t5, equal_protection_kernel__colorblind_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(equa_tr_t5, projected).
narrative_ontology:measurement(equa_tr_t10, equal_protection_kernel__colorblind_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t15, equal_protection_kernel__colorblind_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(equa_tr_t15, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_kernel__colorblind_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t25, equal_protection_kernel__colorblind_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(equa_tr_t25, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__colorblind_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t35, equal_protection_kernel__colorblind_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement_basis(equa_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__colorblind_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(equa_be_t0, projected).
narrative_ontology:measurement(equa_be_t5, equal_protection_kernel__colorblind_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(equa_be_t5, projected).
narrative_ontology:measurement(equa_be_t10, equal_protection_kernel__colorblind_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t15, equal_protection_kernel__colorblind_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(equa_be_t15, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_kernel__colorblind_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t25, equal_protection_kernel__colorblind_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(equa_be_t25, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__colorblind_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t35, equal_protection_kernel__colorblind_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(equa_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__colorblind_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(equa_su_t0, projected).
narrative_ontology:measurement(equa_su_t5, equal_protection_kernel__colorblind_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(equa_su_t5, projected).
narrative_ontology:measurement(equa_su_t10, equal_protection_kernel__colorblind_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t15, equal_protection_kernel__colorblind_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(equa_su_t15, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_kernel__colorblind_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t25, equal_protection_kernel__colorblind_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(equa_su_t25, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__colorblind_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t35, equal_protection_kernel__colorblind_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(equa_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__colorblind_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-reading kernel (equal_protection_kernel). All three readings share the same constitutional text (Fourteenth Amendment, Equal Protection Clause) but interpret it differently, yielding different beneficiary/victim structures, different extractiveness profiles, and different classifications. The colorblind reading forecloses remedial and coexists with (or influences) antisubordination within the broader legal discourse. Each reading is a separate constraint story with its own ε, its own stakeholders, and its own classification computed per-seat by the engine. The three stories are linked via network.affects_constraints to establish the constraint family and enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
