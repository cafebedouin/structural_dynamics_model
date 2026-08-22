% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: Article 3 UDHR: Procedural Due Process Hybrid
 *   domain: constitutional/human rights
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights (and its parallel
 *   in the International Covenant on Civil and Political Rights) guarantees
 *   'the right to life, liberty and the security of person' with two primary
 *   interpretive traditions: a negative-liberty reading (prohibition on state
 *   violence and deprivation except via due process) and a
 *   positive-entitlement reading (obligation to provide material conditions
 *   for security). This constraint story instantiates a third reading: the
 *   procedural-hybrid approach, which focuses on due process protections
 *   (habeas corpus, torture prohibition, judicial review) without resolving
 *   the substantive question of whether 'security' is primarily a negative
 *   right (freedom from state violence) or a positive entitlement (provision
 *   of welfare, healthcare, housing). The hybrid reading is the dominant
 *   operational framing in international human rights law: courts and
 *   monitoring bodies focus on procedural legitimacy and torture prohibition
 *   rather than on adjudicating the liberty/welfare contest. The key tension
 *   is that a state can fully comply with this reading's procedural
 *   requirements while detention systems are filled with persons whose
 *   criminalization is driven by poverty, illness, and lack of access to
 *   counsel — the constraint protects against torture and requires judicial
 *   review but does not resolve whether states must address the conditions
 *   that generate the detention docket.
 *
 * KEY AGENTS:
 *   - detained_persons: powerless, immediate horizon, trapped exit — experience procedural protections as real but incomplete, access to habeas but not to counsel
 *   - states_exercising_detention_power: institutional, generational horizon, arbitrage exit — set detention policy and experience constraint as procedural discipline, not mandate to redistribute
 *   - judicial_system_actors: institutional, generational horizon, mobile exit — benefit from role as guardians of procedural legitimacy without requirement to address substantive inequality
 *   - torture_prohibition_advocates: organized, generational horizon, mobile exit — win bright-line prohibition rule; constraint vindicates their framing without requiring welfare-state responsibilities
 *   - security_establishment: powerful, biographical horizon, constrained exit — bear cost of procedural review and human dignity standards; experience constraint as operational friction
 *   - poor_and_marginalized: powerless, immediate horizon, trapped exit — overrepresented in detention; guaranteed procedure but not resources or material conditions preventing criminalization
 *   - international_monitoring_bodies: institutional, generational horizon, analytical exit — observe and report on compliance with procedure and torture prohibition without adjudicating substantive entitlements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.38).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "Article 3 UDHR: Procedural Due Process Hybrid").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional/human rights").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '8c5ff34a-1e3b-4152-a865-c5308b6ba113').
narrative_ontology:cs_kernel_codification('8c5ff34a-1e3b-4152-a865-c5308b6ba113', fixed_text).
narrative_ontology:cs_authority_grounding('8c5ff34a-1e3b-4152-a865-c5308b6ba113', lineage).
narrative_ontology:cs_interpretation_layer_present('8c5ff34a-1e3b-4152-a865-c5308b6ba113').
narrative_ontology:cs_reading_relation('8c5ff34a-1e3b-4152-a865-c5308b6ba113', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c5ff34a-1e3b-4152-a865-c5308b6ba113', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('8c5ff34a-1e3b-4152-a865-c5308b6ba113', foundational, procedure_legitimates_detention).
narrative_ontology:cs_axiom_status(procedure_legitimates_detention, holdable).
narrative_ontology:cs_axiom_grounding('8c5ff34a-1e3b-4152-a865-c5308b6ba113', procedure_legitimates_detention, deontological).
narrative_ontology:cs_axiom('8c5ff34a-1e3b-4152-a865-c5308b6ba113', foundational, torture_absolutely_prohibited).
narrative_ontology:cs_axiom_status(torture_absolutely_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('8c5ff34a-1e3b-4152-a865-c5308b6ba113', torture_absolutely_prohibited, deontological).
narrative_ontology:cs_reference_frame('8c5ff34a-1e3b-4152-a865-c5308b6ba113', procedural_judicial_restraint).
narrative_ontology:cs_drift_state('8c5ff34a-1e3b-4152-a865-c5308b6ba113', contemporary_detention_inequality_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c5ff34a-1e3b-4152-a865-c5308b6ba113', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, judicial_system_actors).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, rule_of_law_tradition).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, detained_persons).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, marginalized_groups_lacking_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, torture_prohibition_advocates).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, security_establishment).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, procedural_justice_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_corpus_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to detention and must navigate judicial review processes to challenge detention. The constraint guarantees they cannot be tortured and have access to habeas corpus review, but does not guarantee adequate legal representation, timely hearing, or material conditions of dignity. A detained person experiences the procedural protections as real but incomplete — the constraint prevents the worst (torture) but does not remedy poverty, illness, or inability to afford counsel that may have led to detention.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detained_persons, payer,
    powerless, immediate, trapped, universal).

% Sets detention policy and judicial review procedures. The constraint binds them to provide habeas corpus access and to refrain from torture, but permits emergency detention under procedurally defined conditions and does not require them to solve underlying poverty or inequality that generates the detention docket. They experience the constraint as a procedural discipline on state power, not a mandate to redistribute or provide welfare.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_exercising_detention_power, agenda_setter,
    institutional, generational, arbitrage, universal).

% Administer habeas corpus and review detention legality. They gain institutional legitimacy and jurisdictional scope from being the guardians of procedural due process. The constraint elevates their role as the gatekeepers of legality without requiring them to address substantive inequalities in access to justice (cost of counsel, delay, bias in initial arrest decisions).
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, judicial_system_actors, beneficiary,
    institutional, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, judicial_system_actors, agenda_setter).

% Campaign for strong, unambiguous torture bans. The constraint delivers this through absolute prohibition and international oversight. They benefit from having a bright-line rule that cannot be justified by emergency or necessity. The constraint vindicates their position without requiring states to address the poverty-driven detention patterns that generate pressure for detention.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, torture_prohibition_advocates, beneficiary,
    organized, generational, mobile, global).

% Carries the cost of procedural review and torture prohibition compliance. Must design detention facilities to meet human dignity standards, provide judicial access, and absorb judicial determinations of illegality. They experience the constraint as a limitation on their operational flexibility — the procedural requirements slow detention and force articulation of grounds — without addressing their underlying security mandate.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, security_establishment, payer,
    powerful, biographical, constrained, universal).

% Overrepresented in detention systems due to poverty, lack of legal resources, and systemic bias. The constraint guarantees them procedural review and torture prohibition but does NOT guarantee counsel, adequate time to prepare defense, or the material conditions (housing, healthcare) that might prevent criminalization. They experience the constraint as partial protection: real in preventing torture, inadequate in preventing or remedying detention born from poverty.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, marginalized_groups_lacking_resources, payer,
    powerless, immediate, trapped, universal).

% Would argue for emergency detention authorities beyond what Article 3's procedural limits permit. They are excluded from setting the bounds of the constraint itself, though they exercise enormous practical influence over detention systems in practice. Their position — that security sometimes overrides procedure — is not seated at the constraint-setting table.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, national_security_states, excluded,
    institutional, generational, trapped, global).

% Argue that Article 3 should be read as obligating states to provide material conditions (housing, healthcare, legal aid) necessary for genuine security and life. They are not seated in the procedural framework this reading instantiates, though they lobby for reinterpretation. The procedural hybrid reading does not foreclose their position; it simply does not resolve it.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, positive_welfare_reading_advocates, excluded,
    organized, generational, mobile, global).

% Monitor state compliance with Article 3 through regional courts, treaty bodies, and fact-finding missions. They assess whether torture prohibitions are enforced, whether habeas corpus access is real, and whether emergency detention procedures are genuinely limited. They do not design the substantive entitlements question but track whether the procedural protections are meaningful in operation.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, international_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, judicial_system_actors).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared procedural standard for detention legitimacy across signatory states: detentions must be reviewable by an independent body (habeas corpus), torture is absolutely prohibited, and emergency detention is limited to defined circumstances with judicial check. This solves the coordination problem of how states with different substantive political orders can agree on a minimum baseline for permissible constraint on bodily liberty.
% TRANSFER_FUNCTION: Moves decision-making power from state executives and security forces to judicial actors and international monitoring bodies. It also deflects pressure to address substantive inequality (poverty-driven criminalization) onto the procedural machinery: states can comply with Article 3 while detention patterns worsen as poverty deepens, because the constraint does not require states to solve poverty. The constraint shifts legitimacy narrative from 'is detention justified?' to 'was detention procedurally legitimate?'
% ABSENT_VOICES: Detained persons who lack resources for effective legal representation or counsel; advocates for positive welfare obligations; states arguing that security threats sometimes justify suspension of procedural review; persons in extreme poverty whose material deprivation drives criminalization but who are excluded from the substantive framing of the constraint.
% DISAPPEARANCE_RATIONALE: If Article 3's procedural protections vanished, detention systems would operate without international constraint on torture and without meaningful habeas access — the material reality of detention would shift visibly toward higher severity and opacity. However, the disappearance would NOT solve the underlying substantive inequality that drives detention patterns. A world without Article 3 would have less procedural accountability but the same structural drivers of poverty-correlated imprisonment.
% FOUNDING_PROBLEM: Post-WWII humanitarian crisis and documentation of systematic torture in state detention. The founding problem is how to prevent torture and ensure that detention decisions are reviewable by someone other than the detaining authority itself.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international monitoring bodies, and states party to the Covenant affirm that torture remains a live threat — systematic torture is documented annually in at least 40 countries. Regional courts and treaty bodies treat the founding problem (preventing torture, ensuring habeas access) as the core mandate of Article 3. However, these same sources increasingly note that the founding problem's RESOLUTION is incomplete: procedural protections are meaningless if detention dockets are filled with persons whose criminalization is driven by poverty, and the constraint does not address that layer.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the procedural hybrid reading carries genuine coordination function (states agreeing on torture prohibition and habeas corpus baseline) and real protection (torture rates are lower where Article 3 is enforced), but also deflects pressure to address poverty-driven criminalization. The constraint prevents the worst (torture) while permitting structural inequality to drive the detention docket. Suppression is moderate (0.52) because the constraint's enforcement depends on judicial independence and international monitoring — real oversight exists but with significant variation across jurisdictions and vulnerability to state pressure during security crises. Theater rises over time (0.15 to 0.28) as states develop increasingly elaborate procedural compliance mechanisms (rights advisories, appeal processes, torture prevention training) while detention patterns persist or worsen relative to underlying inequality. The measurement series shows the constraint stabilizing in operation over 80 years: initial uncertainty about implementation gives way to routinized procedural machinery that is genuinely enforced in many jurisdictions but increasingly experienced as disconnected from substantive justice by detained persons and advocates. The shared time grid ensures every metric is authored at every point, enabling temporal analysis of the constraint's lifecycle.
 *
 * PERSPECTIVAL GAP:
 *   The judicial system and state-level beneficiaries experience the procedural protections as genuine coordination that solves the humanitarian baseline problem. They frame the constraint as 'we agree on torture prohibition and habeas access, and within those bounds, each jurisdiction can set its own substantive approach to criminalization.' From below, detention systems appear as mechanisms that procedurally legitimate detention while leaving poverty-driven criminalization unconstrained. A detained person without counsel experiences the habeas process as more theater than substance — the right to review exists but the capacity to use it effectively is missing. This is the key mandatrophy question: was Article 3 built to solve 'how do we prevent torture' or 'how do we ensure dignity and security in detention'? The constraint's operation answers the first and leaves the second. International monitoring bodies increasingly report this gap: torture prohibition rates are high, habeas access is formally guaranteed, detention conditions are improving in real terms — yet detention populations are rising and remain concentrated among the poorest groups. The procedural hybrid reading does not resolve this; it manages it by framing the constraint as procedural discipline rather than substantive entitlements.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and marginalized groups are the structural targets (d near 1.0): they are trapped, powerless, immediate horizon, and the constraint provides procedure but not material remedy for the conditions that criminalize them. Security establishment is a secondary payer (d around 0.7): they bear operational costs but are powerful and have arbitrage through emergency authorities and political pressure. Judicial actors are structural beneficiaries (d near 0.1): they gain institutional role and legitimacy without being required to address underlying inequality. States are partially beneficiary (d around 0.3): they coordinate legitimacy and avoid the worst (torture) but carry operational costs. Torture-prohibition advocates and international monitors are near-beneficiary (d around 0.15-0.2): they win their core requirement without having to engage the substantive entitlements debate. The override-free derivation from beneficiary/victim declarations and exit options should produce these directionalities: trapped/powerless payers end up as targets; institutional actors with mobile or arbitrage exit end up as beneficiaries or partial payers. The directionality pattern reflects the hybrid structure: real coordination and protection (beneficiary seats) coexist with deflected substantive justice questions (target seats).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of mandatrophy via theater increase: over the 80-year interval, states have built increasingly elaborate procedural compliance infrastructure (torture-prevention training, appeal mechanisms, rights advisories) while detention populations and detention-driven inequality have persisted or worsened. From the payer seats (detained persons, marginalized groups), this appears as a constraint whose original function (preventing torture, ensuring minimum dignity) is being achieved, but whose secondary function (preventing or remedying detention-through-poverty) is not being addressed — and the procedural theater fills the gap. The founding problem was torture and procedural arbitrary deprivation. That problem is substantially solved in many jurisdictions: torture is rare where Article 3 is enforced, habeas access is real. But the constraint's operation now includes a large secondary function: legitimating detention systems that are structurally unequal by virtue of poverty. States experience the constraint as solved (they have procedures, torture prohibition is in place); detained persons and advocacy groups experience it as incomplete (procedures work but underlying criminalization patterns persist). The theater_ratio rise (0.15 to 0.28) reflects the growth of procedural theater: more training, more documentation, more appeals — all real but experienced by payer seats as disconnected from the actual determinants of detention. The constraint is not becoming inert (it still prevents torture, still requires habeas); it is becoming a hybrid that coordinates procedural baseline while deflecting pressure to address substantive inequality. This is not quite mandatrophy — the constraint is still functional — but it is the condition that enables mandatrophy to approach: if procedural compliance becomes universal while substantive inequality persists, the next move is either doctrinal expansion (courts reading Article 3 as substantive entitlements, moving toward positive_entitlement_reading) or institutional collapse (procedures are seen as theater and public legitimacy erodes). The constraint is stable now but structurally vulnerable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_procedural_frontier,
    'Where is the line between procedural protections Article 3 guarantees (torture prohibition, habeas access) and substantive entitlements (legal counsel quality, material conditions, poverty remedies) it does not mandate?',
    'Evolution of regional court jurisprudence and treaty body recommendations: if courts increasingly read welfare provision into Article 3, the frontier moves toward positive entitlements; if courts maintain the procedural boundary, the frontier remains procedural. The 80-year measurement series shows theater rising while substantive detention patterns persist — continued data on this gap will establish whether courts move doctrine or legitimacy erodes.',
    'If the frontier moves toward positive entitlements, this reading becomes the negative_liberty_reading''s sibling and the positive_entitlement_reading becomes the dominant frame. If the frontier stabilizes as procedural, mandatrophy risk increases: procedures become theater while substantive inequality persists unconstrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_procedural_frontier, conceptual, 'Whether Article 3 protection is primarily procedural or requires substantive entitlements.').

omega_variable(
    poverty_criminalization_causality,
    'Is the overrepresentation of poor and marginalized persons in detention systems caused by poverty-driven criminalization (the constraint does not address the cause) or by discrimination in applying procedural protections (the constraint should address enforcement gaps)?',
    'Comparative empirical study: jurisdictions with identical Article 3 compliance but different poverty levels and social spending; measure whether detention patterns correlate with poverty independent of procedural enforcement variance. If poverty-driven causality is confirmed, the constraint''s limitation becomes structural: procedures cannot remedy patterns caused by poverty.',
    'If poverty-criminalization causality dominates, the constraint is correctly scoped as procedural but incomplete in addressing security; policy response is social welfare, not procedure reform. If enforcement-gap causality dominates, the constraint requires stronger implementation machinery. The distinction determines whether mandatrophy is inevitable (poverty) or preventable (procedure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(poverty_criminalization_causality, empirical, 'Whether detention inequality is driven by poverty-causation or procedure-enforcement gaps.').

omega_variable(
    torture_prohibition_enforcement_variance,
    'Is torture prohibition enforcement actually stable across signatory states, or is compliance stratified by state power and geopolitical position, creating a de facto two-tier system?',
    'Systematic mapping of torture allegations by state power level, regional monitoring body capacity, and international enforcement risk: if torture rates correlate with state power and enforcement capacity rather than formal signatory status, the constraint has a hidden extraction structure favoring powerful states.',
    'If enforcement is genuinely universal, the constraint is as described: coordination on torture prohibition. If enforcement is stratified by power, the constraint carries hidden asymmetry: weak states face real enforcement costs while powerful states face lower enforcement risk. This would reveal the constraint as a snare for powerless states masked as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(torture_prohibition_enforcement_variance, empirical, 'Whether torture-prohibition enforcement is uniform or stratified by state power.').

omega_variable(
    procedural_theater_causality,
    'Does the measured rise in theater_ratio (0.15 to 0.28) reflect genuine improvement in procedural legitimacy and detained persons'' experience, or is it procedural elaboration that does not improve outcomes?',
    'Time-series survey data on detained persons'' experience of habeas process effectiveness, counsel quality, and outcome changes; measure whether theater growth correlates with improvements in actual case outcomes or only with documentation and rights-advisories without outcome change.',
    'If theater correlates with outcome improvement, the constraint is working better over time. If theater grows while outcomes stagnate, mandatrophy is approaching: procedures are becoming theatrical legitimation devices. The distinction determines whether the constraint is stable or requires intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_theater_causality, empirical, 'Whether procedural theater growth reflects real improvement or legitimacy window-dressing.').

omega_variable(
    coexistence_vs_foreclose_boundary,
    'Can the negative_liberty_reading (security as freedom from state violence) and positive_entitlement_reading (security as material provision) genuinely coexist as readings of the same Article 3, or do they logically foreclose each other?',
    'Constructive test: can a single institutional framework (a regional court, a state constitution) authentically hold both readings as live positions without contradiction? Or does moving toward one require retreating from the other? The empirical test is whether any jurisdiction has maintained both simultaneously or if movement is unidirectional.',
    'If they coexist, the procedural hybrid reading''s ''coexists_with'' characterization of both is correct. If they foreclose each other (forcing a movement from negative to positive or vice versa), the procedural hybrid reading is mischaracterized as hybrid rather than as a way station between foreclosing positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_vs_foreclose_boundary, conceptual, 'Whether the liberty/welfare readings of Article 3 logically coexist or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(udhr_tr_t0, observed).
narrative_ontology:measurement(udhr_tr_t10, udhr_article_3__procedural_hybrid_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(udhr_tr_t10, observed).
narrative_ontology:measurement(udhr_tr_t20, udhr_article_3__procedural_hybrid_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(udhr_tr_t20, observed).
narrative_ontology:measurement(udhr_tr_t40, udhr_article_3__procedural_hybrid_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(udhr_tr_t40, observed).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__procedural_hybrid_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(udhr_tr_t60, observed).
narrative_ontology:measurement(udhr_tr_t80, udhr_article_3__procedural_hybrid_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(udhr_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(udhr_be_t0, observed).
narrative_ontology:measurement(udhr_be_t10, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(udhr_be_t10, observed).
narrative_ontology:measurement(udhr_be_t20, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(udhr_be_t20, observed).
narrative_ontology:measurement(udhr_be_t40, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(udhr_be_t40, observed).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement_basis(udhr_be_t60, observed).
narrative_ontology:measurement(udhr_be_t80, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(udhr_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(udhr_su_t0, observed).
narrative_ontology:measurement(udhr_su_t10, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(udhr_su_t10, observed).
narrative_ontology:measurement(udhr_su_t20, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(udhr_su_t20, observed).
narrative_ontology:measurement(udhr_su_t40, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement_basis(udhr_su_t40, observed).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement_basis(udhr_su_t60, observed).
narrative_ontology:measurement(udhr_su_t80, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement_basis(udhr_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% The procedural_hybrid_reading is one of three structurally distinct constraint stories generated from the contested kernel udhr_article_3. The negative_liberty_reading interprets Article 3 as a prohibition on state deprivation except via due process and treats security as freedom from state violence (low extractiveness, mountain or rope type). The positive_entitlement_reading interprets Article 3 as obligating state provision of material conditions (housing, healthcare, counsel) and treats security as material security (higher extractiveness, tangled_rope or snare type as states resist welfare expansion). The procedural_hybrid_reading (this story) focuses on due process protections without resolving the liberty/welfare question, enabling coexistence of both readings in practice. Each reading has distinct ε, distinct beneficiary/victim structures, and distinct procedural/substantive emphasis. They are linked via network.affects_constraints because each reading's instantiation as doctrine in a jurisdiction influences the operational environment for the other readings — courts moving toward positive entitlements create pressure on states to resist the hybrid reading, expanding torture-prohibition enforcement while constraining welfare expansion. The three readings do not average to one constraint; they are three separate constraint stories with independent ε values and temporal trajectories. The decomposition follows the ε-invariance principle: a single constraint story cannot have two substantially different ε values depending on which interpretive frame is adopted; instead, each frame is authored as a separate story with its own stable ε relative to the standing arrangement under that reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, powerless, 0.92).
constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
