% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 Negative Liberty Reading
 *   domain: constitutional_law/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the negative liberty reading of the
 *   UDHR Article 3 kernel: the interpretive claim that Article 3 prohibits
 *   state deprivation of life and liberty except via narrow procedural
 *   justice, and that 'security of person' means freedom from state violence
 *   rather than material welfare provision. The constraint governs states
 *   through international and regional human rights mechanisms, extracting
 *   from state security capacity (capital punishment, broad self-defense,
 *   summary detention) while coordinating a universal floor of individual
 *   protection against arbitrary violence. It is claimed as tangled_rope
 *   because it combines genuine coordination (procedural regularity,
 *   anti-torture norms) with asymmetric extraction (restrictions on
 *   legitimate security functions). The metrics are authored independently:
 *   high extractiveness reflects the heavy burden on state security
 *   apparatus; high resistance reflects persistent state pushback against
 *   abolitionist and due-process expansion.
 *
 * KEY AGENTS:
 *   - individual_rights_bearers: Primary beneficiary (organized/constrained) â receive protection from arbitrary state violence
 *   - state_security_apparatus: Primary payer (institutional/constrained) â bears costs of procedural restrictions and abolition
 *   - international_judiciary: Agenda-setter (institutional/analytical) â interprets and enforces Article 3
 *   - human_rights_organizations: Observer (organized/analytical) â monitor and advocate
 *   - retributive_justice_advocates: Excluded voice (moderate/constrained) â support capital punishment and security-priority frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.82).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 Negative Liberty Reading").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'f74e9068-5e76-4d18-98de-6451155c6d35').
narrative_ontology:cs_kernel_codification('f74e9068-5e76-4d18-98de-6451155c6d35', formalized).
narrative_ontology:cs_authority_grounding('f74e9068-5e76-4d18-98de-6451155c6d35', lineage).
narrative_ontology:cs_interpretation_layer_present('f74e9068-5e76-4d18-98de-6451155c6d35').
narrative_ontology:cs_reading_relation('f74e9068-5e76-4d18-98de-6451155c6d35', udhr_article_3__positive_entitlement_reading, forecloses).
narrative_ontology:cs_reading_relation('f74e9068-5e76-4d18-98de-6451155c6d35', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f74e9068-5e76-4d18-98de-6451155c6d35', foundational, security_as_freedom_from_state_violence).
narrative_ontology:cs_axiom_status(security_as_freedom_from_state_violence, holdable).
narrative_ontology:cs_axiom_grounding('f74e9068-5e76-4d18-98de-6451155c6d35', security_as_freedom_from_state_violence, deontological).
narrative_ontology:cs_axiom('f74e9068-5e76-4d18-98de-6451155c6d35', foundational, procedural_justice_as_exclusive_legitimation).
narrative_ontology:cs_axiom_status(procedural_justice_as_exclusive_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('f74e9068-5e76-4d18-98de-6451155c6d35', procedural_justice_as_exclusive_legitimation, conventional).
narrative_ontology:cs_reference_frame('f74e9068-5e76-4d18-98de-6451155c6d35', negative_liberty_framework).
narrative_ontology:cs_drift_state('f74e9068-5e76-4d18-98de-6451155c6d35', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f74e9068-5e76-4d18-98de-6451155c6d35', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_rights_bearers).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, negative_liberty_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, procedural_justice_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals subject to state jurisdiction who gain protection from arbitrary deprivation of life, torture, and unlawful detention through the interpretive requirement that any state violence must meet narrow procedural justice standards. Their exit from the constraint's protection is bounded by citizenship, territoriality, and the lack of alternative legal orders.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individual_rights_bearers, beneficiary,
    organized, biographical, constrained, global).

% State police, military, penal, and intelligence institutions whose capacity to deploy capital punishment, effect summary detention, and exercise broad lethal self-defense is heavily constrained by due process requirements, habeas corpus, and absolute torture prohibitions. They bear the operational, legal, and strategic costs of expansive procedural safeguards and abolitionist norms.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% Regional and international courts and treaty bodies (e.g., ECtHR, IACtHR, UN Human Rights Committee) that interpret Article 3, adjudicate individual petitions against states, and progressively narrow permissible state violence through evolving precedent. They administer and enforce the interpretive framework without bearing its operational costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_judiciary, agenda_setter,
    institutional, generational, analytical, global).

% NGOs and monitoring bodies that document state violations, represent individual petitioners, and lobby for stricter procedural constraints. They observe and amplify the constraint's operation but do not capture its extracted gains.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, human_rights_organizations, observer,
    organized, generational, analytical, global).

% Victims' rights groups and penal hardliners who support capital punishment and broad state self-defense doctrines. They are structurally excluded from the human rights interpretive forums that define Article 3's meaning; their objections are treated as violations rather than legitimate policy positions.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, retributive_justice_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, universal floor prohibiting arbitrary state killing, torture, and detention, coordinating state behavior toward minimum procedural regularity and enabling individuals to anticipate legal protection across jurisdictions.
% TRANSFER_FUNCTION: Transfers discretion over life-and-death decisions and liberty deprivation from state security apparatus to judicial and quasi-judicial oversight bodies; moves the burden of justification from the individual to the state in all deprivation proceedings.
% ABSENT_VOICES: Retributive justice advocates and state security hardliners who view capital punishment and summary detention as legitimate collective security measures are largely excluded from the interpretive forums that define Article 3's meaning; their objections are treated as human rights violations rather than policy positions.
% DISAPPEARANCE_RATIONALE: If the negative liberty reading of Article 3 vanished overnight, states would regain broad discretion to deploy capital punishment, summary detention, and lethal self-defense doctrines without procedural constraint; the international human rights oversight architecture would lose its foundational limit on arbitrary state violence, and individuals would lose the coordinated expectation of judicial protection.
% FOUNDING_PROBLEM: Post-WWII impunity for arbitrary state violence, summary execution, and torture by totalitarian and colonial regimes lacking any procedural check or individual remedy.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of mid-20th century atrocities is corroborated by independent historians and the Nuremberg/Tokyo tribunals. However, the claim that Article 3's negative liberty reading is the necessary or sufficient response is contested by positive-entitlement and security-priority readings. State consent to the founding treaties was often coerced by geopolitical pressure or conditional on reconstruction aid, so corroboration from fully independent non-beneficiaries is partial at best.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint strips states of capital punishment, expansive self-defense, and summary detention â high-cost limitations on sovereign security power. Suppression (0.68) is moderate-high: states comply under judicial and diplomatic coercion but actively resist in many jurisdictions. Theater_ratio (0.45) indicates substantial ritual compliance (reporting, formal adoption) alongside uneven functional implementation. Accessibility_collapse (0.75) is high because once the human rights framework is accepted, arbitrary execution ceases to be a thinkable policy alternative. Resistance (0.72) is high from states and security actors. Measurements track rising extraction and enforcement maturation over 30 years on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The individual beneficiary seat computes the constraint as protective coordination (low chi, possibly rope-leaning). The state security payer seat computes it as heavy extraction (high chi, snare-leaning). The divergence arises from the same structural facts: procedural justice requirements that protect individuals simultaneously disable security measures the state would otherwise deploy. The agenda-setter seat (international judiciary) computes a moderate type because it benefits from the constraint's authority without paying its operational costs.
 *
 * DIRECTIONALITY LOGIC:
 *   individual_rights_bearers are declared beneficiaries: the constraint subsidizes their security by transferring procedural protections to them (low d). state_security_apparatus is declared payer: the constraint extracts operational discretion and policy autonomy from state institutions (high d). international_judiciary sits near symmetric or beneficiary: they administer the constraint without bearing its costs, but their authority is constituted by the constraint's existence. human_rights_organizations are observers with analytical exit. The excluded retributive_justice_advocates would experience high d if seated, but are structurally absent from the interpretive forums.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â arbitrary state violence and torture in the mid-20th century â is partially live (atrocities persist) but also substantially dead in many jurisdictions. The constraint has not atrophied into pure performance (theater_ratio 0.45 shows significant function remains), nor has its mandate fully outlived its problem. Classifying it as tangled_rope prevents the mandatrophy error of treating it as pure coordination (which would ignore the extraction from state security) or pure extraction (which would ignore the genuine protection of individuals from torture and arbitrary killing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is Article 3''s negative liberty reading the only coherent interpretation of ''security of person,'' or does the text under-determine between negative liberty, positive entitlement, and procedural hybrid readings?',
    'Comparative doctrinal analysis across jurisdictions showing whether the text''s ordinary meaning and drafting history settle the liberty/welfare contest.',
    'If the text under-determines, the negative liberty reading''s high extraction is a constructed judicial choice rather than textual necessity; this would reclassify the authority grounding from lineage toward extraction or practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether Article 3 textually mandates negative liberty or under-determines its interpretation').

omega_variable(
    collective_security_cost,
    'Does the negative liberty reading''s restriction on state security capacity (capital punishment abolition, restrictive self-defense) produce measurable net harm to collective security or public safety?',
    'Cross-national longitudinal studies comparing homicide and security indicators in jurisdictions before and after abolition or due process expansion, controlling for confounders.',
    'If restrictive procedural justice demonstrably reduces security outcomes without compensatory benefits, the constraint''s coordination function is weaker than claimed, shifting type toward snare; if outcomes hold or improve, the coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_security_cost, empirical, 'Empirical test of security trade-offs under negative liberty reading').

omega_variable(
    authority_grounding_ambiguity,
    'Does the negative liberty reading''s authority derive from the UDHR text as fixed lineage, or from the interpretive practice of international courts that has progressively expanded procedural requirements beyond original intent?',
    'Drafting history analysis (travaux prÃ©paratoires) vs. living-instrument doctrine in contemporary jurisprudence.',
    'If authority derives primarily from evolving practice, the reference frame is practice-based and the drift is codification_collapse rather than practice_drift; this changes the commitment-system pattern classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, conceptual, 'Whether authority is textual-lineage or practice-based interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(udhr_tr_t6, udhr_article_3__negative_liberty_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(udhr_tr_t12, udhr_article_3__negative_liberty_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(udhr_tr_t18, udhr_article_3__negative_liberty_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(udhr_tr_t24, udhr_article_3__negative_liberty_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__negative_liberty_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(udhr_be_t6, udhr_article_3__negative_liberty_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(udhr_be_t12, udhr_article_3__negative_liberty_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(udhr_be_t18, udhr_article_3__negative_liberty_reading, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(udhr_be_t24, udhr_article_3__negative_liberty_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__negative_liberty_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(udhr_su_t6, udhr_article_3__negative_liberty_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(udhr_su_t12, udhr_article_3__negative_liberty_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(udhr_su_t18, udhr_article_3__negative_liberty_reading, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(udhr_su_t24, udhr_article_3__negative_liberty_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__negative_liberty_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% The UDHR Article 3 kernel decomposes into three structurally distinct constraints because the label 'security of person' conflates negative liberty, positive entitlement, and procedural hybrid claims. Each reading has a different epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by shared kernel origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
