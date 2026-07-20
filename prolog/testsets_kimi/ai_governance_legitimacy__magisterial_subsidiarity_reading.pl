% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__magisterial_subsidiarity_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: AI Governance Legitimacy â Magisterial Subsidiarity Reading
 *   domain: theological ethics / technology governance / political theology
 *
 * SUMMARY:
 *   This constraint story instantiates the magisterial_subsidiarity_reading
 *   of the ai_governance_legitimacy kernel. It models the claim that AI
 *   governance legitimacy derives from conformity to Catholic Social
 *   Doctrineâcommon good, subsidiarity, solidarity, and universal
 *   destination of goodsâas authoritatively interpreted by the Magisterium.
 *   The constraint operates through moral suasion, civil society pressure,
 *   international law advocacy, and ecclesial witness, imposing normative
 *   costs on private tech monopolies, military-industrial actors, and
 *   extractive finance while coordinating protections for workers, the Global
 *   South, families, and marginalized populations. The claim/metric
 *   independence is maintained: the reading is CLAIMED as tangled_rope while
 *   the authored metrics describe moderate extractiveness, rising theater,
 *   and active enforcement through soft-power mechanisms.
 *
 * KEY AGENTS:
 *   - magisterium: Agenda-setter (institutional/civilizational/identity_locked) â interprets and teaches the normative framework, bears institutional identity costs of maintaining doctrinal coherence.
 *   - workers: Beneficiary (organized/biographical/constrained) â receive labor dignity protections against pure automation logic.
 *   - global_south: Beneficiary (moderate/generational/constrained) â shielded from extractive data colonialism by solidarity principles.
 *   - families: Beneficiary (moderate/generational/constrained) â protected by subordination of technology to relational goods.
 *   - marginalized_populations: Beneficiary (powerless/biographical/trapped) â receive preferential protection under the option for the poor.
 *   - private_tech_monopolies: Payer (powerful/biographical/constrained) â bear compliance costs of transparency, accountability, and subordination to dignity.
 *   - military_industrial_complex: Payer (powerful/generational/constrained) â constrained by solidarity and common good principles regarding autonomous weapons.
 *   - extractive_finance: Payer (powerful/biographical/constrained) â opposed by doctrine resisting commodification of persons.
 *   - secular_democratic_pluralists: Excluded (organized/generational/mobile) â alternative legitimacy frameworks sidelined.
 *   - theological_ethicists: Observer (analytical/civilizational/analytical) â assess structural fit between doctrinal authority and governance outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.48).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy â Magisterial Subsidiarity Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological ethics / technology governance / political theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04').
narrative_ontology:cs_kernel_codification('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', formalized).
narrative_ontology:cs_authority_grounding('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', lineage).
narrative_ontology:cs_interpretation_layer_present('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04').
narrative_ontology:cs_reading_relation('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_reading_relation('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', ai_governance_legitimacy__market_libertarian_reading, influences).
narrative_ontology:cs_axiom('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', foundational, human_dignity_as_nonnegotiable_primary).
narrative_ontology:cs_axiom_status(human_dignity_as_nonnegotiable_primary, holdable).
narrative_ontology:cs_axiom_grounding('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', human_dignity_as_nonnegotiable_primary, deontological).
narrative_ontology:cs_axiom('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', foundational, magisterial_authority_as_authentic_interpreter).
narrative_ontology:cs_axiom_status(magisterial_authority_as_authentic_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', magisterial_authority_as_authentic_interpreter, theological).
narrative_ontology:cs_reference_frame('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', catholic_social_doctrine_tradition).
narrative_ontology:cs_drift_state('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', contemporary_ai_governance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c34cd7d6-8ec8-47d2-89ec-0bcd1e7ecd04', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterial_authority).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches and interprets Catholic Social Doctrine as the normative framework for AI governance. Asserts that legitimacy derives from conformity to principles of common good, subsidiarity, solidarity, and universal destination of goods. Exercises moral authority through encyclicals, pastoral guidance, and ecclesial witness, with institutional identity fused to the doctrinal tradition.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Benefit from governance constraints that prioritize labor dignity, decent work conditions, and worker participation over pure automation and efficiency gains driven by AI deployment.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    organized, biographical, constrained, global).

% Benefits from solidarity and universal destination of goods principles that resist extractive data colonialism and demand equitable distribution of AI benefits and decision-making power across nations.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south, beneficiary,
    moderate, generational, constrained, global).

% Protected by governance that subordinates technology to relational goods, rejecting AI systems that instrumentalize family bonds or reduce persons to behavioral data points.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families, beneficiary,
    moderate, generational, constrained, global).

% Receive preferential protection under the option for the poor embedded in the doctrine; the constraint demands AI governance explicitly protect the vulnerable from algorithmic harm and exclusion.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, biographical, trapped, global).

% Bear costs of compliance with governance norms that reject pure profit maximization, unaccountable algorithmic decision-making, and data extraction. Must submit to transparency, participatory oversight, and subordination of technical efficiency to human dignity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, biographical, constrained, global).

% Constrained by solidarity and common good principles that resist autonomous weapons development, demand strict human oversight of military AI applications, and reject the framing of security through unchecked technological escalation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    powerful, generational, constrained, global).

% Faces doctrinal opposition to speculative financial instruments and data monetization regimes that treat persons and their behaviors as commodities to be extracted without regard to the universal destination of goods.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance, payer,
    powerful, biographical, constrained, global).

% Would argue that legitimacy derives from democratic consent and public reason rather than doctrinal conformity, but are structurally sidelined in forums where this legitimacy framework is asserted; their alternative frameworks are treated as inadequate rather than competing.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_democratic_pluralists, excluded,
    organized, generational, mobile, global).

% Analyze the structural relationship between doctrinal authority and governance legitimacy, assessing whether the constraint coordinates genuine protection or extracts compliance through a normative monopoly over AI ethics discourse.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, theological_ethicists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global AI governance toward protection of human dignity, the vulnerable, and the common good by providing a unified, transnational normative framework that resists fragmentation into pure market efficiency or technocratic optimization logics.
% TRANSFER_FUNCTION: Moves authority over AI legitimacy claims from technocratic experts and market actors to magisterial interpreters and civil society advocates aligned with Catholic Social Doctrine; imposes compliance costs on private tech monopolies, military-industrial actors, and extractive finance in the form of transparency, accountability, and subordination-to-dignity requirements.
% ABSENT_VOICES: Secular humanist ethicists, market libertarian technologists, and democratic pluralists who reject magisterial interpretive monopoly are structurally excluded; they would argue for alternative legitimacy sources but are treated as epistemically deficient rather than as participants in a contested debate.
% DISAPPEARANCE_RATIONALE: If the magisterial subsidiarity reading vanished, Catholic-affiliated governance actors and civil society networks would lose their primary normative anchor for AI policy, the specific protections for workers, families, and the Global South embedded in this doctrinal framework would likely be diluted or reframed under secular human-rights language, and the balance between efficiency and dignity in AI governance would shift measurably.
% FOUNDING_PROBLEM: The unchecked rise of AI governance driven by technocratic efficiency and market logics threatens human dignity, displaces workers, concentrates power in unaccountable private and military institutions, and excludes the vulnerable from the design and benefits of technological systems.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by secular AI ethics researchers, labor advocates, international development organizations, and the UN Secretary-General's AI advisory body, who independently identify algorithmic bias, labor displacement, and autonomous weapons as live threats without relying on the magisterial framework.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is mid-high because the constraint imposes significant normative and compliance costs on powerful actors, demanding they subordinate efficiency and profit to doctrinal principles. Suppression (0.48) is moderate: enforcement relies on moral suasion, civil society pressure, and institutional advocacy rather than physical coercion, but the exclusion of secular and market-libertarian voices from legitimacy forums is structurally significant. Theater_ratio (0.38) reflects that ecclesial witness and magisterial statements carry genuine coordination value but also increasing performative maintenance as the Church seeks relevance in AI discourse. Accessibility_collapse (0.42) indicates that while alternatives (technocratic, market, democratic) persist, they are delegitimized within the magisterial framework. Resistance (0.55) is substantial because private tech, military, and finance actors actively contest the constraint's legitimacy. The temporal series show gradual intensification as magisterial AI guidance has become more specific since the mid-2010s.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (magisterium) experiences the constraint as a genuine service of truth and coordination, protecting the vulnerable from unchecked technocracy. The payer seats (tech monopolies, military-industrial, finance) experience it as normative extraction that raises costs and limits strategic options. The beneficiary seats experience protection mixed with dependency on an authority structure they do not control. The engine computes these divergences from the structural data; the authored claim does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (workers, global_south, families, marginalized_populations) have low directionality: the constraint subsidizes their protection and dignity. Victims (private_tech_monopolies, military_industrial_complex, extractive_finance) have high directionality: the constraint extracts compliance and opportunity costs from them. The magisterium is not declared as beneficiary or victim; its canonical fallback directionality reflects administrative position. The excluded secular-democratic seat has mobile exit but faces delegitimization within the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling as pure extraction (snare) because it carries a genuine coordination function: it solves the collective-action problem of protecting vulnerable populations from algorithmic harm by providing a unified, transnational normative language. It prevents mislabeling as pure coordination (rope) because the beneficiary/victim structure is asymmetricâworkers and the Global South are coordinated while tech monopolies and extractive finance bear concentrated costsâand the constraint requires active enforcement (moral suasion, advocacy) to persist. The tangled_rope classification captures this entanglement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterial_authority_scope,
    'Does the Magisterium''s claim to authoritative interpretation extend to technical AI governance details, or only to broad moral principles?',
    'Comparative textual analysis of magisterial documents to determine whether they make specific technical architecture claims or provide only moral framing and normative boundaries.',
    'If authority extends only to broad principles, extractiveness is lower and the coordination function dominates; if it claims technical specificity, extractiveness rises as control over technical domains becomes extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_scope, conceptual, 'Scope of magisterial interpretive authority in technical domains').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is the constraint''s persistence driven by genuine moral consensus across traditions, or by institutional prestige and soft power projection?',
    'Track policy convergence between Catholic-majority and secular jurisdictions on AI governance; measure correlation between magisterial statements and actual regulatory outcomes independent of pre-existing political alignment.',
    'If prestige-driven, theater_ratio is higher and the constraint drifts toward piton; if consensus-driven, the tangled_rope classification stabilizes with genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Whether persistence rests on consensus or institutional soft power').

omega_variable(
    subsidiarity_solidarity_tension,
    'Does the simultaneous demand for subsidiarity (decentralized decision-making) and solidarity (collective obligation to the common good) create internal contradictions that raise extractiveness or theater?',
    'Case studies of AI governance in Catholic-majority jurisdictions to identify whether subsidiarity and solidarity are operationalized coherently or generate performative compliance that masks unresolved structural conflict.',
    'High internal contradiction would increase theater_ratio and resistance, potentially shifting the constraint toward snare or piton dynamics; coherent integration supports the current tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_tension, conceptual, 'Internal coherence of subsidiarity and solidarity in AI governance application').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The natural-language concept 'AI governance legitimacy' decomposes into at least four structurally distinct constraints (readings): magisterial_subsidiarity_reading, technocratic_optimization_reading, democratic_pluralist_reading, and market_libertarian_reading. Each reading asserts a different primary source of legitimacy, derives different beneficiary/victim structures, and carries different epsilon values. This story instantiates the magisterial_subsidiarity_reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
