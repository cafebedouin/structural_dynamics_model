% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: Democratic Pluralist AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the democratic pluralist reading of
 *   the contested kernel 'AI governance legitimacy.' It asserts that
 *   legitimacy derives exclusively from democratic deliberation and consent
 *   of the governed, denying interpretive monopoly to any single tradition —
 *   religious (Magisterium), technocratic (expert optimization), or market
 *   (voluntary exchange). The encyclical's dignity claims are accepted as one
 *   voice in public reason, not as authoritative interpretation. The
 *   constraint operates as a scaffold: it builds participatory infrastructure
 *   (deliberative forums, transparency mandates, judicial review pathways)
 *   meant to mature into self-sustaining democratic governance of AI, with a
 *   sunset clause triggered when participatory norms become entrenched.
 *   Beneficiaries are civil society, democratic institutions, and minority
 *   rights holders who gain voice and protection; victims are those excluded
 *   from deliberation (structurally marginalized populations, authoritarian
 *   regime subjects) who bear AI harms without representation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.35).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "Democratic Pluralist AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, '43ca778e-8c8a-4ff0-b8b0-9c9c035582e2').
narrative_ontology:cs_kernel_codification('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', distributed).
narrative_ontology:cs_authority_grounding('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', practice).
narrative_ontology:cs_interpretation_layer_present('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2').
narrative_ontology:cs_reading_relation('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', foundational, democratic_consent_as_sole_legitimacy_source).
narrative_ontology:cs_axiom_status(democratic_consent_as_sole_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', democratic_consent_as_sole_legitimacy_source, deontological).
narrative_ontology:cs_axiom('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', foundational, no_single_tradition_holds_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_single_tradition_holds_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', no_single_tradition_holds_interpretive_monopoly, conventional).
narrative_ontology:cs_reference_frame('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', democratic_consent_framework).
narrative_ontology:cs_drift_state('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', contemporary_ai_acceleration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43ca778e-8c8a-4ff0-b8b0-9c9c035582e2', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, democratic_legitimacy_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, public_reason_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, pluralist_interpretive_parity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain structured access to AI governance deliberations through participatory mechanisms; their advocacy capacity is amplified by transparency requirements and consultation mandates. Exit means shifting focus to other issue domains or jurisdictions.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Administer electoral accountability, judicial review, and civil liberties frameworks that constrain AI governance. They set the procedural rules for public deliberation and are themselves constrained by those rules through elections and constitutional review.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Receive protection from majoritarian AI harms through inclusive deliberation requirements and anti-discrimination safeguards. Their exit options are limited by structural marginalization; they depend on the constraint's enforcement for voice.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, constrained, national).

% Bear the costs of AI systems deployed without their consent or input — algorithmic discrimination, surveillance, automation displacement. They are structurally excluded from the deliberative processes the constraint creates, lacking literacy, access, or standing.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_populations, payer,
    powerless, biographical, trapped, global).

% Subject to AI governance imposed by non-democratic states without deliberative legitimacy. The constraint's promise of democratic consent is unavailable to them; they bear extraction without the coordination benefit.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, generational, trapped, global).

% Claims unique interpretive authority over human dignity in AI ethics based on Catholic Social Doctrine. Would object to the denial of its interpretive monopoly but participates in public discourse as one voice among many.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_authority, excluded,
    institutional, civilizational, analytical, global).

% Provide epistemic input to democratic deliberation on AI capabilities, risks, and feasibility. Their authority is advisory, not decisive; they can exit to private sector or international bodies but lose policy influence.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technical_experts, observer,
    powerful, biographical, analytical, global).

% AI developers and deployers whose innovation incentives are shaped by democratic regulation. They lobby, litigate, and can relocate jurisdictions; their consent is not required for legitimacy but their compliance is necessary for effectiveness.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, market_actors, observer,
    powerful, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI governance legitimacy across pluralistic societies by establishing democratic deliberation and consent of the governed as the sole legitimate source of authority, replacing monopolistic claims by religious, technocratic, or market traditions.
% TRANSFER_FUNCTION: Transfers interpretive authority over AI ethics from centralized authorities (Magisterium, technical elites, market mechanisms) to distributed public reason, moving decision-power into transparent political processes that balance diverse values.
% ABSENT_VOICES: Authoritarian regime populations, technically illiterate citizens, future generations, and non-human stakeholders are structurally excluded from current AI governance deliberations; they would object to decisions made without their representation but lack standing in the deliberative architecture.
% DISAPPEARANCE_RATIONALE: If the democratic legitimacy constraint vanished, AI governance would default to technocratic optimization, market libertarian exit, or authoritarian imposition without public accountability — democratic institutions and civil society would lose their primary leverage over AI trajectory.
% FOUNDING_PROBLEM: The problem of legitimate AI governance in pluralistic societies where no single moral tradition commands universal assent, requiring a framework that respects diverse values while enabling collective decision-making on systems that affect all.
% FOUNDING_PROBLEM_CORROBORATION: Political philosophers (Rawls, Habermas), democratic theorists, and international human rights frameworks corroborate the enduring need for public reason in technological governance; the Magisterium and technocratic elites contest the exclusivity of democratic deliberation as legitimacy source.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.40) reflects moderate costs of participatory processes — time, resources, expertise — distributed across society, not concentrated on a single group. Suppression (0.35) captures the constraint's coercive dimension: non-democratic AI governance is actively suppressed through legal and political mechanisms, but alternatives remain legally expressible. Theater ratio (0.20 declining from 0.35) models initial performative inclusion giving way to substantive participation as infrastructure matures. Accessibility collapse (0.45) indicates alternatives (technocratic, market, authoritarian governance) remain conceptually available but are politically delegitimized. Resistance (0.50) reflects active contestation from all three sibling readings. The scaffold classification fits: the constraint builds temporary support for democratic AI governance, with sunset when participatory norms stabilize.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (excluded populations) experience the constraint as a snare — promises of inclusion unfulfilled, suppression without coordination benefit. The agenda_setter seat (democratic institutions) experiences it as a rope — genuine coordination of pluralistic legitimacy. The beneficiary seats (civil society, minorities) experience it as a scaffold — transitional support toward full inclusion. The engine computes this divergence from the structural data; the authored claim (scaffold) reflects the institutional design intent, not the lived reality of all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic institutions (agenda_setter) sit near symmetric (d≈0.5): they administer the constraint but are bound by electoral accountability. Civil society and minority rights holders (beneficiaries) sit at low d (≈0.15-0.25): they receive voice and protection without bearing disproportionate cost. Deliberatively excluded populations and authoritarian regime subjects (payers) sit at high d (≈0.8-0.9): they bear AI harms without the deliberative benefit, trapped by structural exclusion. Magisterial authority (excluded) is not a target of extraction but a denied monopolist — its directionality is analytical. Technical experts and market actors (observers) sit near symmetric with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling democratic coordination as pure extraction by explicitly building participatory infrastructure with sunset. The mandatrophy risk is that the scaffold becomes permanent (piton) if participatory norms fail to entrench, or that the sunset clause is captured by incumbents. The analysis distinguishes: coordination function = inclusive deliberation solving pluralistic legitimacy; extraction = compliance costs on actors who would prefer other governance modes. The constraint is mandatrophy-resolved only if sunset triggers on measurable participatory entrenchment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does this reading''s structural profile (ε=0.40, scaffold, beneficiaries/victims as declared) differ from sibling readings of the same kernel, and what does the contest imply about the kernel''s irreducible ambiguity?',
    'Author sibling constraint stories with their own ε, beneficiaries, victims, and types; compare structural profiles. The kernel''s ambiguity is irreducible if no single reading captures the full constraint field.',
    'If sibling readings show substantially different ε and type classifications, the kernel label ''AI governance legitimacy'' conflates structurally distinct constraints — confirming ε-invariance requires per-reading decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committers-frame structural delta between this reading and its siblings on the shared kernel.').

omega_variable(
    deliberative_effectiveness_empirical,
    'Can inclusive democratic deliberation actually govern AI development at the speed and technical complexity required, or does the constraint''s coordination function collapse under empirical conditions?',
    'Longitudinal study of AI policy outcomes in jurisdictions with strong participatory mechanisms vs. technocratic/executive-dominated systems; measure decision quality, speed, and legitimacy perception.',
    'If deliberation proves ineffective at governing AI, the scaffold''s coordination function fails — the constraint becomes extractive (costs without benefits) or a piton (performative participation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_effectiveness_empirical, empirical, 'Whether the scaffold''s coordination function is empirically viable under AI''s technical and temporal demands.').

omega_variable(
    exclusion_mechanism_structural_vs_internalized,
    'Is the exclusion of marginalized populations from AI deliberation primarily structural (access barriers, literacy gaps) or internalized (alienation, distrust, identity fusion with powerlessness)?',
    'Post-intervention suppression trajectory: if deliberative access improvements (literacy programs, participatory budgets, digital inclusion) fail to increase participation, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression on payer seats is higher than structural measures suggest — the constraint carries its exclusion forward even after formal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized mechanism of deliberative exclusion for victim populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_dem_pluralist_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ai_gov_dem_pluralist_tr_t10, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(ai_gov_dem_pluralist_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(ai_gov_dem_pluralist_tr_t30, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(ai_gov_dem_pluralist_tr_t40, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(ai_gov_dem_pluralist_tr_t50, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_gov_dem_pluralist_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_gov_dem_pluralist_be_t10, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(ai_gov_dem_pluralist_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(ai_gov_dem_pluralist_be_t30, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(ai_gov_dem_pluralist_be_t40, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(ai_gov_dem_pluralist_be_t50, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_dem_pluralist_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_gov_dem_pluralist_su_t10, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(ai_gov_dem_pluralist_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(ai_gov_dem_pluralist_su_t30, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 30, 0.33).
narrative_ontology:measurement(ai_gov_dem_pluralist_su_t40, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(ai_gov_dem_pluralist_su_t50, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 50, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__democratic_pluralist_reading, 0.08).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% Kernel ai_governance_legitimacy decomposes into four readings with distinct ε and type profiles: democratic pluralist (scaffold, ε≈0.40), magisterial subsidiarity (tangled_rope, ε≈0.55), technocratic optimization (rope→tangled_rope drift, ε≈0.30→0.50), market libertarian (rope, ε≈0.25). This reading's denial of interpretive monopoly structurally pressures the magisterial reading's authority claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__democratic_pluralist_reading, institutional, 0.35).
constraint_indexing:directionality_override(ai_governance_legitimacy__democratic_pluralist_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
