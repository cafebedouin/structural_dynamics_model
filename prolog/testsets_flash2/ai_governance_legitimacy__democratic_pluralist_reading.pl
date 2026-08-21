% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: AI Governance Legitimacy: Democratic Pluralist Reading
 *   domain: political_theology/technology_governance/ethics
 *
 * SUMMARY:
 *   This constraint describes the democratic pluralist reading of AI
 *   governance legitimacy, where authority derives from inclusive public
 *   deliberation and consent, rather than from a single religious or
 *   technocratic tradition. It functions as a scaffold, aiming to build
 *   robust participatory infrastructure for AI ethics. The encyclical's
 *   contribution is acknowledged as one voice among many, not as a definitive
 *   interpretive authority. This reading accepts the dignity claims but
 *   denies any unique Magisterial authority to interpret them.
 *
 * KEY AGENTS:
 *   - democratic_institutions: Agenda setter (institutional/constrained)
 *   - civil_society_organizations: Beneficiary (organized/mobile)
 *   - minority_rights_holders: Beneficiary (powerless/constrained)
 *   - excluded_populations_from_deliberation: Payer (powerless/trapped)
 *   - populations_under_authoritarian_regimes: Payer (powerless/trapped)
 *   - religious_authorities: Excluded (institutional/constrained)
 *   - technocratic_elites: Excluded (powerful/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.3).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "AI Governance Legitimacy: Democratic Pluralist Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "political_theology/technology_governance/ethics").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, '40eb99bb-167a-4cf5-bbaf-907b00f57fd0').
narrative_ontology:cs_kernel_codification('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', distributed).
narrative_ontology:cs_authority_grounding('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', practice).
narrative_ontology:cs_interpretation_layer_present('40eb99bb-167a-4cf5-bbaf-907b00f57fd0').
narrative_ontology:cs_reading_relation('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', foundational, legitimacy_from_consent).
narrative_ontology:cs_axiom_status(legitimacy_from_consent, holdable).
narrative_ontology:cs_axiom_grounding('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', legitimacy_from_consent, deontological).
narrative_ontology:cs_axiom('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', foundational, no_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', no_interpretive_monopoly, conventional).
narrative_ontology:cs_reference_frame('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', inclusive_public_reason_framework).
narrative_ontology:cs_drift_state('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('40eb99bb-167a-4cf5-bbaf-907b00f57fd0', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, excluded_populations_from_deliberation).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for establishing and maintaining deliberative processes for AI governance, ensuring broad participation and accountability. They benefit from enhanced legitimacy and public trust.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Actively participate in and facilitate democratic deliberation, advocating for diverse values and ensuring inclusive processes. They benefit from their voices being heard and integrated into policy.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Their values and concerns are explicitly sought and protected through inclusive deliberative processes, preventing their marginalization in AI development and deployment. They benefit from having their interests represented.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, generational, constrained, local).

% Bear the costs of AI systems designed without their input or consideration, leading to potential harms or exacerbation of existing inequalities. Their exclusion undermines the legitimacy claim.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, excluded_populations_from_deliberation, payer,
    powerless, generational, trapped, local).

% Are subject to AI governance models that lack democratic input and consent, potentially leading to surveillance, control, and suppression of dissent. They bear the full cost of non-legitimate governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, generational, trapped, national).

% Their voice is considered one among many in the deliberative process, without a claim to unique interpretive monopoly on AI ethics. They are excluded from a position of sole authority but can participate as stakeholders.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, religious_authorities, excluded,
    institutional, civilizational, constrained, global).

% Their expertise is valued but does not grant them sole authority over AI governance principles. They are expected to contribute to, rather than dictate, the deliberative process. They are excluded from an optimization-only approach.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_elites, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse values and interests into a coherent, publicly reasoned framework for AI governance, preventing fragmentation and ensuring broad societal acceptance.
% TRANSFER_FUNCTION: Transfers authority for ethical principle-setting from singular traditions or expert groups to inclusive, transparent political processes, distributing the burden and benefit of legitimacy.
% ABSENT_VOICES: Populations under authoritarian regimes and those systematically excluded from deliberative processes are absent; they would advocate for fundamental rights and participatory mechanisms.
% DISAPPEARANCE_RATIONALE: If democratic deliberation and consent vanished as the basis for AI governance, the field would likely fragment into competing technocratic, market-driven, or authoritarian models, leading to a crisis of legitimacy and potential societal harms. The current (imperfect) scaffolding would collapse.
% FOUNDING_PROBLEM: The challenge of governing rapidly advancing AI technologies in a way that respects human dignity, promotes justice, and secures broad public acceptance, without falling into technocratic capture or moral absolutism.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, human rights organizations, and international bodies consistently attest to the ongoing challenge of establishing legitimate AI governance, emphasizing the need for democratic input and pluralistic approaches. This corroboration comes from outside the direct beneficiaries of the democratic process itself.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.4) is moderate, reflecting the inherent costs and friction of inclusive democratic processes, but it is not primarily extractive. Suppression (0.3) is low, as the ideal is to minimize coercion and maximize voluntary participation, though some enforcement is needed to ensure inclusivity. Theater ratio (0.15) is low, as the focus is on genuine deliberation rather than performative gestures. The constraint is a scaffold because it aims to build a new, more legitimate governance structure, with an implicit sunset clause on the 'building' phase once robust mechanisms are in place.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of democratic institutions and civil society, this constraint is a necessary and beneficial scaffold for legitimate governance. From the perspective of religious authorities or technocratic elites, it might be seen as an erosion of their traditional authority or an inefficient process, leading to different classifications (e.g., a Snare for those who believe their authority is being illegitimately suppressed).
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic institutions, civil society organizations, and minority rights holders are beneficiaries, as the constraint empowers their participation and protects their interests. Excluded populations and those under authoritarian regimes are victims, as their lack of participation means the constraint's benefits do not reach them, and they bear the costs of non-legitimate AI systems. Religious authorities and technocratic elites are 'excluded' in the sense that their claims to sole interpretive authority are rejected, but they are not 'victims' in the extractive sense, as they can still participate as stakeholders.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a scaffold, designed to be transitional. Its mandate is to establish legitimate AI governance. If it were to persist indefinitely without achieving robust, self-sustaining democratic processes, or if the deliberative processes became performative without genuine impact, it would risk degrading into a Piton or a Snare, where the 'democratic' label covers an extractive or inert structure. The sunset clause implies a transition to a stable, legitimate governance regime, not an endless process of scaffolding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_process_capture_risk,
    'Can democratic deliberative processes for AI governance be captured by well-resourced special interests or technocratic lobbies, undermining the pluralist ideal?',
    'Empirical analysis of actual AI governance initiatives: tracking funding sources, participant demographics, and policy outcomes to detect disproportionate influence.',
    'If captured, the constraint''s effective extractiveness would be higher, and its classification would shift towards a Tangled Rope or Snare, as the coordination function would be subverted for private gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_process_capture_risk, empirical, 'Risk of democratic processes being subverted by powerful actors.').

omega_variable(
    pluralism_vs_coherence_tension,
    'How does the democratic pluralist reading balance the need for inclusive deliberation with the need for coherent, effective AI governance policies?',
    'Conceptual analysis of policy outcomes: evaluating whether policies derived from pluralistic deliberation are sufficiently robust and consistent to address complex AI challenges, or if they lead to fragmentation and inaction.',
    'If pluralism leads to policy incoherence, the constraint''s effectiveness as a scaffold would be undermined, potentially leading to a Piton (ineffective but maintained) or a Snare (if inaction benefits powerful actors).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pluralism_vs_coherence_tension, conceptual, 'Tension between diverse input and policy effectiveness.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the legitimacy of AI governance truly derived from democratic deliberation, or is it ultimately grounded in a more fundamental (e.g., human rights, natural law) framework that democratic processes merely interpret?',
    'Philosophical and legal analysis of the foundational claims of AI ethics, examining whether democratic consent is the ultimate source of moral authority or a mechanism for applying pre-existing moral truths.',
    'If legitimacy is found to derive from a pre-existing framework, the democratic pluralist reading would function more as a Rope (coordinating application of a Mountain) rather than a Scaffold (building the legitimacy itself), potentially lowering its extractiveness and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity of the ultimate source of AI governance legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(ai_g_tr_t40, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(ai_g_tr_t50, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(ai_g_be_t40, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(ai_g_be_t50, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(ai_g_su_t40, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(ai_g_su_t50, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_governance_legitimacy' kernel. Its siblings represent alternative framings of how AI governance derives its authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
