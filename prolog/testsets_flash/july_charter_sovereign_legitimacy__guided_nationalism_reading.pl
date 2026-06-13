% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter: Guided Islamic Nationalism Reading
 *   domain: constitutional_law/political_transitions/state_building
 *
 * SUMMARY:
 *   This constraint describes the 'guided nationalism' reading of a
 *   post-revolutionary July Charter, which establishes an Islamic-nationalist
 *   framework where religious identity is the primary ground for sovereign
 *   legitimacy. This reading constrains secular institutions, elevates
 *   religious law or norms to constitutional status, and identifies secular
 *   civil society and religious minorities as victims. The constraint is
 *   actively enforced to maintain this framework against internal and
 *   external challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.7).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.8).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter: Guided Islamic Nationalism Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'f17feb99-25b5-4537-99ec-0a35eb544aad').
narrative_ontology:cs_kernel_codification('f17feb99-25b5-4537-99ec-0a35eb544aad', fixed_text).
narrative_ontology:cs_authority_grounding('f17feb99-25b5-4537-99ec-0a35eb544aad', lineage).
narrative_ontology:cs_interpretation_layer_present('f17feb99-25b5-4537-99ec-0a35eb544aad').
narrative_ontology:cs_reading_relation('f17feb99-25b5-4537-99ec-0a35eb544aad', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('f17feb99-25b5-4537-99ec-0a35eb544aad', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('f17feb99-25b5-4537-99ec-0a35eb544aad', foundational, islamic_identity_is_sovereign_ground).
narrative_ontology:cs_axiom_status(islamic_identity_is_sovereign_ground, holdable).
narrative_ontology:cs_axiom_grounding('f17feb99-25b5-4537-99ec-0a35eb544aad', islamic_identity_is_sovereign_ground, theological).
narrative_ontology:cs_axiom('f17feb99-25b5-4537-99ec-0a35eb544aad', secondary, national_unity_requires_religious_conformity).
narrative_ontology:cs_axiom_status(national_unity_requires_religious_conformity, holdable).
narrative_ontology:cs_axiom_grounding('f17feb99-25b5-4537-99ec-0a35eb544aad', national_unity_requires_religious_conformity, conventional).
narrative_ontology:cs_reference_frame('f17feb99-25b5-4537-99ec-0a35eb544aad', post_revolutionary_islamic_state).
narrative_ontology:cs_drift_state('f17feb99-25b5-4537-99ec-0a35eb544aad', contemporary_global_secularization_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f17feb99-25b5-4537-99ec-0a35eb544aad', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_institutions).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, political_opposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary architects and beneficiaries of the Islamic-nationalist framework. They control state institutions, interpret the Charter, and direct policy to align with their ideological vision. They benefit from the consolidation of power and resources.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites, agenda_setter,
    institutional, generational, mobile, national).

% Gain constitutional recognition, state funding, and influence over public life. Their interpretations of religious law often inform state policy, reinforcing their authority and social standing. They are coordinated into the state apparatus.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Faces restrictions on freedom of expression, assembly, and association. Their organizations are often monitored, defunded, or dissolved if perceived as challenging the Islamic-nationalist framework. They bear the cost of reduced civic space.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    moderate, biographical, constrained, national).

% Subject to discrimination in law and practice, limited political representation, and social pressure to conform to the dominant religious-nationalist identity. Their ability to practice their faith or culture freely is constrained. Exit means abandoning their homeland or identity.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, generational, identity_locked, national).

% Faces legal and extra-legal suppression, including arrests, bans on political parties, and restrictions on media access. Their ability to challenge the ruling elite is severely curtailed, making political participation a high-risk endeavor.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, political_opposition, payer,
    moderate, biographical, constrained, national).

% Monitor the human rights situation, issue reports, and engage in diplomatic pressure. Their influence is limited by the state's sovereignty claims but they provide an external analytical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To forge a unified national identity and provide a stable governance framework after a period of revolutionary upheaval, by grounding state legitimacy in a shared religious and national heritage.
% TRANSFER_FUNCTION: Transfers political power, legal authority, and cultural influence from secular and minority groups to Islamic-nationalist elites and institutions, in exchange for perceived national unity and stability.
% ABSENT_VOICES: Secular intellectuals, liberal political parties, and representatives of religious minorities are largely excluded from the constitutional drafting and interpretive processes. They would advocate for a pluralistic, secular state with equal rights for all citizens, but their platforms are suppressed.
% DISAPPEARANCE_RATIONALE: If this reading of the Charter vanished, the entire state apparatus and its legitimacy claims would collapse. A power vacuum would emerge, leading to intense contestation over the foundational principles of the state, potentially resulting in civil unrest or a new constitutional order.
% FOUNDING_PROBLEM: The nation faced a crisis of identity and governance after a revolution, with competing visions for the state's future and a need to consolidate power and establish a stable, legitimate order.
% FOUNDING_PROBLEM_CORROBORATION: Islamic-nationalist elites and religious institutions attest that the problem of national identity and stability remains live, requiring the Charter's framework. International observers and secular civil society acknowledge the initial post-revolutionary instability but argue the current framework has become a tool for entrenching power rather than genuinely solving the founding problem, citing human rights reports and political analyses from outside the benefiting parties.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) stems from the redirection of state resources and opportunities towards those aligned with the Islamic-nationalist identity, and the imposition of religious norms on public life. Suppression (0.8) is high due to active enforcement against secular and minority voices, including legal restrictions on political and social organization. The theater ratio (0.2) is relatively low, as the state genuinely pursues its Islamic-nationalist agenda, with less performative maintenance and more direct action. The metrics show a slight increase in extractiveness and suppression over time, indicating a hardening of the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Islamic-nationalist elites, this framework is a legitimate expression of national identity and divine will, providing necessary coordination for a moral society (a Rope or even Mountain). For secular civil society and religious minorities, it is a coercive structure that extracts rights and resources (a Snare). The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic-nationalist elites and religious institutions are clear beneficiaries (d near 0.0) as the constraint channels power and resources to them. Secular civil society, religious minorities, and political opposition are targets (d near 1.0) as they bear the costs of restricted freedoms and discrimination. The state apparatus acts as the agenda-setter, enforcing the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to establish a new national identity and governance structure post-revolution. While the 'founding problem' of national identity and stability is still 'live' for its proponents, the 'guided nationalism' reading has arguably accumulated extraction beyond what is necessary for coordination, indicating a drift towards a Snare. The active enforcement and clear beneficiaries prevent it from being a Piton, as there are concentrated interests maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_legitimacy_source_ambiguity,
    'Is the July Charter''s sovereign legitimacy grounded in popular will, divine mandate, or military power?',
    'Analysis of constitutional court rulings, public discourse, and the actual enforcement mechanisms deployed during crises. If popular will is consistently overridden by religious or military claims, the latter gain weight.',
    'If divine mandate is the true grounding, the constraint is closer to a Mountain (unchangeable by human will); if military power, it''s a Snare. This reading emphasizes divine mandate and national identity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_legitimacy_source_ambiguity, conceptual, 'Ambiguity of the Charter''s ultimate source of sovereign legitimacy.').

omega_variable(
    secular_institutions_status_ambiguity,
    'To what extent can secular institutions operate independently under a constitution that grounds sovereignty in religious-nationalist identity?',
    'Observation of judicial independence, legislative autonomy, and civil society space over time. If secular institutions are consistently subordinated or dissolved, their independence is foreclosed.',
    'If secular institutions are genuinely foreclosed, the constraint''s suppression and extractiveness are higher, as alternatives are structurally eliminated. This reading asserts their subordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_institutions_status_ambiguity, empirical, 'The actual operational space for secular institutions under this reading.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''july_charter_sovereign_legitimacy'' kernel. This ''guided_nationalism_reading'' emphasizes religious identity as the sovereign ground. How would the classification change under sibling readings?',
    'Compare this story''s metrics and stakeholder analysis with those generated for ''secular_democratic_reading'' and ''military_custodian_reading''.',
    'The ''secular_democratic_reading'' would likely show lower extractiveness and suppression, potentially classifying as a Rope or Scaffold. The ''military_custodian_reading'' might show higher suppression and extractiveness, potentially classifying as a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''guided_nationalism_reading'' of the July Charter kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'July Charter Sovereign Legitimacy' kernel. Each reading represents a different structural interpretation of the Charter's foundational principles, leading to different beneficiary/victim sets and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
