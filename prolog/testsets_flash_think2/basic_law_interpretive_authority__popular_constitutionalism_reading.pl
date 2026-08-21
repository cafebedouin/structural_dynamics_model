% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Meaning from Contestation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'popular constitutionalism' reading of how
 *   constitutional meaning is determined. It posits that constitutional
 *   meaning emerges from ongoing democratic contestation rather than being
 *   fixed by terminal institutional adjudication (e.g., by courts or
 *   legislatures alone). This reading is one of several competing
 *   interpretations of the 'basic_law_interpretive_authority' kernel. The
 *   structural delta for this reading is that neither the judiciary nor the
 *   legislature holds terminal authority, constitutional meaning remains
 *   perpetually contestable, and gridlock costs are distributed across
 *   multiple institutional sites.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.4).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism: Meaning from Contestation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3').
narrative_ontology:cs_kernel_codification('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', formalized).
narrative_ontology:cs_authority_grounding('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', practice).
narrative_ontology:cs_interpretation_layer_present('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3').
narrative_ontology:cs_reading_relation('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', foundational, popular_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', popular_sovereignty_is_supreme, deontological).
narrative_ontology:cs_axiom('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', foundational, constitutional_meaning_is_dynamic).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', constitutional_meaning_is_dynamic, conventional).
narrative_ontology:cs_reference_frame('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', perpetual_democratic_contestation).
narrative_ontology:cs_drift_state('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', contemporary_political_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f6ea1eaa-5c62-4b39-9026-bbf156aeaeb3', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, the_citizenry).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_institutions).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional_elites).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, political_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, the_citizenry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in the ongoing contestation of constitutional meaning through elections, protests, and civic engagement. Bears the costs of political gridlock and potential instability, but benefits from self-governance and a responsive constitution. Identity is fused with the democratic system.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, the_citizenry, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, the_citizenry, beneficiary).

% Acts as a primary site for expressing popular will and shaping constitutional interpretation through legislation and oversight. Benefits from increased influence over constitutional development, but is subject to popular pressure and inter-branch contestation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_legislature, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_legislature, beneficiary).

% Interprets the constitution but lacks terminal authority; its interpretations are subject to ongoing democratic review and contestation. Bears the cost of losing its claim to final interpretive supremacy, but retains a significant role in shaping public discourse and legal precedent.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, agenda_setter).

% Includes legal professionals, academics, and former officials who previously held or advocated for terminal interpretive authority (e.g., judicial supremacy). Bears the cost of losing their privileged position and the perceived stability of fixed constitutional meaning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional_elites, payer,
    powerful, biographical, constrained, national).

% Are vulnerable to shifts in constitutional meaning driven by majoritarian preferences, as their rights and protections may be subject to ongoing contestation without a fixed institutional arbiter. Their exit options are severely limited within the system.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, political_minorities, payer,
    powerless, immediate, trapped, national).

% Analyze the theory and practice of popular constitutionalism, documenting its successes, failures, and implications for democratic governance and rights protection. They are outside the direct contestation but inform it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the process by which constitutional meaning remains responsive to popular will and democratic deliberation, preventing any single institution from holding terminal interpretive authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from insulated institutional elites (judiciary, legislature) to the broader democratic process, distributing the costs of constitutional gridlock and contestation across multiple institutional and civic sites.
% ABSENT_VOICES: Advocates for purely technocratic or expert-driven constitutional interpretation, or those who believe in a fixed, unchangeable constitutional meaning, are structurally marginalized. They would argue for stability and predictability over dynamic contestation.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism vanished, the system would likely default to either judicial supremacy (courts as final arbiters) or parliamentary sovereignty (legislature as final arbiter), fundamentally altering the balance of power and the nature of constitutional governance. The ongoing democratic contestation over meaning would cease, leading to a different, more fixed, institutional arrangement.
% FOUNDING_PROBLEM: To reconcile constitutional stability and the rule of law with democratic self-governance, preventing either judicial oligarchy or legislative tyranny by ensuring that constitutional meaning remains accountable to the people.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historians, and civic organizations (e.g., civil rights groups, advocacy organizations) outside of specific institutional beneficiaries attest that the tension between constitutional stability and democratic responsiveness remains a live and critical problem in contemporary governance.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (how to make constitutional meaning) but involves significant, asymmetrically distributed costs (gridlock, vulnerability of minorities) and requires active enforcement through democratic participation and institutional checks. Extractiveness is moderate-high (0.65) due to the inherent costs of ongoing contestation and potential gridlock, which can be borne disproportionately. Suppression is moderate-low (0.40) because the constraint's purpose is to enable, not suppress, contestation, though it does suppress claims of terminal authority. Theater ratio is low (0.10) as the democratic contestation is genuine and functional, not performative. Resistance is high (0.75) because the very nature of this reading is to resist fixed, top-down interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'the_citizenry' and 'democratic_institutions', this constraint is a necessary mechanism for self-governance, albeit with inherent costs. From the perspective of 'institutional_elites' and 'political_minorities', it can be seen as a source of instability, inefficiency, and potential vulnerability, as their preferred fixed interpretations or protections are constantly challenged.
 *
 * DIRECTIONALITY LOGIC:
 *   'The_citizenry' and 'democratic_institutions' are primary beneficiaries, gaining interpretive agency and responsiveness, but 'the_citizenry' also bears gridlock costs, making them a payer. 'Judiciary' and 'elected_legislature' are both agenda-setters and payers, as they lose terminal authority but gain a dynamic role. 'Institutional_elites' and 'political_minorities' are primarily payers, bearing the costs of losing fixed interpretive authority or stable protections, respectively. The constraint actively enforces the ongoing contestation, requiring participation and checks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    terminal_authority_ambiguity,
    'Is constitutional meaning ultimately settled by any institution, or does it remain perpetually open to democratic contestation?',
    'Empirical observation of constitutional crises and their resolution: if a final institutional arbiter consistently emerges, popular constitutionalism''s claim of perpetual contestation is weakened.',
    'If a terminal authority is identified, this reading''s core premise is challenged, potentially reclassifying it as a ''Piton'' (if the contestation becomes theatrical) or ''Snare'' (if a hidden authority extracts from the pretense of contestation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_authority_ambiguity, conceptual, 'Ambiguity regarding the existence of a terminal interpretive authority.').

omega_variable(
    gridlock_cost_distribution,
    'Are the costs of gridlock and instability inherent to popular constitutionalism distributed equitably, or do they disproportionately burden specific groups (e.g., political minorities)?',
    'Quantitative analysis of policy outcomes and social indicators in systems operating under popular constitutionalist principles, disaggregated by demographic and political groups.',
    'If costs are highly asymmetric and concentrated on vulnerable groups, the ''Tangled Rope'' classification leans more towards ''Snare'', as the coordination story becomes cover for extraction from the marginalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_cost_distribution, empirical, 'Fairness of gridlock cost distribution under popular constitutionalism.').

omega_variable(
    reading_of_basic_law_interpretive_authority,
    'This constraint is the ''popular_constitutionalism_reading'' of the ''basic_law_interpretive_authority'' kernel. What would change if the ''judicial_supremacy_reading'' or ''parliamentary_sovereignty_reading'' were adopted?',
    'Conceptual analysis of the logical implications of each sibling reading for institutional design and the distribution of interpretive power.',
    'Adopting a sibling reading would fundamentally alter the distribution of interpretive authority, shifting the constraint''s beneficiaries and victims, and likely changing its classification (e.g., to a ''Rope'' or ''Snare'' depending on the specific institutional design).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_basic_law_interpretive_authority, conceptual, 'This constraint is one reading of the basic_law_interpretive_authority kernel; sibling readings would change the locus of terminal authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_authority' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
