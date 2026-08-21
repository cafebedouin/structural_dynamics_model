% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: State Sovereignty as Primary Border Authority
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primary' reading of the
 *   normative status of borders, asserting that states have foundational
 *   authority to exclude non-members as an instrument of collective
 *   self-determination. This reading is contested by alternative framings
 *   that prioritize freedom of movement or qualified sovereignty. The
 *   constraint functions as a Tangled Rope, coordinating state
 *   self-determination for member citizens while actively extracting from
 *   non-member migrants through enforcement.
 *
 * KEY AGENTS:
 *   - State Governments: Primary agenda-setters and beneficiaries, enforcing exclusion.
 *   - Member Citizens: Primary beneficiaries, exercising collective self-determination.
 *   - Non-Member Migrants & Asylum Seekers: Primary targets/payers, bearing the costs of exclusion.
 *   - Human Rights Advocates & International Law Scholars: Observers, challenging the constraint's legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.78).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.85).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "State Sovereignty as Primary Border Authority").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '912b7064-1265-4388-bd62-1d4c800d6bd5').
narrative_ontology:cs_kernel_codification('912b7064-1265-4388-bd62-1d4c800d6bd5', formalized).
narrative_ontology:cs_authority_grounding('912b7064-1265-4388-bd62-1d4c800d6bd5', lineage).
narrative_ontology:cs_interpretation_layer_present('912b7064-1265-4388-bd62-1d4c800d6bd5').
narrative_ontology:cs_reading_relation('912b7064-1265-4388-bd62-1d4c800d6bd5', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('912b7064-1265-4388-bd62-1d4c800d6bd5', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('912b7064-1265-4388-bd62-1d4c800d6bd5', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('912b7064-1265-4388-bd62-1d4c800d6bd5', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('912b7064-1265-4388-bd62-1d4c800d6bd5', foundational, collective_self_determination_unqualified).
narrative_ontology:cs_axiom_status(collective_self_determination_unqualified, holdable).
narrative_ontology:cs_axiom_grounding('912b7064-1265-4388-bd62-1d4c800d6bd5', collective_self_determination_unqualified, deontological).
narrative_ontology:cs_reference_frame('912b7064-1265-4388-bd62-1d4c800d6bd5', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('912b7064-1265-4388-bd62-1d4c800d6bd5', contemporary_migration_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('912b7064-1265-4388-bd62-1d4c800d6bd5', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, member_citizens).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_governments).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, non_member_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce the state's foundational authority to control its borders and exclude non-members, viewing this as essential for national security, economic stability, and cultural preservation. They benefit from maintaining territorial integrity and the ability to define national membership.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the perceived security, resource control, and cultural cohesion that strong border enforcement provides. They exercise collective self-determination through their state, defining who belongs and who does not. Their mobility is largely unconstrained by their own state's borders.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, member_citizens, beneficiary,
    organized, generational, mobile, national).

% Bear the direct costs of exclusion, including physical danger, economic hardship, and separation from family. Their movement is severely restricted, and they often face detention, deportation, or death in attempts to cross borders. They have no political voice in the states they seek to enter.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, non_member_migrants, payer,
    powerless, immediate, trapped, global).

% Similar to non-member migrants, they face significant barriers to entry and often endure harsh conditions. While international law grants them a right to seek asylum, the 'sovereignty primary' reading often prioritizes state control over these individual rights, leading to de facto exclusion and prolonged limbo.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Challenge the 'sovereignty primary' reading, arguing that state authority is qualified by human rights obligations and the inherent dignity of all persons. They advocate for more open borders and humane treatment of migrants, often facing political resistance and limited legal avenues for change.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_advocates, observer,
    organized, biographical, constrained, global).

% Analyze the legal and philosophical underpinnings of border regimes, often highlighting the tension between state sovereignty and universal human rights. They provide critical analysis but have no direct power to alter state policy.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, state_governments).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective self-determination of a state's member citizens, enabling them to define their political community, control their territory, and manage their resources without external interference.
% TRANSFER_FUNCTION: Transfers security, cultural cohesion, and resource control to member citizens and state governments, while imposing costs of exclusion, displacement, and denied opportunity on non-member migrants and asylum seekers.
% ABSENT_VOICES: Non-member migrants and asylum seekers are structurally excluded from the political processes that define border policies and determine their fate. If present, they would advocate for freedom of movement and universal human rights.
% DISAPPEARANCE_RATIONALE: If the normative claim of primary state sovereignty over borders vanished, states would lose a foundational justification for exclusion. This would lead to open borders, a fundamental reordering of global political and economic structures, and a redefinition of national membership and citizenship.
% FOUNDING_PROBLEM: The need for distinct political communities to define their membership, control their territory, and manage resources for their citizens, ensuring self-governance and protection from external threats.
% FOUNDING_PROBLEM_CORROBORATION: International relations theory, historical state practice since the Westphalian system, and a significant portion of national populations (as evidenced by public opinion and electoral outcomes) corroborate the ongoing perceived need for state self-determination and border control. This corroboration comes from outside the immediate beneficiaries of exclusion, reflecting a broader societal consensus in many states.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint legitimizes significant costs imposed on non-members (denial of entry, forced displacement, economic hardship) for the benefit of member citizens. Suppression is very high (0.85) due to the active and often militarized enforcement of borders, including physical barriers, surveillance, and legal penalties for unauthorized entry. Theater ratio is low (0.1) because border enforcement is a core, functional activity for states adhering to this reading, not primarily performative. Accessibility collapse is high (0.75) as legal and physical alternatives for non-members to enter are severely limited. Resistance is moderate (0.6) from human rights groups and migrants themselves, but often insufficient to overcome state power.
 *
 * PERSPECTIVAL GAP:
 *   State governments and member citizens experience this constraint as a legitimate and necessary mechanism for self-governance and protection, perceiving it as a Rope or even a Mountain. Non-member migrants and asylum seekers, however, experience it as a Snare, facing severe extraction and suppression with no viable exit. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and member citizens are beneficiaries (low d) as they gain control, security, and the ability to define their community. Non-member migrants and asylum seekers are targets (high d) as they bear the direct costs of exclusion and have severely constrained exit options. Human rights advocates and international law scholars are observers, analyzing the system without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (collective self-determination and exclusion) is actively asserted and enforced by its primary beneficiaries. The contestation it faces is over the *legitimacy* of this mandate, not its atrophy. The high extractiveness and suppression indicate it is far from a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Is state sovereignty an absolute right to exclude, or is it fundamentally qualified by universal human rights obligations?',
    'International legal precedent from courts (e.g., ECtHR, ICJ) that explicitly adjudicate the hierarchy of state sovereignty versus human rights in border contexts, or a global consensus shift in international law.',
    'If sovereignty is deemed absolutely primary, the constraint''s legitimacy is reinforced. If human rights are deemed to qualify sovereignty, the constraint''s extractiveness and suppression would be re-evaluated as illegitimate, potentially reclassifying it towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, conceptual, 'The fundamental tension between state sovereignty and human rights in border governance.').

omega_variable(
    collective_self_determination_scope,
    'Does the right to collective self-determination inherently include the right to exclude, even when such exclusion causes severe harm to non-members?',
    'Philosophical and ethical arguments achieving broad consensus on the moral limits of collective self-determination, or a shift in political philosophy that redefines the scope of legitimate exclusion.',
    'If collective self-determination is found to have inherent moral limits regarding exclusion, the constraint''s justification for extraction would weaken, potentially leading to a lower extractiveness score and a reclassification towards a more benign type (e.g., Rope with strong qualifications).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_self_determination_scope, preference, 'The moral scope and limits of collective self-determination in justifying border exclusion.').

omega_variable(
    border_enforcement_efficacy_vs_cost,
    'Is the current level of border enforcement, justified by this reading, genuinely effective at achieving its stated goals (e.g., security, economic stability) at a proportionate cost, or is it largely symbolic and inefficient?',
    'Independent, longitudinal empirical studies comparing the stated benefits of border enforcement (e.g., crime reduction, wage protection) against its direct and indirect costs (e.g., enforcement budgets, human suffering, economic disruption).',
    'If enforcement is found to be largely ineffective or disproportionately costly, the ''theater_ratio'' would increase, and the ''extractiveness'' could be seen as less justified, potentially pushing the constraint towards a Piton or a more clearly extractive Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_enforcement_efficacy_vs_cost, empirical, 'The empirical efficacy and proportionality of border enforcement under this normative claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_normative_status__sovereignty_primary, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(bord_tr_t1965, border_normative_status__sovereignty_primary, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(bord_tr_t1985, border_normative_status__sovereignty_primary, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__sovereignty_primary, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(bord_tr_t2025, border_normative_status__sovereignty_primary, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_normative_status__sovereignty_primary, base_extractiveness, 1945, 0.65).
narrative_ontology:measurement(bord_be_t1965, border_normative_status__sovereignty_primary, base_extractiveness, 1965, 0.68).
narrative_ontology:measurement(bord_be_t1985, border_normative_status__sovereignty_primary, base_extractiveness, 1985, 0.72).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__sovereignty_primary, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(bord_be_t2025, border_normative_status__sovereignty_primary, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_normative_status__sovereignty_primary, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(bord_su_t1965, border_normative_status__sovereignty_primary, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(bord_su_t1985, border_normative_status__sovereignty_primary, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__sovereignty_primary, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(bord_su_t2025, border_normative_status__sovereignty_primary, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
