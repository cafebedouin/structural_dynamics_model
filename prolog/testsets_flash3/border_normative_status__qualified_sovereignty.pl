% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty in Border Control
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'qualified sovereignty' reading of border
 *   control, where states retain authority but must exercise it
 *   proportionately to legitimate interests and consistently with human
 *   rights. It is a Tangled Rope because it genuinely coordinates state
 *   interests with human rights, but also involves asymmetric extraction from
 *   migrants and displaced citizens, requiring active enforcement to maintain
 *   the balance. The metrics reflect the ongoing tension and the
 *   often-imperfect implementation of human rights obligations in practice.
 *
 * KEY AGENTS:
 *   - states_with_legitimate_interests: Agenda setter (institutional/constrained) — balances interests with obligations
 *   - excluded_migrants: Primary payer (powerless/trapped) — bears the direct cost of exclusion
 *   - displaced_citizens: Secondary payer (powerless/identity_locked) — bears costs of restricted return
 *   - international_human_rights_regime: Beneficiary (institutional/analytical) — framework for upholding rights
 *   - human_rights_advocates: Observer (organized/mobile) — monitors and challenges state actions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.45).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.6).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.45).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty in Border Control").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '9587938b-2dd4-4901-a38a-eac08782f4cd').
narrative_ontology:cs_kernel_codification('9587938b-2dd4-4901-a38a-eac08782f4cd', formalized).
narrative_ontology:cs_authority_grounding('9587938b-2dd4-4901-a38a-eac08782f4cd', lineage).
narrative_ontology:cs_interpretation_layer_present('9587938b-2dd4-4901-a38a-eac08782f4cd').
narrative_ontology:cs_reading_relation('9587938b-2dd4-4901-a38a-eac08782f4cd', border_normative_status__sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('9587938b-2dd4-4901-a38a-eac08782f4cd', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('9587938b-2dd4-4901-a38a-eac08782f4cd', foundational, state_sovereignty_is_qualified).
narrative_ontology:cs_axiom_status(state_sovereignty_is_qualified, holdable).
narrative_ontology:cs_axiom_grounding('9587938b-2dd4-4901-a38a-eac08782f4cd', state_sovereignty_is_qualified, deontological).
narrative_ontology:cs_axiom('9587938b-2dd4-4901-a38a-eac08782f4cd', foundational, human_rights_are_universal_and_indivisible).
narrative_ontology:cs_axiom_status(human_rights_are_universal_and_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('9587938b-2dd4-4901-a38a-eac08782f4cd', human_rights_are_universal_and_indivisible, deontological).
narrative_ontology:cs_reference_frame('9587938b-2dd4-4901-a38a-eac08782f4cd', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('9587938b-2dd4-4901-a38a-eac08782f4cd', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9587938b-2dd4-4901-a38a-eac08782f4cd', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, states_with_legitimate_interests).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, international_human_rights_regime).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States are recognized as having legitimate interests in controlling their borders (e.g., security, public health, economic stability) but must justify their actions and ensure proportionality and human rights compliance. They bear the burden of adjudication.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, states_with_legitimate_interests, agenda_setter,
    institutional, generational, constrained, national).

% Individuals seeking entry who are denied based on state interests, even if those interests are legitimate. They bear the direct cost of exclusion, often facing precarious situations or refoulement. Their claims are subject to state adjudication.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Citizens who are unable to return to their own country due to border closures or restrictions, or who face disproportionate burdens at the border. Their right to return is qualified by state capacity and security concerns.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    powerless, biographical, identity_locked, national).

% The body of international laws and norms that this constraint seeks to uphold. It benefits from states adhering to human rights obligations, even if enforcement is imperfect. It provides the framework for challenging disproportionate state actions.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_regime, beneficiary,
    institutional, civilizational, analytical, global).

% Monitor state border practices, document violations, and advocate for stricter adherence to human rights obligations and proportionality. They provide external pressure and legal challenges.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state sovereignty claims with international human rights obligations, providing a framework for legitimate border control that is not absolute but subject to legal and ethical limits.
% TRANSFER_FUNCTION: Transfers the burden of justification and proportionality assessment to states, while transferring the cost of exclusion (e.g., denied entry, prolonged displacement) to migrants and some citizens.
% ABSENT_VOICES: Those who advocate for open borders or a 'freedom primary' reading are largely excluded from the state-centric policy discussions, as their fundamental premise challenges the very notion of state border control authority.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would likely revert to a 'sovereignty primary' approach, leading to more arbitrary and less rights-compliant border policies. The international human rights regime would lose a key mechanism for influencing state behavior, and migrants would face even greater precarity.
% FOUNDING_PROBLEM: The tension between state sovereignty over territory and the universal human rights of individuals, particularly in the context of migration and displacement, where unchecked state power could lead to severe abuses.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and UN bodies consistently attest to the ongoing relevance of balancing sovereignty with human rights, citing numerous contemporary cases of border-related human rights violations. This corroboration comes from outside the direct beneficiaries of state power.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).
:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while states benefit from control, they also bear the cost of justification and potential legal challenges. Suppression (0.6) is significant as states actively enforce border policies, often with coercive measures. Theater ratio (0.2) is relatively low, as the human rights obligations are genuinely debated and sometimes upheld, but there's also performative adherence without full implementation. Accessibility collapse (0.4) is moderate, as legal avenues for challenge exist but are often difficult to access. Resistance (0.5) is also moderate, reflecting ongoing advocacy and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   States, as agenda setters, perceive this as a necessary and legitimate balance, allowing them to manage their borders while respecting international law. Excluded migrants and displaced citizens, however, experience it as a system that prioritizes state interests over their fundamental rights, leading to significant personal costs and limited recourse. The international human rights regime views it as a crucial, albeit imperfect, framework for accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   States are beneficiaries (d near 0.0) as they retain control, albeit qualified. Excluded migrants and displaced citizens are targets (d near 1.0) as they bear the direct costs of exclusion and restriction. The international human rights regime is a beneficiary (d near 0.0) as its principles are affirmed, even if imperfectly implemented.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by recognizing the genuine coordination function of balancing sovereignty with human rights. It also prevents mislabeling it as a pure Rope by acknowledging the significant and often asymmetric extraction from vulnerable populations. The 'contested' status of the founding problem highlights the ongoing debate about whether the constraint's original purpose is still being met or if it has drifted towards greater extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_adjudication_ambiguity,
    'How consistently and effectively are ''legitimate state interests'' and ''proportionality'' adjudicated in practice, particularly for vulnerable populations?',
    'Systematic review of judicial decisions, administrative practices, and independent monitoring reports across multiple jurisdictions, focusing on outcomes for migrants and asylum seekers.',
    'If adjudication is consistently weak or biased, the constraint''s effective extractiveness and suppression are higher than measured, pushing it closer to a Snare. If robust, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_adjudication_ambiguity, empirical, 'Uncertainty regarding the practical application of proportionality and legitimate interest tests.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the suppression experienced by excluded migrants primarily structural (external barriers, legal restrictions) or internalized (fear, lack of knowledge of rights, despair)?',
    'Post-exit trajectory analysis: if migrants continue to self-censor or avoid seeking protection even after legal avenues open or barriers are removed, it suggests internalized suppression. Qualitative interviews and ethnographic studies.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher and more persistent, making exit more difficult even if structural barriers are reduced. This would amplify the Snare-like aspects of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Ambiguity regarding the mechanism of suppression for excluded populations.').

omega_variable(
    kernel_reading_sovereignty_primary_vs_qualified_sovereignty,
    'Is this constraint (qualified_sovereignty) a genuine evolution of international law, or is it merely a rhetorical cover for states to maintain a de facto ''sovereignty_primary'' stance?',
    'Analysis of state practice and international jurisprudence over time: if state actions consistently align with the ''qualified'' aspect, it''s an evolution. If not, the ''sovereignty_primary'' reading remains dominant in practice.',
    'If ''sovereignty_primary'' is dominant in practice, the effective extractiveness and suppression of this constraint are higher, and its coordination function is weaker, pushing it closer to a Snare. If ''qualified_sovereignty'' is genuinely implemented, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sovereignty_primary_vs_qualified_sovereignty, conceptual, 'Ambiguity regarding the practical dominance of the ''qualified_sovereignty'' reading over ''sovereignty_primary''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__qualified_sovereignty, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(bord_tr_t1970, border_normative_status__qualified_sovereignty, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__qualified_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__qualified_sovereignty, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__qualified_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__qualified_sovereignty, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(bord_be_t1970, border_normative_status__qualified_sovereignty, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__qualified_sovereignty, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__qualified_sovereignty, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__qualified_sovereignty, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__qualified_sovereignty, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(bord_su_t1970, border_normative_status__qualified_sovereignty, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__qualified_sovereignty, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__qualified_sovereignty, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__qualified_sovereignty, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('qualified_sovereignty') of the 'border_normative_status' kernel, alongside 'sovereignty_primary' and 'freedom_primary'. Each reading represents a distinct structural claim about border authority and its limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
