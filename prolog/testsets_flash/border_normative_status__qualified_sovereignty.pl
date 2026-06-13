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
 *   proportionately to legitimate state interests and consistently with human
 *   rights obligations. It is a contested kernel, with 'sovereignty_primary'
 *   (absolute state authority) and 'freedom_primary' (absolute freedom of
 *   movement) as sibling readings. This reading attempts to balance these
 *   extremes, creating a framework for conditional legitimacy. The constraint
 *   is classified as a Tangled Rope due to its genuine coordination function
 *   (balancing state interests with rights) and asymmetric extraction (states
 *   benefit from control, while migrants and some citizens bear the costs of
 *   qualified exclusion).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.6).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.7).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.6).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty in Border Control").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '7ae96039-5992-4e22-a4f8-cc207ee77521').
narrative_ontology:cs_kernel_codification('7ae96039-5992-4e22-a4f8-cc207ee77521', formalized).
narrative_ontology:cs_authority_grounding('7ae96039-5992-4e22-a4f8-cc207ee77521', lineage).
narrative_ontology:cs_interpretation_layer_present('7ae96039-5992-4e22-a4f8-cc207ee77521').
narrative_ontology:cs_reading_relation('7ae96039-5992-4e22-a4f8-cc207ee77521', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('7ae96039-5992-4e22-a4f8-cc207ee77521', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('7ae96039-5992-4e22-a4f8-cc207ee77521', foundational, state_sovereignty_is_conditional).
narrative_ontology:cs_axiom_status(state_sovereignty_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('7ae96039-5992-4e22-a4f8-cc207ee77521', state_sovereignty_is_conditional, deontological).
narrative_ontology:cs_axiom('7ae96039-5992-4e22-a4f8-cc207ee77521', foundational, human_rights_are_universal_and_limit_state_power).
narrative_ontology:cs_axiom_status(human_rights_are_universal_and_limit_state_power, holdable).
narrative_ontology:cs_axiom_grounding('7ae96039-5992-4e22-a4f8-cc207ee77521', human_rights_are_universal_and_limit_state_power, deontological).
narrative_ontology:cs_reference_frame('7ae96039-5992-4e22-a4f8-cc207ee77521', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('7ae96039-5992-4e22-a4f8-cc207ee77521', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7ae96039-5992-4e22-a4f8-cc207ee77521', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, states_maintaining_order).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizens_seeking_security).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States assert their right to control borders for national security, economic stability, and cultural preservation. This reading acknowledges their authority but imposes a duty to justify exclusions and respect human rights. They bear the burden of adjudication and proportionality.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, states_maintaining_order, agenda_setter,
    institutional, generational, constrained, national).

% Individuals seeking entry who are denied based on state interests, even if those interests are legitimate. They bear the direct cost of exclusion, often facing precarious situations, family separation, or return to unsafe conditions. Their claims are adjudicated against state interests.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Citizens of a state who are displaced or affected by border policies, such as those separated from family members or facing restrictions on movement due to security measures. They bear indirect costs when state interests override individual human rights in practice.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, national).

% Organizations and individuals who champion human rights and international law. This reading provides a framework for their advocacy, as it explicitly links state border authority to human rights obligations, giving them a basis for challenging disproportionate exclusions.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Courts and organizations (e.g., UNHCR, ICC) that interpret and enforce international human rights law. This reading aligns with their mandate to balance state sovereignty with universal human rights, providing a basis for their judgments and recommendations.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% Citizens who benefit from the state's ability to control its borders, perceiving enhanced security, economic stability, and cultural cohesion. This reading legitimizes border control as a means to these ends, provided it is exercised within human rights limits.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizens_seeking_security, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state interests in security and self-determination with universal human rights obligations, providing a framework for legitimate and proportionate border governance.
% TRANSFER_FUNCTION: Transfers the burden of justification and proportionality onto states for their border control measures, while transferring some agency to human rights frameworks to challenge arbitrary exclusion.
% ABSENT_VOICES: Those who advocate for open borders or absolute freedom of movement are largely absent from the state-centric discourse that this reading attempts to qualify, as their core premise challenges the very notion of state border authority.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would likely revert to more absolute claims of sovereignty, leading to increased arbitrary exclusions and human rights violations at borders. The international legal framework would lose a key mechanism for balancing state power with individual rights, leading to significant geopolitical and humanitarian shifts.
% FOUNDING_PROBLEM: The historical tension between state sovereignty (the right to control borders) and the emerging recognition of universal human rights, particularly in the context of migration and displacement.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and numerous UN resolutions corroborate that this tension remains a live and pressing problem, requiring ongoing adjudication and balancing. The problem is attested by bodies outside the immediate beneficiaries of state power.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6) is substantial because even 'qualified' exclusion imposes significant costs on individuals, and the burden of proof for proportionality often falls on the excluded. Suppression (0.7) is high due to the coercive power of the state at borders and the limited recourse for those denied entry. Theater ratio (0.4) is moderate; while states genuinely engage in human rights discourse, there's often a performative aspect to 'proportionality' claims that masks underlying extractive or exclusionary motives. The metrics show a gradual increase in both extractiveness and suppression over time, reflecting the hardening of border regimes even within this qualified framework.
 *
 * PERSPECTIVAL GAP:
 *   States (agenda_setter) experience this as a legitimate framework for managing national interests, with the human rights aspect as a necessary but manageable constraint. Excluded migrants and displaced citizens (payers) experience it as a system that legitimizes their exclusion, even if 'qualified,' with the burden of proof for human rights violations often insurmountable. Human rights advocates and international legal bodies (beneficiaries/observers) see it as a vital, albeit imperfect, tool for accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   States are beneficiaries (d near 0.0) as they retain control while gaining legitimacy from the human rights framing. Excluded migrants and displaced citizens are targets (d near 1.0) as they bear the direct costs of exclusion and limited mobility. Human rights advocates and international legal bodies are beneficiaries (d near 0.0-0.2) as this reading provides the legal and moral leverage for their work.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by acknowledging the genuine coordination function of balancing state interests with human rights. However, the high extractiveness and suppression, coupled with the rising theater ratio, indicate a risk of mandatrophy where the 'qualification' becomes a performative cover for what is effectively a more extractive border regime. The 'contested' status of the founding problem further highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine instantiation of ''qualified sovereignty'' or is it a performative cover for ''sovereignty primary''?',
    'Empirical analysis of state practice: if state actions consistently prioritize national interest over human rights without genuine proportionality assessment, reclassify towards ''sovereignty_primary''.',
    'If it''s a cover, the effective extractiveness and suppression are higher, and the constraint is closer to a Snare, as the coordination function is largely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Ambiguity between genuine qualified sovereignty and performative adherence.').

omega_variable(
    proportionality_adjudication_burden,
    'Who effectively bears the burden of proving proportionality and human rights compliance in border control decisions?',
    'Legal and sociological study of court cases and administrative processes: if the burden consistently falls on individuals or NGOs, the ''adjudication burden on states'' aspect of this reading is weak.',
    'If the burden is not genuinely on states, the constraint''s effective suppression is higher for migrants, and the ''qualified'' aspect is less effective in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_adjudication_burden, empirical, 'Distribution of the burden of proof for proportionality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/physical barriers) or internalized (fear, lack of knowledge of rights) for excluded migrants?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., after gaining legal status, migrants still self-censor or avoid authorities), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__qualified_sovereignty, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(bord_tr_t1960, border_normative_status__qualified_sovereignty, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(bord_tr_t1980, border_normative_status__qualified_sovereignty, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(bord_tr_t2000, border_normative_status__qualified_sovereignty, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__qualified_sovereignty, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__qualified_sovereignty, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__qualified_sovereignty, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(bord_be_t1960, border_normative_status__qualified_sovereignty, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(bord_be_t1980, border_normative_status__qualified_sovereignty, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(bord_be_t2000, border_normative_status__qualified_sovereignty, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__qualified_sovereignty, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__qualified_sovereignty, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__qualified_sovereignty, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(bord_su_t1960, border_normative_status__qualified_sovereignty, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(bord_su_t1980, border_normative_status__qualified_sovereignty, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(bord_su_t2000, border_normative_status__qualified_sovereignty, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__qualified_sovereignty, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__qualified_sovereignty, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
