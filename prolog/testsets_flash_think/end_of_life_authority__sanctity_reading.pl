% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Prohibition on Intentional Life-Ending (Sanctity of Life Reading)
 *   domain: Medical Ethics / Bioethics / End-of-Life Policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of the broader
 *   'end_of_life_authority' kernel. It posits that human life has intrinsic
 *   value, prohibiting intentional life-ending regardless of individual
 *   preference. This reading is codified in many legal and medical ethical
 *   frameworks, leading to a categorical ban on assisted dying. While framed
 *   as protective, it imposes significant costs on individuals seeking
 *   autonomy in end-of-life decisions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.85).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.9).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Prohibition on Intentional Life-Ending (Sanctity of Life Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "Medical Ethics / Bioethics / End-of-Life Policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '3fd355cd-38a6-4556-9112-bc6d5cff4300').
narrative_ontology:cs_kernel_codification('3fd355cd-38a6-4556-9112-bc6d5cff4300', formalized).
narrative_ontology:cs_authority_grounding('3fd355cd-38a6-4556-9112-bc6d5cff4300', lineage).
narrative_ontology:cs_interpretation_layer_present('3fd355cd-38a6-4556-9112-bc6d5cff4300').
narrative_ontology:cs_reading_relation('3fd355cd-38a6-4556-9112-bc6d5cff4300', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('3fd355cd-38a6-4556-9112-bc6d5cff4300', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('3fd355cd-38a6-4556-9112-bc6d5cff4300', foundational, human_life_intrinsically_valuable).
narrative_ontology:cs_axiom_status(human_life_intrinsically_valuable, holdable).
narrative_ontology:cs_axiom_grounding('3fd355cd-38a6-4556-9112-bc6d5cff4300', human_life_intrinsically_valuable, deontological).
narrative_ontology:cs_axiom('3fd355cd-38a6-4556-9112-bc6d5cff4300', secondary, intentional_killing_morally_wrong).
narrative_ontology:cs_axiom_status(intentional_killing_morally_wrong, holdable).
narrative_ontology:cs_axiom_grounding('3fd355cd-38a6-4556-9112-bc6d5cff4300', intentional_killing_morally_wrong, deontological).
narrative_ontology:cs_reference_frame('3fd355cd-38a6-4556-9112-bc6d5cff4300', intrinsic_value_of_life_framework).
narrative_ontology:cs_drift_state('3fd355cd-38a6-4556-9112-bc6d5cff4300', contemporary_autonomy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3fd355cd-38a6-4556-9112-bc6d5cff4300', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pro_life_advocates).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, medical_profession_life_preservation_wing).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, vulnerable_populations_protected).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_aid).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, severely_disabled_individuals_seeking_aid).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively champion the intrinsic value of human life, advocating for legal and ethical frameworks that prohibit intentional life-ending. They see themselves as protecting a fundamental moral principle and vulnerable populations.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pro_life_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Provide theological and moral grounding for the sanctity of life, influencing public opinion, legal discourse, and medical ethics. They benefit from the perpetuation of a moral order aligned with their doctrines.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Adhere to a primary ethical duty to preserve life, viewing intentional life-ending as contrary to their professional oath. They administer and enforce the prohibition within healthcare settings, often facing moral distress from patients seeking aid.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_profession_life_preservation_wing, agenda_setter,
    institutional, biographical, constrained, national).

% Are seen by proponents as protected from potential coercion or pressure to end their lives, particularly the elderly, disabled, or economically disadvantaged. They benefit from the societal message that their lives are intrinsically valuable, regardless of perceived 'quality'.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, vulnerable_populations_protected, beneficiary,
    powerless, biographical, trapped, local).

% Experience unbearable suffering and wish to control the timing and manner of their death, but are legally and medically prohibited from doing so. They bear the direct cost of the constraint through prolonged suffering and loss of autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_aid, payer,
    powerless, immediate, trapped, local).

% May face chronic, severe conditions and desire the option of assisted dying, but are denied due to the categorical prohibition. They bear the cost of having their preferences overridden by a collective moral stance, often feeling their autonomy is disregarded.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, severely_disabled_individuals_seeking_aid, payer,
    powerless, biographical, trapped, local).

% Campaign for individual self-determination in end-of-life decisions, viewing the prohibition as an infringement on fundamental rights. They bear the cost of continuous advocacy against an entrenched moral and legal framework.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_advocates, payer,
    organized, biographical, constrained, national).

% Analyze and articulate the philosophical and ethical arguments for the sanctity of life, contributing to the intellectual defense of the prohibition. They observe the societal and individual impacts of the constraint.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, ethicists_sanctity_of_life, observer,
    analytical, biographical, analytical, global).

% Enact and uphold laws reflecting the sanctity of life principle, often balancing competing moral and political pressures. They are responsible for the legal enforcement of the prohibition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legislators, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal commitment to the intrinsic value of human life, preventing a perceived devaluation of life and protecting vulnerable populations from potential coercion or pressure to end their lives.
% TRANSFER_FUNCTION: Transfers the ultimate authority over the timing and manner of one's death from the individual to a collective moral and legal framework, enforced by medical and legal systems. This transfers the burden of prolonged suffering onto individuals who desire life-ending options.
% ABSENT_VOICES: Individuals experiencing unbearable suffering who are denied the option of assisted dying, and their families, whose preferences are overridden by the collective prohibition. They are often marginalized in policy debates due to their vulnerability and the sensitive nature of their requests.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, end-of-life care, legal frameworks, and medical practice would undergo significant restructuring. New services for assisted dying would emerge, legal challenges would redefine medical roles, and societal norms around death and autonomy would shift dramatically, reorganizing the entire end-of-life landscape.
% FOUNDING_PROBLEM: Preventing the devaluation of human life, protecting vulnerable individuals (elderly, disabled, economically disadvantaged) from pressure or coercion to end their lives, and upholding a moral order where life is intrinsically sacred and not subject to instrumental valuation.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts, philosophical traditions, and advocacy groups (e.g., some disability rights organizations) corroborate the concern for protecting vulnerable life and the intrinsic value of life. These sources exist outside the direct beneficiaries and attest to the ongoing relevance of the founding problem, even if its application is contested.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the constraint categorically denies individuals the choice to end their suffering, imposing a collective moral framework over personal autonomy. Suppression is also very high (0.90) as the prohibition is actively enforced through legal penalties for medical professionals and the unavailability of legal pathways for patients. The theater ratio is low (0.10) because the enforcement is genuine and directly functional in maintaining the prohibition, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its proponents, this constraint is a necessary moral safeguard, protecting the vulnerable and upholding the sanctity of life. From the perspective of those seeking end-of-life options, it is an oppressive imposition that denies fundamental autonomy and prolongs suffering. The engine's classification will highlight this divergence, showing it as a protective 'rope' for beneficiaries and a coercive 'snare' for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Pro-life advocates, religious institutions, and a segment of the medical profession are clear beneficiaries, as the constraint aligns with their moral and professional commitments. Vulnerable populations are also declared beneficiaries, as the constraint is intended to protect them from coercion. Terminally ill and severely disabled individuals seeking aid, along with autonomy advocates, are the primary victims, bearing the cost of denied choice and prolonged suffering. Legislators act as agenda-setters, codifying and enforcing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of protecting life and the vulnerable is considered 'live' by its proponents, preventing mandatrophy from being declared. However, the 'contested' status of the founding problem (whether the problem of coercion is still as pervasive as claimed, or if the prohibition is now overreaching) indicates a potential for future mandatrophy, especially as societal views on autonomy evolve. The constraint persists due to deeply held moral convictions and institutional inertia, rather than a clear, universally acknowledged coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reflection of an intrinsic moral truth, or a socially constructed prohibition that benefits identifiable groups?',
    'Analysis of cross-cultural and historical variations in end-of-life ethics, and the specific institutional interests that champion this reading.',
    'If primarily socially constructed, the ''emerges_naturally'' aspect of the underlying moral claim is weakened, potentially reclassifying the constraint from a morally-grounded Tangled Rope to a more purely extractive Snare for those denied choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''sanctity_reading'' of the ''end_of_life_authority'' kernel. It structurally places vulnerable populations in the beneficiary set and imposes a categorical prohibition on intentional life-ending, limiting individual autonomy.').

omega_variable(
    coercion_risk_vs_autonomy_denial,
    'What is the actual, empirically measurable risk of coercion for vulnerable populations if assisted dying were legalized with robust safeguards, compared to the harm of denying autonomy to competent individuals?',
    'Longitudinal studies from jurisdictions with legalized assisted dying, evaluating the effectiveness of safeguards and the incidence of coercion, alongside surveys of patient preferences.',
    'If coercion risk is demonstrably low with safeguards, the justification for the categorical prohibition weakens, shifting the balance towards individual autonomy and potentially reducing the perceived ''coordination'' function of the constraint, making its extractive nature more salient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_risk_vs_autonomy_denial, empirical, 'Empirical assessment of the ''slippery slope'' argument and the actual protective function of the prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(end__tr_t50, end_of_life_authority__sanctity_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(end__be_t50, end_of_life_authority__sanctity_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(end__su_t50, end_of_life_authority__sanctity_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_authority' kernel, focusing on the intrinsic value of life. Sibling readings explore individual autonomy and the risks of expanding access to end-of-life options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
