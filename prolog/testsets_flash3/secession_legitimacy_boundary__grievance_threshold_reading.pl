% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy: Grievance Threshold Reading
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint represents the 'grievance threshold' reading of secession
 *   legitimacy, where secession is deemed legitimate if federal actions cross
 *   a demonstrable threshold of structural injustice, irrespective of
 *   constitutional text. This reading posits that while federal unity is
 *   generally desirable, it is not absolute and can be forfeited by severe
 *   and persistent overreach. The constraint is claimed as a 'tangled_rope'
 *   because it attempts to coordinate national unity while implicitly
 *   allowing for extraction (federal overreach) that, if severe enough,
 *   justifies exit. The metrics reflect the ongoing tension and the active
 *   enforcement required to maintain the federal structure against such
 *   claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.65).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy: Grievance Threshold Reading").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '43d99ef2-fd3f-4eb9-8274-b06380e7ac12').
narrative_ontology:cs_kernel_codification('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', distributed).
narrative_ontology:cs_authority_grounding('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', distributed).
narrative_ontology:cs_reading_relation('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', foundational, federal_authority_is_conditional).
narrative_ontology:cs_axiom_status(federal_authority_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', federal_authority_is_conditional, deontological).
narrative_ontology:cs_axiom('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', foundational, structural_injustice_legitimizes_exit).
narrative_ontology:cs_axiom_status(structural_injustice_legitimizes_exit, holdable).
narrative_ontology:cs_axiom_grounding('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', structural_injustice_legitimizes_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', just_federal_compact).
narrative_ontology:cs_drift_state('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', contemporary_resource_disputes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43d99ef2-fd3f-4eb9-8274-b06380e7ac12', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, majority_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, resource_producing_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the integrity of the federation, collects taxes, and distributes resources. Views secession as an existential threat and will use legal and potentially coercive means to prevent it. Benefits from the current distribution of power and resources.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Bear the brunt of perceived structural injustices, such as disproportionate resource extraction or cultural marginalization. Their identity is often tied to their regional distinctiveness, making exit a deeply felt, though difficult, option. They seek to establish the legitimacy of their secession claim by demonstrating federal overreach.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions, payer,
    organized, generational, identity_locked, regional).

% Often overlap with aggrieved regions, but their grievance is specifically tied to the perceived unfair distribution of resource wealth. They contribute significantly to the federal economy but feel they do not receive commensurate benefits. Their exit options are constrained by economic interdependence and federal power.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, resource_producing_regions, payer,
    powerful, biographical, constrained, regional).

% Benefit from the existing federal structure, including resource transfers and political stability. They generally oppose secession, viewing it as a threat to national unity and their own economic interests.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, majority_regions, beneficiary,
    organized, biographical, mobile, national).

% Monitor human rights, self-determination claims, and international law. Their assessment of whether a grievance threshold has been crossed can lend significant moral and political weight to a secessionist movement, influencing international recognition.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the territorial integrity and political stability of the federal state by providing a framework for adjudicating claims of structural injustice, theoretically preventing arbitrary secession while allowing for legitimate grievances to be addressed.
% TRANSFER_FUNCTION: Transfers political legitimacy and potentially resources from the federal government to aggrieved regions if a threshold of structural injustice is demonstrably crossed, or denies such legitimacy if the threshold is not met.
% ABSENT_VOICES: Indigenous nations and other self-governing entities whose sovereignty predates the federal state are often excluded from the primary federal-regional secession debate, despite their lands and rights being directly impacted. Their claims would introduce a multi-layered sovereignty challenge.
% DISAPPEARANCE_RATIONALE: If the concept of a grievance threshold for secession legitimacy vanished, it would remove a key (albeit contested) moral and political check on both federal power and regional aspirations. Secessionist movements would either become purely constitutional/legal battles or devolve into more direct, potentially violent, confrontations, as the moral basis for their claims would be unmoored from any objective standard of injustice.
% FOUNDING_PROBLEM: To balance the principle of national unity and federal authority against the right of self-determination for distinct regional populations, particularly when faced with perceived systemic oppression or exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political theorists, and international human rights organizations outside of the direct federal or regional beneficiary/victim sets corroborate the ongoing tension between national integrity and self-determination, affirming the problem's live status in contemporary political discourse.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the federal system, from the perspective of aggrieved regions, often operates to their detriment, transferring resources or imposing policies that cause injustice. Suppression (0.70) is high due to the federal government's inherent power to enforce its territorial integrity, often through legal and political means that limit regional autonomy and exit options. The theater ratio (0.20) is moderate; while there's genuine debate and legal process, some federal arguments for unity are performative, masking underlying power dynamics. The slight dip in extractiveness and suppression at the end of the interval reflects periods of increased regional resistance or federal concessions, but the overall trend is one of persistent tension.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this constraint is a legitimate framework for managing internal disputes, ensuring stability. From the aggrieved regions' perspective, it is a mechanism that often legitimizes their ongoing extraction until an extremely high bar for 'injustice' is met, requiring immense political and social cost to challenge. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and majority regions are beneficiaries, as the constraint (by generally upholding federal unity) preserves their power and resource flows. Aggrieved and resource-producing regions are victims, as they bear the costs of perceived injustice and are subject to federal authority. International observers are analytical, assessing the legitimacy of claims without direct participation in the federal structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by requiring a demonstrable 'threshold of structural injustice' for secession to be legitimate. It acknowledges that federal systems have a coordination function, but that this function can degrade into extraction if federal actions become sufficiently unjust. It also prevents mislabeling pure extraction as coordination by allowing for legitimate exit when that threshold is crossed, thus providing a check on federal power. The constraint's mandate is to balance unity and justice, which remains live, though its application is highly contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grievance_threshold_objectivity,
    'What constitutes a ''threshold of structural injustice'' and who objectively adjudicates it?',
    'Establishment of an independent, internationally recognized arbitration body with clear criteria for assessing structural injustice, or a consistent body of international legal precedent.',
    'If the threshold can be objectively defined and adjudicated, the constraint moves closer to a ''rope'' by providing clear rules for legitimate exit. If it remains subjective and contested, the constraint retains its ''tangled_rope'' nature, with legitimacy claims serving as a battleground for power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grievance_threshold_objectivity, conceptual, 'Ambiguity in defining and adjudicating the ''grievance threshold'' for secession legitimacy.').

omega_variable(
    federal_overreach_causality,
    'Is the perceived injustice a direct result of federal actions (structural injustice) or a consequence of regional economic/social factors?',
    'Detailed economic and sociological analysis comparing regional outcomes under federal policy versus counterfactual scenarios, controlling for endogenous regional factors.',
    'If federal actions are demonstrably causal, the legitimacy of secession claims strengthens. If regional factors are primary, the federal government''s coordination function is reinforced, and secession claims are weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_overreach_causality, empirical, 'Distinguishing federal structural injustice from other sources of regional grievance.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option for aggrieved regions a result of genuine cultural/historical ties versus federal suppression of alternatives?',
    'Post-referendum analysis in regions where secession has been rejected: if the desire for exit persists strongly despite democratic process, it suggests deeper identity lock; if it dissipates, suppression may have been more dominant.',
    'If identity lock is primary, the constraint''s suppression is more internalized. If federal coercion is the dominant factor, the constraint''s effective suppression is higher, and the ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Structural vs. internalized suppression mechanism for aggrieved regions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
