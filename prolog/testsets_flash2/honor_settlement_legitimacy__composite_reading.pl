% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Decline of Dueling: Composite Reading
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint describes the decline of dueling as a legitimate form of
 *   honor settlement, viewed through a 'composite reading' that emphasizes
 *   multiple reinforcing causal pathways. It argues that dueling became
 *   culturally unthinkable (contraction) but was also suppressed by legal and
 *   social changes that would have independently driven its decline. The
 *   constraint is framed as a Tangled Rope because it initially served a
 *   coordination function for honor culture adherents but became increasingly
 *   extractive as state power and bourgeois norms actively suppressed it,
 *   creating victims among those who clung to the old ways.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.65).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.78).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Decline of Dueling: Composite Reading").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '9c458d01-0394-4406-9a94-71de51b99364').
narrative_ontology:cs_kernel_codification('9c458d01-0394-4406-9a94-71de51b99364', implicit).
narrative_ontology:cs_authority_grounding('9c458d01-0394-4406-9a94-71de51b99364', practice).
narrative_ontology:cs_interpretation_layer_present('9c458d01-0394-4406-9a94-71de51b99364').
narrative_ontology:cs_reading_relation('9c458d01-0394-4406-9a94-71de51b99364', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('9c458d01-0394-4406-9a94-71de51b99364', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('9c458d01-0394-4406-9a94-71de51b99364', foundational, decline_is_overdetermined).
narrative_ontology:cs_axiom_status(decline_is_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('9c458d01-0394-4406-9a94-71de51b99364', decline_is_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('9c458d01-0394-4406-9a94-71de51b99364', foundational, cultural_contraction_is_dominant_edge).
narrative_ontology:cs_axiom_status(cultural_contraction_is_dominant_edge, holdable).
narrative_ontology:cs_axiom_grounding('9c458d01-0394-4406-9a94-71de51b99364', cultural_contraction_is_dominant_edge, empirically_contingent).
narrative_ontology:cs_reference_frame('9c458d01-0394-4406-9a94-71de51b99364', honor_code_legitimacy_framework).
narrative_ontology:cs_drift_state('9c458d01-0394-4406-9a94-71de51b99364', late_19th_century, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9c458d01-0394-4406-9a94-71de51b99364', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_public_sphere).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, dueling_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose social standing and self-concept were tied to the honor code, for whom dueling was a legitimate means of dispute resolution. They faced increasing legal penalties and social ostracization for upholding the practice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_culture_adherents, payer,
    moderate, biographical, identity_locked, local).

% Actively criminalized dueling, imposing fines, imprisonment, and social penalties. It sought to establish a monopoly on legitimate violence and dispute resolution, viewing dueling as a challenge to its authority.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% A rising social class that promoted values of rationality, civility, and legal recourse over violent honor contests. It benefited from the decline of dueling by establishing its own norms of respectable conduct and dispute resolution, which reinforced state legal authority.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_public_sphere, beneficiary,
    organized, generational, mobile, regional).

% Individuals directly involved in duels, facing the immediate risks of injury or death, as well as the legal and social consequences of their actions. Their participation was often driven by social pressure within their honor-bound circles.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, dueling_participants, payer,
    powerless, immediate, trapped, local).

% Analyze the complex interplay of legal, social, and cultural factors that led to dueling's decline, seeking to understand the overdetermined nature of the shift in honor settlement legitimacy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, state_legal_system).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for resolving disputes among gentlemen, maintaining social order within specific honor-bound communities by establishing clear rules for challenging and satisfaction.
% TRANSFER_FUNCTION: Transferred the right to adjudicate honor disputes from individuals and their social circles to the state legal system and the emerging bourgeois public sphere, along with the associated social capital and legitimacy.
% ABSENT_VOICES: Future generations for whom dueling became culturally unthinkable would have expressed bewilderment at its former legitimacy. The voices of those who successfully navigated the transition away from dueling without violence are often underrepresented in historical accounts.
% DISAPPEARANCE_RATIONALE: If the composite mechanisms reinforcing dueling's decline had not emerged, honor culture would have persisted longer, potentially leading to different forms of extra-legal dispute resolution or a more violent social landscape. The shift was fundamental to the development of modern state authority and civil society.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes among elites in a way that upheld their honor and status, in a context where state legal systems were not yet fully dominant or trusted for such matters.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists corroborate that the problem of elite dispute resolution has largely been absorbed by state legal systems and civil norms, rendering the original 'problem' dueling solved obsolete. The state legal system's monopoly on violence is widely accepted, and honor is now defended through non-violent means.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the state and bourgeois public sphere actively extracted the legitimacy of honor-based dispute resolution, imposing costs on adherents. Suppression is very high due to legal criminalization and social ostracization. Theater ratio is low because the decline was a genuine, active process of delegitimization, not merely performative maintenance. Accessibility collapse is high because the cultural and legal shifts made dueling increasingly difficult and unthinkable as an option. Resistance is low because the reinforcing mechanisms eventually overwhelmed the capacity for sustained opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of honor culture adherents, the constraint was a Snare, actively extracting their social standing and freedom to defend honor. From the perspective of the state and bourgeois public sphere, it was a Rope or even a Mountain, representing the natural progression towards a more civilized and lawful society. This composite reading acknowledges both perspectives as contributing to the overdetermined decline.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor culture adherents and dueling participants are targets (high d) as the constraint actively suppressed their practices. The state legal system and bourgeois public sphere are beneficiaries (low d) as they gained legitimacy and social control from dueling's decline. The composite reading emphasizes how these different directionalities reinforced each other.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to resolve honor disputes) became obsolete as the state asserted its monopoly on violence and new social norms emerged. The classification as Tangled Rope captures the hybrid nature of this transition: a genuine coordination function at its origin, but evolving into an extractive mechanism as its mandate atrophied and was actively suppressed by competing systems of legitimacy. The 'dead' status of the founding problem confirms mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_causal_weight,
    'What was the precise relative causal weight of cultural contraction versus legal/institutional suppression in dueling''s decline?',
    'Counterfactual historical analysis, comparing regions with differing rates of legal enforcement or cultural shifts, or detailed micro-historical studies of individual decisions to abstain from dueling.',
    'If contraction was overwhelmingly dominant, the constraint leans more towards a Mountain (cultural inevitability); if legal suppression was more decisive, it leans more towards a Snare (coercive enforcement). This composite reading asserts both were significant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_causal_weight, empirical, 'Determining the primary driver of dueling''s decline.').

omega_variable(
    identity_lock_strength,
    'To what extent was ''identity_locked'' exit for honor culture adherents a genuine internal constraint versus a rational response to external suppression?',
    'Analysis of personal diaries, letters, and memoirs from the period, focusing on internal struggles and justifications for dueling versus external pressures and penalties.',
    'If identity-lock was primarily internal, the ''payer'' seat''s directionality is more self-imposed; if external suppression was the primary driver, the extraction is more directly attributable to the state/bourgeois sphere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, conceptual, 'Distinguishing internal identity-lock from external coercive pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__composite_reading, theater_ratio, 1700, 0.25).
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__composite_reading, theater_ratio, 1750, 0.2).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__composite_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__composite_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1750, 0.5).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1850, 0.65).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1750, 0.5).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_settlement_legitimacy' kernel. This 'composite_reading' emphasizes the interplay of cultural contraction and institutional suppression. It influences and coexists with the 'contraction_reading' (focus on cultural shift) and the 'drop_reading' (focus on residual practice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
