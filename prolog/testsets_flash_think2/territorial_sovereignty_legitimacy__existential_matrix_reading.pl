% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Matrix of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'existential matrix' reading of
 *   territorial sovereignty legitimacy, where territorial control is seen as
 *   an absolute precondition for a people's survival and identity. This
 *   perspective frames conflicts as fundamentally zero-sum, driven by
 *   deep-seated fears rather than legal or historical arguments. Any attempts
 *   at compromise are viewed as structurally unstable, and conflict persists
 *   regardless of formal settlements. This story is one reading of the
 *   'territorial_sovereignty_legitimacy' kernel, focusing on the underlying
 *   drivers rather than juridical claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.9).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.9).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Matrix of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, 'fc9feb81-7c97-4eaa-9f2c-16c1c338080c').
narrative_ontology:cs_kernel_codification('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', implicit).
narrative_ontology:cs_authority_grounding('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', extraction).
narrative_ontology:cs_reading_relation('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', foundational, territorial_control_is_existential_precondition).
narrative_ontology:cs_axiom_status(territorial_control_is_existential_precondition, holdable).
narrative_ontology:cs_axiom_grounding('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', territorial_control_is_existential_precondition, deontological).
narrative_ontology:cs_axiom('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', secondary, conflict_is_zero_sum).
narrative_ontology:cs_axiom_status(conflict_is_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', conflict_is_zero_sum, empirically_contingent).
narrative_ontology:cs_reference_frame('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', perpetual_existential_struggle).
narrative_ontology:cs_drift_state('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', contemporary_international_relations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fc9feb81-7c97-4eaa-9f2c-16c1c338080c', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subjugated_group).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, realpolitik_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, clash_of_civilizations_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A group that has achieved or seeks demographic and military dominance, viewing territorial control as non-negotiable for its collective survival and identity. It actively enforces its claims and suppresses rival narratives and populations, believing any compromise is an existential threat.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_group, agenda_setter,
    powerful, generational, trapped, global).

% A group that is losing or has lost territorial control and faces threats to its collective survival and identity expression. It experiences continuous extraction and suppression, with its resistance framed as an existential struggle against domination.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subjugated_group, payer,
    powerless, generational, trapped, global).

% Composed of states and international organizations that attempt to mediate conflicts and establish legal frameworks for sovereignty. From this reading's perspective, their efforts are largely ineffectual against the underlying existential drivers, often seen as naive or biased.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_community, observer,
    institutional, biographical, analytical, global).

% Academics and experts who analyze sovereignty claims through juridical, historical, or ethical lenses. This reading dismisses their primary focus as epiphenomenal, arguing that legal arguments are merely covers for deeper existential power struggles.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_group).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This arrangement does not primarily solve a coordination problem; rather, it describes a state of perpetual, zero-sum conflict where any 'coordination' is a temporary, unstable equilibrium enforced by power, not mutual benefit.
% TRANSFER_FUNCTION: Territorial control, resources, and the right to self-determination are continuously contested and effectively transferred from the subjugated group to the dominant group through ongoing conflict and the assertion of existential necessity.
% ABSENT_VOICES: Voices advocating for genuine compromise, shared sovereignty, or post-national identity are structurally excluded or dismissed as naive by both sides, which are locked into the existential logic of the conflict. These voices would argue for a re-framing of the conflict as non-zero-sum and resolvable through negotiation.
% DISAPPEARANCE_RATIONALE: If the existential imperative for exclusive territorial control vanished, the fundamental driver of conflict would disappear, allowing for alternative forms of coexistence or political organization. However, the deep-seated identities and fears that constitute 'peoples' in this framework would likely re-manifest in other forms or lead to the dissolution of the 'peoples' as currently conceived, requiring a profound re-imagining of collective identity.
% FOUNDING_PROBLEM: The inherent vulnerability of distinct 'peoples' without exclusive territorial control, leading to a perceived need for absolute security and self-preservation against perceived existential threats from rival groups.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading (e.g., certain nationalist ideologues, some realist international relations theorists) attest that the problem is perpetually live, citing ongoing conflicts and historical precedents. Critics from legal or liberal internationalist perspectives dispute its framing as an 'inherent' problem, arguing it's a constructed narrative that perpetuates conflict; however, they acknowledge the *perception* of the problem is live for many actors.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.9) and suppression (0.9) reflect the zero-sum nature of the conflict: one group's gain in territorial control and identity expression is another's loss, maintained through active enforcement and suppression of alternatives. Accessibility collapse is high (0.9) because compromise is seen as an existential threat, making alternatives unthinkable. Resistance is high (0.9) as both sides are fighting for their perceived survival. Theater ratio is low (0.1) because this reading posits a brutal, underlying reality where diplomatic or legal performances are largely epiphenomenal to the core struggle. The metrics show a stable, high-intensity conflict, reflecting the 'perpetual struggle' inherent in this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'dominant_group,' this constraint is a necessary, if harsh, reality for their survival. From the 'subjugated_group,' it is a brutal snare of pure extraction. The engine's computation of per-seat classification will highlight this divergence, showing how the same 'existential matrix' is experienced as a life-or-death struggle for one and a justification for dominance for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'dominant_group' is the primary beneficiary (d near 0.0) as it achieves and maintains territorial control and identity expression at the expense of others. The 'subjugated_group' is the primary target (d near 1.0), bearing the full cost of dispossession and suppression. The 'international_community' and 'legal_scholars' are analytical observers (d near 0.5), attempting to understand or mediate but not directly benefiting or paying in the existential sense this reading describes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_reality_vs_constructed_narrative,
    'Is the ''existential matrix'' an inherent, irreducible reality of human groups and territory, or a constructed narrative that justifies power dynamics and extraction?',
    'Comparative historical analysis of societies that have successfully transcended zero-sum territorial claims, or sociological studies of how ''existential'' narratives are mobilized by political actors.',
    'If a constructed narrative, the constraint''s extractiveness and suppression are not ''natural'' but contingent, opening pathways for re-framing and resolution. If inherent, the constraint is closer to a Mountain, albeit one with profound extractive consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_reality_vs_constructed_narrative, conceptual, 'Ambiguity regarding the ontological status of the existential matrix.').

omega_variable(
    alternatives_foreclosed_or_suppressed,
    'Are territorial compromise frameworks truly structurally unstable and foreclosed, or are they actively suppressed by actors who benefit from the zero-sum framing?',
    'Analysis of failed and successful peace processes, focusing on the agency of actors in promoting or undermining compromise, rather than assuming inherent instability.',
    'If alternatives are actively suppressed, the constraint''s suppression metric is even more indicative of intentional coercion, and the ''trapped'' exit option for stakeholders is a consequence of agency, not inevitability. If truly foreclosed, the constraint is more rigid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_foreclosed_or_suppressed, empirical, 'Whether alternatives to zero-sum conflict are genuinely impossible or merely prevented.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression primarily structural (military occupation, demographic engineering) or internalized (ideological belief in the necessity of zero-sum conflict, fear of annihilation)?',
    'Post-conflict psychological and sociological studies: if the zero-sum ideology persists after structural barriers are removed, it indicates internalized suppression. If it dissipates, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making resolution more complex. If structural, removing external barriers would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in existential conflicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(terr_tr_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(terr_tr_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 10, 0.86).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(terr_be_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(terr_be_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 50, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 10, 0.86).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(terr_su_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(terr_su_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'territorial_sovereignty_legitimacy' kernel. Each reading offers a different primary justification for sovereignty, leading to different structural properties and classifications. This 'existential_matrix_reading' focuses on survival and identity as the core drivers, framing conflict as zero-sum, in contrast to juridical or historical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
