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
 *   human_readable: Existential Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the 'existential matrix' reading of
 *   territorial sovereignty legitimacy. It posits that legitimacy is not
 *   derived from legal or historical claims, but from the fundamental need of
 *   a people for territorial control as a precondition for collective
 *   survival and identity. This makes territorial conflict inherently
 *   zero-sum, with legal arguments serving as epiphenomenal justifications
 *   for deeper, existential drives. The constraint describes a Snare because
 *   this framing justifies high extraction and suppression by the dominant
 *   group, with identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.9).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '21af9fb4-d336-4e54-a46b-3383fcc97e30').
narrative_ontology:cs_kernel_codification('21af9fb4-d336-4e54-a46b-3383fcc97e30', implicit).
narrative_ontology:cs_authority_grounding('21af9fb4-d336-4e54-a46b-3383fcc97e30', practice).
narrative_ontology:cs_reading_relation('21af9fb4-d336-4e54-a46b-3383fcc97e30', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('21af9fb4-d336-4e54-a46b-3383fcc97e30', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('21af9fb4-d336-4e54-a46b-3383fcc97e30', foundational, collective_survival_requires_territorial_control).
narrative_ontology:cs_axiom_status(collective_survival_requires_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('21af9fb4-d336-4e54-a46b-3383fcc97e30', collective_survival_requires_territorial_control, empirically_contingent).
narrative_ontology:cs_axiom('21af9fb4-d336-4e54-a46b-3383fcc97e30', foundational, identity_expression_requires_territorial_control).
narrative_ontology:cs_axiom_status(identity_expression_requires_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('21af9fb4-d336-4e54-a46b-3383fcc97e30', identity_expression_requires_territorial_control, empirically_contingent).
narrative_ontology:cs_reference_frame('21af9fb4-d336-4e54-a46b-3383fcc97e30', perpetual_intergroup_competition).
narrative_ontology:cs_drift_state('21af9fb4-d336-4e54-a46b-3383fcc97e30', contemporary_conflict_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('21af9fb4-d336-4e54-a46b-3383fcc97e30', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_group).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to secure its collective survival and identity through absolute territorial control, viewing any compromise as an existential threat. Benefits from achieving demographic and military dominance, which this reading justifies as necessary.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_group, agenda_setter,
    institutional, generational, constrained, national).

% Faces an existential threat to its collective survival and identity due to the dominant group's territorial claims. Constantly resists, as its very existence is perceived to be at stake, making exit unthinkable.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_group, payer,
    organized, generational, trapped, national).

% Attempt to apply juridical frameworks and principles of international law to resolve territorial disputes. From the perspective of this reading, their efforts are largely epiphenomenal, failing to address the underlying existential drivers.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_bodies, observer,
    institutional, biographical, analytical, global).

% Work to broker compromises and de-escalate conflicts through negotiation and legal settlements. Their efforts are seen as fundamentally unstable by this reading, as they do not address the zero-sum nature of existential territorial claims.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, peacemakers_diplomats, observer,
    moderate, immediate, constrained, global).

% Would argue for non-existential, shared solutions based on individual and collective rights, transcending zero-sum territorial claims. Their perspective is often marginalized or dismissed by parties operating within the existential matrix.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, universal_human_rights_advocates, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_group).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading posits a fundamentally zero-sum conflict where genuine coordination is impossible due to existential stakes.
% TRANSFER_FUNCTION: Territorial control, resources, and security are transferred from the subordinate group to the dominant group, as the latter asserts its existential claim through power.
% ABSENT_VOICES: Universal human rights advocates and proponents of shared sovereignty models are excluded, as their frameworks are deemed irrelevant or naive in the face of existential threats.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the conflict might be reframed as resolvable through juridical or self-determination principles, leading to different diplomatic and political approaches, potentially opening pathways for compromise and shared governance that are currently foreclosed.
% FOUNDING_PROBLEM: The inherent vulnerability of collective identity and survival in a world of competing peoples, where territorial control is perceived as the ultimate guarantor against annihilation.
% FOUNDING_PROBLEM_CORROBORATION: The lived experience of groups in protracted territorial conflict, particularly those facing demographic or military pressure, often corroborates the existential stakes, even if external observers or legal frameworks dispute the zero-sum nature of legitimacy. The dominant group corroborates it as a justification for their actions; the subordinate group experiences the existential threat, but may frame it as oppression rather than a 'legitimacy matrix'.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the core premise is that one group's survival requires the subordination or displacement of another, leading to maximal extraction of territory and resources. Suppression is also very high (0.90) as the zero-sum nature necessitates active enforcement to maintain dominance and prevent the 'other' from asserting their own existential claims. Theater ratio is low (0.10) because, from this reading's perspective, legal and historical arguments are largely performative cover for the underlying existential struggle, not genuine functional components. Accessibility collapse is near total (0.95) as the zero-sum framing leaves no viable alternatives for compromise or shared existence. Resistance is high (0.90) because the subordinate group's own existential stakes compel continuous struggle.
 *
 * PERSPECTIVAL GAP:
 *   The dominant group perceives its actions as necessary for its survival and identity, justified by this existential reading. The subordinate group experiences the same structure as an existential threat and oppression, compelling resistance. International legal bodies and peacemakers, operating on juridical or diplomatic principles, struggle to address the conflict effectively because they do not acknowledge or prioritize the underlying existential drivers posited by this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'dominant_group' is the clear beneficiary and agenda-setter, as they achieve and maintain territorial control, which this reading frames as essential for their survival. The 'subordinate_group' is the victim and payer, bearing the costs of displacement, loss of resources, and existential threat. International bodies and diplomats are observers, their efforts often rendered ineffective by the zero-sum nature of the conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, framed as a Snare, highlights how an 'existential' justification can serve as a powerful cover story for pure extraction. The 'mandate' of collective survival is used to justify actions that are fundamentally extractive and suppressive, preventing the recognition of alternative, non-zero-sum solutions. The persistence of conflict is not seen as a failure of the constraint, but as its natural expression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_constructed_threat,
    'Is the perceived existential threat a genuine, irreducible condition of collective survival, or is it a socially constructed narrative that justifies zero-sum conflict?',
    'Comparative analysis of similar groups in different political contexts: if groups with similar vulnerabilities achieve security through non-territorial or shared sovereignty arrangements, the threat is likely constructed.',
    'If constructed, the constraint''s extractiveness is purely arbitrary and could be resolved through reframing; if genuine, the constraint''s zero-sum nature is more deeply embedded, making resolution more challenging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_vs_constructed_threat, conceptual, 'Distinguishing genuine existential threat from constructed narratives.').

omega_variable(
    zero_sum_empirical_test,
    'Is the conflict truly zero-sum, or do unacknowledged win-win solutions or shared sovereignty models exist that could satisfy both groups'' core needs?',
    'Empirical study of successful power-sharing or territorial compromise agreements in other protracted conflicts, and their applicability to this specific context.',
    'If win-win solutions are viable, the constraint''s high accessibility_collapse is false, and its classification shifts towards a Tangled Rope (if coordination is possible) or even Rope (if genuinely beneficial for all). If truly zero-sum, the Snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_sum_empirical_test, empirical, 'Testing the empirical validity of the zero-sum claim.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''existential_matrix_reading'' of the ''territorial_sovereignty_legitimacy'' kernel. What structural elements would change if a sibling reading (e.g., ''covenant_continuity_reading'' or ''self_determination_reading'') were adopted?',
    'Analysis of legal and political frameworks that prioritize covenant or self-determination: how do they define beneficiaries, victims, and the scope of legitimate action differently?',
    'Adopting a sibling reading would fundamentally alter the declared beneficiaries, victims, and the justification for extraction, likely shifting the constraint''s type away from a pure Snare by introducing juridical or rights-based coordination elements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural implications of alternative kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(terr_tr_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(terr_tr_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(terr_be_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(terr_be_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(terr_su_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(terr_su_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
