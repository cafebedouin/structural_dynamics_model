% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint, 'honor_violence_legitimacy__composite_reading', is one
 *   interpretation of the 'honor_violence_legitimacy' kernel. It posits an
 *   'overdetermined decline' of honor violence, where its legitimacy erodes
 *   due to both external costs (e.g., state legal intervention, migration to
 *   contexts with different norms) and internal conceptual redefinition
 *   (e.g., honor being redefined to exclude violence). This reading
 *   emphasizes the simultaneous operation of these 'drop' and 'contraction'
 *   mechanisms, leading to a complex, multi-faceted erosion of the
 *   constraint's power, but still resulting in significant extraction for its
 *   victims. The constraint is claimed as a Snare because its core function
 *   remains the coercive extraction of compliance through violence, even as
 *   its social foundations weaken.
 *
 * KEY AGENTS:
 *   - patriarchal_family_heads: Agenda setter/Beneficiary (institutional/identity_locked) — uphold and benefit from the system
 *   - community_elders: Agenda setter/Beneficiary (organized/constrained) — interpret and maintain norms
 *   - women_at_risk: Payer/Victim (powerless/trapped) — primary targets of violence
 *   - younger_family_members: Payer/Victim (powerless/constrained) — subject to strictures, limited exit
 *   - transgressors_of_honor_codes: Payer/Victim (powerless/trapped) — face immediate threat for perceived violations
 *   - state_legal_systems: Observer/Agenda setter (institutional/analytical) — seek to enforce alternative norms
 *   - human_rights_advocates: Excluded (organized/analytical) — campaign against the system from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.75).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.65).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, snare).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'b69f0f1a-8b1d-4d0c-a900-03b5770d1be4').
narrative_ontology:cs_kernel_codification('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', implicit).
narrative_ontology:cs_authority_grounding('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', practice).
narrative_ontology:cs_interpretation_layer_present('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4').
narrative_ontology:cs_reading_relation('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', foundational, honor_requires_violent_defense).
narrative_ontology:cs_axiom_status(honor_requires_violent_defense, holdable).
narrative_ontology:cs_axiom_grounding('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', honor_requires_violent_defense, deontological).
narrative_ontology:cs_axiom('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', secondary, honor_code_ensures_social_order).
narrative_ontology:cs_axiom_status(honor_code_ensures_social_order, holdable).
narrative_ontology:cs_axiom_grounding('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', honor_code_ensures_social_order, conventional).
narrative_ontology:cs_reference_frame('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', traditional_honor_code_supremacy).
narrative_ontology:cs_drift_state('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', late_20th_century, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b69f0f1a-8b1d-4d0c-a900-03b5770d1be4', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, patriarchal_family_heads).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, community_elders).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, women_at_risk).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, younger_family_members).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, transgressors_of_honor_codes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uphold and enforce the honor code within their families and community, benefiting from social control and status. Their identity is often fused with this role, making exit unthinkable.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, patriarchal_family_heads, agenda_setter,
    institutional, generational, identity_locked, local).

% Interpret and adjudicate disputes according to the honor code, maintaining social cohesion and their authority. They benefit from the system's persistence but may face external pressure.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, community_elders, agenda_setter,
    organized, generational, constrained, local).

% Are primary targets of honor violence, bearing the ultimate costs of the system. They are often physically and socially trapped with no viable exit.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_at_risk, payer,
    powerless, immediate, trapped, local).

% Are subject to the honor code's strictures and potential violence for perceived transgressions. Their exit options are severely limited by social and economic dependency.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, younger_family_members, payer,
    powerless, biographical, constrained, local).

% Individuals (of any gender) who are perceived to have violated the community's honor code, facing severe social ostracization or violence. They are often trapped by the immediate threat.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, transgressors_of_honor_codes, payer,
    powerless, immediate, trapped, local).

% Represent an alternative, often competing, legal framework that criminalizes honor violence. They seek to enforce state law but face challenges due to community resistance and cultural norms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_legal_systems, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, state_legal_systems, agenda_setter).

% Actively campaign against honor violence, documenting abuses and advocating for legal and social reform. They are excluded from the internal decision-making of honor-based communities but influence external pressure.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, human_rights_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains social hierarchy, family reputation, and community cohesion through a shared understanding of honor and its violent enforcement, providing a framework for dispute resolution and social control.
% TRANSFER_FUNCTION: Transfers social control, status, and perceived purity to those who enforce the honor code, at the cost of bodily autonomy, freedom, and life for victims and transgressors.
% ABSENT_VOICES: Victims of honor violence, human rights advocates, and proponents of state legal supremacy are structurally excluded from the internal discourse that legitimizes the honor code. They would object to its violent enforcement and demand alternative forms of justice and social organization.
% DISAPPEARANCE_RATIONALE: If the legitimacy of honor violence vanished overnight, the social structures, power dynamics, and community norms that rely on it would have to fundamentally reorganize. Traditional authority figures would lose a key mechanism of control, and communities would need to adopt alternative dispute resolution and social cohesion mechanisms, likely leading to significant social upheaval and redefinition of roles.
% FOUNDING_PROBLEM: To establish and maintain social order, family reputation, and patriarchal control in specific cultural contexts, particularly in the absence of strong state institutions or as a parallel system of justice.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within communities where honor violence persists attest to its ongoing necessity for social order and family honor. However, external legal and human rights bodies, as well as sociological and anthropological studies, corroborate that the original problems (e.g., maintaining social order) are now often better addressed by state law or alternative social norms, and that the system persists due to inertia, power dynamics, and identity-lock, rather than genuine necessity for its violent aspects.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.75) and suppression (0.65) are high, reflecting the severe consequences for victims and the coercive nature of the system. However, the declining trend in measurements over the 20th century reflects the 'overdetermined decline' where both external pressures and internal conceptual shifts gradually weaken the constraint's hold. The theater ratio (0.20) is low, as the violence itself is not performative, though the justifications for its persistence may become increasingly theatrical as its legitimacy wanes. Accessibility collapse (0.40) is moderate, indicating that while alternatives are emerging (e.g., state protection, non-violent conflict resolution), they are not universally available or safe for those within honor-based communities. Resistance (0.55) is significant, reflecting both overt and covert challenges to the system.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (patriarchal family heads, community elders) experience this constraint as a legitimate, necessary mechanism for social order and honor, with their identities often fused with its maintenance. For them, the decline is a threat to tradition. The payer/victim seats (women at risk, younger family members, transgressors) experience it as a coercive, life-threatening snare. State legal systems and human rights advocates view it as a human rights violation. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Patriarchal family heads and community elders are beneficiaries (low d) as they gain social control and status from the system. Women at risk, younger family members, and transgressors are clear targets (high d) as they bear the direct costs of violence and control. State legal systems and human rights advocates are analytical observers or external agenda-setters, not directly benefiting or paying within the honor system itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This composite reading helps prevent mislabeling by acknowledging the complex, multi-causal decline. If only external costs ('drop_reading') were considered, the internal conceptual shifts would be missed, potentially underestimating the system's internal fragility. If only conceptual redefinition ('contraction_reading') were considered, the role of external pressures would be overlooked, potentially overestimating the system's capacity for self-reform. The composite view captures that the constraint's mandate (maintaining honor through violence) is increasingly contested and losing its functional justification from multiple angles, even if it persists through inertia and power dynamics. The 'contested' status of the founding problem and the 'world_rearranges' disappearance verdict, combined with declining metrics, signal a system in deep mandatrophy, but one that still extracts significantly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_impact_of_decline_mechanisms,
    'What is the relative contribution of external costs (drop mechanism) versus internal conceptual redefinition (contraction mechanism) to the overall decline in honor violence legitimacy and practice?',
    'Comparative historical analysis across different communities and regions, correlating specific external interventions (e.g., legal reforms, economic development) and internal ideological shifts (e.g., reinterpretation of religious texts, feminist movements) with changes in honor violence incidence and social acceptance.',
    'A clearer understanding of the dominant mechanism would inform intervention strategies: if external costs are primary, focus on legal enforcement and economic alternatives; if conceptual redefinition is primary, focus on education and internal advocacy. This would refine the understanding of the constraint''s persistence and vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_impact_of_decline_mechanisms, empirical, 'Relative impact of ''drop'' vs. ''contraction'' mechanisms on decline.').

omega_variable(
    victim_set_differentiation_by_mechanism,
    'Do the ''drop'' and ''contraction'' mechanisms affect distinct or overlapping victim sets, and does the nature of extraction differ for each?',
    'Detailed ethnographic and sociological studies tracking specific cases of honor violence and the pathways to its resolution or persistence, identifying whether the violence is primarily driven by external pressures (e.g., fear of state reprisal) or internal normative adherence (e.g., community pressure to uphold redefined honor).',
    'If victim sets and extraction types are distinct, the composite reading implies a more complex, multi-layered Snare with different points of leverage for intervention. If they largely overlap, the mechanisms are more intertwined in their impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_differentiation_by_mechanism, empirical, 'Differentiation of victim sets and extraction by decline mechanism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (social pressure, threat of violence) structural (external barriers, lack of state protection) or internalized (belief in the honor code, fear of social ostracism) for the victims?',
    'Post-exit suppression trajectory: if individuals who escape honor-based communities continue to self-regulate their behavior according to the honor code, it suggests a significant internalized component. Conversely, if freedom from the community immediately liberates behavior, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — victims carry the suppression with them after physical exit, making the Snare more insidious. This would also inform the efficacy of external interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in honor violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 1900, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__composite_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(hono_tr_t1920, honor_violence_legitimacy__composite_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(hono_tr_t1940, honor_violence_legitimacy__composite_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(hono_tr_t1960, honor_violence_legitimacy__composite_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(hono_tr_t1980, honor_violence_legitimacy__composite_reading, theater_ratio, 1980, 0.19).
narrative_ontology:measurement(hono_tr_t2000, honor_violence_legitimacy__composite_reading, theater_ratio, 2000, 0.2).

% Extraction over time
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__composite_reading, base_extractiveness, 1900, 0.9).
narrative_ontology:measurement(hono_be_t1920, honor_violence_legitimacy__composite_reading, base_extractiveness, 1920, 0.85).
narrative_ontology:measurement(hono_be_t1940, honor_violence_legitimacy__composite_reading, base_extractiveness, 1940, 0.8).
narrative_ontology:measurement(hono_be_t1960, honor_violence_legitimacy__composite_reading, base_extractiveness, 1960, 0.78).
narrative_ontology:measurement(hono_be_t1980, honor_violence_legitimacy__composite_reading, base_extractiveness, 1980, 0.76).
narrative_ontology:measurement(hono_be_t2000, honor_violence_legitimacy__composite_reading, base_extractiveness, 2000, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__composite_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(hono_su_t1920, honor_violence_legitimacy__composite_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(hono_su_t1940, honor_violence_legitimacy__composite_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(hono_su_t1960, honor_violence_legitimacy__composite_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(hono_su_t1980, honor_violence_legitimacy__composite_reading, suppression_requirement, 1980, 0.67).
narrative_ontology:measurement(hono_su_t2000, honor_violence_legitimacy__composite_reading, suppression_requirement, 2000, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'honor_violence_legitimacy' kernel family. It represents the 'composite_reading', which integrates the 'drop_reading' (external costs) and 'contraction_reading' (conceptual redefinition) as simultaneous drivers of decline, arguing that neither single mechanism is sufficient alone to explain the observed erosion of legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
