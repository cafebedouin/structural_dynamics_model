% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Firearms Ownership
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, which interprets the right to bear arms as belonging to
 *   individuals for purposes unconnected to militia service. This reading
 *   gained significant jurisprudential traction, particularly after the 2008
 *   Heller decision, and has substantially constrained state and federal
 *   efforts to regulate firearms. The claimed type is 'tangled_rope' because
 *   it provides a coordination function (clarity for gun owners) but also
 *   involves significant asymmetric extraction (costs borne by public safety
 *   advocates and victims of gun violence) and requires active enforcement to
 *   maintain against regulatory challenges.
 *
 * KEY AGENTS:
 *   - individual_firearms_owners: Primary beneficiary (organized/identity_locked)
 *   - firearms_manufacturers: Primary beneficiary (powerful/arbitrage)
 *   - firearms_lobby: Agenda setter (institutional/arbitrage)
 *   - state_legislatures: Primary payer (institutional/constrained)
 *   - gun_violence_victims: Primary payer (powerless/trapped)
 *   - public_safety_advocates: Payer (organized/constrained)
 *   - constitutional_scholars: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment: Individual Right to Firearms Ownership").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'd4497595-2607-488a-9cf5-b12d898668c7').
narrative_ontology:cs_kernel_codification('d4497595-2607-488a-9cf5-b12d898668c7', fixed_text).
narrative_ontology:cs_authority_grounding('d4497595-2607-488a-9cf5-b12d898668c7', lineage).
narrative_ontology:cs_interpretation_layer_present('d4497595-2607-488a-9cf5-b12d898668c7').
narrative_ontology:cs_reading_relation('d4497595-2607-488a-9cf5-b12d898668c7', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('d4497595-2607-488a-9cf5-b12d898668c7', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('d4497595-2607-488a-9cf5-b12d898668c7', foundational, individual_right_unconnected_to_militia).
narrative_ontology:cs_axiom_status(individual_right_unconnected_to_militia, holdable).
narrative_ontology:cs_axiom_grounding('d4497595-2607-488a-9cf5-b12d898668c7', individual_right_unconnected_to_militia, deontological).
narrative_ontology:cs_axiom('d4497595-2607-488a-9cf5-b12d898668c7', secondary, self_defense_is_fundamental_right).
narrative_ontology:cs_axiom_status(self_defense_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('d4497595-2607-488a-9cf5-b12d898668c7', self_defense_is_fundamental_right, deontological).
narrative_ontology:cs_reference_frame('d4497595-2607-488a-9cf5-b12d898668c7', post_heller_individual_right).
narrative_ontology:cs_drift_state('d4497595-2607-488a-9cf5-b12d898668c7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d4497595-2607-488a-9cf5-b12d898668c7', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_lobby).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, individual_liberty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, self_defense_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protection of their right to own firearms for various purposes, including self-defense, sport, and collection, without a direct connection to militia service. Their identity is often tied to this right, making 'exit' (relinquishing ownership or advocacy) highly constrained.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_firearms_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Benefit from a broad interpretation of gun rights that expands the market for their products. They actively lobby to maintain and strengthen this interpretation, leveraging legal challenges and political influence.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers, beneficiary,
    powerful, generational, arbitrage, national).

% Acts as the primary institutional force advocating for and defending the individual right interpretation. They fund legal challenges, political campaigns, and public relations efforts to shape jurisprudence and public opinion, effectively setting the agenda for gun rights policy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_lobby, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the costs of constrained regulatory authority over firearms. Their efforts to enact public safety laws (e.g., universal background checks, assault weapon bans) are frequently challenged and overturned based on this individual rights interpretation, leading to increased legal and social costs.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, generational, constrained, regional).

% Suffer direct harm from gun violence, which advocates argue is exacerbated by the broad availability of firearms under this interpretation. They are often trapped by the consequences of violence and have limited direct power to influence policy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, local).

% Bear the costs of continuous advocacy for stricter gun control measures, often facing significant opposition and legal setbacks due to the individual rights interpretation. Their efforts are aimed at mitigating the perceived negative externalities of broad gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_safety_advocates, payer,
    organized, biographical, constrained, national).

% Analyze the historical context, textual meaning, and jurisprudential evolution of the Second Amendment. They provide academic interpretations that inform legal arguments but do not directly participate in the enforcement or benefit from the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit contested, legal framework for individual firearms ownership, reducing ambiguity for owners and manufacturers regarding their rights and responsibilities under federal law.
% TRANSFER_FUNCTION: Transfers the burden of gun violence and regulatory costs from individual firearms owners and the firearms industry to state governments and the public, by limiting the scope of state-level gun control.
% ABSENT_VOICES: Future generations, who will inherit the consequences of current gun policy, and communities disproportionately affected by gun violence, whose perspectives are often marginalized in the national debate, would advocate for a more restrictive interpretation prioritizing collective safety.
% DISAPPEARANCE_RATIONALE: If this individual rights interpretation vanished overnight, state and federal governments would immediately move to enact stricter gun control laws, leading to a significant reduction in firearms availability, a restructuring of the firearms industry, and a fundamental shift in the legal landscape surrounding gun ownership.
% FOUNDING_PROBLEM: The Second Amendment was established to ensure the security of a free state through a well-regulated militia, and to protect the right of the people to keep and bear arms.
% FOUNDING_PROBLEM_CORROBORATION: The firearms lobby and many individual owners attest the problem of individual self-defense and protection against potential tyranny is still live. Constitutional scholars and public safety advocates argue the original problem of militia service is largely obsolete in its 18th-century form, and the contemporary problem is gun violence, not militia readiness; historical analysis and public health data from outside the benefiting parties support this shifted-function reading.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the broad interpretation of individual rights imposes significant costs on society in terms of gun violence and limited regulatory capacity, while concentrating benefits on gun owners and the firearms industry. Suppression (0.75) is high due to the active legal and political enforcement required to maintain this interpretation against legislative efforts and public pressure. Theater ratio (0.20) is low, as the constraint's operation is genuinely functional in protecting gun ownership, even if its coordination story is contested. The temporal measurements reflect the increasing judicial and political entrenchment of this reading, leading to rising extractiveness and suppression over time, particularly after key Supreme Court decisions.
 *
 * PERSPECTIVAL GAP:
 *   The individual rights reading is experienced as a fundamental protection by firearms owners and the industry, while public safety advocates and state legislatures experience it as a significant impediment to public welfare. The engine's per-seat classification will reflect this divergence: a 'rope' or 'mountain' for beneficiaries, and a 'snare' or 'tangled_rope' for payers, demonstrating how the same legal text can instantiate different constraint types depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual firearms owners, manufacturers, and the firearms lobby are beneficiaries (low d) as the constraint protects and expands their interests. State legislatures, gun violence victims, and public safety advocates are targets (high d) as they bear the costs of limited regulatory power and the consequences of gun violence. Constitutional scholars are analytical observers (d=0.5). The 'identity_locked' exit option for individual owners reflects the deep personal and cultural significance of gun ownership for many, making relinquishing this right or advocacy for it highly unlikely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this constraint as a pure 'rope' (as proponents might claim) by highlighting the significant asymmetric extraction and active enforcement required. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function it provides for gun owners. The 'contested' status of the founding problem further supports the tangled_rope classification, indicating a shift from its original mandate towards a more extractive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine individual right, or a constructed legal interpretation that benefits identifiable agents?',
    'Comparative legal analysis of other constitutional rights and their historical evolution, alongside empirical studies of the social and economic impacts of this specific interpretation.',
    'If primarily constructed, the constraint''s effective extractiveness is higher than its stated coordination function suggests, potentially reclassifying it closer to a ''snare'' for affected parties. If a genuine individual right, its coordination function is more central.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between a natural right and a constructed legal interpretation.').

omega_variable(
    militia_clause_severability,
    'Is the ''right of the people to keep and bear arms'' clause truly unconnected to the ''well regulated Militia'' clause, or is the connection merely downplayed in this reading?',
    'Historical textual analysis of 18th-century legal and political discourse, and examination of originalist arguments that emphasize the militia context.',
    'If the clauses are found to be strongly connected, this reading''s claim of an unconnected individual right is weakened, potentially shifting its classification towards a ''collective_right_reading'' or ''civic_right_reading'' for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_severability, empirical, 'Whether the individual right is truly severable from the militia clause.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal precedent, political lobbying) or internalized (cultural identity, fear of government overreach)?',
    'Post-judicial shift analysis: if regulatory efforts persist after a hypothetical weakening of the individual right interpretation, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target (e.g., state legislatures) carries the suppression with them after legal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for regulatory efforts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_scope__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_scope__individual_right_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_scope__individual_right_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_scope__individual_right_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_scope__individual_right_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(seco_be_t1985, second_amendment_scope__individual_right_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(seco_be_t2000, second_amendment_scope__individual_right_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(seco_be_t2016, second_amendment_scope__individual_right_reading, base_extractiveness, 2016, 0.67).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__individual_right_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_scope__individual_right_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(seco_su_t1985, second_amendment_scope__individual_right_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(seco_su_t2000, second_amendment_scope__individual_right_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(seco_su_t2016, second_amendment_scope__individual_right_reading, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__individual_right_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, gun_control_legislation_effectiveness).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, public_safety_funding_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_scope' kernel. It focuses on the individual right interpretation, distinct from collective_right_reading and civic_right_reading, which emphasize state authority or militia service respectively. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
