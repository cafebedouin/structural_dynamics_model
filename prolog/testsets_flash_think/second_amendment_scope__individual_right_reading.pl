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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms Reading
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the 'individual right' reading of the
 *   Second Amendment, which interprets the right to keep and bear arms as
 *   belonging to individuals for self-defense, unconnected to militia
 *   service. This reading, solidified by Supreme Court decisions like D.C. v.
 *   Heller (2008) and NYSRPA v. Bruen (2022), significantly limits the power
 *   of state and local governments to regulate firearms. The claimed type
 *   'rope' reflects the ideal of coordinating individual liberty and
 *   self-defense, while the high extractiveness and suppression metrics
 *   reflect the actual structural impact on state regulatory authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.7).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.8).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment: Individual Right to Bear Arms Reading").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '0956af7a-ed1f-4518-b8ed-46d6b5ba2269').
narrative_ontology:cs_kernel_codification('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', fixed_text).
narrative_ontology:cs_authority_grounding('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', lineage).
narrative_ontology:cs_interpretation_layer_present('0956af7a-ed1f-4518-b8ed-46d6b5ba2269').
narrative_ontology:cs_reading_relation('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', foundational, individual_self_defense_fundamental_right).
narrative_ontology:cs_axiom_status(individual_self_defense_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', individual_self_defense_fundamental_right, deontological).
narrative_ontology:cs_axiom('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', foundational, militia_clause_prefatory_not_limiting).
narrative_ontology:cs_axiom_status(militia_clause_prefatory_not_limiting, holdable).
narrative_ontology:cs_axiom_grounding('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', militia_clause_prefatory_not_limiting, conventional).
narrative_ontology:cs_reference_frame('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', post_heller_jurisprudence).
narrative_ontology:cs_drift_state('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', contemporary_judicial_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0956af7a-ed1f-4518-b8ed-46d6b5ba2269', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers_and_sellers).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who own firearms for self-defense, sport, or collection. This reading grants them a fundamental right, largely unencumbered by state regulation, making them primary beneficiaries. Their identity is often tied to this right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Businesses that profit from the sale and distribution of firearms and accessories. This reading expands their market and reduces regulatory burdens, making them significant beneficiaries.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers_and_sellers, beneficiary,
    institutional, generational, arbitrage, national).

% State governmental bodies responsible for enacting laws. This reading severely constrains their ability to regulate firearms, forcing them to bear the costs of increased gun violence and limited policy options.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, biographical, constrained, national).

% Organizations and individuals campaigning for stricter gun control measures to reduce gun violence. This reading undermines their policy goals and forces them to operate within a highly restrictive legal framework.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% The ultimate arbiter of constitutional interpretation, whose rulings (e.g., Heller, Bruen) have established and expanded this individual rights reading, effectively setting the agenda for firearms regulation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Legal scholars and advocates who argue the Second Amendment protects only a state's right to maintain a militia, not individual gun ownership. Their interpretation has been largely rejected by the Supreme Court, excluding them from the dominant legal discourse.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, collective_right_proponents, excluded,
    organized, generational, constrained, national).

% Advocates who believe the right to bear arms is individual but conditioned on militia service. While closer to the individual right, their emphasis on militia connection is sidelined by the current dominant reading, making them effectively excluded from its full scope.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, civic_right_proponents, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual self-defense and the capacity for citizens to resist potential tyranny, ensuring a populace capable of armed self-protection.
% TRANSFER_FUNCTION: Transfers significant regulatory power over firearms from state and local governments to individual citizens and the federal judiciary; it also transfers the burden of managing gun violence from state policy to individual responsibility and federal court oversight.
% ABSENT_VOICES: Proponents of the collective and civic right readings are largely absent from the current dominant legal and political conversation, as their interpretations have been superseded by the individual rights framework. They would argue for greater state regulatory authority and a stronger connection to militia service.
% DISAPPEARANCE_RATIONALE: If this individual rights interpretation vanished overnight, state and federal governments would likely move swiftly to enact comprehensive gun control legislation, fundamentally altering gun ownership, public safety, and the balance of power between states and the federal judiciary.
% FOUNDING_PROBLEM: To ensure the security of a free state by protecting the right of the people to keep and bear arms, understood as both an individual right and a component of a well-regulated militia.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the individual right reading cite historical context emphasizing individual liberty and self-defense. Opponents, including many historians and legal scholars, contest this, arguing the original intent was primarily tied to the collective defense and militia service, with corroboration from early state constitutions and historical treatises that emphasize the militia context.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because this reading effectively extracts regulatory power from state legislatures, preventing them from enacting desired public safety measures. Suppression is also high (0.8) as it actively suppresses alternative interpretations and state-level policy choices through judicial enforcement. Theater ratio is low (0.1) because the judicial enforcement of this right is direct and functional, not performative. Accessibility collapse is moderate (0.6) as it severely limits state alternatives for gun control, but doesn't entirely eliminate all policy options. Resistance is moderate (0.5) due to ongoing political and legal challenges from public safety advocates and some state governments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of firearms owners and manufacturers, this constraint is a vital protection of liberty and a coordination mechanism for self-defense. From the perspective of state legislatures and public safety advocates, it is a highly extractive and suppressive force that undermines their ability to protect citizens. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Firearms owners and manufacturers are clear beneficiaries, as the constraint protects and expands their rights and markets. State legislatures and public safety advocates are victims, as their regulatory and policy goals are suppressed. The Supreme Court acts as the agenda-setter, defining and enforcing the scope of this right.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Was the original intent of the Second Amendment primarily to protect an individual right to self-defense, or a collective right related to militia service, or an individual right conditioned on militia service?',
    'Further historical and legal scholarship, potentially new textual discoveries, or a constitutional amendment clarifying intent.',
    'Resolution would fundamentally alter the beneficiary and victim sets, and thus the extractiveness and suppression of the constraint. If a collective or civic right reading were established, state regulatory power would increase significantly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity regarding the Second Amendment''s original intent and its impact on the scope of individual vs. collective rights.').

omega_variable(
    public_safety_impact_quantification,
    'What is the quantifiable impact of this individual rights reading on public safety outcomes (e.g., rates of gun violence, accidental deaths)?',
    'Longitudinal epidemiological studies, comparative analysis across jurisdictions with different regulatory regimes, and robust statistical modeling.',
    'Empirical evidence of severe negative public safety impacts would strengthen arguments for reinterpreting or amending the Second Amendment, potentially shifting the constraint''s classification towards a Snare from the public''s perspective. Conversely, evidence of no significant impact would bolster the current reading''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_impact_quantification, empirical, 'Quantifying the causal link between the individual rights interpretation and public safety outcomes.').

omega_variable(
    reading_vs_constructed_right,
    'Is the individual right to bear arms a genuine, inherent right discovered through constitutional interpretation, or a constructed legal fiction serving specific political and economic interests?',
    'Philosophical and jurisprudential debate, coupled with analysis of lobbying efforts and campaign finance related to firearms policy. The question is ultimately conceptual and preference-driven.',
    'If primarily a constructed legal fiction, the constraint''s legitimacy would be undermined, potentially reclassifying it as a Snare or Tangled Rope from a critical perspective, highlighting the extraction from state sovereignty and public safety.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_constructed_right, conceptual, 'Whether the individual right is an inherent right or a legal construct.').

omega_variable(
    committer_frame_individual_right_reading,
    'This constraint is the ''individual_right_reading'' of the ''second_amendment_scope'' kernel. What would be the structural changes if a sibling reading were adopted?',
    'Analysis of hypothetical Supreme Court rulings or constitutional amendments adopting the ''collective_right_reading'' or ''civic_right_reading''.',
    'If the ''collective_right_reading'' were adopted, individual firearms owners would largely move from beneficiary to payer/victim, and state legislatures would move from victim to beneficiary, drastically reducing extractiveness from state power. If the ''civic_right_reading'' were adopted, individual rights would be conditioned on militia service, altering the beneficiary set and increasing state regulatory capacity related to militia organization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_individual_right_reading, conceptual, 'Structural impact of alternative Second Amendment interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_scope__individual_right_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_scope__individual_right_reading, theater_ratio, 1985, 0.07).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_scope__individual_right_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_scope__individual_right_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__individual_right_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_scope__individual_right_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(seco_be_t1985, second_amendment_scope__individual_right_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(seco_be_t2000, second_amendment_scope__individual_right_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(seco_be_t2016, second_amendment_scope__individual_right_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__individual_right_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_scope__individual_right_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(seco_su_t1985, second_amendment_scope__individual_right_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(seco_su_t2000, second_amendment_scope__individual_right_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.75).
narrative_ontology:measurement(seco_su_t2016, second_amendment_scope__individual_right_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__individual_right_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, public_safety_policy).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment's scope. Each reading constitutes a separate constraint due to differing ε values, beneficiary/victim sets, and structural impacts. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
