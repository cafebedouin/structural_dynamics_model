% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers (Commander-in-Chief Reading)
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint story instantiates the 'inherent executive' reading of
 *   the war powers allocation kernel. It describes the claim that the
 *   Commander-in-Chief power grants the President inherent authority to
 *   deploy military force in defense of national interests without prior
 *   congressional authorization. This reading centralizes war-making
 *   decisions in the executive, treating congressional authorization as a
 *   courtesy rather than a constitutional requirement. The metrics reflect a
 *   high degree of extraction from congressional power and active suppression
 *   of legislative alternatives, alongside a coordination function for swift
 *   action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.78).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.85).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '020828c1-e857-467e-a377-a97929cc8951').
narrative_ontology:cs_kernel_codification('020828c1-e857-467e-a377-a97929cc8951', fixed_text).
narrative_ontology:cs_authority_grounding('020828c1-e857-467e-a377-a97929cc8951', lineage).
narrative_ontology:cs_interpretation_layer_present('020828c1-e857-467e-a377-a97929cc8951').
narrative_ontology:cs_reading_relation('020828c1-e857-467e-a377-a97929cc8951', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('020828c1-e857-467e-a377-a97929cc8951', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('020828c1-e857-467e-a377-a97929cc8951', foundational, executive_unity_in_foreign_affairs).
narrative_ontology:cs_axiom_status(executive_unity_in_foreign_affairs, holdable).
narrative_ontology:cs_axiom_grounding('020828c1-e857-467e-a377-a97929cc8951', executive_unity_in_foreign_affairs, instrumental).
narrative_ontology:cs_axiom('020828c1-e857-467e-a377-a97929cc8951', foundational, commander_in_chief_plenary_power).
narrative_ontology:cs_axiom_status(commander_in_chief_plenary_power, holdable).
narrative_ontology:cs_axiom_grounding('020828c1-e857-467e-a377-a97929cc8951', commander_in_chief_plenary_power, conventional).
narrative_ontology:cs_reference_frame('020828c1-e857-467e-a377-a97929cc8951', post_wwii_executive_ascendancy).
narrative_ontology:cs_drift_state('020828c1-e857-467e-a377-a97929cc8951', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('020828c1-e857-467e-a377-a97929cc8951', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, us_congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, constitutional_checks_and_balances).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, us_military).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, american_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As Commander-in-Chief, the President claims and exercises inherent authority to deploy military force in defense of national interests, often without prior congressional authorization. This reading grants maximum flexibility and speed in foreign policy and military action.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_president, agenda_setter,
    institutional, biographical, arbitrage, global).

% Benefits from the expanded authority of the President, allowing for more streamlined decision-making and execution of foreign policy and military operations without the delays or political constraints of congressional approval.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary,
    institutional, generational, constrained, global).

% Bears the cost of diminished constitutional war powers, as its authority to declare war and authorize military force is bypassed or treated as a courtesy. Its options are to legislate limits (often vetoed), use appropriations (difficult to block ongoing operations), or litigate (courts often defer).
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, us_congress, payer,
    institutional, generational, constrained, national).

% Analyze and debate the constitutional implications of executive war powers, often highlighting the erosion of congressional authority and the shift in the balance of powers. Their influence is primarily academic and advisory.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Benefits from clear, swift orders and unified command in military operations, avoiding potential delays or political interference from Congress. Its primary role is to execute presidential directives.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, us_military, beneficiary,
    organized, immediate, constrained, global).

% Bears the costs of military engagements (lives, resources) initiated by executive action, often without full public debate or explicit democratic consent through their elected representatives. Their ability to influence policy is indirect and often reactive.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, american_public, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, the_president).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the United States to respond swiftly and decisively to perceived threats to national security and interests, providing a unified command structure for military action.
% TRANSFER_FUNCTION: Transfers the primary decision-making authority for deploying military force from the legislative branch (Congress) to the executive branch (President), centralizing control over war powers.
% ABSENT_VOICES: International law advocates would argue for adherence to UN Charter principles and international legal frameworks for the use of force. Civil liberties groups would raise concerns about unchecked executive power and its potential impact on democratic accountability and individual rights.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the President would be constitutionally required to seek explicit congressional authorization for most military deployments beyond immediate self-defense. This would fundamentally alter the process of foreign policy, military intervention, and the balance of power between the branches of government.
% FOUNDING_PROBLEM: The need for a single, decisive actor capable of responding to sudden foreign threats and conducting foreign policy with speed and unity, particularly in times of war or crisis.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch officials and some legal scholars argue that the need for swift, decisive action in a complex global security environment remains live. However, many constitutional scholars and members of Congress contend that the original problem has been over-addressed, leading to an imbalance of power; their testimony and historical analysis provide corroboration for the contested status.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because this reading effectively reallocates significant constitutional power from Congress to the President, allowing the executive to initiate costly military actions. Suppression is very high (0.85) as the executive actively resists legislative attempts to reclaim or constrain war powers, often through legal interpretations and political maneuvering that limit congressional options. Theater ratio is moderate (0.45) reflecting instances where the executive engages in performative consultation with Congress (e.g., War Powers Resolution reports) while maintaining its unilateral authority. Accessibility collapse is high (0.70) because the executive's consistent assertion of inherent authority has made it difficult for Congress to effectively assert its own constitutional role, collapsing practical alternatives to unilateral action. Resistance is moderate (0.60) from Congress and legal scholars, but this resistance has largely failed to reverse the trend of executive power expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's perspective, this reading is a necessary adaptation to modern global threats, ensuring national security. From Congress's perspective, it represents an unconstitutional usurpation of power, eroding democratic accountability. The engine will compute these divergent classifications based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and the Executive Branch are clear beneficiaries, gaining expanded authority and operational flexibility (low directionality). Congress and the constitutional checks and balances are the primary targets, losing their constitutional prerogative over war powers (high directionality). The American public bears the diffuse costs of wars initiated under this authority, often without direct democratic input. The US military benefits from clear command but is constrained by its duty to follow orders.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_national_interest,
    'How broadly can ''national interests'' be interpreted to justify unilateral executive military action?',
    'Judicial review establishing clear, narrow definitions of ''national interest'' that trigger inherent executive authority, or legislative action defining specific criteria for such interventions.',
    'A narrow interpretation would significantly constrain executive power, potentially reclassifying the constraint towards a Rope or Scaffold. A broad interpretation would reinforce its extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_national_interest, conceptual, 'Ambiguity in defining ''national interests'' as a trigger for unilateral force.').

omega_variable(
    necessity_of_unilateral_action,
    'Is unilateral executive action truly necessary for effective national security in all cases where it is invoked, or could congressional authorization be obtained without compromising security?',
    'Empirical analysis of historical interventions: comparing outcomes of unilateral actions versus those with prior congressional authorization, and assessing the feasibility of seeking authorization in specific crisis scenarios.',
    'If unilateral action is often not strictly necessary, the ''coordination'' aspect of this reading weakens, pushing the classification closer to a Snare. If it is consistently necessary, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_unilateral_action, empirical, 'Whether the claimed necessity of unilateral action is empirically justified.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''inherent executive'' reading of war powers, or is it a strategic framing to justify executive power expansion?',
    'Analysis of executive branch legal opinions (e.g., OLC memos) over time, comparing their consistency with originalist or textualist interpretations versus their alignment with contemporary executive policy goals. Cross-referencing with historical practice and constitutional debates.',
    'If primarily a strategic framing, the ''inherent'' aspect is weakened, and the constraint''s extractive nature becomes more pronounced, potentially shifting classification towards a Snare. If genuinely rooted in a consistent constitutional theory, the Tangled Rope classification is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the true constitutional grounding versus strategic justification of inherent executive war powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1950, war_powers_allocation__inherent_executive_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(war__tr_t1960, war_powers_allocation__inherent_executive_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(war__tr_t1970, war_powers_allocation__inherent_executive_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(war__tr_t1980, war_powers_allocation__inherent_executive_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(war__tr_t1990, war_powers_allocation__inherent_executive_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(war__tr_t2000, war_powers_allocation__inherent_executive_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(war__tr_t2010, war_powers_allocation__inherent_executive_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(war__tr_t2020, war_powers_allocation__inherent_executive_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(war__be_t1950, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(war__be_t1960, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(war__be_t1970, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(war__be_t1980, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(war__be_t1990, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(war__be_t2000, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(war__be_t2010, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement(war__be_t2020, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1950, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(war__su_t1960, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(war__su_t1970, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(war__su_t1980, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(war__su_t1990, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(war__su_t2000, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(war__su_t2010, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(war__su_t2020, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_oversight_of_executive).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, treaty_ratification_process).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, executive_privilege_claims).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel. It focuses on the executive's claim of inherent authority, distinct from readings emphasizing congressional primacy or functional accommodation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
