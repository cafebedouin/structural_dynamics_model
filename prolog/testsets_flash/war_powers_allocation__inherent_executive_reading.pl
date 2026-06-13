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
 *   This constraint describes the 'inherent executive' reading of war powers
 *   allocation, where the President, as Commander-in-Chief, possesses
 *   inherent authority to deploy military force in defense of national
 *   interests without prior Congressional authorization. Congressional
 *   authorization is treated as a courtesy or a post-hoc ratification through
 *   appropriations, rather than a constitutional prerequisite. This reading
 *   has gained prominence through executive branch legal interpretations and
 *   historical practice, particularly since the mid-20th century.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.75).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '3fb37811-7dc6-4b81-9d92-0374f6679382').
narrative_ontology:cs_kernel_codification('3fb37811-7dc6-4b81-9d92-0374f6679382', fixed_text).
narrative_ontology:cs_authority_grounding('3fb37811-7dc6-4b81-9d92-0374f6679382', lineage).
narrative_ontology:cs_interpretation_layer_present('3fb37811-7dc6-4b81-9d92-0374f6679382').
narrative_ontology:cs_reading_relation('3fb37811-7dc6-4b81-9d92-0374f6679382', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fb37811-7dc6-4b81-9d92-0374f6679382', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('3fb37811-7dc6-4b81-9d92-0374f6679382', foundational, executive_unity_in_foreign_affairs).
narrative_ontology:cs_axiom_status(executive_unity_in_foreign_affairs, holdable).
narrative_ontology:cs_axiom_grounding('3fb37811-7dc6-4b81-9d92-0374f6679382', executive_unity_in_foreign_affairs, deontological).
narrative_ontology:cs_axiom('3fb37811-7dc6-4b81-9d92-0374f6679382', foundational, commander_in_chief_as_sole_initiator).
narrative_ontology:cs_axiom_status(commander_in_chief_as_sole_initiator, holdable).
narrative_ontology:cs_axiom_grounding('3fb37811-7dc6-4b81-9d92-0374f6679382', commander_in_chief_as_sole_initiator, conventional).
narrative_ontology:cs_reference_frame('3fb37811-7dc6-4b81-9d92-0374f6679382', unitary_executive_national_security).
narrative_ontology:cs_drift_state('3fb37811-7dc6-4b81-9d92-0374f6679382', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3fb37811-7dc6-4b81-9d92-0374f6679382', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch_agencies).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, the_judiciary).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, public_discourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, international_allies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As Commander-in-Chief, the President asserts and exercises inherent authority to deploy military force, interpreting 'national interests' broadly. This position allows for rapid, unilateral action, bypassing legislative checks and consolidating power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_president, agenda_setter,
    institutional, biographical, arbitrage, national).

% Agencies like the Department of Defense and State Department benefit from the President's inherent authority, gaining flexibility and speed in implementing foreign policy and military operations without constant legislative interference. They provide the operational capacity for executive action.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch_agencies, beneficiary,
    institutional, generational, constrained, national).

% Congress bears the cost of diminished constitutional authority over war-making. Its options are limited to post-hoc funding decisions, non-binding resolutions, or politically costly impeachment proceedings, rather than pre-emptive authorization. This reduces its ability to shape foreign policy and military engagements.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% The judiciary is often reluctant to rule on war powers disputes, citing political question doctrine or lack of standing. This effectively cedes ground to the executive, making it a passive enabler of executive unilateralism and a victim of its own institutional self-restraint.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_judiciary, payer,
    institutional, civilizational, trapped, national).

% The public loses a robust democratic debate on military interventions when decisions are made unilaterally by the executive. This reduces accountability and can lead to prolonged engagements without broad societal consensus, bearing the costs in lives and resources.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, public_discourse, payer,
    moderate, immediate, constrained, national).

% Allies can benefit from the speed and decisiveness of US executive action in crises, as it allows for rapid response to shared threats. However, they also bear the risk of being drawn into conflicts initiated without broad domestic consensus.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, international_allies, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, the_president).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable rapid, decisive military action in defense of perceived national interests, particularly in situations requiring speed and secrecy, thereby coordinating national security responses.
% TRANSFER_FUNCTION: Transfers the authority to initiate military force from Congress to the President, along with the associated political capital, resources, and accountability burdens.
% ABSENT_VOICES: Constitutional scholars advocating for strict adherence to Congressional war powers, and segments of the public who demand greater democratic accountability for military interventions, are often marginalized in the executive's framing of 'inherent authority'.
% DISAPPEARANCE_RATIONALE: If the inherent executive war powers vanished overnight, the President would be severely constrained in deploying force, requiring explicit and timely Congressional authorization for most military actions. This would fundamentally alter the balance of power, slow down responses, and force a re-negotiation of foreign policy decision-making.
% FOUNDING_PROBLEM: The need for a unified, decisive command structure to respond to immediate threats and protect national security, particularly in a world with rapidly evolving geopolitical challenges.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch legal opinions and national security advisors consistently attest to the live nature of this problem, citing ongoing threats and the need for agility. While some in Congress and academia contest the *solution* (unilateral executive power), the underlying problem of national security threats is widely acknowledged as live.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because this reading allows the executive to commit national resources and lives without direct legislative consent, imposing costs on Congress (loss of war-making authority) and the public (unilateral military engagements). Suppression (0.75) is high due to the executive's ability to act quickly and present Congress with a fait accompli, limiting legislative options to defunding (politically difficult) or post-hoc condemnation. The theater ratio (0.4) reflects that while some executive actions genuinely require speed and secrecy, a significant portion of the 'national interest' justification serves to bypass legislative checks.
 *
 * PERSPECTIVAL GAP:
 *   The President and executive branch agencies experience this as a necessary flexibility for national security, enabling decisive action. Congress, however, experiences it as an erosion of its constitutional war-making authority, reducing its role to oversight and funding rather than initiation. The judiciary is often reluctant to intervene, further entrenching executive power.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and executive branch agencies are clear beneficiaries (d=0.0-0.1) as they gain significant unilateral power. Congress is a primary victim (d=0.9-1.0) as its constitutional role is diminished. The judiciary is also a victim (d=0.7-0.8) due to its limited ability to check executive action. Public discourse is a victim (d=0.8-0.9) as it loses a critical forum for debate on military interventions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it purports to coordinate national defense (a genuine collective action problem) but does so through asymmetric extraction, concentrating power in the executive at the expense of legislative checks. The 'inherent authority' claim serves as cover for this extraction. Mandatrophy is present in the sense that the original constitutional intent for war powers (requiring Congressional declaration) has atrophied in practice, replaced by executive assertion of inherent authority. Resolving this would require reasserting Congressional primacy or a clear constitutional re-allocation of powers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_delegated_authority,
    'Is the President''s authority to deploy force truly inherent, or is it a delegated power from Congress that has been expanded through practice?',
    'Supreme Court ruling explicitly defining the scope of inherent executive power in war-making, or a constitutional amendment clarifying war powers allocation.',
    'If delegated, the constraint''s legitimacy would shift from constitutional interpretation to legislative intent, potentially increasing congressional leverage and reducing executive unilateralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherent_vs_delegated_authority, conceptual, 'Ambiguity between inherent and delegated executive authority.').

omega_variable(
    congressional_acquiescence_vs_consent,
    'Does Congressional funding of military operations imply consent or merely acquiescence to executive action?',
    'Clear legislative action (e.g., War Powers Resolution enforcement, explicit non-funding of unauthorized operations) that distinguishes between funding and authorization.',
    'If acquiescence, the constraint''s extractive nature from Congress is clearer; if consent, Congress is a more active participant, and the constraint leans more towards coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_acquiescence_vs_consent, empirical, 'Ambiguity of Congressional role in funding unauthorized military actions.').

omega_variable(
    reading_of_war_powers_allocation,
    'This constraint is the ''inherent_executive_reading'' of the ''war_powers_allocation'' kernel. What would change if the ''congressional_primacy_reading'' were adopted?',
    'A shift in judicial precedent or political practice to prioritize explicit congressional authorization for military force beyond immediate defense.',
    'The ''congressional_primacy_reading'' would significantly increase the power of Congress (moving it from victim to agenda_setter), reduce executive unilateralism, and likely decrease the overall extractiveness of the war powers allocation by requiring broader consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_war_powers_allocation, conceptual, 'Impact of adopting the ''congressional_primacy_reading'' of war powers.').

omega_variable(
    reading_of_war_powers_allocation_functional_accommodation,
    'This constraint is the ''inherent_executive_reading'' of the ''war_powers_allocation'' kernel. What would change if the ''functional_accommodation_reading'' were adopted?',
    'A shift in political practice and judicial interpretation to explicitly differentiate between types of military deployments, with varying authorization requirements.',
    'The ''functional_accommodation_reading'' would introduce more nuance, potentially reducing executive unilateralism for prolonged campaigns while preserving flexibility for immediate threats. This would make the constraint less consistently extractive from Congress, depending on the specific operational context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_war_powers_allocation_functional_accommodation, conceptual, 'Impact of adopting the ''functional_accommodation_reading'' of war powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__inherent_executive_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__inherent_executive_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__inherent_executive_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__inherent_executive_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__inherent_executive_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__inherent_executive_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, executive_order_authority).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, national_security_classification).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel. Each reading represents a distinct structural claim about the allocation of war powers, with different beneficiaries, victims, and levels of extraction. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
