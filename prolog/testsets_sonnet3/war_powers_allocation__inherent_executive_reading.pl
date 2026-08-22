% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Inherent Executive War Powers Reading (Commander-in-Chief Unilateralism)
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This story instantiates the inherent_executive_reading of the
 *   war_powers_allocation kernel: the claim that Article II's
 *   Commander-in-Chief clause grants the president freestanding authority to
 *   deploy force in defense of national interests without prior congressional
 *   authorization, with congressional appropriations functioning as
 *   after-the-fact ratification rather than a genuine ex-ante check. This is
 *   a distinct constraint from the congressional_primacy_reading (which holds
 *   authorization is constitutionally required) and the
 *   functional_accommodation_reading (which conditions the answer on
 *   operational context) — under DP-001 ε-invariance, each reading is
 *   authored as its own constraint with its own beneficiary/victim structure
 *   and its own ε, not as one story averaged across positions.
 *
 * KEY AGENTS:
 *   - executive_branch: agenda-setter and primary beneficiary — controls deployment timing and legal characterization
 *   - congress_war_power: structural payer — holds nominal power that arrives too late to bind the decision
 *   - deployed_service_members: powerless payer bearing direct physical risk with no voice
 *   - affected_foreign_populations: powerless payer external to the domestic process entirely
 *   - constitutional_law_scholars: analytical observer tracking doctrinal drift from founding-era text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.32).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Reading (Commander-in-Chief Unilateralism)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '1f6cb02f-b787-45c8-9423-90171033d900').
narrative_ontology:cs_kernel_codification('1f6cb02f-b787-45c8-9423-90171033d900', fixed_text).
narrative_ontology:cs_authority_grounding('1f6cb02f-b787-45c8-9423-90171033d900', lineage).
narrative_ontology:cs_interpretation_layer_present('1f6cb02f-b787-45c8-9423-90171033d900').
narrative_ontology:cs_reading_relation('1f6cb02f-b787-45c8-9423-90171033d900', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('1f6cb02f-b787-45c8-9423-90171033d900', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('1f6cb02f-b787-45c8-9423-90171033d900', foundational, commander_in_chief_clause_grants_freestanding_defense_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_clause_grants_freestanding_defense_authority, holdable).
narrative_ontology:cs_axiom_grounding('1f6cb02f-b787-45c8-9423-90171033d900', commander_in_chief_clause_grants_freestanding_defense_authority, conventional).
narrative_ontology:cs_axiom('1f6cb02f-b787-45c8-9423-90171033d900', secondary, appropriations_vote_constitutes_sufficient_congressional_check).
narrative_ontology:cs_axiom_status(appropriations_vote_constitutes_sufficient_congressional_check, holdable).
narrative_ontology:cs_axiom_grounding('1f6cb02f-b787-45c8-9423-90171033d900', appropriations_vote_constitutes_sufficient_congressional_check, instrumental).
narrative_ontology:cs_reference_frame('1f6cb02f-b787-45c8-9423-90171033d900', commander_in_chief_repel_sudden_attack_doctrine).
narrative_ontology:cs_drift_state('1f6cb02f-b787-45c8-9423-90171033d900', post_9_11_expansive_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f6cb02f-b787-45c8-9423-90171033d900', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, standing_military_apparatus).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_bureaucracy).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress_war_power).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, constitutional_deliberation_norm).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, deployed_service_members).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, affected_foreign_populations).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, commander_in_chief_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines when force is deployed, characterizes the legal basis afterward, and controls classified intelligence justifying the deployment. Faces essentially no prior check; subsequent congressional response is limited to funding votes that arrive after the deployment is a fact on the ground, which functions as ratification rather than authorization.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary).

% Holds the constitutional declare-war and appropriations power on paper but in practice votes on funding for operations already underway, where withholding funds is framed as abandoning troops in the field. Lacks a pre-deployment veto; its formal authority is real but structurally arrives too late to bind the initial decision.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress_war_power, payer,
    institutional, generational, constrained, national).

% Receives sustained budgetary and operational continuity because deployment decisions are insulated from the slower, more contestable legislative authorization process. Institutional planning benefits from executive discretion over the timing and posture of force.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, standing_military_apparatus, beneficiary,
    institutional, generational, arbitrage, global).

% Operates within a legal framework (OLC opinions, executive precedent) that it itself helps generate, giving it durable interpretive control over what counts as 'defense of national interests' with minimal external contestability.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_bureaucracy, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the direct physical and mortal risk of decisions made without the deliberative check a declaration of war would impose. Cannot decline deployment once ordered; have no voice in the authorization decision at all.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, deployed_service_members, payer,
    powerless, immediate, trapped, global).

% Experience the consequences of force deployed under this reading — strikes, occupation, destabilization — with no standing in the U.S. constitutional process that authorized or failed to authorize the action against them.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, affected_foreign_populations, payer,
    powerless, immediate, trapped, regional).

% The norm that collective, deliberated decisions should precede war is a non-actor value 'held' by the constitutional order itself; every unilateral deployment made routine erodes the practice of seeking authorization before acting, thinning the norm for the future.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, constitutional_deliberation_norm, payer,
    moderate, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(war_powers_allocation__inherent_executive_reading, constitutional_deliberation_norm).

% Analyze the historical pattern of unilateral deployments (Korea, Kosovo, Libya, Syria strikes) against the constitutional text and founding-era debates, generally concluding the practice has drifted far from the original design regardless of doctrinal justification offered by successive administrations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid, unified response to genuine emergent threats where legislative deliberation would be too slow to prevent harm — a single decision-maker with command authority and access to time-sensitive intelligence can act before a threat matures.
% TRANSFER_FUNCTION: Moves the practical power to initiate armed conflict from the collective, deliberative body (Congress) to a single office (the presidency), and moves risk from institutional decision-makers to deployed personnel and foreign populations who have no voice in the decision.
% ABSENT_VOICES: Deployed service members and foreign civilians in target areas have no seat in the authorization process at all. Congress as an institution is present but structurally late — voting on appropriations after facts on the ground are set, which this reading treats as sufficient input.
% DISAPPEARANCE_RATIONALE: If this reading's unilateral deployment authority vanished overnight, every prospective military action would require prior congressional authorization, materially slowing response timelines, ending the appropriations-as-ratification pattern, and shifting institutional planning, procurement, and doctrine within the executive and military branches toward anticipatory legislative engagement.
% FOUNDING_PROBLEM: The Constitution's framers recognized that some threats (sudden attack, imminent invasion) require immediate response faster than a deliberative body can convene and vote — the Commander-in-Chief clause was meant to permit repelling sudden attacks without waiting for a declaration of war.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and its OLC attorneys attest the founding problem remains live and has expanded to cover any action defending broadly-construed 'national interests.' Constitutional law scholars, several retired members of Congress across party lines, and War Powers Resolution proponents attest from outside the benefiting institutions that the original narrow 'repel sudden attack' problem has been stretched to cover discretionary, non-emergency force projection the framers did not contemplate authorizing unilaterally.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.68 and rising over the measured interval because the pattern of unilateral deployment has accumulated as institutional practice (Korea through post-9/11 strikes), with each instance cited as precedent for the next, widening what counts as 'defense of national interests.' Suppression is authored comparatively low (0.32) because this reading does not require coercively closing off Congress's formal power — Congress retains the appropriations vote and the theoretical ability to legislate constraints (War Powers Resolution) — the constraint operates instead through structural timing (authorization arrives after facts are set) rather than direct suppression. Theater ratio is substantial and rising (0.55) because War Powers Resolution reporting requirements and post-hoc congressional briefings increasingly function as compliance performance around a decision already made.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch, standing military apparatus, and national security bureaucracy are beneficiaries: they gain operational speed, planning continuity, and durable interpretive control, placing them near the beneficiary end of directionality. Congress is a structural payer under this reading specifically because its constitutional role is real on paper but arrives late in practice, which the derivation captures as constrained exit despite institutional power. Deployed service members and affected foreign populations are trapped, powerless targets bearing the sharpest end of extraction with no voice in the authorizing decision at all — their directionality sits at the full-target end regardless of the executive's characterization of the action.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (repelling sudden attack before a deliberative body can convene) is genuinely live in narrow emergency cases, but this reading extends the same inherent-authority logic to non-emergency, discretionary force projection — the founding problem's status is authored as contested precisely because the reading's proponents and its outside critics disagree about whether the original narrow justification still covers the current scope of practice. Classifying this as tangled_rope (rather than snare) preserves the genuine coordination function for true emergencies while still registering the asymmetric extraction that has accreted onto it — collapsing it to pure extraction would erase the real defensive-speed rationale; treating it as a clean mountain or rope would erase the accumulating cost to congressional power, service members, and foreign populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_authority_scope_ambiguity,
    'Does the Commander-in-Chief clause''s inherent authority extend to any action framable as ''national interest defense,'' or only to repelling sudden attacks — and who authoritatively settles that boundary absent judicial review of political questions?',
    'Comparative analysis of founding-era Commander-in-Chief clause debates (Federalist 69, Pacificus-Helvidius) against the post-1945 pattern of executive deployments, cross-checked against the rare instances of judicial engagement (e.g., Youngstown''s concurrences) that touch war powers without full political-question avoidance.',
    'A narrow-scope resolution would reclassify most modern unilateral deployments as extraction beyond the constraint''s legitimate coordination function, pushing this reading toward snare; a broad-scope resolution would validate the reading''s coordination claim as intended rather than drifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_authority_scope_ambiguity, conceptual, 'Whether inherent executive authority was ever meant to cover discretionary force projection or only genuine sudden-attack response.').

omega_variable(
    kernel_reading_contest_location,
    'The war_powers_allocation kernel is read three ways (inherent_executive, congressional_primacy, functional_accommodation) — where exactly does the disagreement live: in the text of Article I/II, in founding-era practice, or in two centuries of accreted precedent that each side selectively cites?',
    'This is the committer-structure question routed here per Rule 2: it is not resolvable within this single reading''s story and is recorded for cross-reading analysis alongside the sibling constraints.',
    'If the disagreement is located primarily in accreted precedent rather than original text, this reading''s claim to being the ''inherent'' original design is weaker than its name suggests, and its ε is better understood as measuring a drifted practice rather than a stable constitutional allocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locates the kernel-level disagreement among the three war-powers readings for cross-story analysis.').

omega_variable(
    appropriations_as_ratification_validity,
    'Does a post-hoc congressional appropriations vote for an already-underway military operation constitute genuine democratic ratification, or is it a coerced choice (fund troops already at risk, or be blamed for abandoning them) that only simulates authorization?',
    'Examine congressional voting patterns and floor statements on wartime appropriations for evidence of genuine deliberation versus statements explicitly citing political impossibility of withholding funds from deployed troops.',
    'If appropriations votes are shown to be structurally coerced rather than deliberative, the theater_ratio and suppression figures should be revised upward, and the tangled_rope classification''s coordination claim weakens further toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_as_ratification_validity, empirical, 'Whether appropriations votes for ongoing operations are genuine authorization or coerced ratification theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t12, war_powers_allocation__inherent_executive_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(war__tr_t12, observed).
narrative_ontology:measurement(war__tr_t24, war_powers_allocation__inherent_executive_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(war__tr_t24, observed).
narrative_ontology:measurement(war__tr_t36, war_powers_allocation__inherent_executive_reading, theater_ratio, 36, 0.44).
narrative_ontology:measurement_basis(war__tr_t36, observed).
narrative_ontology:measurement(war__tr_t48, war_powers_allocation__inherent_executive_reading, theater_ratio, 48, 0.49).
narrative_ontology:measurement_basis(war__tr_t48, observed).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__inherent_executive_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement_basis(war__tr_t60, observed).
narrative_ontology:measurement(war__tr_t70, war_powers_allocation__inherent_executive_reading, theater_ratio, 70, 0.55).
narrative_ontology:measurement_basis(war__tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t12, war_powers_allocation__inherent_executive_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(war__be_t12, observed).
narrative_ontology:measurement(war__be_t24, war_powers_allocation__inherent_executive_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(war__be_t24, observed).
narrative_ontology:measurement(war__be_t36, war_powers_allocation__inherent_executive_reading, base_extractiveness, 36, 0.58).
narrative_ontology:measurement_basis(war__be_t36, observed).
narrative_ontology:measurement(war__be_t48, war_powers_allocation__inherent_executive_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement_basis(war__be_t48, observed).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__inherent_executive_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(war__be_t60, observed).
narrative_ontology:measurement(war__be_t70, war_powers_allocation__inherent_executive_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement_basis(war__be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t12, war_powers_allocation__inherent_executive_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement_basis(war__su_t12, observed).
narrative_ontology:measurement(war__su_t24, war_powers_allocation__inherent_executive_reading, suppression_requirement, 24, 0.25).
narrative_ontology:measurement_basis(war__su_t24, observed).
narrative_ontology:measurement(war__su_t36, war_powers_allocation__inherent_executive_reading, suppression_requirement, 36, 0.27).
narrative_ontology:measurement_basis(war__su_t36, observed).
narrative_ontology:measurement(war__su_t48, war_powers_allocation__inherent_executive_reading, suppression_requirement, 48, 0.29).
narrative_ontology:measurement_basis(war__su_t48, observed).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__inherent_executive_reading, suppression_requirement, 60, 0.31).
narrative_ontology:measurement_basis(war__su_t60, observed).
narrative_ontology:measurement(war__su_t70, war_powers_allocation__inherent_executive_reading, suppression_requirement, 70, 0.32).
narrative_ontology:measurement_basis(war__su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__inherent_executive_reading, 0.1).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_resolution_enforcement).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, appropriations_ratification_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the war_powers_allocation kernel. congressional_primacy_reading authors a low-ε, high-suppression-of-executive-discretion constraint where unauthorized deployment is itself the extraction. functional_accommodation_reading authors a moderate, context-conditional ε. This story (inherent_executive_reading) authors the highest ε of the three because it treats congressional authorization as optional courtesy, placing the greatest share of unchecked discretion — and therefore the greatest accumulated extraction from Congress, service members, and foreign populations — inside the executive seat. All three must remain linked via affects_constraints per the ε-invariance decomposition rule; none averages or hedges against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
