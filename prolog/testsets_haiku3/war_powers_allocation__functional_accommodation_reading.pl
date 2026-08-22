% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation (Functional Accommodation Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The functional accommodation reading of war powers allocation treats the
 *   Constitution's silence on imminent-threat authority as intentional: the
 *   Framers implicitly endorsed a context-dependent split where immediate
 *   threats justify unilateral executive action and prolonged campaigns
 *   require congressional authorization. This reading claims to resolve the
 *   tension between legislative war power and executive commander-in-chief
 *   power through pragmatic operational categories rather than categorical
 *   rule. The constraint itself operates as a tangled rope: it coordinates
 *   genuine speed requirements in imminent scenarios while extracting
 *   congressional authority over prolonged campaigns through the ambiguity
 *   zone between 'imminent' and 'planned.' Both branches claim authority
 *   within the gray area; the suppression metric reflects the active
 *   enforcement of categorical boundaries to prevent either branch from
 *   claiming complete authority.
 *
 * KEY AGENTS:
 *   - executive_president: Sets initiation authority unilaterally in imminent scenarios, controls operational scope in authorized campaigns (institutional power, constrained exit)
 *   - congress_war_powers_committee: Retains formal war-declaration power but lost operational control over initiation (institutional power, identity_locked exit — constitutional duty prevents unilateral withdrawal)
 *   - military_command_structure: Benefits from clarity and speed; operates under broad authorization without mission-specific renewal (institutional power, constrained exit)
 *   - general_public_war_constituents: Bears costs without meaningful initiation input; Congress theoretically represents will but authorization is post-hoc (powerless, trapped exit)
 *   - constitutional_law_interpreters: Analytical seat observing and interpreting the functional accommodation framework itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.71).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation (Functional Accommodation Reading)").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '89d364f2-0e43-491c-835c-ea989277e471').
narrative_ontology:cs_kernel_codification('89d364f2-0e43-491c-835c-ea989277e471', fixed_text).
narrative_ontology:cs_authority_grounding('89d364f2-0e43-491c-835c-ea989277e471', lineage).
narrative_ontology:cs_interpretation_layer_present('89d364f2-0e43-491c-835c-ea989277e471').
narrative_ontology:cs_reading_relation('89d364f2-0e43-491c-835c-ea989277e471', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('89d364f2-0e43-491c-835c-ea989277e471', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('89d364f2-0e43-491c-835c-ea989277e471', foundational, context_dependent_allocation_constitutionally_valid).
narrative_ontology:cs_axiom_status(context_dependent_allocation_constitutionally_valid, holdable).
narrative_ontology:cs_axiom_grounding('89d364f2-0e43-491c-835c-ea989277e471', context_dependent_allocation_constitutionally_valid, deontological).
narrative_ontology:cs_axiom('89d364f2-0e43-491c-835c-ea989277e471', foundational, imminence_justifies_unilateral_executive_action).
narrative_ontology:cs_axiom_status(imminence_justifies_unilateral_executive_action, holdable).
narrative_ontology:cs_axiom_grounding('89d364f2-0e43-491c-835c-ea989277e471', imminence_justifies_unilateral_executive_action, instrumental).
narrative_ontology:cs_reference_frame('89d364f2-0e43-491c-835c-ea989277e471', constitutional_urgency_deliberation_balance).
narrative_ontology:cs_drift_state('89d364f2-0e43-491c-835c-ea989277e471', contemporary_post_authorization_erosion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('89d364f2-0e43-491c-835c-ea989277e471', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch_rapid_response_capability).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress_in_prolonged_campaigns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, military_command_structure).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, general_public_war_constituents).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress_war_powers_committee).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, general_public_war_constituents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises unilateral military authority in response to imminent threats without prior congressional authorization, justified by operational urgency and commander-in-chief powers. In prolonged campaigns, formally requires authorization but retains de facto control over interpretation of authorization scope. Can expand authorized missions through operational discretion without returning to Congress.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_president, agenda_setter,
    institutional, biographical, constrained, national).

% Retains formal war-declaration power but lost operational control over initiation decisions. In imminent-threat scenarios, Congress is bypassed entirely; in prolonged campaigns, Congress can authorize but cannot effectively revoke or narrow authorization once operations begin. Congressional war powers have become largely post-hoc ratification rather than prior authorization.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress_war_powers_committee, payer,
    institutional, biographical, identity_locked, national).

% Gains operational clarity and speed from unilateral executive initiation authority in immediate threats; benefits from the ambiguity zone where prolonged campaigns operate under broad delegated authorization without requirement for mission-specific renewal. Can initiate operations quickly, adjust strategy without legislative bottleneck.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_command_structure, beneficiary,
    institutional, generational, constrained, global).

% Bears the costs of war (casualties, economic disruption, opportunity costs) without meaningful input into initiation decisions in imminent scenarios. In prolonged campaigns, Congress theoretically represents public will through authorization, but authorization is often vague and broad, limiting actual public control. Public benefits from rapid response capability in genuine imminent threats but pays for expansions of authorization scope driven by executive interpretation.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, general_public_war_constituents, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, general_public_war_constituents, beneficiary).

% Faces U.S. military response determined by this allocation rule; cannot participate in the U.S. constitutional process that governs initiation authority. The adversary's interests in warning or negotiation are structurally excluded from the functional-accommodation framework.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, foreign_adversarial_state, excluded,
    institutional, biographical, trapped, global).

% Courts, scholars, and legal commentators analyze and interpret the line between imminent-threat unilateral action and prolonged-campaign authorization requirements. The functional accommodation reading itself is an interpretive position that privileges context and operational necessity over bright-line rules.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, constitutional_law_interpreters, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, executive_president).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates military initiation authority between branches based on operational context: immediate threats are solved by executive speed; prolonged campaigns are solved by distributed decision-making (executive + congressional authorization) to prevent unilateral unlimited war.
% TRANSFER_FUNCTION: Moves operational control from Congress (formal authority over war declaration) to the Executive (actual authority over initiation decisions), with the scope of transfer varying by whether the threat is imminent. In imminent scenarios, transfer is near-total; in prolonged scenarios, transfer is conditional and formally reviewable but practically irreversible once authorized.
% ABSENT_VOICES: Adversarial states are structurally excluded — they cannot participate in the U.S. constitutional process governing initiation decisions. State legislatures and local constituencies affected by military deployment are also absent from the direct authorization conversation. International law perspectives on the legitimacy of unilateral action are external to this domestic constitutional reading.
% DISAPPEARANCE_RATIONALE: If the functional-accommodation rule disappeared, the ambiguity zone would close: either Congress would require explicit authorization for all military action beyond immediate self-defense (congressional_primacy model), or the President would claim inherent authority for all deployments (inherent_executive model). Military response times to imminent threats would lengthen; the political economy of prolonged campaigns would shift from executive de facto control to genuine congressional partnership or opposition.
% FOUNDING_PROBLEM: The Constitution allocates war powers between branches but is silent on the boundary between imminent threats (where speed matters) and planned campaigns (where deliberation is possible). Early practice developed a pragmatic split: imminent threats received executive response; longer operations required legislative authorization. This reading codifies that split as constitutional accommodation rather than resolving it in either direction.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch officials and military strategists attest that imminent-threat response requires executive speed and that the founding problem remains live. Congressional committees and constitutional scholars attest that the distinction has been eroded — that 'imminent threat' has expanded operationally and authorization has become post-hoc ratification. Historical analysis from outside the Executive (e.g., War Powers Resolution legislative history, academic constitutional law) supports the contested reading.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 reflects the constraint's dual structure: genuine coordination benefit (imminent-threat speed) coupled with asymmetric extraction (executive de facto control over campaign scope). The measurement series shows extractiveness rising from 0.48 to 0.62 over the first 40 time units (expansion of what counts as 'imminent threat' and operational scope creep under authorization), then declining slightly to 0.58 at t=50 (reflecting post-war contraction and renewed congressional assertion of oversight). Suppression is high (0.71) because the constraint's persistence depends on active boundary-maintenance: courts decline to adjudicate imminence, Congress declines to revoke broad authorizations, and executive officials actively argue for expansive interpretations of operational discretion. Theater ratio remains moderate (0.35–0.44) because while some executive framing is rhetorical, imminent-threat authority genuinely does have operational validity — the theater is not performative maintenance of a dead function, but rather rhetorical expansion of what qualifies as 'imminent.' Accessibility collapse is moderate (0.48) because the alternative (bright-line congressional control or inherent executive authority) remains intellectually coherent and is actively advocated; the functional accommodation reading is contested, not inevitable. Resistance is high (0.73) because Congress and constitutional scholars actively contest the reading, adversarial states resist its application, and public constituencies express skepticism about authorization breadth.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (Congress) and the beneficiary seat (Executive) experience radically different constraint types from the same structural arrangement. Congress experiences the functional accommodation reading as a rhetorical expansion of executive authority; the Executive experiences it as principled constitutional interpretation. The Military and courts experience it as operational necessity balancing. The engine should compute Congress's seat as approaching snare in imminent-threat frequency, tangled_rope in prolonged campaigns, and piton during periods of strategic inertia. The Executive's seat should compute as rope (coordination + controlled transfer). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The Executive's directionality is low (~0.2–0.3): it benefits from the ambiguity zone and collects operational authority. Congress's directionality is high (~0.7–0.8): it bears the cost of lost control over initiation and campaign scope despite retaining formal authority. The Military benefits from clarity and speed (directionality ~0.15–0.25). The public's directionality varies by scenario: in imminent threats (~0.6–0.7, near target) because they cannot influence initiation; in prolonged campaigns (~0.45–0.55, near symmetric) because Congress theoretically represents them. Foreign adversaries are structurally excluded (they do not appear on the directionality axis — exclusion is not a directionality value but a role distinction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (urgency vs. deliberation) is live but the functional accommodation reading may have outlived its functional purpose: if modern operational speed means the Executive can initiate within hours and seek retroactive authorization within days, the temporal distinction collapses. The authorization becomes nominally concurrent rather than sequential. However, the constraint persists because it serves extractive institutional interests (Executive control, military budget certainty) that would not disappear even if the founding problem were solved. The theater ratio's stability around 0.40–0.44 suggests the constraint is not yet performative maintenance — it still does real coordination work in imminent scenarios — but the measurement trajectory (rising extractiveness despite stable theater) is a mandatrophy warning signal. If extractiveness continues rising while theater ratio and operational imminence do not, the constraint is transitioning from coordination to rent-extraction, and mandatrophy would apply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_definition_drift,
    'What constitutes an ''imminent threat'' under the functional accommodation reading, and has the operational definition expanded beyond the Framers'' likely original understanding?',
    'Historical textual analysis of imminent-threat uses in Framers'' era; contemporary legal doctrine requiring courts to adjudicate specific threat claims rather than deferring to executive declarations.',
    'If imminence has expanded operationally to cover near-future threats, cyber threats, and preemptive operations, the functional accommodation reading collapses into de facto inherent_executive_reading. If imminence remains genuinely temporally narrow (hours to days), the reading''s coordination function is intact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminence_definition_drift, empirical, 'Whether the operational definition of imminence has drifted from the reading''s theoretical grounding.').

omega_variable(
    authorization_revocability_ambiguity,
    'Does a congressional authorization for military force create a durable delegation that the Executive can interpret operationally without renewal, or must Congress be able to revoke and narrow authorization as operations evolve?',
    'Congressional passage of time-limited authorizations with explicit end dates; court adjudication of congressional rescission rights; legislative history of War Powers Resolution amendment attempts.',
    'If authorization is durable and operationally revisable only by the Executive, the constraint is closer to snare (asymmetric extraction); if Congress retains active revocation power, it remains tangled_rope (coordination with transfer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_revocability_ambiguity, conceptual, 'Whether the coordination function (authorization) includes meaningful congressional control over its scope and duration.').

omega_variable(
    reading_coexistence_precarity,
    'Can the functional accommodation reading coexist with inherent_executive_reading in the same institutional framework, or are they foreclosed by each other''s core premises?',
    'Test case: a president claims imminent-threat authority for an operation that Congress disputes as non-imminent. Does the court''s verdict favor one reading''s premises over the other, or does the court defer?',
    'If courts foreclose inherent_executive_reading by requiring imminent-threat proof, the functional accommodation reading becomes the binding framework. If courts defer to executive imminence declarations, inherent_executive_reading effectively controls and functional accommodation becomes cover. If courts genuinely enforce the boundary, both readings coexist through institutional role-specialization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_precarity, conceptual, 'Whether the functional accommodation reading is a stable independent framework or a transitional step toward inherent_executive authority.').

omega_variable(
    suppression_mechanism_structural_vs_rhetorical,
    'Is the measured suppression (0.71) enforced by structural barriers (courts declining jurisdiction, Congress lacking enforcement mechanism) or by rhetorical claims (Executive assertions of authority, scholarly defense of the reading)?',
    'Observation of congressional behavior in practice: do members voting for authorization express genuine acceptance of the reading, or do they express forced acceptance due to political constraints or lack of alternatives?',
    'If suppression is structural (institutional-role-based), the constraint persists independent of public acceptance. If suppression is rhetorical (dependent on continued scholarly and executive defense), a shift in elite consensus would collapse the constraint rapidly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_rhetorical, empirical, 'Whether suppression persists through institutional design or through rhetorical maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t8, war_powers_allocation__functional_accommodation_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(war__tr_t8, observed).
narrative_ontology:measurement(war__tr_t16, war_powers_allocation__functional_accommodation_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(war__tr_t16, observed).
narrative_ontology:measurement(war__tr_t24, war_powers_allocation__functional_accommodation_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(war__tr_t24, observed).
narrative_ontology:measurement(war__tr_t32, war_powers_allocation__functional_accommodation_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement_basis(war__tr_t32, observed).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(war__tr_t40, observed).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__functional_accommodation_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(war__tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t8, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(war__be_t8, observed).
narrative_ontology:measurement(war__be_t16, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(war__be_t16, observed).
narrative_ontology:measurement(war__be_t24, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(war__be_t24, observed).
narrative_ontology:measurement(war__be_t32, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement_basis(war__be_t32, observed).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(war__be_t40, observed).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(war__be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t8, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(war__su_t8, observed).
narrative_ontology:measurement(war__su_t16, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(war__su_t16, observed).
narrative_ontology:measurement(war__su_t24, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(war__su_t24, observed).
narrative_ontology:measurement(war__su_t32, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement_basis(war__su_t32, observed).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement_basis(war__su_t40, observed).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(war__su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.18).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel decomposes into three constraints, one per sibling reading. This file instantiates functional_accommodation_reading. The congressional_primacy_reading claims that all military action requires explicit prior authorization (high extraction suppression, snare reading from congressional seat, rope reading from executive seat). The inherent_executive_reading claims the President has inherent authority for all deployments in national defense (low extraction, mountain-like reading from executive seat, snare reading from congressional seat). The functional accommodation reading claimed here occupies the contested middle ground: imminent threats justify unilateral action, prolonged campaigns require authorization. All three readings compete in contemporary constitutional discourse; they are linked via this network declaration because they share the same kernel and structural dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
