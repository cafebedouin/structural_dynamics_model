% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: War Powers Allocation â Congressional Primacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The war powers allocation kernel concerns which branch of the United
 *   States government holds constitutional authority to initiate military
 *   hostilities beyond immediate national defense. This constraint story
 *   instantiates the congressional primacy reading: the claim that Article I,
 *   Section 8 requires explicit congressional authorization for such force,
 *   and that the post-1945 standing arrangementâwherein presidents
 *   routinely bypass Congress via AUMF stretching, signing statements, and
 *   Article II assertionsâoperates as an asymmetrically extractive
 *   constraint on legislative war powers. The executive branch is the
 *   structural beneficiary of this bypass; Congress is the victim. The
 *   reading sees the current arrangement as a tangled rope: it coordinates
 *   national security speed through unified executive command, but
 *   simultaneously extracts constitutional authority from the legislative
 *   branch. This story authors high extraction and suppression metrics
 *   independently of the claimed type; the engine computes any divergence.
 *
 * KEY AGENTS:
 *   - executive_branch (institutional/arbitrage): Primary beneficiary and agenda-setter; defends and expands unilateral war-making authority through legal and institutional innovation.
 *   - legislative_branch (institutional/constrained): Primary payer; constitutionally holds war powers but is structurally bypassed and lacks effective enforcement mechanisms.
 *   - federal_judiciary (institutional/analytical): Observer seat; avoids direct adjudication of war powers disputes, serving as an analytical rather than enforcing agent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.82).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.75).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "War Powers Allocation â Congressional Primacy Reading").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '036eceeb-742e-41a7-acf7-310b6cc8fb95').
narrative_ontology:cs_kernel_codification('036eceeb-742e-41a7-acf7-310b6cc8fb95', formalized).
narrative_ontology:cs_authority_grounding('036eceeb-742e-41a7-acf7-310b6cc8fb95', lineage).
narrative_ontology:cs_interpretation_layer_present('036eceeb-742e-41a7-acf7-310b6cc8fb95').
narrative_ontology:cs_reading_relation('036eceeb-742e-41a7-acf7-310b6cc8fb95', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('036eceeb-742e-41a7-acf7-310b6cc8fb95', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('036eceeb-742e-41a7-acf7-310b6cc8fb95', foundational, non_defensive_force_requires_legislative_authorization).
narrative_ontology:cs_axiom_status(non_defensive_force_requires_legislative_authorization, holdable).
narrative_ontology:cs_axiom_grounding('036eceeb-742e-41a7-acf7-310b6cc8fb95', non_defensive_force_requires_legislative_authorization, conventional).
narrative_ontology:cs_axiom('036eceeb-742e-41a7-acf7-310b6cc8fb95', foundational, commander_in_chief_power_is_subordinate_to_congressional_war_declaration).
narrative_ontology:cs_axiom_status(commander_in_chief_power_is_subordinate_to_congressional_war_declaration, holdable).
narrative_ontology:cs_axiom_grounding('036eceeb-742e-41a7-acf7-310b6cc8fb95', commander_in_chief_power_is_subordinate_to_congressional_war_declaration, conventional).
narrative_ontology:cs_reference_frame('036eceeb-742e-41a7-acf7-310b6cc8fb95', congressional_declaration_supremacy).
narrative_ontology:cs_drift_state('036eceeb-742e-41a7-acf7-310b6cc8fb95', post_1945_security_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('036eceeb-742e-41a7-acf7-310b6cc8fb95', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, legislative_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and defends unilateral military deployment authority through Office of Legal Counsel opinions, signing statements, and operational precedent; collects expanded institutional autonomy and operational flexibility; can reshape the legal and interpretive environment to sustain bypass of congressional authorization.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, executive_branch, beneficiary).

% Constitutionally holds the power to declare war and authorize force but is routinely bypassed via AUMF elasticity, emergency declarations, and executive Article II claims; attempts to reassert authority through the War Powers Resolution, funding restrictions, and hearings are regularly circumvented or politically overridden; lacks effective institutional mechanisms to compel pre-deployment authorization.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, legislative_branch, payer,
    institutional, generational, constrained, national).

% Avoids adjudicating war powers disputes under the political question doctrine; occasionally hears standing challenges but rarely restrains executive military action; occupies an analytical seat that observes the inter-branch divergence without resolving it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid, unified national security decision-making and operational coherence without the delays of legislative deliberation, coordinating the state's external violence capacity through a single executive command structure.
% TRANSFER_FUNCTION: Moves war powers authority, institutional legitimacy, and democratic accountability from the legislative branch to the executive branch; transfers operational autonomy to the executive while the legislative branch bears the formal costs of authorization without corresponding control.
% ABSENT_VOICES: Strict constructionist legislators demanding formal declarations of war; anti-interventionist constituencies excluded by AUMF elasticity and closed-door consultations; state-level militia voices displaced by federal standing military doctrine.
% DISAPPEARANCE_RATIONALE: Strict enforcement of congressional authorization requirements would force advance legislative deliberation, likely reducing discretionary military interventions; the national security legal and operational apparatus would reorganize around defensive postures, treaty pre-authorization, or explicit statutory mandates.
% FOUNDING_PROBLEM: How to reconcile the need for rapid, decisive national security action with the constitutional assignment of war powers to Congress and the principle of democratic deliberation before sustained hostilities.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and founding-era textualists attest to the priority of legislative authorization. The executive branch and national security legal community argue the post-1945 threat environment has functionally superseded the founding problem. Corroboration from outside the executive-benefiting parties exists in academic legal history and legislative history, though operational military perspectives dispute its current viability.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the standing arrangement systematically transfers war powers authority from Congress to the Executive without equivalent return. Suppression is high (0.75) because the executive's institutional and legal machinery (OLC opinions, AUMF elasticity, political question doctrine) actively suppresses effective congressional reassertion and alternative oversight mechanisms. Theater is moderate (0.45): congressional AUMF votes and War Powers notifications are partly performative, authorizing or rubber-stamping executive-initiated operations rather than exercising independent deliberative judgment. Accessibility collapse (0.65) reflects the disappearance of formal declarations of war and strict WPR compliance from viable institutional memory. Resistance (0.45) captures intermittent congressional pushback (Yemen, Libya) that is usually overridden. The measurement series track a monotonic increase in extraction and theater from the post-WWII period through the post-9/11 era, reflecting institutional drift.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch experiences the constraint as necessary coordination for national security and a legitimate inheritance of operational autonomy. The legislative branch experiences the same arrangement as constitutional bypass and institutional disenfranchisement. The federal judiciary occupies an analytical seat that observes the divergence without resolving it. The engine computes this gap from the structural data: same spatial scope and nominal power, but divergent directionality due to beneficiary-payer asymmetry and differentiated exit options (arbitrage vs constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   The executive_branch is declared as both agenda_setter and beneficiary with arbitrage-grade exit (can reshape the legal and institutional environment to sustain its position), placing its derived directionality near the full-beneficiary end. The legislative_branch is declared as payer with constrained exit (cannot easily abandon the constitutional framework or the national security state), placing its directionality near the full-target end. The federal_judiciary is an observer with analytical exit, sitting near symmetric. No override is required: the structural derivation captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâbalancing security speed with democratic deliberationâremains live in the abstract but contested in its current instantiation. The arrangement risks mislabeling as pure coordination (Rope) because national security genuinely requires rapid decision-making; the victim declaration and high extraction metric prevent this by surfacing the asymmetric authority transfer. It risks mislabeling as pure extraction (Snare) if the coordination function (unified command, operational coherence) is ignored; the declared coordination type and moderate theater ratio preserve the hybrid classification. The temporal measurements show extraction accumulation over decades, supporting the Tangled Rope diagnosis rather than a static coordination equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_target_ambiguity,
    'Does the constraint structurally suppress executive claims of inherent constitutional authority, or does the executive bypass suppress congressional war power claims?',
    'Comparative analysis of successful legal challenges: if courts routinely reject inherent authority claims, the constraint suppresses executive overreach; if courts reject congressional standing or WPR challenges, the arrangement suppresses legislative claims.',
    'If suppression targets congressional claims, the directionality and effective extraction for the legislative branch are higher than structurally derived; if it targets executive claims, the executive is the primary target and the classification shifts toward a failed or coordination constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_target_ambiguity, conceptual, 'Ambiguity in whether suppression targets executive or legislative authority claims').

omega_variable(
    authorization_boundary,
    'What operational threshold distinguishes ''immediate defense'' from force requiring authorization, and who defines it?',
    'Systematic coding of executive justifications for military actions since 1945 against actual operational profiles to identify where the boundary is drawn in practice versus where the congressional primacy reading would draw it.',
    'If the executive defines the boundary unilaterally, the extraction is higher because the exception swallows the rule; if an independent arbiter defines it, the constraint functions closer to its claimed coordination-extraction balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_boundary, empirical, 'Uncertainty about the boundary between immediate defense and authorized force').

omega_variable(
    institutional_enforcement_asymmetry,
    'Why does the legislative branch fail to enforce the authorization requirement despite holding constitutional text, the power of the purse, and oversight authority?',
    'Comparative institutional analysis of successful congressional reassertions of war powers versus failed attempts, controlling for partisan alignment and threat perception.',
    'If enforcement failure is structural (collective action, information asymmetry, voter salience), the constraint is more extractive than if it is merely contingent on current political alignments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_asymmetry, empirical, 'Structural reasons for congressional enforcement failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__congressional_primacy_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__congressional_primacy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__congressional_primacy_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__congressional_primacy_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel decomposes into three structurally distinct constraints under different readings. This reading (congressional primacy) asserts high extraction from Congress and forecloses the inherent executive reading; the inherent executive reading asserts the converse extraction; the functional accommodation reading moderates both. Each carries distinct epsilon values, beneficiary structures, and directionalities. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
