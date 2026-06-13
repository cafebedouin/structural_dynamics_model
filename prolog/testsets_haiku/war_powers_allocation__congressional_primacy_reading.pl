% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: War Powers Allocation: Congressional Primacy Reading
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The War Powers Clause of Article I vests in Congress the power 'To
 *   declare War.' Article II vests in the President the role of
 *   Commander-in-Chief. For over two centuries, these provisions have been
 *   read in fundamentally different ways. Under the congressional primacy
 *   reading—the reading instantiated in THIS constraint story—the
 *   Constitution requires explicit legislative authorization before the
 *   president deploys military force beyond immediate self-defense. This
 *   reading grounds itself in the Founders' deliberate decision to lodge war
 *   power in the legislature, treating war as a matter too grave for
 *   unilateral executive action. However, successive administrations have
 *   asserted inherent executive authority to deploy force without prior
 *   authorization, invoking emergency doctrines, commander-in-chief
 *   prerogatives, and de facto practice that Congress has repeatedly funded
 *   retroactively. The constraint is claimed by Congress and constitutional
 *   scholars who defend the congressional primacy reading; it is
 *   systematically suppressed by executive actors who assert a rival reading.
 *   The consequence is a tangle: the constraint exists as law and
 *   constitutional principle; it is structured to coordinate democratic
 *   deliberation on war; yet it extracts from Congress the power to enforce
 *   it against executive reinterpretation, and the executive extracts from
 *   Congress the decision-making authority the constraint nominally reserves.
 *   This is why it is tangled_rope: it coordinates a genuine principle (no
 *   unilateral war) but does so while extracting from the coordinate body
 *   (Congress) the effective power to ensure the coordination.
 *
 * KEY AGENTS:
 *   - Legislative Branch: Claims the constraint; cannot reliably enforce it against rival readings. Experiences extraction when bypassed; benefits theoretically from the constraint's legitimating principle. Dual-positioned: agenda-setter of the rule, victim of executive evasion.
 *   - Executive Branch: Contests the constraint through alternative readings; asserts inherent authority; benefits from the constraint's unenforceability by maintaining both rhetorical compliance and practical freedom of action.
 *   - Military Command: Identity-locked to civilian control; caught between the constraint and the chain of command. Forced to execute orders that may violate the constraint but cannot refuse without institutional defiance.
 *   - Courts: Observationally abstain from enforcing the constraint, treating war powers as a non-justiciable political question. Their abstention implicitly enables executive suppression.
 *   - Constitutional Scholars (Congressional Primacy Faction): Benefit from the constraint as an interpretive anchor; bear the cost of watching it atrophy.
 *   - Affected Foreign Populations and U.S. Military Personnel: Excluded or powerless in the constraint system; bear the costs of deployments made without the deliberation the constraint nominates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.72).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "War Powers Allocation: Congressional Primacy Reading").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'bd2089c8-2bfd-4d12-aa69-48a64ac6a50f').
narrative_ontology:cs_kernel_codification('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', fixed_text).
narrative_ontology:cs_authority_grounding('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', lineage).
narrative_ontology:cs_interpretation_layer_present('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f').
narrative_ontology:cs_reading_relation('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', foundational, legislative_authorization_necessary_for_prolonged_force).
narrative_ontology:cs_axiom_status(legislative_authorization_necessary_for_prolonged_force, holdable).
narrative_ontology:cs_axiom_grounding('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', legislative_authorization_necessary_for_prolonged_force, deontological).
narrative_ontology:cs_axiom('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', foundational, separation_of_powers_precludes_unilateral_war_making).
narrative_ontology:cs_axiom_status(separation_of_powers_precludes_unilateral_war_making, holdable).
narrative_ontology:cs_axiom_grounding('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', separation_of_powers_precludes_unilateral_war_making, deontological).
narrative_ontology:cs_reference_frame('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', constitutional_legislative_war_power_supremacy).
narrative_ontology:cs_drift_state('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', contemporary_post_cold_war_security_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bd2089c8-2bfd-4d12-aa69-48a64ac6a50f', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, legislative_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, legislative_branch).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).

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
 *   The extractiveness score (0.68 at interval end) reflects the measure to which executive unilateral action extracts congressional war power: the president can deploy force, face funding choices and political pressure post-hoc, and usually prevail because the cost of defunding deployed forces is politically prohibitive. The suppression score (0.72) is high because the constraint's enforcement is actively suppressed through executive reinterpretation, judicial abstention, and the normalization of emergency/inherent authority doctrines. Accessibility collapse (0.78) is substantial but not absolute: alternatives to military action exist (diplomacy, economic pressure, multilateral action through the UN) but become harder to access once deployment is already underway. Resistance (0.59) is moderate—Congress often resists and demands authorizations after the fact, but this resistance rarely prevents action. Theater ratio (0.41) reflects the constraint's partial performativity: a real authorization structure exists (Congress does sometimes formally authorize war), but a growing share of military operations proceed outside this structure, and the authorization machinery itself has become a post-hoc ratification of executive decisions already made. The measurement series spanning 1789–2026 tracks the progressive erosion of congressional enforcement capacity and the rise of suppression mechanisms (emergency doctrine, judicial abstention). The gridded coercion metrics show that suppression has intensified at the organizational level (executive institutions hardening claims of authority) and individual level (military personnel and affected civilians experiencing suppression), while structural alternatives have become less accessible.
 *
 * PERSPECTIVAL GAP:
 *   The congressional seat experiences the constraint as weak, unenforced, and extractive—Congress sets the legal framework but the executive determines the facts on the ground. The executive seat experiences the constraint as a rule it can follow (by invoking emergency or inherent authority) while preserving practical freedom of action—the constraint is real enough to provide rhetorical cover but permeable enough to allow action. The military seat experiences the constraint as an ambiguity that creates institutional stress and divided loyalty. The scholarly seat experiences the constraint as a constitutional principle being progressively hollowed out.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative branch directionality: Congress nominally sets the war powers rules but cannot enforce them against executive reinterpretation. Congress benefits from the constraint's rhetorical authority (it can invoke it to demand authorizations, restrict funds, assert prerogatives) but bears the extraction cost when the executive bypasses it. Congress is simultaneously beneficiary (theoretically) and victim (practically). Derived directionality is near symmetric with target tilt: Congress has institutional power but constrained exit—it cannot leave the constitutional system or dissolve the executive. Executive directionality: The executive benefits substantially from the constraint's unenforceability. It can claim compliance with constitutional principle while freely deploying force through reinterpretation. The executive's directionality is near beneficiary end: it has institutional power, arbitrage-grade exit (through alternative readings, emergency doctrines, practice normalization), and gains from the constraint's weakness. The override is not needed; the structural derivation already captures the asymmetry. Military command directionality: Military leadership is caught between the constraint (congressional authorization) and the command structure (presidential orders). Their exit is identity-locked—refusal to obey either violates their institutional identity. Their directionality is near target end: they bear the costs of the institutional ambiguity without being able to resolve it. Foreign populations and affected civilians: powerless and structurally excluded, their directionality would be at the absolute target end if they were positioned in the constraint system at all, but they are not formally seated. This reflects the constraint's design: it is a constraint on what the U.S. government does, not a constraint that includes those affected by the government's action.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint manifests strong mandatrophy signals. The founding problem (preventing unilateral executive war-making) remains live—the post-9/11 era shows expanded executive deployments through emergency doctrines, kinetic operations in multiple countries, drone strikes, cyber operations, and covert military actions, many without formal congressional authorization or post-hoc ratification. Yet the constraint (congressional authorization requirement) has become increasingly difficult to enforce. Congress often funds military operations after the fact rather than authorizing them before; courts decline to adjudicate war powers disputes; emergency and inherent authority doctrines have expanded to accommodate most executive action; and the theater_ratio suggests that a growing share of the constraint's activity is rhetorical (authorizations sought when politically convenient, ignored when inconvenient). The constraint has not been formally abandoned—it remains part of constitutional law—but its active enforcement capacity has atrophied. This is the classic mandatrophy pattern: the problem the constraint was designed to solve persists, but the constraint has become an inert principle, a check that does not check. The measurement series confirms this: from 1789–1945, extractiveness rose but remained under 0.60 because congressional deliberation was still generally sought (even if increasingly bypassed). From 1945–2026, extractiveness rose sharply and suppression intensified, reflecting the post-war shift to executive dominance in military deployments. The theater_ratio shows increasing performativity: the constraint's machinery is invoked more often but accomplishes less.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_vs_inherent_authority_boundary,
    'Is the constitutional boundary between emergency powers (acknowledged by Congress for immediate threats) and inherent executive authority (asserted without authorization) a stable doctrine or a shifting front controlled by executive reinterpretation?',
    'Mapping the historical record of executive claims: if each administration expands the definition of ''immediate threat'' or ''emergency deployment'' beyond the previous, the boundary is not stable but is being erased by incremental redefinition. If courts accept each new claim as falling within prior doctrine, the normalization is complete.',
    'If the boundary is stable, the constraint remains partly enforceable through judicial review of whether particular actions fall within emergency scope. If unstable and shifting, the constraint has been functionally dissolved—all action can be claimed as emergency or inherent authority, and the distinction from unilateral war powers becomes semantic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_vs_inherent_authority_boundary, empirical, 'Whether emergency power doctrines have systematically expanded to absorb most military deployments.').

omega_variable(
    congressional_capacity_vs_deliberation_requirement,
    'Does the constraint require congressional deliberation before force deployment, or does it only require that Congress exist and could theoretically deliberate if it chose to? Is inaction consent?',
    'Legislative history, statutory interpretation, and historical practice: (1) Did the Founders expect Congress to affirmatively authorize each war, or only to fund it after action was taken? (2) Have successive Congresses acquiesced to executive deployments through funding votes, transforming tacit funding into retroactive authorization? (3) Is the requirement for explicit authorization or for congressional participation in any form?',
    'If the constraint requires affirmative authorization before deployment, then de facto action followed by Congress funding it violates the constraint. If the constraint only requires congressional capacity to object, the constraint becomes nearly empty—Congress is always capable but often passive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_capacity_vs_deliberation_requirement, conceptual, 'Whether the constraint requires affirmative authorization or only the possibility of congressional action.').

omega_variable(
    justiciability_abstention_structural_role,
    'Is judicial abstention from war powers disputes (treating them as non-justiciable political questions) a neutral observational stance, or is it a form of implicit suppression that enables executive bypassing?',
    'Comparative constitutional law: how do other democracies with similar separation-of-powers principles handle justiciability of war powers? Do they enforce the constraint more effectively through judicial review, and what are the institutional consequences? Does U.S. case law show systematic deference to executive claims when courts do enter the domain?',
    'If judicial abstention is neutral, the constraint persists through political (congressional) remedies and constitutional interpretation outside courts. If abstention is suppressive, it is part of the enforcement machinery of the constraint''s violation—courts enable executive bypassing by refusing to check it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(justiciability_abstention_structural_role, empirical, 'Whether institutional doctrines of non-justiciability structure suppression of the war powers constraint.').

omega_variable(
    reading_foreclosure_from_rival_framings,
    'This reading (congressional primacy) instantiates one interpretation of the War Powers Clause. The inherent_executive_reading interprets the same clause to grant the president independent authority. Can both readings coexist in the same constitutional framework, or does accepting one logically rule out the other?',
    'Constitutional hermeneutics: if the clause text permits both readings with equal plausibility, they coexist. If the text, original intent, or doctrinal structure definitively supports one reading over another, the other is foreclosed. If the readings rest on irreconcilable premises (e.g., one requires separation of powers to be strict, the other flexible), one forecloses the other.',
    'If the readings coexist, both are live political/constitutional claims and the constraint operates as a zone of institutional contestation. If this reading forecloses the rival, the constraint becomes a binding constitutional fact, not a reading. If the rival forecloses this one, the constraint is constitutionally overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_from_rival_framings, conceptual, 'Whether the congressional primacy reading forecloses, coexists with, or is foreclosed by the inherent executive authority reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of the congressional primacy constraint structural (courts refuse to hear cases, political costs of challenging the president are high, institutional capacity to enforce is weak) or internalized (Congress members believe the president really does have inherent authority, or believe the constraint is too inconvenient to enforce)?',
    'Post-constraint enforcement trajectory: if suppression is purely structural and barriers were removed (courts became willing to hear cases, political coalition against executive action formed), would enforcement resume? If suppression is internalized, removal of structural barriers alone would not restore enforcement—Congress has accepted the executive reading or decided enforcement is too costly.',
    'If structural, the constraint could be revived through institutional reform (court reform, congressional assertion of power). If internalized, revival requires shifting congressional and public opinion about what the Constitution means, which is a much deeper change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of the constraint is structural or has been internalized into institutional practice.').

omega_variable(
    mandatrophy_status_war_powers,
    'Has the founding problem (unchecked executive war-making) been solved, or has the constraint that was designed to prevent it atrophied while the problem persists?',
    'Comparative analysis: (1) Count the number of military deployments since 1945 that lacked explicit congressional authorization or a plausible emergency justification accepted at the time of deployment. (2) Assess whether the founding problem (executive unilateralism in war) is alive or dead. (3) Measure whether Congress actively enforces the constraint or has accepted a subordinate role. (4) Evaluate whether the constraint''s atrophy serves any coordinating function or is purely inertial.',
    'If mandatrophy is resolved (founding problem dead, constraint serves no function), the constraint should be formally abandoned or reinterpreted. If mandatrophy is active (founding problem live, constraint weak), the constraint is extractive or zombie-like—it maintains rhetorical authority while enabling the very thing it was meant to prevent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_status_war_powers, empirical, 'Whether the constraint is zombified: founder problem persists while constraint enforcement has atrophied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1789, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1789, 0.08).
narrative_ontology:measurement(war__tr_t1898, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1898, 0.12).
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(war__tr_t1973, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1973, 0.32).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(war__tr_t2026, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(war__be_t1789, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(war__be_t1898, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1898, 0.28).
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1945, 0.52).
narrative_ontology:measurement(war__be_t1973, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1973, 0.62).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(war__be_t2026, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1789, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1789, 0.22).
narrative_ontology:measurement(war__su_t1898, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1898, 0.35).
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1945, 0.54).
narrative_ontology:measurement(war__su_t1973, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1973, 0.68).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(war__su_t2026, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__congressional_primacy_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel permits three structurally distinct constraints: congressional_primacy_reading (this story) asserts mandatory authorization; functional_accommodation_reading permits context-sensitive authority (emergency vs. prolonged); inherent_executive_reading asserts presidential authority as primary. Each reading instantiates different ε, different beneficiary/victim sets, and different classification paths. They are linked because all three are live readings of the same constitutional language, and accepting one reading partially constrains which others remain coherent. This story networks to both siblings in affects_constraints because the congressional primacy reading forecloses (or significantly pressures) the inherent_executive reading while coexisting with the functional_accommodation reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
