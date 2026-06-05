% ============================================================================
% CONSTRAINT STORY: civil_rights_era_amendments__twenty_sixth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civil_rights_era_amendments__twenty_sixth_amendment, []).

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
 *   constraint_id: civil_rights_era_amendments__twenty_sixth_amendment
 *   human_readable: Twenty-Sixth Amendment: Conscription Without Franchise
 *   domain: political/constitutional_rights
 *
 * SUMMARY:
 *   The Twenty-Sixth Amendment (1971) lowered the voting age from 21 to 18 in
 *   record time — ratified in just over three months — on the principle that
 *   conscription without electoral voice is a form of extraction. This
 *   constraint describes a specific reading of the civil-rights-era
 *   constitutional amendment kernel: one that grounds franchise expansion in
 *   the obligation of civic participation. The amendment suppressed age-based
 *   disfranchisement by establishing eighteen as the federal voting age
 *   boundary, benefiting young voters and constraining state authority to set
 *   higher thresholds. The historical context is Vietnam War conscription,
 *   where 18-year-old draftees had no electoral voice in decisions about war,
 *   military service, or the continuation of conscription itself. The
 *   constraint exhibits classic tangled-rope characteristics: beneficiaries
 *   (young voters) gain franchise participation (coordination benefit), but
 *   the mechanism simultaneously extracts from those (states,
 *   age-restrictionist coalitions) who previously controlled the franchise
 *   boundary. The amendment represents both a genuine expansion of democratic
 *   participation and an institutional recalibration of power over electoral
 *   rules.
 *
 * KEY AGENTS:
 *   - Conscripted Youth (18-20 age cohort): Primary victims pre-amendment (powerless/trapped) — bear full cost of military service with zero electoral voice
 *   - Young Voters (Post-amendment): Primary beneficiaries (moderate/mobile) — gain electoral participation and voice in future conscription decisions
 *   - Age-Restrictionist State Legislatures: Secondary victims (institutional/constrained) — lose monopoly power over franchise boundaries; forced to align with federal standard
 *   - Federal Government / Pro-Amendment Coalition: Secondary beneficiary (institutional/arbitrage) — gains electoral legitimacy, expands electoral base, resolves constitutional contradiction
 *   - Youth Voting Rights Movement: Organized intermediary (organized/constrained) — mobilized the moral clarity of the 'old enough to fight' principle; provided political pressure for ratification
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks treating eighteen as a natural or inevitable maturity boundary rather than a negotiated institutional settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civil_rights_era_amendments__twenty_sixth_amendment, 0.35).
domain_priors:suppression_score(civil_rights_era_amendments__twenty_sixth_amendment, 0.62).
domain_priors:theater_ratio(civil_rights_era_amendments__twenty_sixth_amendment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_sixth_amendment, extractiveness, 0.35).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_sixth_amendment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_sixth_amendment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civil_rights_era_amendments__twenty_sixth_amendment, tangled_rope).
narrative_ontology:human_readable(civil_rights_era_amendments__twenty_sixth_amendment, "Twenty-Sixth Amendment: Conscription Without Franchise").
narrative_ontology:topic_domain(civil_rights_era_amendments__twenty_sixth_amendment, "political/constitutional_rights").

domain_priors:requires_active_enforcement(civil_rights_era_amendments__twenty_sixth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(civil_rights_era_amendments__twenty_sixth_amendment, '7d56f0b9-4fb5-4092-8856-36fa7795d77b').
narrative_ontology:cs_kernel_codification('7d56f0b9-4fb5-4092-8856-36fa7795d77b', formalized).
narrative_ontology:cs_authority_grounding('7d56f0b9-4fb5-4092-8856-36fa7795d77b', lineage).
narrative_ontology:cs_interpretation_layer_present('7d56f0b9-4fb5-4092-8856-36fa7795d77b').
narrative_ontology:cs_reading_relation('7d56f0b9-4fb5-4092-8856-36fa7795d77b', civil_rights_era_amendments__twenty_fifth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('7d56f0b9-4fb5-4092-8856-36fa7795d77b', civil_rights_era_amendments__twenty_fourth_amendment, influences).
narrative_ontology:cs_reading_relation('7d56f0b9-4fb5-4092-8856-36fa7795d77b', civil_rights_era_amendments__twenty_third_amendment, coexists_with).
narrative_ontology:cs_axiom('7d56f0b9-4fb5-4092-8856-36fa7795d77b', foundational, conscription_obligates_electoral_voice).
narrative_ontology:cs_axiom_status(conscription_obligates_electoral_voice, holdable).
narrative_ontology:cs_axiom_grounding('7d56f0b9-4fb5-4092-8856-36fa7795d77b', conscription_obligates_electoral_voice, deontological).
narrative_ontology:cs_axiom('7d56f0b9-4fb5-4092-8856-36fa7795d77b', secondary, civic_obligation_entails_franchise_capacity).
narrative_ontology:cs_axiom_status(civic_obligation_entails_franchise_capacity, holdable).
narrative_ontology:cs_axiom_grounding('7d56f0b9-4fb5-4092-8856-36fa7795d77b', civic_obligation_entails_franchise_capacity, instrumental).
narrative_ontology:cs_reference_frame('7d56f0b9-4fb5-4092-8856-36fa7795d77b', age_based_electoral_exclusion_framework).
narrative_ontology:cs_drift_state('7d56f0b9-4fb5-4092-8856-36fa7795d77b', post_amendment_ratification, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('7d56f0b9-4fb5-4092-8856-36fa7795d77b', '').
narrative_ontology:cs_kernel_id(civil_rights_era_amendments__twenty_sixth_amendment, civil_rights_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civil_rights_era_amendments__twenty_sixth_amendment, young_voters_aged_18_to_20).
narrative_ontology:constraint_victim(civil_rights_era_amendments__twenty_sixth_amendment, age_restrictionist_state_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED BUT DISENFRANCHISED YOUTH (SNARE) — Pre-amendment: bearing the full cost of military conscription (death, injury, lost years) with zero electoral voice in the decisions that sent them to war. No exit from conscription, no exit from disfranchisement. Maximum experienced extraction — the raw injustice that motivated amendment ratification. The constraint is pure extraction with suppression via law.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AGE-RESTRICTIONIST STATE LEGISLATURES (TANGLED ROPE) — States resisting the amendment had a genuine coordination function: administering voter registration and election procedures. But they also extracted status and power by controlling the franchise boundary. The constraint is hybrid: coordination (election administration) plus asymmetric extraction (monopoly power over who votes). States faced constrained exit — federal amendment can override state preference, but the ratification process itself provided some voice.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT / PRO-AMENDMENT COALITION (ROPE) — Institutional actor benefiting from amendment ratification (increased electoral base, legitimacy boost from addressing the conscription-without-franchise contradiction). Experiences the constraint as pure coordination: establishing common federal voting age eliminates inter-state arbitrage and clarifies electoral machinery. Arbitrage exit available — federal institutions can align incentives across states via constitutional amendment.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: YOUTH VOTING RIGHTS MOVEMENT (SCAFFOLD) — Organized actors (student movements, civil rights coalitions, anti-war organizers) saw the amendment as a temporary solution addressing the specific injustice of conscription without franchise. The movement had a sunset implicit in its framing: once voting age is lowered, the core contradiction (conscripted without voice) is resolved. Theater is relatively low because the argument ('old enough to fight, old enough to vote') was direct and morally clear — minimal performative rhetorical padding needed.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: YOUNG VOTERS POST-AMENDMENT (ROPE) — After ratification, newly enfranchised 18-20 year-olds experience the constraint as coordination: voting is a mechanism for participating in collective decisions. No exploitation visible from this perspective — they are part of the coordinating body now. Mobile exit available (emigration, abstention), but franchise itself appears cooperative.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL MATURITY VIEW (MOUNTAIN) — From a civilizational perspective, age-eighteen is a 'natural' boundary for political maturity, making universal adult suffrage (age 18+) appear as an inevitable or transcendent solution rather than a contingent political choice. This perspective risks naturalizing what is actually an institutional boundary negotiation. However, the structural data reveals this as a false summit: the boundary was contested, state-dependent, and extracted benefits for those who controlled franchise access.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civil_rights_era_amendments__twenty_sixth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_sixth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(civil_rights_era_amendments__twenty_sixth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Pre-amendment extractiveness (0.68) reflects the full asymmetry of conscription without franchise — young people bore costs while having zero voice. Post-amendment extractiveness drops to 0.35 because the mechanism resolves the core contradiction: those conscripted now have electoral participation. However, extractiveness doesn't reach zero because the amendment doesn't eliminate conscription itself, only guarantees electoral voice about conscription. The residual value reflects that institutional inertia means new voters often don't exercise marginal influence proportional to their electoral power. Suppression (0.62): Moderate-high. Pre-amendment suppression was extreme (age-based law prohibited voting outright). Post-amendment suppression is moderate because the legal barrier is removed, but structural barriers remain (first-time voter mobilization costs, campaign targeting of older cohorts, generational political preferences). The value captures that suppression exists in registration friction and structural political disadvantage, not in explicit legal prohibition. Theater ratio (0.48): Moderate. Pre-amendment theater was lower (0.35) because the conscription-without-franchise argument was direct and morally transparent — little performative padding needed. Post-amendment theater increases (0.48) because the amendment becomes ritualized (Youth Vote campaigns, periodic voting-age debates at state level) and the original moral clarity fades into institutional routine.
 *
 * PERSPECTIVAL GAP:
 *   The amendment demonstrates a complete perspectival reversal through ratification. Pre-amendment: conscripted youth see snare (pure extraction), state legislatures see rope (franchise administration is coordination), analytical observer sees mountain (eighteen-year-old disfranchisement appears 'natural' to tradition). Post-amendment: youth see rope (franchise is coordination), states see tangled rope (must administer new boundary while losing control), analytical observer sees false summit (the previous 'naturalness' revealed as contingent institutional power). The gap is maximal because the same constitutional structure (voting age boundary) shifts from appearing inevitable and natural to appearing as a negotiated settlement. This is the defining feature of a kernel reading: it reorganizes which observations appear as natural law and which appear as contestable institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The amendment's directionality structure shifts with ratification. Pre-amendment: conscripted youth face d ≈ 0.95 (full victim status) with no escape; age-restrictionist states face d ≈ 0.10 (full beneficiary status from franchise control). Post-amendment: young voters face d ≈ 0.45 (symmetric — costs and benefits both present); state legislatures face d ≈ 0.55 (modest victim status — lost privilege but not harmed). The federal pro-amendment coalition faces d ≈ 0.05 (full beneficiary). These shifts drive the perspectival classification changes: snare → tangled rope for the target group, extraction → rope for the beneficiary. The amendment itself is the mechanism that computes the new d values — ratification EN MASSE resets the directionality landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via amendment mechanism. Pre-amendment mandatrophy: Is the age-based franchise restriction an immutable natural law (mountain) or extractive institutional choice (snare)? The amendment's rapid ratification (72 days) settles the question empirically — the restriction was contingent and removable, proving it was not a mountain. Post-amendment mandatrophy: Is the new eighteen-year boundary a natural or contingent choice? This remains formally open (omega variable: age_boundary_contingency), but the empirical evidence that other democracies use different boundaries (Austria 16, Scotland 16 for some elections) proves contingency. The amendment's own logic (tying franchise to conscription obligation) suggests that the boundary is instrumental, not natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    age_boundary_contingency,
    'Is eighteen the uniquely correct franchise age, or is it an institutional settlement among multiple defensible boundaries?',
    'Comparative analysis across democracies and time periods; evidence of changing norms and empirical literacy/competence data; historical counterfactual analysis of what voting age advocates proposed',
    'If boundary is contingent: the amendment is a political choice, not a discovery of natural maturity. If boundary is quasi-natural: amendment is convergence on an inevitable standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(age_boundary_contingency, conceptual, 'Whether age eighteen is contingent or quasi-natural for franchise eligibility').

omega_variable(
    conscription_legitimacy_coupling,
    'Does the principle ''old enough to fight, old enough to vote'' establish that conscription requires electoral voice, or is it merely a rhetorical move in a franchise expansion argument?',
    'Examination of pre-amendment conscription debates; analysis of whether conscription was treated as requiring electoral consent before the amendment; post-amendment analysis of whether conscription resistance decreased among enfranchised youth',
    'If principle is structural: conscription''s legitimacy depends on electoral voice. If rhetorical: the principle was a one-time argument for franchise expansion but does not establish ongoing linkage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conscription_legitimacy_coupling, conceptual, 'Whether conscription legitimacy depends on electoral franchise').

omega_variable(
    state_authority_suppression_mechanism,
    'Did states suppress youth voting as deliberate extractive control, or as administrative convenience and traditional practice?',
    'Historical examination of state legislative intent; analysis of enforcement costs for age restrictions; evidence of explicit benefit-capture vs inherited institutional inertia',
    'If deliberate extraction: states were snares. If inherited practice: suppression was structural but not necessarily intentional, and the amendment corrects a piton-like degraded institution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_suppression_mechanism, empirical, 'Whether age-based disfranchisement was deliberate extraction or administrative inheritance').

omega_variable(
    kernel_reading_boundary,
    'Is this constraint (conscription without franchise) a distinct reading of the civil-rights-era constitutional amendment kernel, or is it merely describing a policy outcome of multiple amendments?',
    'Analysis of foundational axioms across the sibling readings (23rd, 24th, 25th); identification of what makes this reading''s core premise distinct from its siblings; examination of whether the amendment presupposes different authority structures or axioms than siblings',
    'If distinct reading: this constraint instantiates a particular committer position (linking franchise to civic duty/obligation). If output of multiple readings: the constraint is a compound effect rather than a unitary principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether Twenty-Sixth Amendment reading is a distinct kernel reading or compound effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civil_rights_era_amendments__twenty_sixth_amendment, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twenty_sixth_theater_pre_amendment, civil_rights_era_amendments__twenty_sixth_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(twenty_sixth_theater_immediate_post, civil_rights_era_amendments__twenty_sixth_amendment, theater_ratio, 1, 0.48).
narrative_ontology:measurement(twenty_sixth_theater_decade_out, civil_rights_era_amendments__twenty_sixth_amendment, theater_ratio, 5, 0.52).

% Extraction over time
narrative_ontology:measurement(twenty_sixth_extractiveness_pre_amendment, civil_rights_era_amendments__twenty_sixth_amendment, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(twenty_sixth_extractiveness_immediate_post, civil_rights_era_amendments__twenty_sixth_amendment, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(twenty_sixth_extractiveness_decade_out, civil_rights_era_amendments__twenty_sixth_amendment, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civil_rights_era_amendments__twenty_sixth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_sixth_amendment, civil_rights_era_amendments__twenty_fourth_amendment).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_sixth_amendment, civil_rights_era_amendments__twenty_third_amendment).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_sixth_amendment, civil_rights_era_amendments__twenty_fifth_amendment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the civil-rights-era constitutional amendment kernel. The other readings (Twenty-Third, Twenty-Fourth, Twenty-Fifth Amendments) are separate constraint stories with different extractiveness values and foundational axioms. All four are linked via the kernel relationship. The Twenty-Sixth reading focuses on the conscription-without-franchise asymmetry; others focus on geographic access, economic barriers, and executive machinery respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
