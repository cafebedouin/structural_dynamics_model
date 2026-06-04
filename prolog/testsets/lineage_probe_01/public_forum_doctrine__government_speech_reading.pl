% ============================================================================
% CONSTRAINT STORY: public_forum_doctrine__government_speech_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_forum_doctrine__government_speech_reading, []).

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
 *   constraint_id: public_forum_doctrine__government_speech_reading
 *   human_readable: Government Speech Exception to Public Forum Doctrine
 *   domain: constitutional_law/first_amendment
 *
 * SUMMARY:
 *   The government speech exception to the public forum doctrine creates a
 *   structural tension at the core of First Amendment jurisprudence. The
 *   doctrine's foundational principle — that government property used as a
 *   forum for public expression must be viewpoint-neutral — encounters its
 *   exception when the government itself is the speaker. License-plate
 *   designs, monuments, official seals, and government-program messaging are
 *   treated as government expression, not forums open to all viewpoints. This
 *   exception suppresses access claims by preventing the government's
 *   property-as-forum from being weaponized against the government's
 *   preferred message. The constraint is a tangled rope: it serves a genuine
 *   coordination function (government needs to communicate its own policies
 *   and values) while extracting suppression costs from access claimants and
 *   viewpoint minorities. The exception has expanded over the measurement
 *   interval (2010–2026), with courts increasingly willing to invoke it and
 *   apply it broadly to state-controlled channels.
 *
 * KEY AGENTS:
 *   - Government Institutional Actor: Primary beneficiary (institutional/arbitrage) — captures the power to control messaging on state property; can exclude disfavored viewpoints by claiming government speech
 *   - Access Claimants: Primary victims (powerless/trapped) — seek to use government property for minority views; excluded by speech exception; no exit from state monopoly
 *   - Viewpoint Minorities: Secondary victims (powerless/trapped) — bear suppression cost when government excludes disfavored messages via speech exception
 *   - Civil Rights Organizations: Organized victims (organized/constrained) — can litigate and mobilize political pressure but face high litigation costs and entrenched precedent
 *   - Lower Courts: Institutional actors (institutional/constrained) — bound by precedent; limited interpretive agency in applying or narrowing the exception
 *   - Supreme Court: Powerful institutional actor (powerful/mobile) — retains doctrinal authority to expand, narrow, or sunset the exception through new precedent
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a judicially constructed doctrine as an inherent structural limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_forum_doctrine__government_speech_reading, 0.52).
domain_priors:suppression_score(public_forum_doctrine__government_speech_reading, 0.65).
domain_priors:theater_ratio(public_forum_doctrine__government_speech_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_forum_doctrine__government_speech_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(public_forum_doctrine__government_speech_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(public_forum_doctrine__government_speech_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_forum_doctrine__government_speech_reading, tangled_rope).
narrative_ontology:human_readable(public_forum_doctrine__government_speech_reading, "Government Speech Exception to Public Forum Doctrine").
narrative_ontology:topic_domain(public_forum_doctrine__government_speech_reading, "constitutional_law/first_amendment").

domain_priors:requires_active_enforcement(public_forum_doctrine__government_speech_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_forum_doctrine__government_speech_reading, '9024627b-118a-4a93-8e65-f0aeb7777eb0').
narrative_ontology:cs_kernel_codification('9024627b-118a-4a93-8e65-f0aeb7777eb0', fixed_text).
narrative_ontology:cs_authority_grounding('9024627b-118a-4a93-8e65-f0aeb7777eb0', lineage).
narrative_ontology:cs_interpretation_layer_present('9024627b-118a-4a93-8e65-f0aeb7777eb0').
narrative_ontology:cs_reading_relation('9024627b-118a-4a93-8e65-f0aeb7777eb0', public_forum_doctrine__designated_forum_reading, influences).
narrative_ontology:cs_reading_relation('9024627b-118a-4a93-8e65-f0aeb7777eb0', public_forum_doctrine__traditional_forum_reading, influences).
narrative_ontology:cs_axiom('9024627b-118a-4a93-8e65-f0aeb7777eb0', foundational, government_messaging_prerogative).
narrative_ontology:cs_axiom_status(government_messaging_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('9024627b-118a-4a93-8e65-f0aeb7777eb0', government_messaging_prerogative, deontological).
narrative_ontology:cs_axiom('9024627b-118a-4a93-8e65-f0aeb7777eb0', secondary, property_curatorship_triggers_exception).
narrative_ontology:cs_axiom_status(property_curatorship_triggers_exception, holdable).
narrative_ontology:cs_axiom_grounding('9024627b-118a-4a93-8e65-f0aeb7777eb0', property_curatorship_triggers_exception, conventional).
narrative_ontology:cs_reference_frame('9024627b-118a-4a93-8e65-f0aeb7777eb0', neutral_forum_default).
narrative_ontology:cs_drift_state('9024627b-118a-4a93-8e65-f0aeb7777eb0', contemporary_expanded_exception_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9024627b-118a-4a93-8e65-f0aeb7777eb0', '').
narrative_ontology:cs_kernel_id(public_forum_doctrine__government_speech_reading, public_forum_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_forum_doctrine__government_speech_reading, government_institutional_messaging).
narrative_ontology:constraint_victim(public_forum_doctrine__government_speech_reading, access_claimants_to_state_channels).
narrative_ontology:constraint_victim(public_forum_doctrine__government_speech_reading, viewpoint_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCESS CLAIMANT WITH MINORITY VIEW (SNARE) — Seeks to use government property (parks, bulletin boards, license-plate registry) to express disfavored message. Government speech doctrine permits state to exclude them by claiming the channel is state expression, not forum. No exit: the property is monopoly-held by the state. Maximum extraction — suppression backed by constitutional doctrine itself.
constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized actors benefit from the forum doctrine's core coordinating function (viewpoint neutrality on government property generally) but face suppression when government claims speech exception. High costs to challenge via litigation; some ability to organize political pressure for policy change. Mixed extraction and coordination.
constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT INSTITUTIONAL ACTOR (ROPE) — Sees the speech exception as enabling coordination of state messaging: monuments express public values, license-plate designs communicate state identity, programs implement policy. The exception is a coordination mechanism — government needs to speak its own message. Net beneficiary with arbitrage options (can litigate to expand or contract exception).
constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CORPORATIONS (POWERFUL/MOBILE) — Powerful actors (nonprofits, business groups) can arbitrage between jurisdictions: if one state's license-plate registry applies the speech exception to exclude disfavored messages, they relocate operations or challenge via well-funded litigation. Constrained but mobile. Experience extraction but retain exit capacity.
constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LOWER COURTS (INSTITUTIONAL/CONSTRAINED) — Courts are bound by precedent but retain some interpretive agency. They experience the government speech exception as both coordinating (provides clear rule: state can speak) and extractive (constrains their ability to enforce forum neutrality). Suppression comes from appellate precedent; constrained exit.
constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SUPREME COURT DOCTRINE EVOLUTION (SCAFFOLD) — The Court retains doctrinal authority to narrow or expand the speech exception through new precedent. The current exception is a temporary equilibrium: if courts reinterpret 'government speech' more narrowly (e.g., requiring actual government authorship, not mere control), the exception sunsets. Chi moderately low because the Court has clear exit — they can overrule their own precedent.
constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a foundational perspective, any government monopoly on speech regarding state property is an inherent structural feature: the state cannot allow private parties to commandeer its own monuments and channels without surrendering the ability to communicate its own policies. This perspective treats the exception as a natural law of institutional property rights. However, the structural data contradicts this — beneficiaries and victims are identifiable, suppression is active, and the doctrine is judicially constructed, not logically immutable.
constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_forum_doctrine__government_speech_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_forum_doctrine__government_speech_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_forum_doctrine__government_speech_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_forum_doctrine__government_speech_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The government speech exception provides real benefits to the government (ability to communicate state values and policy without being forced to accept contrary messages on state property) and real costs to access claimants (exclusion from channels that would otherwise be open forums). The asymmetry is not as severe as a pure snare (ε ≈ 0.72) because the exception is bounded: it applies only where government can credibly claim authorship or curation, not to all government property. Courts have not applied it to traditional forums (parks, streets), where forum neutrality remains strong. Suppression (0.65): High. The exception suppresses access claims through constitutional doctrine itself — not merely through policy or resource barriers, but through First Amendment precedent that exempts the channel from scrutiny. Once government successfully invokes the exception, access claimants have no doctrinal remedy. However, suppression is not total: political organizing, litigation strategy changes, and public pressure can still create pressure for doctrinal shift. Theater ratio (0.48): Moderate-low. The exception involves less performative activity than many constraints — it is a straightforward doctrinal rule applied by courts without elaborate ritual. The slight decrease in theater over the interval (0.52 to 0.48) reflects courts applying the rule more directly as precedent solidifies.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a severe perspectival gap between government and access claimants. Government sees coordination (Rope) — the exception enables necessary state messaging. Access claimants see pure extraction (Snare) — they are excluded from channels via constitutional doctrine. The gap widens as the exception expands. Organized civil rights actors see a mixed constraint (Tangled Rope) — some benefit from the forum doctrine's core neutrality principle on other properties, but face suppression on speech-exception properties. The Supreme Court (Scaffold) retains the ability to alter doctrine, giving it lower-extraction experience. Lower courts (Tangled Rope) are constrained by precedent but have some interpretive flexibility. The analytical observer (Mountain) risks naturalizing a constructed doctrine as inherent institutional necessity — but the structural data (identified beneficiaries, measurable suppression, active judicial enforcement) contradicts the mountain classification, suggesting it is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   The government actor experiences low directionality (d ≈ 0.20): beneficiary with arbitrage options — can litigate, retains exit. Access claimants experience high directionality (d ≈ 0.90): victims trapped by state monopoly, no exit. This maps to the tangled rope classification at moderate power: the constraint coordinates (government speech function) while extracting (suppression of access). The suppression is active and enforced by courts, not merely passive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    government_speech_scope_underspecification,
    'What constitutes ''government speech''? Does the exception apply only to messages the government directly authors, or to any message on government property where government could claim curatorship?',
    'Doctrinal analysis of precedent boundaries (e.g., license-plate designs: are they government speech because the state controls the registry, or private speech permitted by the state?); empirical examination of court decisions disambiguating authorship vs. control',
    'If narrow (authorship required): speech exception covers monuments, official seals, legislative records — narrow scope, lower extractiveness. If broad (control sufficient): covers any property where state exercises editorial discretion — broad scope, higher extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_speech_scope_underspecification, conceptual, 'Scope ambiguity: government authorship vs. government control as trigger for speech exception').

omega_variable(
    property_monopoly_legitimacy,
    'Does the government''s monopoly on speech regarding state-owned property reflect a natural institutional limit, or an unjustified exclusion of non-preferred viewpoints from shared civic resources?',
    'Historical analysis of forum doctrine''s evolution; comparison with other democracies'' approaches to access on state property; empirical assessment of whether forum neutrality substantially impairs government''s ability to communicate policy',
    'If natural limit: government speech exception is justified and the constraint is a coordination mechanism (lower extraction classification). If unjustified exclusion: exception is extractive suppression of access (higher extraction classification, closer to snare from many perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_monopoly_legitimacy, preference, 'Whether property monopoly limitation is inherent or contingent').

omega_variable(
    doctrinal_precedent_direction,
    'Is the government speech exception expanding (courts increasingly willing to invoke it) or stabilizing (exception confined to core cases like monuments and official emblems)?',
    'Temporal analysis of case law from 2000–2026: frequency of speech exception invocation, breadth of property contexts to which courts apply it, success rates of government speech claims',
    'If expanding: suppression is increasing, extractiveness rising, constraint moving toward snare from access-claimant perspective. If stabilizing: exception becoming predictable, suppression stable, constraint remaining tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_precedent_direction, empirical, 'Trajectory of government speech exception expansion or containment').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Which of the three readings (government_speech, designated_forum, traditional_forum) does the doctrine''s current equilibrium actually instantiate? Are courts applying multiple readings simultaneously in different contexts?',
    'Doctrinal mapping of recent major cases (10+ years, broad property contexts) to reading typology; identification of property categories where each reading dominates',
    'If readings coexist: the constraint structure is that multiple frameworks are simultaneously operative, creating discontinuity in doctrine. If one reading dominates: constraint structure is cleaner but may hide suppressed alternative readings. Affects assignment of kernel relation types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether multiple kernel readings coexist in contemporary doctrine or one reading dominates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_forum_doctrine__government_speech_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfm_gov_speech_tr_t0, public_forum_doctrine__government_speech_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pfm_gov_speech_tr_t5, public_forum_doctrine__government_speech_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(pfm_gov_speech_tr_t10, public_forum_doctrine__government_speech_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(pfm_gov_speech_be_t0, public_forum_doctrine__government_speech_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pfm_gov_speech_be_t5, public_forum_doctrine__government_speech_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pfm_gov_speech_be_t10, public_forum_doctrine__government_speech_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(pfm_gov_speech_su_t0, public_forum_doctrine__government_speech_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(pfm_gov_speech_su_t5, public_forum_doctrine__government_speech_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(pfm_gov_speech_su_t10, public_forum_doctrine__government_speech_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_forum_doctrine__government_speech_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(public_forum_doctrine__government_speech_reading, public_forum_doctrine__designated_forum_reading).
narrative_ontology:affects_constraint(public_forum_doctrine__government_speech_reading, public_forum_doctrine__traditional_forum_reading).

% DUAL FORMULATION NOTE:
% The public forum doctrine kernel contains three structurally distinct constraints, one per reading. All three share the same base legal text (the forum doctrine cases) but instantiate different structural interpretations. The government_speech_reading is one constraint story; the designated_forum_reading and traditional_forum_reading are separate stories. Network edges reflect that expanding one reading (government speech) constrains the others (designated forum, traditional forum). Decomposition is necessary because epsilon values differ: government speech exception ε=0.52 (extraction from access claimants), while traditional forum neutrality ε≈0.15 (coordination mechanism with minimal extraction), and designated forum has intermediate ε≈0.30. The three readings belong to the same kernel (same foundational text, same doctrinal controversy) but have different structural properties and should be modeled as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
