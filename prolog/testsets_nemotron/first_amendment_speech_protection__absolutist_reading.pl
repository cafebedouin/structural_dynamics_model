% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Speech Protection
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the First
 *   Amendment's Speech Clause: the text 'Congress shall make no law...
 *   abridging the freedom of speech' is read categorically, protecting all
 *   speech except narrow historical exclusions (incitement, true threats,
 *   obscenity, defamation, fraud). The reading maximizes the protected speech
 *   set by treating content-based restrictions as presumptively invalid. The
 *   structural consequence is that harms flowing from protected speech — hate
 *   speech, systemic harassment, coordinated disinformation targeting
 *   minorities — are externalized as the cost of liberty. The constraint
 *   requires active judicial enforcement to maintain the categorical boundary
 *   against legislative encroachment. The engine will compute per-seat
 *   classifications from the declared beneficiaries (speakers, majority
 *   opinion holders) and victims (targeted minorities, vulnerable
 *   populations), producing seat divergence: the agenda-setter (judiciary)
 *   and beneficiaries experience coordination; the victims experience
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.18).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.12).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '6968d8bd-c84b-4884-a60a-5c3519062608').
narrative_ontology:cs_kernel_codification('6968d8bd-c84b-4884-a60a-5c3519062608', fixed_text).
narrative_ontology:cs_authority_grounding('6968d8bd-c84b-4884-a60a-5c3519062608', lineage).
narrative_ontology:cs_interpretation_layer_present('6968d8bd-c84b-4884-a60a-5c3519062608').
narrative_ontology:cs_reading_relation('6968d8bd-c84b-4884-a60a-5c3519062608', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('6968d8bd-c84b-4884-a60a-5c3519062608', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('6968d8bd-c84b-4884-a60a-5c3519062608', foundational, textual_categoricalism).
narrative_ontology:cs_axiom_status(textual_categoricalism, holdable).
narrative_ontology:cs_axiom_grounding('6968d8bd-c84b-4884-a60a-5c3519062608', textual_categoricalism, conventional).
narrative_ontology:cs_axiom('6968d8bd-c84b-4884-a60a-5c3519062608', foundational, viewpoint_neutrality_as_constraint).
narrative_ontology:cs_axiom_status(viewpoint_neutrality_as_constraint, holdable).
narrative_ontology:cs_axiom_grounding('6968d8bd-c84b-4884-a60a-5c3519062608', viewpoint_neutrality_as_constraint, deontological).
narrative_ontology:cs_reference_frame('6968d8bd-c84b-4884-a60a-5c3519062608', founding_era_categorical_prohibition).
narrative_ontology:cs_drift_state('6968d8bd-c84b-4884-a60a-5c3519062608', contemporary_algorithmic_amplification_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6968d8bd-c84b-4884-a60a-5c3519062608', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers_and_publishers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_public_opinion_holder).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minority_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, majority_public_opinion_holder).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, categorical_textualism).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, viewpoint_neutrality_principle).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, chilling_effect_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the categorical rule through judicial review: strikes down content-based speech restrictions, defines and polices the narrow historical exclusions, bears the institutional cost of maintaining legitimacy while enforcing an unpopular rule (protecting hated speech). Does not collect rents from the constraint but gains structural authority from being its authoritative interpreter.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Includes political speakers, media organizations, artists, protesters, and commercial speakers. Gains the ability to speak without prior restraint or content-based penalty. Can exit the constraint's coverage by choosing not to speak, but the constraint's value is precisely the option to speak freely. Major platforms and publishers have arbitrage-grade exit (can relocate, litigate, influence doctrine); individual speakers have constrained exit.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers_and_publishers, beneficiary,
    organized, biographical, mobile, national).

% Benefits from a public sphere where majority views circulate without state suppression. Also bears diffuse costs when protected speech degrades public discourse (disinformation, polarization). Exit is constrained — cannot opt out of the information environment — but the net position is beneficial because the alternative (state-controlled discourse) is worse for majority opinion holders.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_public_opinion_holder, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, majority_public_opinion_holder, payer).

% Racial, religious, gender, and sexual minorities who are disproportionately targeted by protected hate speech, harassment campaigns, and coordinated disinformation. Bear the externalized costs of absolutist protection: psychological harm, civic exclusion, economic discrimination, physical violence incited by protected speech. Exit is identity_locked — demographic membership is not choosable; the speech environment is inescapable. No alternative institutional forum exists where the constraint does not apply.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minority_groups, payer,
    powerless, generational, identity_locked, national).

% Includes children, institutionalized persons, undocumented immigrants, and others with limited civic voice. Bear costs from protected speech that exploits vulnerability (fraud targeting elderly, radicalization of youth, exploitation of institutionalized populations). Exit is trapped — structural barriers (legal status, institutional control, developmental capacity) prevent meaningful exit from the speech environment.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, vulnerable_populations, payer,
    powerless, biographical, trapped, national).

% Would regulate speech to address harms (hate speech laws, disinformation regulation, campaign finance) but is structurally blocked by the constraint. Its regulatory power is the object of the constraint's coordination function. Can attempt constitutional amendment (prohibitively difficult) or appoint judges who favor sibling readings (constrained exit). Not a victim — its power is constrained by design, not extracted from.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, state_legislature, excluded,
    powerful, biographical, constrained, national).

% Analyze the constraint's operation across seats, trace its doctrinal evolution, and evaluate its structural consequences. Do not collect from or pay into the constraint. Provide the external corroboration for the founding problem status.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing government censorship: no single speaker can credibly commit to resisting state suppression, but a categorical constitutional rule enforced by an independent judiciary creates a stable equilibrium where the state refrains from content-based regulation.
% TRANSFER_FUNCTION: Moves the cost of speech harms (hate speech, harassment, disinformation) from the state (which would internalize them via regulation) onto targeted minorities and vulnerable populations, who bear them without consent or compensation. In exchange, speakers gain immunity from state suppression.
% ABSENT_VOICES: Targeted minorities and vulnerable populations were structurally excluded from the constitutional convention and ratification debates. Their descendants remain excluded from the interpretive community that maintains the absolutist reading — the judiciary, organized bar, and constitutional academy have historically been demographically unrepresentative. They would object to the externalization of harm costs if present in the interpretive conversation.
% DISAPPEARANCE_RATIONALE: If the absolutist constraint vanished overnight, legislatures would enact hate speech regulations, platform accountability laws, and disinformation statutes within months. The speech environment would reorganize around harm-based exceptions. Targeted minorities would gain legal recourse; speakers would face new compliance costs; the judiciary would lose its categorical adjudicatory role. The world rearranges because the constraint actively structures the legislative-judicial-speech market equilibrium.
% FOUNDING_PROBLEM: The founding problem was preventing the new federal government from enacting sedition laws and suppressing political dissent — the Alien and Sedition Acts of 1798 were the proximate threat. The categorical 'no law' rule was built to make censorship constitutionally impossible, not merely politically difficult.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing state censorship of dissent) is attested as live by civil liberties organizations (ACLU, FIRE), press freedom advocates (RSF, CPJ), and dissident movements globally — all outside the primary beneficiary set of majority speakers. However, the *scope* of the live problem is contested: original threat was state suppression of political speech; modern threats include private platform governance and algorithmic amplification, which the absolutist reading does not reach.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).
:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.18) because the constraint primarily coordinates by blocking government suppression rather than extracting resources. However, it is non-zero because the categorical rule externalizes harm costs onto victims who cannot exit the speech environment. Suppression is low (0.12) because the constraint's function is anti-suppression — it suppresses government's ability to suppress speech. Theater ratio is minimal (0.08) because the coordination function (protecting speech from state control) is genuine and actively litigated. Accessibility collapse is high (0.72) because once the categorical rule is accepted, alternatives (balancing tests, harm-based exceptions) are structurally excluded from legitimate constitutional argument. Resistance is moderate (0.25) because the reading faces persistent challenge from competing readings and from affected groups seeking narrower protection.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different effective types per seat: for speakers/beneficiaries, a Rope (coordination against state suppression); for targeted minorities/victims, a Snare (extraction of dignity/safety with no exit); for the judiciary/agenda_setter, a Tangled Rope (genuine coordination function + asymmetric extraction via externalized harm). The claimed_type 'tangled_rope' reflects the aggregate structural reality — the constraint cannot be purely coordinative because it has identifiable victims bearing uncompensated costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and majority opinion holders are beneficiaries (d near 0.0): the constraint subsidizes their expressive freedom by blocking state interference. Targeted minorities and vulnerable populations are victims (d near 1.0): they bear the externalized costs of protected hate speech, harassment, and disinformation without consent or exit option — identity_locked by demographic membership. The judiciary (agenda_setter) sits near symmetric (d ~0.5): it bears enforcement costs but gains institutional legitimacy from maintaining the categorical boundary. The state (excluded/powerful) is constrained from regulating speech but is not a direct victim — its regulatory power is the object of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing government censorship of political dissent) remains live but has shifted: the original threat was sedition acts; the modern threat includes platform governance and algorithmic amplification. The constraint has not atrophied — its coordination function is actively invoked — but the victim set has expanded as speech harms have scaled with technology. Mandatrophy is not resolved; the arrangement continues to serve its founding purpose while generating new extraction patterns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly does the structural disagreement between absolutist_reading and harm_limited_reading locate — in the beneficiary/victim assignment, the coordination function definition, or the scope of ''narrow historical exclusions''?',
    'Comparative case-law analysis: map each reading''s beneficiary/victim sets and exception lists across the same factual scenarios (e.g., targeted harassment, algorithmic amplification, hate speech). The divergence point will show which structural element carries the disagreement.',
    'If the disagreement is in beneficiary/victim assignment, the readings share a coordination function but differ on who bears its cost — a distributive dispute. If in coordination function, they are different constraints entirely (per ε-invariance). If in exception scope, the readings differ on the boundary of the categorical rule — a boundary dispute within shared structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural locus of disagreement between absolutist and harm-limited readings of the First Amendment kernel').

omega_variable(
    harm_externalization_measurement,
    'Can the externalized harm costs borne by targeted minorities under absolutist protection be quantified in a way that is commensurable with the coordination benefit (preventing state censorship)?',
    'Empirical research on measurable harms from protected hate speech and disinformation (health outcomes, civic participation, economic opportunity) vs. counterfactual harms from state censorship regimes. Requires cross-jurisdictional comparison (e.g., US vs. European hate speech regimes).',
    'If harms are quantifiable and substantial, the extraction component of this constraint is larger than the base_properties.extractiveness score suggests — the engine''s effective extraction for victim seats will be higher. If harms are speculative or offset by countervailing benefits of free speech, the current ε is defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_externalization_measurement, empirical, 'Commensurability of externalized harm costs vs. coordination benefits in absolutist speech protection').

omega_variable(
    narrow_exclusions_boundary,
    'Are the ''narrow historical exclusions'' (incitement, true threats, obscenity, defamation, fraud) structurally stable, or does the absolutist reading''s logic tend to expand or contract them over time?',
    'Longitudinal analysis of Supreme Court doctrine: track whether the exception categories have narrowed (expanding protection) or widened (contracting protection) under absolutist-adjacent majorities. Compare with sibling readings'' exception trajectories.',
    'If exceptions are contracting, the constraint''s extractiveness on victims increases over time (more harmful speech protected). If exceptions are stable, the current ε trajectory is accurate. If exceptions are expanding, the constraint is drifting toward a more balanced reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_exclusions_boundary, empirical, 'Stability of the exception boundary in absolutist First Amendment doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1791, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1791, 0.02).
narrative_ontology:measurement(firs_tr_t1868, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1868, 0.03).
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1969, 0.06).
narrative_ontology:measurement(firs_tr_t1992, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1992, 0.07).
narrative_ontology:measurement(firs_tr_t2026, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(firs_be_t1791, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1791, 0.05).
narrative_ontology:measurement(firs_be_t1868, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1868, 0.08).
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1919, 0.12).
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1969, 0.15).
narrative_ontology:measurement(firs_be_t1992, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1992, 0.16).
narrative_ontology:measurement(firs_be_t2026, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1791, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1791, 0.05).
narrative_ontology:measurement(firs_su_t1868, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1868, 0.08).
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1919, 0.15).
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1969, 0.1).
narrative_ontology:measurement(firs_su_t1992, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1992, 0.11).
narrative_ontology:measurement(firs_su_t2026, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__absolutist_reading, 0.1).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single colloquial label 'First Amendment speech protection' into three structurally distinct constraints with different ε values, beneficiary/victim structures, and claimed types. The absolutist_reading (this story) has ε=0.18 and claimed_type=tangled_rope. The categorical_balancing_reading would have higher ε (more state regulatory power retained) and different victim sets. The harm_limited_reading would have lower ε for speakers but higher extractiveness on state power. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, institutional, 0.35).
constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
