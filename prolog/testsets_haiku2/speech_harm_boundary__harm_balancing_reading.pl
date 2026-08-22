% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Protection with Demonstrated-Harm Balancing (Harm-Balancing Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the harm-balancing reading of the contested
 *   kernel speech_harm_boundary. Under this reading, speech receives
 *   presumptive constitutional protection but yields to restrictions when a
 *   court, weighing speaker interests against target interests, determines
 *   that demonstrable harm meets a proportionality threshold. This is ONE of
 *   three readings of the kernel — the absolutist reading and the dignity
 *   reading are separate constraint stories with their own ε values,
 *   beneficiary/victim structures, and types. The harm-balancing reading
 *   claims to be a tangled_rope: it coordinates speaker presumptive freedom
 *   AND target recourse via judicial proportionality balancing, but it
 *   asymmetrically extracts by placing the burden of proving harm on targets
 *   through costly litigation. The authored metrics reflect moderate
 *   extractiveness (0.58 at endpoint) because the reading does protect speech
 *   substantially, but suppression is nontrivial (0.52) because maintaining
 *   the boundary requires active judicial enforcement against both absolutist
 *   and dignity-based challenges.
 *
 * KEY AGENTS:
 *   - Speakers exercising presumptive protection — benefit from the presumption; face restriction only upon demonstrated harm.
 *   - Members of targeted groups — bear costs of proving particularized harm through adjudication; many speech harms fall below demonstrability threshold.
 *   - Judicial system — agenda-setter; applies the proportionality balancing; collects authority from the kernel reading itself.
 *   - Legislatures — partly foreclosed from regulating speech before harm is demonstrated; constrained by the reading's presumption.
 *   - Absolutist and dignity advocates — excluded from consensus; mount active intellectual/legal resistance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.52).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Protection with Demonstrated-Harm Balancing (Harm-Balancing Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'cc00d731-3be0-4588-a8a0-87481b142748').
narrative_ontology:cs_kernel_codification('cc00d731-3be0-4588-a8a0-87481b142748', fixed_text).
narrative_ontology:cs_authority_grounding('cc00d731-3be0-4588-a8a0-87481b142748', lineage).
narrative_ontology:cs_interpretation_layer_present('cc00d731-3be0-4588-a8a0-87481b142748').
narrative_ontology:cs_reading_relation('cc00d731-3be0-4588-a8a0-87481b142748', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc00d731-3be0-4588-a8a0-87481b142748', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('cc00d731-3be0-4588-a8a0-87481b142748', foundational, harm_demonstrability_precondition).
narrative_ontology:cs_axiom_status(harm_demonstrability_precondition, holdable).
narrative_ontology:cs_axiom_grounding('cc00d731-3be0-4588-a8a0-87481b142748', harm_demonstrability_precondition, empirically_contingent).
narrative_ontology:cs_axiom('cc00d731-3be0-4588-a8a0-87481b142748', foundational, proportionality_balancing_principle).
narrative_ontology:cs_axiom_status(proportionality_balancing_principle, holdable).
narrative_ontology:cs_axiom_grounding('cc00d731-3be0-4588-a8a0-87481b142748', proportionality_balancing_principle, deontological).
narrative_ontology:cs_reference_frame('cc00d731-3be0-4588-a8a0-87481b142748', speech_protection_presumptive_with_judicial_balancing).
narrative_ontology:cs_drift_state('cc00d731-3be0-4588-a8a0-87481b142748', digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc00d731-3be0-4588-a8a0-87481b142748', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, speakers_exercising_presumptive_protection).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, judicial_system_applying_proportionality).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, members_of_targeted_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, harassment_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate under a presumption that their speech is protected unless and until a court demonstrates particularized harm meeting the balancing threshold. They bear the burden of coordinating communication without prior restraint but face restriction only when harm is substantiated through adjudication.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_exercising_presumptive_protection, beneficiary,
    moderate, biographical, mobile, national).

% Experience hateful speech and group-based harassment directed at them. Their recourse depends on proving particularized, demonstrable harm through judicial process; they bear the cost of litigating to establish that threshold, and many groups suffer speech harms that do not meet the demonstrability standard.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, members_of_targeted_groups, payer,
    moderate, biographical, constrained, national).

% Adjudicates speech cases by weighing speaker interests against demonstrated harm; sets and enforces the proportionality boundary through case law. Collects authority to decide which speech is protected and which restrictions are constitutional from the kernel reading itself.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, judicial_system_applying_proportionality, agenda_setter,
    institutional, generational, analytical, national).

% Are partly foreclosed from regulating speech at the threshold where harm is alleged but not yet demonstrated; the presumption of protection limits what they can enact. They remain the formal lawmakers but operate within judicial constraints on the speech boundary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislatures_and_policymakers, excluded,
    institutional, generational, constrained, national).

% Argue that the harm-balancing threshold is too permissive and erodes effective speech protection; they reject the reading's core premise. They are excluded from the consensus that grounds this reading but mount active resistance.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_advocates, excluded,
    powerful, biographical, constrained, national).

% Argue that personhood-denying speech should be categorically unprotected without requiring demonstrated particularized harm; they reject the reading's harm-demonstration requirement. They are excluded from the consensus that grounds this reading and advocate for a lower threshold.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, dignity_advocates, excluded,
    powerful, biographical, constrained, national).

% Operate the technical apparatus of harm demonstration: academic researchers, forensic psychologists, social-science methodologists who operationalize what counts as 'demonstrated harm' for adjudication. Their frameworks shape which harms are legible to courts.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, harm_measurement_interpreters, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables speech to circulate with a presumption of protection while maintaining a judicial mechanism to restrict speech that causes demonstrable harm. Coordinates the competing interests of speaker freedom and target protection through proportionality balancing rather than categorical exclusion.
% TRANSFER_FUNCTION: Transfers the cost of proving particularized harm from the restricting authority (burden of proof on the speech-limiter) to the target of speech (who must litigate to establish harm meets the threshold). Shifts restrictions onto speakers only when harm is adjudicated to meet the proportionality standard.
% ABSENT_VOICES: Absolutist speakers who reject any harm threshold (their core premise forecloses the harm-balancing reading itself); dignity-based advocates who would categorically exclude personhood-denying speech without requiring demonstrated harm demonstration; low-resourced groups unable to litigate to establish harm; communities where harm operates through epistemic exclusion or slow accumulation rather than acute incidents that fit forensic frameworks.
% DISAPPEARANCE_RATIONALE: If the harm-balancing boundary vanished overnight, speech governance would reorganize: either back toward near-absolute protection (if absolutist reading takes hold) or toward categorical restrictions on defined-harmful-category speech (if dignity reading takes hold). The current balance itself structures which speech restrictions are constitutional and which are not.
% FOUNDING_PROBLEM: Early speech jurisprudence offered no mechanism to address communication that caused real, measurable harm to target groups — either all speech was protected or all could be restricted, with no proportionate middle ground. The harm-balancing reading emerged to provide a judicial method for evaluating harm claims and calibrating restrictions.
% FOUNDING_PROBLEM_CORROBORATION: Courts and speech scholars from this reading attest the founding problem motivated the doctrine. Absolutist scholars attest the problem statement itself mischaracterizes free speech history; dignity scholars attest the problem understates the harm that goes unaddressed by the balancing approach. Outside corroboration: comparative constitutional law shows democracies split roughly into three groups (absolutist, balancing, categorical) with no consensus.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness series (0.48→0.58) shows the harm-balancing boundary hardening slightly over time as case law accumulates: courts develop sharper methods for operationalizing 'demonstrated harm,' and the demonstrability threshold becomes more demanding, which shifts costs toward targets. Theater rises gently (0.28→0.38) because the judicial adjudication apparatus itself becomes more elaborate and ritualized — proportionality balancing requires extensive fact-finding, expert testimony, and reasoned opinions, some of which is genuine adjudication and some performative jurisprudence. Suppression stays moderate (0.44→0.52) because the reading faces active, organized resistance from both absolutist and dignity camps, so maintaining the boundary requires continuous enforcement — the boundary is not self-stabilizing. The measurement grid shares all three metrics at every time point so the temporal relationships are coherent.
 *
 * PERSPECTIVAL GAP:
 *   The speaker seat and the targeted-group seat should compute dramatically differently from the harm-balancing engine: the speaker has presumptive protection and low effective extraction; the target has the burden of proof and high extraction. Both sit within a single legal system, but they experience radically different directionality. The judicial seat computes as the agenda-setter that derives authority from applying the reading itself — it has strong interest in the reading persisting because the reading grants the court boundary-setting power. This creates a subtle institutional feedback: the reading empowers courts, courts invest in administering the reading, the reading persists. A snare reading of the same legal system might view the court as captured by the speaker presumption and complicit in target extraction. The divergence is exactly what the engine computes from structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers under the harm-balancing reading sit near the beneficiary end: they have presumptive protection and face restriction only when harm is substantiated. Their d is low (around 0.2–0.3, driven by the presumption and the high burden courts impose on speech limiters). Members of targeted groups sit near the target end: they must prove harm through costly adjudication, face high risk that their harm does not meet the demonstrability standard, and can be silenced by speech if it causes psychological/social injury below what courts recognize. Their d is high (around 0.7–0.8). The judicial system derives d from its institutional role: it administers the boundary, so it sits near-symmetric (d~0.5) from a structural view, though it benefits from the authority the reading grants it. Legislatures are constrained but retain some formal power, so their d is moderate (around 0.45). Absolutist and dignity advocates are excluded from the consensus, so their d is uncalculated here — they operate as critics and countervailing seats in the larger system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (speech harms need a proportionate remedy, not all-or-nothing) was live when the reading emerged. It remains contested: courts and speech scholars attest it is still live; absolutists attest harm-talk is a cover story for silencing unpopular speech; dignity scholars attest the problem understates structural harms. Mandatrophy is NOT resolved — the reading does not claim the founding problem has gone away, and the measurement series shows extractiveness and suppression rising slightly over time, which is consistent with the problem remaining live but the remedy becoming more rigid. The tangled_rope classification (genuine coordination function + asymmetric extraction) sits appropriately: the reading does coordinate speaker protection and target recourse, but it does so through a mechanism that privileges demonstrability over other harm categories, which produces extraction (targets pay litigation costs for harms that may not be measurable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrability_bias_by_harm_type,
    'Does the requirement that harm be ''demonstrated'' systematically exclude certain categories of speech injury (epistemic harm, slow accumulation, collective harm to groups) that are harder to measure than acute individual harms?',
    'Systematic study of which speech harms courts accept as demonstrated vs. reject as too diffuse; comparison to harm categories excluded by measurement methodology.',
    'If demonstrability systematically biases against group/structural harms, the reading effectively protects speech that causes unmeasurable-but-real injury to targeted groups, deepening extraction for those groups and converting the harm-balancing boundary into a hidden categorical exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrability_bias_by_harm_type, empirical, 'Whether demonstrability requirement biases against certain harm categories').

omega_variable(
    absolutist_vs_balancing_logical_relationship,
    'Does the harm-balancing reading logically FORECLOSE the absolutist reading (are they incompatible in any single framework), or do they COEXIST as distinct positions held by different parties?',
    'Examine whether a judicial system could hold both readings simultaneously (balancing in practice while protecting absolute presumption in rhetoric) or whether accepting balancing requires rejecting absolutism.',
    'If they foreclose each other, the three readings are in genuine logical conflict and the kernel is genuinely plural. If they coexist (balancing in some contexts, absolutism in others), the kernel is a container for competing institutional positions rather than a set of logically incompatible readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_vs_balancing_logical_relationship, conceptual, 'Whether harm-balancing and absolutism are logically incompatible or institutionally coexistent').

omega_variable(
    institutional_interest_in_boundary_maintenance,
    'Do courts have structural incentive to maintain the harm-balancing boundary precisely because the reading grants courts authority to adjudicate the boundary? Is the reading partially sustained by judicial institutional interest rather than principled commitment?',
    'Historical analysis of judicial reasoning and institutional expansion; comparison to systems where the harm-balancing boundary is enforced by legislatures or other bodies.',
    'If courts sustain the reading partly through institutional self-interest, the reading exhibits snare properties (courts extract authority from maintaining the boundary) and is not pure tangled_rope. The extraction might be characterizable as agenda-setting capture rather than pure asymmetric cost-shifting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_interest_in_boundary_maintenance, empirical, 'Whether judicial institutions sustain the harm-balancing reading partly through institutional interest').

omega_variable(
    kernel_reading_stability_under_digitalization,
    'As speech circulation moves to digital platforms with algorithmic amplification and global reach, does the harm-balancing reading remain operable? Does ''demonstrable particularized harm'' remain intelligible at platform scale?',
    'Empirical analysis of how courts apply the harm-balancing standard to digital speech; assessment of whether the reading can accommodate algorithmic amplification as a harm multiplier.',
    'If the reading becomes inoperable at platform scale, the institutional pressure shifts toward either absolutism (platforms cannot adjudicate per-harm) or categorical exclusion (platforms must remove broad categories without case-by-case balancing). This pressure could produce a drift reading from balancing to either neighbor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability_under_digitalization, empirical, 'Whether harm-balancing reading scales to digital speech circulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__harm_balancing_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__harm_balancing_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__harm_balancing_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__harm_balancing_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel speech_harm_boundary. The sibling readings (absolutist_reading and dignity_reading) are separate constraint stories with distinct ε values and beneficiary/victim structures. All three readings share the kernel commitment that speech receives some protection; they differ in the conditions under which protection yields. The three-way contest structures the speech domain; no single reading operates in isolation. See network.affects_constraints for dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, moderate, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
