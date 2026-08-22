% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Speech Protection Absolutist Reading — Near-Absolute Speaker Autonomy, Narrow Harm Override
 *   domain: constitutional/political/communication_ethics
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the contested
 *   speech-harm-boundary kernel: speech protection operates near-absolutely,
 *   the set of unprotected categories (incitement, true threats, defamation
 *   with actual malice, obscenity) is kept deliberately narrow, and the
 *   threshold for harm to override protection is set extremely high. The
 *   reading is defended by high-reach speakers, media institutions, and civil
 *   liberties litigators as necessary insurance against state and
 *   majoritarian censorship of dissent. Its structural cost falls on those
 *   targeted by speech that causes real, cumulative harm but does not clear
 *   the narrow legal thresholds — they have essentially no legal recourse and
 *   are expected to rely on 'more speech' as a remedy that assumes a
 *   reach-symmetry they do not have. This is a reading of the kernel, not a
 *   synthesis of it: the sibling readings (harm_balancing_reading,
 *   dignity_reading) draw the line differently and produce different
 *   beneficiary/victim structures and different epsilon under their own
 *   lights — they are separate constraint stories, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - high_reach_speakers: Primary beneficiary (powerful/arbitrage) — near-total protection for expansive claims
 *   - media_and_publishing_institutions: Primary beneficiary (institutional/arbitrage) — operates without routine liability exposure
 *   - targets_of_hate_speech: Primary target (powerless/trapped) — bears harm with minimal legal recourse
 *   - vulnerable_minority_communities: Secondary target (powerless/trapped) — absorbs diffuse cumulative harm across many sub-threshold incidents
 *   - civil_liberties_litigators: Agenda setter (organized/analytical) — actively defends and expands the narrow unprotected-category boundary
 *   - courts_and_appellate_judiciary: Agenda setter (institutional/analytical) — administers and could in principle move the doctrinal line
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.35).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Speech Protection Absolutist Reading — Near-Absolute Speaker Autonomy, Narrow Harm Override").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional/political/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '55770af8-e417-46df-bee5-cc1ec1b31195').
narrative_ontology:cs_kernel_codification('55770af8-e417-46df-bee5-cc1ec1b31195', fixed_text).
narrative_ontology:cs_authority_grounding('55770af8-e417-46df-bee5-cc1ec1b31195', lineage).
narrative_ontology:cs_interpretation_layer_present('55770af8-e417-46df-bee5-cc1ec1b31195').
narrative_ontology:cs_reading_relation('55770af8-e417-46df-bee5-cc1ec1b31195', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('55770af8-e417-46df-bee5-cc1ec1b31195', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_axiom('55770af8-e417-46df-bee5-cc1ec1b31195', foundational, state_censorship_is_the_paradigm_harm).
narrative_ontology:cs_axiom_status(state_censorship_is_the_paradigm_harm, holdable).
narrative_ontology:cs_axiom_grounding('55770af8-e417-46df-bee5-cc1ec1b31195', state_censorship_is_the_paradigm_harm, deontological).
narrative_ontology:cs_axiom('55770af8-e417-46df-bee5-cc1ec1b31195', foundational, counter_speech_is_a_sufficient_remedy_for_private_harm).
narrative_ontology:cs_axiom_status(counter_speech_is_a_sufficient_remedy_for_private_harm, holdable).
narrative_ontology:cs_axiom_grounding('55770af8-e417-46df-bee5-cc1ec1b31195', counter_speech_is_a_sufficient_remedy_for_private_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('55770af8-e417-46df-bee5-cc1ec1b31195', categorical_unprotected_class_doctrine).
narrative_ontology:cs_drift_state('55770af8-e417-46df-bee5-cc1ec1b31195', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55770af8-e417-46df-bee5-cc1ec1b31195', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, high_reach_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, media_and_publishing_institutions).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, civil_liberties_litigators).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, harassment_targets_with_limited_platform_access).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, vulnerable_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Public figures, commentators, and organizations with large platforms operate under a legal regime that protects nearly all of their speech short of incitement, true threats, defamation with actual malice, or obscenity. They can make sweeping, harmful, or degrading claims about targets with minimal legal exposure, and they benefit from courts treating the harm threshold as extremely hard to clear.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, high_reach_speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Newspapers, broadcasters, and publishers rely on the doctrine's narrow unprotected-category set to publish investigative, controversial, and offensive material without pre-clearance or routine liability. They actively litigate to keep the override threshold high and treat any expansion of unprotected categories as an existential threat to their operating model.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, media_and_publishing_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Activists and minority political voices depend on the same near-absolute protection to criticize government and powerful institutions without being silenced through defamation suits or incitement charges brought in bad faith. Their benefit is real but their actual exit options if the doctrine flipped would be far worse than those of high-reach speakers.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, constrained, national).

% Individuals and groups subjected to degrading, dehumanizing, or threatening speech that falls short of true threats or incitement have almost no legal recourse under this reading. They bear the reputational, psychological, and social costs directly and cannot exit the speech environment they are embedded in — leaving a platform or community does not remove the speech's downstream effects on their standing.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_hate_speech, payer,
    powerless, biographical, trapped, national).

% People subjected to sustained, coordinated harassment campaigns that individually fall under the protected-speech umbrella have no legal counter and typically lack the platform reach to counter-speak effectively. The doctrine's 'more speech, not enforced silence' remedy assumes a symmetry of reach that does not exist for them.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, harassment_targets_with_limited_platform_access, payer,
    powerless, immediate, trapped, regional).

% Communities repeatedly targeted by speech that stops short of incitement but normalizes hostility toward them bear cumulative, diffuse costs — social exclusion, discrimination, and safety risk — that the doctrine treats as outside its cognizance because no single utterance clears the harm threshold.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, vulnerable_minority_communities, payer,
    powerless, generational, trapped, national).

% Organizations and lawyers who litigate to keep the unprotected-category set narrow and the harm override threshold high. They set and defend the doctrinal boundary through strategic litigation, amicus briefs, and public advocacy, treating any softening as a slippery slope toward viewpoint-based censorship.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, civil_liberties_litigators, agenda_setter,
    organized, generational, analytical, national).

% Courts apply and refine the doctrine case by case, holding the line on the narrow unprotected-category set. They administer the boundary and could in principle expand the harm override, but doing so would require overturning substantial precedent they themselves built and continue to cite as settled.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts_and_appellate_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Scholars and advocates who favor a dignity-based or harm-balancing framework are structurally excluded from shaping the doctrine's operative boundary — their arguments are heard in academic and legislative venues but rarely move the constitutional floor, which remains set by the absolutist tradition's own precedent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, dignity_and_harm_balancing_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable, viewpoint-neutral rule that lets speakers, publishers, and courts avoid case-by-case adjudication of whether particular content is 'too harmful' to say — a bright-line rule reduces arbitrary suppression and protects unpopular or minority political speech from majoritarian silencing.
% TRANSFER_FUNCTION: Moves the cost of tolerating harmful, degrading, or threatening speech from the state (which would otherwise have to police content) and from speakers (who would otherwise face liability) onto the individuals and communities targeted by that speech, who absorb reputational, psychological, and social harm with minimal legal recourse.
% ABSENT_VOICES: Targets of speech that falls short of the incitement/true-threat/defamation/obscenity carve-outs are structurally unheard in the doctrine's own operative test — their harm is acknowledged rhetorically ('more speech is the remedy') but is not a cognizable legal injury. Dignity-reading and harm-balancing-reading advocates are excluded from setting the operative constitutional floor even where they dominate academic and international human-rights discourse.
% DISAPPEARANCE_RATIONALE: If the absolutist reading's near-absolute protection and narrow unprotected-category set were replaced overnight by a harm-balancing or dignity standard, huge swaths of currently-protected political, artistic, and offensive speech would become litigable or actionable; media institutions would restructure editorial review, platforms would face new liability exposure, and the current beneficiaries (high-reach speakers, publishers) would lose significant operating latitude while currently-unprotected targets would gain new legal footing.
% FOUNDING_PROBLEM: Historical suppression of dissenting, minority, and unpopular political speech by state and majoritarian actors — sedition prosecutions, licensing regimes, and content-based censorship used to entrench incumbent power and silence critics.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties litigators and courts attest the founding problem (state suppression of dissent) remains fully live and cite ongoing prosecutions of speech in other jurisdictions as evidence. Independent sources outside the doctrine's direct beneficiaries — comparative constitutional scholars and international human-rights bodies — attest that the doctrine's harm-tolerance has drifted well past its original anti-censorship rationale into providing cover for speech that causes concentrated harm to identifiable, less-powerful groups, a function the founding problem does not obviously require.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the doctrine's operation transfers real, non-trivial harm costs onto identifiable targets who cannot access legal remedy or effective counter-speech, while conferring substantial operating latitude on high-reach speakers and institutions. Suppression is comparatively low (0.35) because the doctrine's defining feature is the ABSENCE of suppressive machinery against speakers — the coercive apparatus here runs the other direction, foreclosing remedies for targets rather than compelling silence. Theater ratio is low (0.2): the doctrinal function (protecting dissent from state censorship) is substantially real, not primarily performative, though rising modestly as the doctrine is invoked to shield harms increasingly distant from the founding anti-censorship rationale. Accessibility collapse (0.4) is moderate — alternative doctrinal framings (harm-balancing, dignity) remain visible and litigated in other jurisdictions and in academic discourse, so alternatives have not collapsed as completely as under a genuine mountain. Resistance (0.62) is substantial: dignity and harm-balancing advocates, targeted communities, and comparative law scholars actively contest the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (courts, litigators) the doctrine is a principled, viewpoint-neutral bulwark against censorship; from the payer seats (harassment targets, minority communities) the identical rule operates as a standing license for accumulating, uncompensated harm. The engine computes these divergent seat-level classifications from the authored power/exit/beneficiary structure; this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   High-reach speakers and publishing institutions sit near the beneficiary end: they collect the doctrine's protective value directly and have mobile/arbitrage exit (they can route content through multiple jurisdictions or platforms). Targets of sub-threshold harmful speech sit near the full-target end: they are trapped (cannot exit the speech environment that damages their standing) and bear concentrated or cumulative costs with no structural offset. Political dissidents are a genuine beneficiary group but with much weaker exit than high-reach speakers — their benefit is real but their position is more fragile, which is why they are modeled at moderate power rather than powerful.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state suppression of political dissent — remains genuinely live in many jurisdictions, which is why founding_problem_status is coded 'contested' rather than 'dead': this is not a pure zombie mandate. But the doctrine's operative reach has drifted from anti-censorship insurance into a general shield for concentrated harms against less-powerful targets that have little to do with state suppression of dissent. The classification as tangled_rope (rather than snare) reflects that genuine coordination value persists (protecting dissidents, enabling investigative journalism) alongside real, asymmetric extraction (harm absorbed by powerless targets) — both must be true simultaneously for tangled_rope, and both are authored here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the absolutist reading''s narrow unprotected-category set the uniquely correct interpretation of the speech-harm kernel, or is it one contestable reading among the absolutist/harm-balancing/dignity triad, each internally coherent?',
    'Track which reading a given jurisdiction''s supreme/constitutional court actually adopts over time, and whether jurisdictions converge or diverge as comparative pressure mounts; a genuine convergence toward one reading would suggest a discoverable structural fact rather than a pure value choice.',
    'If the readings genuinely converge under comparative pressure, that supports treating one reading as structurally privileged (closer to mountain); if they persist as stable, non-converging alternatives across jurisdictions with different underlying constitutional commitments, that supports treating the kernel as irreducibly contested (each reading remains a distinct, non-adjudicable tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the absolutist reading is discoverable truth or one contestable framing among three live readings of the same kernel.').

omega_variable(
    counter_speech_symmetry_assumption,
    'Does the doctrine''s core remedy assumption — that the answer to harmful speech is more speech — hold empirically given massive asymmetries in platform reach and social capital between high-reach speakers and their targets?',
    'Empirical study of whether targeted individuals/communities can, in practice, achieve reach parity with speakers who harm them, across platform types and social contexts; measure actual counter-speech effectiveness rates.',
    'If counter-speech is empirically ineffective at the reach asymmetries typically observed, the doctrine''s coordination justification (marketplace of ideas self-corrects) weakens substantially, pushing the classification toward snare; if counter-speech is shown to be broadly effective, the tangled_rope''s coordination leg is stronger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_symmetry_assumption, empirical, 'Whether the marketplace-of-ideas remedy is empirically sound given real-world reach asymmetries.').

omega_variable(
    founding_problem_scope_drift,
    'Has the doctrine''s protective scope drifted from its founding anti-state-censorship rationale into a broader shield covering private-actor harms the founding problem never contemplated?',
    'Historical and doctrinal analysis comparing the category of speech the doctrine protected at founding (primarily political and religious dissent against state suppression) against the category of speech it protects today (including targeted harassment campaigns and dehumanizing rhetoric against private individuals).',
    'If substantial scope drift is confirmed, the founding_problem_status classification of ''contested'' should trend toward ''dead'' for the private-harm subset even while remaining ''live'' for the core anti-state-censorship function — suggesting the single constraint may itself warrant future decomposition along the state-censorship vs. private-harm axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_scope_drift, empirical, 'Whether the doctrine''s operative scope has drifted beyond its founding anti-censorship rationale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__absolutist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__absolutist_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__absolutist_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__absolutist_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(spee_tr_t50, speech_harm_boundary__absolutist_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(spee_tr_t60, speech_harm_boundary__absolutist_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__absolutist_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__absolutist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__absolutist_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__absolutist_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__absolutist_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(spee_be_t60, speech_harm_boundary__absolutist_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__absolutist_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__absolutist_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__absolutist_reading, suppression_requirement, 30, 0.33).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__absolutist_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__absolutist_reading, suppression_requirement, 50, 0.345).
narrative_ontology:measurement(spee_su_t60, speech_harm_boundary__absolutist_reading, suppression_requirement, 60, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__absolutist_reading, 0.1).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the speech_harm_boundary kernel. absolutist_reading (this story) authors near-absolute protection with a narrow unprotected-category set and high epsilon borne by targets who cannot clear the harm threshold. harm_balancing_reading authors presumptive protection yielding to demonstrated harm via proportionality balancing — a structurally distinct beneficiary/victim allocation with a lower epsilon for targets and correspondingly different speaker-side costs. dignity_reading authors protection categorically subordinate to human dignity, with personhood-denying speech entirely unprotected — the most target-protective, lowest-epsilon-for-targets reading of the three. Each story has its own epsilon, its own claimed_type, and its own stakeholder set per the epsilon-invariance principle; they are linked here rather than merged into one measurement-parameterized constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
