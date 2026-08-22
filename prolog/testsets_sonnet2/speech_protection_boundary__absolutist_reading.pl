% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist (Brandenburg) Reading of the Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story authors the absolutist (Brandenburg) reading of the speech
 *   protection boundary kernel: speech protection is near-absolute, and the
 *   sole exception is speech directed at, and likely to produce, imminent
 *   lawless action. Under this reading the protected set is maximized and the
 *   unprotected set is narrow by design. The reading functions as genuine
 *   coordination — it gives speakers, publishers, and lower courts a
 *   predictable, viewpoint-neutral bright line that forecloses discretionary
 *   suppression of dissent — while also externalizing aggregate, non-imminent
 *   harm onto minoritized communities and harassment targets who cannot use
 *   the doctrine's incitement threshold to obtain redress. This story does
 *   not describe the harm_limited_reading or the balancing_reading; those are
 *   separate constraints with their own ε values, authored separately and
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional/analytical) — administers and could revise the standard
 *   - political_dissidents and controversial_speakers: primary beneficiaries (moderate/mobile) — protected advocacy space
 *   - targeted_minority_communities and harassment_targets: primary payers (powerless/trapped) — absorb aggregate harm as externality
 *   - civil_liberties_organizations: beneficiary and co-agenda-setter (organized/mobile) — litigates to preserve the standard
 *   - harm_limited_reading_advocates: excluded (organized/constrained) — alternative framework without doctrinal control
 *   - constitutional_scholars: analytical observer — traces the doctrine's history and effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.4).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.2).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist (Brandenburg) Reading of the Speech Protection Boundary").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '2719bd20-c2e4-4960-b1ce-25e8ee7d421d').
narrative_ontology:cs_kernel_codification('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', formalized).
narrative_ontology:cs_authority_grounding('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', lineage).
narrative_ontology:cs_interpretation_layer_present('2719bd20-c2e4-4960-b1ce-25e8ee7d421d').
narrative_ontology:cs_reading_relation('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', foundational, imminence_is_the_only_legitimate_harm_threshold).
narrative_ontology:cs_axiom_status(imminence_is_the_only_legitimate_harm_threshold, holdable).
narrative_ontology:cs_axiom_grounding('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', imminence_is_the_only_legitimate_harm_threshold, deontological).
narrative_ontology:cs_axiom('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', secondary, counterspeech_and_marketplace_correction_remedy_non_imminent_harm).
narrative_ontology:cs_axiom_status(counterspeech_and_marketplace_correction_remedy_non_imminent_harm, holdable).
narrative_ontology:cs_axiom_grounding('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', counterspeech_and_marketplace_correction_remedy_non_imminent_harm, instrumental).
narrative_ontology:cs_reference_frame('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', brandenburg_incitement_standard).
narrative_ontology:cs_drift_state('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', contemporary_online_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2719bd20-c2e4-4960-b1ce-25e8ee7d421d', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, controversial_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, press_and_publishers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targeted_minority_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, harassment_targets).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, counterspeech_as_remedy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the Brandenburg standard when adjudicating speech cases, striking down restrictions unless speech is directed at inciting imminent lawless action and likely to produce it. Administers the boundary through case law and can, in principle, revise the standard, though doing so requires overturning long-settled precedent.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Rely on the near-absolute protection to organize, protest, and criticize government and powerful institutions without fear of prosecution for advocacy alone, no matter how provocative, as long as it stops short of direct incitement to imminent violence.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, mobile, national).

% Includes extremist organizers, provocateurs, and fringe ideologues who use the doctrine's high bar to continue public advocacy that stops short of literal incitement, even where the content is understood by targets as threatening or dehumanizing.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, controversial_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Operate with confidence that publishing controversial, offensive, or unpopular material will not trigger liability absent direct incitement, enabling investigative and adversarial journalism without prior restraint concerns.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, press_and_publishers, beneficiary,
    organized, generational, mobile, national).

% Litigate to preserve and extend the Brandenburg standard, treating it as the load-bearing doctrine protecting all future unpopular speech, including their own advocacy work. Actively shapes the doctrine's application through strategic litigation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__absolutist_reading, civil_liberties_organizations, agenda_setter).

% Bear the accumulated, diffuse harm of protected hate speech, dehumanizing rhetoric, and organized harassment campaigns that never individually cross the imminent-incitement line but collectively produce chilling effects, psychological harm, and material insecurity. Cannot opt out of the public sphere where this speech circulates and have no direct legal remedy under the standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targeted_minority_communities, payer,
    powerless, biographical, trapped, national).

% Individuals subjected to sustained, coordinated online or public harassment campaigns that fall short of direct incitement to imminent violence. Have limited recourse because the doctrine treats each individual utterance in isolation rather than the aggregate campaign.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harassment_targets, payer,
    powerless, immediate, trapped, regional).

% Legal scholars and advocacy groups who argue protection should be conditional on absence of dignity and equality harm. Their framework is structurally excluded from controlling doctrine under the current reading; they participate in academic and political discourse but cannot bind judicial outcomes.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harm_limited_reading_advocates, excluded,
    organized, generational, constrained, national).

% Study the doctrine's history, its departure from earlier bad-tendency and clear-and-present-danger tests, and its empirical effects on both dissident speech and vulnerable communities, without a direct stake in litigation outcomes.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, content-neutral bright-line rule that lets speakers, publishers, and lower courts know in advance what speech is protected, avoiding case-by-case discretionary suppression of unpopular viewpoints by transient majorities or officials.
% TRANSFER_FUNCTION: Moves the risk of harm from speech-based hostility away from state actors and speakers and onto the individuals and communities targeted by that hostility, who absorb the psychological, social, and sometimes physical costs of speech that stops short of the doctrine's narrow incitement threshold.
% ABSENT_VOICES: Targeted minority communities and harassment targets are formally represented in the abstract by amicus briefs and scholarship but have no doctrinal standing to argue that aggregate or dignitary harm, rather than imminence, should determine protection. Harm-limited and balancing-reading advocates are active in academic and legislative debate but cannot control outcomes under this reading's controlling precedent.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard were replaced overnight by a harm-based or balancing test, a substantial volume of currently protected advocacy, provocative political speech, and inflammatory rhetoric would become subject to prosecution or civil liability; political dissidents and press organizations would face materially higher legal risk, while targeted communities would gain new legal remedies against speech currently beyond reach.
% FOUNDING_PROBLEM: The doctrine was built to solve the problem of governments and majorities using vague, discretionary harm or sedition standards to suppress dissent, radical political organizing, and unpopular viewpoints, as had occurred extensively under prior clear-and-present-danger and bad-tendency tests.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and much of the federal judiciary attest the founding problem — discretionary suppression of dissent — remains fully live and cite ongoing government efforts to restrict protest and unpopular speech. Independent civil rights scholars and international human rights bodies, outside the beneficiary set, attest that the doctrine as applied has produced a distinct and under-addressed harm: sustained externalized cost to targeted communities that the original anti-suppression rationale did not anticipate and does not remedy.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.4, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).
:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.40 at interval end, rising slowly from 0.25) because the doctrine's cost to targeted communities is real but diffuse and indirect — it is an externality of a genuinely functioning coordination rule, not a direct extraction mechanism. Suppression is authored low (0.20) because the doctrine's entire point is to minimize suppression of speech; what it does not minimize is harm experienced downstream of protected speech, which is a distinct axis from suppression as defined here. Theater ratio is low and only slowly rising (0.10 to 0.15) because enforcement of the standard is substantively applied in courts, not performative. Accessibility collapse is moderate (0.35): once the doctrine is understood, alternative doctrinal frameworks (harm-based, balancing) remain live in scholarship and legislative debate, so collapse is partial, not complete. Resistance is moderate-high (0.55) reflecting sustained, organized pushback from harm-limited and balancing-reading advocates, critical race theorists, and international human rights bodies.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits as agenda_setter with analytical exit — it administers the rule and bears no personal cost from either outcome. Political dissidents, controversial speakers, press, and civil liberties organizations are structural beneficiaries: the near-absolute protection subsidizes their expressive activity, so they derive low d. Targeted minority communities and harassment targets are structural payers with trapped exit — they cannot opt out of the public sphere where protected speech circulates and have no doctrinal remedy for aggregate harm, so they derive high d. This is a case where the same doctrine produces low d for organized, mobile beneficiaries and high d for powerless, trapped payers — the asymmetry is central to the reading's contested status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governmental and majoritarian suppression of dissent via vague harm standards — remains partly live (contemporary efforts to restrict protest speech continue), which argues against treating the doctrine as pure mandatrophy. But the founding problem's original scope (protecting political dissent from state suppression) has drifted to also shield conduct with no dissident political content (targeted harassment, organized hate campaigns) that the doctrine's architects did not centrally contemplate. The disappearance_verdict of world_rearranges combined with founding_problem_status of contested signals a live coordination function riding alongside an externality the original justification does not fully cover — exactly the structure a Tangled Rope classification would flag if the coordination and extraction elements were shown to be inseparable; this story instead classifies as rope because the coordination function is judged genuine and primary, with the externality authored honestly rather than smoothed away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_externality_separability,
    'Is the Brandenburg standard''s coordination benefit (predictability, protection from discretionary suppression) structurally separable from its externalized aggregate harm to targeted communities, or does achieving the coordination benefit require accepting the externality as its price?',
    'Comparative doctrinal analysis of jurisdictions with narrower incitement standards or harm-based carve-outs: if such jurisdictions retain comparable protection against discretionary state suppression of dissent while reducing harm to targeted groups, the functions are separable.',
    'If separable, the externality is better modeled as avoidable extraction riding on the coordination function (pushing toward tangled_rope); if inseparable, the externality is closer to an inherent cost of the coordination mechanism itself (supporting the rope classification authored here).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_externality_separability, conceptual, 'Whether the doctrine''s coordination benefit and its externalized harm can be structurally decoupled.').

omega_variable(
    kernel_reading_selection_as_political_outcome,
    'Is the persistence of the absolutist reading as controlling doctrine (rather than the harm_limited or balancing readings) itself best explained as a settled constitutional consensus, or as the outcome of which litigants and interest groups have historically had standing and resources to shape First Amendment case law?',
    'Historical analysis of the litigant composition and funding behind landmark speech-protection cases versus the composition of communities bearing the externalized harm; comparative study of jurisdictions (e.g., European hate-speech frameworks) that adopted harm-limited readings and their downstream effects.',
    'If the absolutist reading''s dominance reflects asymmetric litigation capacity rather than superior normative resolution of the underlying tension, the reading''s claim to represent settled coordination is weaker than the doctrine''s self-presentation suggests, and its structural resemblance to false-summit dynamics (a mountain-like inevitability claim serving identifiable beneficiaries) increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_as_political_outcome, conceptual, 'Whether doctrinal dominance reflects normative settlement or asymmetric access to constitutional litigation.').

omega_variable(
    aggregate_versus_individuated_harm_measurement,
    'Does treating each speech act in isolation (as Brandenburg requires) systematically undercount harm that is real only in aggregate, such as sustained harassment campaigns or normalized dehumanizing rhetoric?',
    'Empirical studies measuring psychological and material harm to targeted communities under isolated-incident versus aggregate-pattern legal frameworks in comparable jurisdictions.',
    'If aggregate harm is substantially undercounted by the doctrine''s atomistic unit of analysis, the authored extractiveness value (0.40) may understate true harm to targeted communities, and the doctrine''s coordination benefit would need to be weighed against a larger externality than currently modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_versus_individuated_harm_measurement, empirical, 'Whether the doctrine''s per-incident analysis structurally undercounts aggregate harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__absolutist_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__absolutist_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__absolutist_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__absolutist_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(spee_tr_t50, speech_protection_boundary__absolutist_reading, theater_ratio, 50, 0.145).
narrative_ontology:measurement(spee_tr_t60, speech_protection_boundary__absolutist_reading, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__absolutist_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__absolutist_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__absolutist_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__absolutist_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(spee_be_t50, speech_protection_boundary__absolutist_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement(spee_be_t60, speech_protection_boundary__absolutist_reading, base_extractiveness, 60, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(speech_protection_boundary__absolutist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, balancing_reading).

% DUAL FORMULATION NOTE:
% This story, harm_limited_reading, and balancing_reading form a three-member constraint family decomposing the natural-language concept 'the speech protection boundary' per the ε-invariance principle. Each reading is evaluated as a distinct structural claim with its own ε: this absolutist reading authors ε=0.40 (genuine coordination with externalized aggregate harm); the harm_limited_reading is expected to author substantially higher ε for the same standing arrangement (treating the externality as central extraction); the balancing_reading occupies an intermediate position (case-by-case adjudication reduces bright-line predictability but allows harm to enter the calculus). All three link to each other via affects_constraints. None is the 'correct' measurement of a single underlying constraint — they are three different constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
