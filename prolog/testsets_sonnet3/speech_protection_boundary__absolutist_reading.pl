% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Near-Absolute Speech Protection with Brandenburg Imminence Exception
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story authors the absolutist reading of the speech protection
 *   boundary kernel: the Brandenburg imminence standard, which limits the
 *   harm exception to speech that incites imminent lawless action and
 *   constitutionally forecloses broader restriction. Under this reading's own
 *   lights, the standing arrangement is a near-absolute protective regime
 *   whose primary structural cost is the aggregate, non-actionable harm borne
 *   by targeted minority communities and harassment targets — harm that
 *   exists as an externality precisely because the imminence line is drawn
 *   where it is. This is NOT a story about the harm-limited or balancing
 *   readings; those are separate constraints (harm_limited_reading,
 *   balancing_reading) sharing this kernel with different beneficiary/victim
 *   structures and different epsilon.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter administering and enforcing the imminence test
 *   - political_dissidents and controversial_speakers: primary beneficiaries whose advocacy is protected regardless of content
 *   - targeted_minority_communities and harassment_targets: bear cumulative dignitary and psychological harm with no doctrinal remedy
 *   - civil_liberties_organizations: co-administer the doctrine's boundaries through strategic litigation
 *   - comparative_legal_scholars: analytical observers documenting the U.S. standard's divergence from peer democracies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.42).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.28).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Near-Absolute Speech Protection with Brandenburg Imminence Exception").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '8653e644-c41e-4c0b-892a-51941ef871ed').
narrative_ontology:cs_kernel_codification('8653e644-c41e-4c0b-892a-51941ef871ed', fixed_text).
narrative_ontology:cs_authority_grounding('8653e644-c41e-4c0b-892a-51941ef871ed', lineage).
narrative_ontology:cs_interpretation_layer_present('8653e644-c41e-4c0b-892a-51941ef871ed').
narrative_ontology:cs_reading_relation('8653e644-c41e-4c0b-892a-51941ef871ed', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('8653e644-c41e-4c0b-892a-51941ef871ed', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('8653e644-c41e-4c0b-892a-51941ef871ed', foundational, government_suppression_is_primary_threat).
narrative_ontology:cs_axiom_status(government_suppression_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('8653e644-c41e-4c0b-892a-51941ef871ed', government_suppression_is_primary_threat, deontological).
narrative_ontology:cs_axiom('8653e644-c41e-4c0b-892a-51941ef871ed', foundational, bright_line_imminence_test_superior_to_case_by_case_weighing).
narrative_ontology:cs_axiom_status(bright_line_imminence_test_superior_to_case_by_case_weighing, holdable).
narrative_ontology:cs_axiom_grounding('8653e644-c41e-4c0b-892a-51941ef871ed', bright_line_imminence_test_superior_to_case_by_case_weighing, instrumental).
narrative_ontology:cs_reference_frame('8653e644-c41e-4c0b-892a-51941ef871ed', brandenburg_imminence_baseline).
narrative_ontology:cs_drift_state('8653e644-c41e-4c0b-892a-51941ef871ed', contemporary_online_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8653e644-c41e-4c0b-892a-51941ef871ed', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, controversial_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, press_and_publishers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targeted_minority_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, harassment_targets).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, content_neutrality_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_theory).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, government_distrust_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies and enforces the Brandenburg imminence test in every case where speech regulation is challenged, striking down laws that reach beyond incitement to imminent lawless action. Maintains the doctrine as settled precedent and treats departures from it as constitutionally suspect.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Can advocate unpopular, radical, or government-critical positions without fear of prosecution absent a showing of imminent incitement. Historically the doctrine's central beneficiaries — the standard was forged in cases involving unpopular political speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, mobile, national).

% Includes extremist organizers, provocateurs, and hate-group figures who can organize and speak publicly so long as their rhetoric stops short of directly inciting imminent violence. The standard's breadth protects them as a structural byproduct of protecting dissidents generally.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, controversial_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Rely on the high bar for speech restriction to publish investigative reporting, opinion, and unpopular commentary without risk of liability for downstream harms not rising to imminent incitement. Has institutional resources to litigate any erosion of the standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, press_and_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Litigate to preserve and extend the Brandenburg standard, framing any narrowing as the first step toward viewpoint-based censorship. Actively shapes doctrine through strategic litigation and amicus practice, functioning as an unofficial co-administrator of the standard's boundaries.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_organizations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__absolutist_reading, civil_liberties_organizations, agenda_setter).

% Bear the cumulative, dignitary, and psychological harm of racist, dehumanizing, or threatening speech that never crosses the imminence line and therefore cannot be regulated. Cannot exit the jurisdiction's speech regime and have no legal remedy under the doctrine as applied; harm accrues as aggregate exposure rather than any single actionable incident.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targeted_minority_communities, payer,
    powerless, biographical, trapped, national).

% Individuals subjected to sustained, targeted, non-imminent verbal harassment or degradation campaigns that fall outside the incitement exception. Local law enforcement and civil remedies are constrained by the same doctrinal ceiling; withdrawal from public or online spaces is often their only practical exit.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harassment_targets, payer,
    powerless, immediate, trapped, local).

% Would enact narrower speech regulations addressing group defamation, harassment, or dignitary harm but are foreclosed by the doctrine's near-categorical protection; their proposed statutes are struck down or chilled before enactment.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislatures_seeking_hate_speech_regulation, excluded,
    organized, biographical, constrained, national).

% Study how the U.S. absolutist standard diverges from peer democracies that regulate hate speech and harassment more aggressively, documenting the tradeoffs each regime accepts without advocating a resolution.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, comparative_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, viewpoint-neutral rule that prevents government officials — including hostile future majorities — from suppressing speech based on content or unpopularity, coordinating trust that today's dissident speech will not be punished by tomorrow's government.
% TRANSFER_FUNCTION: Moves the burden of unregulated harmful speech from would-be censors (who cannot act) and speakers (who face no liability short of incitement) onto those exposed to that speech — concentrating cumulative dignitary and psychological cost on minoritized and harassment-target populations who have no remedy under the standard.
% ABSENT_VOICES: Targeted minority communities and harassment targets are structurally present as payers but institutionally absent from doctrinal formation — courts adjudicate speaker rights against government power, with the diffuse harm to listeners treated as outside the cognizable injury the doctrine is built to weigh.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished, legislatures could immediately enact broader speech restrictions addressing group defamation, harassment, and hate speech; political and controversial speakers would face new liability exposure; civil liberties litigation practice built around defending the standard would lose its central doctrinal anchor. The rearrangement would be immediate and structural, not cosmetic.
% FOUNDING_PROBLEM: The doctrine was built to solve two founding problems: (1) government suppression of unpopular political speech (the Espionage Act prosecutions, the Red Scare, McCarthyism) using vague 'bad tendency' or 'clear and present danger' standards that swept in ordinary advocacy; and (2) the specific case (Brandenburg v. Ohio) reversing a Klan leader's conviction, replacing a loose incitement standard with a strict imminence requirement.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and much of the judiciary attest the founding problem — government suppression of dissident political speech — remains live, citing ongoing prosecutions of protesters and activists abroad and periodic domestic proposals to criminalize unpopular advocacy. Critical race theorists, comparative constitutional scholars, and international human rights bodies (outside the doctrine's beneficiary set) attest that the standard, forged to protect political dissidents, now also operationally shields organized hate speech and harassment campaigns that the founding cases never contemplated, and that this second function has no comparable corroborating defense from outside the free-speech advocacy community itself.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.42 at 2024) reflecting genuine coordination value (protecting dissident speech from state suppression) alongside a real, if diffuse, cost borne by those exposed to unregulated harmful speech. Suppression is comparatively low (0.28) because the doctrine does not suppress alternatives to itself so much as it forecloses regulatory alternatives — legislatures cannot easily route around it, but the doctrine itself does not require heavy enforcement machinery against dissenters; its suppressive force runs toward foreclosed legislation, not toward silencing individuals. Accessibility collapse is moderate (0.35): the doctrine has genuine rival readings actively contested in academic and political discourse, unlike a settled natural law. Resistance is elevated (0.55) because civil rights groups, critical race scholars, and comparative-law critics actively contest the standard's sufficiency.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary and civil liberties seats, this is coordination: a stable, predictable, viewpoint-neutral rule protecting political speech against a hostile state. From targeted minority communities and harassment targets, the same rule computes as extraction-adjacent: it forecloses any remedy for cumulative harm and treats their exposure as a constitutionally necessary cost of protecting speech generally. The engine should compute divergent seat classifications from these structural facts, not from any claim this story makes about which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Political dissidents, controversial speakers, press, and civil liberties organizations are declared beneficiaries because the doctrine's central function — protecting political and unpopular speech from suppression — accrues directly to them, and their exit options (mobile, arbitrage) reflect institutional and individual capacity to invoke and benefit from the doctrine. Targeted minority communities and harassment targets are declared victims because the same doctrinal boundary that protects speakers forecloses any legal remedy for the harm they absorb; their exit options are trapped, reflecting that withdrawal from public life is the only practical response available to them under this reading. The federal judiciary sits as agenda_setter, administering the boundary rather than collecting from it directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state suppression of dissident political speech) remains live by the judiciary and civil liberties seats' own account, which prevents this from being mislabeled pure extraction — there is a real, ongoing coordination function protecting political speech from a state that has historically abused vaguer standards to prosecute dissidents. But the doctrine's protective scope has migrated beyond its founding cases (a Klan leader's incitement conviction) to shield a wider category of harmful speech, and no corroborating source outside the beneficiary set affirms that this expanded shielding function is itself still solving a live problem — it is simply carried along by the same bright-line rule. This is the contested-status genealogy the R5 interview is designed to surface, not resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_line_naturalness_vs_construction,
    'Is the Brandenburg imminence threshold a principled, near-necessary boundary for protecting political speech from state overreach, or a constructed line that happens to also shield organized hate speech and harassment as an unintended structural byproduct?',
    'Comparative doctrinal analysis: examine whether peer democracies that reject the imminence-only standard show measurably worse outcomes for protecting dissident political speech from state suppression, controlling for other institutional differences (independent judiciary, federalism, etc.).',
    'If the imminence line is close to structurally necessary for protecting dissidents, the doctrine''s externalized harm is better read as an unavoidable coordination cost. If the line is a contingent historical artifact that could be redrawn without sacrificing dissident protection, the externalized harm looks more like avoidable extraction the current reading declines to address.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imminence_line_naturalness_vs_construction, conceptual, 'Whether the Brandenburg line is structurally necessary or a contingent construction with unaddressed externalities.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the absolutist, harm_limited, and balancing readings of the speech_protection_boundary kernel disagree — is it about what counts as ''harm,'' about who bears the burden of proof, or about the proper role of courts versus legislatures in drawing the line?',
    'Structural comparison across the three sibling constraint stories: compare each reading''s beneficiary/victim declarations and requires_active_enforcement postures to isolate whether the disagreement is definitional (what is harm), evidentiary (how is harm shown), or institutional (who decides).',
    'If the disagreement is primarily institutional (courts vs. legislatures), the readings may be more compatible than they appear, with courts able to defer to legislative harm-based findings under a modified imminence standard. If the disagreement is definitional (what counts as harm), the readings are likely genuinely irreconcilable within a single doctrinal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the structural site of disagreement among the kernel''s three readings.').

omega_variable(
    aggregate_harm_measurability,
    'Can the aggregate, cumulative dignitary and psychological harm borne by targeted minority communities under the absolutist standard be measured with sufficient rigor to compare against the coordination benefit of protecting dissident speech, or is this an incommensurability problem?',
    'Longitudinal social-psychological research on communities subject to sustained non-imminent hate speech exposure, compared against jurisdictions with narrower speech protections, controlling for other social conditions.',
    'If the harm is measurable and substantial relative to the coordination benefit, this strengthens the case that the current reading''s extractiveness score understates the real cost. If measurement proves intractable, the extractiveness figure here remains a considered estimate rather than an empirically anchored one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregate_harm_measurability, empirical, 'Whether the diffuse harm this reading treats as externality can be rigorously measured against the doctrine''s coordination benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement_basis(spee_tr_t1969, observed).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__absolutist_reading, theater_ratio, 1980, 0.07).
narrative_ontology:measurement_basis(spee_tr_t1980, observed).
narrative_ontology:measurement(spee_tr_t1995, speech_protection_boundary__absolutist_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement_basis(spee_tr_t1995, observed).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_boundary__absolutist_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement_basis(spee_tr_t2005, observed).
narrative_ontology:measurement(spee_tr_t2016, speech_protection_boundary__absolutist_reading, theater_ratio, 2016, 0.13).
narrative_ontology:measurement_basis(spee_tr_t2016, observed).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__absolutist_reading, theater_ratio, 2024, 0.15).
narrative_ontology:measurement_basis(spee_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.22).
narrative_ontology:measurement_basis(spee_be_t1969, observed).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__absolutist_reading, base_extractiveness, 1980, 0.26).
narrative_ontology:measurement_basis(spee_be_t1980, observed).
narrative_ontology:measurement(spee_be_t1995, speech_protection_boundary__absolutist_reading, base_extractiveness, 1995, 0.31).
narrative_ontology:measurement_basis(spee_be_t1995, observed).
narrative_ontology:measurement(spee_be_t2005, speech_protection_boundary__absolutist_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement_basis(spee_be_t2005, observed).
narrative_ontology:measurement(spee_be_t2016, speech_protection_boundary__absolutist_reading, base_extractiveness, 2016, 0.39).
narrative_ontology:measurement_basis(spee_be_t2016, observed).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__absolutist_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(spee_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__absolutist_reading, suppression_requirement, 1969, 0.18).
narrative_ontology:measurement_basis(spee_su_t1969, observed).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__absolutist_reading, suppression_requirement, 1980, 0.19).
narrative_ontology:measurement_basis(spee_su_t1980, observed).
narrative_ontology:measurement(spee_su_t1995, speech_protection_boundary__absolutist_reading, suppression_requirement, 1995, 0.21).
narrative_ontology:measurement_basis(spee_su_t1995, observed).
narrative_ontology:measurement(spee_su_t2005, speech_protection_boundary__absolutist_reading, suppression_requirement, 2005, 0.23).
narrative_ontology:measurement_basis(spee_su_t2005, observed).
narrative_ontology:measurement(spee_su_t2016, speech_protection_boundary__absolutist_reading, suppression_requirement, 2016, 0.26).
narrative_ontology:measurement_basis(spee_su_t2016, observed).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__absolutist_reading, suppression_requirement, 2024, 0.28).
narrative_ontology:measurement_basis(spee_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, balancing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the speech_protection_boundary kernel. absolutist_reading (this file) maximizes protected speech and treats aggregate community harm as an externality outside the doctrine's cognizable injury. harm_limited_reading conditions protection on absence of significant dignitary/equality harm, shifting the victim set toward speakers whose expression is newly restricted. balancing_reading replaces the bright-line rule with case-by-case weighing, producing a different (more variable, less predictable) extraction profile. Each carries its own epsilon and stakeholder structure; none is a measurement of the same constraint under a different observable — they are three structurally distinct constraints sharing one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
