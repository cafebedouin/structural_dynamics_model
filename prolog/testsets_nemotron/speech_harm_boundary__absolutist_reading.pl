% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection with Narrow Harm Override
 *   domain: constitutional/political/communication
 *
 * SUMMARY:
 *   The absolutist reading of the speech-harm boundary kernel instantiates a
 *   constraint that protects speaker autonomy near-absolutely, recognizing
 *   only four narrow unprotected categories (incitement per Brandenburg, true
 *   threats, defamation per Sullivan, obscenity per Miller). The harm
 *   override threshold is extremely high — speech must be directed to
 *   imminent lawless action, constitute a true threat, meet actual malice for
 *   public figures, or satisfy the Miller test. This constraint transfers the
 *   cost of speech harms from speakers to targets: targeted communities bear
 *   hate speech and group defamation; vulnerable speakers bear weaponized
 *   counter-speech and SLAPP suits; reputation holders bear defamation with
 *   minimal recourse. The claimed coordination function is speaker certainty
 *   and prevention of state censorship; the actual operation extracts from
 *   the powerless to subsidize the powerful. The constraint is claimed as
 *   mountain (natural right) by its beneficiaries but operates as snare
 *   (active enforcement of narrow categories, structural harm
 *   externalization, excluded alternatives).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.88).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.12).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, snare).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Speech Protection with Narrow Harm Override").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional/political/communication").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '2ded0d9f-88de-4fe7-add1-9fa051816fbd').
narrative_ontology:cs_kernel_codification('2ded0d9f-88de-4fe7-add1-9fa051816fbd', fixed_text).
narrative_ontology:cs_authority_grounding('2ded0d9f-88de-4fe7-add1-9fa051816fbd', lineage).
narrative_ontology:cs_interpretation_layer_present('2ded0d9f-88de-4fe7-add1-9fa051816fbd').
narrative_ontology:cs_reading_relation('2ded0d9f-88de-4fe7-add1-9fa051816fbd', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ded0d9f-88de-4fe7-add1-9fa051816fbd', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('2ded0d9f-88de-4fe7-add1-9fa051816fbd', foundational, speaker_autonomy_primacy).
narrative_ontology:cs_axiom_status(speaker_autonomy_primacy, holdable).
narrative_ontology:cs_axiom_grounding('2ded0d9f-88de-4fe7-add1-9fa051816fbd', speaker_autonomy_primacy, deontological).
narrative_ontology:cs_axiom('2ded0d9f-88de-4fe7-add1-9fa051816fbd', secondary, state_competence_skepticism).
narrative_ontology:cs_axiom_status(state_competence_skepticism, holdable).
narrative_ontology:cs_axiom_grounding('2ded0d9f-88de-4fe7-add1-9fa051816fbd', state_competence_skepticism, empirically_contingent).
narrative_ontology:cs_reference_frame('2ded0d9f-88de-4fe7-add1-9fa051816fbd', classical_liberal_speech_protection).
narrative_ontology:cs_drift_state('2ded0d9f-88de-4fe7-add1-9fa051816fbd', contemporary_digital_amplification_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2ded0d9f-88de-4fe7-add1-9fa051816fbd', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targeted_communities).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, vulnerable_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, institutional_reputation_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, civil_liberties_organizations).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, speaker_autonomy_primacy).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, content_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy near-absolute protection to speak without state interference; the narrow unprotected categories (incitement, true threats, defamation, obscenity) are precisely defined and rarely applied. Exit from the constraint is not needed — speakers benefit from the arrangement. The harm their speech may cause to others is externalized; they do not bear the cost of the constraint's narrow override threshold.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers, beneficiary,
    organized, biographical, mobile, national).

% Bear the concentrated harm of protected hate speech, harassment, and group defamation that falls below the high override threshold. Cannot exit the speech environment — identity makes them perpetual targets. Legal remedies are structurally foreclosed by the constraint's design; the harm is the price of others' absolute protection. No countervailing coordination benefit accrues to them from the arrangement.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targeted_communities, payer,
    powerless, generational, trapped, national).

% Experience chilling effects when absolutist protection is weaponized by powerful actors (SLAPP suits, platform amplification of counter-speech, doxxing). The constraint's logic treats their self-censorship as a choice, not a structural outcome. Exit options are constrained — they need public discourse but face asymmetric retaliation. Some benefit from the same absolute protection when they speak, but the net flow is extractive for this seat.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, vulnerable_speakers, payer,
    moderate, biographical, constrained, national).

% Bear concentrated defamation and reputational harm with narrow legal recourse (actual malice standard for public figures). The constraint extracts from their interest in reputation integrity to subsidize speaker autonomy. They have resources to litigate but face a structurally tilted playing field; exit from public discourse is not viable for their role. Net payers despite high power.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, institutional_reputation_holders, payer,
    powerful, biographical, constrained, national).

% Administer the boundary through doctrinal tests (Brandenburg imminence, true threats objectivity, actual malice). The narrow unprotected categories are judicial creations maintained by precedent. Courts could widen the override threshold but face institutional pressure from the constraint's own logic — expanding unprotected speech is coded as betraying the First Amendment. They are the constraint's maintenance crew, not its beneficiary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Build institutional identity and fundraising around defending the absolutist boundary. The constraint's narrow override threshold is their product — they litigate to maintain it, file amicus briefs, shape public discourse. They benefit professionally and materially from the arrangement's persistence. Exit is arbitrage-grade: they could pivot to balancing frameworks but would lose their distinctive market position.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, civil_liberties_organizations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, civil_liberties_organizations, agenda_setter).

% Argue that personhood-denying speech should be categorically unprotected (dignity_reading). Their position is structurally excluded from the constraint's operative logic — the absolutist framework treats dignity-based restrictions as viewpoint discrimination. They participate in academic and international discourse but have no pathway to influence domestic constitutional doctrine under the current arrangement.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, dignity_advocates, excluded,
    moderate, generational, constrained, national).

% Advocate proportionality balancing (harm_balancing_reading) — speech protection presumptive but yielding to demonstrated harm. Their framework dominates comparative constitutional law (Canada, Germany, ECHR) but is treated as foreign import in U.S. doctrine. They are excluded from the constraint's internal logic but not from the broader conversation; exit to comparative forums is mobile.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, balancing_scholars, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable speech environment where speakers need not anticipate shifting state judgment about the value or harm of their expression. Solves the coordination problem of "who decides what speech is too harmful" by answering: almost never the state, almost always the speaker.
% TRANSFER_FUNCTION: Transfers the cost of speech harms from speakers to targets. Speakers externalize the harms their expression causes (psychological injury, reputational damage, group subordination, democratic erosion); targeted communities, vulnerable speakers, and reputation holders absorb those costs without compensation. The constraint moves harm downstream to the least powerful parties.
% ABSENT_VOICES: Dignity advocates (argue for personhood-denying speech as categorically unprotected) and balancing scholars (argue for proportionality) are structurally excluded from the constraint's operative logic. Dignity advocates are excluded because the framework treats their position as viewpoint discrimination; balancing scholars are excluded because their comparative-law framework is treated as inapplicable to U.S. constitutional structure. Both are present in academic and international discourse but have no seat at the doctrinal table.
% DISAPPEARANCE_RATIONALE: If the absolutist boundary vanished overnight, hate speech and group defamation laws would likely be enacted within years (as in every other liberal democracy), defamation standards would revert to negligence, platforms would face liability for algorithmic amplification of harm, and the cost of speech would be internalized to speakers. The speech ecosystem would reorganize around harm internalization rather than harm externalization.
% FOUNDING_PROBLEM: The constraint was built to solve the problem of state censorship — the historical pattern of governments suppressing dissent, minority viewpoints, and unpopular speech under the guise of protecting public order, morality, or national security. The founding generation (Holmes, Brandeis, later Brennan) experienced direct state suppression of anti-war, labor, and civil rights speech and constructed the near-absolute boundary as a structural guarantee against that recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and originalist scholars attest the founding problem is live — state censorship pressure persists in new forms (platform pressure, national security leaks, protest suppression). Dignity advocates, comparative constitutional scholars, and critical race theorists attest the founding problem is substantially solved — the state no longer directly suppresses core political speech, and the constraint now primarily protects powerful speakers from accountability for harm to the powerless. No consensus exists; the dispute is structural.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.88) because the constraint structurally transfers harm costs from speakers to targets with no compensation mechanism — the narrower the unprotected categories, the more harm is externalized. Suppression is low (0.12) because the constraint does not suppress speakers; it suppresses legal remedies for targets. Theater ratio is near-zero (0.05) — the doctrinal tests are real, actively litigated, and functionally determinative; there is no performative gap. Accessibility collapse is low (0.15) — alternatives (balancing, dignity frameworks) exist and operate in comparative jurisdictions; the constraint does not foreclose imagining them. Resistance is high (0.72) — sustained academic, international, and movement pressure challenges the boundary; the constraint persists despite active contestation, not because of consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the speaker/civil-liberties seat, the constraint is a genuine mountain — a natural right that prevents state tyranny. From the targeted-community seat, it is a snare — an enforced structure that extracts their dignity and safety for others' autonomy. From the court seat, it is a tangled rope — a coordination mechanism (predictable doctrine) with asymmetric extraction (harm externalization) requiring active doctrinal maintenance. The engine computes these per-seat types from the structural data; the single claimed_type (snare) reflects the authoring seat's structural judgment that extraction dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and civil liberties organizations are beneficiaries (d near 0.0) — they collect the subsidy of externalized harm costs and institutional identity. Targeted communities are trapped payers (d near 1.0) — identity-locked into harm absorption with no exit. Vulnerable speakers and reputation holders are constrained payers (d ~0.7) — they have some power but face structurally tilted fields. Courts are analytical agenda-setters (d ~0.5) — they maintain the machinery but gain no net subsidy. Dignity advocates and balancing scholars are excluded (no directionality — they are not in the constraint's operative field).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state censorship) is contested — partially solved (direct state suppression of core political speech has receded) but the constraint has expanded far beyond that solution. The constraint now protects commercial speech, hate speech, and algorithmic amplification — none of which were the founding concern. The arrangement persists because its beneficiaries (speakers, civil liberties orgs) have institutionalized the founding problem as permanently live, preventing sunset. This is mandatrophy: the constraint's mandate has outlived its function, but the constraint remains because the beneficiaries capture the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_externalization_measurement,
    'Can the aggregate harm transferred to targets under the absolutist boundary be quantified, and does it exceed the coordination benefit of speaker certainty?',
    'Longitudinal study of hate speech harm metrics (psychological, social, democratic) in absolutist vs. balancing jurisdictions, controlling for cultural variables. Platform-level natural experiments (policy changes on harassment, hate speech) provide quasi-experimental data.',
    'If quantified harm substantially exceeds coordination benefit, the constraint''s claimed coordination function is falsified — it operates as a pure transfer mechanism. If coordination benefit exceeds harm, the snare classification is contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_externalization_measurement, empirical, 'Whether the constraint''s net social welfare is positive or negative.').

omega_variable(
    absolutist_boundary_naturalness,
    'Is the narrow unprotected category set (incitement, true threats, defamation, obscenity) a genuine structural feature of speech regulation, or a constructed boundary that serves speaker-autonomy ideology?',
    'Comparative constitutional analysis: if every other liberal democracy has wider unprotected categories (hate speech, group defamation, dignity violations) and functions without collapsing into censorship, the U.S. boundary is constructed, not structural. Historical analysis of the categories'' doctrinal evolution — each was contested and narrowed over time.',
    'If the boundary is constructed and ideologically motivated, the constraint is a false summit candidate (mountain claim masking extraction). If structurally necessary, the high extraction may be the price of genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_boundary_naturalness, conceptual, 'Whether the absolutist boundary is a natural law of speech regulation or an ideological construction.').

omega_variable(
    kernel_reading_absolutist,
    'How does the absolutist reading of the speech_harm_boundary kernel structurally relate to its sibling readings (harm_balancing_reading, dignity_reading)?',
    'Track doctrinal uptake: if U.S. courts cite comparative balancing frameworks as persuasive, the absolutist reading''s structural dominance erodes (influences). If a constitutional amendment or Supreme Court super-precedent enshrines balancing, the absolutist reading is foreclosed. If state-level hate speech laws survive strict scrutiny, coexistence is structurally confirmed.',
    'The reading_relations in cs_structure declare this reading coexists_with both siblings. If empirical uptake shows foreclosure or influence, the kernel''s structural dynamics shift — the constraint family''s classification may change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_absolutist, conceptual, 'Structural relationship between this kernel reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1919, speech_harm_boundary__absolutist_reading, theater_ratio, 1919, 0.02).
narrative_ontology:measurement(spee_tr_t1940, speech_harm_boundary__absolutist_reading, theater_ratio, 1940, 0.03).
narrative_ontology:measurement(spee_tr_t1969, speech_harm_boundary__absolutist_reading, theater_ratio, 1969, 0.04).
narrative_ontology:measurement(spee_tr_t1990, speech_harm_boundary__absolutist_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(spee_tr_t2010, speech_harm_boundary__absolutist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__absolutist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t1919, speech_harm_boundary__absolutist_reading, base_extractiveness, 1919, 0.25).
narrative_ontology:measurement(spee_be_t1940, speech_harm_boundary__absolutist_reading, base_extractiveness, 1940, 0.35).
narrative_ontology:measurement(spee_be_t1969, speech_harm_boundary__absolutist_reading, base_extractiveness, 1969, 0.65).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__absolutist_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__absolutist_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__absolutist_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1919, speech_harm_boundary__absolutist_reading, suppression_requirement, 1919, 0.65).
narrative_ontology:measurement(spee_su_t1940, speech_harm_boundary__absolutist_reading, suppression_requirement, 1940, 0.45).
narrative_ontology:measurement(spee_su_t1969, speech_harm_boundary__absolutist_reading, suppression_requirement, 1969, 0.25).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__absolutist_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__absolutist_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__absolutist_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the speech_harm_boundary kernel family. The three readings decompose the natural-language concept 'free speech protection' into structurally distinct constraints with different ε values, different victim sets, and different coordination functions. The absolutist reading has high ε (0.88) because its narrow override threshold externalizes maximum harm; the balancing reading has moderate ε (estimated ~0.45) because proportionality internalizes some harm; the dignity reading has low ε (estimated ~0.25) because categorical bans on dignity-violating speech internalize harm at the source. They are linked via affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, powerful, 0.7).
constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
