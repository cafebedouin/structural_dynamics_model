% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Qiyas and Istihsan as Legitimate Extension of Divine Intent
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The Hanafi school of Islamic law, founded in 8th-century Kufa, developed
 *   the most expansive theory of analogical reasoning (qiyas) and juristic
 *   preference (istihsan) among the four Sunni schools. Its central claim:
 *   reason is a legitimate tool for extending divine intent to novel cases,
 *   not a human innovation that corrupts revelation. This constraint story
 *   captures the Hanafi reading of the contested kernel
 *   'jurisprudential_method_kernel' — the question of how Islamic law derives
 *   rulings beyond explicit text. The method coordinates a vast legal system
 *   across time and space but extracts legitimacy from textualist claimants
 *   whose exclusive-authenticity thesis it structurally undermines.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.55).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Qiyas and Istihsan as Legitimate Extension of Divine Intent").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'df39f823-8fe2-42e4-9232-f380c071b195').
narrative_ontology:cs_kernel_codification('df39f823-8fe2-42e4-9232-f380c071b195', distributed).
narrative_ontology:cs_authority_grounding('df39f823-8fe2-42e4-9232-f380c071b195', practice).
narrative_ontology:cs_interpretation_layer_present('df39f823-8fe2-42e4-9232-f380c071b195').
narrative_ontology:cs_reading_relation('df39f823-8fe2-42e4-9232-f380c071b195', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_reading_relation('df39f823-8fe2-42e4-9232-f380c071b195', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('df39f823-8fe2-42e4-9232-f380c071b195', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_axiom('df39f823-8fe2-42e4-9232-f380c071b195', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('df39f823-8fe2-42e4-9232-f380c071b195', reason_extends_divine_intent, deontological).
narrative_ontology:cs_axiom('df39f823-8fe2-42e4-9232-f380c071b195', secondary, istihsan_validates_equitable_outcome).
narrative_ontology:cs_axiom_status(istihsan_validates_equitable_outcome, holdable).
narrative_ontology:cs_axiom_grounding('df39f823-8fe2-42e4-9232-f380c071b195', istihsan_validates_equitable_outcome, instrumental).
narrative_ontology:cs_reference_frame('df39f823-8fe2-42e4-9232-f380c071b195', companion_era_ijtihad).
narrative_ontology:cs_drift_state('df39f823-8fe2-42e4-9232-f380c071b195', classical_mature_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df39f823-8fe2-42e4-9232-f380c071b195', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_guidance).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_guidance).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, reason_as_divine_extension).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, istihsan_as_equitable_correction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, teach, and administer the Hanafi methodological framework across the Islamic world. Their professional identity and institutional authority are constituted through mastery of qiyas and istihsan. Exit would mean abandoning the school that defines their scholarly lineage and institutional position.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, beneficiary).

% Scholars trained in the rationalist disciplines (kalam, falsafa, usul al-fiqh) who find in the Hanafi method a legitimate home for their intellectual tools. They gain professional recognition and interpretive authority through the school's validation of reason. Exit to another school would require retraining and loss of methodological coherence.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_scholars, beneficiary,
    organized, biographical, constrained, global).

% Scholars and communities (especially Hanbali-aligned) who claim exclusive authenticity through literal adherence to Qur'an/Hadith text. The Hanafi method's expansive use of reason structurally undermines their claim that textual fidelity alone constitutes legitimate law. They bear the cost of having their authenticity claim contested in every novel case where analogy extends beyond explicit text.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_claimants, payer,
    organized, generational, constrained, global).

% Ordinary Muslims who rely on Hanafi fatwas for practical guidance in novel situations (commercial contracts, new technologies, social changes). They benefit from having systematic answers but pay through reduced transparency — they cannot easily verify whether a ruling truly traces to divine intent or reflects juristic preference. Can switch schools relatively easily in many contexts.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_guidance, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_guidance, payer).

% Maliki, Shafi'i, and Hanbali institutional authorities whose methodological frameworks compete for adherence. They are excluded from the Hanafi internal conversation but must respond to its claims in the marketplace of scholarly authority. Their exit is trapped — they cannot abandon their own schools without institutional collapse.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, competing_school_authorities, excluded,
    institutional, generational, trapped, global).

% Modern scholar of Islamic legal history analyzing the Hanafi method's structural operation across its formative and mature periods. Sees the full coordination-extraction dynamic without being subject to it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jurisprudential_method_kernel__hanafi_reading, historical_analyst).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic, reason-based methodology for deriving Islamic legal rulings on novel cases not explicitly addressed in Qur'an/Hadith, enabling the law to function across changing commercial, technological, and social conditions.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy from textual literalism to trained juristic reason. In novel cases, the transfer flows from textualist claimants (who lose exclusive authenticity) to Hanafi jurists (who gain authority to declare what divine intent requires in new circumstances).
% ABSENT_VOICES: Early proto-Hanbali traditionalists (e.g., Ahmad ibn Hanbal's circle) who rejected qiyas entirely as bid'ah — they were politically suppressed during the Mihna and later marginalized in Abbasid judicial appointments. Also excluded: non-Sunni legal traditions (Shi'i usul, Ibadi fiqh) that developed entirely different methodological frameworks.
% DISAPPEARANCE_RATIONALE: If the Hanafi method vanished overnight, the vast body of Hanafi-derived law governing commercial transactions, personal status, and state administration across the Ottoman, Mughal, and post-colonial worlds would lose its methodological foundation. Courts would need to adopt alternative schools' methodologies or develop new ones — a massive rearrangement of legal practice affecting millions.
% FOUNDING_PROBLEM: Early Islamic expansion created novel legal situations (new lands, conquered peoples, complex commercial instruments, administrative structures) with no explicit textual guidance. The companions' opinions were insufficiently systematic. A method was needed to extend divine law to unprecedented cases without claiming new revelation.
% FOUNDING_PROBLEM_CORROBORATION: Classical usul al-fiqh texts across schools (including al-Shafi'i's Risala and al-Ghazali's Mustasfa) attest that the proliferation of novel cases drove methodological development. Modern historians (Schacht, Calder, Hallaq) corroborate from outside the tradition that early Islamic state-building created unprecedented legal demands. Hanafi jurists claim the problem remains live (new cases always arise); Hanbali critics claim the founding problem was a pretext for uncontrolled rationalist expansion.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the method's legitimacy gain in novel cases comes at the direct expense of textualist exclusivity — every analogical extension is a transfer of authority from text to jurist reason. Suppression (0.55) is moderate: the method doesn't forcibly silence textualists (they persist as a rival school) but it occupies the institutional high ground (Abbasid and Ottoman judiciaries) making textualist alternatives structurally disadvantaged. Theater ratio (0.22) is low-moderate: the coordination function (systematic law for novel cases) is genuine and substantial; the performative element grows over centuries as the school's dominance becomes self-reinforcing. Accessibility collapse (0.35) is low: alternative schools remain live options. Resistance (0.48) is moderate: textualist critique persists but never dislodges the method's institutional embeddedness.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanafi jurist seat, the method is genuine coordination solving a real problem (novel cases need answers). From the textualist seat, the same structure is extraction masquerading as coordination — the 'novel case' category expands endlessly to justify juristic authority. The engine computes this divergence from the structural data: identity_locked agenda_setters vs. constrained payers with competing institutional bases.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists are agenda_setters (they define the method) and beneficiaries (they collect interpretive authority). Their identity_locked exit reflects professional constitution through the school. Rationalist-trained scholars are beneficiaries with constrained exit — they invest in the method's rationalist validation. Textualist claimants are payers: their authenticity claim is the extraction target. Lay Muslims are dual: beneficiaries of systematic guidance, payers of opaque authority. Competing schools are excluded — trapped in their own institutional commitments. The analytical observer sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (novel cases from imperial expansion) was real and live for centuries. By the late Abbasid period, the method had become self-justifying: the school's very dominance created the 'novel cases' it claimed to solve (through its own previous rulings). The mandate (extend divine intent to new situations) atrophied into a mechanism for reproducing juristic authority. The constraint persists not because the founding problem demands it, but because the institutional ecosystem (madrasas, judiciaries, waqfs) is built on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Hanafi method a distinct constraint instantiated from the jurisprudential_method_kernel, or merely a variant interpretation of a single constraint?',
    'Test ε-invariance: if measuring extractiveness via ''novel commercial cases'' yields high ε but via ''ritual purity cases'' yields low ε, the kernel contains multiple constraints. The ε-invariance principle requires separate stories for each stable ε.',
    'If ε varies by case domain, the kernel decomposes into multiple constraints (e.g., hanafi_commercial_qiyas, hanafi_ritual_qiyas) each with its own classification. This story would then represent only one slice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates one ε-invariant constraint or masks domain-dependent variation').

omega_variable(
    extraction_boundary_novel_cases,
    'Does the high extractiveness on novel cases reflect genuine coordination necessity (no textual guidance exists) or manufactured novelty (jurists expand ''novel'' to capture authority)?',
    'Historical analysis of case classification: track what percentage of Hanafi qiyas rulings address genuinely unprecedented situations vs. cases where textual guidance exists but is analogically extended anyway.',
    'If manufactured novelty dominates, the constraint trends toward snare (coordination as cover). If genuine novelty dominates, tangled_rope holds — coordination function is real, extraction is its price.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary_novel_cases, empirical, 'Whether the coordination-extraction boundary in novel cases is structurally necessary or strategically expanded').

omega_variable(
    textualist_victimhood_nature,
    'Are textualist claimants genuine victims of extraction, or do they occupy a rival coordination niche that the Hanafi method merely competes with?',
    'Compare institutional outcomes: do textualist communities (Hanbali-dominated regions) show worse legal functionality for novel cases, or merely different functionality? If worse, extraction is real; if different, competition not extraction.',
    'If textualists are merely rival coordinators, the victim designation is contested and the constraint may be rope (competing coordination systems). If they are structurally disadvantaged in handling novelty, victimhood stands and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_victimhood_nature, empirical, 'Whether textualist claimants bear asymmetric costs or simply lose a coordination competition').

omega_variable(
    istihsan_as_coordination_or_extraction,
    'Is istihsan (juristic preference) a genuine equity correction that coordinates fair outcomes, or a discretionary override that enables extraction?',
    'Analyze istihsan invocations: track whether they systematically favor particular social classes (merchants, rulers, landowners) or distribute equitably. Cross-reference with historical outcomes for affected parties.',
    'If istihsan systematically favors powerful parties, it is an extraction mechanism within the coordination structure — strengthening the snare interpretation. If distribution is equitable, it supports the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(istihsan_as_coordination_or_extraction, empirical, 'Whether the equity override mechanism serves justice or power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 150, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t150, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t300, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 300, 0.12).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t450, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 450, 0.16).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t600, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 600, 0.19).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t750, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 750, 0.21).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t900, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 900, 0.22).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t1050, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1050, 0.22).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_tr_t1200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1200, 0.22).

% Extraction over time
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t150, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t300, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 300, 0.48).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t450, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 450, 0.58).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t600, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t750, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 750, 0.65).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t900, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 900, 0.67).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t1050, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1050, 0.68).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_be_t1200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1200, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t150, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 150, 0.25).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t300, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 300, 0.38).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t450, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 450, 0.45).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t600, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 600, 0.5).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t750, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 750, 0.52).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t900, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 900, 0.54).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t1050, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1050, 0.55).
narrative_ontology:measurement(jurisprudential_method_kernel__hanafi_reading_su_t1200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1200, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.1).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, ottoman_judicial_institutionalization).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, mughal_legal_codification).

% DUAL FORMULATION NOTE:
% This constraint is the hanafi_reading of the jurisprudential_method_kernel. It decomposes the kernel's methodological contest into four ε-invariant constraints (hanafi, maliki, shafii, hanbali readings), each with distinct beneficiary/victim structures and extractiveness profiles. The Hanafi reading has the highest ε on novel cases due to its expansive qiyas and istihsan; the Hanbali reading has near-zero ε on novel cases (refuses them) but high suppression of rationalist methods; the Maliki and Shafi'i readings occupy intermediate positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, institutional, 0.15).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
