% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: First Amendment Balancing Test (Judicial Weighing Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the balancing reading of the
 *   speech_protection_boundary kernel: courts determine whether speech is
 *   protected by weighing First Amendment interests against other
 *   constitutional values and demonstrated harms on a case-by-case basis.
 *   Unlike the absolutist reading (near-categorical protection) or the
 *   harm-limited reading (protection conditional on absence of dignitary
 *   harm), this reading distributes the gatekeeper role across the judiciary
 *   and produces a shifting, context-dependent boundary. It is claimed as a
 *   necessary coordination mechanism for pluralistic self-governance, but it
 *   concentrates interpretive power in courts and regulatory latitude in the
 *   state while imposing uncertainty costs on speakers.
 *
 * KEY AGENTS:
 *   - Judiciary (agenda_setter/secondary beneficiary): institutional power, analytical exit â sets and administers balancing tests
 *   - Government regulators (beneficiary): institutional power, constrained exit â gain latitude to restrict speech under judicial balancing
 *   - Speakers (payer): moderate power, constrained exit â bear chilling effects and legal uncertainty
 *   - Absolutist advocates (excluded): organized power, constrained exit â structurally marginalized proponents of categorical rules
 *   - Constitutional scholars (observer): analytical power, analytical exit â external analytical seat documenting drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.55).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.45).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "First Amendment Balancing Test (Judicial Weighing Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '9ef73640-add2-4a75-b094-44a69955cfe8').
narrative_ontology:cs_kernel_codification('9ef73640-add2-4a75-b094-44a69955cfe8', formalized).
narrative_ontology:cs_authority_grounding('9ef73640-add2-4a75-b094-44a69955cfe8', lineage).
narrative_ontology:cs_interpretation_layer_present('9ef73640-add2-4a75-b094-44a69955cfe8').
narrative_ontology:cs_reading_relation('9ef73640-add2-4a75-b094-44a69955cfe8', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ef73640-add2-4a75-b094-44a69955cfe8', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('9ef73640-add2-4a75-b094-44a69955cfe8', foundational, contextual_protection).
narrative_ontology:cs_axiom_status(contextual_protection, holdable).
narrative_ontology:cs_axiom_grounding('9ef73640-add2-4a75-b094-44a69955cfe8', contextual_protection, conventional).
narrative_ontology:cs_axiom('9ef73640-add2-4a75-b094-44a69955cfe8', foundational, judicial_primacy_in_speech_boundary).
narrative_ontology:cs_axiom_status(judicial_primacy_in_speech_boundary, holdable).
narrative_ontology:cs_axiom_grounding('9ef73640-add2-4a75-b094-44a69955cfe8', judicial_primacy_in_speech_boundary, conventional).
narrative_ontology:cs_reference_frame('9ef73640-add2-4a75-b094-44a69955cfe8', constitutional_pluralism).
narrative_ontology:cs_drift_state('9ef73640-add2-4a75-b094-44a69955cfe8', contemporary_culture_wars_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ef73640-add2-4a75-b094-44a69955cfe8', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, government_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the balancing tests that determine whether speech is protected, weighing First Amendment interests against competing constitutional values and demonstrated harms on a case-by-case basis. Retains expansive gatekeeping authority over the speech boundary; its institutional centrality grows with each contested application.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, judiciary, beneficiary).

% Benefit from judicial latitude that permits the regulation of speech when courts find that competing constitutional values or demonstrated harms outweigh First Amendment interests. The balancing framework supplies a doctrinal defense for statutory restrictions that would fail under categorical protection regimes.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, government_regulators, beneficiary,
    institutional, generational, constrained, national).

% Bear the costs of legal uncertainty and chilling effects. Because the protected-unprotected boundary shifts with factual context, speakers cannot rely on bright-line rules and must anticipate how courts will weigh their expression against countervailing values, frequently self-censoring to avoid liability or state attention.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers, payer,
    moderate, biographical, constrained, national).

% Argue for near-absolute speech protection and categorical per se rules. Their position is structurally marginalized within the balancing framework; they appear in dissenting opinions and academic critique but do not control the operative doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_advocates, excluded,
    organized, generational, constrained, national).

% Analyze and critique the balancing framework from outside the bench and active bar. They document doctrinal drift, empirically estimate chilling effects, and situate the balancing reading alongside its sibling absolutist and harm-limited readings within the broader constitutional kernel.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves conflicts between expressive liberty and competing constitutional values (equality, dignity, privacy, national security) in a pluralistic society by replacing categorical rules with a contextual, case-by-case weighing method.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed constitutional text and categorical rules to the judiciary; moves regulatory latitude to the state; moves the costs of legal uncertainty and chilling effects to speakers.
% ABSENT_VOICES: Absolutist advocates who would argue for near-categorical protection and bright-line speech rules are structurally excluded from controlling the doctrine; resource-poor speakers who cannot litigate through multi-tier balancing frameworks are practically absent.
% DISAPPEARANCE_RATIONALE: If the balancing framework vanished overnight, courts would revert to categorical rules or per se tests, stripping the judiciary of contextual gatekeeping power, eliminating the state's latitude to regulate in the name of competing values, and removing the chilling uncertainty that currently hovers over controversial speech.
% FOUNDING_PROBLEM: How to protect expressive liberty while permitting the state to address genuine harms (defamation, incitement, privacy violations, equality harms) in a complex industrial and digital society where categorical rules either over-protect harmful speech or under-protect legitimate regulatory interests.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside the judiciary attest that categorical rules proved unworkable in mid-twentieth-century industrial society. Critics from the absolutist reading and civil libertarian seats counter that the balancing framework itself generates more chilling harm than the categorical rules it replaced; legislative hearing testimony and amicus briefs from non-beneficiary groups document both positions.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate-high because the framework transfers substantial interpretive authority to the judiciary and regulatory latitude to the state, while speakers pay in uncertainty and self-censorship. Suppression (0.45) is moderate: the constraint does not directly suppress but enables state suppression through judicial approval, and its persistence depends on active judicial enforcement. Theater ratio (0.32) reflects growing formulae in balancing tests that perform neutrality while encoding preference. Accessibility collapse (0.60) captures the marginalization of categorical alternatives once the balancing framework is accepted. Resistance (0.50) reflects ongoing critique from absolutists, dissident speakers, and libertarian legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the balancing framework is necessary coordination â a method to manage irreducible conflict between constitutional values without the brutality of categorical rules. From the speaker's seat, the same structure operates as open-ended extraction: the boundary shifts with the composition of the bench, the costs of litigation are prohibitive, and the ex ante uncertainty chills expression before it reaches a courtroom. The engine computes this divergence from the structural data: agenda_setter + beneficiary + analytical exit yields low directionality and damped effective extraction; payer + victim + constrained exit yields high directionality and amplified effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary and government regulators are declared beneficiaries, deriving low directionality (near the full-beneficiary end) because the constraint subsidizes their authority and regulatory scope. Speakers are declared victims, deriving high directionality (near the full-target end) because the constraint extracts from them through uncertainty and chilling effects. Absolutist advocates are excluded rather than victimized by the active extraction mechanism; their structural role is absence from the gatekeeping conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The balancing framework was built to solve the failure of categorical speech rules in a complex society. That founding problem remains live and contested â new communication technologies continually test bright-line rules. The R5 fields record this live status, preventing premature classification as a piton (atrophied performance) or snare (pure extraction). The constraint avoids mandatrophy mislabeling because the coordination function â managing pluralistic conflict over speech â is still genuinely performed, even though the metrics reveal substantial extractive overhead. A dead founding problem plus high theater would signal piton; a live founding problem plus asymmetric extraction signals tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_incommensurability,
    'Does the speech protection boundary constitute a single constraint with contested interpretations, or three distinct constraints (absolutist, balancing, harm-limited) that share a constitutional kernel but instantiate different epsilon values, beneficiary-victim structures, and gatekeeper roles?',
    'Corpus-level comparison of epsilon, directionality profiles, and stakeholder structures across the three sibling readings; if epsilon and directionality diverge significantly, the readings are distinct constraints under the epsilon-invariance principle.',
    'If the readings instantiate distinct constraints, the balancing reading''s classification as tangled_rope does not generalize to the kernel itself; the kernel is a constraint family requiring separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three readings are one constraint or a constraint family').

omega_variable(
    balancing_neutrality,
    'Is judicial balancing a neutral method of constitutional interpretation, or does it systematically encode majoritarian or elite preferences under the guise of neutral weighing?',
    'Empirical analysis of balancing outcomes across speaker types (powerful versus powerless, mainstream versus dissident) and issue areas to detect systematic asymmetry in how competing constitutional values are weighted.',
    'If systematic bias is found, the coordination function is partly cover for extraction, pushing the constraint toward snare; if outcomes are unbiased, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_neutrality, empirical, 'Empirical neutrality of judicial balancing').

omega_variable(
    chilling_effect_magnitude,
    'How large is the chilling effect on speakers under a balancing regime compared to a categorical protection regime?',
    'Comparative empirical studies of speaker behavior and self-censorship rates in jurisdictions with balancing tests versus jurisdictions with near-categorical protection, controlling for political culture and legal tradition.',
    'A large chilling effect would increase speakers'' directionality toward full target, raising effective extraction; a negligible effect would support the coordination story and lower the seat-specific extraction computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Magnitude of speech chilling under balancing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spb_bal_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spb_bal_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(spb_bal_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(spb_bal_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(spb_bal_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(spb_bal_tr_t50, speech_protection_boundary__balancing_reading, theater_ratio, 50, 0.32).

% Extraction over time
narrative_ontology:measurement(spb_bal_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spb_bal_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(spb_bal_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(spb_bal_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(spb_bal_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(spb_bal_be_t50, speech_protection_boundary__balancing_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(spb_bal_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spb_bal_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(spb_bal_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(spb_bal_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(spb_bal_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(spb_bal_su_t50, speech_protection_boundary__balancing_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_boundary kernel. The three readings (absolutist, balancing, harm-limited) instantiate structurally distinct constraints with different epsilon values, beneficiary structures, and gatekeeper roles. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
