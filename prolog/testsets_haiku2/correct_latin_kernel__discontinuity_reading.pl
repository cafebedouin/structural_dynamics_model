% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Discontinuity Reading of the Classical Latin Kernel
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   The discontinuity reading of the Classical Latin kernel treats Medieval
 *   Latin as structurally separate from Classical Latin — a distinct system
 *   arising from late-antique disruption, not organic evolution. Under this
 *   reading, reconstruction of 'correct' Latin requires symbolic reoccupation
 *   of Classical forms from corrupted medieval texts. This reading generates
 *   a tangled coordination/extraction structure: it coordinates scholarly
 *   consensus around a shared standard (the Classical system as the
 *   referent), but does so by suppressing alternative readings (the
 *   continuity hypothesis) and by reframing medieval communities' own
 *   linguistic practices as errors. The constraint persists because the
 *   reconstructionist establishment controls textbook pedagogy, manuscript
 *   editing standards, and classical training pipelines — institutional
 *   gatekeeping that enforces the discontinuity frame.
 *
 * KEY AGENTS:
 *   - philological_reconstructionists: agenda-setter (institutional) — set the reconstruction standard and train students in Classical forms as correct
 *   - continuity_hypothesis_defenders: payer (powerful/mobile) — defend an evolutionary reading and bear the cost of academic marginalization
 *   - medieval_latin_living_tradition_speakers: payer (moderate/identity-locked, historically dead) — their linguistic practices reframed as corruptions, epistemically present only through manuscripts
 *   - classical_literacy_gatekeepers: beneficiary (institutional) — collect prestige and gating authority from maintaining the Classical/Medieval boundary
 *   - textual_evidence_interpretive_community: observer (institutional/analytical) — can measure empirical linguistic patterns that would arbitrate the dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Discontinuity Reading of the Classical Latin Kernel").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '5a938a39-bd03-43e6-95d9-efcd15719514').
narrative_ontology:cs_kernel_codification('5a938a39-bd03-43e6-95d9-efcd15719514', distributed).
narrative_ontology:cs_authority_grounding('5a938a39-bd03-43e6-95d9-efcd15719514', extraction).
narrative_ontology:cs_interpretation_layer_present('5a938a39-bd03-43e6-95d9-efcd15719514').
narrative_ontology:cs_reading_relation('5a938a39-bd03-43e6-95d9-efcd15719514', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('5a938a39-bd03-43e6-95d9-efcd15719514', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('5a938a39-bd03-43e6-95d9-efcd15719514', foundational, classical_medieval_structural_rupture).
narrative_ontology:cs_axiom_status(classical_medieval_structural_rupture, holdable).
narrative_ontology:cs_axiom_grounding('5a938a39-bd03-43e6-95d9-efcd15719514', classical_medieval_structural_rupture, empirically_contingent).
narrative_ontology:cs_axiom('5a938a39-bd03-43e6-95d9-efcd15719514', secondary, medieval_forms_as_corruptions_ontology).
narrative_ontology:cs_axiom_status(medieval_forms_as_corruptions_ontology, holdable).
narrative_ontology:cs_axiom_grounding('5a938a39-bd03-43e6-95d9-efcd15719514', medieval_forms_as_corruptions_ontology, deontological).
narrative_ontology:cs_reference_frame('5a938a39-bd03-43e6-95d9-efcd15719514', classical_latin_as_normative_standard).
narrative_ontology:cs_drift_state('5a938a39-bd03-43e6-95d9-efcd15719514', contemporary_computational_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5a938a39-bd03-43e6-95d9-efcd15719514', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, philological_reconstructionists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, medieval_specialists_affirming_rupture).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, continuity_hypothesis_defenders).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_living_tradition_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_literacy_gatekeepers).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, structural_rupture_in_late_antiquity).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, medieval_forms_as_corruptions_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medieval Latin specialists, manuscript editors, and historical linguists who treat medieval texts as corruptions of an original Classical system. They set the interpretive agenda by establishing reconstruction procedures, training students in Classical forms as the standard, and adjudicating whether medieval manuscripts preserve or depart from 'correct' Latin. They define correctness through textual recovery methods.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, philological_reconstructionists, agenda_setter,
    institutional, generational, constrained, global).

% Scholars (primarily historical linguists, sociolinguists, evolutionary linguists) who argue that Medieval Latin is the natural phonological and morphological evolution of spoken Vulgar Latin into a distinct but intelligible system, not a corruption. They pay the cost of defending an interpretation that contradicts the established philological consensus and faces active suppression in textbook pedagogy and disciplinary prestige. Their alternatives include accepting the corruption framework or exiting Latin scholarship entirely.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, continuity_hypothesis_defenders, payer,
    powerful, biographical, mobile, global).

% Monks, ecclesiastical scholars, and medieval communities who used Medieval Latin as a living, productive language with its own grammar and conventions. Their reading and writing practices are reframed as errors and departures rather than as evidence of a coherent system. They are historically dead but epistemically present through manuscript evidence; their voice is excluded from the modern academic conversation that decides whether their usage is 'correct.'
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_living_tradition_speakers, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, medieval_latin_living_tradition_speakers, excluded).

% University classicists, publishers of Latin textbooks, and educational administrators who benefit from the discontinuity framing because it justifies the Ciceronian standard as THE correct form to teach, creating demand for specialized training and gatekeeping authority over 'proper' Latin education. They collect prestige and disciplinary authority from maintaining the Classical/Medieval boundary.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_literacy_gatekeepers, beneficiary,
    institutional, generational, constrained, global).

% Paleographers, codicologists, and digital humanities scholars who examine manuscripts without a prior commitment to either reading. They can measure variant frequencies, trace transmission paths, and model linguistic drift empirically, potentially providing evidence that would resolve the dispute.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, textual_evidence_interpretive_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, classical_literacy_gatekeepers).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for evaluating Latin texts: a metric by which correctness is measured, variants are classified as errors or intentional archaisms, and reconstruction procedures are legitimized. Solves the problem of how to adjudicate disputes about what a corrupted or ambiguous Latin passage should mean.
% TRANSFER_FUNCTION: Moves interpretive authority from medieval manuscript communities (dead but present in texts) to modern reconstructionists who now decide what medieval Latin 'really meant' to say (in Classical forms). Transfers prestige and disciplinary gatekeeping from medieval philology to Classical training. Every medieval manuscript interpretation that frames medieval forms as corruptions of Classical structure enriches the reconstructionist framework and confirms its authority.
% ABSENT_VOICES: Medieval Latin speakers are historically absent but epistemically present via manuscripts — they cannot defend their own system as coherent rather than corrupt. Modern evolutionary linguists defending continuity are structurally marginalized in classical philology departments and face suppression from textbook gatekeeping. Neither constituency is seated in the consensus-setting institutions.
% DISAPPEARANCE_RATIONALE: If this reading vanished and were replaced by the continuity reading, the entire apparatus of Classical Latin reconstruction would be reframed: medieval forms would be evidence of linguistic evolution, not corruption; Medieval Latin would be recognized as a distinct system with its own coherent grammar; and the pedagogical hierarchy (Classical as the standard, Medieval as deviant) would invert or dissolve. University Latin curricula, textbook hierarchies, and the prestige distribution in classical philology would reorganize.
% FOUNDING_PROBLEM: Late antiquity saw substantial change in Latin: phonological collapse of distinctions, morphological simplification, and lexical innovation. The question is whether these changes constitute corruption of a single system or the emergence of a new system (Medieval Latin) from natural linguistic drift.
% FOUNDING_PROBLEM_CORROBORATION: Reconstructionist philologists attest the problem is corruption that must be corrected via textual recovery. Historical linguists (outside the classical establishment) attest the problem is linguistic evolution and that the 'corruption' frame is a value judgment masquerading as description. Paleographic and codicological evidence from manuscript study shows that medieval scribes applied consistent, rule-governed transformations, suggesting system-internal logic rather than random error — this corroboration comes from textual evidence specialists outside both committed camps.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 terminal) and rises over the interval: the discontinuity frame consolidates power as reconstructionist methods become canonical in textbook pedagogy and manuscript editing. Medieval linguistic evidence is progressively reinterpreted through the lens of corruption rather than systemic coherence, extracting interpretive authority from medieval communities and concentrating it in the modern establishment. Suppression is high (0.71) because the framework requires active work to exclude the continuity hypothesis from prestigious publishing venues and graduate curricula; it is not self-evident but defended. Theater is substantial (0.58): a significant share of reconstruction work is performative establishment of the boundary (emphasizing classical purity, marking medieval forms as errors) rather than linguistic discovery. Accessibility collapse is low (0.45) because the continuity hypothesis remains technically available to any scholar with training in historical linguistics; it is suppressed, not structurally impossible. Resistance is high (0.68) because evolutionary linguists continue to publish evidence for linguistic drift, and paleographers document that medieval scribes applied consistent transformations — empirical pressure against the corruption frame.
 *
 * PERSPECTIVAL GAP:
 *   The reconstructionist and continuity-defender seats will compute as different types. Reconstructionists (beneficiary/agenda-setter) will compute as rope or tangled-rope beneficiary-side (coordination function + limited extraction of interpretive authority). Continuity-defenders and medieval communities (payers) will compute as tangled-rope or snare (active suppression + substantial extraction of interpretive authority). The divergence is structural asymmetry, not metric ambiguity — it is the engine's job to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Philological reconstructionists are full beneficiaries: they set the agenda, define correctness, train the next generation, and collect prestige and gating authority from the discontinuity frame — d approaches 0.0 (subsidy). Continuity-hypothesis defenders are targets: they defend an interpretation that contradicts consensus, face career suppression in classical philology departments, and must invest extra labor to publish against gatekeeping — d approaches 1.0 (extraction). Medieval Latin speakers are victims by definition: their own system is reframed as error, they cannot defend themselves (historically dead), and their epistemically present manuscripts are subject to reinterpretation by outsiders — d approaches 1.0. Classical literacy gatekeepers are beneficiaries (prestige, authority, justified hierarchy in pedagogy) — d near 0.0. Textual evidence specialists are observers: they can generate evidence that resolves the dispute but are not parties to the extraction — d = 0.5 (symmetric, analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows mandatrophy signals: the founding problem (what happened to Latin in late antiquity) is contested/live, but the founding_problem_status is held at 'contested' because continuity linguists and paleographers produce ongoing evidence that medieval forms are not corruptions but evolved variants. The discontinuity reading asserts the problem is solved (by establishing the Classical standard as the referent), but that settlement is precisely what is disputed. Specifically: if medieval linguistic evidence is re-read as evolutionary (not corrupted), the reconstruction problem reformulates: instead of 'correct' the Latin 'really' is, the question becomes 'how did Medieval Latin systematically evolve from earlier forms.' The mandatrophy is not yet resolved because the empirical evidence (paleographic, linguistic-frequency, textual-variance data) has not decisively settled the dispute; the reading persists by institutional enforcement rather than empirical closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_drift_vs_corruption,
    'Are medieval Latin forms the result of natural, rule-governed linguistic evolution from Vulgar Latin, or are they corruptions/errors in reproducing Classical forms?',
    'Quantitative analysis of medieval manuscript variants: measuring whether medieval scribes apply consistent, systemic transformations (evidence for evolution/new system) or random errors distributed across different exemplars (evidence for corruption). Phonological reconstruction of Vulgar Latin from Classical texts and medieval evidence to test whether medieval forms are predictable from phonological drift.',
    'If medieval forms are systematically derivable from phonological/morphological evolution, the discontinuity reading collapses into the continuity or hybrid reading. If medieval forms are truly idiosyncratic errors in copying Classical texts, the discontinuity reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_drift_vs_corruption, empirical, 'Whether medieval Latin variants are systematic linguistic evolution or random corruption.').

omega_variable(
    kernel_rupture_vs_reading_choice,
    'Is the ''discontinuity'' of Classical and Medieval Latin a fact of linguistic history (the kernel itself changed/ruptured), or is it a reading-choice (modern scholars choosing to frame them as distinct to justify reconstructionist methods)?',
    'Epistemological: examine what it would mean for a kernel to be ''objectively discontinuous'' independent of any reading. If the discontinuity reading''s force depends on the interpretive choice to treat medieval evidence as corruption rather than evidence of a living system, then the discontinuity is a framing choice, not a discovered fact. If medieval scribes themselves recognized a boundary (metadata, explicit reflection in texts), the discontinuity is historically attested.',
    'If discontinuity is a reading-choice (conceptual), then all three readings — continuity, discontinuity, hybrid — are equally defensible as frameworks for organizing the same evidence. If discontinuity is a discovered historical rupture (empirical), the discontinuity reading has stronger epistemic grounding. The engine computes which reading foreclosed which; the resolution of this omega determines whether foreclosure is structural or interpretive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_rupture_vs_reading_choice, conceptual, 'Whether discontinuity is a fact of linguistic history or a reading-choice.').

omega_variable(
    suppression_mechanism_textual_vs_institutional,
    'Is the measured suppression (0.71) primarily structural (medieval evidence genuinely does not survive in the text record, making reconstruction hard) or internalized/institutional (medieval evidence exists but is systematically reinterpreted as error by the reconstructionist framework)?',
    'Audit of manuscript survival rates for medieval versus classical texts; count of medieval linguistic variants documented and explicitly labeled as ''errors'' or ''corruptions'' in modern editions versus classified neutrally as variants. Post-framework suppression trajectory: if scholars who adopt the continuity reading report no barrier to accessing medieval evidence, suppression is framework-dependent (institutional) rather than structural.',
    'If suppression is primarily institutional (the reconstructionist framework suppresses alternative readings of medieval evidence), then removing the framework would expose the medieval evidence as systematically coherent, supporting the continuity reading. If suppression is structural (medieval evidence is genuinely fragmentary and hard to reconstruct), the discontinuity reading has material grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_textual_vs_institutional, empirical, 'Whether suppression is structural (evidence scarcity) or framework-dependent (reinterpretation as error).').

omega_variable(
    committer_frame_kernel_identity,
    'What is the kernel that the discontinuity and continuity readings diverge on — is it the empirical claim (what Latin historically was) or the normative claim (what Latin ''should'' be reconstructed as)?',
    'Explicit comparison of the three readings'' empirical commitments (what happened to Latin) versus their normative commitments (how should we adjudicate correctness, what pedagogical standard should govern teaching). If all three readings agree on the empirical history but disagree on the standard, the kernel is normative (definitional). If they disagree on the history, the kernel is empirical (historical).',
    'If the kernel is normative, the discontinuity reading wins interpretive authority by controlling the standard-setting institutions, not by superior evidence. If the kernel is empirical, the reading that best predicts manuscript evidence wins. This determines whether the constraint is primarily coordinating on a standard (rope) or extracting interpretive authority (snare/tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_identity, conceptual, 'Whether the contested kernel is empirical (what happened) or normative (what should count as correct).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t3, correct_latin_kernel__discontinuity_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement_basis(corr_tr_t3, observed).
narrative_ontology:measurement(corr_tr_t6, correct_latin_kernel__discontinuity_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement_basis(corr_tr_t6, observed).
narrative_ontology:measurement(corr_tr_t12, correct_latin_kernel__discontinuity_reading, theater_ratio, 12, 0.56).
narrative_ontology:measurement_basis(corr_tr_t12, observed).
narrative_ontology:measurement(corr_tr_t18, correct_latin_kernel__discontinuity_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement_basis(corr_tr_t18, observed).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__discontinuity_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(corr_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t3, correct_latin_kernel__discontinuity_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(corr_be_t3, observed).
narrative_ontology:measurement(corr_be_t6, correct_latin_kernel__discontinuity_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(corr_be_t6, observed).
narrative_ontology:measurement(corr_be_t12, correct_latin_kernel__discontinuity_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(corr_be_t12, observed).
narrative_ontology:measurement(corr_be_t18, correct_latin_kernel__discontinuity_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(corr_be_t18, observed).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__discontinuity_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(corr_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t3, correct_latin_kernel__discontinuity_reading, suppression_requirement, 3, 0.66).
narrative_ontology:measurement_basis(corr_su_t3, observed).
narrative_ontology:measurement(corr_su_t6, correct_latin_kernel__discontinuity_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(corr_su_t6, observed).
narrative_ontology:measurement(corr_su_t12, correct_latin_kernel__discontinuity_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(corr_su_t12, observed).
narrative_ontology:measurement(corr_su_t18, correct_latin_kernel__discontinuity_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement_basis(corr_su_t18, observed).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__discontinuity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(corr_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_kernel decomposes into three distinct constraint stories, each instantiating a different reading of how to interpret the relationship between Classical and Medieval Latin. The discontinuity_reading treats them as structurally distinct systems (reconstruction as symbolic recovery of lost form). The continuity_reading treats Medieval Latin as natural evolution from Classical (reconstruction as internal correction). The hybrid_reading splits the difference (morphology continuous, syntax/lexicon discontinuous). Each reading establishes different beneficiaries, victims, and extraction mechanisms. The three stories are linked via network.affects_constraints to enable comparative analysis of how alternative framings of the same historical domain produce different constraint classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
