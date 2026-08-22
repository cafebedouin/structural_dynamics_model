% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Political Speech Protection Hierarchy (Democratic Participation Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the democratic-participation reading of the
 *   speech-protection kernel. The reading holds that speech protection is
 *   STRONGEST for political expression necessary for self-governance
 *   (campaign speech, protest, legislative deliberation, organizing) and
 *   WEAKER for non-political speech (commercial, entertainment, personal).
 *   The doctrinal hierarchy is formalized in constitutional law (strict
 *   scrutiny for content-based restrictions on political speech, rational
 *   basis for non-political restrictions) and is the dominant reading in
 *   liberal democracies' jurisprudence. The reading coordinates democratic
 *   deliberation by privileging the speech that sustains it; it
 *   simultaneously extracts from non-political speakers and from subordinated
 *   groups targeted by political hate speech. The constraint is CLAIMED as
 *   tangled_rope (genuine coordination function + asymmetric extraction) and
 *   the authored metrics describe operation consistent with that claim:
 *   moderate extractiveness (0.38), moderate suppression (0.42 — the
 *   suppression is the boundary-drawing work required to police the
 *   political/non-political distinction without openly censoring either
 *   side), and rising theater ratio over time (the boundary work becomes
 *   increasingly performative as technological change and political
 *   polarization complicate what counts as 'political').
 *
 * KEY AGENTS:
 *   - political_speakers (beneficiaries): organized agents with mobile exit, receive maximal protection
 *   - democratic_institutions (structural beneficiaries): courts, legislatures, electoral processes gain doctrinal priority
 *   - non_political_speakers (payers): moderate power, constrained exit, face heightened scrutiny
 *   - subordinated_groups_as_speech_targets (payers): powerless, trapped, bear costs of hate speech protected as political expression
 *   - state_speech_regulators (agenda_setters): courts and legislatures operationalize the boundary
 *   - competing_readings_advocates (excluded): absolutist, dignity, harm-threshold, and marketplace readers are present but operate under this reading's constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.38).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.42).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Political Speech Protection Hierarchy (Democratic Participation Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '64217042-fd24-4704-b29f-9ba9dff9ea0b').
narrative_ontology:cs_kernel_codification('64217042-fd24-4704-b29f-9ba9dff9ea0b', fixed_text).
narrative_ontology:cs_authority_grounding('64217042-fd24-4704-b29f-9ba9dff9ea0b', lineage).
narrative_ontology:cs_interpretation_layer_present('64217042-fd24-4704-b29f-9ba9dff9ea0b').
narrative_ontology:cs_reading_relation('64217042-fd24-4704-b29f-9ba9dff9ea0b', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('64217042-fd24-4704-b29f-9ba9dff9ea0b', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('64217042-fd24-4704-b29f-9ba9dff9ea0b', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('64217042-fd24-4704-b29f-9ba9dff9ea0b', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_axiom('64217042-fd24-4704-b29f-9ba9dff9ea0b', foundational, political_speech_hierarchical_priority).
narrative_ontology:cs_axiom_status(political_speech_hierarchical_priority, holdable).
narrative_ontology:cs_axiom_grounding('64217042-fd24-4704-b29f-9ba9dff9ea0b', political_speech_hierarchical_priority, deontological).
narrative_ontology:cs_axiom('64217042-fd24-4704-b29f-9ba9dff9ea0b', foundational, self_governance_requires_robust_opposition_speech).
narrative_ontology:cs_axiom_status(self_governance_requires_robust_opposition_speech, holdable).
narrative_ontology:cs_axiom_grounding('64217042-fd24-4704-b29f-9ba9dff9ea0b', self_governance_requires_robust_opposition_speech, deontological).
narrative_ontology:cs_reference_frame('64217042-fd24-4704-b29f-9ba9dff9ea0b', constitutional_political_speech_priority).
narrative_ontology:cs_drift_state('64217042-fd24-4704-b29f-9ba9dff9ea0b', contemporary_algorithmic_amplification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('64217042-fd24-4704-b29f-9ba9dff9ea0b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, civic_deliberation_frameworks).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, subordinated_groups_as_speech_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive maximal protection for speech that contributes to democratic deliberation: campaign rhetoric, protest, legislative testimony, newspaper editorials about governance, union organizing. Their speech enjoys robust presumptions against restriction. They can modify their message and venue to stay within protected zones.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_speakers, beneficiary,
    organized, generational, mobile, national).

% The framework vindicates institutional self-governance: courts, legislatures, electoral processes depend on robust political speech to function. The constraint embeds the normative claim that democracies require strong protection for the speech that sustains them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, democratic_institutions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(speech_protection_kernel__democratic_participation_reading, democratic_institutions).

% The doctrine vindicates deliberative forums (town halls, press, legislatures, courts) by providing constitutional priority to speech they depend on. These are not actors collecting rents; they are frameworks the reading sanctifies.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, civic_deliberation_frameworks, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(speech_protection_kernel__democratic_participation_reading, civic_deliberation_frameworks).

% Receive weaker protection: commercial speech, entertainment, personal gossip, artistic expression not tied to governance, religious speech (in many jurisdictions under this reading). They can be restricted on grounds (commercial interest, harm, decorum) that would not reach political speech. Their exit is constrained by the hierarchical doctrine itself — if they reframe personal grievance as political speech, they may gain protection, but the reframing requirement is a form of suppression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_political_speakers, payer,
    moderate, biographical, constrained, national).

% When political speakers use political speech protections to circulate hate speech, conspiracy narratives, or dehumanizing rhetoric directed at them, they bear the cost of that speech. The democratic-participation reading does not view their silencing as an option (that would restrict political speech) and does not view their harm as grounds for restricting the speaker (harm to listeners is not the reading's threshold). Their exit is the constraint itself: they cannot silence the speech, cannot leave the nation, cannot reframe the harm as 'not really political.'
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, subordinated_groups_as_speech_targets, payer,
    powerless, biographical, trapped, national).

% Courts and legislatures administer the doctrine, drawing the line between political and non-political speech, deciding what count as permissible exceptions (national security, incitement, true threats). They must enforce the hierarchy while claiming neutrality about the content of political speech itself. Their task is to police the boundary without censoring either side.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, state_speech_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Absolutist readers (arguing no harm threshold applies), dignity readers (arguing structural subordination overrides political-speech priority), harm-threshold readers (arguing demonstrable victim injury is grounds for restriction), and marketplace readers (arguing false speech undermines the truth-discovery function) are present in jurisprudence and scholarship but operate under constitutional constraints this reading produces. They argue for different doctrinal boundaries; their full positions are not permitted to govern simultaneously with this reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, competing_readings_advocates, excluded,
    powerful, generational, mobile, national).

% Judicial institutions interpret what counts as 'political' and what exceptions apply. They do not author the reading — the reading is a constitutional principle courts apply — but they operationalize the boundary between protected and unprotected categories through case decisions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, courts_as_interpreters, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for democratic deliberation by granting robust protection to speech necessary for citizens and institutions to participate in self-governance: candidates for office can speak without prior restraint, citizens can organize politically, legislatures can debate freely, press can investigate. The coordination solves the collective-action problem of creating space for democratic contestation.
% TRANSFER_FUNCTION: Transfers expressive liberty preferentially to those who engage in democratic participation (political speakers, civic institutions, deliberative forums) and away from those whose speech serves other purposes (commercial speech, entertainment, personal grievance unconnected to governance, subordinated groups seeking to silence hate speech directed at them). The transfer is asymmetric: political speech gains protection, non-political speech loses it or faces heightened scrutiny.
% ABSENT_VOICES: Subordinated groups targeted by political hate speech would argue for restrictions protecting them from dehumanizing rhetoric; absolutist free-speech advocates would argue for removing all content-based hierarchy; dignity readers would argue for prioritizing protection of targets over speaker protection; harm-threshold readers would argue for empirical investigation of listener injury. These positions are not foreclosed by the doctrine — they appear in competing scholarship and minority judicial opinions — but they are muted or excluded when this reading governs.
% DISAPPEARANCE_RATIONALE: If the presumption favoring political speech vanished, speech restrictions on non-political expression would not automatically apply to political expression. Campaign speech, protest, legislative testimony, union organizing would lose their doctrinal shield and would be subject to the same scrutiny as commercial or entertainment speech. The range of permissible speaker regulation would expand across all categories. Political contestation would operate under greater legal uncertainty.
% FOUNDING_PROBLEM: Democratic self-governance requires that citizens and institutions be free to propose, debate, and contest the rules by which they are governed, without prior restraint from those holding power. If incumbents can suppress opposition speech, democracy fails. The problem is structural: power-holders have incentive to silence critics, but democracy requires critics to be heard.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, constitutional scholars, and democratic theorists outside the benefiting parties (judicial institutions, political speakers themselves) attest that democracies dependent on electoral contestation and legislative deliberation require robust protection for opposition speech. Authoritarian transitions and historic censorship regimes provide comparative evidence that speech suppression precedes democratic collapse. Courts cite this scholarship; the founding problem is broadly attested from outside the benefiting institutional seats.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.28 at start) because the reading genuinely coordinates democratic deliberation (the coordination function is real and substantial) but does so by privileging certain speakers over others. Non-political speakers and subordinated groups pay for that priority. The suppression metric (0.42) reflects the active enforcement work required to police the political/non-political boundary: courts must decide which speech is 'political' in context, which exceptions apply, whether a novel form of speech (internet memes, algorithmic amplification, AI synthesis) counts as protected political speech. This boundary-work is non-trivial and suppressive in the sense that it forecloses exits for those on the non-protected side — they cannot silence the speech and cannot reframe it out of the political category once it is marked as such. Theater ratio rises (0.18 to 0.28) because courts increasingly perform 'political speech' analysis (extensive doctrinal discussion, precedent parsing) while the actual protection outcome remains stable: political speech is protected, non-political is not. The performance-to-function ratio increases as the doctrine becomes more elaborate without producing different real outcomes for payers. The measurements track a stabilizing constraint: extractiveness rises early as the doctrine crystallizes, then plateaus; suppression and theater rise slowly as the boundary-policing work becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   From the political-speaker seat and the democratic-institution seat, this constraint is genuine coordination grounded in necessity: democracies require robust protection for opposition speech to function. From the non-political-speaker seat, the constraint is a hierarchy that deprioritizes their expression. From the subordinated-group seat, the constraint is pure extraction: they bear the cost (exposure to dehumanizing political speech) without collecting the benefit (their own speech gets no special protection, and harm to them is not grounds for restricting political speakers). The engine computes these divergences from the structural data: beneficiary seats compute low directionality (d near 0.0 on political speech), payer seats compute high directionality (d near 1.0), and the constraint's scope and power atoms differentiate the magnitude of effective extraction. The perspectival gap is the entire point: a constraint that appeared symmetric from all seats would not be extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Political speakers (organized, mobile, national scope) are the primary beneficiaries: d ≈ 0.1 (slight target position if speech ventures into unprotected territory, but baseline position is highly subsidized). Democratic institutions and deliberation frameworks are structural beneficiaries: d ≈ 0.0 (the reading vindicates them; they collect maximum benefit). Non-political speakers (moderate power, constrained exit, national scope) are secondary payers: d ≈ 0.65 (they face higher scrutiny, limited alternatives, but retain some expressive space). Subordinated groups targeted by political hate speech (powerless, identity-locked exit, national/universal scope) are primary payers: d ≈ 0.95 (they cannot escape the speech, cannot silence it, bear the full cost, and gain zero protection). State regulators (institutional power, generational horizon, constrained exit by role) sit at d ≈ 0.5 (they benefit from doctrinal clarity but bear the work of boundary-enforcement and face pressure from all sides). The directionality overrides are not needed here: the structural derivation (beneficiary/victim + exit) produces the right values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democracies require opposition speech) is LIVE: every transition to authoritarianism involves speech suppression, and every democracy depends on robust political speech. The disappearance verdict is WORLD_REARRANGES: the constraint's removal would reshape the speech-protection landscape, expanding restriction possibilities across all categories. These align: the founding problem is still the problem the constraint solves, and removing the constraint would cause reorganization. No mandatrophy signal. However, the reading does face a secondary question: as the political/non-political boundary becomes harder to police (internet speech, AI synthesis, algorithmic curation) and as political speech is increasingly weaponized against subordinated groups, the doctrine's theatre ratio rises. The performance-to-function gap (0.18 to 0.28) suggests that courts are expending more work to maintain the boundary without changing the real outcomes for any seat — the suppression-work is increasing faster than the extractiveness-reduction is happening. This is a piton-direction signal (performance-heavy maintenance of a stable extraction), not mandatrophy (the founding problem is not dead, just increasingly theatrical to defend). The boundary-hierarchy structure persists not because the coordination problem is solved more efficiently but because the doctrine is institutionalized and because the competing readings (absolutist, dignity, harm-threshold) have not yet displaced it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_speech_boundary_ambiguity,
    'What counts as ''political speech'' for purposes of heightened protection? Where does the boundary lie between political expression (protected) and non-political expression using political language (weaker protection)?',
    'Systematic study of actual court decisions: which categories of speech receive political-speech protection? (e.g., Does advertising for a political candidate count? Speech by a political movement? Speech about politics by a non-politician? Entertainment with political themes?) Historical case-law analysis and comparative jurisprudence across jurisdictions would reveal the working boundary.',
    'A narrow boundary (political speech = only campaign/legislative/protest speech) makes the extraction sharper and the theater ratio higher (more boundary-policing work). A broad boundary (political speech = any speech engaging governance questions) flattens the hierarchy and reduces extraction. The boundary ambiguity is the mechanism through which subordinated groups become trapped — their own political expression about their oppression is sometimes deemed not ''political enough'' for heightened protection, while hate speech against them is deemed political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_speech_boundary_ambiguity, empirical, 'Definitional boundary of ''political speech'' in practice').

omega_variable(
    hate_speech_as_political_expression,
    'Is hate speech circulated through political movements (e.g., conspiracy rhetoric, dehumanization narratives) properly classified as ''political speech'' for protection purposes, or is the political vehicle distinct from the hate-speech content?',
    'Analysis of cases where hate speech is defended as political speech: what reasoning do courts offer? Do they protect the speech because it is political, or despite it being hate speech? Study of jurisdictions that have separated political-expression protection from hate-speech restrictions to observe whether the democratic-self-governance function survives the separation.',
    'If hate speech circulated through political channels is deemed political and protected, subordinated groups bear maximal extraction and the constraint is extractive to them at d≈1.0. If hate speech can be restricted even when circulated politically, the extraction on subordinated groups is reduced and an alternative reading (dignity or harm-threshold) becomes more operative. This omega is the crux where the democratic-participation reading most sharply conflicts with readings that center protection for targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hate_speech_as_political_expression, conceptual, 'Whether hate speech channeled through political movements retains political-speech protection or can be separately restricted').

omega_variable(
    theater_ratio_interpretation,
    'Does the rising theater ratio (0.18 to 0.28) indicate that boundary-policing work is becoming performative without changing real outcomes, or is the increasing elaboration of doctrine (more nuanced precedent, more careful categorization) a sign that courts are refining the boundary without changing its gross shape?',
    'Fine-grained analysis of court decisions: are outcomes changing? (e.g., Is political speech being protected at higher rates? Are non-political restrictions being applied differently?) Or is the same protection pattern being maintained through increasingly elaborate justification? Measurement of reversal rates, scope of exceptions, actual reach of each category would clarify whether the theater is masking mandate decay or enabling boundary stability.',
    'If theater masks stability (same outcomes, more talk), the constraint risks piton classification: performatively maintained extraction without functional change. If the elaboration is enabling refinement (different outcomes in edge cases, evolving definition), the theater ratio is doctrinal maturation, not degradation. This distinction matters for assessing whether the constraint will persist or fade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Whether rising theater reflects doctrinal performance without outcome change or genuine boundary refinement').

omega_variable(
    competing_reading_foreclosure,
    'Does the democratic-participation reading logically FORECLOSE the absolutist reading (no content hierarchy, speech near-categorical protection)? Or do they COEXIST as different interpretive traditions held by different institutional actors?',
    'Jurisprudential history: have courts that adopt the democratic-participation reading explicitly rejected the absolutist position, or do they coexist in the case law with different judges/courts holding each? Study of constitutional amendment or major doctrinal overhaul: would accepting absolutism require overturning the democratic-participation reading, or merely shifting the boundary?',
    'If foreclosure holds, the readings are mutually exclusive; one reading cannot be adopted without rejecting the other. If coexistence holds, both readings are live options that different institutional seats adopt. This affects how sibling readings are classified (forecloses vs. coexists_with) and whether the kernel is truly contested or whether one reading has achieved dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether the democratic-participation and absolutist readings logically foreclose each other or coexist as live interpretive options').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.42, rising to 0.42) a structural property of the doctrine (the boundary is enforced externally through court decisions) or internalized (speakers adopt the political/non-political distinction themselves, filtering their own speech to stay on the protected side)?',
    'Behavioral study: do speakers who move from one jurisdiction to another (where the boundary differs) change their behavior? Do they continue self-filtering based on the internalized rule, or do they adjust to the new structural rules? Analysis of speech-code literature and speaker-interview data would show whether suppression persists absent external enforcement.',
    'If suppression is purely structural, it is reversible: remove the doctrine and speakers can immediately expand their expression. If suppression is partially internalized, it persists even if the doctrine changes — subordinated groups might continue to self-censor even after protection expands, or political speakers might continue to claim political-speech authority even if the boundary opens. Internalized suppression reduces the effectiveness of remedies that rely on doctrinal change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is structural (enforced externally) or internalized (self-adopted by speakers)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__democratic_participation_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(spee_tr_t8, observed).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__democratic_participation_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement_basis(spee_tr_t16, observed).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__democratic_participation_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(spee_tr_t24, observed).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__democratic_participation_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(spee_tr_t32, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__democratic_participation_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__democratic_participation_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(spee_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement_basis(spee_be_t8, observed).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement_basis(spee_be_t16, observed).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(spee_be_t24, observed).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(spee_be_t32, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(spee_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(spee_su_t8, observed).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(spee_su_t16, observed).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement_basis(spee_su_t24, observed).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(spee_su_t32, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(spee_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__democratic_participation_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the contested speech-protection kernel. All five readings share the same referent (the standing arrangement of constitutional speech protection) but differ in their structural interpretation of what that protection means and which speech receives priority. The democratic-participation reading establishes a hierarchy: political speech strongest, non-political weaker. Sibling readings reject or modify this hierarchy (absolutist: no hierarchy; dignity: hierarchy inverted to protect targets; harm-threshold: harm, not speech-type, is determinative; marketplace: false speech vs. true speech is the distinction, not political vs. non-political). Each reading has different ε values, different beneficiary/victim structures, and different enforcement dynamics. The readings are linked through affects_constraints to document that they are all readings of the same kernel — the network records the family decomposition per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
