% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Synchronic/Diachronic Seam Demand on IP-Origin Accounts (M4/M5 Collapse Test)
 *   domain: legal philosophy/intellectual property/historical jurisprudence
 *
 * SUMMARY:
 *   Within the historiography of intellectual property, the shorthand 'IP
 *   emerged in 1710' covers at least two distinct claims — that ownable
 *   expression became legally coherent (category emergence) and that the
 *   author entered the legitimate claimant set (occupancy change). This story
 *   instantiates the synchronic_diachronic_seam reading of that kernel: the
 *   standing methodological demand that the two axes be treated as formally
 *   independent unless shown otherwise, or else recognized as a temporal
 *   framing artifact — the M4/M5 collapse test. The arrangement under
 *   contest, and the referent of every metric below, is the field's unified
 *   single-event frame: the practice of letting one date carry both loads.
 *   Assessed by this reading's own lights, that frame performs real
 *   coordination (a citable common origin keeping a decentralized field
 *   mutually intelligible) while transferring precision and narrative
 *   authority away from decomposed-account producers. Per the
 *   epsilon-invariance principle this is one file of a three-story family;
 *   the sibling readings author their own constraints and epsilon values
 *   elsewhere, and nothing here averages across them. KEY AGENTS (by
 *   structural relationship): - legal_history_gatekeepers: agenda-setting
 *   administrators (institutional/arbitrage) — run the review and curriculum
 *   machinery the frame rides on - grand_narrative_historians: primary
 *   beneficiary (powerful/identity_locked) — collect the narrative-authority
 *   surplus - doctrinal_origin_traditions: secondary beneficiary
 *   (institutional/constrained) — collect legitimacy from the clean founding
 *   act - decomposition_scholars: primary payer (moderate/constrained) — bear
 *   the precision costs - thinkability_program_scholars and
 *   first_holding_program_scholars: dual-positioned (protected niche plus
 *   proof burden) - policy_analysis_consumers: payer (organized/mobile) —
 *   absorb misdiagnosed levers - civil_law_genealogists: excluded
 *   (organized/trapped) — object from outside the canon -
 *   historiography_methodologists: analytical observer — sees the full
 *   structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.66).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.56).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.66).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Synchronic/Diachronic Seam Demand on IP-Origin Accounts (M4/M5 Collapse Test)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal philosophy/intellectual property/historical jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '2cec0e24-e2a2-4203-b73b-ba316472c005').
narrative_ontology:cs_kernel_codification('2cec0e24-e2a2-4203-b73b-ba316472c005', distributed).
narrative_ontology:cs_authority_grounding('2cec0e24-e2a2-4203-b73b-ba316472c005', expertise).
narrative_ontology:cs_interpretation_layer_present('2cec0e24-e2a2-4203-b73b-ba316472c005').
narrative_ontology:cs_reading_relation('2cec0e24-e2a2-4203-b73b-ba316472c005', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('2cec0e24-e2a2-4203-b73b-ba316472c005', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('2cec0e24-e2a2-4203-b73b-ba316472c005', foundational, independence_or_framing_artifact).
narrative_ontology:cs_axiom_status(independence_or_framing_artifact, holdable).
narrative_ontology:cs_axiom_grounding('2cec0e24-e2a2-4203-b73b-ba316472c005', independence_or_framing_artifact, empirically_contingent).
narrative_ontology:cs_axiom('2cec0e24-e2a2-4203-b73b-ba316472c005', foundational, dispute_counting_precedence).
narrative_ontology:cs_axiom_status(dispute_counting_precedence, holdable).
narrative_ontology:cs_axiom_grounding('2cec0e24-e2a2-4203-b73b-ba316472c005', dispute_counting_precedence, instrumental).
narrative_ontology:cs_reference_frame('2cec0e24-e2a2-4203-b73b-ba316472c005', provisional_two_axis_kernel).
narrative_ontology:cs_drift_state('2cec0e24-e2a2-4203-b73b-ba316472c005', contemporary_revisionist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2cec0e24-e2a2-4203-b73b-ba316472c005', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, grand_narrative_historians).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, doctrinal_origin_traditions).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, thinkability_program_scholars).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, first_holding_program_scholars).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, decomposition_scholars).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, policy_analysis_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, thinkability_program_scholars).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, first_holding_program_scholars).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, m4_m5_collapse_test_protocol).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, two_axis_periodization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit the journals, book series, and casebooks through which origin accounts circulate. They set what counts as a well-formed answer to the question of what happened in 1710, and their review practices currently favor accounts that treat the statute as a single event. Holding that preference costs them little; if the field's preference flipped to two-axis accounts they could adopt the new norm at similar cost while keeping their positional advantage.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_history_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Senior synthesists whose books, prizes, and lecture circuits trade on the singularity of 1710 — one act, one birth, one arc from the stationers' monopoly to authorial right. Their authority compounds with each citation of the unified story. Recasting a life's synthesis as a preliminary sketch of a two-axis process would unravel the contribution their reputation rests on, so leaving the unified frame is not a realistic personal option.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, grand_narrative_historians, beneficiary,
    powerful, generational, identity_locked, global).

% Courts, commentaries, and bar lore that ground today's intellectual-property powers in a clean founding act. A single founding event supports the claim that the system was born legitimate and complete; a two-axis origin invites the question of which axis carries the legitimacy. Their commitments are fixed in precedent and pedagogy, so revising the origin story means relitigating foundations.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, doctrinal_origin_traditions, beneficiary,
    institutional, generational, constrained, national).

% Periodization specialists and book-history-trained jurists who publish accounts separating category coherence from claimant entry. Reviewers ask why the split is needed, citations flow to single-event summaries, and grant panels read fine-grained splits as pedantry. Partial exit exists — adjacent fields such as print culture and economic history reward their granularity — but moving means leaving the doctrinal conversation they trained for.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, decomposition_scholars, payer,
    moderate, biographical, constrained, global).

% Researchers whose program rests on evidence that ownable expression became legally coherent around 1710. The seam test protects their subject matter from being absorbed into a claimant-entry story: if the axes vary independently, their dimension is real and fundable. The same test bars them from presuming independence — they must now produce divergence cases, and a failed test dissolves their program's premise.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, thinkability_program_scholars, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, thinkability_program_scholars, payer).

% Researchers whose program rests on evidence that the author entered the legitimate claimant set around 1710. Symmetrically positioned: the seam test certifies their disagreement with the coherence program as substantive rather than verbal, but requires them to show occupancy changing independently of category formation, on pain of their program merging into its rival.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, first_holding_program_scholars, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, first_holding_program_scholars, payer).

% Offices and analysts who import origin narratives into policy design — deciding, for instance, whether expansion is driven by the boundaries of protectable subject matter or by who may claim rights. Working from the unified frame, they attribute to one lever what belongs to the other and misprice reforms. Correction is available — commissioning bespoke periodization — at a cost most offices decline to pay.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, policy_analysis_consumers, payer,
    organized, immediate, mobile, national).

% Scholars of droit d'auteur and neighboring continental traditions whose origin events differ in kind and date from the Anglophone statute. They would object that the seam debate presupposes the Anglophone canon and that their genealogies may split the two axes differently. They stand outside the journal-and-casebook circuit that runs the debate, and entering it would require reframing their traditions in its terms.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, civil_law_genealogists, excluded,
    organized, generational, trapped, continental).

% Philosophers of history and metascience researchers who track how fields count their own disputes. They neither draw on the origin narrative nor pay its costs; they use the seam controversy as a case study in framing artifacts and dispute well-formedness.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, historiography_methodologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, grand_narrative_historians).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The unified single-event frame gives a large decentralized field one citable origin: curricula, casebooks, citation practice, and doctrinal legitimacy all anchor to the same dated act, keeping the field's historical references mutually intelligible without a central registry.
% TRANSFER_FUNCTION: Moves analytical precision and narrative authority from producers of decomposed accounts to holders of the unified frame: evidence generated for one axis is spendable on the other's account, and the simplification surplus accrues to seats whose contributions trade on singularity.
% ABSENT_VOICES: Civil-law genealogists would object that the debate presupposes the Anglophone canon and that continental genealogies may split the axes differently; pre-statute printing-privilege practitioners' categories, which map onto neither axis cleanly, have no seat at all. Both stand outside the Anglophone journal-and-casebook circuit that administers the frame.
% DISAPPEARANCE_RATIONALE: If the unified frame vanished overnight, curricula would fork along the two axes, citation practice would split between coherence evidence and occupancy evidence, doctrinal legitimacy arguments would have to rebuild on function rather than founding, and the two substantive research programs would either sharpen into genuinely distinct inquiries or merge — the field's map of its own past would be redrawn.
% FOUNDING_PROBLEM: Early historiography faced an undifferentiated mass of eighteenth-century developments — the statute, the stationers' wind-down, shifting author-publisher relations — and needed a single teachable handle; the dated act of 1710 served as that handle for both category formation and claimant entry at once.
% FOUNDING_PROBLEM_CORROBORATION: Print-culture empiricists and comparative-law genealogists attest from outside the benefiting parties that the eighteenth-century developments were plural and gradual, supporting the view that the single handle is analytically obsolete even where pedagogically live. Grand narrators and doctrinal commentators attest continued liveness; no corroborator outside the beneficiary set attests the single-event account's analytical liveness — that asymmetry is itself the signal behind the contested status.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66) reflects the conflation's standing transfer: evidence generated for one axis is spendable on the other, and the simplification surplus accrues to seats trading on singularity while precision costs land on decomposers and policy consumers. Suppression (0.56) is authored as a raw structural property — enforcement intensity, not participant belief — and is deliberately left unscaled here; the engine scales extractiveness by directionality and scope, never suppression. Its rising trajectory is the story's central dynamic: the frame began the interval as self-evident background requiring little defense and now requires active reviewer pushback, syllabus inertia, and citation-canon maintenance. Theater (0.46) is elevated by centenary ceremonial culture — a bicentenary peak near t=20 and a tercentenary peak near t=120 bracket a mid-century trough, giving one full oscillation across the interval; the oscillation is plausibly part of the maintenance mechanism itself (periodic re-singularization of the event), an attribution flagged in omega centenary_oscillation_attribution rather than assumed. Accessibility collapse (0.50): two-axis accounts are publishable and increasingly common, so alternatives persist once the seam is seen. Resistance (0.62): revisionist historiography, the seam-test program, and comparative genealogy actively contest the unified frame. The claimed type is tangled_rope on this reading's own structural assessment; the metrics were authored independently as descriptive facts, and the engine computes per-seat types from the structural data — any divergence between claim and computed type is the measurement, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   From the gatekeeping seat the frame is curation — a well-formed-answer filter that keeps teaching coherent and peer review tractable. From the decomposition seat the same filter is flattening — distinctions argued at article length returned as 'the standard story with extra steps.' The sibling-program seats experience protection and burden simultaneously: the seam test certifies their dispute as substantive and puts their premises in jeopardy in the same motion. Policy consumers experience the frame as authoritative background until a reform priced on the wrong lever fails. The engine computes these divergences from power, exit, and directional position; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Grand narrators and doctrinal traditions sit near the beneficiary end: the frame subsidizes their authority and legitimacy, and their exits are poor — one seat's synthesis is identity-fused, the other's is precedent-fixed. Decomposition scholars and policy consumers sit near the target end: they fund the frame's precision costs with constrained or costly exit. The two sibling programs are genuinely dual-positioned — protected niche (benefit) plus proof burden (cost) — placing them mid-range; the beneficiaries array records their primary position and secondary_role carries the other side. The gatekeepers derive near-beneficiary despite administering the frame because their arbitrage-grade exit dominates: they profit under either frame and can switch at low cost. No directionality overrides are needed — the derivation from declared roles, power, and exit options already produces these relationships, and the blunt power-atom-keyed override surface could not distinguish the dual-positioned programs from same-atom payers without misfiring.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an undifferentiated mass of eighteenth-century developments needing a teachable handle — is live for pedagogy and dead-or-contested for analysis; recording founding_problem_status as contested keeps the mismatch visible without tripping a zombie flag, since the verdict is world_rearranges and the status is not dead. Classifying the frame as tangled_rope rather than snare prevents reading reviewer friction as pure predation — the coordination function is real, since without a shared origin reference the field cannot cite a common past at all. Refraining from rope prevents excusing the transfer — the same structure that coordinates also spends one axis's evidence on the other's account. If the M4/M5 test resolves decisively the arrangement should migrate: an artifact verdict collapses the kernel family and the residual single-event usage decays toward theatrical maintenance of a framing habit; an independence verdict converts the frame into a two-axis standard whose leftover single-event usage becomes the extractive residue. mandatrophy_resolved is deliberately left unset — resolution awaits the test's outcome, not a metric threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    m4_m5_collapse_verdict,
    'Are thinkability (category coherence of ownable expression) and first-holding (entry into the legitimate claimant set) formally independent dimensions, or does their co-occurrence at 1710 reflect a temporal framing artifact?',
    'Counterfactual periodization: locate divergence cases where the axes come apart — pre-1710 arrangements in which expression was already treated as ownable but authors were not claimants, and later or comparative arrangements in which claimant scope moved without category change — and test whether the co-occurrence survives their inclusion.',
    'An independence verdict authenticates the kernel''s two-axis structure and both sibling readings survive as distinct research programs; an artifact verdict renders the kernel spurious, collapses the family into a single claim, and converts this constraint into a note about framing habits rather than a standing methodological demand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_collapse_verdict, empirical, 'The reading''s own open question: independence or artifact.').

omega_variable(
    kernel_reading_instantiation_choice,
    'Does instantiating the seam test as a standalone constraint, rather than folding it into either substantive sibling reading, correctly locate the dispute — or is the seam merely a property of the siblings'' relationship that should not carry its own epsilon?',
    'If the M4/M5 test resolves to ''always co-occur,'' the seam reading degenerates into a framing-artifact remark inside whichever sibling survives and this file should merge; if it resolves to ''independent,'' the seam persists as a permanent methodological layer with its own beneficiary/victim structure, confirming standalone status.',
    'Determines whether this story remains a separate constraint in the corpus or is absorbed into a sibling file; a wrong instantiation choice would either double-count one dispute as two or erase a genuine methodological arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation_choice, conceptual, 'Committer-frame omega: whether this reading of kernel ip_category_emergence warrants its own constraint file.').

omega_variable(
    anglophone_canon_relative_seam,
    'Does the seam''s structure depend on the Anglophone 1710 canon, such that civil-law genealogies (for example the 1791 French rights-of-author decrees) would split or join the axes differently?',
    'Run the M4/M5 divergence search on continental genealogies: if category coherence and claimant entry separate in droit d''auteur history where they coincide in the Anglophone record, the seam is canon-relative.',
    'A canon-relative seam narrows this constraint''s scope to Anglophone historiography and strengthens the excluded civil-law genealogists'' objection; a seam that replicates across traditions supports the kernel structure''s authenticity and raises the constraint''s effective reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anglophone_canon_relative_seam, empirical, 'Whether the seam generalizes beyond the Anglophone canon.').

omega_variable(
    enforcement_ratchet_reversibility,
    'Is the rising enforcement intensity defending the unified frame a reversible response to revisionist pressure, or a ratchet that will persist after the seam dispute settles?',
    'Track gatekeeping behavior after a decisive M4/M5 verdict: if reviewer and curriculum practices relax once the dispute is resolved, the trajectory was pressure-responsive; if enforcement holds at peak levels against a settled question, it has ratcheted.',
    'Reversible enforcement supports decay of this arrangement toward a plain coordination standard after resolution; a persistent ratchet shifts the arrangement''s character toward coercion-sustained maintenance with the gatekeeping seat as durable collector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_reversibility, empirical, 'Whether the enforcement build-up is cyclical pressure response or a one-way ratchet.').

omega_variable(
    centenary_oscillation_attribution,
    'Are the periodic theater spikes around statutory anniversaries an intermittent-reinforcement mechanism that actively re-singularizes the event, or incidental ceremonial culture riding on an independently stable frame?',
    'Compare citation-canonicalization and syllabus-adoption rates for single-event accounts in windows following centenary peaks against matched control periods; a significant post-peak lift indicates the oscillation does maintenance work.',
    'If the oscillation is a mechanism, the theater trajectory understates the frame''s active maintenance and effective extraction runs higher than the scalar suggests; if it is noise, the theater series should be flattened and the maintenance story rests entirely on the enforcement ratchet.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(centenary_oscillation_attribution, empirical, 'Attribution of the centenary-cycle theater oscillation to mechanism versus ceremony.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seam_collapse_test_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.3).
narrative_ontology:measurement(seam_collapse_test_tr_t20, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 20, 0.45).
narrative_ontology:measurement(seam_collapse_test_tr_t40, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 40, 0.28).
narrative_ontology:measurement(seam_collapse_test_tr_t60, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 60, 0.25).
narrative_ontology:measurement(seam_collapse_test_tr_t80, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 80, 0.27).
narrative_ontology:measurement(seam_collapse_test_tr_t100, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 100, 0.33).
narrative_ontology:measurement(seam_collapse_test_tr_t120, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 120, 0.46).

% Extraction over time
narrative_ontology:measurement(seam_collapse_test_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(seam_collapse_test_be_t20, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(seam_collapse_test_be_t40, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(seam_collapse_test_be_t60, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(seam_collapse_test_be_t80, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 80, 0.61).
narrative_ontology:measurement(seam_collapse_test_be_t100, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 100, 0.64).
narrative_ontology:measurement(seam_collapse_test_be_t120, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 120, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(seam_collapse_test_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(seam_collapse_test_su_t20, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(seam_collapse_test_su_t40, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(seam_collapse_test_su_t60, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(seam_collapse_test_su_t80, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 80, 0.47).
narrative_ontology:measurement(seam_collapse_test_su_t100, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(seam_collapse_test_su_t120, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 120, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'IP emerged in 1710' measures differently by observable, so it is three stories. thinkability_reading authors epsilon for the category-coherence arrangement; first_holding_reading authors epsilon for the claimant-entry arrangement; this file authors epsilon for the unified single-event frame as the seam reading assesses it. Direction of influence: the substantive siblings supply the candidate divergence cases the M4/M5 test consumes, while this reading's verdict feeds back into both siblings' legitimacy conditions and funding prospects (declared as influences edges in cs_structure.reading_relations). No member of the family is orphaned; each links to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
