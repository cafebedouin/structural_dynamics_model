% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Syncretic Ontological Order (Syncretic Reading of the Shinbutsu Kernel)
 *   domain: religious/historical/ontological
 *
 * SUMMARY:
 *   Medieval Japan ran two large cult systems in parallel: kami worship
 *   rooted in locality, clan lineage, and the court's ritual calendar, and a
 *   Buddhist establishment with its own soteriology, ordination lines, and
 *   monastic economy. The syncretic commitment under honji-suijaku
 *   metaphysics — kami are descended traces (suijaku) of buddha-originals
 *   (honji), one cosmological order articulated at two levels — integrated
 *   them into a single ritual-political order of combined shrine-temples,
 *   kami rank grants, and state-protection liturgy. This story instantiates
 *   the syncretic reading of the kernel shinbutsu_ontological_commitment and
 *   of it alone: the epsilon referent is the standing medieval arrangement —
 *   the enforced integration of shrine and temple institutions under
 *   buddha-side interpretation — assessed by this reading's own lights. The
 *   sibling readings (partition, incoherence) are separate constraints, not
 *   averaged here. The claim/metrics gap is deliberate: the reading CLAIMS a
 *   unified, doctrinally coherent, mutually beneficial order, while the
 *   authored metrics describe substantially extractive, actively enforced
 *   operation in which Shinto autonomy is the structural casualty — the
 *   engine measures the divergence; the claim is not reconciled to the
 *   metrics.
 *
 * KEY AGENTS:
 *   - esoteric_doctrinal_academies: agenda-setting seat (institutional/arbitrage) — produces and adjudicates the kami-buddha identity framework through which every other seat's legitimacy runs
 *   - buddhist_monastic_complexes: primary beneficiary (institutional/arbitrage) — collects shrine revenues, offerings, and labor through the combined institutions; fields the enforcement capacity
 *   - integrated_shrine_priest_lineages: dual-positioned (organized/identity_locked) — collects rank, recognition, and resources; pays in doctrinal subordination of its kami
 *   - autonomous_shinto_priest_lineages: primary target (organized/identity_locked) — bears the suppression of kami-side autonomy; excluded from doctrinal adjudication
 *   - local_kami_cult_communities: diffuse payer with incidental benefit (moderate/constrained) — keeps festivals and cult sites under reframed meaning
 *   - imperial_court: beneficiary-administrator (institutional/constrained) — draws dual legitimacy and arbitrates, but only inside the framework's terms
 *   - rival_doctrinal_movements: excluded voice (organized/trapped) — preaches alternative ontologies outside the interpretive conversation
 *   - doctrinal_historiographers: analytical observer (analytical/analytical) — reconstructs the full structure, including the sibling readings this reading forecloses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.66).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.68).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Ontological Order (Syncretic Reading of the Shinbutsu Kernel)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/historical/ontological").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '5dca86bb-3056-4c64-ab6c-bcb3f2beed40').
narrative_ontology:cs_kernel_codification('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', formalized).
narrative_ontology:cs_authority_grounding('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', lineage).
narrative_ontology:cs_interpretation_layer_present('5dca86bb-3056-4c64-ab6c-bcb3f2beed40').
narrative_ontology:cs_reading_relation('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', shinbutsu_ontological_commitment__incoherence_reading, forecloses).
narrative_ontology:cs_axiom('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', secondary, buddha_side_hermeneutic_priority).
narrative_ontology:cs_axiom_status(buddha_side_hermeneutic_priority, holdable).
narrative_ontology:cs_axiom_grounding('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', buddha_side_hermeneutic_priority, theological).
narrative_ontology:cs_reference_frame('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', unified_dharmadhatu_order).
narrative_ontology:cs_drift_state('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', contemporary_historiography, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('5dca86bb-3056-4c64-ab6c-bcb3f2beed40', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, esoteric_doctrinal_academies).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, integrated_shrine_priest_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_priest_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cult_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cult_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, integrated_shrine_priest_lineages).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_metaphysics).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, chingo_kokka_state_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Tendai and Shingon doctrinal centers (Enryakuji, Onjoji, Koyasan) produce the commentaries and ordination curricula through which claims that a kami is a manifestation of a buddha are articulated and adjudicated. They define what a kami can be said to be, train the clergy who staff the combined shrine-temples, and can absorb a shrine's resistance by re-describing its kami as a trace of a buddha. Leaving the framework is not a live option: the framework is their own production and its maintenance is their institutional purpose.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, esoteric_doctrinal_academies, agenda_setter,
    institutional, generational, arbitrage, national).

% The great temple-shrine combinations (Todaiji, Kofukuji with Kasuga, Enryakuji with Hie) collect shrine revenues, pilgrim offerings, and labor through the institutions the unity doctrine licenses. They petition the court for kami rank elevations that bind shrines closer, and they field armed monastics to defend contested claims when re-description fails. Their gains are the most concentrated in the arrangement.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_complexes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_complexes, agenda_setter).

% Shrine lineages (Kasuga, Kamo, Hie, Sumiyoshi) operating inside the synthesis receive court rank, state recognition, and access to Buddhist ritual resources and prestige their cults could not command alone. They pay with doctrinal subordination: their kami are legible to the center only as manifestations of buddhas, and their own theological voice reaches the center only through Buddhist commentary. Their lineage identity is the kami cult itself, so exit would dissolve what they are.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, integrated_shrine_priest_lineages, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, integrated_shrine_priest_lineages, payer).

% Priestly lineages, most prominently at Ise, that refuse the trace-doctrine and assert the kami's autonomous primacy. They are outside doctrinal adjudication, their texts circulate under restriction, their claims are re-described by the academies as lower-level truths, and their shrines' political standing erodes as court favor follows the integrated lineages. Exiting would mean abandoning the cult that constitutes the lineage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_priest_lineages, payer,
    organized, generational, identity_locked, regional).

% Village congregations keep their festivals, oracles, and cult sites, and gain access to Buddhist funerary and merit-making services through the combined institutions. They pay in reframed meaning — their kami are officially buddha-manifestations — and cult resources flow upward to the temple-shrine centers. Leaving would cost them either the cult or the funerary apparatus; most stay.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cult_communities, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cult_communities, beneficiary).

% The court grants kami ranks, arbitrates shrine-temple disputes, and draws legitimacy from both cult systems through the state-protection ideology the unity doctrine underwrites. It benefits from a single integrated ritual order, but its arbitration operates only inside the framework's terms and its own legitimacy is woven into the arrangement it administers.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, agenda_setter).

% Movements rejecting the esoteric synthesis — Nichiren's attacks on kami-worship, Pure Land lineages indifferent to kami ontology, later Yoshida Shinto's inversion of the trace-doctrine — stand outside the doctrinal conversation the academies adjudicate. They can preach alternatives and attract followers, but cannot compel a seat at the interpretive table; the center re-describes their objections rather than answering them.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, rival_doctrinal_movements, excluded,
    organized, biographical, trapped, national).

% Modern scholars of Japanese religion, from the postwar kenmitsu-taisei historiography onward, reconstruct the arrangement from dispute records, shrine documents, and doctrinal corpora, and assess whether the coherence the doctrine claims was lived, enforced, or negotiated. From this seat the reading's own framing is visible as one framing among siblings.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, doctrinal_historiographers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_complexes).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two institutionally distinct cult systems — kami cults rooted in locality, clan lineage, and the court ritual calendar, and a Buddhist establishment with its own soteriology, ordination lines, and monastic economy — into one workable ritual-political order: shared festival calendars, combined shrine-temples (jingūji), a single state-protection ideology, and mutual legitimation among court, shrines, and monastic centers.
% TRANSFER_FUNCTION: Moves doctrinal authority and interpretive control from shrine lineages to the Buddhist academies; moves court recognition, rank, and ritual resources to shrines that accept reframing under buddha-identity; moves shrine revenues, offerings, and labor toward the great temple complexes through the combined institutions.
% ABSENT_VOICES: The autonomous kami-side — Ise priestly lineages and anti-Buddhist cult partisans — is structurally absent from doctrinal adjudication; present, they would contest the trace-status of kami as the founding act of subordination. Later rival movements (Nichiren, Yoshida Shinto) are likewise outside the conversation; their objections are re-described by the center rather than answered.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would unravel the combined shrine-temple institutions, the kami rank system, the state-protection liturgy, and the ordination curricula training combined clergy; the court would lose its unified ritual apparatus and shrine-temple jurisdiction would revert to open contest. The medieval ritual-political order is organized around this commitment.
% FOUNDING_PROBLEM: Two parallel cult systems with competing claims on legitimacy, land, labor, and the court's ritual attention coexisted uneasily; the arrangement was built to make kami worship and Buddhist practice cohere within one polity without permanent jurisdictional war between shrines and temples.
% FOUNDING_PROBLEM_CORROBORATION: Court chronicles and shrine-temple dispute records — sources outside the Buddhist beneficiary set — attest the jurisdictional conflict the arrangement was built to end. Whether it ended or displaced the conflict is disputed: the academies attest a solved problem; shrine-side documents (Ise records) attest continued subordination; and modern kenmitsu-taisei historiography, written from outside every beneficiary set, corroborates that the arrangement persisted through enforcement and institutional capture rather than self-evident doctrinal coherence.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) is substantial but bounded: the arrangement moves interpretive authority, revenue, and labor from shrine-side to temple-side, yet also delivers real goods to paying seats — state rank, funerary and merit-making services, ritual integration — so the transfer is not pure rent. Suppression (0.68) is high because persistence depends on actively containing the kami-side alternative: restricted circulation of anti-syncretic texts, re-description of dissent as lower-level truth, court favor channeled to integrated lineages, and armed enforcement by monastic complexes where re-description failed. Theater (0.31) is low-moderate: the doctrinal apparatus performs real integrative work across the interval, but an increasing share of late-medieval activity defends the hierarchy rather than producing integration. Accessibility_collapse (0.55): alternatives persist at the margins — Ise's internal traditions, congregational dual practice, eventually Yoshida Kanetomo's inversion — but any shrine seeking central legitimacy must work through buddha-identity. Resistance (0.5): sustained, organized kami-side resistance across the whole interval, visible in dispute records and rank contests, short of systemic revolt. The three metric series run on one shared time grid (all metrics at all eight points); suppression_requirement is tracked because enforcement capacity is the story's dynamic — the machinery (warrior monks, edicts, doctrinal gatekeeping) visibly built up over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the academies' seat the arrangement is a doctrinal achievement: the framework is their production and every seat's legitimacy runs through it, so the structure presents as pure coordination. From the autonomous shrine lineages' seat the same structure is subordination with a liturgy: their cult's meaning is set elsewhere and their exit dissolves their identity. The dual-positioned integrated lineages should compute intermediate: they genuinely collect and genuinely pay, and identity lock makes exit costly in both directions. The court's seat is near-symmetric — real dual legitimacy, real dependency. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The academies and great complexes sit near the beneficiary end: they collect the transfer and control the rules, with arbitrage-grade exit because any resistance can be re-described inside the framework they author. Integrated shrine lineages derive low-to-mid d from their beneficiary declaration — they do collect rank and resources — while their identity lock and doctrinal subordination hold their lived position near the middle; the omega on their net position records the residual ambiguity rather than an override, because the structural data genuinely supports both readings. Autonomous lineages and local cult communities are the targets: they bear the transfer and the reframing, with identity-locked or constrained exit. The court is near-symmetric. Rival movements are excluded rather than coordinated — their exclusion is part of what the enforcement maintains. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Pure extraction would erase the genuine coordination: the integration solved a real pluralism problem and delivered goods to most seats, which is why it held for seven centuries and why its dissolution in 1868 met violent resistance. Pure coordination would erase the asymmetry the structural delta names: the coordination's terms were set by the buddha-side, the kami-side paid in autonomy, and active enforcement was required to hold the terms. The founding problem is authored contested rather than dead: the pluralism it managed was real, but whether it was solved or conquered is exactly what the sibling readings dispute. The R5 mismatch cell (contested status x world_rearranges verdict) is the honest one: no zombie flag, and no clean coordination story either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel shinbutsu_ontological_commitment — the syncretic reading. What would the classification become if the same kernel were instantiated under a sibling reading?',
    'Generate the sibling stories (partition_reading, incoherence_reading) as separate constraints and compare computed classifications across the kernel family; the delta in victim sets and epsilon locates the disagreement structurally.',
    'Under the partition reading the victim set shifts to whoever is harmed by domain separation and the buddha-side transfer largely disappears; under the incoherence reading the arrangement may fail to certify as a stable commitment at all. This story''s classification is valid only for the syncretic reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this is the syncretic reading of the shinbutsu kernel; the siblings are separate constraints, not averaged here.').

omega_variable(
    coherence_enforcement_ambiguity,
    'Is the doctrinal coherence this reading asserts a property of the commitment itself, or an artifact of enforcement suppressing the alternatives that would have revealed incoherence?',
    'Compare doctrinal production across periods of strong versus weak enforcement capacity; examine shrine-lineage documents for internal dissent that the official corpus smooths over.',
    'If coherence is enforcement-produced, extractiveness rises, the enforcement share of persistence grows, and the reading drifts toward the incoherence sibling''s structure; if genuinely lived, the coordination half of this reading is stronger than the extraction record suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_enforcement_ambiguity, empirical, 'Whether the unity doctrine''s coherence was lived or enforced.').

omega_variable(
    integrated_lineage_net_position,
    'Were the integrated shrine priest lineages net beneficiaries of the synthesis (rank, resources, prestige) or net payers (subordination of their kami and their theology)?',
    'Lineage revenue records, kami rank-grant patterns, and lineage-internal documents weighed against the subordination costs the lineages themselves recorded.',
    'If net payers, their directionality moves toward the target end and the arrangement shifts toward pure extraction; if net beneficiaries, the coordination function is more robust than the extraction reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integrated_lineage_net_position, empirical, 'Net position of the dual-positioned integrated shrine lineages.').

omega_variable(
    meiji_discontinuity_signal,
    'Does the violent resistance to the 1868 shinbutsu bunri separation indicate the unity was genuinely lived (supporting this reading), or that institutional interests defended their assets (supporting the incoherence sibling)?',
    'Analysis of post-separation popular practice: whether combined kami-buddha worship persisted at the congregational level after institutional dissolution, and what the rioting shrine and temple communities said they were defending.',
    'Genuinely lived unity strengthens this reading''s coordination claim; asset-defense reduces it and supports the sibling. This is the sharpest available natural experiment on the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_discontinuity_signal, empirical, 'What the Meiji separation reveals about whether the unity was lived.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 900, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t900, observed).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1000, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t1000, observed).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1100, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1100, 0.2).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t1100, observed).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1200, 0.23).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t1200, observed).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1300, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1300, 0.25).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t1300, observed).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1400, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1400, 0.27).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t1400, observed).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1500, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1500, 0.29).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t1500, observed).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1600, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1600, 0.31).
narrative_ontology:measurement_basis(shinbutsu_syncretic_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.44).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t900, observed).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1000, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1000, 0.5).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t1000, observed).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1100, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t1100, observed).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1200, 0.6).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t1200, observed).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1300, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1300, 0.63).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t1300, observed).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1400, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1400, 0.65).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t1400, observed).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1500, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1500, 0.67).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t1500, observed).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1600, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1600, 0.66).
narrative_ontology:measurement_basis(shinbutsu_syncretic_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.48).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t900, observed).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1000, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1000, 0.52).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t1000, observed).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1100, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1100, 0.57).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t1100, observed).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1200, 0.62).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t1200, observed).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1300, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1300, 0.65).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t1300, observed).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1400, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1400, 0.67).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t1400, observed).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1500, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1500, 0.69).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t1500, observed).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1600, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1600, 0.68).
narrative_ontology:measurement_basis(shinbutsu_syncretic_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu-shugo' (kami-buddha harmonization) covers three structurally distinct claims about what the medieval arrangement ontologically committed to. This file is the syncretic reading (unity with buddha-side priority; high institutional integration, suppressed Shinto autonomy). The partition reading and the incoherence reading are separate constraints with their own epsilon, beneficiary/victim sets, and classifications; they are linked here as siblings of one kernel, not averaged into this story. The syncretic reading is historically upstream: the institutional order it describes is the arrangement the other two readings interpret or deny, so this story's edges run to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
