% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__contingent_thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__contingent_thinkability_reading, []).

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
 *   constraint_id: zero_as_number_entry__contingent_thinkability_reading
 *   human_readable: Contingent Thinkability of Zero-as-Number (Transmission-Decisive Reading)
 *   domain: history of mathematics / philosophy of mathematics / conceptual history
 *
 * SUMMARY:
 *   A historiographical thesis has become an operative constraint:
 *   zero-as-number became thinkable in Europe only through contact with
 *   Indian and Islamic mathematics, and absent that transmission the
 *   Greek/Aristotelian framework — number as plurality of units of substance,
 *   the barred void, refusal of 'nothing' as an arithmetical object —
 *   precludes indigenous generation. Institutionalized through curricula,
 *   textbook narratives, funding frames, and review norms, the account
 *   assigns priority credit to non-Western traditions and imposes a
 *   dependency admission on the European tradition's autonomy narrative. KEY
 *   AGENTS (by structural relationship): - indian_mathematical_tradition:
 *   primary beneficiary (organized/identity_locked) -
 *   islamic_mathematical_tradition: primary beneficiary
 *   (organized/identity_locked) - postcolonial_historians: beneficiary and
 *   agenda-setter (institutional/constrained) -
 *   european_mathematical_tradition: primary target
 *   (institutional/identity_locked) - universalist_history_scholars:
 *   secondary target (moderate/constrained) -
 *   greek_computational_practice_scholars: excluded voice (moderate/trapped)
 *   - transmission_philologists: analytical observer (sees the manuscript
 *   structure whole). This file instantiates the contingent_thinkability
 *   reading of kernel zero_as_number_entry; sibling readings are separate
 *   constraints linked below. Claim and metrics are authored independently:
 *   claimed type is tangled_rope — genuine coordination (correction of a real
 *   credit misallocation on a real documentary trail) joined to asymmetric,
 *   actively enforced extraction — while the metrics describe substantially
 *   extractive operation. The epsilon referent is the standing arrangement
 *   under contest, the institutionalized transmission-decisive
 *   historiography, as this reading itself assesses it.
 *
 * KEY AGENTS:
 *   - indian_mathematical_tradition: primary beneficiary (organized/identity_locked) — collects restored priority recognition
 *   - islamic_mathematical_tradition: primary beneficiary (organized/identity_locked) — collects conduit-and-developer credit
 *   - postcolonial_historians: beneficiary and agenda-setter (institutional/constrained) — administers the account and accrues its gains
 *   - european_mathematical_tradition: primary target (institutional/identity_locked) — bears the dependency admission
 *   - universalist_history_scholars: secondary target (moderate/constrained) — bears professional costs of dissent
 *   - greek_computational_practice_scholars: excluded voice (moderate/trapped) — holds barrier-softening evidence outside the venues
 *   - transmission_philologists: analytical observer (analytical/analytical) — sees the documentary structure whole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, 0.66).
domain_priors:suppression_score(zero_as_number_entry__contingent_thinkability_reading, 0.57).
domain_priors:theater_ratio(zero_as_number_entry__contingent_thinkability_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__contingent_thinkability_reading, tangled_rope).
narrative_ontology:human_readable(zero_as_number_entry__contingent_thinkability_reading, "Contingent Thinkability of Zero-as-Number (Transmission-Decisive Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__contingent_thinkability_reading, "history of mathematics / philosophy of mathematics / conceptual history").

domain_priors:requires_active_enforcement(zero_as_number_entry__contingent_thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__contingent_thinkability_reading, 'd7b778ed-4105-44e4-be2d-ea4a25e57e03').
narrative_ontology:cs_kernel_codification('d7b778ed-4105-44e4-be2d-ea4a25e57e03', distributed).
narrative_ontology:cs_authority_grounding('d7b778ed-4105-44e4-be2d-ea4a25e57e03', expertise).
narrative_ontology:cs_interpretation_layer_present('d7b778ed-4105-44e4-be2d-ea4a25e57e03').
narrative_ontology:cs_reading_relation('d7b778ed-4105-44e4-be2d-ea4a25e57e03', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7b778ed-4105-44e4-be2d-ea4a25e57e03', zero_as_number_entry__hybrid_scaffolding_reading, forecloses).
narrative_ontology:cs_axiom('d7b778ed-4105-44e4-be2d-ea4a25e57e03', foundational, greek_framework_barriers_block_indigenous_zero).
narrative_ontology:cs_axiom_status(greek_framework_barriers_block_indigenous_zero, holdable).
narrative_ontology:cs_axiom_grounding('d7b778ed-4105-44e4-be2d-ea4a25e57e03', greek_framework_barriers_block_indigenous_zero, empirically_contingent).
narrative_ontology:cs_axiom('d7b778ed-4105-44e4-be2d-ea4a25e57e03', foundational, priority_credit_follows_documented_transmission).
narrative_ontology:cs_axiom_status(priority_credit_follows_documented_transmission, holdable).
narrative_ontology:cs_axiom_grounding('d7b778ed-4105-44e4-be2d-ea4a25e57e03', priority_credit_follows_documented_transmission, deontological).
narrative_ontology:cs_reference_frame('d7b778ed-4105-44e4-be2d-ea4a25e57e03', transmission_constituted_thinkability).
narrative_ontology:cs_drift_state('d7b778ed-4105-44e4-be2d-ea4a25e57e03', contemporary_abacist_reassessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d7b778ed-4105-44e4-be2d-ea4a25e57e03', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, indian_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, islamic_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, postcolonial_historians).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, universalist_history_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed the earliest fully positional decimal notation with a dedicated zero sign and the first explicit rules for operating on zero as a number. Under the reading's ascendancy its representatives collect restored priority in textbooks, museum narratives, commemorations, and scholarly credit. The recognition claim is the tradition's restored standing; abandoning it would dissolve that standing, so the seat holds its position rather than trading on alternatives.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, indian_mathematical_tradition, beneficiary,
    organized, civilizational, identity_locked, global).

% Synthesized and transmitted the positional system westward; al-Khwarizmi's arithmetic and the Arabic intermediary corpus are the hinge the account turns on. Its modern representatives — scholars of Arabic science, heritage institutions — collect curricular presence and credit as both conduit and developer. Like the Indian seat, its recognition claim is constitutive; exiting the claim would erase the restored standing.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, islamic_mathematical_tradition, beneficiary,
    organized, civilizational, identity_locked, global).

% Inherits a self-narrative running Greeks to Renaissance to modern mathematics as an internally generated achievement. Wherever the account governs, its curricula, commemorations, and canonical histories must carry the dependency admission: modern numeration arrived from outside and could not have arisen internally. Revising the narrative is constitutive-costly; dropping it altogether would dismantle the identity on which the tradition's institutions are built.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition, payer,
    institutional, civilizational, identity_locked, global).

% Historians and philosophers who treat mathematical concepts as culture-independent discoveries and read transmission-centered accounts as sociological overlay on a fixed mathematical reality. Their grant applications, submissions, and panel proposals meet skeptical review under the prevailing account; switching research programs means abandoning accumulated expertise, staying means paying standing. Early-career members bear the sharpest costs.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, universalist_history_scholars, payer,
    moderate, biographical, constrained, global).

% Run the journals, edited volumes, curriculum panels, and funding streams through which the account is administered. They accrue citations, positions, consultancies, and convening authority from the account's centrality, and enforce its norms at review and in syllabus standards. Their authority depends on the account remaining the organizing frame; pivoting to rival framings would forfeit the position they have built inside it.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, postcolonial_historians, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__contingent_thinkability_reading, postcolonial_historians, agenda_setter).

% Specialists documenting pre-transmission Hellenistic and European computational practices that complicate the absolute-barriers premise: sexagesimal placeholder conventions, counting-board positional computation, computus reckoning. Their findings circulate in specialist literature but are shut out of syntheses, textbooks, and the flagship venues where the account is debated; objecting from inside those venues is effectively closed, and their expertise binds them to the subject.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, greek_computational_practice_scholars, excluded,
    moderate, biographical, trapped, regional).

% Trace the manuscript chains — Sanskrit treatises, Arabic redactions, twelfth-century Latin translations, Fibonacci's Liber Abaci — establishing what moved, when, and through which hands. They hold the documentary structure whole and take no seat in the credit dispute; their codices are the shared evidentiary floor beneath every reading.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, transmission_philologists, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__contingent_thinkability_reading, postcolonial_historians).
narrative_ontology:fixing_cost_class(zero_as_number_entry__contingent_thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates priority credit for zero-as-number across civilizations according to the documented transmission record, replacing mutually inconsistent national origin myths with a single teachable chronology: Sanskrit positional notation with an operable zero, Arabic synthesis and transmission, Latin adoption.
% TRANSFER_FUNCTION: Moves intellectual status and curricular presence toward the Indian and Islamic traditions and the scholars who administer their recognition; moves the dependency admission — a revision cost borne in narrative and institutional self-description — onto the European tradition, and professional costs onto scholars holding universalist accounts.
% ABSENT_VOICES: Greek-computational-practice scholars hold manuscript evidence that softens the absolute-barriers premise and sit outside the syntheses and flagship venues; universalist historians object from a shrinking professional margin. Neither is seated on the curriculum boards and journal editorial layers where the account is administered.
% DISAPPEARANCE_RATIONALE: If the account vanished overnight, the entire credit-allocation settlement would reopen: curricula rewritten again, the postcolonial historiography built on the transmission-decisive frame loses its organizing spine, the European tradition's narrative obligations lapse, and the counterfactual question returns as an open contest rather than a settled premise.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century histories credited Europe with autonomous invention of modern numeration while manuscript evidence showed transmission from Indian and Arabic sources; the arrangement was built to correct that misallocation and to stop the recurring erasure of non-Western origins.
% FOUNDING_PROBLEM_CORROBORATION: The original misallocation is corroborated from outside the benefiting parties by the documentary record itself — twelfth-century Latin translations of al-Khwarizmi's arithmetic and Fibonacci's 1202 Liber Abaci explicitly credit Arabic sources, attested by transmission philologists working the manuscripts, a seat with no stake in the dispute. The founding problem's current status is disputed across seats: postcolonial historians attest public curricula still require the correction, while universalist scholars attest the correction has overshot into administered dogma.
narrative_ontology:disappearance_verdict(zero_as_number_entry__contingent_thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__contingent_thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__contingent_thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__contingent_thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__contingent_thinkability_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.66: the account does real explanatory work — the transmission is documentary fact — but its institutionalized form converts a historical finding into a boundary marker. The core counterfactual ('would not have emerged indigenously') is one no dataset can settle, yet it is administered as settled, and deviation carries review, funding, and syllabus costs. Suppression 0.57 is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation; the enforcement machinery here is discursive (review gating, standards, funding frames), not coercive, hence moderate rather than high. Theater 0.24: transmission acknowledgment is increasingly ceremonial in curricula whose actual research practice changes little — performative credit without redistributed attention. Accessibility collapse 0.45: rival readings survive at the margins rather than collapsing, which is itself evidence the constraint is constructed rather than natural-law-like. Resistance 0.52: sustained counter-scholarship meets the account wherever it hardens. Coordination typed identity_coordination: the account's primary ongoing function is boundary maintenance — who counts as rigorous historian versus Eurocentric apologist. Flagged per FNL guidance that identity framings can cover extraction: the coupling profile concentrates costs on low-power, wide-scope seats (early-career universalists), keeping the excess-extraction flag live despite the complexity offset. All tracked series share one time grid (0/10/20/30/40/50) so every metric is authored at every examined point; trajectories are monotonic (no cyclical dynamics), tracing the enforcement infrastructure maturing as the account moved from corrective thesis to administered standard.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From inside the gatekeeping seat the arrangement is overdue correction maintained against backsliding — enforcement reads as rigor, the dependency admission reads as honesty. From the European-tradition seat and the universalist seat the identical structure reads as enforced asymmetry: a counterfactual administered as fact and a professional price on testing it. The two payer seats differ sharply in power — an institutionally embedded tradition versus precarious scholars — which frustrates coalition formation between them: the tradition's revision is slow and identity-laden, the scholars' dissent is fast but individually priced. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. The Indian and Islamic tradition seats sit near the beneficiary end (recognition flows to them; their identity-lock amplifies attachment to the arrangement). Postcolonial historians are dual-positioned: beneficiaries collecting career capital and administrators enforcing norms — the engine should damp their effective extraction toward subsidy, with the secondary agenda-setter role preventing full inversion. The European tradition is a high-directionality target amplified by identity_lock: trapped nearer the full-target end than a mobile agent would be, since exit means dissolving the narrative its institutions are built on. Universalist scholars are high-directionality targets with constrained exit — mobile enough to switch subfields at personal cost, locked enough that most pay. Transmission philologists occupy the analytical seat and fall outside the derivation. No directionality overrides were needed: beneficiary/victim plus exit options already produce the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the account as pure extraction (snare) ignores the genuine coordination function: the documentary record really does establish transmission, and reallocating credit solves a real misallocation — a snare verdict would license discarding the correction along with its enforcement apparatus. Reading it as pure rope ignores the enforcement asymmetry: the counterfactual core is administered as settled, dissent pays, and identifiable seats bear costs the arrangement's operators do not. Mandatrophy is not declared: the founding problem (anti-Eurocentric correction) remains contested-live, not outlived — the arrangement has not outlived its mandate so much as hardened past the point its mandate requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'Which reading of the zero-entry kernel correctly fixes the constraint''s structure — is European thinkability of zero-as-number transmitted content (this reading), triggered latent structure (hybrid), or ontologically fixed availability (universal)?',
    'Comparative adjudication across the three sibling files: whichever reading best survives the pre-transmission computational-practice evidence fixes the family''s beneficiary/victim geometry and epsilon profile.',
    'If the hybrid reading wins, this story''s epsilon falls (less transmitted content means a smaller dependency admission), the victim set contracts, and the foreclosure edge to the hybrid sibling inverts; if the universal reading wins, the victim set vanishes and measured extraction collapses toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this file is one reading of kernel zero_as_number_entry; sibling readings would redraw the beneficiary/victim sets and epsilon.').

omega_variable(
    barrier_absoluteness_counterfactual,
    'Were the Greek/Aristotelian metaphysical barriers truly prohibitive of indigenous European generation of zero-as-number, or did latent computational routes exist (counting-board positional practice, computus reckoning, sexagesimal placeholder conventions) that transmission merely accelerated?',
    'Systematic survey of pre-transmission Hellenistic and European computational artifacts for spontaneous movement toward positional-zero treatment, scored against the documented Sanskrit/Arabic developmental sequence by analysts outside the credit dispute.',
    'Latent-route evidence collapses this reading toward the hybrid sibling, lowers epsilon, and voids the dependency-admission claim against the European tradition; confirmed absence strengthens the reading''s core premise and its extractive profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_absoluteness_counterfactual, empirical, 'Testability of the reading''s core counterfactual — the load-bearing premise that indigenous generation was blocked.').

omega_variable(
    enforcement_extraction_separability,
    'Is the measured extraction produced by the historical thesis itself or by disciplinary boundary-policing riding on it?',
    'Compare career outcomes and review outcomes for counterfactual-testing proposals in venues where the thesis is openly contested versus venues that administer it as settled.',
    'If separable, reforming the gatekeeping layer restores the account to near-coordination operation without touching its content; if inseparable, the extraction is intrinsic to the reading''s institutional form and remediation requires reframing the counterfactual as open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_extraction_separability, empirical, 'Whether the extractive component belongs to the claim or to its administration.').

omega_variable(
    dependency_admission_valence,
    'Does bearing the dependency admission constitute imposed cost extracted from the European tradition, or warranted correction — a cost the tradition owes for a century of misallocated credit?',
    'Preference-theoretic adjudication separating the factual question (what the manuscript record shows) from the normative question (what bearing it should cost), surveyed among historiographers with no seat in the dispute.',
    'If the valence is corrective rather than extractive, epsilon drops sharply and the classification trends toward pure coordination; if extractive, the tangled-rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_admission_valence, preference, 'Whether the victim seat''s cost is imposition or owed payment — a values question the scalar metrics cannot settle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__contingent_thinkability_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_entry_contingent_tr_t0, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zero_entry_contingent_tr_t10, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(zero_entry_contingent_tr_t20, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(zero_entry_contingent_tr_t30, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(zero_entry_contingent_tr_t40, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(zero_entry_contingent_tr_t50, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 50, 0.24).

% Extraction over time
narrative_ontology:measurement(zero_entry_contingent_be_t0, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(zero_entry_contingent_be_t10, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(zero_entry_contingent_be_t20, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(zero_entry_contingent_be_t30, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(zero_entry_contingent_be_t40, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(zero_entry_contingent_be_t50, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 50, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(zero_entry_contingent_su_t0, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(zero_entry_contingent_su_t10, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(zero_entry_contingent_su_t20, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(zero_entry_contingent_su_t30, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(zero_entry_contingent_su_t40, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(zero_entry_contingent_su_t50, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 50, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__contingent_thinkability_reading, identity_coordination).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry__universal_discovery_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% 'How zero entered European mathematics' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the contingent reading (this file — transmission-decisive, high extraction on cultural contingency, victim set includes the European tradition's dependency admission), the hybrid scaffolding reading (latent receiver-side structure triggered by external scaffolding — intermediate extraction, thinner transmitted-content credit), and the universal discovery reading (ontological fixity — negligible extraction, no dependency admission, no priority rents). The upstream member is the shared documentary transmission record as established by transmission philology; this reading consumes it downstream and cites it as warrant, which is why its edges point outward to both siblings. Changing the observable used to evaluate 'zero's entry' changes epsilon because the observables belong to different constraints, not because one constraint is observer-relative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
