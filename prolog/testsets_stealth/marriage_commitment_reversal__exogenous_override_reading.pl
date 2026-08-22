% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercive Override of the LDS Marriage Commitment (Exogenous Override Reading)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   Between the Morrill Act (1862) and the Second Manifesto (1904), the
 *   United States federal government dismantled the LDS Church's
 *   plural-marriage practice through escalating coercion: criminal
 *   prosecution of practitioners, disfranchisement, corporate dissolution,
 *   and property escheatment, culminating in the Woodruff Manifesto (1890)
 *   suspending new plural marriages. This file instantiates the
 *   exogenous_override_reading of the marriage_commitment_reversal kernel:
 *   the reversal was produced by external federal coercion, not by internal
 *   doctrinal revision, and Section 132 remains canonized and unrevised — the
 *   principle is preserved while the practice is suspended, leaving a
 *   persistent doctrine-practice gap. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope (a genuine jurisdictional
 *   coordination function runs through the same structure that extracts
 *   institutional autonomy) while the authored metrics describe heavily
 *   extractive, actively enforced operation. The sibling readings —
 *   endogenous_reinterpretation_reading and practice_doctrine_gap — are
 *   separate constraint files linked through network.affects_constraints, not
 *   folded into this one.
 *
 * KEY AGENTS:
 *   - federal_government: agenda-setter and receipt-of-gain seat (institutional/arbitrage) — extracts institutional autonomy, controls enforcement intensity at will
 *   - lds_church_leadership: payer administering its own compliance (organized/identity_locked) — bears the loss of autonomy while running the compliance regime
 *   - plural_marriage_practitioners: primary target (moderate/trapped) — bear prosecution, disfranchisement, and abrogated covenants directly
 *   - rank_and_file_membership: dual beneficiary/payer (moderate/constrained) — gain statehood and normalization, inherit the suspended covenant and its costs
 *   - protestant_moral_reform_movement: secondary beneficiary (organized/mobile) — collected the policy outcome, bears no ongoing cost
 *   - fundamentalist_dissenters: excluded voice (powerless/identity_locked) — refused the suspension, excommunicated, carried the practice outside
 *   - federal_judiciary: analytical observer (institutional/analytical) — supplied the constitutional architecture (Reynolds, Late Corporation) without initiating or bearing anything
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.7).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercive Override of the LDS Marriage Commitment (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'bfadac7c-147b-42ce-a855-ee373e4f2e8a').
narrative_ontology:cs_kernel_codification('bfadac7c-147b-42ce-a855-ee373e4f2e8a', fixed_text).
narrative_ontology:cs_authority_grounding('bfadac7c-147b-42ce-a855-ee373e4f2e8a', lineage).
narrative_ontology:cs_interpretation_layer_present('bfadac7c-147b-42ce-a855-ee373e4f2e8a').
narrative_ontology:cs_reading_relation('bfadac7c-147b-42ce-a855-ee373e4f2e8a', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfadac7c-147b-42ce-a855-ee373e4f2e8a', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('bfadac7c-147b-42ce-a855-ee373e4f2e8a', foundational, unrevoked_principle_yields_only_to_compulsion).
narrative_ontology:cs_axiom_status(unrevoked_principle_yields_only_to_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('bfadac7c-147b-42ce-a855-ee373e4f2e8a', unrevoked_principle_yields_only_to_compulsion, empirically_contingent).
narrative_ontology:cs_axiom('bfadac7c-147b-42ce-a855-ee373e4f2e8a', foundational, duress_manifesto_is_not_doctrinal_revision).
narrative_ontology:cs_axiom_status(duress_manifesto_is_not_doctrinal_revision, holdable).
narrative_ontology:cs_axiom_grounding('bfadac7c-147b-42ce-a855-ee373e4f2e8a', duress_manifesto_is_not_doctrinal_revision, conventional).
narrative_ontology:cs_reference_frame('bfadac7c-147b-42ce-a855-ee373e4f2e8a', unrevoked_section132_binding_principle).
narrative_ontology:cs_drift_state('bfadac7c-147b-42ce-a855-ee373e4f2e8a', post_manifesto_compliance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bfadac7c-147b-42ce-a855-ee373e4f2e8a', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, protestant_moral_reform_movement).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, reynolds_belief_action_distinction).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, plenary_territorial_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and the Justice Department criminalized plural marriage (Morrill Act 1862, Edmunds Act 1882, Edmunds-Tucker Act 1887), dissolved the church's corporate charter, escheated its surplus property, disfranchised practitioners, and sustained prosecutions through federal marshals and commissioners. The same statutory machinery could be tightened or relaxed at will, and the prospect of Utah statehood gave Washington a lever it could hold or release. The institutional autonomy surrendered by the church accrued to this seat.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, continental).

% National reform associations and denominational presses campaigned for three decades for federal suppression of plural marriage, supplying the political demand behind the statutes. Once the practice was suspended they collected the policy outcome they had sought and redirected attention to other causes, bearing almost none of the arrangement's ongoing cost.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, protestant_moral_reform_movement, beneficiary,
    organized, generational, mobile, national).

% The First Presidency and Quorum of the Twelve directed the church's response: leaders hid from arrest, tested the statutes in court, managed a scattered membership, and finally issued the 1890 Manifesto suspending new plural marriages. They lost corporate existence, property, and control over their own marriage practice, and they now administer the compliance regime that keeps the church inside civil law. Exit would mean dissolving the institution or repudiating canonized revelation; the office they hold is constituted by continuity with both.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership, payer,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership, agenda_setter).

% Men and women in plural households faced imprisonment, fines, disfranchisement, and family separation; after 1890 their covenant form could no longer be entered or openly lived. Some relocated to colonies in Mexico and Canada; most simply bore the prohibition. Their commitments were made under the church's prior teaching and could not be unblessed by statute.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practitioners, payer,
    moderate, biographical, trapped, regional).

% Ordinary members gained an end to the prosecution climate, restored church property, Utah statehood, and normalized national citizenship. They also inherited a suspended covenant practice their own scriptures still teach, and they funded litigation, relocation, and the compliance apparatus through tithing. Leaving the church meant forfeiting community, temple ordinances, and identity at once.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_membership, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_membership, payer).

% Members who regarded the suspension itself as a betrayal of a living revelation had no seat in the accommodation negotiated between the First Presidency and federal authorities. Those who continued the practice after the 1904 Second Manifesto were excommunicated and later formed separate communities carrying the practice forward outside the church, prosecuted under the same statutes their ancestors had been promised protection from.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, fundamentalist_dissenters, excluded,
    powerless, generational, identity_locked, regional).

% The Supreme Court sustained the anti-polygamy statutes (Reynolds v. United States, 1879) and the corporate dissolution (Late Corporation of the Church of Jesus Christ of Latter-day Saints v. United States, 1890), drawing the belief/action line that made the enforcement architecture constitutional. It adjudicated among the seats, initiated nothing, and bore nothing.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolved a live jurisdictional conflict: a territorial community organized its family law around a covenant form the national legal order prohibited. The override established one marriage law across the territory, ended the competing-sovereignties standoff, and cleared the path to Utah statehood on national terms.
% TRANSFER_FUNCTION: Moved institutional autonomy — control over marriage practice, corporate existence, real property, and the political franchise of practitioners — from the LDS Church and its plural households to the federal government and the national monogamous marriage regime.
% ABSENT_VOICES: The people whose covenants were suspended — plural wives and husbands, and later those who refused the suspension — were not parties to the negotiation between the First Presidency and federal officials. The Manifesto was issued over their practice without their consent, and the fundamentalist objection was met with excommunication rather than representation.
% DISAPPEARANCE_RATIONALE: Remove the override overnight and the suspended practice resumes wherever adherents dare: the documented post-Manifesto covert continuance and the later fundamentalist schisms show the demand never died. Utah's statehood settlement, the returned-property arrangements, and the church's entire legal posture presuppose the suspension. The surrounding arrangements depend on it.
% FOUNDING_PROBLEM: Congress framed the problem as a territory within the republic maintaining a marriage practice contrary to federal law, sustained by a corporate religious polity claiming independent sovereignty — a jurisdictional and moral disorder to be eliminated before statehood could be granted.
% FOUNDING_PROBLEM_CORROBORATION: Outside the federal beneficiary set, the First Presidency's own 1890-1891 declarations concede the coercive force behind the suspension, corroborating that the conflict and the threat were real even while disputing the federal framing; independent historical scholarship on the antipolygamy campaign attests both the reality of the jurisdictional conflict and the asymmetry of power that settled it. No source outside the federal apparatus attests that dissolving a church corporation was necessary to the stated problem.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends high (0.70) because the standing arrangement at interval end is a permanent curtailment of institutional autonomy: the practice stays suspended under a standing legal prohibition and the Smoot-hearings precedent, while Section 132 remains canonized — the church governs its own marriage doctrine only within limits someone else set. Suppression (0.62) is authored as a raw structural property, unscaled by power or scope: the enforcement machinery peaked in the late 1880s (leadership in hiding, mass prosecutions), decayed after amnesty and statehood, and re-hardened around the 1904 Second Manifesto — a coercion wave cycle visible in the series. Theater ratio rises monotonically after 1890 (0.32 to 0.52) because the compliance layer became increasingly performative: public conformity and sworn denials during the Smoot hearings over a documented substrate of post-Manifesto plural marriages, driving the proxy-for-function drift the temporal track watches for. Accessibility collapse (0.62) is substantial but incomplete — Mexico and Canada colonies, covert continuance, and eventual schism were real alternatives, unlike a natural law's. Resistance (0.42) reflects the end state: the great resistance era (litigation, concealment, exile, political organization) collapsed into compliance, leaving scattered fundamentalist refusal and scholarly contestation. Coalition failure among the victims is structural: the leadership's monopoly on sealing authority meant practitioners could not organize independently of the very office administering their compliance. All three tracked series run on one shared seven-point grid; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute different types from identical structural data. From the federal seat (arbitrage-grade exit, beneficiary-end directionality), the arrangement reads as lawful enforcement of a uniform family law and successful territorial integration — coordination it built and could unwind. From the leadership seat (identity_locked, high directionality despite administering compliance), the same structure is survival management under duress: an office constituted by continuity with both the institution and the canon, compelled to suspend what it cannot revise. From the practitioner seat (trapped, highest directionality), it is an abrogated covenant — the costs landed on households that never sat at the negotiating table. The rank-and-file sit near symmetric: real gains (statehood, peace, restored property) against a suspended practice their scriptures still teach. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. federal_government is declared beneficiary with arbitrage exit — it collects the autonomy transfer and can modulate enforcement costlessly — placing it near the full-beneficiary end. protestant_moral_reform_movement is a beneficiary but an indirect one: it collected the policy outcome and exited the scene, receiving none of the ongoing extraction; its d sits low but its receipt is nil. lds_church_leadership is declared victim with identity_locked exit — the derivation places it near the full-target end, correctly overriding what its secondary agenda_setter role might suggest: administering compliance is not profiting from it, and the identity lock (the office IS the continuity of institution and canon) removes exit entirely. plural_marriage_practitioners, victim and trapped, sit nearest the full-target end. rank_and_file_membership's dual beneficiary/payer position lands near symmetric. federal_judiciary is analytical and enters no extraction arithmetic. No directionality overrides are authored: the structural data plus exit options produce the correct d values, and the schema's power-atom-keyed override surface is too blunt to distinguish the two moderate-power seats (practitioners vs. rank-and-file) without corrupting each other's derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a territorial jurisdiction running family law against the national legal order — was formally resolved by statehood in 1896, yet the arrangement persists as a standing limitation on institutional autonomy layered over an unrevised canon. Authoring founding_problem_status=contested against disappearance_verdict=world_rearranges surfaces exactly the mismatch the R5 consumer cross-checks: the problem's resolution is disputed while the dependence is not. The tangled_rope classification does double duty here: it prevents the genuine coordination achievement (uniform law, statehood, an end to armed standoff) from being mislabeled as pure extraction, and it prevents the persistent autonomy extraction from being excused as a completed transition. The rising theater series marks where the arrangement risks decaying toward performance — compliance maintained as ritual over a practice already gone underground — without asserting that decay has completed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_of_reversal,
    'This constraint is one reading of kernel marriage_commitment_reversal — the exogenous_override_reading. The disagreement with the endogenous_reinterpretation_reading sits at the causal locus of the 1890 reversal (divine revelation versus federal compulsion), and the practice_doctrine_gap reading treats the resulting ambiguity itself as the constraint. What changes structurally if a sibling reading is adopted?',
    'Comparative classification across the three linked stories: the engine classifies each reading independently, and the divergence in computed types and effective extraction across the family is the measurement.',
    'Under the endogenous reading, epsilon drops toward coordination cost and the victim set thins (the church acts as its own agenda-setter); under the gap reading, the beneficiary/victim structure recedes and the persistent text-practice divergence becomes the classified object. This file''s high-extraction verdict holds only within the exogenous frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_reversal, conceptual, 'Committer-frame indexicality: the reversal''s causal locus determines which constraint is being measured.').

omega_variable(
    post_manifesto_continuance_extent,
    'How many new plural marriages were solemnized between the 1890 Manifesto and the 1904 Second Manifesto, and under what degree of official knowledge or sanction?',
    'Archival reconstruction: temple and colony records, sealed testimony from the Smoot hearings, and demographic analysis of post-1890 births in plural households.',
    'Large-scale continuance drives theater_ratio toward pure performance and pushes the arrangement toward extraction-with-cover; negligible continuance supports a sincere-compliance reading and lowers the theater trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_continuance_extent, empirical, 'Scope of covert post-Manifesto practice, the factual substrate of the theater ratio.').

omega_variable(
    woodruff_vision_status,
    'Was the September 23, 1890 vision an authentic revelatory event, or retrospective legitimation of a decision already forced by circumstance?',
    'Partially resolvable: contemporary diaries and correspondence predating the public account can establish whether the vision narrative preceded or followed the decision; the revelatory content itself is not document-resolvable.',
    'An authentic vision collapses this reading into the endogenous sibling and reclassifies the reversal as internal adaptation; demonstrated retroactive construction strengthens the exogenous account and the extraction verdict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(woodruff_vision_status, conceptual, 'Authenticity of the vision narrative — the exact seam between this reading and its endogenous sibling.').

omega_variable(
    counterfactual_persistence_without_threat,
    'Absent federal coercion, would the plural-marriage practice have persisted indefinitely?',
    'Comparative analysis of covenant communities facing no external prohibition, plus internal-strain indicators: economic burden of plural households, generational attrition rates, and leadership correspondence anticipating change.',
    'If internal abandonment was imminent, the override''s measured extraction shrinks toward hastening an inevitable adjustment; if persistence was indefinite, the override is the sole operative cause and the full extraction measure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_persistence_without_threat, empirical, 'Counterfactual baseline for attributing the reversal to coercion rather than internal evolution.').

omega_variable(
    section132_latent_restoration,
    'Does the church''s retained teaching that the plural-marriage principle will one day be restored constitute a live latent commitment or a dead letter?',
    'Track official discourse across generations: frequency and placement of restoration rhetoric, whether Section 132 remains in curriculum and temple instruction, and how leadership answers direct questions about the principle''s status.',
    'A live latent commitment keeps the doctrine-practice gap structurally loaded — the arrangement carries future-rearrangement potential and cannot settle into inertial maintenance; a dead letter converts the residue into pure history and lowers the standing extraction measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section132_latent_restoration, conceptual, 'Whether the preserved-as-principle clause of this reading names an operative commitment or a relic.').

omega_variable(
    obedience_internalization_split,
    'How much of the membership''s post-1890 compliance is structural (statutory prohibition plus remembered coercion) versus internalized (obedience-to-civil-law taught as covenant virtue)?',
    'Cross-generational attitude comparison: cohorts raised before versus after the Manifesto, measured against enforcement intensity at the time of formation.',
    'A large internalized share means the suppression travels with the community even where statutes lapse — the effective suppression exceeds the structural measure and persists independently of federal enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obedience_internalization_split, empirical, 'Structural versus internalized components of the compliance that sustains the doctrine-practice gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exo_override_tr_t0, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(exo_override_tr_t0, observed).
narrative_ontology:measurement(exo_override_tr_t8, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(exo_override_tr_t8, observed).
narrative_ontology:measurement(exo_override_tr_t11, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 11, 0.15).
narrative_ontology:measurement_basis(exo_override_tr_t11, observed).
narrative_ontology:measurement(exo_override_tr_t16, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(exo_override_tr_t16, observed).
narrative_ontology:measurement(exo_override_tr_t20, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(exo_override_tr_t20, observed).
narrative_ontology:measurement(exo_override_tr_t24, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(exo_override_tr_t24, observed).
narrative_ontology:measurement(exo_override_tr_t30, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(exo_override_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(exo_override_be_t0, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(exo_override_be_t0, observed).
narrative_ontology:measurement(exo_override_be_t8, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(exo_override_be_t8, observed).
narrative_ontology:measurement(exo_override_be_t11, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 11, 0.76).
narrative_ontology:measurement_basis(exo_override_be_t11, observed).
narrative_ontology:measurement(exo_override_be_t16, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 16, 0.82).
narrative_ontology:measurement_basis(exo_override_be_t16, observed).
narrative_ontology:measurement(exo_override_be_t20, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(exo_override_be_t20, observed).
narrative_ontology:measurement(exo_override_be_t24, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(exo_override_be_t24, observed).
narrative_ontology:measurement(exo_override_be_t30, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(exo_override_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(exo_override_su_t0, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(exo_override_su_t0, observed).
narrative_ontology:measurement(exo_override_su_t8, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(exo_override_su_t8, observed).
narrative_ontology:measurement(exo_override_su_t11, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 11, 0.86).
narrative_ontology:measurement_basis(exo_override_su_t11, observed).
narrative_ontology:measurement(exo_override_su_t16, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement_basis(exo_override_su_t16, observed).
narrative_ontology:measurement(exo_override_su_t20, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(exo_override_su_t20, observed).
narrative_ontology:measurement(exo_override_su_t24, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(exo_override_su_t24, observed).
narrative_ontology:measurement(exo_override_su_t30, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(exo_override_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% One colloquial event — 'the 1890 Manifesto ended plural marriage' — decomposes into three structurally distinct constraints per the epsilon-invariance principle: the endogenous_reinterpretation_reading (internal adaptive reinterpretation via revelation), this exogenous_override_reading (externally coerced reversal with an unrevised canon), and the practice_doctrine_gap (the persistent divergence between canon and conduct as the standing structure). Each carries its own epsilon, beneficiary/victim set, and classification. They are linked because the causal account in this file generates the downstream structural residue classified by the gap reading, while the endogenous reading competes for the same causal locus; measuring all three separately is what disambiguates the label 'the Manifesto' into precise claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
