% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This story instantiates the incoherence_reading of the
 *   shinbutsu_ontological_commitment kernel: shinbutsu-shugo (the combinatory
 *   kami-buddha practice tradition spanning roughly the Heian through Edo
 *   periods) is read here as having never achieved a stable ontological
 *   settlement about what kami-buddha co-veneration actually claimed
 *   metaphysically. Institutions tolerated this incoherence because it served
 *   administrative and coordination purposes better than resolution would
 *   have. This is a distinct constraint from the syncretic_reading (which
 *   holds honji-suijaku metaphysics as a genuinely unified cosmological
 *   order) and the partition_reading (which holds Shinto and Buddhism as
 *   separable domain-specific systems). The ε here reflects the specific
 *   claim that no fixed metaphysics existed at the practice level — a
 *   moderate, drift-prone extraction pattern (administrators benefiting from
 *   ambiguity at the expense of clarity-seeking practitioners and, later,
 *   Meiji state-builders), not the negligible ε of a settled syncretic
 *   cosmology nor the different extraction profile of a partition
 *   arrangement.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrators: primary beneficiary of maintained ambiguity (organized/constrained)
 *   - tokugawa_bakufu_religious_bureaucracy: institutional beneficiary using incoherence for administrative convenience (institutional/mobile)
 *   - lay_practitioners_seeking_doctrinal_clarity: bear the cost of unresolved metaphysics (powerless/trapped)
 *   - meiji_state_builders_pre_separation: inherit the incoherence as a costly obstacle to state Shinto construction (powerful/constrained)
 *   - honji_suijaku_theorists: excluded voice whose systematic metaphysics this reading discounts (moderate/identity_locked)
 *   - historians_of_japanese_religion: analytical observer reconstructing which reading the documentary record actually supports (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.42).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.28).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, piton).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, 'b41a3136-ae58-4f17-b19f-0cee54910e69').
narrative_ontology:cs_kernel_codification('b41a3136-ae58-4f17-b19f-0cee54910e69', implicit).
narrative_ontology:cs_authority_grounding('b41a3136-ae58-4f17-b19f-0cee54910e69', practice).
narrative_ontology:cs_interpretation_layer_present('b41a3136-ae58-4f17-b19f-0cee54910e69').
narrative_ontology:cs_reading_relation('b41a3136-ae58-4f17-b19f-0cee54910e69', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('b41a3136-ae58-4f17-b19f-0cee54910e69', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('b41a3136-ae58-4f17-b19f-0cee54910e69', foundational, no_settled_metaphysics_existed_at_practice_level).
narrative_ontology:cs_axiom_status(no_settled_metaphysics_existed_at_practice_level, holdable).
narrative_ontology:cs_axiom_grounding('b41a3136-ae58-4f17-b19f-0cee54910e69', no_settled_metaphysics_existed_at_practice_level, empirically_contingent).
narrative_ontology:cs_axiom('b41a3136-ae58-4f17-b19f-0cee54910e69', secondary, elite_theological_synthesis_did_not_penetrate_popular_practice).
narrative_ontology:cs_axiom_status(elite_theological_synthesis_did_not_penetrate_popular_practice, holdable).
narrative_ontology:cs_axiom_grounding('b41a3136-ae58-4f17-b19f-0cee54910e69', elite_theological_synthesis_did_not_penetrate_popular_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('b41a3136-ae58-4f17-b19f-0cee54910e69', practice_level_ritual_coordination_without_settled_metaphysics).
narrative_ontology:cs_drift_state('b41a3136-ae58-4f17-b19f-0cee54910e69', meiji_shinbutsu_bunri, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('b41a3136-ae58-4f17-b19f-0cee54910e69', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrators).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu_religious_bureaucracy).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders_pre_separation).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, practice_precedes_doctrine_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run combined shrine-temple complexes (jingu-ji) where kami are venerated alongside buddhas without a settled account of what, ontologically, is actually being worshipped. The lack of a fixed doctrine lets them absorb whichever ritual, land grant, or patronage arrangement is locally advantageous. Clarity would force a choice that could shrink their institutional footprint, so ambiguity is maintained rather than resolved.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrators, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrators, agenda_setter).

% Administers religious affairs through a temple registration system (terauke) that does not require doctrinal coherence between kami and buddha cults to function administratively. The incoherence is convenient: it lets the state regulate population and loyalty through religious registration without adjudicating a metaphysical dispute it has no interest in settling.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu_religious_bureaucracy, beneficiary,
    institutional, generational, mobile, national).

% Participate in combined rites — kami festivals, buddhist funerary rites, syncretic pilgrimage — without any institution offering a coherent account of what ontological claim is actually being made about kami-buddha relations. Those seeking a settled answer about what they believe find none on offer; the institutions that could provide one have no incentive to.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Beginning in 1868, attempt to construct a state Shinto distinct from Buddhism to underwrite imperial legitimacy and national identity. They inherit centuries of institutionally tolerated incoherence with no clean doctrinal seam to cut along, which is precisely why shinbutsu bunri (the forced separation) required violent, ad hoc administrative decree (haibutsu kishaku) rather than a tidy legal partition. The prior incoherence is a cost they must pay to achieve separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders_pre_separation, payer,
    powerful, generational, constrained, national).

% Medieval and early-modern scholar-monks who developed unified cosmological frameworks (kami as local manifestations of buddhas) precisely to resolve the incoherence this reading claims was never resolved. Their systematic metaphysics is treated, in this reading, as elite theological overlay on a practice-level incoherence that persisted underneath regardless of what doctrine was officially professed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, honji_suijaku_theorists, excluded,
    moderate, civilizational, identity_locked, national).

% Examine temple records, ritual manuals, and administrative documents to determine whether shinbutsu-shugo institutions operated on a coherent (if unstated) metaphysics, a genuinely separable dual system, or no stable ontological commitment at all. Their reconstructions are contested and depend heavily on which documentary layer (elite theological vs. local ritual practice) is weighted as authoritative.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutional tolerance of ontological ambiguity allowed shrine-temple complexes, ritual specialists, and the state religious bureaucracy to coordinate practice (shared ritual calendars, shared land, shared personnel) without requiring prior agreement on what was metaphysically true about kami and buddhas.
% TRANSFER_FUNCTION: Moves interpretive burden away from institutions (who need not resolve or defend a coherent doctrine) and onto lay practitioners and, later, onto Meiji state-builders (who must retroactively manufacture a separable doctrine where none was cleanly maintained).
% ABSENT_VOICES: Lay practitioners wanting doctrinal clarity are not organized into any body that could demand it; their situation is described only through later ethnographic and folklore reconstruction, not through contemporary self-testimony. Honji-suijaku theorists are excluded from this reading's account because their systematic metaphysics would, if credited, undermine the incoherence claim.
% DISAPPEARANCE_RATIONALE: If the tolerated incoherence had never existed — i.e., if a stable ontological commitment had been institutionally enforced from the outset — shrine-temple administration would have required an early doctrinal settlement (either full syncretic fusion or full partition), shrinking the space for locally opportunistic ritual arrangements. Whether this counts as 'the world rearranging' is disputed among historians who read the same institutional record as evidence for stable syncretism (syncretic_reading) or clean functional partition (partition_reading) rather than incoherence.
% FOUNDING_PROBLEM: Shinbutsu-shugo institutions needed to administer overlapping ritual, land, and population-registration functions across kami and buddha cults without a state or clergy strong enough (or interested enough) to impose a single settled metaphysics.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era officials attest, from outside the beneficiary set, that the arrangement's ambiguity was administratively costly once national identity construction required separable religious categories — this is precisely why haibutsu kishaku was violent and improvised rather than a simple legal partition. Some contemporary historians of religion (outside both the Tokugawa bureaucracy and the shrine-temple administrators) corroborate the absence of settled doctrine at the practice level, though this corroboration is contested by scholars defending the syncretic_reading.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) and rises gradually — the incoherence itself is not designed extraction, but institutions that benefit from ambiguity (administrators avoiding doctrinal accountability, bureaucrats avoiding metaphysical adjudication) accumulate a mild rent over centuries as the gap between practiced ritual and any settled account widens. Theater ratio is the more diagnostic metric here (rising from 0.3 to 0.58): as centuries pass, ritual performance increasingly substitutes for doctrinal commitment — the performative apparatus (rites, festivals, temple registration) persists and even intensifies while the underlying ontological question is never addressed, which is exactly the piton signature of atrophied function maintained by institutional inertia and theatrical continuation. Suppression is low-moderate (0.28) because no one is coerced into the ambiguity — it is tolerated, not enforced — but accessibility_collapse and resistance are both moderate-low (0.35, 0.4) since alternative, more doctrinally settled religious forms (strict Buddhist sectarianism, later State Shinto) remained available and were periodically advocated by identifiable factions.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple administrators and the bakufu bureaucracy sit near the beneficiary end: ambiguity is functionally convenient for both, and their exit options (constrained/mobile) reflect that they are not trapped by the incoherence — they profit from its persistence and could in principle resolve it but choose not to. Lay practitioners sit near the target end: trapped locally, biographical time horizon, no capacity to demand doctrinal resolution. Meiji state-builders are a distinctive case — powerful but constrained by history, they pay a real transition cost (the violence of haibutsu kishaku) precisely because the incoherence was never cleanly resolved; the derivation correctly assigns them victim status relative to THIS specific constraint even though they hold high general power, because their power cannot simply dissolve centuries of institutionally settled ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administering overlapping ritual/land/registration functions without a strong center capable of imposing settled metaphysics) is dead by the Meiji restoration — the state now has both the will and the administrative capacity to impose a settlement, and does so violently via shinbutsu bunri. Yet the disappearance_verdict is 'contested' rather than 'world_rearranges' because historians dispute whether the underlying practice-level reality was ever incoherent (this reading) or was instead a genuine syncretic system (syncretic_reading) that only appeared incoherent from a later, externally-imposed dualist frame. Classifying this as piton rather than snare avoids mislabeling institutional inertia as active extraction: no party enforces the incoherence through coercion, and no party is harmed severely enough to force resolution until an external actor (the Meiji state) with different incentives arrives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentary_layer_selection_bias,
    'Does the incoherence reading emerge from privileging administrative and popular-practice records over elite theological (honji-suijaku) texts, and would privileging the latter instead support the syncretic_reading?',
    'Systematic comparison of the ontological commitments implied by temple administrative records, popular ritual manuals, and elite doctrinal treatises across matched time periods and regions, weighted by relative circulation and practitioner exposure rather than survival bias in the textual record.',
    'If elite theological texts demonstrably shaped everyday practice (rather than existing as isolated scholastic overlay), the incoherence claim weakens substantially and the constraint''s ε should be read as closer to the syncretic_reading''s negligible extraction. If practice-level sources show no trace of doctrinal awareness even where theological texts circulated, the incoherence reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentary_layer_selection_bias, conceptual, 'Whether the incoherence claim is an artifact of which documentary layer is treated as authoritative.').

omega_variable(
    incoherence_vs_deliberate_non_commitment,
    'Was the absence of a stable ontological commitment a genuine cognitive/institutional incoherence, or a deliberate, functionally rational strategy of non-commitment that should not be characterized as ''incoherent'' at all?',
    'Examine whether institutional actors who benefited from ambiguity (shrine-temple administrators, bakufu bureaucrats) show evidence of consciously avoiding doctrinal settlement versus simply never having encountered the question as salient.',
    'If deliberate, the constraint is better modeled as a rope or tangled_rope (functional strategic ambiguity serving real coordination) rather than piton (atrophied/inertial incoherence). This omega documents the committer-level ambiguity in what ''incoherence'' means as a structural claim, distinct from the sibling readings'' claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incoherence_vs_deliberate_non_commitment, conceptual, 'Whether tolerated incoherence was strategic ambiguity or genuine unresolved indeterminacy.').

omega_variable(
    meiji_retrospective_construction,
    'Is the incoherence reading itself partly a retrospective construction by Meiji-era and modern scholars who needed shinbutsu-shugo to appear incoherent in order to justify or explain the violence of forced separation?',
    'Trace the historiography of the incoherence claim to determine whether it predates Meiji state-building projects or emerges primarily from post-separation justificatory scholarship.',
    'If the incoherence framing is substantially a Meiji-era retrospective construction, this constraint''s claimed_type and beneficiary structure would need revision — the Meiji state-builders would shift from victim (paying the cost of prior incoherence) toward beneficiary (the incoherence framing legitimizes their separation project).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_retrospective_construction, empirical, 'Whether the incoherence claim is historically prior to or retrospectively constructed by the separation project it is invoked to explain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 300, 0.5).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 400, 0.55).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 500, 0.58).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 200, 0.34).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 300, 0.38).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 400, 0.4).
narrative_ontology:measurement(shin_be_t500, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 500, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__incoherence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_ontological_commitment kernel, decomposed per the ε-invariance principle: syncretic_reading (stable unified cosmology, negligible-to-low ε, rope-flavored), partition_reading (stable domain separation without integration, low ε, rope-flavored), and this incoherence_reading (no stable commitment at any level, moderate rising ε, piton-flavored due to accumulating theatrical ritual maintenance over a genealogically dead founding problem). The three do not average into one ε; each is a structurally distinct claim about the same historical phenomenon and must be evaluated independently. Meiji shinbutsu bunri (forced separation, 1868) is the historical event whose interpretation differs sharply depending on which reading is adopted: under incoherence_reading, separation is costly precisely because no clean seam existed; under partition_reading, separation is comparatively cheap because functional domains were already distinct; under syncretic_reading, separation is a genuine rupture of a previously unified cosmological order.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
