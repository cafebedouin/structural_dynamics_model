% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: LDS Plural Marriage Reversal Under Federal Coercion (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This constraint story models the exogenous_override_reading of the
 *   marriage_commitment_reversal kernel: the LDS Church's 1890 cessation of
 *   plural marriage practice was driven by overwhelming federal coercion
 *   (Edmunds Act 1882, Edmunds-Tucker Act 1887, disincorporation threat,
 *   asset seizure) without internal doctrinal revision. Section 132 of the
 *   Doctrine & Covenants — the 1843 revelation commanding plural marriage —
 *   remains canonized scripture. The federal government extracted
 *   institutional autonomy (control over marriage definition, territorial
 *   governance, church corporate existence) as the price of Utah statehood
 *   and institutional survival. The doctrine-practice gap persists: the
 *   principle is preserved while the practice is suspended in public
 *   compliance. This reading authors high extractiveness (0.82) because the
 *   federal government is the structural beneficiary, and LDS institutional
 *   sovereignty is the primary victim.
 *
 * KEY AGENTS:
 *   - federal_territorial_control: Primary beneficiary (institutional/powerful) — extracts LDS autonomy over marriage definition and territorial governance as condition of statehood
 *   - federal_prosecutors: Secondary beneficiary (institutional/powerful) — wield legal machinery (Edmunds, Edmunds-Tucker) to coerce compliance
 *   - lds_institutional_sovereignty: Primary victim (institutional/powerful) — loses autonomy over marital theology, corporate existence, and territorial self-governance
 *   - polygamous_families: Direct victims (moderate/constrained) — face prosecution, property loss, family separation, exile
 *   - lds_members_under_prosecution: Victims (moderate/trapped) — subject to disfranchisement, testimony restrictions, legal penalties
 *   - lds_first_presidency: Agenda setter under coercion (institutional/constrained) — issues Manifesto to preserve institutional survival, claims revelation but structural pressure is documented
 *   - lds_quorum_twelve: Secondary agenda setters (organized/constrained) — sustain the Manifesto, manage the doctrine-practice gap
 *   - utah_statehood_advocates: Beneficiaries (organized/mobile) — achieve statehood through church compliance
 *   - fundamentalist_offshoots: Excluded (moderate/trapped) — reject Manifesto, continue practice, face ongoing suppression
 *   - historical_analysts: Observers (analytical/analytical) — assess causal structure of the reversal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "LDS Plural Marriage Reversal Under Federal Coercion (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '334091e6-c088-4965-869b-02d198218428').
narrative_ontology:cs_kernel_codification('334091e6-c088-4965-869b-02d198218428', formalized).
narrative_ontology:cs_authority_grounding('334091e6-c088-4965-869b-02d198218428', lineage).
narrative_ontology:cs_interpretation_layer_present('334091e6-c088-4965-869b-02d198218428').
narrative_ontology:cs_reading_relation('334091e6-c088-4965-869b-02d198218428', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('334091e6-c088-4965-869b-02d198218428', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('334091e6-c088-4965-869b-02d198218428', foundational, federal_coercion_drove_reversal).
narrative_ontology:cs_axiom_status(federal_coercion_drove_reversal, holdable).
narrative_ontology:cs_axiom_grounding('334091e6-c088-4965-869b-02d198218428', federal_coercion_drove_reversal, empirically_contingent).
narrative_ontology:cs_axiom('334091e6-c088-4965-869b-02d198218428', foundational, section_132_remains_binding_principle).
narrative_ontology:cs_axiom_status(section_132_remains_binding_principle, holdable).
narrative_ontology:cs_axiom_grounding('334091e6-c088-4965-869b-02d198218428', section_132_remains_binding_principle, conventional).
narrative_ontology:cs_reference_frame('334091e6-c088-4965-869b-02d198218428', prophetic_authority_under_persecution).
narrative_ontology:cs_drift_state('334091e6-c088-4965-869b-02d198218428', post_1890_manifesto, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('334091e6-c088-4965-869b-02d198218428', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_control).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_prosecutors).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, polygamous_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_members_under_prosecution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, utah_statehood_advocates).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, federal_supremacy_over_territorial_governance).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, state_monopoly_on_marriage_definition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls territorial governance and statehood admission. Uses marriage law as leverage to extract LDS submission to federal authority. Gains monopoly on marriage definition and territorial political order. Can walk away (arbitrage) — the constraint serves federal interests; if it ceased, federal control would not diminish.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_control, beneficiary,
    institutional, generational, arbitrage, national).

% Operate the legal machinery (Morrill, Edmunds, Edmunds-Tucker Acts) that prosecutes polygamists, seizes church assets, disincorporates the church, disfranchises voters. They set the enforcement agenda that forces the Manifesto. Mobile — they rotate assignments; the constraint is a career case, not their identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_prosecutors, agenda_setter,
    institutional, biographical, mobile, national).

% The corporate church entity loses autonomy over its defining theological practice, corporate existence (Edmunds-Tucker disincorporation), property (asset seizure), and territorial self-governance. Exit is constrained — compliance is the price of institutional survival and statehood; resistance means institutional death. The 1890 Manifesto is the capitulation document.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty, payer,
    institutional, generational, constrained, continental).

% Face direct prosecution, property confiscation, family separation (men imprisoned, women and children displaced), exile to Mexico/Canada. Exit options are constrained — they can flee (some do), go underground, or abandon families. The Manifesto does not protect existing families; it only stops new plural marriages.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, polygamous_families, payer,
    moderate, biographical, constrained, regional).

% Subject to disfranchisement (Edmunds Act), testimony restrictions (cannot serve on juries, hold office), criminal prosecution for cohabitation. Trapped — they are the rank-and-file who cannot flee easily; the constraint extracts their civil rights and religious practice directly.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_members_under_prosecution, payer,
    moderate, biographical, trapped, regional).

% Issues the 1890 Manifesto (Woodruff) and 1904 Second Manifesto (Smith). Publicly frames reversal as prophetic revelation; privately documents federal pressure. They administer the constraint but are also its primary institutional victim — their autonomy is extracted. Exit is constrained: refuse and the church is destroyed; comply and they preserve the institution at cost of theological coherence.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency, agenda_setter,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency, payer).

% Sustain the Manifesto, manage the doctrine-practice gap, excommunicate post-Manifesto plural marriage advocates (post-1904). They are the interpretive layer that absorbs the drift between Section 132 and practice. Constrained — they are bound by prophetic succession and institutional loyalty.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_quorum_twelve, agenda_setter,
    organized, biographical, constrained, continental).

% Non-LDS and moderate LDS political actors who achieve Utah statehood (1896) through church compliance. They gain congressional representation, federal infrastructure, and normalized territorial governance. Mobile — statehood is achieved; the constraint served its purpose for them.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, utah_statehood_advocates, beneficiary,
    organized, generational, mobile, regional).

% Reject the Manifesto, continue plural marriage practice, face ongoing federal and mainline LDS suppression. They are structurally excluded from the settlement — their existence proves the doctrine-practice gap is unresolved. Trapped — they cannot rejoin the mainstream without abandoning their core practice; they cannot practice openly without prosecution.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, fundamentalist_offshoots, excluded,
    moderate, generational, trapped, regional).

% Assess the causal structure of the 1890 reversal from outside the contested commitments. They see the federal legal pressure, the church's internal documents, the doctrine-practice gap, and the fundamentalist schism. They do not collect from or pay into the constraint; they model its classification.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the federal-territorial conflict over marriage definition and theocratic governance by imposing monogamous marriage as the condition of Utah statehood and institutional survival. The federal government coordinates a uniform marriage regime across territories; the LDS Church coordinates its institutional survival.
% TRANSFER_FUNCTION: Moves institutional autonomy (control over marital theology, corporate existence, territorial self-governance) from the LDS Church to the federal government, in exchange for statehood and cessation of prosecution. Moves civil rights and family integrity from polygamous families to the federal legal apparatus.
% ABSENT_VOICES: Polygamous women's direct testimony (largely filtered through male leadership or hostile federal courts); fundamentalist offshoots (excluded from the 1890/1904 settlements); rank-and-file LDS members who lacked voice in the Manifesto process; Native American nations in Utah Territory whose marital practices were also suppressed but are absent from the LDS-federal binary.
% DISAPPEARANCE_RATIONALE: If the exogenous override constraint vanished overnight, the LDS Church would face the question of whether Section 132 requires active plural marriage practice. Fundamentalist offshoots would claim vindication. The federal marriage monopoly would lose its founding enforcement precedent. Utah's statehood compact would be retrospectively questioned. The world rearranges because the doctrine-practice gap is a live structural tension, not a settled fact.
% FOUNDING_PROBLEM: Federal territorial governance required uniform marriage law and republican institutions; LDS theocratic governance required plural marriage as a defining religious practice. The two systems were incompatible in the same territory.
% FOUNDING_PROBLEM_CORROBORATION: Federal territorial governors, congressional records, and Utah statehood enabling acts corroborate that the federal-territorial governance conflict was resolved by 1896. LDS Church official histories acknowledge statehood was the goal. No non-beneficiary source attests that the theological requirement for plural marriage was resolved — only that practice was suspended.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.82: The federal government extracted near-total concession on the defining practice of LDS theology in exchange for institutional survival and statehood. The Morrill Anti-Bigamy Act (1862) began the pressure; Edmunds (1882) criminalized cohabitation; Edmunds-Tucker (1887) disincorporated the church and seized assets. The 1890 Manifesto was the capitulation. Suppression 0.78: Federal legal machinery actively prosecuted, disenfranchised, and seized property; the constraint's persistence required continuous enforcement. Theater 0.45: The Manifesto frames the reversal as prophetic revelation (Woodruff's vision), creating a performative layer over the coercive reality. Accessibility collapse 0.65: Alternatives (exile to Mexico/Canada, underground practice, fundamentalist schism) existed but were costly and marginal. Resistance 0.55: Significant armed standoffs (e.g., 1880s), legal challenges, and eventual fundamentalist schism show real but ultimately unsuccessful resistance. The measurement grid uses shared time points (1862, 1874, 1882, 1887, 1890, 1904) across all metrics — Morrill Act, Poland Act, Edmunds Act, Edmunds-Tucker Act, Manifesto, Second Manifesto.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, this is a rope/scaffold: coordination of territorial governance, resolution of the 'Mormon question,' enforcement of monogamous marriage norms. From the LDS institutional sovereignty seat, this is a snare/tangled_rope: extraction of theological autonomy under threat of institutional death, with the coordination function (statehood) as the price paid. From the polygamous families seat, this is a snare: pure extraction of religious liberty and family integrity. The engine computes per-seat χ from these structural positions; the claimed_type (tangled_rope) reflects this reading's assessment that genuine coordination (statehood, federal-territorial order) coexists with asymmetric extraction (LDS autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal territorial control and prosecutors are structural beneficiaries (d ~0.15): they gain territorial governance, statehood control, and monopoly on marriage definition. LDS institutional sovereignty is the primary target (d ~0.85): it loses autonomy over its defining theological practice and corporate existence. Polygamous families and prosecuted members are direct targets (d ~0.9): they bear prosecution, property loss, family destruction. LDS First Presidency is agenda_setter under coercion — they issue the Manifesto but their exit options are constrained (institutional death vs. compliance), placing them near the target end despite nominal authority. The doctrine-practice gap means the constraint extracts ongoing cognitive/internalized compliance (Section 132 binds in principle but not practice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal-territorial conflict over marriage definition and theocratic governance) was live in 1862-1890. By 1896 (Utah statehood) the territorial governance problem was solved. By 1904 (Second Manifesto) the federal prosecution apparatus had largely achieved its aims. Yet Section 132 remains canonized, and the church maintains that plural marriage is an eternal principle suspended only 'for the time being.' The arrangement persists without its founding problem — a textbook mandatrophy candidate. The constraint has not been formally retired (no sunset clause), the doctrine-practice gap is maintained theatrically, and fundamentalist offshoots demonstrate that the extraction remains enforceable. The church could renounce Section 132 (cheap fixing_cost) but does not, because the doctrinal ambiguity serves institutional identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel marriage_commitment_reversal, and does it instantiate the exogenous_override_reading specifically?',
    'Structural decomposition of the kernel into its constituent readings; this file declares the reading_id explicitly in cs_structure and commentary.kernel_context.',
    'If the kernel decomposition is rejected, this constraint merges with its siblings and loses its distinct extractiveness profile (high ε from federal coercion vs. low ε from internal revelation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this file is the exogenous_override_reading of the marriage_commitment_reversal kernel.').

omega_variable(
    doctrine_practice_gap_persistence,
    'Does the doctrine-practice gap (Section 132 preserved while practice suspended) represent a stable equilibrium or an unstable tension that must resolve?',
    'Longitudinal observation of LDS doctrinal discourse: if Section 132 is formally renounced or canonically suspended, the gap closes; if it persists indefinitely, the gap is a structural feature of the reading.',
    'If stable, the constraint is a permanent tangled_rope with built-in doctrinal incoherence; if unstable, it trends toward snare (if practice resumes) or scaffold (if doctrine is formally retired).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap_persistence, empirical, 'Whether the Section 132 preservation creates a permanent structural ambiguity.').

omega_variable(
    federal_coercion_vs_internal_revelation,
    'Was the 1890 Manifesto driven primarily by federal coercion (this reading) or by Woodruff''s vision reinterpreting divine will (endogenous_reinterpretation_reading)?',
    'Historical analysis of Woodruff''s journals, contemporary federal correspondence, and the sequence of legal defeats preceding the Manifesto.',
    'If coercion-driven, extractiveness is high (federal extraction of autonomy); if revelation-driven, extractiveness is low (internal coordination). The two readings author different ε for the same event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_coercion_vs_internal_revelation, empirical, 'Causal attribution of the practice reversal: exogenous coercion vs. endogenous revelation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of plural marriage practice structural (federal legal barriers, asset seizure, disincorporation) or internalized (LDS members accepting the Manifesto as binding revelation)?',
    'Post-1890 suppression trajectory: if plural marriage resumes in fundamentalist offshoots despite structural barriers, internalized suppression is weaker; if mainline LDS members treat plural marriage as doctrinally forbidden rather than suspended, internalized suppression is strong.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression after formal exit options open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the exogenous override reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.15).
narrative_ontology:measurement(marr_tr_t1874, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1874, 0.22).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.35).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.42).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.45).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1904, 0.38).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(marr_be_t1874, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1874, 0.45).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.62).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.75).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.82).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1904, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(marr_su_t1874, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1874, 0.45).
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.65).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.75).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.78).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1904, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__exogenous_override_reading, 0.1).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, lds_temple_covenant_structure).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, utah_statehood_constitutional_order).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the marriage_commitment_reversal kernel into three structurally distinct constraints with different ε values and victim/beneficiary structures. The exogenous_override_reading has high ε (federal coercion), the endogenous_reinterpretation_reading has low ε (internal revelation), and the practice_doctrine_gap models the persistent doctrinal ambiguity as its own constraint. They are linked via affects_constraints because the federal coercion reading creates the conditions for the doctrine-practice gap, and the endogenous reading is the church's official narrative that coexists with the exogenous reading in historical discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, institutional, 0.85).
constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
