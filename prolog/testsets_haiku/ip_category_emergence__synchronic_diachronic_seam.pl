% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Synchronic-Diachronic Seam in IP Category Emergence and First-Holding
 *   domain: legal/philosophical
 *
 * SUMMARY:
 *   This reading tests whether 1710 (the Statute of Anne) marks a single,
 *   unified moment when IP became thinkable AND authors became rights-holders
 *   (synchronic frame), or whether these are separable historical events
 *   compressed into one statute for institutional convenience. The synchronic
 *   reading is the dominant institutional frame in Anglo-American IP
 *   law—courts and legislatures treat 1710 as marking both thinkability
 *   emergence and occupancy transfer. Challengers (historians,
 *   temporal-boundary contestants, category skeptics) argue that thinkability
 *   (the intellectual/cultural concept of ownable expression) emerged
 *   gradually over the 17th century from changes in printing technology and
 *   circulation, while occupancy transfer (scribes/patrons → authors as
 *   rights-claimants) followed different economic logic and was contested
 *   well into the 18th century. This reading asks: are these two components
 *   formally independent (and thus the synchronic frame is a spurious
 *   compression), or do they necessarily co-occur (validating the kernel
 *   structure)? The answer determines whether the constraint is an authentic
 *   coordination mechanism or a tangled rope hiding temporal disaggregation.
 *
 * KEY AGENTS:
 *   - Statutory formalists: institutional authorities (courts, legislatures, IP offices) enforcing the 1710 synchronic reading through doctrine and precedent.
 *   - Temporal boundary contestants: scholars and jurists arguing for temporal independence, bearing the cost of maintaining the distinction against institutional resistance.
 *   - Category skeptics: theorists questioning whether 'intellectual property' is even a coherent category, suggesting 1710 authorial copyright and 2024 patent law are retrospective unifications.
 *   - Continental civil-law traditions: observing the English-language debate as parochial; their own IP systems ground authorship in personality and trace to different statutory moments.
 *   - Pre-1710 expression claimants: excluded from the dispute; their occupancy claims are retroactively foreclosed by the synchronic frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.62).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.41).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Synchronic-Diachronic Seam in IP Category Emergence and First-Holding").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal/philosophical").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, 'c885e932-6c21-4ba1-80b8-85a55f9a7bbd').
narrative_ontology:cs_kernel_codification('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', fixed_text).
narrative_ontology:cs_authority_grounding('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', extraction).
narrative_ontology:cs_interpretation_layer_present('c885e932-6c21-4ba1-80b8-85a55f9a7bbd').
narrative_ontology:cs_reading_relation('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_axiom('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', foundational, temporal_unity_principle).
narrative_ontology:cs_axiom_status(temporal_unity_principle, holdable).
narrative_ontology:cs_axiom_grounding('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', temporal_unity_principle, conventional).
narrative_ontology:cs_axiom('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', secondary, statutory_formalism_supremacy).
narrative_ontology:cs_axiom_status(statutory_formalism_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', statutory_formalism_supremacy, deontological).
narrative_ontology:cs_reference_frame('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', statutory_synchrony_1710).
narrative_ontology:cs_drift_state('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', contemporary_historiographical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c885e932-6c21-4ba1-80b8-85a55f9a7bbd', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, jurisprudential_coherence_doctrine).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, temporal_boundary_contestants).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, category_skeptics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The doctrine benefits from maintaining that 1710 (the Statute of Anne date) marks a single, coherent, synchronic moment when IP became thinkable AND when first occupancy transferred to authors simultaneously. This doctrine collects no rents directly but anchors institutional legitimacy of copyright regimes across jurisdictions.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, jurisprudential_coherence_doctrine, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ip_category_emergence__synchronic_diachronic_seam, jurisprudential_coherence_doctrine).

% Scholars, historians, and jurists who argue that thinkability (the intellectual/cultural possibility space of owning expression) and first-holding (the actual transfer of occupancy from scribes/patrons to authors as a legal class) are separable events, not coterminous. They bear the cost of maintaining this distinction in the face of institutional resistance to temporal fragmentation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, temporal_boundary_contestants, payer,
    powerful, biographical, constrained, global).

% Legal theorists and historians who question whether IP-as-a-category is even coherent across time, suggesting that calling both 1710 authorial copyright and 2024 patent law instances of the same 'intellectual property' system is a retrospective unification. They invest effort contesting the category itself, which requires continuous argumentation against the naturalized framing.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, category_skeptics, payer,
    moderate, biographical, constrained, global).

% Legal scholars and institutional authorities (courts, legislatures, IP offices) who read statutory text and legislative history as fixing the boundary at 1710 and treat that date as marking both the thinkability threshold and the first occupancy transfer. They enforce the interpretation through doctrine and precedent.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, statutory_formalists, agenda_setter,
    institutional, generational, mobile, global).

% The estates of pre-1710 authors (now deceased, their works in cultural commons or patron holdings) have no voice in this dispute. If the synchronic reading is wrong—if thinkability preceded occupancy transfer—the exclusion retroactively denies these claimants standing they might have had.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, pre_1710_expression_claimants, excluded,
    powerless, civilizational, trapped, universal).

% Systems that ground author's rights in personality and droit d'auteur rather than labor-occupation, and trace their coherence to different statutory moments (e.g., French Revolutionary era). They observe the English-language dispute about 1710 synchronicity as parochial—their own historical boundary-setting follows different logic entirely.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, continental_civil_law_traditions, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, diffuse).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single canonical moment (1710) when IP became thinkable and authors became rights-holders, collapsing temporal ambiguity into one interpretive coordinate so courts and legislatures can reference 'the foundation' of the system without indexing to multiple foundation dates.
% TRANSFER_FUNCTION: Transfers interpretive authority from historians (who track thinkability emergence over centuries) and sociologists (who track occupancy change through economic/social forces) to statutes and courts, centering institutional readings of legislative intent over lived historical contingency.
% ABSENT_VOICES: Actual 17th-century authors, patrons, and scribes, whose lived experience of who-gets-paid-for-expression is archived only through institutional documentation that courts now read for fixed intent. Pre-1710 expression claimants have no present standing. Non-Anglophone IP traditions are systematized separately and their boundary-logic is treated as incomparable rather than parallel.
% DISAPPEARANCE_RATIONALE: If the synchronic requirement vanished—if courts permitted thinkability and first-holding to be dated independently—the copyright system would not collapse, but its historical narrative would fragment. Cases would turn on which temporal frame (cognitive/cultural possibility vs. occupancy transfer) applied; international harmonization would require mediating different readings. Some argue the system would become more honest; others argue it would lose coherence.
% FOUNDING_PROBLEM: By 1710, the printing press and manuscript circulation had created economic pressure on authors and publishers. The Statute of Anne had to make copyright thinkable (express it in statutory language) AND transfer occupancy from patron-dominated scribal labor to author-as-claimant. The question is whether these happened in the same moment by the same act, or whether they were logically/chronologically separable events compressed into one statute.
% FOUNDING_PROBLEM_CORROBORATION: Statutory formalists and institutional authorities in Anglo-American IP law attest that 1710 is the unified foundation. Intellectual historians (e.g., Eisenstein, Johns, Pottage) and legal historians outside the IP establishment attest that thinkability had been emergent for a century and occupancy was still contested decades after 1710—the statute unified them retrospectively, not simultaneously. Neither camp speaks as outsiders; both are invested. No corroboration exists from a fully external epistemic seat.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is 0.62 because the constraint transfers interpretive authority from historians and sociologists to statutory courts, suppressing temporal nuance in favor of institutional coherence. Suppression is 0.41 (moderate): the synchronic frame is enforced through doctrine and precedent, but open historical scholarship remains possible—scholars can publish temporal disaggregation even if courts don't cite it. Theater is 0.58 (high): much institutional activity is devoted to maintaining the 1710 unity as the 'foundation,' but the actual dispute about thinkability vs. occupancy is increasingly visible in academic literature. The measurement series show extractiveness and theater rising toward 1710 as the Statute approaches (thinkability becomes codifiable, occupancy claims consolidate), then moderating slightly post-1710 as the institutional frame solidifies and no longer needs constant performative restatement. The claim of tangled_rope reflects that the constraint has both a real coordination function (fixing a boundary so courts can reference it) AND hidden asymmetric extraction (temporal clarity is suppressed to preserve institutional legitimacy).
 *
 * PERSPECTIVAL GAP:
 *   The statutory formalist seat computes the synchronic frame as genuine coordination—a way to resolve temporal ambiguity and give the system a knowable foundation. The temporal-boundary-contestant seat computes it as enforced extraction—a way to suppress inconvenient historical facts that would fragment institutional authority. The category-skeptic seat computes it as theatrical maintenance of a category that is itself spurious. The engine derives these divergences from the structural data: formalists have mobile exit (can revise the statute), contestants have constrained exit (must argue within institutional constraints), skeptics have moderate power (can publish but not legislate). The divergence is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   The jurisprudential coherence doctrine is the structural beneficiary (the synchronic frame shores up its authority and eliminates temporal nuance as a legitimate objection). Temporal-boundary contestants are the primary targets (they bear the cost of arguing against the institutional grain). Category skeptics are secondary targets (they must argue that the category itself is incoherent, which institutions resist more fiercely than disagreement about boundaries). Statutory formalists occupy the agenda-setter role but also a beneficiary position (the synchronic frame preserves their interpretive authority). The beneficiary declaration covers the doctrine, not the individuals, because the beneficiary is an institutional pattern, not an actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (expressing ownership of expression economically and legally after printing scaled circulation) remains partially live: copyright still exists because authors still produce under institutions claiming to represent it. But the occupancy-transfer problem (should 1710 or some other date mark the transfer) is increasingly dead—historical evidence about pre-1710 authorial consciousness and post-1710 occupancy contestation is well-documented and published. The synchronic reading persists not because the founding problem demands it, but because statutory authority enforces temporal unity. This is a live candidate for mandatrophy: the founding problem's death is masked by institutional performance of the synchronic frame. The constraint is not a false summit (it is genuinely extractive, not a natural law being naturalized), but it is a piton-adjacent tangled_rope—it coordinates something real (giving courts a reference date) while suppressing something real (temporal complexity that would make the system harder to govern but more honest).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_occupancy_independence,
    'Are thinkability (the logical/intellectual coherence of owning expression) and first-holding (the institutional transfer of occupancy to authors) formally independent events, or does one necessarily imply the other in the same moment?',
    'Comparative institutional history: examine whether thinkability concepts emerged at measurably different rates in parallel jurisdictions (Scotland, France, Netherlands) from occupancy transfer, or whether they track together across all cases.',
    'If independent, the synchronic reading is a spurious compression and the kernel structure (1710 as foundation) is revealed as a retrospective unification. If co-occurring, the synchronic reading is vindicated and thinkability/first-holding form a genuine structural unit. Classification would shift from tangled_rope (containing hidden temporal disaggregation) to rope (authentic coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_occupancy_independence, empirical, 'Whether the two components of the IP founding moment are logically independent.').

omega_variable(
    reading_dependence_on_statutory_authority,
    'Does the synchronic reading persist because it is true, or because statutory authority enforces it? That is, if courts and legislatures ceased to insist on the 1710 unity, would the constraint survive as an intellectual position?',
    'Counterfactual: jurisdictions that permit explicit temporal disaggregation (thinkability dated to one moment, occupancy transfer to another) and track outcomes for doctrinal coherence and dispute resolution.',
    'High enforcement-dependence would indicate the constraint is a snare (holding the system together by suppressing temporal clarity), not a rope. Low dependence would indicate the synchronic frame has independent conceptual force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_dependence_on_statutory_authority, conceptual, 'Whether the synchronic frame depends on institutional enforcement or has independent conceptual viability.').

omega_variable(
    continental_vs_anglo_american_incommensurability,
    'Do continental IP traditions (droit d''auteur, moral rights grounded in personality) rely on a different temporal synchrony, or are they genuinely measuring different kernels?',
    'Mapping continental founding moments (French Revolution-era legislation, Napoleonic codes) against the same thinkability/occupancy distinction and determining whether they show the same synchronic compression or different temporal relationships.',
    'If they compress identically, the synchronic seam is universal. If they disaggregate differently, the kernel itself is culturally contingent and the synchronic reading is parochial, not foundational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continental_vs_anglo_american_incommensurability, conceptual, 'Whether the synchronic-diachronic boundary is universal or culturally specific.').

omega_variable(
    statutory_formalism_vs_historical_reconstruction,
    'Is statutory text (the Statute of Anne) the authoritative voice on what 1710 meant, or is archival/economic history? Can they disagree without one invalidating the other?',
    'Institutional tolerance test: whether courts accept briefs that cite pre-1710 evidence of thinkability or occupancy emergence as legitimate historical context, or whether they treat the statute as foreclosing historical inquiry.',
    'High tolerance would indicate the readings coexist (thinkability_reading and first_holding_reading remain live alongside synchronic_reading). Low tolerance would indicate the synchronic reading forecloses or suppresses the others, making the constraint extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_formalism_vs_historical_reconstruction, empirical, 'Whether statutory formalism permits or excludes historical disagreement about the temporal boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 1600, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1600, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1600, 0.42).
narrative_ontology:measurement(ip_c_tr_t1650, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1650, 0.5).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1710, 0.58).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1730, 0.6).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1750, 0.55).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1600, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement(ip_c_be_t1650, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1650, 0.48).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1710, 0.62).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1730, 0.65).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1750, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1600, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1600, 0.28).
narrative_ontology:measurement(ip_c_su_t1650, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1650, 0.32).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1710, 0.41).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1730, 0.43).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1750, 0.39).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% The ip_category_emergence kernel decomposes into three constraint stories, each a reading of the contested question of when IP became a coherent legal category. The thinkability_reading dates emergence to when the concept of owning expression became intellectually coherent (gradual, 17th century). The first_holding_reading dates emergence to when authors became recognized rights-holders (consolidating 18th century, post-1710). This reading (synchronic_diachronic_seam) tests whether these two dates must co-occur or can vary independently. Each reading has its own ε value: thinkability_reading is lower-extraction (the concept emerged naturally from printing and circulation); first_holding_reading is higher-extraction (occupancy transfer involved suppressing prior claimants); this reading is moderate-extraction (the synchronic frame suppresses temporal complexity to preserve institutional coherence). The three readings are linked by network.affects_constraints because the viability of the synchronic frame depends on whether thinkability and first-holding can be shown to be independent—if they are, the synchronic reading collapses and the kernel structure becomes diachronic disaggregation rather than unified emergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
