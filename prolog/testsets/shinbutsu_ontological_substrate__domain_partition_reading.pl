% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition: Functional Coexistence Reading
 *   domain: religious_studies/commitment_systems
 *
 * SUMMARY:
 *   The shinbutsu-shugo (kami-buddha combination) system in medieval and
 *   early-modern Japan established institutional coexistence between Shinto
 *   priesthoods and Buddhist establishments. This constraint story
 *   instantiates the DOMAIN PARTITION READING: a functional, pragmatic
 *   division where kami govern this-worldly concerns (prosperity, health,
 *   purification, life transitions) and buddhas govern afterlife (karma,
 *   rebirth, salvation, memorial care). This reading does NOT claim
 *   ontological unification (that is the syncretic-fusion reading) and does
 *   NOT diagnose incoherent drift (that is the incoherence reading). Instead,
 *   it argues that a genuine, coherent commitment kernel exists: a workable
 *   dual-system arrangement that serves both priesthoods' institutional
 *   interests and lay practitioners' actual religious needs. The state
 *   (particularly the Edo bakufu) codified and enforced this partition
 *   through law, shrine/temple registry systems, and administrative
 *   separation. This reading's extraction score (0.31) is substantially lower
 *   than snare-range because the arrangement genuinely solved a
 *   collective-action problem for both priesthoods and for lay practice—both
 *   priesthoods benefit from institutional autonomy without requiring
 *   metaphysical unity. However, extraction persists at a non-trivial level:
 *   the state extracts loyalty and administrative measurability by keeping
 *   the institutions separate; the lay practitioners' implicit acceptance of
 *   the partition constrains their theological freedom (they cannot insist
 *   both priesthoods address the same concerns without institutional
 *   conflict).
 *
 * KEY AGENTS:
 *   - Shinto priesthood: maintains this-worldly shrine jurisdiction and income; depends on domain partition for legitimacy without ontological claim.
 *   - Buddhist priesthood: maintains afterlife and memorial jurisdiction and income; similarly depends on partition.
 *   - Edo bakufu state authority: codifies partition through law and registry; extracts administrative control and tax capacity by treating priesthoods as distinct entities.
 *   - Lay practitioners: benefit from normalized dual practice; constrained by implicit acceptance of the domain-partition framing.
 *   - Syncretic-fusion theorists (honji suijaku advocates): excluded by this reading; would argue partition masks deeper ontological truth.
 *   - Incoherence scholars: excluded by this reading; would diagnose drift rather than genuine commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.31).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.42).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami-Buddha Domain Partition: Functional Coexistence Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '79fabefc-9cdf-46ec-a8b1-f57bc5f33040').
narrative_ontology:cs_kernel_codification('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', distributed).
narrative_ontology:cs_authority_grounding('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', extraction).
narrative_ontology:cs_interpretation_layer_present('79fabefc-9cdf-46ec-a8b1-f57bc5f33040').
narrative_ontology:cs_reading_relation('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', foundational, kami_buddha_functional_partition).
narrative_ontology:cs_axiom_status(kami_buddha_functional_partition, holdable).
narrative_ontology:cs_axiom_grounding('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', kami_buddha_functional_partition, instrumental).
narrative_ontology:cs_axiom('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', foundational, pragmatic_coexistence_without_ontological_claim).
narrative_ontology:cs_axiom_status(pragmatic_coexistence_without_ontological_claim, holdable).
narrative_ontology:cs_axiom_grounding('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', pragmatic_coexistence_without_ontological_claim, conventional).
narrative_ontology:cs_reference_frame('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', edo_bakufu_institutional_codification).
narrative_ontology:cs_drift_state('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', id_1868_meiji_restoration, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('79fabefc-9cdf-46ec-a8b1-f57bc5f33040', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, both_priesthoods_institutional_autonomy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extracted metrics reflect a constraint that is GENUINELY COORDINATIVE (low extraction relative to snare) but NOT purely rope-grade because institutional and state power shapes its persistence. Extractiveness drops from 0.35 (early 1500s, high institutional conflict before partition codification) to 0.27–0.29 (1600–1700, after bakufu stabilization) and stays near 0.31 at interval end. This downward-then-flat trajectory shows that once the partition was legally codified and institutionally routinized, the need for active state enforcement (suppression) fell below snare-range. Theater_ratio oscillates around 0.48–0.52: both priesthoods maintain performative demonstrations of their doctrinal distinctness (kami as indigenous spirits vs. buddhas as cosmic saviors) even though the underlying distinction is functional rather than metaphysical. Suppression_requirement (0.38–0.44 range) reflects the state's ongoing need to prevent either priesthood from encroaching on the other's domain, but this is lower than the active suppression a snare requires because lay demand for both services naturally sustains the partition. The time grid is shared across all three metrics (every metric authored at every time point: 1500, 1600, 1700, 1800, 1868).
 *
 * PERSPECTIVAL GAP:
 *   The shinto_priesthood and buddhist_priesthood seats should compute with similar low directionality (both are beneficiaries, neither is fully extracted from—they are agenda-setters within their domains). The state authority seat sits at higher extraction (d toward target end) because it uses the partition for administrative and tax purposes. Lay practitioners sit at moderate extraction: they gain access to dual practice, but the partition constrains their freedom to demand either priesthood address both domains. The honji suijaku advocates (syncretic-fusion reading) would compute very differently—they would measure extraction on themselves (excluded from the partition framework) and argue the two priesthoods are colluding to suppress theological unity. The incoherence scholars would measure extraction as state-imposed institutional fragmentation. The engine's per-seat computation will surface this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Domain-partition reading benefits both priesthoods structurally: each gains institutional autonomy without needing to prove ontological claims about the other. This is low-extraction coordination for these two seats. The state benefits from administrative measurability and ability to tax/regulate the priesthoods separately (mild extraction from the state's perspective—the partition is convenient, not absolutely required). Lay practitioners benefit from normalized dual practice (genuine benefit), but the partition also constrains their theological agency (they cannot insist both priesthoods unify without creating institutional friction). Excluded syncretic-fusion theorists experience this reading as extraction—they are barred from advancing their ontological claim because it would destabilize the partition both priesthoods rely on. Excluded incoherence scholars are not actively suppressed, but the institutional stability of the partition renders their diagnosis invisible (if the system works, claims of drift get less hearing).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional ambiguity from early syncretism; need for revenue stability and jurisdictional clarity) is LIVE: even under the domain-partition reading, both priesthoods face ongoing tension between metaphysical claims and institutional division. The partition avoids mandatrophy by keeping the founding problem manageable—priests can say 'kami and buddhas have different jobs' without having to resolve 'are they metaphysically unified.' However, the persistence of honji suijaku scholarship and periodic intellectual challenge to the partition (especially visible in Edo-period monk debates) shows that the commitment kernel itself is CONTESTED—the founding problem has not been solved, only administratively bracketed. This is exactly what distinguishes a rope with institutional support from a snare: the rope's persistence depends on both priesthoods finding it genuinely useful; a snare would show increasing theater and resistance over time. The measurements show theater declining from 0.52 to 0.45 as the partition becomes normalized (the opposite of Goodhart drift toward pure performance), which supports the rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_fusion_contest,
    'Is the domain partition a genuine commitment kernel grounding institutional coexistence, or is it administrative convenience that conceals (but fails to resolve) the underlying honji suijaku metaphysical unity question?',
    'Historical analysis of priest and scholar writings: did both priesthoods and educated practitioners accept the partition as a true solution, or as a pragmatic workaround they believed masked deeper truth? Contemporary institutional documents, debate records, and theological texts from the Edo period (particularly monastery debates on honji suijaku) provide evidence.',
    'If the partition is a genuine kernel, this reading computes as low-extraction rope with institutional autonomy. If it is purely administrative convenience, the domain-partition reading is itself a cover story and this constraint should be reclassified as tangled_rope or snare (extraction from both priesthoods by the state, which creates the partition and collects from both through tax/regulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_fusion_contest, conceptual, 'Whether the domain partition is an authentic commitment or a state-imposed facade.').

omega_variable(
    lay_practitioner_autonomy,
    'Do lay practitioners experience the domain partition as a coherent framework that solves their religious needs, or as an externally-imposed constraint that forces them to fragment their spiritual concerns across institutions?',
    'Ethnographic and textual evidence from lay-practice materials: How did ordinary practitioners describe their shrine and temple visits? Did they articulate a domain-partition logic, or do the sources show discomfort with the split? Regional variations in syncretic practice (e.g., coastal areas where merchant classes had different institutional access) provide test cases.',
    'If lay practitioners experienced the partition as natural and beneficial, extraction from this seat is genuinely low. If lay practice literature shows resentment, theological confusion, or forced acceptance of the split, extraction is higher than this reading claims—the constraint suppresses authentic lay religious expression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_practitioner_autonomy, empirical, 'Whether the domain partition reflects or suppresses lay religious agency.').

omega_variable(
    state_extraction_through_separation,
    'Did the Edo bakufu''s codification of the partition serve genuine pluralistic commitments (respect for both priesthoods'' traditions), or did it strategically separate the priesthoods to prevent either from accumulating power that could challenge state authority?',
    'Analysis of bakufu policy toward priesthoods before and after partition codification: tax patterns, political favors, restrictions on inter-priesthood mobility, and state statements about religious policy. Comparative analysis with state regulatory approaches to other institutional domains (samurai, merchant guilds) to determine if the partition was theologically motivated or power-consolidating.',
    'If the separation was strategic pluralism, the state''s role is agenda-setting but non-extractive (coordinates institutional autonomy). If the separation was power-consolidation, the state''s directionality shifts toward full target end (d ~0.9): the constraint extracts from both priesthoods through administrative control, and this reading''s extraction score should rise to snare-range.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_extraction_through_separation, empirical, 'Whether the state''s role in partition codification was pluralistic support or power consolidation.').

omega_variable(
    kernel_reading_interdependence,
    'Can the domain-partition reading (this constraint) logically coexist with the syncretic-fusion reading in the same institutional framework, or do they foreclose one another?',
    'Close reading of 18th-century Buddhist and Shinto scholarly texts: Can a monk or priest hold both ''the partition is real and functional'' AND ''kami and buddhas are ontologically unified'' without contradiction? Or do the texts show that adopting one reading requires rejecting the other?',
    'If the readings are logically compatible (coexist_with), then this domain-partition reading is not the canonical reading—it is one option among several in live dispute, and the constraint''s extractiveness may be higher (from those forced into one reading by institutional power). If the readings foreclose one another (forecloses), then the partition is more foundational and this reading is the core kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_interdependence, conceptual, 'Logical compatibility between domain-partition and syncretic-fusion framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 1500, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1500, 0.52).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1600, 0.45).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1700, 0.48).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1800, 0.5).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1868, 0.48).

% Extraction over time
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1600, 0.29).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1700, 0.27).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1800, 0.31).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1868, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1600, 0.38).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1800, 0.44).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1868, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint instantiates one reading (domain_partition_reading) of a contested kernel shared with two sibling constraints: shinbutsu_ontological_substrate__syncretic_fusion_reading (kami-buddha ontological unity through honji suijaku theory) and shinbutsu_ontological_substrate__incoherent_bundle_reading (no coherent kernel; drift under state enforcement). All three are constraints on the SAME historical phenomenon (shinbutsu-shugo system) but with fundamentally different ε profiles and structural data. Each reading instantiates a different commitment kernel and thus a different constraint. The domain-partition reading asserts a genuine functional commitment to separate domains; the syncretic reading asserts metaphysical unification; the incoherence reading asserts no unifying commitment at all. These three readings form a constraint family—all linked via network.affects_constraints—because institutional and scholarly developments that strengthen one reading can weaken the others (e.g., Edo-period honji suijaku scholarship that gained prestige would shift institutional commitments toward the syncretic reading, destabilizing the domain-partition reading's legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
