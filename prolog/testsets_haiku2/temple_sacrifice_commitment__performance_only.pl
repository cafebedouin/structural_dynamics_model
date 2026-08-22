% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment (Performance-Only Reading)
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This constraint instantiates the performance-only reading of the temple
 *   sacrifice commitment kernel: the proposition that material instantiation
 *   is necessary for the law to count as 'occupied'—that study without
 *   performance is archival preservation of a defunct practice, not
 *   participation in the commitment itself. The kernel is the halakhic
 *   community's response to the impossibility of Temple sacrifice after 70
 *   CE; this reading holds that the commitment remains dormant, maintained
 *   textually but not functionally. The constraint is a piton: it persists
 *   not because any party benefits from its maintenance (extraction is low,
 *   near 0.15) nor because dramatic coercion holds it in place (suppression
 *   is minimal, near 0.08), but because the institutional architecture of
 *   halakhic study—the yeshiva system, the textual canon, the interpretive
 *   tradition—carries the constraint forward by inertia. The theater ratio is
 *   high (0.72): the constraint's observable activity (daily study of
 *   sacrifice laws, detailed commentaries on Temple procedures, hypothetical
 *   adjudications) is mostly performative maintenance—it enacts the
 *   commitment's presence in textual form while bracketing the question of
 *   its functional status. No party is currently harmed by the constraint (no
 *   victim set is named), but the reading explicitly identifies a potential
 *   future victim class—those who would be subject to restoration attempts
 *   without ethical evolution.
 *
 * KEY AGENTS:
 *   - Orthodox halakhic community: custodian of the textual and interpretive infrastructure; maintains the constraint through study while claiming non-occupation of the commitment itself
 *   - Jewish scholars and jurists: derive professional standing from scholarship; benefit from the reading's permission to study archivally without claiming to perform the law
 *   - Potential future restorers (excluded): prospective constituency unnamed in current discourse; named by the reading as future-contingent victims if restoration were attempted
 *   - Other religious traditions (observer): witness the management of a suspended commitment; shape interfaith dialogue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.15).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.08).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment (Performance-Only Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '99e9f932-3b9e-4167-825e-08a6618b852a').
narrative_ontology:cs_kernel_codification('99e9f932-3b9e-4167-825e-08a6618b852a', fixed_text).
narrative_ontology:cs_authority_grounding('99e9f932-3b9e-4167-825e-08a6618b852a', lineage).
narrative_ontology:cs_interpretation_layer_present('99e9f932-3b9e-4167-825e-08a6618b852a').
narrative_ontology:cs_reading_relation('99e9f932-3b9e-4167-825e-08a6618b852a', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('99e9f932-3b9e-4167-825e-08a6618b852a', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_reading_relation('99e9f932-3b9e-4167-825e-08a6618b852a', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('99e9f932-3b9e-4167-825e-08a6618b852a', foundational, material_instantiation_necessary_for_occupation).
narrative_ontology:cs_axiom_status(material_instantiation_necessary_for_occupation, holdable).
narrative_ontology:cs_axiom_grounding('99e9f932-3b9e-4167-825e-08a6618b852a', material_instantiation_necessary_for_occupation, deontological).
narrative_ontology:cs_axiom('99e9f932-3b9e-4167-825e-08a6618b852a', secondary, commitment_dormancy_persists).
narrative_ontology:cs_axiom_status(commitment_dormancy_persists, holdable).
narrative_ontology:cs_axiom_grounding('99e9f932-3b9e-4167-825e-08a6618b852a', commitment_dormancy_persists, deontological).
narrative_ontology:cs_reference_frame('99e9f932-3b9e-4167-825e-08a6618b852a', post_temple_destruction_dormancy).
narrative_ontology:cs_drift_state('99e9f932-3b9e-4167-825e-08a6618b852a', contemporary_2026, gap(stable, minor, true)).
narrative_ontology:cs_created_at('99e9f932-3b9e-4167-825e-08a6618b852a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, jewish_scholars_and_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the textual and interpretive infrastructure for sacrifice law through study and commentary. Under the performance-only reading, they are the custodians of a dormant commitment—neither occupying it through study nor enabling its material instantiation. Their role is administrative preservation without functional participation. The tension is structural: they keep the commitment alive in form while acknowledging its substantive inactivity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, orthodox_halakhic_community, agenda_setter,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, orthodox_halakhic_community, observer).

% Derive intellectual and professional standing from deep engagement with sacrifice law texts. The performance-only reading permits their scholarly work to proceed without claiming to occupy the commitment itself—study is legitimate archival preservation and intellectual exercise, not a substitute performance. They benefit from the reading's clarity about study's limits, which allows rigorous scholarship without metaphysical conflation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, jewish_scholars_and_jurists, beneficiary,
    powerful, biographical, mobile, global).

% A prospective constituency with no current voice: those who would attempt material restoration of sacrifice if the commitment were reanimated. The performance-only reading does not acknowledge them as present participants but names them as a future-contingent victim class—restoration without ethical evolution would impose constraints on them. Their absence from current discourse is structural to the reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, potential_future_restorers, excluded,
    powerless, civilizational, trapped, global).

% Witness the halakhic community's management of a suspended commitment. The performance-only reading's clarity about dormancy vs. archival preservation is analytically legible to outsiders and shapes interfaith dialogue about superseded practices.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, other_religious_traditions, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__performance_only, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Study of sacrifice law coordinates the transmission and preservation of a complex textual tradition across generations. It maintains institutional memory of intricate legal reasoning, taxonomies, and interpretive methods that would be lost if the texts were simply archived without active scholarly engagement. The coordination is textual and intellectual, not material-performance centered.
% TRANSFER_FUNCTION: The constraint moves professional prestige and intellectual authority toward scholars and jurists who master the sacrifice-law corpus. It also moves time and cognitive resource from the broader Jewish community toward maintenance of a dormant textual system. Under the performance-only reading, there is no transfer TO the commitment itself—no material goods offered, no participation achieved. The transfer is among human actors (scholars gaining authority) and from society (toward preservation of a defunct practice's corpus).
% ABSENT_VOICES: The voices most absent are those who would perform sacrifice if restoration were attempted: the prospective priests, the animals that would be brought, the communities that would bear the ethical and practical weight of reinstatement. The performance-only reading explicitly renders them as structurally excluded from present discourse, future-contingent only. Also absent: critiques from within the tradition that the reading's dormancy claim is itself an avoidance, or that study without commitment is intellectual escapism.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished (i.e., if the halakhic community abandoned the constraint that material instantiation is required), the world would rearrange: the sibling readings would move from marginal to central. Study would be reframed as commitment-occupation, or as suspended preparatory exercise, or as authorized transformation. The institutional and scholarly infrastructure would remain, but its legitimacy narrative would shift. The disappearance is contested because some readings hold that the reading is already functionally inert—that its disappearance would be unnoticed, others that it is the hinge on which the entire halakhic community's self-understanding of dormancy rests.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), material sacrifice became impossible. The halakhic commitment to sacrifice law persisted but could not be materially instantiated. The founding problem was: how to maintain the legal and spiritual integrity of a binding divine command when its material conditions are irreversibly absent? The performance-only reading answers: study is archival preservation of the command's legal form, not substitution for it. The command is dormant, not abrogated.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream halakhic authorities across centuries (Maimonides, Talmudic commentators, medieval responsa literature) attest that the founding problem persists: sacrifice law remains halakhically binding in principle despite material impossibility. The performance-only reading's assertion that study is archival preservation rather than commitment-occupation is attested by scholars working within the halakhic tradition who explicitly distinguish between textual mastery and the claim to be 'performing' the law. However, no corroboration comes from outside the halakhic tradition proper—comparative religious-studies observers note the reading but do not adjudicate its internal coherence. The founding problem itself is live across all readings; the status of study as archival-vs.-occupational is internally contested.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The piton classification rests on the measurement profile: theater_ratio rising from 0.65 to 0.72 over the interval (from Second Temple destruction to present) indicates that the observable activity of the constraint is increasingly performative maintenance—study elaborate and reverent, but bracketing the functional question. Extractiveness declining from 0.22 to 0.15 indicates that no party is accumulating concentrated benefit from the constraint's persistence; scholars gain prestige but not extraction in the technical sense (they are not targets, nor do they forcibly hold others to the constraint). Suppression near 0.08 throughout indicates no active coercive force maintaining the constraint—it persists because the institutional machinery of halakhic study cranks forward, not because compliance is enforced or alternatives are suppressed. The measurement series use one time grid (shared across all metrics at all five time points) to avoid the misalignment error. The trajectory shows stabilization rather than drift: by 1000 CE, the theater ratio and extractiveness both reach their final values and hold steady—the constraint has achieved a stable inertial state. The claim (piton) and metrics (low extraction, high theater, zero suppression) are authored independently and intentionally diverge from how a beneficiary party (the halakhic scholars) might frame it.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic seat, the constraint is live commitment. From the observer seat (computing from metrics), it is inertial maintenance. Scholars experience study as honoring divine law; observers see repetitive institutional activity with high theater. The gap is the signature of a piton.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no directionality derivation in the technical sense because there are no contemporary beneficiaries or victims. The halakhic scholars listed as beneficiaries gain professional standing, not extraction—they are not targets of the constraint's operation, and they are not forcibly held to it. The constraint's sole contemporary function is textual maintenance. The performance-only reading's innovation is naming a future-contingent victim class: those who would be harmed if restoration were attempted without ethical evolution. This naming is structurally important—it signals that the reading is not ethically neutral about dormancy, but rather that dormancy is preferable to restoration-without-evolution. The constraint's directionality is zero across all contemporary seats (d near 0.5 for scholars—symmetric benefit and burden, neither extractive nor subsidizing) because no real transfer is occurring. The only d that deviates is the prospective future restorer (d near 1.0, fully target, if restoration occurred).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is precise: did the founding problem (how to maintain sacrifice law after Temple destruction) outlive its function? The performance-only reading answers: the founding problem is still live—the commitment remains binding, just materially impossible. However, the constraint itself (study without performance) may have undergone mandate atrophy: it was originally erected to preserve legal knowledge during temporary impossibility; after 2000 years, 'temporary' has become permanent, and the constraint now persists as a matter of institutional inertia rather than active problem-solving. The six_questions.founding_problem_status is 'live' (the commitment remains; instantiation is still desired), but the relationship between founding problem and current constraint is contested. The performance-only reading explicitly brackets the question: it does not claim mandate atrophy; it simply asserts that study is archival, not occupation. The sibling readings (study_as_exercise, hybrid_preparatory, symbolic_transformation) resolve mandate differently—they claim that study IS occupation or preparation or authorized new-performance, thus resolving the temporal pressure. This reading leaves the temporal tension unresolved, which is structurally appropriate for a piton: the constraint persists without active justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_vs_suspended_commitment,
    'Is the sacrifice commitment genuinely dormant (inert, awaiting restoration) or is it in suspended animation (held ready, maintained at operational capacity)? The performance-only reading asserts dormancy; the hybrid_preparatory reading asserts suspension.',
    'Examine the detailed content of halakhic study: if scholars focus on textual minutiae, variant traditions, and theoretical subtleties without maintaining operative competence in performing sacrifice (e.g., ritual sequencing, implements, contemporary applicability), dormancy is supported. If study maintains rehearsal-grade competence and scenario-planning for restoration, suspension is supported.',
    'Dormancy supports the piton classification (inertial maintenance); suspension supports the rope classification (genuine preparation). The constraint''s entire legitimacy narrative shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_vs_suspended_commitment, conceptual, 'Whether the commitment is dormant or suspended.').

omega_variable(
    archival_vs_theological_performance,
    'Is study of sacrifice law ''merely'' archival preservation (a historical and intellectual record of a defunct practice), or does it carry theological weight as continued prayer or engagement with divine will, even if not material instantiation?',
    'Comparative analysis of halakhic discourse: does it frame study in archival language (historical documentation, legal preservation) or theological language (communion with divine intent, occupancy through intellectual engagement)? Mixed language within the same authority indicates conceptual instability.',
    'Pure archival framing supports performance-only (low-epsilon rope, not occupation); theological framing undermines it and favors study_as_exercise or symbolic_transformation readings. The constraint''s coherence depends on this distinction holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(archival_vs_theological_performance, empirical, 'Whether study is framed as archival or theological performance.').

omega_variable(
    future_restoration_ethical_burden,
    'If the commitment were materially restored (e.g., through Temple reconstruction and reestablishment of sacrifice), what ethical obligations would the halakhic community incur? The performance-only reading asserts that restoration without ethical evolution would create a future victim class; is this burden real or rhetorical?',
    'Examine contemporary halakhic responsa addressing hypothetical restoration: what ethical guardrails or transformations are named? How seriously is the tension between ''law is binding'' and ''context has changed'' treated?',
    'If restoration is named as ethically fraught, the performance-only reading''s naming of future victims is structurally honest and the constraint bears future victim-load. If restoration is treated as legally straightforward (merely awaiting conditions), the performance-only reading''s victim-naming is performative, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_restoration_ethical_burden, empirical, 'Whether future restoration would carry ethical obligations that burden the commitment.').

omega_variable(
    study_legitimacy_without_occupation,
    'Can the halakhic community coherently maintain that study of sacrifice law is legitimate and valuable WITHOUT claiming that study occupies or performs the commitment? Or is the performance-only reading''s separation itself unstable—does it eventually collapse into either study_as_exercise (study IS performance) or symbolic_transformation (study IS the new form)?',
    'Longitudinal analysis of halakhic scholarship: does the separation hold across centuries, or does language drift toward occupation-framing or transformation-framing? Examine high-prestige scholars'' self-descriptions.',
    'If the separation collapses, the performance-only reading is transitional (a temporary way station) and the constraint''s true type is contested within the reading itself. If the separation holds, the reading is stable and the piton classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_legitimacy_without_occupation, conceptual, 'Whether the separation between legitimate study and commitment-occupation is structurally stable.').

omega_variable(
    alternative_readings_kernel_identity,
    'Do all four readings (performance_only, study_as_exercise, hybrid_preparatory, symbolic_transformation) address the same kernel, or do they instantiate fundamentally different kernels that are merely linguistically bundled under ''sacrifice commitment''?',
    'Test whether a single halakhic authority or scholar could hold two of the readings simultaneously without contradiction. If yes, they are readings of one kernel. If no, they are incommensurate kernels.',
    'If the readings share a kernel, the performance-only reading coexists with others in a contested system. If they are incommensurate kernels, the performance-only reading is not one reading of a shared commitment but a distinct constraint altogether, and the family links (affects_constraints) are misleading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_kernel_identity, conceptual, 'Whether the four readings instantiate one kernel or multiple kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.65).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__performance_only, theater_ratio, 500, 0.68).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__performance_only, theater_ratio, 1000, 0.71).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__performance_only, theater_ratio, 1500, 0.72).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__performance_only, theater_ratio, 2000, 0.72).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__performance_only, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__performance_only, base_extractiveness, 1000, 0.16).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__performance_only, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__performance_only, base_extractiveness, 2000, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% The performance-only reading is one of four structurally distinct instantiations of the temple_sacrifice_commitment kernel. All four address the halakhic community's response to the material impossibility of Temple sacrifice after 70 CE. The performance-only reading differs from its siblings in denying that study or prayer constitute occupation or transformation of the commitment—the commitment is dormant, maintained archivally but not functionally. The eps values across the sibling set are uniformly low (this constraint: 0.15; study_as_exercise: 0.12; hybrid_preparatory: 0.13; symbolic_transformation: 0.14) because none extract substantial benefit or impose suppression in the contemporary period. The readings differ not in extractiveness but in framing the commitment's status: alive-and-suspended, occupied-through-study, preparing-for-restoration, or transformed-into-prayer. Each reading instantiates a different constraint because ε is invariant only within a single reading—the standing arrangement under contest is 'whether study occupies the commitment,' and that question has a different answer in each reading. The performance-only reading is upstream (foundational negation; the other readings must argue against this reading's core premise) and influences the others (they must articulate why their reading is not merely claiming what performance-only denies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
