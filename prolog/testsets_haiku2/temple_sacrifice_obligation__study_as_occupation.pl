% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Legitimate Obligation Fulfillment
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, the rabbinic
 *   tradition faced a crisis: the obligation to study sacrifice law remained
 *   textually mandated, but the obligation to perform animal sacrifice became
 *   materially impossible. The constraint studied here is ONE READING of how
 *   this crisis was resolved: through the principle that study of sacrifice
 *   law constitutes a legitimate occupation of the obligation in the Temple's
 *   absence. This reading interprets the obligation as fulfilled through
 *   intellectual engagement rather than through suspension or archiving. The
 *   constraint operates at the intersection of textual exegesis,
 *   authority-structure legitimation, and community practice: the rabbinic
 *   sages established that study satisfies the obligation, and this ruling
 *   has been sustained across nearly two millennia by an interpretive
 *   consensus. The claim/metric gap is deliberate and reflects the reading's
 *   structural character: claimed as ROPE (genuine coordination solving the
 *   post-Temple obligation crisis) while the authored metrics are very low on
 *   extractiveness and suppression because the reading produces no victim set
 *   and requires minimal enforcement—the consensus that study fulfills the
 *   obligation is maintained not through coercion but through shared
 *   religious conviction that the interpretation is legitimate.
 *
 * KEY AGENTS:
 *   - Rabbinic interpretive authority: sets the framework that study of sacrifice law occupies the obligation (institutional, civilizational horizon)
 *   - Observant Jewish community: fulfills the obligation through study rather than impossible animal sacrifice (organized, generational horizon)
 *   - Textual tradition keepers: maintain the transmitted knowledge of sacrifice law in detail, making study possible (institutional)
 *   - Messianic redemption expectant: excluded voice arguing the obligation remains suspended, not fulfilled, until Temple restoration (moderate, civilizational)
 *   - Post-Temple halakhic consensus: the shared acceptance that study-fulfillment is legitimate, the enforcement mechanism of the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.12).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Legitimate Obligation Fulfillment").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'bb244f19-1b59-4493-b552-47d2d81887ff').
narrative_ontology:cs_kernel_codification('bb244f19-1b59-4493-b552-47d2d81887ff', fixed_text).
narrative_ontology:cs_authority_grounding('bb244f19-1b59-4493-b552-47d2d81887ff', lineage).
narrative_ontology:cs_interpretation_layer_present('bb244f19-1b59-4493-b552-47d2d81887ff').
narrative_ontology:cs_reading_relation('bb244f19-1b59-4493-b552-47d2d81887ff', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_reading_relation('bb244f19-1b59-4493-b552-47d2d81887ff', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('bb244f19-1b59-4493-b552-47d2d81887ff', foundational, study_occupies_obligation).
narrative_ontology:cs_axiom_status(study_occupies_obligation, holdable).
narrative_ontology:cs_axiom_grounding('bb244f19-1b59-4493-b552-47d2d81887ff', study_occupies_obligation, deontological).
narrative_ontology:cs_axiom('bb244f19-1b59-4493-b552-47d2d81887ff', foundational, rabbinic_authority_to_redefine_obligation).
narrative_ontology:cs_axiom_status(rabbinic_authority_to_redefine_obligation, holdable).
narrative_ontology:cs_axiom_grounding('bb244f19-1b59-4493-b552-47d2d81887ff', rabbinic_authority_to_redefine_obligation, conventional).
narrative_ontology:cs_reference_frame('bb244f19-1b59-4493-b552-47d2d81887ff', post_temple_obligation_continuity).
narrative_ontology:cs_drift_state('bb244f19-1b59-4493-b552-47d2d81887ff', contemporary_jewish_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bb244f19-1b59-4493-b552-47d2d81887ff', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jewish_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, textual_tradition_keepers).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, post_temple_halakhic_consensus).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, substitutability_of_study_for_performance).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, intellectual_engagement_as_ritual_equivalent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rabbinic sages and their successors who established and maintain the principle that study of sacrifice law substitutes for performance. They set the interpretive framework that redefines obligation-fulfillment from physical sacrifice to intellectual engagement. Their authority grounds itself in the claim that halakhic study preserves the obligation's spiritual substance even when performance is materially impossible.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Fulfills the obligation through study rather than animal sacrifice. They gain a practice that is materially feasible (study is always possible), spiritually coherent (the obligation is met, not suspended), and institutionally sustainable (requires no temple, no priesthood, no access to Jerusalem). The constraint enables continuous religious practice in diaspora and post-Temple conditions.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jewish_community, beneficiary,
    organized, generational, constrained, global).

% Would argue that the obligation remains suspended, not fulfilled, and study merely preserves knowledge for when the Temple is rebuilt and actual sacrifice resumes. They contest the reading's core premise—that study constitutes occupation of the obligation—and advocate instead for maintenance of the obligation's unsatisfied status as a permanent anticipation of restoration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_redemption_expectant, excluded,
    moderate, civilizational, constrained, global).

% Maintain the written and oral transmission of sacrifice law in exhaustive detail. The constraint assigns them a central role: their interpretive work and transmission is not auxiliary to fulfillment but IS the fulfillment. They become the primary custodians of the obligation's ongoing existence.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, textual_tradition_keepers, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, textual_tradition_keepers, agenda_setter).

% Hold that the obligation requires actual performance and cannot be satisfied through study alone. They view the constraint as a workaround that converts an unfulfillable obligation into a performed substitute, but at the cost of changing the obligation's essential nature. Their position is marginal in diaspora Judaism but persistent in certain currents of contemporary Temple-restoration theology.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, temple_restoration_literalists, observer,
    moderate, civilizational, constrained, regional).

% The broad rabbinic and community agreement that study satisfies the obligation. This consensus is the constraint's enforcement mechanism—not coercion but shared acceptance that the reading is legitimate. It enables the constraint to operate with minimal suppression.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, post_temple_halakhic_consensus, beneficiary,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the crisis of obligation-fulfillment after Temple destruction: when the primary referent (animal sacrifice in the Temple) becomes materially impossible, the constraint redefines the obligation around its substitute (study of the law). This keeps the obligation alive as a practice and as a motivator of learning, rather than letting it lapse into suspension or abandonment.
% TRANSFER_FUNCTION: Moves the locus of obligation-fulfillment from priesthood and Temple to rabbinic sages and the learned community. Authority over interpretation of the obligation becomes centralized in the interpretive apparatus that decides what counts as legitimate study. The constraint transfers spiritual validation from material performance to intellectual engagement.
% ABSENT_VOICES: Temple-restoration literalists and the messianic-suspension reading would argue that study does NOT constitute fulfillment and that the obligation remains unpaid, merely deferred. Sectarian groups (Karaites, Sadducees if they persisted) would reject rabbinic authority to redefine the obligation. These voices are structurally excluded from the consensus that defines study-fulfillment as legitimate.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if the rabbinic ruling that study satisfies the obligation were revoked and replaced with (say) suspension or archiving—the entire post-Temple Jewish obligation-structure would reorganize. Communities would shift to either a suspension reading (obligation frozen, awaiting Temple) or an archiving reading (study preserves knowledge but does not fulfill). The religious practice and the status of study-as-obligation would fundamentally change.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE made the performance of animal sacrifice impossible. The obligation to study sacrifice law remained in the textual tradition, but performance became materially unrealizable. The foundational problem: how to maintain the obligation as a living practice when its primary form (sacrifice in the Temple) is no longer available.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic sources (Mishnah, Talmud, medieval codes) attest that this problem drove the development of the study-fulfillment principle. Post-Temple halakhic authorities (Rambam, Tur, Shulchan Aruch) confirm that study is the accepted substitute. Contemporary Jewish communities across denominations practice this constraint: studying sacrifice law is understood as fulfilling the obligation. The consensus exists across observant communities independent of which particular schools author the original ruling.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the constraint produces no extraction pyramid: the rabbinic authority benefits from having set the interpretive frame, but that is not the same as extracting from an identified victim set. There are no victims—the observant community gains feasibility of practice, the sages gain authority, the textual keepers gain centrality. The constraint solves a coordination problem (post-Temple obligation sustenance) without generating asymmetric extraction. Suppression is low (0.15) because the constraint's persistence depends almost entirely on shared acceptance, not on active coercion or alternative suppression—the alternative readings (messianic-suspension, study-as-archiving) are excluded from consensus but not forcibly prevented. The theater ratio is minimal (0.08) because nearly all of the constraint's operation is functional: study actually does occur, obligations are actually fulfilled in the community's understanding, and the interpretive work is genuine. The measurement series shows very slight rise over 2000 years as the constraint faces periodic challenges from Temple-restoration literalists and requires reinforcement through restatement in each generation's halakhic works, but the rise is modest because the core consensus remains stable. The accessibility collapse is low (0.22) because alternatives persist—the messianic-suspension reading and study-as-archiving reading remain available as coherent intellectual positions, even though they are not the dominant reading in diaspora Judaism.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic-authority seat and the observant-community seat should perceive this constraint differently. From the rabbinical perspective, the constraint is a legitimate intellectual solution to an impossibility: study preserves the obligation and fulfills it in a way that is coherent with the law's continuing authority. From the observant community's perspective (at the aggregate level), the constraint is a blessing—it makes practice possible. From the messianic-literalist perspective (excluded), the constraint is a reinterpretation that evacuates the obligation of its essential meaning: study is not sacrifice, and no reinterpretation can make it so. The engine computes these divergences from the structural data—the declared readings of the kernel, the beneficiary/victim declarations, and the exit options. The authored claim (rope) reflects the rabbinic and observant-community perspective; the exclusion of the messianic reading reflects the constraint's consensus structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic interpretive authority is the structural beneficiary: they set the framework and gain authority-validation for their interpretive power. The observant Jewish community is structurally at the beneficial end of directionality (d near 0.2–0.3) because they genuinely benefit from the constraint—it makes the obligation practice-able. The excluded messianic-literalists sit near the target end (high d) in the sense that they must accept a constraint they contest; however, they are not victims of active extraction because they are not compelled to perform study—they have chosen alternative observance or marginalization. The constraint's low extractiveness means there is no steep directionality gradient: most parties experience it as coordination rather than targeting. Rabbinic authority derives d from beneficiary status (they author the frame) and from their institutional power and analytical exit options; the observant community derives low d from genuine beneficiary position (obligation made feasible) and from their organized power but constrained exit (commitment to Jewish practice). Temple-restoration literalists derive higher d (closer to 0.6–0.7) because they experience the constraint as foreclosing their preferred reading, but because the constraint operates through consensus rather than coercion, their exit is exit from the observance itself, not from an extractive trap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction, obligation persists, performance impossible) was live at t=0 (70 CE). It remains live at t=2000 (contemporary): the Temple has not been rebuilt, so the problem of fulfilling a performance-dependent obligation in the Temple's absence persists. The disappearance verdict is world_rearranges because if the constraint—the ruling that study satisfies obligation—were revoked, the entire post-Temple obligation structure would reorganize. This is not a case of mandatrophy (the founding problem dying while the constraint persists); it is a case of a constraint whose founding problem remains eternally live because the material condition (Temple absence) remains unchanged. The constraint has NOT experienced mandate death. If anything, it experiences continuous mandate reinvigoration: each generation of observant Jews faces the same founding problem and reaffirms the study-fulfillment solution. The constraint shows no signs of persisting after its mandate has atrophied—instead it shows signs of continuous re-validation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitutability_of_study_for_performance,
    'Is study of sacrifice law genuinely equivalent to the performance of sacrifice in fulfilling the obligation, or is it a legitimate substitute that fulfills the obligation in a modified form?',
    'Textual and comparative analysis: does the rabbinic tradition claim strict equivalence or a substitution-with-modification? Examination of halakhic sources that address what counts as ''fulfilling'' the obligation when performance is impossible.',
    'Strict equivalence would support the reading''s framing as a pure coordination solution (study = sacrifice, obligation met fully). Substitution-with-modification would suggest the reading involves a re-interpretation of what the obligation IS, which is a form of restructuring rather than mere substitution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitutability_of_study_for_performance, conceptual, 'Whether study is ontologically equivalent to sacrifice or a legitimate alternative that redefines the obligation.').

omega_variable(
    consensus_durability_vs_coercion,
    'Is the constraint''s persistence sustained by genuine consensus that the reading is legitimate, or by institutional authority suppressing alternative readings?',
    'Historical study of competing readings across different Jewish communities and traditions: do dissenting views (messianic literalists, Karaites) persist openly or are they actively suppressed? Examination of whether communities that adopt alternative readings (messianic-suspension, study-as-archiving) are excluded from rabbinic consensus or excluded from practice itself.',
    'Genuine consensus would confirm the constraint operates as a rope (coordination accepted by beneficiaries). Active suppression of alternatives would suggest some snare-like dynamics (forcing acceptance of the reading). So far, the evidence suggests consensus rather than suppression, but competing contemporary readings of the obligation show the consensus is not total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_durability_vs_coercion, empirical, 'Whether the constraint persists through agreement or through authority-enforced suppression of alternatives.').

omega_variable(
    reading_identity_fusion_and_exit,
    'Is commitment to the ''study_as_occupation'' reading fused with religious identity such that rejecting the reading would require leaving the observant Jewish community, or can the reading be rejected while remaining observant?',
    'Ethnographic study of contemporary communities: do individuals or groups who hold alternative readings (messianic-suspension) remain full participants in observant practice, or are they marginalized? Can one be a fully observant Jew while holding the study-as-archiving reading instead?',
    'If identity-fused (exit requires leaving community), the constraint carries suppression through identity-lock mechanisms even though formal coercion is minimal. If reading rejection is compatible with observance, the constraint operates with genuinely low suppression and minimal identity-binding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion_and_exit, empirical, 'Whether the reading is identity-fused or intellectually contestable within observant practice.').

omega_variable(
    committer_frame_reading_contest,
    'Does the ''study_as_occupation'' reading logically foreclose the ''messianic_suspension'' and ''study_as_archiving'' readings, or do all three readings remain live intellectual positions that coexist across different parties and traditions?',
    'Logical analysis of the core premises: (1) study occupies the obligation vs. (2) study preserves knowledge without fulfilling vs. (3) obligation is suspended—are these mutually exclusive in a single framework, or can different parties maintain them simultaneously? Textual examination of whether rabbinic sources attempt to rule out the alternatives or merely establish their own reading as authoritative.',
    'If foreclosing, this reading''s relationship to siblings is ''forecloses''. If coexisting, the relationship is ''coexists_with''. If this reading influences but does not eliminate the alternatives, the relationship is ''influences''. The classification determines the cs_structure.reading_relations entries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_contest, conceptual, 'Structural relationship between readings: do they logically foreclose each other or coexist as live alternatives?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t250, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 250, 0.06).
narrative_ontology:measurement_basis(temp_tr_t250, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 500, 0.07).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1000, 0.08).
narrative_ontology:measurement_basis(temp_tr_t1000, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.08).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(temp_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t250, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 250, 0.1).
narrative_ontology:measurement_basis(temp_be_t250, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 500, 0.11).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement_basis(temp_be_t1000, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement_basis(temp_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t250, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 250, 0.12).
narrative_ontology:measurement_basis(temp_su_t250, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 500, 0.13).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1000, 0.15).
narrative_ontology:measurement_basis(temp_su_t1000, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement_basis(temp_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_obligation kernel. The sibling readings (study_as_archiving and messianic_suspension) are separate constraint stories with different ε values and different victim/beneficiary structures. The study_as_occupation reading (this one) proposes lowest extractiveness because it frames study as fulfilling the obligation—no unsatisfied obligation, no victims. The study_as_archiving reading proposes moderate extractiveness because it frames study as preserving knowledge WITHOUT fulfilling, leaving the obligation perpetually unpaid—a form of suspended debt. The messianic_suspension reading proposes very low extractiveness because it frames the obligation as suspended, not extracted—waiting but not owed. All three readings share the same referent (the standing commitment to study and perform sacrifice law) and the same material condition (Temple absence, performance impossible). They diverge in how they interpret the obligation's status and whether it is fulfilled, fulfilled-as-modified, archived, or suspended. Analyze them as separate ε-invariant constraints joined by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_occupation, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
