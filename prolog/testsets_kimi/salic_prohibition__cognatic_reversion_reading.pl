% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Cognatic Reversion Succession Rule (Non-Frankish Territories)
 *   domain: constitutional/dynastic_law
 *
 * SUMMARY:
 *   The Salic Law prohibition on female succession is interpreted by the
 *   cognatic reversion reading as a strictly Frankish legal custom with no
 *   binding force on non-Frankish territories. Under this reading, succession
 *   follows cognatic primogeniture (eldest child regardless of sex), and
 *   territorial integrity supersedes agnatic purity. This reading was
 *   deployed to justify female succession in realms such as Spain and the
 *   Austrian Habsburg lands, directly contesting the immutable mandate and
 *   sovereign override readings that upheld agnatic exclusion. The constraint
 *   story models the operative dynastic succession rule under this reading: a
 *   coordination mechanism that keeps realms intact by permitting direct
 *   cognatic inheritance, while asymmetrically dispossessing collateral
 *   agnatic lines that would otherwise succeed.
 *
 * KEY AGENTS:
 *   - cognatic_successors: Primary beneficiary (moderate/identity_locked) â inherit thrones under cognatic primogeniture
 *   - collateral_agnatic_lines: Primary target (powerful/mobile) â lose succession rights to cognatic heirs and often resist by force
 *   - dynastic_councils: Agenda-setter (institutional/constrained) â adjudicate succession claims and interpret territorial law
 *   - frankish_jurists: Excluded voice (institutional/constrained) â argue for universal agnatic validity but are not seated in non-Frankish councils
 *   - foreign_courts: Analytical observer (institutional/analytical) â recognize or contest the legitimacy of cognatic heirs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.72).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.75).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Cognatic Reversion Succession Rule (Non-Frankish Territories)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional/dynastic_law").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '410078d7-f1e8-4d49-8372-96c3481c03d4').
narrative_ontology:cs_kernel_codification('410078d7-f1e8-4d49-8372-96c3481c03d4', fixed_text).
narrative_ontology:cs_authority_grounding('410078d7-f1e8-4d49-8372-96c3481c03d4', practice).
narrative_ontology:cs_interpretation_layer_present('410078d7-f1e8-4d49-8372-96c3481c03d4').
narrative_ontology:cs_reading_relation('410078d7-f1e8-4d49-8372-96c3481c03d4', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('410078d7-f1e8-4d49-8372-96c3481c03d4', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('410078d7-f1e8-4d49-8372-96c3481c03d4', foundational, territorial_integrity_over_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_over_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('410078d7-f1e8-4d49-8372-96c3481c03d4', territorial_integrity_over_agnatic_purity, conventional).
narrative_ontology:cs_axiom('410078d7-f1e8-4d49-8372-96c3481c03d4', foundational, salic_law_limited_frankish_jurisdiction).
narrative_ontology:cs_axiom_status(salic_law_limited_frankish_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('410078d7-f1e8-4d49-8372-96c3481c03d4', salic_law_limited_frankish_jurisdiction, empirically_contingent).
narrative_ontology:cs_reference_frame('410078d7-f1e8-4d49-8372-96c3481c03d4', cognatic_succession_custom).
narrative_ontology:cs_drift_state('410078d7-f1e8-4d49-8372-96c3481c03d4', salic_encroachment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('410078d7-f1e8-4d49-8372-96c3481c03d4', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, cognatic_successors).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, collateral_agnatic_lines).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit thrones and territories under cognatic primogeniture when the direct line produces no male heir. Their claim depends entirely on the constraint's enforcement; they are locked into the dynastic identity and cannot exit the succession role. They benefit from territorial integrity but bear the risk of contestation, deposition, or assassination.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_successors, beneficiary,
    moderate, biographical, identity_locked, national).

% Would inherit under strict agnatic rules but are bypassed by cognatic heirs under the territorial integrity principle. They often possess independent military resources, allied armies, and foreign support, enabling them to resist the constraint through war or diplomacy. Their exit is mobile: they can fight, seek foreign backing, negotiate compensation, or accept dispossession.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, collateral_agnatic_lines, payer,
    powerful, biographical, mobile, continental).

% Adjudicate succession claims by interpreting territorial customary law and dynastic house laws. They enforce the cognatic reversion rule by validating direct descendants and excluding collateral agnatic claimants. They are constrained by political pressure from the incumbent dynasty, the military balance, and the threat of foreign intervention.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, dynastic_councils, agenda_setter,
    institutional, generational, constrained, national).

% Maintain that Salic Law is universally binding divine or fundamental customary law. They are structurally excluded from non-Frankish dynastic councils where the cognatic rule is adjudicated, though their writings circulate as persuasive authority. They cannot exit the interpretive tradition without abandoning their professional and doctrinal identity.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, frankish_jurists, excluded,
    institutional, generational, constrained, continental).

% Observe and recognize, or refuse to recognize, cognatic succession claims based on their own political interests, dynastic treaties, and balance-of-power calculations. They do not set the rule but their recognition determines whether the succession achieves international legitimacy. They remain analytically outside the constraint.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, foreign_courts, observer,
    institutional, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, cognatic_successors).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents fragmentation of dynastic realms by allowing direct descendants (including females) to inherit rather than splitting territories among agnatic collaterals or merging them with foreign agnatic lines.
% TRANSFER_FUNCTION: Moves dynastic title and territorial sovereignty from collateral agnatic lines to direct cognatic descendants (including females), under the principle that territorial integrity supersedes agnatic purity.
% ABSENT_VOICES: Frankish jurists and clergy who argue for the immutable divine or customary validity of universal agnatic succession; agnatic claimants in excluded collateral branches who are not seated in the councils that decide the rule.
% DISAPPEARANCE_RATIONALE: Territories held together by cognatic succession (such as Spain or the Austrian Habsburg lands) would face immediate contested successions, partition treaties, or absorption by agnatic claimants. Dynastic maps would reorganize around agnatic exclusion within a generation.
% FOUNDING_PROBLEM: Dynastic fragmentation and succession crises caused by strict agnatic exclusion of direct descendants when no male heir exists; the need to keep realms intact under a single sovereign rather than partitioning them among distant male relatives.
% FOUNDING_PROBLEM_CORROBORATION: Non-beneficiary diplomatic archives and neutral observers (e.g., Venetian ambassadors, Papal nuncios) documented succession crises and territorial fragmentation under strict agnatic exclusion in comparable territories, corroborating the problem from outside the cognatic successor seat. Collateral agnatic lines dispute the framing entirely.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72 at interval end) is high because entire realms are transferred away from agnatic collateral branches to direct cognatic descendants. Suppression (0.75) is high because the rule's persistence requires suppressing agnatic claimants who often possess independent military capacity and foreign support. Theater ratio (0.35) is moderate: heraldic and legal ritual is present, but the underlying transfer of sovereignty is materially consequential. Accessibility collapse (0.45) is moderate â agnatic alternatives are legally excluded but remain politically latent. Resistance (0.70) is high, evidenced by succession wars launched by displaced agnatic claimants.
 *
 * PERSPECTIVAL GAP:
 *   From the cognatic heir's seat, the constraint is legitimate succession law preserving territorial integrity; from the collateral agnatic seat, it is expropriation dressed as legal reform. The dynastic council experiences it as a necessary coordination device to prevent fragmentation. The engine computes this divergence from structural data: same constraint, opposing directionalities for heirs versus displaced claimants.
 *
 * DIRECTIONALITY LOGIC:
 *   Cognatic successors are structural beneficiaries (low d): the constraint transfers sovereignty to them. Collateral agnatic lines are structural targets (high d): they bear the loss of expected inheritance. Dynastic councils sit near symmetric (moderate d): they administer the rule and gain stability but bear the political cost of enforcement. Frankish jurists are structurally excluded; their directionality is irrelevant because they are not in the room where the constraint is applied.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a piton because its coordination function (preventing dynastic fragmentation) remains live and its extractive transfer (dispossessing collaterals) is structurally coupled to that function â territorial integrity under this reading cannot be maintained without transferring the realm away from the agnatic collateral line. It is not a snare because the coordination is genuine: realms do hold together under cognatic succession. The theater ratio (0.35) does not indicate atrophy; it indicates moderate ritual surrounding a still-functional power transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frankish_jurisdiction_boundary,
    'Is the distinction between Frankish and non-Frankish jurisdiction a historically defensible legal boundary or a post-hoc rationalization fabricated to justify cognatic claims?',
    'Comparative legal-historical analysis of pre-Salic succession customs in Iberian, Italian, and Germanic territories versus Frankish heartlands.',
    'If the boundary is fabricated, this reading loses its primary grounding and collapses toward sovereign_override or immutable_mandate depending on whether the law is treated as positive or divine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frankish_jurisdiction_boundary, conceptual, 'Whether the Frankish jurisdictional limit is historically genuine').

omega_variable(
    cognatic_succession_conflict_frequency,
    'Does cognatic succession actually reduce the frequency of succession wars relative to agnatic partition, or does it merely internationalize conflict by inviting foreign agnatic intervention?',
    'Quantitative historical analysis of armed succession disputes under cognatic versus agnatic regimes in early modern Europe.',
    'If cognatic succession increases net conflict, the coordination function is undermined and the constraint shifts toward snare; if it decreases conflict, the coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognatic_succession_conflict_frequency, empirical, 'Whether cognatic succession reduces or displaces dynastic warfare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t4, salic_prohibition__cognatic_reversion_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__cognatic_reversion_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(sali_tr_t12, salic_prohibition__cognatic_reversion_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__cognatic_reversion_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__cognatic_reversion_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sali_be_t4, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(sali_be_t12, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sali_su_t4, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(sali_su_t12, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
