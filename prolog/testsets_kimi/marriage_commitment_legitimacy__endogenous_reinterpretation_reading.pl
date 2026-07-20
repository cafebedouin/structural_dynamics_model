% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Revelation Reading of the 1890 Manifesto Marriage Commitment Reversal
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   In 1890, the Church of Jesus Christ of Latter-day Saints issued a
 *   Manifesto ending plural marriage. The endogenous_reinterpretation reading
 *   treats this document as genuine prophetic revelation: God commanded the
 *   reversal to preserve the Church from federal dissolution, reframing
 *   monogamy as a new covenant stage rather than a doctrinal rupture. This
 *   constraint binds membership to accept the reversal as divine, coordinates
 *   the community around the new practice, and preserves prophetic succession
 *   legitimacy. It is authored as a tangled_rope with low extractiveness
 *   because the genuine coordination function (institutional survival,
 *   communal unity) is coupled with asymmetric cost-bearing (practitioners
 *   abandon plural theology and families; membership absorbs doctrinal
 *   whiplash) while the hierarchy accrues legitimacy. The claim/metric
 *   independence is honored: the reading presents itself as rope-like divine
 *   coordination, while the metrics acknowledge moderate structural
 *   extraction in the enforcement and reframing costs.
 *
 * KEY AGENTS:
 *   - church_presidency: Agenda-setter (institutional/generational/constrained) â issues and administers the revelatory framing
 *   - church_membership: Dual-position beneficiary/payer (organized/biographical/identity_locked) â benefits from survival, pays doctrinal reversal costs
 *   - plural_marriage_practitioners: Primary payer (powerless/biographical/trapped) â bear direct practice-dissolution costs under legal and ecclesiastical pressure
 *   - federal_authorities: Excluded catalyst (institutional/biographical/analytical) â applied existential pressure but are excluded from the theological legitimacy narrative
 *   - independent_historians: Analytical observer (analytical/generational/analytical) â attest survival threat but do not corroborate revelatory claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.32).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.38).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Endogenous Revelation Reading of the 1890 Manifesto Marriage Commitment Reversal").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'd8e299aa-53c5-4eaa-8f2f-3a94fcf1053d').
narrative_ontology:cs_kernel_codification('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', fixed_text).
narrative_ontology:cs_authority_grounding('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', lineage).
narrative_ontology:cs_interpretation_layer_present('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d').
narrative_ontology:cs_reading_relation('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', foundational, prophetic_revelation_manifesto).
narrative_ontology:cs_axiom_status(prophetic_revelation_manifesto, holdable).
narrative_ontology:cs_axiom_grounding('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', prophetic_revelation_manifesto, theological).
narrative_ontology:cs_axiom('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', foundational, monogamy_as_new_covenant_stage).
narrative_ontology:cs_axiom_status(monogamy_as_new_covenant_stage, holdable).
narrative_ontology:cs_axiom_grounding('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', monogamy_as_new_covenant_stage, theological).
narrative_ontology:cs_reference_frame('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', prophetic_authority_framework).
narrative_ontology:cs_drift_state('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', post_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d8e299aa-53c5-4eaa-8f2f-3a94fcf1053d', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_presidency).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_marriage_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_legitimacy).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, doctrinal_continuity_through_reframing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued and promulgates the Manifesto as binding divine revelation; administers the theological reframing of monogamy as a new covenant stage; enforces compliance through excommunication and doctrinal instruction; their prophetic authority and institutional legitimacy depend on maintaining the endogenous revelation frame.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_presidency, agenda_setter,
    institutional, generational, constrained, global).

% Receive the Manifesto as binding doctrine; benefit from the Church's preserved federal standing and institutional continuity; pay by accepting a major reversal of prior sacred practice, abandoning plural marriage theology, and reorienting family structure around monogamy. Religious identity fusion makes exit psychologically and socially prohibitive.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_membership, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_membership, payer).

% Bear the immediate practical and theological costs of the reversal; must dissolve plural unions or face excommunication and federal criminal prosecution; their prior religious practice is reframed as discontinued rather than eternally binding. They lack institutional power to resist the doctrinal change.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_marriage_practitioners, payer,
    powerless, biographical, trapped, national).

% Applied confiscation, disincorporation, and imprisonment pressure that created the existential crisis prompting the Manifesto. They are excluded from the theological legitimacy conversation because the Church frames the response as internal revelation rather than external capitulation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_authorities, excluded,
    institutional, biographical, analytical, national).

% Analyze federal enforcement records, Church leadership deliberations, and doctrinal shifts. They corroborate the survival threat from outside the benefiting parties but do not corroborate the specific revelatory claims, which are self-asserted by the Church hierarchy.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, independent_historians, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the Church's institutional existence and communal unity under existential federal pressure by reframing monogamous marriage as a divinely authorized new covenant stage, thereby maintaining theological continuity and prophetic legitimacy for a dispersed membership.
% TRANSFER_FUNCTION: Moves doctrinal authority from plural marriage theology to monogamous covenant theology; moves compliance burden and practice-change costs from the institution to the membership, with concentrated costs on plural marriage practitioners who must dissolve families and abandon prior sacred commitments.
% ABSENT_VOICES: Theological fundamentalists who view the reversal as doctrinal corruption; practicing polygamists who experienced family dissolution and spiritual betrayal; secular historians who read the Manifesto as political accommodation rather than supernatural communication. These voices are systematically excluded from the institutional legitimacy narrative through doctrinal boundary maintenance.
% DISAPPEARANCE_RATIONALE: If the endogenous revelation framing vanished overnight, the Church's claim to theological continuity would collapse, prophetic succession would be delegitimized, and the community would likely fracture between capitulation narratives and schismatic resistance movements. The institutional identity is organized around this constraint.
% FOUNDING_PROBLEM: Federal confiscation of Church property, disincorporation, and imprisonment of leaders under anti-polygamy laws threatened the Church's survival as an institution in the 1880s.
% FOUNDING_PROBLEM_CORROBORATION: Federal government records and independent historians outside the Church attest the enforcement pressure and survival threat. However, the specific revelatory status of the Manifesto is attested only by the Church hierarchy itself; no external non-beneficiary source corroborates the supernatural causation claim.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.32 (low-moderate) because the constraint genuinely coordinates collective survival but asymmetrically transfers doctrinal and practice-change costs to membership and practitioners. Suppression (0.38) reflects the institutional enforcement needed to reframe plural marriage as superseded and to marginalize resistance. Theater_ratio (0.30) captures the performative work of presenting a federal-exigency reversal as an internally generated new covenant stage; this declines over the measurement interval as generational normalization advances. Accessibility_collapse (0.45) is moderate because alternative readings (exogenous, hybrid) circulate outside the institutional frame, though within the frame alternatives are theologically inaccessible. Resistance (0.35) reflects persistent fundamentalist and practitioner opposition that was actively suppressed in the first decades after 1890.
 *
 * PERSPECTIVAL GAP:
 *   The church_presidency seat experiences the constraint as preservation of authority and communal salvation; the plural_marriage_practitioners seat experiences it as enforced abandonment of sacred covenant and family structure. The church_membership seat is split: many experience relief and continuity, while others experience identity rupture. The engine will compute different per-seat classifications from these structural asymmetries; the agenda_setter seat likely computes closer to rope, while the trapped practitioner seat computes closer to snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The church_presidency is the structural beneficiary (d near 0.0): the constraint subsidizes their prophetic legitimacy and institutional survival. The church_membership sits near symmetric (d ~0.4) because they receive survival benefits while paying doctrinal compliance costs. Plural_marriage_practitioners are full targets (d near 1.0): they pay through dissolved families, excommunication risk, and theological displacement. Federal_authorities and independent_historians are external with analytical exit; their directionality is analytically neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead: the federal government ceased anti-polygamy enforcement decades ago. However, the constraint persists because the revelation framing cannot be abandoned without destroying the prophetic succession logic. The R5 genealogy interview (founding_problem_status: dead + disappearance_verdict: world_rearranges) flags a potential mandatrophy capture, but the theater_ratio is not high enough to qualify as pure piton. Instead, the constraint reads as a tangled_rope whose coordination function (maintaining a unified global church) has partially replaced its founding extraction-reduction function, while still extracting compliance costs from specific victim seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_classification,
    'Does the endogenous_reinterpretation reading of the marriage_commitment_legitimacy kernel represent genuine coordination, or does its classification converge toward snare under the exogenous_override reading?',
    'Comparative corpus analysis of all three kernel readings; evaluate whether the revelatory claim is structurally separable from the institutional survival function.',
    'If the revelatory claim is inseparable from survival strategy, this reading reclassifies as tangled_rope with higher theater_ratio; if separable, it remains low-extraction coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_classification, conceptual, 'Kernel reading location and structural classification ambiguity.').

omega_variable(
    federal_pressure_causality,
    'Did federal pressure function as catalyst, sufficient cause, or irrelevant context for the Manifesto?',
    'Historiographic review of federal enforcement timelines versus Church leadership revelation claims; archival analysis of internal deliberations.',
    'If federal pressure was sufficient cause, the endogenous reading''s core premise is undermined; if catalyst, it remains viable; if irrelevant, it strengthens toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_pressure_causality, empirical, 'Ambiguity about the causal status of federal pressure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For members bearing the doctrinal reversal cost, is compliance driven by internalized identity-lock or structural institutional barriers?',
    'Post-exit trajectory analysis: if suppressed dissent persists after social and structural exit from the Church, reclassify as partially internalized.',
    'Internalized suppression increases effective extraction beyond the structural measure and shifts the constraint toward snare-like dynamics for identity-locked members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(marr_tr_t35, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 35, 0.3).
narrative_ontology:measurement(marr_tr_t65, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 65, 0.24).
narrative_ontology:measurement(marr_tr_t95, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 95, 0.2).
narrative_ontology:measurement(marr_tr_t130, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 130, 0.16).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(marr_be_t35, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 35, 0.3).
narrative_ontology:measurement(marr_be_t65, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 65, 0.26).
narrative_ontology:measurement(marr_be_t95, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 95, 0.23).
narrative_ontology:measurement(marr_be_t130, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 130, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(marr_su_t35, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 35, 0.4).
narrative_ontology:measurement(marr_su_t65, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 65, 0.32).
narrative_ontology:measurement(marr_su_t95, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 95, 0.25).
narrative_ontology:measurement(marr_su_t130, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 130, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_legitimacy kernel. The kernel is the 1890 Manifesto's legitimacy claim; each reading instantiates a structurally distinct constraint with different epsilon values and beneficiary/victim structures. The upstream kernel claim influences all dependent readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
