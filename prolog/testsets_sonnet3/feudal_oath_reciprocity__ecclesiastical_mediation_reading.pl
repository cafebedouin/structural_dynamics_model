% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Bounded by Ecclesiastical Charity Doctrine
 *   domain: medieval political economy / legal history / institutional analysis
 *
 * SUMMARY:
 *   This story reads the feudal oath through the lens of ecclesiastical
 *   mediation: the church treats the vassal's oath as sacramentally binding,
 *   and holds the lord's exercise of lordship to a standard of Christian
 *   charity that limits arbitrary or excessive extraction. Under this reading
 *   the oath is a hybrid structure — it genuinely coordinates lord and vassal
 *   by giving the weaker party a credible sanction against overreach, but it
 *   also erects the church as an extractive interpretive authority that
 *   collects fees, penance revenue, and jurisdictional standing from every
 *   dispute it adjudicates, while leaving unfree peasant tenants (who never
 *   swore the oath) essentially outside its protective reach. This is a
 *   distinct constraint from the lord_extraction_reading (which treats the
 *   oath as authorizing maximal extraction bounded only by capacity) and the
 *   vassal_coordination_reading (which treats obligations as fixed and
 *   charter-enforced); each reading has its own epsilon and its own
 *   stakeholder structure, linked here only by shared kernel identity, not
 *   shared classification.
 *
 * KEY AGENTS:
 *   - diocesan_clergy: agenda_setter (institutional/arbitrage) — adjudicates charity violations, collects jurisdictional revenue
 *   - extractive_lords: payer (powerful/constrained) — bound by sacramental consequence beyond customary limits
 *   - vassal_smallholders: beneficiary/payer (moderate/constrained) — gains a real but unevenly enforced check
 *   - peasant_tenants: payer (powerless/trapped) — outside the sworn-oath relationship the doctrine governs
 *   - ecclesiastical_courts: beneficiary (institutional/arbitrage) — analytical seat that also extracts
 *   - royal_secular_courts: excluded (institutional/constrained) — sidelined by the sacramental framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.48).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Bounded by Ecclesiastical Charity Doctrine").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval political economy / legal history / institutional analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '911c8d90-a577-4637-9494-164a00b81151').
narrative_ontology:cs_kernel_codification('911c8d90-a577-4637-9494-164a00b81151', distributed).
narrative_ontology:cs_authority_grounding('911c8d90-a577-4637-9494-164a00b81151', lineage).
narrative_ontology:cs_interpretation_layer_present('911c8d90-a577-4637-9494-164a00b81151').
narrative_ontology:cs_reading_relation('911c8d90-a577-4637-9494-164a00b81151', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('911c8d90-a577-4637-9494-164a00b81151', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('911c8d90-a577-4637-9494-164a00b81151', foundational, sacramental_oath_binds_conscience_above_secular_custom).
narrative_ontology:cs_axiom_status(sacramental_oath_binds_conscience_above_secular_custom, holdable).
narrative_ontology:cs_axiom_grounding('911c8d90-a577-4637-9494-164a00b81151', sacramental_oath_binds_conscience_above_secular_custom, theological).
narrative_ontology:cs_axiom('911c8d90-a577-4637-9494-164a00b81151', foundational, caritas_imposes_substantive_limit_on_lordly_exaction).
narrative_ontology:cs_axiom_status(caritas_imposes_substantive_limit_on_lordly_exaction, holdable).
narrative_ontology:cs_axiom_grounding('911c8d90-a577-4637-9494-164a00b81151', caritas_imposes_substantive_limit_on_lordly_exaction, deontological).
narrative_ontology:cs_reference_frame('911c8d90-a577-4637-9494-164a00b81151', sacramental_fealty_under_divine_witness).
narrative_ontology:cs_drift_state('911c8d90-a577-4637-9494-164a00b81151', high_medieval_jurisdictional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('911c8d90-a577-4637-9494-164a00b81151', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_courts).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, diocesan_clergy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_smallholders).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractive_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasant_tenants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_smallholders).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_binds_conscience).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, caritas_limits_lordly_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates whether a lord's exactions violate the charity and good-faith conditions attached to the oath sworn on relics or Gospels. Can impose penance, threaten excommunication, or withhold sacraments from a lord judged to have broken faith. Gains standing as the interpretive authority sitting above both lord and vassal, and collects tithes, legacies, and jurisdictional fees that flow from that position.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, diocesan_clergy, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Swore the oath on holy relics and is therefore bound not merely by feudal custom but by sacramental consequence — breaking faith risks the soul, not just the fief. Wants to raise levies, extend labor duties, or seize wardship revenue beyond customary bounds, but faces ecclesiastical censure, interdict threats, or being named oath-breaker before peers if the church judges the extraction uncharitable. Cannot simply repudiate the church's interpretive role without risking legitimacy among other lords and his own vassals.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractive_lords, payer,
    powerful, generational, constrained, regional).

% Holds land in exchange for service and swore reciprocal oath; can appeal to ecclesiastical judgment when a lord's demands exceed customary or charitable bounds, gaining a real check they would otherwise lack. Still owes labor, military service, and dues under the underlying feudal bargain, and church intervention is uneven — it depends on local clergy's willingness to confront a powerful patron.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_smallholders, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_smallholders, payer).

% Bears the base weight of labor dues, tallage, and customary payments regardless of which reading of the oath prevails; the ecclesiastical charity doctrine occasionally moderates the worst exactions but was never designed with the unfree tenant's own oath status in view, since most peasants swore no formal oath and sit outside the mutual-fealty relationship the doctrine directly governs.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasant_tenants, payer,
    powerless, biographical, trapped, local).

% Hears disputes over broken faith and oath violation, extending church jurisdiction into what would otherwise be a purely secular lord-vassal matter. Each case adjudicated strengthens the precedent that lordly power is conditional on ecclesiastical standards of charity, expanding the court's reach and revenue from fees, penance commutations, and jurisdictional fines.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_courts, beneficiary,
    institutional, civilizational, arbitrage, regional).

% Would prefer that disputes over feudal obligation stay within royal or seigneurial jurisdiction rather than being reframed as matters of conscience and sacrament subject to church courts. Has limited standing to contest the doctrine directly without appearing to attack the sacramental basis of oath-taking itself, which underwrites royal oaths of fealty too.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, royal_secular_courts, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuine, mutually recognized limit on lordly extraction by attaching the vassal oath to sacramental consequence: a lord who violates charity-bound obligations risks spiritual and reputational sanction, which gives vassals a credible check that pure secular custom alone would not sustain.
% TRANSFER_FUNCTION: Moves interpretive authority and adjudicatory fees toward ecclesiastical institutions; moves a measure of protection from arbitrary exaction toward oath-bound vassals; leaves the base burden on unfree peasant tenants largely untouched since they are outside the sworn-oath relationship this doctrine directly governs.
% ABSENT_VOICES: Peasant tenants who owe labor and dues but never swore the formal reciprocal oath have no standing before the ecclesiastical court under this doctrine — their exploitation is invisible to a framework built around sworn fealty between propertied parties. Royal secular courts are also structurally sidelined, unable to contest a doctrine grounded in sacrament without undermining their own oath-based legitimacy claims.
% DISAPPEARANCE_RATIONALE: If ecclesiastical mediation of the oath vanished, lords would face only customary and coercive checks on extraction — vassals would lose the credible threat of spiritual and reputational sanction, church courts would lose a major source of jurisdiction and revenue, and disputes over broken faith would default to purely secular arbitration or force, changing both the pace and ceiling of lordly exaction.
% FOUNDING_PROBLEM: Feudal reciprocity had no reliable enforcement mechanism beyond the lord's own restraint and the vassal's capacity for armed resistance; the church supplied a shared moral vocabulary (charity, good faith, sacramental consequence) that both parties recognized as binding, giving the weaker party a lever the secular relationship alone did not provide.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers and canon lawyers within the church attest the doctrine still functions as a live check on lordly overreach in the period. Secular legal historians and, later, royal court records suggest that by the point secular kingship consolidated, the doctrine had become more often invoked as leverage in inter-elite disputes than as protection for ordinary vassals, and rarely reached unfree tenants at all — corroboration from outside the ecclesiastical beneficiary is mixed rather than clearly affirming.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint is genuinely hybrid: it caps lordly exaction below what pure secular custom would tolerate, but it does so by installing the church as a second extractive layer collecting fees and jurisdictional revenue from every enforcement action. Suppression (0.55) reflects the real coercive weight of excommunication and interdict threats used to compel compliance — this is not merely persuasive moral authority but backed by sanctions with severe social and political consequences for a medieval lord. Theater ratio is moderate-low (0.32) and rises modestly over the two-century interval as the doctrine's protective enforcement becomes more selectively invoked in inter-elite disputes rather than genuinely defending ordinary vassals, consistent with the founding_problem drifting from contested toward partially performative.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical court's seat, this doctrine is a coherent extension of pastoral care over consciences bound by sacred oath. From the extractive lord's seat, it is an external constraint imposed by an institution with its own extractive interest in the dispute. From the peasant tenant's seat — who never swore the oath at all — the entire apparatus is structurally invisible; whatever protection it offers never reaches them. The engine should compute these as different seat-level types from the same structural data, not reconcile them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Diocesan clergy and ecclesiastical courts sit near the beneficiary end: they gain interpretive authority and extractive revenue without bearing the underlying costs of lordship or tenancy. Extractive lords sit toward the target end: sacramental consequence constrains behavior they would otherwise control unilaterally. Vassal smallholders are genuinely mixed — real protective benefit, but still owe the underlying feudal service, so directionality sits near symmetric. Peasant tenants are declared as payers who receive essentially none of the doctrine's protective benefit, placing them near the full-target end despite not being oath parties at all — their directionality here is closer to that of a bystander bearing costs of a system built around others.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — lack of any credible check on lordly extraction — was genuinely live at the doctrine's origin, which is why this reading is authored as tangled_rope rather than snare: there is a real coordination function alongside the extraction. The mandatrophy risk is that as royal secular courts consolidate independent jurisdiction over feudal disputes in later centuries, the church's founding rationale weakens while its jurisdictional fee-collection persists, which is exactly the live/dead status ambiguity captured in founding_problem_status: contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_sincerity_vs_instrumental_capture,
    'Was ecclesiastical mediation of feudal oaths a sincere pastoral extension of charity doctrine, or was it substantially an instrumental strategy by the church to capture jurisdiction and revenue from secular lord-vassal disputes?',
    'Comparative analysis of ecclesiastical court case records: does enforcement correlate with genuine grievance severity, or with the fee/jurisdictional value of the dispute and the political leverage available against the lord in question?',
    'If substantially instrumental, the coordination function claimed here is thinner than authored and the constraint drifts toward the lord_extraction_reading''s assessment that the doctrine is cover for a different extraction; if substantially sincere, the tangled_rope classification with genuine coordination is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_sincerity_vs_instrumental_capture, empirical, 'Whether ecclesiastical mediation was sincere pastoral function or jurisdictional capture strategy.').

omega_variable(
    reading_boundary_which_kernel_reading_governs_a_given_dispute,
    'In any actual medieval dispute, which of the three kernel readings (ecclesiastical mediation, lord extraction, vassal coordination charter-text) actually governed the outcome, and was this determined by which party had more leverage to invoke their preferred framing rather than by the oath''s inherent structure?',
    'Case-by-case study of dispute resolution outcomes: track which authority (ecclesiastical court, secular charter enforcement, or unilateral lordly assertion) actually prevailed and correlate with relative power of the parties.',
    'If outcome consistently tracks power rather than doctrine, all three kernel readings may be post-hoc rationalizations for outcomes actually determined by capacity — this would suggest the kernel itself, not just this reading, requires reassessment as description versus legitimating narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_which_kernel_reading_governs_a_given_dispute, conceptual, 'Whether the three sibling readings describe real distinct mechanisms or post-hoc framings of power-determined outcomes.').

omega_variable(
    peasant_exclusion_naturalness,
    'Is the exclusion of unfree peasant tenants from oath-based protection a natural consequence of the oath''s legal form (they were never parties to it), or a constructed boundary that could have been extended but was not because doing so would have reduced lordly and ecclesiastical revenue alike?',
    'Examine whether any ecclesiastical authorities of the period argued for extending charity-doctrine protections to unfree tenants, and what became of such arguments.',
    'If extension was argued and blocked by joint lord-church interest, the peasant exclusion is itself part of the extractive structure rather than a neutral boundary condition, raising the effective extractiveness of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_exclusion_naturalness, conceptual, 'Whether peasant exclusion from the doctrine was structurally necessary or an interested construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(feud_tr_t120, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 120, 0.27).
narrative_ontology:measurement(feud_tr_t160, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 160, 0.3).
narrative_ontology:measurement(feud_tr_t200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 200, 0.32).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(feud_be_t120, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 120, 0.46).
narrative_ontology:measurement(feud_be_t160, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 160, 0.47).
narrative_ontology:measurement(feud_be_t200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 200, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement(feud_su_t120, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 120, 0.52).
narrative_ontology:measurement(feud_su_t160, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 160, 0.53).
narrative_ontology:measurement(feud_su_t200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 200, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
