% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment as Performance-Only Obligation (Suspended, Not Fulfilled)
 *   domain: religious/legal/epistemic
 *
 * SUMMARY:
 *   This story instantiates the performance_only reading of the
 *   sacrifice-commandment kernel: sacrificial commandments require physical
 *   execution at the Temple altar, and in the Temple's absence the
 *   commandment is legally suspended (teluyah) rather than fulfilled by any
 *   substitute act, including study. This is one of three readings of the
 *   same kernel — the sibling readings (study_as_performance,
 *   archive_maintenance) are separate constraint stories with their own ε
 *   values and their own beneficiary/victim structures; they are not
 *   represented here except as excluded/observer seats and as network links.
 *   Under this reading specifically, 1,900 years of extraordinarily
 *   sophisticated legal-technical scholarship (Seder Kodashim and its
 *   commentarial tradition) is categorically denied the status of
 *   commandment-fulfillment, no matter how rigorous or devoted, because the
 *   reading's foundational axiom ties fulfillment strictly to physical
 *   performance at a location that does not exist.
 *
 * KEY AGENTS:
 *   - rabbinic_legal_authorities: institutional agenda-setter who rules the suspension classification and controls restoration criteria
 *   - messianic_restorationist_institutions: organized beneficiary whose fundraising and mobilization logic depends on the commandment remaining structurally unfulfilled
 *   - talmudic_sacrifice_study_scholars: identity-locked payers whose career-scale labor is denied present fulfillment status
 *   - lay_practitioners_seeking_present_fulfillment: powerless payers bearing the psychological cost of permanent unfulfillability
 *   - study_as_performance_adherents: excluded voice holding the sibling reading, subordinated but not erased
 *   - comparative_halakhic_scholars: analytical observer tracing the doctrine's historical hardening
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.71).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.52).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment as Performance-Only Obligation (Suspended, Not Fulfilled)").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/legal/epistemic").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '2f23a1b9-e101-4e94-8432-d6018092a369').
narrative_ontology:cs_kernel_codification('2f23a1b9-e101-4e94-8432-d6018092a369', fixed_text).
narrative_ontology:cs_authority_grounding('2f23a1b9-e101-4e94-8432-d6018092a369', lineage).
narrative_ontology:cs_interpretation_layer_present('2f23a1b9-e101-4e94-8432-d6018092a369').
narrative_ontology:cs_reading_relation('2f23a1b9-e101-4e94-8432-d6018092a369', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('2f23a1b9-e101-4e94-8432-d6018092a369', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('2f23a1b9-e101-4e94-8432-d6018092a369', foundational, fulfillment_requires_physical_act_at_altar).
narrative_ontology:cs_axiom_status(fulfillment_requires_physical_act_at_altar, holdable).
narrative_ontology:cs_axiom_grounding('2f23a1b9-e101-4e94-8432-d6018092a369', fulfillment_requires_physical_act_at_altar, conventional).
narrative_ontology:cs_axiom('2f23a1b9-e101-4e94-8432-d6018092a369', foundational, suspension_preserves_obligation_without_substitute_discharge).
narrative_ontology:cs_axiom_status(suspension_preserves_obligation_without_substitute_discharge, holdable).
narrative_ontology:cs_axiom_grounding('2f23a1b9-e101-4e94-8432-d6018092a369', suspension_preserves_obligation_without_substitute_discharge, conventional).
narrative_ontology:cs_reference_frame('2f23a1b9-e101-4e94-8432-d6018092a369', temple_era_physical_performance_standard).
narrative_ontology:cs_drift_state('2f23a1b9-e101-4e94-8432-d6018092a369', post_destruction_rabbinic_consolidation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('2f23a1b9-e101-4e94-8432-d6018092a369', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_legal_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, messianic_restorationist_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, talmudic_sacrifice_study_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, lay_practitioners_seeking_present_fulfillment).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, temple_centrality_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, commandment_indivisibility_from_physical_act).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates and transmits the ruling that korbanot are performance-bound commandments, suspended (not annulled, not fulfilled-by-substitute) absent the Temple. This ruling anchors the legal category 'suspended obligation' (mitzvot ha-teluyot ba-aretz-adjacent framing), which in turn structures how much interpretive authority accrues to those who maintain the technical corpus. Sets the terms under which any future performance could resume, and thereby controls the criteria for restoration itself.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_legal_authorities, agenda_setter,
    institutional, civilizational, analytical, global).

% Organizations and movements oriented toward eventual Temple rebuilding draw legitimacy and fundraising capacity directly from the performance-only reading: if study or memorial practice already fulfilled the commandment, urgency and donor mobilization toward physical restoration would collapse. The suspended-not-fulfilled framing is the structural precondition for their institutional purpose.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_restorationist_institutions, beneficiary,
    organized, civilizational, arbitrage, global).

% Devote careers to mastering Seder Kodashim and sacrificial law in granular technical detail, under a framework that classifies their entire scholarly output as preparatory rather than as commandment-fulfilling in itself. Their labor is real and their expertise is genuine, but the performance-only reading structurally denies that labor present religious weight equal to physical performance, directing life-scale intellectual investment toward an act none of them will live to perform. Exit is blocked by decades of accumulated identity and communal role as a Kodashim scholar; abandoning the framework would strand that identity.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, talmudic_sacrifice_study_scholars, payer,
    moderate, biographical, identity_locked, national).

% Observant individuals who want their religious practice to count as complete in the present are told that an entire category of commandment remains categorically unfulfillable and merely suspended, regardless of any devotion, study, or memorial observance they undertake. They bear the psychological and liturgical cost of praying for restoration of something they structurally cannot access, with no substitute performance recognized as equivalent.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, lay_practitioners_seeking_present_fulfillment, payer,
    powerless, biographical, constrained, global).

% Hold that intellectual engagement with sacrificial law itself constitutes fulfillment of the commandment (the sibling reading). Under the performance-only kernel reading, their position is treated as consolation or misreading rather than as a live legal possibility with equal standing; their argument is acknowledged in commentary literature but does not carry adjudicatory weight in the dominant performance-only framing.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, study_as_performance_adherents, excluded,
    moderate, generational, constrained, global).

% Study the historical development of the suspension doctrine across post-Destruction rabbinic literature, comparing it to the sibling readings and to how other traditions have handled irrecoverable ritual obligations. They can trace how the performance-only reading hardened over centuries but do not adjudicate practice.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, comparative_halakhic_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical, legal, and liturgical coherence of the sacrificial system across a 1,900-year discontinuity, so that if the Temple is ever rebuilt, the commandment can be resumed exactly rather than reconstructed from fragments — a genuine transmission problem given no living practitioners.
% TRANSFER_FUNCTION: Directs enormous cumulative scholarly attention and institutional legitimacy toward the unperformable act and toward those who administer its legal category, while withholding recognition of present-tense fulfillment from lay practitioners and denying career-equivalent standing to those who read study itself as performance.
% ABSENT_VOICES: Practitioners of the study_as_performance reading are present in the literature but structurally subordinated — their argument exists but does not carry the adjudicatory weight the performance-only ruling holds. Lay practitioners who want present completion have no formal channel to contest the suspension classification; it is presented as settled law, not open question.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished, the study_as_performance reading would likely absorb its institutional space, revaluing centuries of Kodashim scholarship as commandment-fulfilling in itself and removing the psychological weight of permanent unfulfillability from lay practice. Restorationist institutions would lose a structural argument for urgency but not necessarily their existence. Rabbinic authorities dispute this: some hold the performance-only reading is simply correct legal description of the text, not a constructed arrangement that could 'disappear.'
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the rabbinic tradition needed to determine the legal status of commandments that had become physically impossible to perform — whether they were annulled, transferred to substitute performance, or suspended pending restoration — to prevent both despair-driven abandonment of the tradition and unauthorized improvised sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities within the tradition attest the founding problem (legal continuity across discontinuity) remains live because restoration remains theologically anticipated. Comparative historians of religion, writing from outside the tradition's own legitimating framework, note that the practical function shifted long ago from continuity-preservation to identity-and-institution-maintenance for scholarly and messianic-organizational classes, since no living generation has ever performed the acts being preserved.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising across the interval because the reading directs an increasing share of cumulative scholarly and institutional attention toward an act that remains, by the reading's own terms, impossible — the study labor itself is real and valuable, but it accrues no fulfillment-credit under this reading, which is precisely what constitutes the extraction (attention and life-labor diverted from what the reading itself defines as 'living law'). Theater ratio rises sharply (0.2 to 0.68) because as centuries pass without a Temple, an increasing proportion of the tradition's energy around this commandment becomes commemorative and performative (liturgical mentions, fast-day laments, memorial texts) rather than functional legal preparation — the suspension itself becomes the thing performed. Suppression is moderate (0.52): there is no coercive apparatus forcing scholars into Kodashim study, but the doctrine structurally forecloses the study_as_performance alternative from carrying equal legal weight, which is a real (if soft) suppression of an available exit. Accessibility collapse is moderate-low (0.4) precisely because the sibling readings remain alive in the literature — this is not a case where alternatives have vanished, only where one reading holds dominant institutional weight. Resistance (0.58) reflects centuries of internal halakhic argument (including proto-forms of the study_as_performance position) pushing back against the strict suspension reading.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, this is simply an accurate description of what the text and legal tradition require — a mountain-like reading of an unambiguous halakhic category, not a constructed extraction. From the scholar-payer seat, the same structure computes as a tangled rope: genuine coordination value (preserving technical continuity) bundled with an asymmetric cost (denial of present fulfillment-credit for a lifetime of labor) that only the dominant reading's institutional position sustains. The engine should register this divergence rather than resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic legal authorities set the classification and bear no personal cost from it — they sit near the beneficiary end structurally, controlling the criteria without paying the extraction. Restorationist institutions are organized beneficiaries whose purpose depends on the suspension persisting. Talmudic scholars and lay practitioners are payers: the scholars pay in denied recognition for identity-locked career investment, and lay practitioners pay in denied present-completion despite full observance. Study-as-performance adherents are excluded rather than victimized outright — their claim exists but is subordinated, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading is not obviously a case of an outlived mandate — the founding problem (legal continuity pending literal restoration) is treated by the tradition itself as still live, since restoration is anticipated rather than ruled out. The mismatch signal here is between founding_problem_status (contested) and disappearance_verdict (contested): both are genuinely disputed rather than one masking the other, which is itself informative — this is not a clean zombie-mandate case, but a live doctrinal fault line where institutional interest (restorationist fundraising, rabbinic classificatory authority) is entangled with a claim that may also be structurally true on its own terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_construction,
    'Is the classification of sacrificial commandments as ''suspended, not fulfilled'' an accurate description of an unambiguous halakhic category fixed at the moment of Temple destruction, or a subsequent institutional consolidation that could have gone differently (i.e., could the tradition have converged on study_as_performance instead)?',
    'Close textual-historical analysis of the earliest post-Destruction rabbinic sources (e.g., Mishnah Taanit, early amoraic statements on Kodashim study) to determine whether the suspension framing was contested at the outset or emerged as consensus only later, and whether early alternative framings functionally equivalent to study_as_performance were live options that were subsequently foreclosed.',
    'If the suspension reading was essentially settled from the earliest sources, this constraint is closer to a genuine doctrinal mountain from within the tradition''s own epistemic framework. If it was one of several live options that hardened into dominance through later institutional consolidation, the high extractiveness score is better explained as a constructed outcome that happened to entrench itself, strengthening the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_construction, conceptual, 'Whether the performance-only classification is doctrinally fixed or a historically contingent institutional consolidation among live alternatives.').

omega_variable(
    restoration_institution_incentive_alignment,
    'To what extent does the persistence of the strict performance-only reading track genuine theological commitment to eventual Temple restoration, versus track the institutional and fundraising interests of restorationist organizations that benefit from urgency generated by unfulfillability?',
    'Compare doctrinal positions and institutional funding patterns across communities that hold the performance-only reading strongly versus those more sympathetic to study_as_performance or archive_maintenance, controlling for the presence or absence of organized restorationist fundraising infrastructure.',
    'Strong correlation between reading strength and institutional fundraising dependence would support classifying the beneficiary structure as more concentrated and extractive than a purely doctrinal reading would suggest; weak correlation would support treating the reading as theologically autonomous.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_institution_incentive_alignment, empirical, 'Whether restorationist institutional interest, rather than doctrine alone, sustains the strict suspension classification.').

omega_variable(
    kernel_reading_dominance_mechanism,
    'Why did the performance_only reading achieve and retain dominant institutional/legal weight over the study_as_performance and archive_maintenance readings across most of rabbinic history, despite all three being textually arguable?',
    'Trace the citation and adjudicatory history of the three reading-families across major halakhic codifiers (Rambam, Shulchan Aruch commentators, and successors) to identify whether dominance tracked argumentative strength, institutional power of particular schools, or path-dependent early codification.',
    'This bears directly on how the reading_relations in cs_structure should be weighted going forward and on whether future reclassification of the kernel is plausible or foreclosed in practice even where not foreclosed in principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance_mechanism, conceptual, 'The historical mechanism by which one reading of the kernel became institutionally dominant among textually live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t0, projected).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_commandment__performance_only, theater_ratio, 300, 0.3).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_commandment__performance_only, theater_ratio, 700, 0.45).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_commandment__performance_only, theater_ratio, 1100, 0.55).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.62).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.68).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sacr_be_t0, projected).
narrative_ontology:measurement(sacr_be_t300, sacrifice_commandment__performance_only, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(sacr_be_t700, sacrifice_commandment__performance_only, base_extractiveness, 700, 0.55).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_commandment__performance_only, base_extractiveness, 1100, 0.62).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.67).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(sacr_su_t0, projected).
narrative_ontology:measurement(sacr_su_t300, sacrifice_commandment__performance_only, suppression_requirement, 300, 0.35).
narrative_ontology:measurement(sacr_su_t700, sacrifice_commandment__performance_only, suppression_requirement, 700, 0.4).
narrative_ontology:measurement(sacr_su_t1100, sacrifice_commandment__performance_only, suppression_requirement, 1100, 0.44).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.1).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the sacrifice commandment after the Temple's destruction' per the ε-invariance principle. performance_only (this story, high ε ~0.71) denies fulfillment-credit to study; study_as_performance (sibling, lower expected ε) grants full fulfillment-credit to study, removing the extraction this story identifies; archive_maintenance (sibling, lowest expected ε) makes no fulfillment claim at all, framing study as pure preparatory infrastructure, which sidesteps the extraction question by denying that fulfillment is currently at stake. The three readings cannot be merged into one constraint because they assign structurally different beneficiary/victim sets and different ε values to the same underlying textual kernel; each is authored as its own file and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
