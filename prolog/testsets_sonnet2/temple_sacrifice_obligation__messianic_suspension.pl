% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Suspended Sacrificial Obligation Pending Messianic Restoration
 *   domain: religious/legal/theological
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple, the commandments of
 *   animal sacrifice became impossible to perform in their prescribed form.
 *   Rather than declare the community in permanent violation of divine law,
 *   or declare the obligation fulfilled by some substitute, this reading
 *   holds that the obligation is suspended — held in abeyance, neither
 *   discharged nor breached — pending a future messianic restoration of the
 *   Temple and its service. This is one of three structurally distinct
 *   readings of the same underlying kernel (the status of the sacrificial
 *   commandments post-Destruction); the other two (study-as-occupation,
 *   study-as-archiving) are separate constraint stories linked by network
 *   edges, not measurement variants of this one.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: administer the suspension doctrine and its deferral logic
 *   - observant_community: relieved of an impossible duty without incurring guilt
 *   - individual_worshippers: experience psychological and legal relief
 *   - messianic_restoration_narrative: the non-agent doctrinal anchor the suspension refers to
 *   - competing_reading_communities: excluded from this reading's own framework, hold sibling positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Suspended Sacrificial Obligation Pending Messianic Restoration").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/legal/theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '10c3aaa6-e1ff-4721-97e5-9942a4f2eee8').
narrative_ontology:cs_kernel_codification('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', fixed_text).
narrative_ontology:cs_authority_grounding('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', lineage).
narrative_ontology:cs_interpretation_layer_present('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8').
narrative_ontology:cs_reading_relation('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', foundational, obligation_status_deferred_not_discharged).
narrative_ontology:cs_axiom_status(obligation_status_deferred_not_discharged, holdable).
narrative_ontology:cs_axiom_grounding('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', obligation_status_deferred_not_discharged, conventional).
narrative_ontology:cs_axiom('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', secondary, study_neither_compliance_nor_preparation).
narrative_ontology:cs_axiom_status(study_neither_compliance_nor_preparation, holdable).
narrative_ontology:cs_axiom_grounding('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', study_neither_compliance_nor_preparation, conventional).
narrative_ontology:cs_reference_frame('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('10c3aaa6-e1ff-4721-97e5-9942a4f2eee8', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, observant_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, individual_worshippers).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, divine_covenant_continuity).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, temple_restoration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rule on the status of sacrificial obligations in the Temple's absence, holding that the commandment is neither annulled nor actively binding but suspended awaiting a future restoration. They administer the framework that defers adjudication rather than resolving it, and their communal authority rests partly on being custodians of this deferral.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities, agenda_setter,
    institutional, civilizational, analytical, global).

% Live under a legal system where a core commandment is formally still part of the law but practically unenforceable and unenforced. They are not required to perform sacrifice, not condemned for failing to, and not required to seek a substitute practice — the suspension relieves them of an impossible duty while preserving the commandment's formal standing.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_community, beneficiary,
    organized, generational, constrained, global).

% Experience the suspension as psychological and legal relief: they are not sinning by not sacrificing, and no ritual guilt attaches. Their exit options regarding the obligation itself are moot — there is nothing currently to comply with or resist.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, individual_worshippers, beneficiary,
    powerless, biographical, constrained, local).

% The doctrinal claim that a future restoration event will reactivate the obligation. It is not an actor; it is the conceptual anchor the suspension depends on and refers to for its own resolution condition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_narrative, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_narrative).

% Communities and scholars who hold that study of sacrificial law constitutes occupation of the obligation, or that study merely archives it for later use, are not represented within this reading's own framework — the suspension reading treats the obligation as genuinely dormant rather than as something currently discharged or merely preserved, which forecloses room for their account within a single legal position.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, competing_reading_communities, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal resolution for an obligation that cannot currently be performed (no Temple, no altar, no priestly service in the required form), avoiding both false claims of compliance and false accusations of violation across generations of adherents.
% TRANSFER_FUNCTION: Moves nothing materially — no goods, labor, or money are transferred under this reading. What is transferred is legal/psychological standing: adherents are relieved of guilt and of any substitute-performance requirement, and the deferral is displaced onto an undated future restoration event.
% ABSENT_VOICES: Adherents of the study-as-occupation reading, who hold that scholarly engagement itself discharges the obligation, are not party to this reading's framework — for them the suspension reading understates what study accomplishes. Adherents of study-as-archiving likewise are not represented; this reading treats study as neither compliance nor preparation, which they would dispute.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine vanished, halakhic authorities would need to resolve the underlying status question directly — declaring either an ongoing violation (creating a permanent low-grade guilt structure across the observant community) or an ongoing fulfillment substitute (opening the door to the occupation or archiving readings by default). Whether this counts as the world rearranging or staying the same is itself disputed within the tradition, since some communities already function as though one of the sibling readings is operative in practice.
% FOUNDING_PROBLEM: After the Second Temple's destruction, the sacrificial commandments became impossible to perform in their prescribed form, creating an acute legal and theological problem: is the community now in permanent violation of a divine commandment it cannot fulfill?
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish law and comparative religion scholars outside the halakhic tradition itself corroborate that the destruction of the Temple created a genuine unresolved legal problem requiring some doctrinal response; the suspension framework is one of several attested resolutions studied in the secondary literature, not merely a claim internal to the authorities who benefit from holding it.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.05, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.05) because under this reading there is no current transfer of goods, labor, or standing extracted from any party — the obligation is inert, not operative, so nothing is being taken from anyone in its name. Suppression is low (0.08): no one is coerced into compliance because there is nothing to comply with, though mild suppression exists in that the suspension framework forecloses easy adoption of the sibling readings within the same institutional voice. Theater ratio is modest (0.15) and rises slightly over the interval as commemorative practices (fast days, liturgical mentions) accumulate around the absent obligation without constituting its performance. Resistance is very low (0.1) — the doctrine is broadly accepted within its tradition and meets little internal challenge, though it stands opposite two live sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The observant community and individual worshippers are structural beneficiaries: the suspension relieves them of an impossible-to-fulfill duty and its attendant guilt, at essentially zero cost to any party. Halakhic authorities administer the framework and gain interpretive authority from being its custodians, but extract nothing material. There are no victims and no payer role under this reading, because nothing is currently owed and nothing is currently taken.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is the paradigm case of a mandate whose fulfillment condition has NOT arrived rather than one that has outlived its function — the founding problem (an unperformable divine commandment) remains live, so this is not mandatrophy. The suspension resolves the tension precisely by declining to either declare premature fulfillment or premature abandonment, which is why classification correctly reads low extraction rather than tangled_rope or piton: there is no ongoing extraction to layer onto a defunct coordination function, because the coordination function (managing an unperformable duty without generating guilt or false compliance) remains actively served.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_occupation_boundary,
    'Is the sacrificial obligation genuinely inert during the exile period, or does sustained scholarly engagement with its laws constitute a form of present occupation/fulfillment, as the sibling study_as_occupation reading holds?',
    'Comparative analysis of primary halakhic sources across the three readings'' proof-texts; examination of whether communities that emphasize intensive sacrificial-law study treat themselves as under a lesser or different legal status than communities holding strict suspension.',
    'If occupation is correct, this reading''s extractiveness and beneficiary structure remain similar but the classification of study activity shifts from theater-adjacent to substantive compliance, which would reduce the theater_ratio and change how the constraint interacts with the archiving reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_vs_occupation_boundary, conceptual, 'Whether suspension is truly total or partially occupied by scholarly practice.').

omega_variable(
    restoration_event_definiteness,
    'Is the messianic restoration event a definite, eventually-verifiable future occurrence, or an indefinitely deferred regulative ideal that structurally never resolves?',
    'Analysis of the doctrinal literature on messianic timing claims and how different authorities within the tradition treat the deferral''s determinacy; historical pattern of how the community has responded to messianic-claimant episodes.',
    'If indefinitely deferred, the suspension functions less like a temporary scaffold with a real sunset and more like a permanent doctrinal feature — this would not change the low extraction reading but would affect whether the constraint should ever be re-evaluated as scaffold-like rather than rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_event_definiteness, conceptual, 'Whether the restoration condition is a genuine future event or an open-ended deferral.').

omega_variable(
    authority_benefit_from_deferral,
    'Do halakhic authorities benefit from perpetuating the suspension framework itself (interpretive authority, communal cohesion) in a way that creates incentive against ever declaring the restoration condition met?',
    'Historical review of episodes where restoration or Temple-rebuilding was practically proposed and how authorities responded; examine whether institutional incentives shaped doctrinal timing.',
    'If authorities have a structural incentive to never resolve the suspension, the beneficiary declaration for halakhic_authorities would need to be weighted more heavily and this reading would sit closer to a tangled_rope than a rope, despite low material extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_benefit_from_deferral, empirical, 'Whether interpretive authorities have incentive to perpetuate rather than resolve the suspension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 400, 0.12).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 800, 0.13).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1200, 0.14).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1600, 0.15).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1950, 0.15).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 400, 0.04).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 800, 0.04).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1200, 0.05).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1950, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the temple_sacrifice_obligation kernel, decomposed per the epsilon-invariance principle because the three readings assign structurally different functions to study and different resolution logics to the obligation's status, which would otherwise force a single story to average across incompatible epsilon values. messianic_suspension treats the obligation as genuinely inert (very low epsilon, no victims); study_as_occupation treats scholarly study as itself constituting present fulfillment (different beneficiary/coordination structure); study_as_archiving treats study as preservation without fulfillment (an intermediate position). All three should be read together as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
