% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Syncretism as Incoherent State-Enforced Bundle
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   From 1600 onwards, the Tokugawa state enforced a nominally unified
 *   religious framework bridging Shinto and Buddhism through the honji
 *   suijaku ('original essence, trace manifestation') doctrine. This doctrine
 *   claims that buddhas are the original metaphysical reality and kami are
 *   their temporary worldly manifestations. This reading argues that no
 *   coherent ontological kernel exists — syncretism is accumulated
 *   institutional drift under state enforcement, not a unified philosophical
 *   commitment. The Tokugawa state benefits by maintaining both
 *   constituencies under unified administrative control; practitioners bear
 *   the burden of participating in two incompatible frameworks without
 *   resolution. Enforcement is active: shrine licensing, temple registration,
 *   canonical interpretation, and suppression of alternative theologies
 *   (Christianity, revival Shinto) all maintain the incoherent bundle. The
 *   theater ratio rises over the interval (0.25 to 0.58) as the constraint's
 *   functional problem-solving capacity diminishes and maintenance becomes
 *   increasingly performative.
 *
 * KEY AGENTS:
 *   - Tokugawa state: agenda-setter and primary beneficiary; enforces the syncretistic framework through administrative law and licensing control.
 *   - Shinto practitioners: payers with identity-lock; required to accept buddha subordination; theological autonomy suppressed.
 *   - Buddhist monks: institutional beneficiaries; gain resources and authority; manage the conceptual contradiction.
 *   - Folk religionists: trapped payers; operate at local level but nominally subject to state doctrine; bear cognitive load of incoherence.
 *   - Intellectual elite: observers and secondary beneficiaries; serve as canonical interpreters; benefit from status but dependent on incoherence persisting.
 *   - Alternative Christian converts: excluded; persecuted; represent suppressed alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Syncretism as Incoherent State-Enforced Bundle").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e').
narrative_ontology:cs_kernel_codification('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', distributed).
narrative_ontology:cs_authority_grounding('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', extraction).
narrative_ontology:cs_interpretation_layer_present('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e').
narrative_ontology:cs_reading_relation('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', foundational, no_coherent_kernel_exists).
narrative_ontology:cs_axiom_status(no_coherent_kernel_exists, holdable).
narrative_ontology:cs_axiom_grounding('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', no_coherent_kernel_exists, empirically_contingent).
narrative_ontology:cs_axiom('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', foundational, syncretism_as_state_enforcement_not_philosophical_commitment).
narrative_ontology:cs_axiom_status(syncretism_as_state_enforcement_not_philosophical_commitment, holdable).
narrative_ontology:cs_axiom_grounding('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', syncretism_as_state_enforcement_not_philosophical_commitment, instrumental).
narrative_ontology:cs_reference_frame('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', autonomous_religious_traditions_before_state_fusion).
narrative_ontology:cs_drift_state('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', contemporary_meiji_period_end, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc9c0d61-a5fa-4aea-ae83-67b7ccc91a7e', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_state).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, folk_religionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, intellectual_elite).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional_unity_through_ontological_incoherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the fusion of kami and buddha worship through administrative law, temple-shrine networks, and canonical texts. Benefits from the unified religious structure as a tool for centralizing authority: a coherent ontology would require choosing between traditions, fracturing support; incoherence allows both constituencies to nominally submit while remaining in structural tension. Maintains enforcement by controlling ordination, temple licensing, land allocation, and canonical interpretation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_state, agenda_setter,
    institutional, generational, analytical, national).

% Required to accept buddha-statue presence in kami shrines and honji suijaku reinterpretation of kami as buddha emanations. Their indigenous kami theology — where kami are autonomous, this-worldly, and ontologically distinct — is suppressed in favor of a framework that nominally preserves kami worship while subordinating it to Buddhist metaphysical hierarchy. Exit is constrained by family tradition, land tenure (shrine maintenance), and the state's legal framework; identity is locked into shrine stewardship roles across generations.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_practitioners, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_practitioners, beneficiary).

% Gain institutional resources and state patronage through the fusion framework, which allows them to interpret kami as lower-order manifestations of buddhas and thereby incorporate kami worship into the Buddhist cosmology. They manage shrine-temple complexes (jinjabutsuji) and collect offerings from both traditions. However, they must maintain the pretense that both traditions are equally honored, constrain their theological dominance claims in public discourse, and manage the contradiction between Buddhism's theoretical primacy and Shinto's political inviolability.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks, payer).

% Practice household and village kami veneration, agricultural rites, and spirit appeasement at a granular level where state doctrine has minimal reach. They are nominally subordinate to the state-enforced syncretism but operate in a space where enforcement is diffuse. They bear the cognitive burden of participating in two incompatible ritual systems without ontological resolution: they honor kami as autonomous presences while also accepting buddha-realm teachings that would subordinate those kami. Their exit options are trapped — renouncing either tradition is socially and economically catastrophic.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, folk_religionists, payer,
    powerless, biographical, trapped, local).

% Serves as canonical interpreters and theological arbiters of the syncretistic doctrine. They benefit from the status of being keepers of a sophisticated philosophical synthesis (honji suijaku is intellectually elaborate) and have access to patronage from both state and institutional religion. They occupy an observer seat because they are not the primary extractors, but they benefit from the arrangement's durability and their stake in preserving the incoherence (intellectual coherence would require resolution, ending their role as reconcilers).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, intellectual_elite, observer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, intellectual_elite, beneficiary).

% Would reject the entire framework as incompatible with monotheistic Christianity. They are excluded from the conversation by state enforcement; Christianity is periodically persecuted and forced underground, making the syncretistic framework a hegemonic constraint against which alternative religious commitment is impossible without severe legal and social penalty. Their absence from the negotiating table is the enforcement machinery itself.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, alternative_christian_converts, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_state).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally solves the institutional problem of managing two historically distinct religious traditions (Shinto and Buddhism) under unified state control. The honji suijaku framework presents itself as a coherent ontology that honors both, allowing the state to appear neutral while enforcing religious unity.
% TRANSFER_FUNCTION: Transfers authority from decentralized, autonomous kami shrines and independent Buddhist monasteries to state-controlled temple-shrine complexes where the state dictates doctrine, collects offerings, and licenses clergy. Moves intellectual legitimacy from folk practitioners and local shrine keepers to the educated elite who interpret canonical texts. Transfers doctrinal autonomy from Shinto and Buddhist communities to the state apparatus managing the fusion.
% ABSENT_VOICES: Christian converts, independent Shinto revivalists, and Buddhist reform movements that reject syncretism are excluded by law and administrative apparatus. They would argue for theological clarity and institutional autonomy but are prevented from participating in the conversation. Their absence is enforced, not accidental.
% DISAPPEARANCE_RATIONALE: If the syncretistic framework vanished, practitioners would face a choice: revert to autonomous Shinto theology and practice, embrace Buddhism with doctrinal clarity, or develop hybrid frameworks of their own design. The state would lose a primary tool for unified religious control. Local shrine authority would reconstitute around indigenous kami theology; monastery networks would restructure around Buddhist doctrine. The unified religious apparatus the state built would fragment immediately.
% FOUNDING_PROBLEM: Early Tokugawa period: competing power bases in independent temples and shrines threatened state consolidation; Buddhist institutional wealth rivaled state authority; Shinto practitioners maintained autonomous, locally-grounded religious authority outside state channels. The state needed a framework to subordinate both traditions without destroying either (both had deep social roots and abandoning either would fracture social legitimacy).
% FOUNDING_PROBLEM_CORROBORATION: Tokugawa state documents (edicts on shrine licensing, temple registrations) attest the founding problem and claim syncretism solved it by unifying under state authority. Modern historians of Japanese religion, scholars outside the benefiting parties, and practitioners themselves attest that by the Meiji period, syncretism had become institutional inertia rather than active problem-solving — the founding problem (fragmented authority) was solved; syncretism persisted not because it solved anything but because it had become bureaucratically embedded and the intellectual elite depended on interpreting it.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the state transfers authority from autonomous traditions to centralized control, collects offerings through state-licensed clergy, and extracts intellectual labor from interpreters maintaining the fiction of coherence. Suppression (0.71) is measured through active enforcement: ordination control, shrine licensing, canonical constraint, and persecution of alternatives. Theater ratio (0.58) rises over the interval because early in the Tokugawa period, syncretism served genuine coordination (unifying fractious power bases); by the Meiji period, it persists as inertia — the founding problem is solved, but the arrangement is maintained theatrically through bureaucratic maintenance and ritualized reaffirmation. Accessibility collapse (0.64) reflects the state's control of ordination and licensing, which removes alternatives for practitioners who want institutional legitimacy; however, folk practice operates in an underground gray space where alternatives partly persist. Resistance (0.52) shows substantial practitioner resistance: Shinto revivalists (18th century nativism) contest the framework from within, Christian converts resist from without, and folk practitioners navigate around it. The measurement series uses a shared time grid across all three metrics, with projections for the pre-1660 founding period and observed data thereafter.
 *
 * PERSPECTIVAL GAP:
 *   The state seat and the practitioner seats experience fundamentally different constraints. The state sees a unified religious apparatus it controls; practitioners see two irreconcilable traditions they must honor simultaneously. The Buddhist institutional seat benefits while also bearing the cost of intellectual contradiction (maintaining the synthesis as doctrine). Folk practitioners bear the burden of practical navigation without institutional resources. The gap is structural and irreconcilable — it is exactly the snare dynamic: the beneficiary (state) has engineered the incoherence, while the payers (practitioners) bear the cognitive and institutional costs of living with it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Tokugawa state sits at the agenda-setter position with high power and analytical exit (it defines the rules); its directionality is near-zero (full beneficiary: it collects authority and resources). Shinto practitioners and folk religionists are trapped or identity-locked targets; their directionality is high (1.0 or near-1.0 target end). Buddhist monks benefit institutionally but are constrained by having to maintain the pretense of non-dominance; their directionality is ambiguous (symmetric-to-moderate-beneficiary). Intellectual elite are observers with mobile exit; they benefit from the arrangement but are not bound by it the way practitioners are. The per-seat classifications should diverge: from the state's seat, this is a coordination mechanism (unified religious authority); from the trapped practitioner seat, this is extraction (forced incoherent participation). The engine computes this from the structural data authoring does not reconcile the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented religious authority threatening state consolidation) was real in 1600 and live until ~1720. By 1780, the founding problem was solved — syncretism had successfully centralized authority, tamed competing institutions, and achieved unified control. However, the constraint persists from 1780–1868 not because it solves the founding problem but because dismantling it would require administrative reorganization and would displease entrenched interests (state clergy, intellectual elite, Buddhist institutions). The rising theater_ratio (0.25→0.58) documents this: enforcement shifts from problem-solving to maintenance-of-status-quo. The constraint exhibits mandatrophy — the mandate has died, but the arrangement persists through institutional inertia. This is why the measured extractiveness plateaus after 1780 (0.67→0.68): extractiveness rises with the founding problem's solution (more people are integrated into the system and constrained by it), then stagnates because the system is maintained, not because it solves anything. A piton classification (inertial maintenance) is tempting here, but the presence of active beneficiaries (state, Buddhist institutions, intellectual elite) and the persistence of suppression machinery (not just theatrical but enforced) argues for snare rather than piton — the arrangement is maintained because parties benefit from extraction, not merely from institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_incoherence_vs_practical_functioning,
    'Is the measured incoherence structural (no resolution possible within state apparatus) or performative (practitioners navigate it pragmatically without resolution being necessary)?',
    'Ethnographic study of practitioner cognition and lived experience across social strata (elite interpreters vs. folk practitioners vs. monks): do they experience syncretism as paradox requiring resolution, or as a workable multi-framework arrangement?',
    'If structural incoherence is real and unresolvable, the constraint is a snare imposing cognitive burden without functional benefit; if pragmatically navigable, it becomes a tangled rope with real (if messy) coordination function. The measured extraction would remain the same; the classification would shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_incoherence_vs_practical_functioning, empirical, 'Whether incoherence is lived as paradox or managed as context.').

omega_variable(
    state_intentionality_vs_institutional_accident,
    'Did the state deliberately construct incoherence as a control mechanism, or did it accumulate accidentally through institutional layering and then become entrenched?',
    'Historical analysis of Tokugawa edicts, policy documents, and administrative correspondence: explicit statements about the syncretistic framework''s intended function vs. evidence of reactive administrative patch-work.',
    'Deliberate construction would confirm this reading''s snare classification (state as conscious architect of extraction). Accidental accumulation would suggest a piton classification (incoherence persists by institutional inertia, not by design). The beneficiary structure remains the same; the organizing principle changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intentionality_vs_institutional_accident, empirical, 'Whether syncretism is engineered extraction or accumulated administrative drift.').

omega_variable(
    committer_frame_alternative,
    'This reading instantiates ONE interpretation of the shinbutsu kernel: that no coherent kernel exists and syncretism is state-enforced incoherence. The sibling readings (''syncretic_fusion_reading'' and ''domain_partition_reading'') claim there IS a coherent kernel (either metaphysical unity or functional separation). Can all three readings coexist, or must they be mutually exclusive within a single analytical framework?',
    'Philosophical analysis: does ''incoherence'' as a reading''s core claim logically preclude the other readings'' core claims? Or do they describe the constraint from different interpretive seats (committer-frame analysis: what the kernel IS vs. what institutional actors do with it)?',
    'If they are logically exclusive, the reading relations should use ''forecloses''. If they describe the same kernel from different seats (what it is vs. what it does), they ''coexist_with''. This determines whether the committer framework supports genuine triadic contest or decomposition into parallel stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_alternative, conceptual, 'Whether the incoherence reading forecloses the fusion and partition readings or coexists with them in committer contest.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (enforcement machinery, legal penalties, administrative control) or internalized (practitioners accept contradictory beliefs as normal, identity-locked into the framework)?',
    'Post-constraint observation: Meiji elimination of syncretism and analysis of how quickly practitioners reorganized around autonomous traditions. If reorganization was rapid, suppression was primarily structural; if practitioners struggled to leave the syncretistic framework even after legal removal, suppression was partly internalized.',
    'If primarily internalized, the constraint''s effective suppression persists beyond formal removal — practitioners carry the framework with them. If structural, removal would quickly dissolve the arrangement. This affects post-constraint behavioral prediction and the interpretation of ''identity_locked'' exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement_basis(shin_tr_t1600, projected).
narrative_ontology:measurement(shin_tr_t1660, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1660, 0.35).
narrative_ontology:measurement_basis(shin_tr_t1660, observed).
narrative_ontology:measurement(shin_tr_t1720, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1720, 0.45).
narrative_ontology:measurement_basis(shin_tr_t1720, observed).
narrative_ontology:measurement(shin_tr_t1780, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1780, 0.54).
narrative_ontology:measurement_basis(shin_tr_t1780, observed).
narrative_ontology:measurement(shin_tr_t1830, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1830, 0.58).
narrative_ontology:measurement_basis(shin_tr_t1830, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1868, 0.58).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement_basis(shin_be_t1600, projected).
narrative_ontology:measurement(shin_be_t1660, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1660, 0.51).
narrative_ontology:measurement_basis(shin_be_t1660, observed).
narrative_ontology:measurement(shin_be_t1720, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1720, 0.62).
narrative_ontology:measurement_basis(shin_be_t1720, observed).
narrative_ontology:measurement(shin_be_t1780, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1780, 0.67).
narrative_ontology:measurement_basis(shin_be_t1780, observed).
narrative_ontology:measurement(shin_be_t1830, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1830, 0.68).
narrative_ontology:measurement_basis(shin_be_t1830, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1868, 0.68).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1600, 0.48).
narrative_ontology:measurement_basis(shin_su_t1600, projected).
narrative_ontology:measurement(shin_su_t1660, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1660, 0.58).
narrative_ontology:measurement_basis(shin_su_t1660, observed).
narrative_ontology:measurement(shin_su_t1720, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1720, 0.64).
narrative_ontology:measurement_basis(shin_su_t1720, observed).
narrative_ontology:measurement(shin_su_t1780, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1780, 0.7).
narrative_ontology:measurement_basis(shin_su_t1780, observed).
narrative_ontology:measurement(shin_su_t1830, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1830, 0.71).
narrative_ontology:measurement_basis(shin_su_t1830, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1868, 0.71).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).

% DUAL FORMULATION NOTE:
% Three readings of the shinbutsu kernel compete: syncretic_fusion_reading (kami and buddhas are ontologically unified), domain_partition_reading (kami and buddhas govern separate domains), and incoherent_bundle_reading (no coherent kernel exists; syncretism is state-enforced incoherence). All three are linked as constraint family members. This reading claims no coherent ontological foundation exists — the other readings mistake institutional structure for metaphysical truth. The readings share the same raw facts (honji suijaku doctrine, state enforcement, practitioner participation) but interpret them differently. This reading emphasizes the beneficiary structure (state extraction) and the incoherent burden on practitioners; it is classified as snare under this interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
