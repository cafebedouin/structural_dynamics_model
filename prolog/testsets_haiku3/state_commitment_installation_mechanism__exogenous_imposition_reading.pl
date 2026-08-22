% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: State Authority Exogenous Commitment Installation Mechanism
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the exogenous imposition reading of the state
 *   commitment installation kernel: new commitments (law codes, professional
 *   standards, bureaucratic procedures, educational curricula, religious
 *   doctrine) gain legitimacy when a centralized state authority holding a
 *   transformation mandate decrees them top-down, backed by enforcement
 *   machinery (courts, inspectorates, schools), without prior grassroots
 *   consultation or local demonstration of superiority. The state appears as
 *   both beneficiary (gains the authority and capacity that installation
 *   proves) and beneficiary of extraction (consolidates control). The kernel
 *   itself is contested — two sibling readings claim endogenous climb
 *   (bottom-up validation) and hybrid cascade (apex decree + fringe
 *   validation) are the actual mechanisms. This story models exogenous
 *   imposition structurally and empirically, independent of whether it is the
 *   superior or even the dominant reading in practice. The claim and metrics
 *   are aligned: the constraint is claimed as tangled rope (genuine
 *   coordination function for the state, asymmetric extraction from
 *   grassroots) and the metrics describe active enforcement, moderately high
 *   extraction rising to a plateau at t=25 (the stabilization point after
 *   initial installation resistance subsides), and rising theater ratio (the
 *   enforcement apparatus increasingly performs legitimacy rather than
 *   enforces compliance as grassroots resistance hardens into learned
 *   withdrawal).
 *
 * KEY AGENTS:
 *   - state_authority_center: Holds transformation mandate and issues decrees; benefits from installation success via demonstrated capacity and bureaucratic expansion
 *   - institutional_periphery: Professional bodies and regional courts forced to adopt new commitments under threat of marginalization; bear retraining and delegitimation costs
 *   - grassroots_practitioners: Village authorities, customary judges, craft masters whose standing erodes as state-appointed enforcers replace local endorsement; identity-locked into professional roles that are undermined
 *   - state_enforcement_apparatus: Courts, schools, inspectorates gain authority and budget from administering the new commitment regime
 *   - alternative_commitment_advocates: Excluded from decree process; would advocate for negotiated adoption or bottom-up validation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.72).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State Authority Exogenous Commitment Installation Mechanism").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '5e829d18-34cb-4b3b-a584-071a0cafd062').
narrative_ontology:cs_kernel_codification('5e829d18-34cb-4b3b-a584-071a0cafd062', distributed).
narrative_ontology:cs_authority_grounding('5e829d18-34cb-4b3b-a584-071a0cafd062', extraction).
narrative_ontology:cs_interpretation_layer_present('5e829d18-34cb-4b3b-a584-071a0cafd062').
narrative_ontology:cs_reading_relation('5e829d18-34cb-4b3b-a584-071a0cafd062', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e829d18-34cb-4b3b-a584-071a0cafd062', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('5e829d18-34cb-4b3b-a584-071a0cafd062', foundational, centralized_authority_unilateral_mandate).
narrative_ontology:cs_axiom_status(centralized_authority_unilateral_mandate, holdable).
narrative_ontology:cs_axiom_grounding('5e829d18-34cb-4b3b-a584-071a0cafd062', centralized_authority_unilateral_mandate, deontological).
narrative_ontology:cs_axiom('5e829d18-34cb-4b3b-a584-071a0cafd062', foundational, enforcement_apparatus_legitimacy_through_installation_success).
narrative_ontology:cs_axiom_status(enforcement_apparatus_legitimacy_through_installation_success, holdable).
narrative_ontology:cs_axiom_grounding('5e829d18-34cb-4b3b-a584-071a0cafd062', enforcement_apparatus_legitimacy_through_installation_success, instrumental).
narrative_ontology:cs_reference_frame('5e829d18-34cb-4b3b-a584-071a0cafd062', centralized_state_authority_transformation_mandate).
narrative_ontology:cs_drift_state('5e829d18-34cb-4b3b-a584-071a0cafd062', contemporary_governance_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5e829d18-34cb-4b3b-a584-071a0cafd062', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority_center).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, centralization_agenda).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional_periphery).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_practitioners).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_enforcement_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the mandate to transform state institutions and cultural commitments (law codes, educational standards, bureaucratic norms, religious doctrine). Designs and decrees new commitments without prior constituency consultation. Deploys enforcement machinery (courts, inspectors, schools, regulators) to install them. Collects legitimacy from successful imposition — the ability to make commitments stick becomes proof of state capacity.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority_center, agenda_setter,
    institutional, generational, analytical, national).

% Professional bodies, regional courts, guilds, and intermediate institutions that previously operated under customary or mixed authority structures. Must adopt the new commitment or face marginalization. They bear the cost of retraining, restructuring, and abandoning prior legitimating narratives. Their regional standing erodes as enforcement shifts to state-appointed inspectors.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional_periphery, payer,
    moderate, biographical, constrained, regional).

% Village elders, craft masters, local healers, customary judges, and community authorities whose legitimacy rested on inherited knowledge and local endorsement. The new commitment is imposed via decree and school-based training, undermining their standing. They cannot exit without severing professional and social identity. Resistance is diffuse and easily suppressed.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Kinship structures, religious councils, and customary governance systems that held legitimacy before the new commitment. They face contradiction: some aspects (literacy standards, hygiene codes, legal precedent standardization) offer genuine coordination benefit to them once internalized, but installation via fiat rather than endorsement delegitimizes the adoption process itself. They are trapped between resistance (losing standing faster) and compliance (losing autonomy).
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authority, payer,
    moderate, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authority, beneficiary).

% Courts, inspectorates, schools, licensing bodies, and regulatory agencies that gain authority and budget from administering the new commitment. Their institutional power is constituted by the commitment's enforcement. They have a direct incentive to maintain and tighten the regime regardless of grassroots reception.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_enforcement_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Intellectuals, clergy, local authority holders, and practitioners who favor a different commitment (one that climbs from proven local success, or one that hybridizes state and local validation). They are excluded from the decree process and marginalized by enforcement of the competing state commitment. Their voice would argue for consultation, demonstration, and institutional respect.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, alternative_commitment_advocates, excluded,
    moderate, biographical, constrained, national).

% Historians, sociologists, and institutional analysts examining whether state-imposed commitments survive long-term, whether they require grassroots validation eventually, or whether they ossify as theater. They observe whether the exogenous imposition mechanism can sustain itself or whether the sibling readings (endogenous climb, hybrid cascade) capture the actual long-term pattern.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, comparative_state_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority_center).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes commitments (legal codes, professional standards, bureaucratic practices, educational curricula, religious doctrine) across a heterogeneous territory, enabling state-level resource allocation, tax collection, and security administration that depend on uniform procedures. Solves the state's coordination problem of territory-wide integration, not the grassroots coordination problem of local knowledge sharing.
% TRANSFER_FUNCTION: Moves authority and legitimacy from customary/regional/professional institutions to the state center and its enforcement apparatus. Grassroots and intermediate practitioners lose discretion and standing; state agencies gain budget and control. The extraction is not of material goods but of the right to decide what counts as legitimate knowledge and practice.
% ABSENT_VOICES: Practitioners who would advocate for bottom-up validation of the new commitment (demonstration that it works locally before decree), religious or customary leaders who would negotiate hybrid approaches, and the grassroots beneficiaries of the commitment's actual coordination function (who never get to say whether they wanted it this way) are excluded by the decree structure itself.
% DISAPPEARANCE_RATIONALE: If exogenous imposition mechanisms vanished — if the state lost its enforcement apparatus or retreated to endorsing only locally-proven commitments — institutional authority would rearrange around customary/professional/regional bases, decentralization would accelerate, and state-level uniform procedure would fragment. The constraint's removal unmakes the state's capacity for centralized transformation.
% FOUNDING_PROBLEM: Territory-wide governance under heterogeneous local law codes, professional standards, and customary practices made tax collection, conscription, legal procedure, and public health coordination impossible at scale. A centralized state needed uniform commitment structures to function.
% FOUNDING_PROBLEM_CORROBORATION: The state authority and comparative historians of state formation attest the founding problem is live — heterogeneity of local law and practice is an ongoing coordination impediment that centralized commitment installation solves. But practitioners and grassroots observers attest that the founding problem could be solved by negotiated standardization (the hybrid reading) or by endorsing commitments that proved themselves locally (the endogenous reading), not only by decree. The state's monopoly on legitimate answers to the founding problem is itself under contest.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 at t=0 (nascent decree, high uncertainty about enforceability) to 0.68 by t=15 (installation complete, enforcement normalized) and plateaus at 0.68 thereafter. The rise reflects the centralization dynamic: as the commitment spreads and becomes institutionalized, the state's extraction of decision-making authority solidifies. Suppression requirement peaks at 0.72 and holds there — initial installation met significant grassroots resistance (t=5-15) but the constraint's enforcement capacity hardened faster than resistance could organize. By t=25 the suppression requirement stabilizes: resistance persists but is isolated, atomized, and increasingly internalized (the identity_locked exit_options for grassroots_practitioners captures this — they have stopped fighting the constraint and started accepting it as unchangeable). Theater ratio rises steadily (0.22 to 0.41), indicating that an increasing share of enforcement activity is performative display of state legitimacy rather than coercive suppression of actual defiance. This is characteristic of a tangled rope after the extraction becomes entrenched: the enforcement apparatus must stage elaborate rituals (school ceremonies, legal formalities, bureaucratic procedures) to justify the regime's continuance, even as grassroots acceptance has curdled into resignation.
 *
 * PERSPECTIVAL GAP:
 *   From the state authority's seat, this constraint is a successful coordination mechanism: it solved the founding problem (heterogeneous local practices preventing centralized administration). From the grassroots_practitioners' seat, it is coercive extraction of legitimacy with a coordination rationale. From the institutional_periphery's seat, it is an institutional capture mechanism: they become administrators of someone else's vision. The engine should compute substantial per-seat divergence. The state seat will compute the constraint as rope or light tangled_rope (genuine coordination); the grassroots seat will compute snare (pure extraction hiding behind coordination language). This divergence is not an error — it is the measurement the system exists to capture: the same structural fact (new commitments decried top-down) produces different classifications depending on whether you benefit from or bear the cost of its installation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state authority, enforcement apparatus) are positioned at d near 0.0-0.25, receiving the benefits of consolidation without bearing costs. Victims (grassroots_practitioners, institutional_periphery, customary_authority) occupy d in the 0.6-0.9 range, bearing authority loss and enforcement costs. The identity_locked exit for grassroots_practitioners is structurally crucial: they cannot leave the profession without losing the identity that defines their social position, which means effective d approaches 1.0 (trapped target). The institutional_periphery has slightly better alternatives (regional autonomy frameworks, professional guild resistance) so d approaches 0.75-0.85 rather than 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (heterogeneous local practices prevent state-level administration) is genuinely live and the exogenous imposition mechanism does solve it. However, the mechanism's persistence depends on maintenance of the enforcement apparatus and willingness to suppress grassroots alternatives indefinitely. The mandatrophy question surfaces in the comparison with the sibling readings: if endogenous climb (bottom-up adoption after local success) or hybrid cascade (apex decree + fringe validation) prove more durable because they don't require constant enforcement, then exogenous imposition is solving the founding problem through a mechanism whose cost exceeds the benefit by the alternatives' measure. The theater_ratio trajectory (0.22 rising to 0.41 and holding) is a mandatrophy warning sign: as theater increases, it suggests the constraint is persisting through ceremonial maintenance rather than because the founding problem remains acutely urgent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_imposition_durability,
    'Can top-down decree-and-enforcement mechanisms sustain new commitments long-term without local validation eventually accumulating?',
    'Historical trajectory: if exogenously imposed commitments become self-maintaining (theater increases but enforcement costs don''t, suggesting internalization), exogenous imposition was sufficient. If grassroots resistance hardens into covert non-compliance, or if enforcement costs spiral indefinitely, exogenous imposition was necessary but not sufficient — local validation (endogenous climb or hybrid cascade) is required for durability.',
    'If exogenous imposition alone is sufficient, this reading''s claim holds: state authority + transformation mandate + enforcement is the legitimate mechanism. If local validation is ultimately required, the exogenous reading captures only phase 1, and the hybrid or endogenous readings are the more fundamental account of how commitments stabilize. Classification would drift from tangled_rope toward snare (pure extraction that eventually collapses) if durability requires hidden local validation the mechanism claims not to need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_imposition_durability, empirical, 'Whether exogenous imposition is a sufficient long-term mechanism or requires eventual grassroots validation.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the suppression decline from t=15-40 (plateau at 0.72) due to grassroots internalization (targets accept the regime''s legitimacy frame) or due to atomization and escape (targets give up collective resistance)?',
    'Post-removal observation: if the new commitment dissolves rapidly after enforcement ceases, suppression was external and never internalized. If the commitment persists after enforcement infrastructure collapses, suppression was internalized. If partial persistence emerges (some communities adopt, others revert), the mechanism was mixed.',
    'Pure internalization suggests the constraint is more durable and less extractive than suppression levels alone indicate — the targets have accepted the regime. Pure atomization/escape suggests the suppression reading is correct and targets remain coercively held. Mixed mechanisms suggest the constraint is a partial rope (some genuine adoption) and partial snare (some coerced compliance awaiting opportunity to revert).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether measured suppression decline reflects internalization or defeat of organized resistance.').

omega_variable(
    contested_kernel_committer_frame,
    'Is exogenous imposition the actual mechanism states use, or is it a rationalization after the fact for what was really a hybrid or endogenous process?',
    'Archival evidence: examining state records (decrees, enforcement reports, institutional correspondence) against grassroots records (adoption patterns, resistance movements, local-initiative emergence) to test whether top-down decree preceded or followed actual grassroots adoption.',
    'If archival evidence shows grassroots adoption preceding or forcing decree, the exogenous reading''s core premise is false and the constraint belongs to a different family (hybrid cascade, endogenous climb). If evidence shows decree clearly preceding adoption and enforcement installing compliance, exogenous imposition stands as the accurate mechanism. If evidence is mixed (some commitments exogenous, others endogenous), the kernel decomposes and multiple constraint stories are required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_committer_frame, conceptual, 'Whether exogenous imposition is the real causal mechanism or a post-hoc rationalization of hybrid/endogenous processes.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Is the state authority itself the beneficiary, or is ''centralization agenda'' (an abstract organizing principle) the beneficiary, and the state authority is merely its executor?',
    'Tracking whether the state authority resists or amplifies enforcement when the commitment''s local effects prove harmful or unpopular. Authority that adapts commitments to local feedback is the beneficiary. Authority that doubles down on enforcement despite local costs is a payer for centralization agenda''s benefit.',
    'If the state authority is the beneficiary, the constraint is tangled rope (state gains coordination + extraction). If centralization agenda is the beneficiary and the state is a captive executor, the constraint is snare (abstract principle extracts from both practitioners and state, using state apparatus as the mechanism). The classification hinges on whether the state is the seat that captures the constraint''s gains or merely the instrument through which extraction flows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, conceptual, 'Whether the state authority benefits from the constraint or is itself captured by a centralization imperative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(stat_tr_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(stat_be_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.14).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_commitment_installation_mechanism kernel. The kernel names the persisting question: what mechanism brings new state-level commitments into legitimacy and durability? The exogenous_imposition_reading asserts top-down decree by centralized authority is the mechanism. The endogenous_climb_reading asserts bottom-up adoption after local demonstration is the mechanism. The hybrid_cascade_reading asserts apex decree can work only if it cascades downward AND receives local validation. These are not measurements of the same constraint from different angles — they are rival claims about what the actual causal mechanism is. Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different type. They are linked because the empirical evidence on durability, fairness, and historical prevalence will favor one reading over the others; a corpus analysis will test which mechanism states actually deploy and whether it persists or fails. The network edge (affects_constraints) documents the reading family; the omegas document the committer structure and irreducible uncertainties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
