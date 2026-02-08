% ============================================================================
% CONSTRAINT STORY: blackstone_smd_control
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_blackstone_smd_control, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: blackstone_smd_control
 * human_readable: Blackstone Senior Managing Director Voting Control
 * domain: economic
 * * SUMMARY:
 * This corporate governance structure, established during Blackstone's 2007 IPO,
 * uses a multi-class unit structure to grant Senior Managing Directors (SMDs)
 * absolute voting control over the firm's general partner. This permanently
 * prevents public unitholders from electing directors or influencing management,
 * creating a structural asymmetry between founders and public investors.
 * * KEY AGENTS:
 * - Public Unitholders: Subjects (Powerless) who provide capital with no governance rights.
 * - Senior Managing Directors (SMDs): Beneficiaries (Institutional) who retain control.
 * - Corporate Governance Auditor: Observer (Analytical) who analyzes the structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(blackstone_smd_control, 0.80). % High power asymmetry; SMDs extract capital and fees while denying governance rights.
domain_priors:suppression_score(blackstone_smd_control, 0.90).   % Public unitholders have no legal mechanism (e.g., proxy contests) to override the general partner.
domain_priors:theater_ratio(blackstone_smd_control, 0.10).       % This is a highly functional, non-performative legal structure.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(blackstone_smd_control, extractiveness, 0.80).
narrative_ontology:constraint_metric(blackstone_smd_control, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(blackstone_smd_control, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as a necessary structure for long-term stability (coordination),
% but its function is pure enforcement of founder control.
narrative_ontology:constraint_claim(blackstone_smd_control, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(blackstone_smd_control). % Required for Tangled Rope (enforced via corporate charter and Delaware law).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(blackstone_smd_control, senior_managing_directors).
narrative_ontology:constraint_victim(blackstone_smd_control, public_unitholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PUBLIC UNITHOLDER (SNARE)
% For the investor, this is a trap. They are economically bound to the firm's
% performance but have zero strategic leverage or ability to correct mismanagement.
constraint_indexing:constraint_classification(blackstone_smd_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SENIOR MANAGING DIRECTORS (ROPE)
% For the SMDs, this structure is a coordination tool. It allows them to access
% public capital markets without ceding control or facing short-term pressures
% from activist investors, thus preserving their long-term vision.
constraint_indexing:constraint_classification(blackstone_smd_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The auditor sees both functions. It's a 'Rope' for the SMDs (coordination)
% tangled with a 'Snare' for unitholders (asymmetric extraction). It requires
% active legal enforcement and has clear beneficiaries and victims.
constraint_indexing:constraint_classification(blackstone_smd_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_smd_control_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(blackstone_smd_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_smd_control, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the gap as a Tangled Rope.
    constraint_indexing:constraint_classification(blackstone_smd_control, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(blackstone_smd_control, extractiveness, E),
    E >= 0.46.

:- end_tests(blackstone_smd_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE: [RESOLVED MANDATROPHY]
 * The base scores reflect a structure with extreme power asymmetry (E=0.80) and
 * inescapable rules (S=0.90). The perspectival gap is stark: public unitholders
 * experience a 'Snare' (investment without influence), while the controlling
 * SMDs see a 'Rope' (a tool to coordinate public listing while retaining control).
 *
 * The analytical classification must be 'Tangled Rope'. It is not a 'Mountain'
 * because it is a constructed legal artifice, not a natural law. It correctly
 * identifies the dual nature of the constraint: it has a genuine coordination
 * function for one group (the SMDs) that is inextricably linked to, and enables,
 * asymmetric extraction from another group (the unitholders). This resolves the
 * mandatrophy by acknowledging both perspectives within a single classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_blackstone_smd_control,
    'Will Delaware courts uphold the partnership''s right to contractually waive fiduciary duties owed to public unitholders, or will a legal limit emerge to protect minority investors?',
    'Monitor subsequent litigation in the Delaware Chancery Court (e.g., Dieckman v. Regency) regarding limited partnership agreements.',
    'If duties are fully waivable, the Snare aspect dominates. If courts impose limits, a weak Rope for investors emerges.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(blackstone_smd_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This structure was severe from its inception at the IPO. The drift reflects
% its increasing entrenchment and normalization over the decade following.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(blackstone_smd_control_tr_t0, blackstone_smd_control, theater_ratio, 0, 0.05).
narrative_ontology:measurement(blackstone_smd_control_tr_t5, blackstone_smd_control, theater_ratio, 5, 0.08).
narrative_ontology:measurement(blackstone_smd_control_tr_t10, blackstone_smd_control, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(blackstone_smd_control_ex_t0, blackstone_smd_control, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(blackstone_smd_control_ex_t5, blackstone_smd_control, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(blackstone_smd_control_ex_t10, blackstone_smd_control, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint's coordination function is the enforcement of the corporate charter.
narrative_ontology:coordination_type(blackstone_smd_control, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */