% ============================================================================
% CONSTRAINT STORY: ia_digital_preservation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_ia_digital_preservation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ia_digital_preservation
 * human_readable: The Internet Archive Preservation-Copyright Conflict
 * domain: technological/legal
 * * SUMMARY:
 * The Internet Archive (IA) acts as a global memory bank, but its operations
 * are constrained by a high-stakes collision with copyright law. While it
 * coordinates knowledge access for the public, it faces predatory legal
 * extractions (lawsuits from major publishers) that threaten its centralized
 * existence and force the removal of content, victimizing both the archive and its users.
 * * KEY AGENTS:
 * - Researchers/Historians: Subject (Powerless, trapped by link rot and legal blocks)
 * - The Internet Archive: Beneficiary/Victim (Institutional, coordinates access but suffers extraction)
 * - Major Publishers: Beneficiary/Victim (Organized, enforce extraction but also benefit from the archive's existence as a historical record)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.58) due to the massive financial damages sought in lawsuits
% (e.g., Hachette v. IA) and the mandatory removal of 500,000+ digitized books.
domain_priors:base_extractiveness(ia_digital_preservation, 0.58). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(ia_digital_preservation, 0.72).   % Structural property (raw, unscaled). High due to legal restrictions and the technical difficulty of alternatives.
domain_priors:theater_ratio(ia_digital_preservation, 0.22).       % Piton detection (>= 0.70). Low; high functionality.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ia_digital_preservation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ia_digital_preservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ia_digital_preservation, theater_ratio, 0.22).

% Constraint self-claim (what does the constraint claim to be?)
% The IA frames its mission as a public good and coordination effort.
narrative_ontology:constraint_claim(ia_digital_preservation, tangled_rope).
narrative_ontology:human_readable(ia_digital_preservation, "The Internet Archive Preservation-Copyright Conflict").
narrative_ontology:topic_domain(ia_digital_preservation, "technological/legal").

% Binary flags
% The DWeb backup's role as a scaffold sunsets if the central IA's legal status is secured.
narrative_ontology:has_sunset_clause(ia_digital_preservation).
domain_priors:requires_active_enforcement(ia_digital_preservation). % Requires constant legal defense and crawling.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(ia_digital_preservation, researchers_and_public).
narrative_ontology:constraint_victim(ia_digital_preservation, internet_archive_itself).
narrative_ontology:constraint_victim(ia_digital_preservation, copyright_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE RESEARCHER/HISTORIAN (SNARE)
% For the user, the constraint is a trap where access to knowledge is
% precarious and subject to sudden removal by court order.
% χ = 0.58 * π(powerless=1.5) * σ(global=1.2) = 1.044. A powerful Snare.
constraint_indexing:constraint_classification(ia_digital_preservation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INTERNET ARCHIVE (ROPE)
% From the IA's internal perspective, it is a pure coordination mechanism for
% digital history, creating a shared cultural record.
% χ = 0.58 * π(institutional=-0.2) * σ(global=1.2) = -0.139. A clear Rope.
constraint_indexing:constraint_classification(ia_digital_preservation, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the system is a hybrid. It provides a genuine coordination
% function (beneficiaries exist) but also creates asymmetric extraction (victims exist)
% and requires active enforcement (legal battles). This is the canonical Tangled Rope signature.
% χ = 0.58 * π(analytical=1.15) * σ(global=1.2) = 0.80.
constraint_indexing:constraint_classification(ia_digital_preservation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE DWEB COMMUNITY (SCAFFOLD)
% The "Decentralized Web" backup effort acts as a temporary support structure
% (Scaffold) to ensure data survival if the central hub is legally severed.
% Its necessity is temporary, contingent on the central IA's survival.
constraint_indexing:constraint_classification(ia_digital_preservation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(ia_digital_preservation).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ia_preservation_tests).

test(perspectival_gap) :-
    % Verify the core gap: IA sees a Rope, users see a Snare.
    constraint_indexing:constraint_classification(ia_digital_preservation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ia_digital_preservation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ia_digital_preservation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that the conditions for a Tangled Rope classification are met.
    domain_priors:requires_active_enforcement(ia_digital_preservation),
    narrative_ontology:constraint_beneficiary(ia_digital_preservation, _), % Derives has_coordination_function
    narrative_ontology:constraint_victim(ia_digital_preservation, _).       % Derives has_asymmetric_extraction

test(scaffold_structural_properties) :-
    % Verify that the conditions for a Scaffold classification are met.
    narrative_ontology:has_sunset_clause(ia_digital_preservation),
    narrative_ontology:constraint_beneficiary(ia_digital_preservation, _). % Derives has_coordination_function

:- end_tests(ia_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint is a canonical Tangled Rope. The extraction score (0.58) reflects
 * the massive financial and content extractions imposed by the Hachette and Sony
 * lawsuits. The perspectival gap is acute: the IA's internal mission is pure
 * coordination (Rope), but for a powerless user, the threat of content removal
 * and legal blocks makes it a Snare. The analytical view, which requires a
 * coordination function, asymmetric extraction, and active enforcement, correctly
 * identifies it as a Tangled Rope. The Scaffold classification for the DWeb
 * backup is contingent on the interpretation that its role is temporary support,
 * which sunsets if the main IA becomes legally stable.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system prevents mislabeling digital preservation as pure extraction by
 * acknowledging the PetaBox architecture and WARC format as genuine technical
 * coordination functions. The Tangled Rope classification correctly captures the
 * hybrid nature of the system, where this coordination is coupled with severe
 * legal extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dweb_resilience,
    'Can decentralization (IPFS/Filecoin) successfully exit the legal Snare of centralized jurisdiction?',
    'Analysis of data availability for removed Hachette books on the DWeb network in 2027.',
    'Success = Conversion to Mountain (Immutable History); Failure = Hard Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
% Interval covers 30 years from IA's founding (1996) to the 2026 legal/technical reset.
narrative_ontology:interval(ia_digital_preservation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from a pure coordination
% project into a high-stakes legal battleground. This is required as base_extractiveness > 0.46.

% Theater ratio over time:
narrative_ontology:measurement(ia_tr_t0, ia_digital_preservation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ia_tr_t15, ia_digital_preservation, theater_ratio, 15, 0.15).
narrative_ontology:measurement(ia_tr_t30, ia_digital_preservation, theater_ratio, 30, 0.22).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(ia_ex_t0, ia_digital_preservation, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(ia_ex_t15, ia_digital_preservation, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(ia_ex_t30, ia_digital_preservation, base_extractiveness, 30, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The IA serves as a foundational piece of global digital infrastructure.
narrative_ontology:coordination_type(ia_digital_preservation, global_infrastructure).

% Network relationships: The legal precedent set here directly influences copyright law.
narrative_ontology:affects_constraint(ia_digital_preservation, copyright_term_extension).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */