% ============================================================================
% CONSTRAINT STORY: INTERNET_ARCHIVE_PRESERVATION
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_ia_preservation, []).

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
    constraint_indexing:constraint_classification/3.

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
 * coordinates knowledge access, it faces predatory legal extractions (lawsuits) 
 * that threaten its centralized existence.
 * * KEY AGENTS:
 * - The Historian/User: Subject (Powerless/Trapped by link rot and legal blocks)
 * - The Archive (IA): Beneficiary (Institutional/Coordinating)
 * - Major Publishers/Labels: Auditor (Organized/Enforcing commercial extraction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.58) due to the $600M+ legal threats and mandatory 
% removal of 500,000+ digitized books.
domain_priors:base_extractiveness(ia_digital_preservation, 0.58). 
domain_priors:suppression_score(ia_digital_preservation, 0.72).   % Dynamic web complexity + Legal restrictions
domain_priors:theater_ratio(ia_digital_preservation, 0.22).       % High functionality; "church" setting is symbolic.

% Binary flags
domain_priors:requires_active_enforcement(ia_digital_preservation). % Requires constant legal defense and crawling.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RESEARCHER/HISTORIAN (SNARE)
% For the user, the constraint is a trap where access to out-of-print 
% works is extracted by court-ordered "negotiated judgments."
constraint_indexing:constraint_classification(ia_digital_preservation, snare, 
    context(agent_power(individual_powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE INTERNET ARCHIVE (ROPE)
% Viewed as a master coordination mechanism for digital history, 
% utilizing the WARC format to create a shared cultural record.
constraint_indexing:constraint_classification(ia_digital_preservation, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE COMMERCIAL PUBLISHER (TANGLED ROPE)
% Detects a hybrid signature: IA provides "fair use" coordination while 
% extracting potential ebook/streaming revenue from copyright holders.
constraint_indexing:constraint_classification(ia_digital_preservation, tangled_rope, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(ia_digital_preservation, E), E >= 0.46.

% PERSPECTIVE 4: THE DWEB/IPFS BACKUP (SCAFFOLD)
% The "Decentralized Web" acts as a temporary support structure 
% (Scaffold) to ensure data survival even if the central hub is legally severed.
constraint_indexing:constraint_classification(ia_digital_preservation, scaffold, 
    context(agent_power(analytical), 
            time_horizon(immediate), 
            exit_options(arbitrage), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ia_preservation_tests).

test(perspectival_gap) :-
    % IA sees the mission as a Rope, while the trapped user sees a legal Snare.
    constraint_indexing:constraint_classification(ia_digital_preservation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ia_digital_preservation, snare, context(agent_power(individual_powerless), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(ia_digital_preservation, E),
    E >= 0.46. % Validates high-extraction Tangled Rope status.

:- end_tests(ia_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.58) reflects the massive financial and content 
 * extractions imposed by the Hachette and Sony settlements. The perspectival 
 * gap is acute: IA coordinates the "Long Now" (Rope), while publishers see 
 * an "illegal record store" (Snare/Tangled Rope).
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system prevents mislabeling digital preservation as pure extraction by 
 * acknowledging the PetaBox architecture as a genuine technical coordination 
 * (Rope) for low-cost storage (PUE efficiency).
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

% Required for external script parsing
% Covers the period from IA's founding to the 2025 legal/technical reset.
narrative_ontology:interval(ia_digital_preservation, 1996, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
