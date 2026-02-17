% ============================================================================
% CONSTRAINT STORY: blackstone_conflicts_of_interest
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_blackstone_conflicts_of_interest, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: blackstone_conflicts_of_interest
 * human_readable: Fiduciary Conflict Allocation in Blackstone S-1
 * domain: corporate_governance
 * * SUMMARY:
 * A structural legal constraint established in Blackstone's 2007 IPO S-1 filing
 * where the interests of the firm's investment funds (and their Limited Partners)
 * are explicitly and legally prioritized over the interests of new public common
 * unitholders. This creates a "fiduciary hierarchy" where the public investor
 * is contractually positioned at the bottom, with their traditional fiduciary
 * protections waived.
 * * KEY AGENTS:
 * - Public Common Unitholders: The subjects, who buy into a structure with limited fiduciary protection.
 * - Fund Limited Partners (LPs) & Senior Managing Directors: The beneficiaries, whose interests are legally prioritized.
 * - Legal/Financial Analysts: Analytical observers of the corporate structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(blackstone_conflicts_of_interest, 0.75). % High extraction of trust and legal recourse.
domain_priors:suppression_score(blackstone_conflicts_of_interest, 0.85).   % Partnership agreement contractually waives/limits traditional fiduciary duties.
domain_priors:theater_ratio(blackstone_conflicts_of_interest, 0.10).       % Low theater; the mechanism is explicitly functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, extractiveness, 0.75).
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary enforcement mechanism to resolve inherent conflicts.
narrative_ontology:constraint_claim(blackstone_conflicts_of_interest, tangled_rope).
narrative_ontology:human_readable(blackstone_conflicts_of_interest, "Fiduciary Conflict Allocation in Blackstone S-1").

% Binary flags
domain_priors:requires_active_enforcement(blackstone_conflicts_of_interest). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, fund_limited_partners).
narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, senior_managing_directors).
narrative_ontology:constraint_victim(blackstone_conflicts_of_interest, public_common_unitholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The public unitholder enters a structure where their interests are legally
% subordinated. The high extraction of fiduciary duty is felt as a trap.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For Fund LPs and the General Partner, this structure is pure coordination. It
% legally guarantees that the firm's primary loyalty remains with the fund
% investors, preventing dilution of focus or resources post-IPO.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, this is a Tangled Rope. It has a genuine coordination function
% (aligning the firm with its primary clients, the LPs) but achieves this via
% asymmetric extraction (stripping public unitholders of standard protections)
% and requires active enforcement through the partnership agreement.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_conflicts_of_interest_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the public unitholder and the fund LPs.
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify that the base extractiveness meets the threshold for a Snare/Tangled Rope.
    narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, extractiveness, E),
    E >= 0.46.

test(tangled_rope_structural_properties) :-
    % Verify that the necessary structural properties for a Tangled Rope are declared.
    ( narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, _) -> true ; !, fail),
    ( narrative_ontology:constraint_victim(blackstone_conflicts_of_interest, _) -> true ; !, fail),
    ( domain_priors:requires_active_enforcement(blackstone_conflicts_of_interest) -> true ; !, fail).

:- end_tests(blackstone_conflicts_of_interest_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a legal structure that is not deceptive but is highly
 * coercive and extractive for one class of stakeholders.
 * - Extractiveness (0.75): Represents the high value of the waived fiduciary duties. Public investors pay a price (in risk and lack of recourse) that benefits other parties.
 * - Suppression (0.85): The partnership agreement, governed by permissive Delaware law, effectively suppresses alternative legal challenges based on traditional fiduciary standards.
 * - Perspectival Gap: The gap is stark. For beneficiaries (LPs), it's a 'Rope' that secures their investment's priority. For subjects (public unitholders), it's a 'Snare' that contractually removes their power.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Classifying this as a 'Tangled Rope' from an analytical view prevents mandatrophy. A pure 'Snare' classification would miss the genuine, and crucial, coordination function the structure provides for the fund LPs. A pure 'Rope' classification would ignore the severe, asymmetric extraction imposed on public investors. The Tangled Rope correctly identifies it as a hybrid system where coordination for one group is funded by the extraction from another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_blackstone_good_faith,
    'To what extent does the implied covenant of good faith and fair dealing under Delaware law practically limit the General Partner''s ability to act against public unitholder interests?',
    'Analysis of Delaware Court of Chancery rulings on similar Master Limited Partnership agreements.',
    'If the covenant is a strong check, the structure is closer to a Rope. If it is a weak, procedural check, it remains a powerful Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(blackstone_conflicts_of_interest, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint was established at the IPO and has been structurally stable.
% The measurements reflect a consistently high-extraction, low-theater reality.
%
% Theater ratio over time:
narrative_ontology:measurement(bcoi_tr_t0, blackstone_conflicts_of_interest, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bcoi_tr_t5, blackstone_conflicts_of_interest, theater_ratio, 5, 0.10).
narrative_ontology:measurement(bcoi_tr_t10, blackstone_conflicts_of_interest, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(bcoi_ex_t0, blackstone_conflicts_of_interest, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(bcoi_ex_t5, blackstone_conflicts_of_interest, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(bcoi_ex_t10, blackstone_conflicts_of_interest, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a legal framework designed to enforce a specific hierarchy of duties.
narrative_ontology:coordination_type(blackstone_conflicts_of_interest, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */