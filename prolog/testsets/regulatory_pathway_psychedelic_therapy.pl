% ============================================================================
% CONSTRAINT STORY: regulatory_pathway_psychedelic_therapy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_regulatory_pathway_psychedelic_therapy, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_pathway_psychedelic_therapy
 *   human_readable: The Regulatory and Clinical Pathway for Novel Psychedelic Therapies
 *   domain: technological/political
 *
 * SUMMARY:
 *   This constraint represents the complex, high-cost, and high-suppression
 *   regulatory framework (e.g., FDA clinical trials) required to bring novel
 *   psychedelic compounds like DMT to market for treating conditions like
-  *   depression. While it serves a genuine coordination function—ensuring
+  *   depression. While it serves a genuine coordination function—ensuring
 *   public safety and therapeutic efficacy—it also creates enormous barriers
 *   to entry, which asymmetrically benefit well-capitalized pharmaceutical
 *   firms and extract significant value (in time, access, and cost) from
 *   patients.
 *
 * KEY AGENTS (by structural relationship):
 *   - Patients with treatment-resistant depression: Primary target (powerless/trapped) — bears the cost of delayed access, high prices, and limited options.
 *   - Psychedelic pharmaceutical developers: Primary beneficiary (institutional/arbitrage) — navigates the pathway to secure patent-protected market exclusivity, benefiting from the high barriers to entry.
 *   - Government Regulators (e.g., FDA): Inter-institutional actor (institutional/constrained) — enforces the pathway to ensure public safety, but is constrained by political mandates and historical drug policy.
 *   - Analytical Observer: Analytical psychopharmacologist — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_pathway_psychedelic_therapy, 0.48).
domain_priors:suppression_score(regulatory_pathway_psychedelic_therapy, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(regulatory_pathway_psychedelic_therapy, 0.35).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, extractiveness, 0.48).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, theater_ratio, 0.35).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(regulatory_pathway_psychedelic_therapy, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(regulatory_pathway_psychedelic_therapy). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, psychedelic_pharma_developers).
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, public_safety_mandate). % The coordination function
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, patients_with_depression).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PATIENT WITH DEPRESSION (PRIMARY TARGET)
% For a patient trapped by their condition with few effective alternatives,
% the regulatory pathway is an opaque, slow, and costly barrier. The
% coordination function (long-term public safety) is an abstraction compared
% to their immediate suffering. High ε combined with their derived d≈0.95 and
% national scope σ=1.0 results in χ > 0.66, a clear Snare.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PHARMACEUTICAL DEVELOPER (PRIMARY BENEFICIARY)
% For the company with the capital to navigate the trials, the high
% suppression and cost are a feature, not a bug. It creates a powerful
% competitive moat, ensuring that if they succeed, they will have a highly
% profitable, patent-protected market. For them, it is a pure coordination
% mechanism (Rope). Their derived d≈0.05 and global scope σ=1.2 results in a
% negative χ, indicating a subsidy.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both sides: the genuine, necessary coordination function
% (public safety) and the severe, asymmetric extraction imposed on patients
% and the healthcare system. The combination of high suppression, high base
% extraction, and the presence of both beneficiaries and victims makes this a
% canonical Tangled Rope.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE GOVERNMENT REGULATOR (INTER-INSTITUTIONAL)
% The regulator (e.g., FDA) is an institutional actor tasked with enforcing
% this system. Their exit is 'constrained' by their public mandate and
% political reality. They see the system as a coordination Rope, designed to
% balance innovation with safety. Their directionality is more neutral than
% the developer's but still sees the system as functional. We use an override
% to capture this specific structural position.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_pathway_psychedelic_therapy_tests).

test(perspectival_gap) :-
    % Verify the gap between the patient (Snare) and the developer (Rope).
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_classification_is_tangled_rope) :-
    % The analytical perspective must identify the hybrid nature.
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, _),
    narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, _),
    domain_priors:requires_active_enforcement(regulatory_pathway_psychedelic_therapy).

:- end_tests(regulatory_pathway_psychedelic_therapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): The enormous cost of multi-phase clinical
 *     trials, legal compliance, and specialized infrastructure represents a
 *     significant inherent cost, much of which is passed on.
 *   - Suppression (S=0.80): High due to the Schedule I legal status of the
 *     compounds, stringent DEA and FDA oversight, and the cultural/political
 *     stigma that restricts research and funding.
 *   - Theater Ratio (T=0.35): While much of the process is functional (e.g.,
 *     safety trials), a significant portion is arguably theatrical, stemming
 *     from outdated "war on drugs" policies rather than modern risk assessment.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Patients experience a Snare: a high-cost, low-access
 *   system that extracts their time, hope, and money. Pharmaceutical
 *   developers, however, see a Rope: the high barriers to entry, once
 *   surmounted, coordinate the market in their favor, creating a lucrative
 *   monopoly. The "cost" to them is an investment in a competitive moat.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'psychedelic_pharma_developers' who gain market
 *     exclusivity, and the abstract 'public_safety_mandate' which represents
 *     the system's coordination function.
 *   - Victim: 'patients_with_depression' who bear the costs of delayed
 *     innovation and access. This clear beneficiary/victim structure, combined
 *     with their different exit options (arbitrage vs. trapped), is what
 *     drives the perspectival gap via the directionality function f(d).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the pharmaceutical developer and the
 *   government regulator. Both are 'institutional', but their relationship to
 *   the constraint differs. The developer has `exit_options(arbitrage)`—they
 *   can invest their capital elsewhere. The regulator has
 *   `exit_options(constrained)`—they cannot simply abandon their public safety
 *   mandate. The directionality override for the regulator (d=0.35) reflects
 *   this more neutral, constrained position compared to the pure beneficiary
 *   (d≈0.05).
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of a Tangled Rope, preventing misclassification.
 *   A simplistic analysis might label the system a Snare (focusing only on
 *   patient outcomes) or a Rope (focusing only on the stated goal of public
 *   safety). The Deferential Realism framework, by indexing to different
 *   agents, reveals its dual nature: a system with a genuine coordination
 *   function that has been co-opted to produce massive asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_regulatory_pathway_psychedelic_therapy,
    'Is the high suppression and cost (ε=0.48, S=0.80) a necessary byproduct of ensuring public safety, or is it a form of institutional inertia and regulatory capture that primarily serves to limit competition?',
    'Comparative analysis of regulatory pathways for other high-risk, high-reward therapeutics, and declassified internal memos from regulatory bodies.',
    'If necessary, the system is a tragic but functional Tangled Rope. If inflated, it is a Snare masquerading as a Tangled Rope, with the coordination function serving as theatrical justification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_pathway_psychedelic_therapy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time. Initially (post-1970), it was
% almost pure suppression (high theater). As research became viable, the
% economic extraction (cost of trials) grew, while the purely political
% theater slightly receded, replaced by a more functional (but still
% extractive) process.

% Theater ratio: high initially due to political motivations, now lower but still present.
narrative_ontology:measurement(rpst_tr_t0, regulatory_pathway_psychedelic_therapy, theater_ratio, 0, 0.60).
narrative_ontology:measurement(rpst_tr_t5, regulatory_pathway_psychedelic_therapy, theater_ratio, 5, 0.45).
narrative_ontology:measurement(rpst_tr_t10, regulatory_pathway_psychedelic_therapy, theater_ratio, 10, 0.35).

% Extraction: low initially (as there was no path), now high due to the cost of modern trials.
narrative_ontology:measurement(rpst_ex_t0, regulatory_pathway_psychedelic_therapy, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rpst_ex_t5, regulatory_pathway_psychedelic_therapy, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(rpst_ex_t10, regulatory_pathway_psychedelic_therapy, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The pathway is a mechanism for enforcing standards.
narrative_ontology:coordination_type(regulatory_pathway_psychedelic_therapy, enforcement_mechanism).

% Network relationships: This pathway's difficulty is structurally influenced
% by the legal classification of the substances involved.
narrative_ontology:affects_constraint(schedule_one_classification_dmt, regulatory_pathway_psychedelic_therapy).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of constraints governing psychedelic
% therapy. The upstream constraint, 'schedule_one_classification_dmt', creates
% the high-suppression environment in which this one operates.
% Related stories:
%   - schedule_one_classification_dmt (ε≈0.55, Piton/Snare)

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% We override the regulator's directionality. The automatic derivation, based
% on 'beneficiary' status and 'constrained' exit, would produce a d≈0.25-0.30.
% We set it to d=0.35 to reflect a position that is more aligned with the system's
% costs and political pressures than a pure beneficiary, but still views the
% system as fundamentally coordinating.
constraint_indexing:directionality_override(regulatory_pathway_psychedelic_therapy, institutional, 0.35).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */