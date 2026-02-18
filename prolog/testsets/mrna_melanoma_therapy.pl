% ============================================================================
% CONSTRAINT STORY: mrna_melanoma_therapy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_mrna_melanoma_therapy, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mrna_melanoma_therapy
 *   human_readable: "Personalized mRNA Cancer Vaccine Protocol (mRNA-4157/V940)"
 *   domain: technological / economic / healthcare
 *
 * SUMMARY:
 *   This constraint represents the socio-technical and economic structure
 *   governing access to a new, highly effective personalized mRNA vaccine for
 *   high-risk melanoma. The therapy combines genetic sequencing of a patient's
 *   tumor, custom synthesis of an mRNA vaccine targeting its specific mutations,
 *   and administration alongside immunotherapy. While a landmark medical
 *   achievement, its implementation is constrained by high costs, complex logistics,
 *   and a proprietary intellectual property regime.
 *
 * KEY AGENTS (by structural relationship):
 *   - Uninsured/Underinsured Patients: Primary target (powerless/trapped) — bear the cost of exclusion from a life-saving therapy.
 *   - Pharmaceutical Developers (e.g., Moderna/Merck): Primary beneficiary (institutional/arbitrage) — benefit from profits and market position secured by patents.
 *   - Healthcare Payers (Insurers/National Health Systems): Inter-institutional actor (institutional/constrained) — bear the direct financial cost, forcing difficult resource allocation choices.
 *   - High-Risk Melanoma Patients (with access): Secondary beneficiary (powerless/constrained) — benefit from improved survival odds but are constrained by the treatment protocol.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mrna_melanoma_therapy, 0.55). % High extraction due to patent monopoly and novel tech pricing.
domain_priors:suppression_score(mrna_melanoma_therapy, 0.75).   % Structural property (raw, unscaled). Patent law actively suppresses alternatives.
domain_priors:theater_ratio(mrna_melanoma_therapy, 0.10).       % Piton detection (>= 0.70). Currently low; the therapy is genuinely effective.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mrna_melanoma_therapy, extractiveness, 0.55).
narrative_ontology:constraint_metric(mrna_melanoma_therapy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(mrna_melanoma_therapy, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(mrna_melanoma_therapy, tangled_rope).
narrative_ontology:human_readable(mrna_melanoma_therapy, "Personalized mRNA Cancer Vaccine Protocol (mRNA-4157/V940)").
narrative_ontology:topic_domain(mrna_melanoma_therapy, "technological / economic / healthcare").

% --- Binary flags ---
domain_priors:requires_active_enforcement(mrna_melanoma_therapy). % Required for Tangled Rope. Enforcement is via intellectual property law.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(mrna_melanoma_therapy, pharmaceutical_developers).
narrative_ontology:constraint_beneficiary(mrna_melanoma_therapy, high_risk_melanoma_patients). % They benefit from the *outcome*, a key part of the coordination function.

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(mrna_melanoma_therapy, uninsured_patients). % Cost of exclusion.
narrative_ontology:constraint_victim(mrna_melanoma_therapy, healthcare_payers). % Direct financial cost.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE UNINSURED PATIENT (SNARE)
% Sees a life-saving treatment that is inaccessible due to its price structure.
% The existence of the cure becomes a source of extraction (desperation, debt).
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PHARMACEUTICAL DEVELOPER (ROPE)
% Views the system as a successful coordination of R&D, IP law, and manufacturing
% to solve a complex medical problem while generating returns for shareholders.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes both the genuine coordination function (a biological rope targeting
% cancer) and the asymmetric extraction enabled by the economic structure (a snare).
% This is the basis for the constraint_claim.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE HEALTHCARE PAYER (TANGLED ROPE)
% Experiences the constraint as a dilemma. It is a victim of the high price but a
% beneficiary of improved patient outcomes (which can reduce other long-term costs).
% Their exit is constrained; refusing to cover a breakthrough therapy is politically
% and ethically untenable. The engine derives a moderate `d`, resulting in a Tangled Rope.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mrna_melanoma_therapy_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(mrna_melanoma_therapy, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(mrna_melanoma_therapy, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated: powerless (Snare) vs institutional (Rope)').

test(tangled_rope_conditions_met, [nondet]) :-
    narrative_ontology:constraint_claim(mrna_melanoma_therapy, tangled_rope),
    domain_priors:requires_active_enforcement(mrna_melanoma_therapy),
    narrative_ontology:constraint_beneficiary(mrna_melanoma_therapy, _),
    narrative_ontology:constraint_victim(mrna_melanoma_therapy, _),
    format('... Tangled Rope structural requirements validated (enforcement, beneficiary, victim)').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(mrna_melanoma_therapy, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(mrna_melanoma_therapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This reflects the significant pricing power granted by patents on a breakthrough therapy. The value is high but not maximal, acknowledging that some of the price is tied to complex manufacturing and R&D costs.
 *   - Suppression (0.75): Intellectual property law is a powerful suppressor of alternatives. For the duration of the patent, no generic or biosimilar competition is possible, leaving patients and payers with few-to-no functionally equivalent options.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the pharmaceutical developer (institutional/arbitrage), the system is a Rope: a beautiful mechanism for coordinating capital, research, and regulation to produce a life-saving, profitable product. For the uninsured patient (powerless/trapped), the exact same system is a Snare: its very existence highlights their exclusion and inability to access a cure, creating immense psychological and financial pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `pharmaceutical_developers` are the primary economic beneficiaries. `high_risk_melanoma_patients` are the primary medical beneficiaries; their inclusion as beneficiaries is critical because it establishes the constraint's genuine coordination function, a prerequisite for the Tangled Rope classification.
 *   - Victims: `uninsured_patients` are victims of exclusion. `healthcare_payers` are victims of the price structure. This dual-victim structure captures both the individual and systemic costs. The directionality engine uses these declarations to compute different `d` values for each agent, explaining why the Payer and the uninsured Patient, while both victims, may experience the constraint differently.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The Healthcare Payer perspective demonstrates a key inter-institutional tension. They are an institutional actor, but their `constrained` exit options (they cannot easily refuse to cover the drug) put them in a weaker bargaining position relative to the developer's `arbitrage` exit options (they can sell the drug to other markets). This asymmetry is central to the extractive dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   This story prevents mandatrophy by refusing a simplistic classification. Labeling it purely a Snare ("price gouging") would ignore the incredible, life-saving coordination it achieves. Labeling it purely a Rope ("innovation") would whitewash the severe extractive consequences of its access model. The Tangled Rope classification, supported by the divergent perspectives, correctly identifies it as a system with both a powerful, beneficial function and a significant, asymmetric cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mrna_melanoma_therapy,
    'Is the high price a necessary condition for future R&D (functional coordination), or is it primarily rent-seeking on past public investment and patient desperation (asymmetric extraction)?',
    'Full, audited transparency of R&D costs, manufacturing expenses, and the proportion of foundational research funded publicly.',
    'If necessary for R&D, the Rope component is stronger. If primarily rent-seeking, the Snare component dominates and ε should be higher.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mrna_melanoma_therapy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), requiring temporal data.
% We model the initial launch (T=0) and project forward 10 years (T=10).

% Theater ratio over time: Initially low, but may rise as marketing budgets grow.
narrative_ontology:measurement(mrna_tr_t0, mrna_melanoma_therapy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mrna_tr_t5, mrna_melanoma_therapy, theater_ratio, 5, 0.08).
narrative_ontology:measurement(mrna_tr_t10, mrna_melanoma_therapy, theater_ratio, 10, 0.10).

% Extraction over time: Starts very high at launch and may slightly decrease as
% manufacturing is optimized or initial R&D is recouped, but patents will keep it high.
narrative_ontology:measurement(mrna_ex_t0, mrna_melanoma_therapy, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(mrna_ex_t5, mrna_melanoma_therapy, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(mrna_ex_t10, mrna_melanoma_therapy, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The therapy is a sophisticated mechanism for allocating
% biological and manufacturing resources to a patient-specific problem.
narrative_ontology:coordination_type(mrna_melanoma_therapy, resource_allocation).

% Network relationships: This constraint is structurally dependent on the
% broader intellectual property regime that enables its pricing model.
narrative_ontology:affects_constraint(pharmaceutical_patent_law, mrna_melanoma_therapy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural declarations of
% beneficiary/victim combined with the distinct exit_options (trapped, arbitrage,
% constrained) allow the directionality engine to derive accurate and
% differentiated `d` values for each agent perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */