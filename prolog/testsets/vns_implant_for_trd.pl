% ============================================================================
% CONSTRAINT STORY: vns_implant_for_trd
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_vns_implant_for_trd, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vns_implant_for_trd
 *   human_readable: Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint represents the system surrounding a surgically implanted
 *   Vagus Nerve Stimulation (VNS) device for patients with treatment-resistant
 *   depression (TRD). While offering a potential therapeutic pathway where
 *   other treatments have failed, the system involves high costs, significant
 *   information asymmetry, regulatory gatekeeping, and a profit motive for
 *   the manufacturer. The constraint's structure is defined by the interaction
 *   between desperate patients, profit-seeking manufacturers, cost-bearing
 *   insurers, and regulating bodies.
 *
 * KEY AGENTS (by structural relationship):
 *   - patients_with_trd: Primary target (powerless/trapped) — bears the physical risks and is coerced by their condition into a high-cost, last-resort option.
 *   - device_manufacturer_livanova: Primary beneficiary (institutional/arbitrage) — profits from the device's sale and ongoing clinical relationship.
 *   - healthcare_insurers_cms: Institutional victim (institutional/constrained) — bears the high financial cost of the treatment, with limited ability to refuse coverage for an approved, effective therapy.
 *   - medical_researchers: Analytical observer — evaluates the long-term efficacy and costs of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vns_implant_for_trd, 0.55).
domain_priors:suppression_score(vns_implant_for_trd, 0.85).   % Structural property (raw, unscaled). High due to the definition of "treatment-resistant".
domain_priors:theater_ratio(vns_implant_for_trd, 0.10).       % Piton detection (>= 0.70). Low, as the activity is highly functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vns_implant_for_trd, extractiveness, 0.55).
narrative_ontology:constraint_metric(vns_implant_for_trd, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vns_implant_for_trd, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(vns_implant_for_trd, tangled_rope).
narrative_ontology:human_readable(vns_implant_for_trd, "Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression").

% --- Binary flags ---
domain_priors:requires_active_enforcement(vns_implant_for_trd). % Requires FDA approval, insurance reimbursement policies, surgical standards.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, device_manufacturer_livanova).
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, implanting_physicians).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(vns_implant_for_trd, patients_with_trd).
narrative_ontology:constraint_victim(vns_implant_for_trd, healthcare_insurers_cms).

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

% PERSPECTIVE 1: THE PATIENT (SNARE)
% The patient is the target. By definition, other options have failed,
% creating a 'trapped' condition. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This high effective extraction and high suppression make it a Snare.
constraint_indexing:constraint_classification(vns_implant_for_trd, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE DEVICE MANUFACTURER (ROPE)
% The manufacturer is the primary beneficiary, with capital mobility.
% Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% From this view, it's a pure coordination mechanism for delivering a therapy.
constraint_indexing:constraint_classification(vns_implant_for_trd, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ ≈ 0.55 * 1.15 * 1.2 ≈ 0.76. This, with high ε & suppression, is a Tangled Rope.
constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4: HEALTHCARE INSURER / CMS (TANGLED ROPE)
% An institutional actor, but a victim of the cost structure. Its exit is
% constrained by its mandate to cover effective care. The engine derives a
% mid-to-high d, leading to a Tangled Rope classification. It recognizes the
% therapeutic coordination but is subjected to the high extraction.
constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ORGANIZED PATIENT ADVOCACY GROUP (TANGLED ROPE)
% If patients organize (Dynamic Coalition), their power becomes 'organized' and
% exit 'constrained'. They are still victims. This lowers their derived `d`
% compared to the powerless individual. The lower effective extraction (χ)
% shifts their classification from Snare to Tangled Rope, as they can now
% perceive and negotiate the trade-offs of the system's coordination function.
constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vns_implant_for_trd_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(vns_implant_for_trd, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(vns_implant_for_trd, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_victim_is_tangled_rope) :-
    % Verifies that the institutional victim (insurer) also sees a Tangled Rope, distinct from the beneficiary's Rope.
    constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(high_extraction_and_suppression_are_set) :-
    domain_priors:base_extractiveness(vns_implant_for_trd, E), E >= 0.46,
    domain_priors:suppression_score(vns_implant_for_trd, S), S >= 0.60.

:- end_tests(vns_implant_for_trd_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.55): Reflects a high-cost, high-margin medical device where value is extracted from the healthcare system and passed to the manufacturer.
 *   - Suppression (0.85): Extremely high because the target population is, by definition, "treatment-resistant," meaning alternatives have already failed. The constraint's existence depends on the lack of viable alternatives.
 *   - The combination of a genuine therapeutic function (coordination) and high, asymmetric costs (extraction) makes this a canonical Tangled Rope from a systemic view.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The powerless, trapped patient experiences the system as a Snare—a coercive, high-cost "choice" born of desperation. The manufacturer, with capital mobility (arbitrage exit), sees it as a Rope—an elegant solution that coordinates resources to solve a problem and generate profit. The analytical view must reconcile these, landing on Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Value flows from the general pool of healthcare funds (paid by everyone, managed by insurers) and directly from patients towards the beneficiaries: the device manufacturer and the medical providers who perform the implantation. The `constraint_beneficiary` and `constraint_victim` declarations model this flow, which in turn drives the directionality `d` and the resulting perspectival classifications.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The insurer (CMS) provides a key inter-institutional perspective. Although a powerful `institutional` actor, it is a `victim` of the cost structure. Its `constrained` exit (due to its mandate) differentiates it from the manufacturer's `arbitrage` exit. This results in CMS perceiving a Tangled Rope—it sees the coordination benefit for patients but also feels the coercive financial extraction, unlike the manufacturer who only sees upside.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents misclassification. A purely market-based view might see this as a simple Rope (voluntary exchange), ignoring the coercive context of TRD. A purely critical view might see it as a predatory Snare, ignoring the genuine, albeit limited, therapeutic benefit. The Tangled Rope classification correctly captures the hybrid nature: it is a system with a real coordination function that is coupled to a severe extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_vns_implant_for_trd,
    'Is the observed long-term efficacy (30% remission at 5 years) a robust therapeutic effect justifying the high cost and invasiveness, or is it a marginal gain amplified by placebo effects and patient selection bias?',
    'Large-scale, long-term, independently funded, randomized controlled trials with a sham surgery control group.',
    'If robustly effective, the Tangled Rope classification holds. If marginal or driven by placebo, the extractive component dominates, pushing the constraint closer to a pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(vns_implant_for_trd, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Since base_extractiveness > 0.46, temporal data is required to track
% lifecycle drift. The model shows a stable system post-approval, with
% extraction high from the outset due to R&D and market positioning.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(vns_implant_for_trd_tr_t0, vns_implant_for_trd, theater_ratio, 0, 0.05).
narrative_ontology:measurement(vns_implant_for_trd_tr_t5, vns_implant_for_trd, theater_ratio, 5, 0.10).
narrative_ontology:measurement(vns_implant_for_trd_tr_t10, vns_implant_for_trd, theater_ratio, 10, 0.10).

% Extraction over time (starts high and stabilizes):
narrative_ontology:measurement(vns_implant_for_trd_ex_t0, vns_implant_for_trd, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(vns_implant_for_trd_ex_t5, vns_implant_for_trd, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(vns_implant_for_trd_ex_t10, vns_implant_for_trd, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It allocates scarce, high-tech medical resources.
narrative_ontology:coordination_type(vns_implant_for_trd, resource_allocation).

% Network relationships: The success and regulatory pathway of this device
% influences the evaluation of future, similar neuro-technologies.
narrative_ontology:affects_constraint(vns_implant_for_trd, fda_device_approval_pathway).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation chain,
% using the declared beneficiary/victim groups and their differing exit options
% (arbitrage vs. trapped vs. constrained), accurately models the
% directionality for each key agent.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */