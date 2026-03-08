% ============================================================================
% CONSTRAINT STORY: constraint_coupling_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constraint_coupling_amplification, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constraint_coupling_amplification
 *   human_readable: Constraint Coupling Amplification in Systems of Obligation
 *   domain: moral_psychology/agency_depletion/systems_of_obligation
 *
 * SUMMARY:
 *   Constraint coupling amplification occurs when two independently
 *   moderate-extraction constraints interact through identity fusion and
 *   secrecy requirements to produce severe combined extraction that crosses
 *   the Snare threshold. The canonical example: an agent facing workplace
 *   exploitation (chi 0.56) who is also in an abusive relationship (chi
 *   0.60). Each constraint individually would permit exit at biographical
 *   timescales with constrained options. But coupling through identity fusion
 *   (the agent's self-concept is constituted through both roles: 'good
 *   employee' and 'loyal partner') and secrecy requirements (the abusive
 *   partner forbids disclosure of the relationship; the workplace culture
 *   punishes personal problems) eliminates exit options from both. Leaving
 *   the job would trigger relationship crisis (loss of income makes the agent
 *   more dependent on the abusive partner). Leaving the relationship would
 *   trigger workplace crisis (emotional distress and potential homelessness
 *   impair job performance; disclosure of the abuse violates the workplace's
 *   'professionalism' norm). The coupling coefficient transforms the
 *   effective extraction to chi 0.98, trapping the agent in both constraints
 *   simultaneously. This pattern scales across domains: debt + medical
 *   crisis, immigration status + labor exploitation, professional licensing +
 *   institutional capture, national identity + authoritarian governance.
 *
 * KEY AGENTS:
 *   - Agent in Coupled System: Primary victim (powerless/trapped) — experiences amplified extraction chi 0.98 from coupling of two moderate constraints; cannot exit either without catastrophic failure in both
 *   - Potential Support Network: Secondary victim (moderate/constrained) — family, friends, community blocked from providing exit assistance by secrecy requirement; extraction through enforced ignorance
 *   - Primary Institutional Beneficiary: Beneficiary (institutional/arbitrage) — employer, creditor, landlord imposing first constraint; experiences as pure coordination; coupling mechanism invisible from this position
 *   - Secondary Institutional Beneficiary: Beneficiary (institutional/arbitrage) — abusive partner, cult leader, blackmailer imposing second constraint; may actively exploit coupling but experiences as coordination from their position
 *   - Advocacy Organizations: Organized agents (organized/mobile) — domestic violence services, debt relief, workplace rights groups working to break coupling; face institutional resistance and resource constraints
 *   - Analytical Observer: Universal view (analytical/analytical) — sees coupling mechanism as measurable structural amplification; identifies transformation rule blocking and exit option elimination as policy-addressable features
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constraint_coupling_amplification, 0.78).
domain_priors:suppression_score(constraint_coupling_amplification, 0.88).
domain_priors:theater_ratio(constraint_coupling_amplification, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_coupling_amplification, extractiveness, 0.78).
narrative_ontology:constraint_metric(constraint_coupling_amplification, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(constraint_coupling_amplification, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constraint_coupling_amplification, snare).
narrative_ontology:human_readable(constraint_coupling_amplification, "Constraint Coupling Amplification in Systems of Obligation").
narrative_ontology:topic_domain(constraint_coupling_amplification, "moral_psychology/agency_depletion/systems_of_obligation").

domain_priors:requires_active_enforcement(constraint_coupling_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constraint_coupling_amplification, institutional_beneficiary_of_primary_constraint).
narrative_ontology:constraint_beneficiary(constraint_coupling_amplification, institutional_beneficiary_of_secondary_constraint).
narrative_ontology:constraint_victim(constraint_coupling_amplification, agent_in_coupled_system).
narrative_ontology:constraint_victim(constraint_coupling_amplification, potential_support_network).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGENT IN COUPLED SYSTEM (SNARE) — Experiences the full amplified extraction (chi 0.98). Each constraint individually would be survivable (chi 0.56, 0.60), but coupling through identity fusion and secrecy requirements eliminates exit options. Cannot leave either constraint without triggering catastrophic failure in both domains. The coupling coefficient transforms moderate extraction into entrapment.
constraint_indexing:constraint_classification(constraint_coupling_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: POTENTIAL SUPPORT NETWORK (TANGLED ROPE) — Family, friends, or community members who could provide exit assistance but are blocked by the secrecy requirement. They experience the constraint as mixed: genuine desire to help (coordination function) coupled with inability to act on incomplete information (extraction through enforced ignorance). The secrecy component of the coupling mechanism extracts from them by preventing coordination that would otherwise occur.
constraint_indexing:constraint_classification(constraint_coupling_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIMARY INSTITUTIONAL BENEFICIARY (ROPE) — The institution imposing the first constraint (e.g., employer, creditor, landlord) experiences this as pure coordination. From their position, they are simply enforcing a legitimate obligation. They do not see the coupling mechanism because they are not party to the secondary constraint. The amplification is invisible from this perspective.
constraint_indexing:constraint_classification(constraint_coupling_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECONDARY INSTITUTIONAL BENEFICIARY (ROPE) — The institution imposing the second constraint (e.g., abusive partner, cult leader, blackmailer) also experiences this as coordination from their position. They may actively exploit the coupling (recognizing that the agent cannot exit because of the primary constraint), but from their structural position, they are simply maintaining their relationship. The extraction is real but invisible to the beneficiary.
constraint_indexing:constraint_classification(constraint_coupling_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Groups working on domestic violence, debt relief, workplace rights, or cult recovery see the coupling pattern clearly and work to break it. They experience the constraint as tangled rope: genuine coordination function (helping trapped agents) mixed with extraction (institutional resistance to decoupling interventions, legal barriers to breaking secrecy, resource constraints). They can exit by choosing different advocacy targets, but face significant costs.
constraint_indexing:constraint_classification(constraint_coupling_amplification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the analytical position, the coupling mechanism is clearly visible as a structural amplification phenomenon. Two moderate-extraction constraints (chi 0.56, 0.60) combine through identity fusion and secrecy requirements to produce effective chi 0.98. The transformation rule blocking (cannot apply exit strategies from either constraint independently) and exit option elimination (leaving one triggers failure in both) are measurable structural features. This is not a natural law but a contingent institutional arrangement that could be decoupled through policy intervention.
constraint_indexing:constraint_classification(constraint_coupling_amplification, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_coupling_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constraint_coupling_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constraint_coupling_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constraint_coupling_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constraint_coupling_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The base extractiveness represents the weighted average of the coupled system, not the effective chi experienced by the trapped agent (0.98). The base value reflects that the coupling mechanism itself adds extraction beyond the sum of the individual constraints: the identity fusion requirement, the secrecy enforcement, and the transformation rule blocking all impose additional costs. The measurement trajectory shows gradual accumulation as the coupling tightens over time — initial coupling is weak (0.58), but as identity fusion deepens and secrecy becomes more enforced, extraction increases to 0.78. Suppression (0.88): Very high. The coupling mechanism eliminates exit options that would exist for either constraint independently. An agent facing only workplace exploitation could quit; an agent facing only an abusive relationship could leave. But the coupled system blocks both exits: leaving one triggers failure in the other. The suppression is not total (0.88 rather than 1.0) because coordinated intervention (simultaneous provision of new employment and domestic violence shelter) can break the coupling, but such intervention is rare and resource-intensive. Theater ratio (0.45): Moderate. The constraints themselves have genuine functional content (the workplace produces real output; the relationship may have genuine attachment components), but the coupling mechanism has significant theatrical elements: the secrecy requirement is performative (maintained through shame and identity protection rather than material necessity), and the identity fusion is partly constructed through narrative rather than structural dependency. The theater ratio increases over time as the functional components atrophy and the performative maintenance becomes more dominant.
 *
 * PERSPECTIVAL GAP:
 *   The institutional beneficiaries see pure coordination (Rope) because the coupling mechanism is invisible from their position — they are party to only one constraint and do not see how it interacts with the other. The trapped agent sees pure extraction (Snare) because they experience the full amplified chi and have no exit options. The potential support network sees mixed coordination and extraction (Tangled Rope) because they genuinely want to help but are blocked by the secrecy requirement. The advocacy organizations also see Tangled Rope because they are working to break the coupling but face institutional resistance. The analytical observer sees Snare because the coupling mechanism is structurally measurable and clearly produces severe extraction, but also recognizes that this is a contingent institutional arrangement, not a natural law. The perspectival gap is maximal between the institutional beneficiaries (Rope) and the trapped agent (Snare), reflecting the fundamental asymmetry: the beneficiaries do not experience the coupling because they are not subject to both constraints simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The trapped agent is a victim of both constraints with no exit options, yielding maximum directionality (d ≈ 0.95). The potential support network is blocked from helping by the secrecy requirement, making them secondary victims with constrained exit (d ≈ 0.60). The institutional beneficiaries experience low directionality (d ≈ 0.10) because they are primary beneficiaries with arbitrage exit options — they can replace the agent costlessly. The advocacy organizations are organized agents working to break the coupling, with mobile exit options (they can choose different advocacy targets), yielding moderate directionality (d ≈ 0.45). The analytical observer uses the canonical analytical directionality (d ≈ 0.72). The coupling mechanism amplifies the trapped agent's experienced extraction through three pathways: (1) transformation rule blocking — exit strategies that work for single constraints fail for coupled constraints; (2) exit option elimination — leaving one constraint triggers failure in the other; (3) identity fusion — the agent's self-concept is constituted through both constraints, making exit psychologically unthinkable even when materially possible. The effective chi calculation: chi_A = 0.56, chi_B = 0.60, coupling_coefficient = 0.75, chi_effective = chi_A + chi_B + (coupling_coefficient × chi_A × chi_B) = 0.56 + 0.60 + (0.75 × 0.56 × 0.60) = 0.56 + 0.60 + 0.252 = 1.412, capped at 1.0, but experienced as 0.98 due to residual agency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that coupling amplification is a real structural phenomenon, not a mislabeling of coordination as extraction. The individual constraints (chi 0.56, 0.60) are themselves Tangled Ropes — they have genuine coordination functions mixed with extraction. But the coupling mechanism transforms the system into a Snare by eliminating the exit options that would exist for either constraint independently. The mandatrophy question 'Is this really extraction or just hard coordination?' is answered by the omega variables: empirical measurement of the coupling coefficient, the identity fusion threshold, and the amplification reversibility will determine whether the coupling is a necessary feature of the coordination (in which case the Snare classification is correct) or an artifact of poor institutional design (in which case policy intervention can decouple the constraints and restore the Tangled Rope classification for each individually). The analytical observer's Snare classification is not a false summit — the coupling mechanism is a real structural feature that produces measurable severe extraction. But the classification is contingent on the coupling persisting; decoupling interventions would change the classification by changing the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coupling_coefficient_measurement,
    'What is the precise coupling coefficient that transforms independent moderate extraction into combined severe extraction?',
    'Empirical measurement of exit attempt outcomes: success rate when attempting to leave one constraint while maintaining the other, compared to baseline exit rates for uncoupled constraints of similar individual severity',
    'If coupling coefficient < 0.3: constraints are weakly coupled, agent retains sequential exit option. If coefficient > 0.7: constraints are strongly coupled, exit from either triggers catastrophic failure in both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coupling_coefficient_measurement, empirical, 'Magnitude of coupling coefficient in constraint amplification').

omega_variable(
    identity_fusion_threshold,
    'At what point does identity fusion become the primary coupling mechanism versus material dependency?',
    'Longitudinal analysis of agents who successfully decoupled: did they require identity reconstruction (therapeutic intervention, community support, narrative reframing) or material support (financial assistance, housing, legal protection)? Proportion of each intervention type indicates mechanism dominance.',
    'If identity fusion dominates: decoupling requires psychological intervention and long timeline. If material dependency dominates: decoupling requires resource provision and shorter timeline. Mixed cases require both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_threshold, empirical, 'Relative contribution of identity fusion versus material dependency to coupling strength').

omega_variable(
    secrecy_enforcement_mechanism,
    'Is the secrecy requirement externally enforced (threats, surveillance, punishment) or internally maintained (shame, identity protection, cognitive dissonance)?',
    'Analysis of disclosure patterns: do agents disclose when external enforcement is removed but internal barriers remain? Comparison of disclosure rates across different enforcement removal scenarios (legal protection vs therapeutic support vs community acceptance).',
    'If externally enforced: legal intervention can break coupling. If internally maintained: psychological intervention required. If both: coordinated legal and therapeutic intervention necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secrecy_enforcement_mechanism, empirical, 'Mechanism maintaining secrecy requirement in coupled constraints').

omega_variable(
    amplification_reversibility,
    'Can the coupling be broken by addressing one constraint, or must both be addressed simultaneously?',
    'Intervention outcome analysis: success rates of single-constraint interventions (debt relief alone, domestic violence shelter alone, workplace accommodation alone) versus coordinated interventions addressing both constraints simultaneously',
    'If sequential decoupling works: intervention can be staged and resourced incrementally. If simultaneous decoupling required: intervention must be comprehensive and resource-intensive from the start.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amplification_reversibility, empirical, 'Whether coupled constraints can be addressed sequentially or require simultaneous intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constraint_coupling_amplification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coupling_amp_tr_t0, constraint_coupling_amplification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(coupling_amp_tr_t2, constraint_coupling_amplification, theater_ratio, 2, 0.38).
narrative_ontology:measurement(coupling_amp_tr_t4, constraint_coupling_amplification, theater_ratio, 4, 0.4).
narrative_ontology:measurement(coupling_amp_tr_t6, constraint_coupling_amplification, theater_ratio, 6, 0.42).
narrative_ontology:measurement(coupling_amp_tr_t8, constraint_coupling_amplification, theater_ratio, 8, 0.44).
narrative_ontology:measurement(coupling_amp_tr_t10, constraint_coupling_amplification, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(coupling_amp_initial_extraction, constraint_coupling_amplification, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(coupling_amp_be_t2, constraint_coupling_amplification, base_extractiveness, 2, 0.64).
narrative_ontology:measurement(coupling_amp_be_t4, constraint_coupling_amplification, base_extractiveness, 4, 0.7).
narrative_ontology:measurement(coupling_amp_be_t6, constraint_coupling_amplification, base_extractiveness, 6, 0.74).
narrative_ontology:measurement(coupling_amp_be_t8, constraint_coupling_amplification, base_extractiveness, 8, 0.76).
narrative_ontology:measurement(coupling_amp_be_t10, constraint_coupling_amplification, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constraint_coupling_amplification, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of indexical_relativity_of_extraction (which establishes that extraction is observer-relative) and asymmetric_duty_structure (which provides one of the component constraints in typical coupling scenarios). The coupling amplification mechanism is a distinct structural phenomenon: it takes two moderate-extraction constraints and amplifies them through interaction effects (identity fusion, secrecy requirements, transformation rule blocking) to produce severe extraction. The upstream constraints have their own extractiveness values; this constraint models the amplification phenomenon itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
