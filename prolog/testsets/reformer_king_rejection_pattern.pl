% ============================================================================
% CONSTRAINT STORY: reformer_king_rejection_pattern
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformer_king_rejection_pattern, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformer_king_rejection_pattern
 *   human_readable: Spartan Reformer King Rejection Pattern (Agis IV, Cleomenes III, Nabis)
 *   domain: ancient_politics/reform_attempts
 *
 * SUMMARY:
 *   The Spartan reformer king rejection pattern emerges across three
 *   independent reform attempts spanning 50 years (Agis IV c. 244 BCE,
 *   Cleomenes III 235-222 BCE, Nabis 207-192 BCE). Each reformer confronted
 *   the same structural problem: Spartan citizenship (Spartiate status) had
 *   become demographically unsustainable through the concentration of land
 *   ownership and the exclusion of perioikoi and helots from political
 *   participation. Demographic collapse threatened the state's military
 *   capacity and ultimately its survival. Each reformer attempted to restore
 *   the citizen body by extending or redistributing Spartiate status. Each
 *   met institutional resistance from the ephorate and gerousia — bodies
 *   whose authority derived from their role as kernel-preserving guardians.
 *   Each was neutralized: Agis through execution, Cleomenes through military
 *   defeat against a Macedonian-Spartan alliance, Nabis through
 *   assassination. The constraint is the framework's prediction that
 *   authority structures grounded in the preservation of a fixed kernel
 *   cannot accommodate reform of that kernel without dismantling themselves.
 *   The pattern is not contingent on individual reformer skill or political
 *   judgment but structural — it repeats across three independent instances
 *   with minor variations in method but identical outcomes.
 *
 * KEY AGENTS:
 *   - Agis IV, Cleomenes III, Nabis: Reformer kings (institutional/arbitrage, biographical time horizon) — highest formal authority but zero capacity to execute kernel reform; experience total extraction through institutional veto and elimination
 *   - Ephorate: Institutional guardian of kernel preservation (institutional/arbitrage, immediate time) — experiences constraint as pure coordination of privilege preservation; benefits from veto structure
 *   - Gerousia: Council of elders (institutional/arbitrage, immediate time) — co-benefits from kernel preservation; shares veto authority with ephorate
 *   - Conservative Spartiate citizenship: Broad citizen body (moderate/constrained, generational time) — maintains ritual resistance to reform from identity attachment; experiencing theater increase as ritual decouples from function
 *   - Perioikoi and helots: Subject populations (powerless/trapped, generational time) — structurally necessary to state but excluded from reform debate; bear extraction costs while denied benefits of state participation
 *   - Spartan state container: The body politic (institutional/arbitrage, civilizational time) — faces demographic extinction; structural trap: survival requires dismantling the kernel-preserving institutions that constitute its authority
 *   - Analytical observer: Civilizational perspective (analytical/analytical, civilizational time) — risks naturalizing a contingent institutional outcome as an immutable law of kernel-based authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformer_king_rejection_pattern, 0.68).
domain_priors:suppression_score(reformer_king_rejection_pattern, 0.72).
domain_priors:theater_ratio(reformer_king_rejection_pattern, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformer_king_rejection_pattern, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformer_king_rejection_pattern, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformer_king_rejection_pattern, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformer_king_rejection_pattern, snare).
narrative_ontology:human_readable(reformer_king_rejection_pattern, "Spartan Reformer King Rejection Pattern (Agis IV, Cleomenes III, Nabis)").
narrative_ontology:topic_domain(reformer_king_rejection_pattern, "ancient_politics/reform_attempts").

domain_priors:requires_active_enforcement(reformer_king_rejection_pattern).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformer_king_rejection_pattern, kernel_preserving_oligarchy).
narrative_ontology:constraint_beneficiary(reformer_king_rejection_pattern, ephorate).
narrative_ontology:constraint_beneficiary(reformer_king_rejection_pattern, gerousia).
narrative_ontology:constraint_victim(reformer_king_rejection_pattern, spartan_state_survival).
narrative_ontology:constraint_victim(reformer_king_rejection_pattern, reform_initiators).
narrative_ontology:constraint_victim(reformer_king_rejection_pattern, excluded_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REFORMER KING (SNARE) — Agis, Cleomenes, Nabis each occupy the highest formal authority position yet lack capacity to execute reform despite demographic necessity. Trapped by constitutional veto powers of ephorate and gerousia; exit from kingship means death. The kingship itself becomes a snare: maximum formal authority paired with zero de facto power over the constraint mechanism. Each reformer experiences total extraction — their legitimacy, resources, and ultimately their lives are consumed by the resistance structure.
constraint_indexing:constraint_classification(reformer_king_rejection_pattern, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE KERNEL-PRESERVING OLIGARCHY (ROPE) — Ephorate and gerousia experience the constraint as coordination. They coordinate Spartiate identity preservation, mutual defense of oligarchic privilege, and collective veto over any change that would dilute kernel status. No extraction is experienced — they are pure beneficiaries. They have complete exit capacity (can shift to alternative mechanisms of authority) but do not perceive the need. This is their genuine rope: coordination for collective preservation of privilege.
constraint_indexing:constraint_classification(reformer_king_rejection_pattern, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE CONSERVATIVE CITIZEN BODY (PITON) — The broader Spartiate citizenry experience the constraint as degraded institutional theater. They maintain ritual resistance to reform (voting against proposals, supporting ephoral vetoes) out of identity attachment to Spartiate exclusivity, but the underlying function — preventing demographic catastrophe — is decoupling from the ritual. The theater of citizenship purity persists even as the state faces extinction. By Nabis's time, the citizen body's resistance is largely performative: maintaining the symbolic boundary costs less than acknowledging that the boundary is killing the state.
constraint_indexing:constraint_classification(reformer_king_rejection_pattern, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: PERIOIKOI AND SUBJECT POPULATIONS (TANGLED ROPE) — These agents are structurally necessary to Spartan military and economic function (perioikoi as soldiers and farmers; helots as labor force) yet excluded from citizenship and political status. The constraint extracts their labor while denying them voice in the state's governance. They benefit from continued Spartan state existence (which protects them from larger Greek powers) but bear all costs of the exclusion. Constrained exit: perioikoi cannot simply leave; helots face violent suppression. Genuine coordination function: the Spartan system does organize their labor, albeit coercively.
constraint_indexing:constraint_classification(reformer_king_rejection_pattern, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: THE SPARTAN STATE CONTAINER (SNARE) — The state qua container faces demographic collapse: the number of Spartiates eligible for full citizen status has declined from 9,000 (5th century) to 700 (3rd century). Reform is structurally necessary for survival. But the constraint mechanism is that the kernel-preserving structures (ephorate, gerousia) that give the state its identity become lethal when applied to the problem they were designed to prevent (elite dissolution). The state experiences the constraint as a structural trap: survival requires dismantling the very institutions that constitute its authority. No exit available except subjugation by Macedonian power (Cleomenes) or assassination of the reformer (Nabis) or execution (Agis).
constraint_indexing:constraint_classification(reformer_king_rejection_pattern, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the repeated failure of reform across three independent reformers suggests an immutable structural law: authority systems grounded in kernel fixity cannot accommodate reform of the kernel without dismantling themselves. This appears as a natural law of political institutions — a logical limit intrinsic to hierarchical privilege. However, the engine's false summit detector will identify this as naturalization: the pattern is not a law of nature but a structurally contingent outcome of specific institutional choices (the Spartan constitution's veto gates and the identity fusion between Spartiate status and political legitimacy).
constraint_indexing:constraint_classification(reformer_king_rejection_pattern, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformer_king_rejection_pattern_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reformer_king_rejection_pattern, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformer_king_rejection_pattern, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformer_king_rejection_pattern, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reformer_king_rejection_pattern, TR),
    TR >= 0.70.

:- end_tests(reformer_king_rejection_pattern_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and sustained. The constraint extracts the reformer kings' political capital, resources, and ultimately their lives without producing structural change. Agis was executed; Cleomenes was defeated in foreign war; Nabis was assassinated. The extraction accelerates over the interval (0.42 → 0.68) as the kernel-preserving oligarchy tightens its veto mechanisms and builds external alliances (Macedonia) to suppress reform. The extraction is effective and comprehensive. Suppression (0.72): High. Multiple mechanisms prevent reform: constitutional veto power of ephorate and gerousia; identity fusion between Spartiate status and political legitimacy; military control by kernel-preserving families; alliance formation with external powers (Macedonia under Antigonus Doson) to suppress reformers. The suppression is not incidental — it is the core function of the oligarchic structure. Theater ratio (0.55): Moderate, rising. Early reform attempts (Agis) involved genuine ideological debate about citizenship and redistribution. By Nabis, the theater has increased: the forms of resistance persist (ephoral veto, citizen assemblies voting against reform) while the underlying function (preventing state collapse) has decoupled. The oligarchy maintains the ritual of kernel preservation even as the kernel's preservation is killing the state. The measurement shows theater rising from 0.35 to 0.55 as the institutional response becomes more formulaic and less substantively engaged.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests six distinct classifications depending on observation point. The reformer king (powerless/trapped) sees pure extraction (Snare) — all investment toward reform is consumed by veto and suppression; they cannot escape except by abandoning reform or dying. The kernel-preserving oligarchy (institutional/arbitrage) sees pure coordination (Rope) — they are solving the legitimate problem of maintaining Spartiate identity and privilege through collective veto; no extraction is experienced by them. The broader citizen body (moderate/constrained) sees degraded ritual (Piton) — they maintain resistance patterns that were once functional but have become performative as the state faces extinction. Subject populations (powerless/constrained) see mixed extraction and coordination (Tangled Rope) — they are both organized into the state's labor and military structure and exploited through exclusion. The state container (institutional/arbitrage) sees the ultimate snare (Snare) — survival requires dismantling the authority structures that constitute its identity. The analytical observer (analytical/analytical) risks seeing a natural law (Mountain) — that kernel-based authority cannot reform itself — but this is a false summit naturalizing a contingent institutional outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values flow from beneficiary/victim declarations and exit options. The kernel-preserving oligarchy (ephorate, gerousia) are beneficiaries with arbitrage exit (can shift to alternative power mechanisms if veto fails) — low d → negative chi → rope classification. The reformer kings are victims with trapped exit (kingship is a snare; attempting to escape means death) — high d → high f(d) → high chi → snare classification. The state container is a victim with arbitrage exit (could submit to Macedonian rule and preserve core functions) — moderate-high d → moderate-high f(d) → high chi, but chi is capped by the state's status as institutional actor; this produces the snare classification despite institutional power atom. Subject populations are victims with constrained exit (cannot leave Spartan territory without severe cost; cannot organize resistance without military suppression) — high d → high f(d) → high chi, with mixed coordination function producing tangled rope. The analytical observer derives d from their position of epistemic distance without structural stake (d ≈ 0.72, canonical analytical) — high f(d) → temptation toward mountain, resisted by structural data showing deliberate institutional design, not natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: Extractiveness (0.68) exceeds the critical threshold (0.46), requiring mandatrophy resolution. The constraint is classified as Snare (primary observation from reformer king perspective) but contains genuine coordination functions: the ephorate/gerousia genuinely coordinate kernel preservation (Rope); the state container requires coordination of military and economic resources (Tangled Rope); subject populations require coordinated labor integration (Tangled Rope). The mandatrophy is resolved by recognizing that the constraint IS a snare in its operation (extraction without alternative) but CONTAINS coordination functions that are subordinated to extractive goals. The kernel preservation is the real coordination function; the reformer king's political death is the real extraction. The classification as Snare is correct because the extraction dominates the coordination — the constraint's primary function in practice is to eliminate threats to kernel preservation, not to coordinate state survival. The Rope and Tangled Rope classifications from other perspectives are real but secondary — they describe what the structure could be if extraction were removed, not what it is under kernel-preserving authority. Mandatrophy is resolved by the framework's claim that this is the inevitable outcome of kernel-based authority systems facing survival-level reform: the structure prioritizes its own kernel over the container's survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_fusion_binding,
    'Is the ephorate and gerousia''s resistance to reform driven by institutional logic (structural veto power) or by identity fusion (Spartiate status as constitutive of the leaders'' self-concept)?',
    'Counterfactual institutional design: Would the same oligarchs have accepted reform if the veto structure were removed but Spartiate-exclusive identity preserved? Historical evidence from contemporary Achaean and other Greek reform movements where institutional veto was weaker.',
    'If primarily institutional logic: constraint is contingent on constitutional design and could be reformed by rewriting veto gates. If primarily identity fusion: constraint persists across institutional contexts and requires identity-frame shift to resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_fusion_binding, conceptual, 'Whether resistance is structural veto or identity fusion').

omega_variable(
    reform_timing_causality,
    'Did demographic collapse cause the reform attempts, or did reform attempts expose an existing constraint that was always lethal?',
    'Comparative analysis: Did earlier Greek states that faced demographic pressure without reform-oriented leadership also experience crisis? Did Spartan states without demographic crisis ever attempt kernel reform and fail?',
    'If demographic collapse is necessary trigger: constraint only activates when survival pressures exceed identity preservation. If constraint is always active: demographic crisis merely makes its effects visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_timing_causality, empirical, 'Whether demographic collapse caused or revealed the constraint').

omega_variable(
    alternative_institutional_escape,
    'Could Sparta have reformed the kernel through institutional mechanisms other than direct royal edict (e.g., gradual helot manumission, perioikoi co-option into military officer caste, economic integration without political status)?',
    'Textual and archaeological evidence of actual reform attempts and their mechanisms. Comparison with Macedonian, Achaean, and other hellenistic state models for alternative integration pathways.',
    'If alternatives existed: reformers chose confrontational paths that maximally triggered the constraint. If no alternatives: constraint is genuinely omnibus and blocks all reform pathways.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_institutional_escape, empirical, 'Whether institutional bypass pathways existed').

omega_variable(
    mandatrophy_false_summit_distinction,
    'Is this constraint a genuine natural law of kernel-based authority (Mountain) or a false summit that naturalizes the contingent outcome of Sparta''s specific constitutional structure?',
    'Examine whether other ancient states with kernel-based authority (Roman patrician-plebeian hierarchy, Athenian citizen exclusivity) faced and resolved the same constraint differently. Test whether the pattern repeats across different kernel definitions and institutional veto structures.',
    'If true mountain: kernel-based authority is inherently incompatible with survival-level reform across all historical contexts. If false summit: the constraint is an artifact of Sparta''s constitutional design and could have been navigated differently with different institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_false_summit_distinction, empirical, 'Whether the constraint is a natural law or a false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformer_king_rejection_pattern, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reform_theater_t0, reformer_king_rejection_pattern, theater_ratio, 0, 0.35).
narrative_ontology:measurement(reform_theater_t15, reformer_king_rejection_pattern, theater_ratio, 15, 0.55).
narrative_ontology:measurement(reform_theater_t30, reformer_king_rejection_pattern, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(reform_extraction_t0, reformer_king_rejection_pattern, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(reform_extraction_t15, reformer_king_rejection_pattern, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(reform_extraction_t30, reformer_king_rejection_pattern, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformer_king_rejection_pattern, enforcement_mechanism).
narrative_ontology:affects_constraint(reformer_king_rejection_pattern, spartan_demographic_collapse).
narrative_ontology:affects_constraint(reformer_king_rejection_pattern, helot_rebellion_suppression).
narrative_ontology:affects_constraint(reformer_king_rejection_pattern, macedonian_hegemony_imposition).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family modeling Spartan state collapse. The reformer_king_rejection_pattern is downstream of spartan_demographic_collapse (the underlying demographic crisis that makes reform necessary) and upstream of macedonian_hegemony_imposition (the eventual external resolution). Each family member has distinct epsilon values reflecting different structural mechanisms: demographic_collapse (ε≈0.35, Tangled Rope: coordination of economic allocation with embedded inequality) → reformer_king_rejection (ε≈0.68, Snare: veto-based extraction of reform capacity) → macedonian_hegemony (ε≈0.45, Tangled Rope: external coordination with internal suppression). The family models how a state's internal institutional structure can become incompatible with necessary reform, and how that incompatibility is ultimately resolved by external force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
