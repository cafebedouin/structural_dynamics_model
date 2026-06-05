% ============================================================================
% CONSTRAINT STORY: idf_knock_on_roof_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_idf_knock_on_roof_policy, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: idf_knock_on_roof_policy
 *   human_readable: IDF 'Knock on the Roof' Warning Policy
 *   domain: political/military/humanitarian
 *
 * SUMMARY:
 *   The 'Knock on the Roof' policy represents a military practice in which
 *   the Israel Defense Forces (IDF) issues warnings (via direct strikes on
 *   building roofs, SMS messages, or leaflets) to civilians in target zones
 *   before conducting strikes intended to destroy military targets. The
 *   policy exists in structural tension between humanitarian law requirements
 *   for civilian precaution and military operational doctrine emphasizing
 *   speed, precision, and strategic effect. The constraint extracts civilian
 *   protection capacity from the humanitarian legal system by providing a
 *   procedural mechanism (warning) that appears to satisfy precaution
 *   requirements while systematically failing to enable meaningful
 *   evacuation. The warning functions as both a coordination mechanism (IDF
 *   can claim compliance with distinction principle) and an extraction
 *   apparatus (civilian populations are forced into impossible choices:
 *   remain and risk death, or attempt evacuation into infrastructure
 *   incapable of handling mass displacement). The policy's theater ratio has
 *   increased over time as the humanitarian framing has accumulated
 *   institutional legitimacy despite consistent evidence of evacuation
 *   failure.
 *
 * KEY AGENTS:
 *   - Palestinian Civilians in Target Zones: Primary victims (powerless/trapped) — receive warning but face blocked evacuation routes, no safe destination, structural incapacity of humanitarian infrastructure
 *   - IDF Operational Command: Primary beneficiary (institutional/arbitrage) — extracts targeting data and international legal buffer from warning system; can modify or abandon policy
 *   - Gaza Healthcare and Humanitarian Services: Secondary victims (moderate/constrained) — must manage mass casualty surges created by warning-strike sequence; cannot refuse service provision
 *   - International Humanitarian Law Institutions: Institutional observer (institutional/constrained) — trapped between defending norms and accepting warning system as compliant practice; benefits from documented compliance framework
 *   - International Media and Advocacy Organizations: Organized observers (organized/constrained) — constrained by institutional pressure to accept warnings as evidence of restraint; bear cost of documenting systematic evacuation failure without institutional response
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — detects extraction mechanism (civilian protection extracted from humanitarian norms through procedural compliance); reveals suppression of alternative targeting protocols
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(idf_knock_on_roof_policy, 0.58).
domain_priors:suppression_score(idf_knock_on_roof_policy, 0.72).
domain_priors:theater_ratio(idf_knock_on_roof_policy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(idf_knock_on_roof_policy, snare).
narrative_ontology:human_readable(idf_knock_on_roof_policy, "IDF 'Knock on the Roof' Warning Policy").
narrative_ontology:topic_domain(idf_knock_on_roof_policy, "political/military/humanitarian").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(idf_knock_on_roof_policy, idf_operational_commanders).
narrative_ontology:constraint_victim(idf_knock_on_roof_policy, palestinian_civilians_in_target_zones).
narrative_ontology:constraint_victim(idf_knock_on_roof_policy, humanitarian_evacuation_capacity).
narrative_ontology:constraint_victim(idf_knock_on_roof_policy, civilian_life_preservation_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN CIVILIANS IN TARGET ZONES (SNARE) — Receive warning (knock or SMS) but lack meaningful exit: overwhelmed evacuation infrastructure, blocked border crossings, no safe destination, building collapse during 'grace period'. Extraction mechanism: forced choice between remaining (death) or attempting evacuation under fire. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.61.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GAZA HEALTHCARE & HUMANITARIAN SERVICES (SNARE) — Warning system creates extractive load: must manage mass casualty surges, collapsed infrastructure, accelerated triage under impossible conditions. Constrained exit (cannot refuse to serve). Extraction: humanitarian capacity is weaponized against civilians through asymmetric warning structure. d≈0.80, f(d)≈1.20, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IDF OPERATIONAL COMMAND (ROPE) — Experiences policy as pure coordination: warning system enables extraction of targeting accuracy data, reduces civilian presence during strike, and provides legal/international relations buffer ('we warned them'). Arbitrage exit via policy modification. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary through coordination function (apparent adherence to humanitarian norms).
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL HUMANITARIAN & LEGAL INSTITUTIONS (TANGLED ROPE) — System provides coordination function (documented warnings can validate claim of civilian precaution) but also enforces institutional extraction: trapped between defending humanitarian norms and accepting warning system as sufficient compliance. Active enforcement required (institutions must affirm or condemn). Benefits from documented warning practice (coordination framework); bears cost of moral hazard (efficiency of killing masked by warning theater). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN LAW INSTITUTIONS (PITON) — Warning system became ritualized compliance mechanism maintaining institutional appearance of restraint. Theater ratio (0.68) reflects that warnings are performative: evacuation failure is built into the system, yet institutions continue citing warnings as proof of 'proportionality' and 'distinction' adherence. Inertia-driven: replacing warning system with genuine precaution would require reimagining targeting doctrine, but institutional legitimacy is tied to current practice. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.30.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HUMANITARIAN NORMS (SNARE) — From civilizational scale, policy extracts legitimacy from humanitarian law framework while systematically undermining actual civilian protection. Warning system violates core distinction principle (civilians cannot be reduced to decision-making nodes in a targeting algorithm). Suppression of alternatives (policy framed as compliant vs alternative targeting regimes) is total. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(idf_knock_on_roof_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(idf_knock_on_roof_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(idf_knock_on_roof_policy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(idf_knock_on_roof_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(idf_knock_on_roof_policy, TR),
    TR >= 0.70.

:- end_tests(idf_knock_on_roof_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The policy creates systematic extraction of civilian protection capacity through asymmetric warning structure. Civilians receive notice but lack agency (no safe exit, no meaningful choice). The extraction is not total (warnings do enable some self-protective action) but severe because the structural design of the constraint makes evacuation failure predictable and built-in. Theater ratio (0.68): High. The warning system's legitimacy depends on performative compliance with humanitarian norms rather than actual civilian protection. Early policy (T=0, theater=0.42) was presented as genuine precaution; over time (T=16, theater=0.68) the disconnect between procedure and outcome has become clearer, yet institutional acceptance of the system as 'humanitarian' has increased, indicating growing ritualization. Suppression (0.72): High. Multiple mechanisms suppress civilian alternatives: (1) geographical containment (sealed borders, no third country refuge), (2) infrastructure incapacity (evacuation routes, shelters, medical services overwhelmed), (3) information asymmetry (civilians cannot verify strike timing or coordinate evacuation), (4) procedural entrapment (accepting warning as sufficient completes the extraction — refusal to evacuate presented as civilian choice, not systemic failure).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap driven by structural position and exit capacity. The IDF sees Rope — a coordination mechanism enabling targeting with humanitarian compliance. The command's arbitrage exit (ability to modify policy) transforms the constraint from their perspective into pure coordination. Palestinian civilians in target zones see Snare — extraction mechanism with no exit. Their trapped position and absent alternatives make the warning itself part of the extraction apparatus. International humanitarian institutions see Tangled Rope — they benefit from the coordination function (documented warnings validate humanitarian framework) while bearing the cost of enforcing a system that fails to protect. The analytical observer detects the Snare hidden within the Rope-shaped institutional framing: the warning system extracts legitimacy from humanitarian norms without providing the civilian protection those norms require. The key distinction: those with arbitrage exit experience the constraint as coordination; those without exit experience it as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   IDF Operational Command: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Command extracts operational data and legal cover from the warning system; retains full capacity to modify policy if costs exceed benefits. International humanitarian institutions: Both + constrained → d≈0.55, f(d)≈0.75. Moderate extraction. Institutions benefit from the coordination framework (documented warnings) but are constrained by the system's failure (humanitarian norms violated, yet institutions cite compliance). They cannot arbitrage out without undermining their own legitimacy. Palestinian civilians: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Civilians receive warning (procedural inclusion) but lack exit options (trapped by geography, infrastructure, information). The warning itself becomes part of the extraction mechanism: procedural fairness without substantive protection. Gaza humanitarian services: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction. Services are constrained by resource limits and obligation to serve; they cannot arbitrage out. The warning system extracts their capacity by forcing triage under impossible conditions.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that institutional perspectives and victim perspectives generate fundamentally different classifications because they occupy different structural positions relative to exit options. The IDF sees Rope (coordination with arbitrage exit). International institutions see Tangled Rope (mixed coordination and extraction, constrained exit). Civilians see Snare (extraction, trapped exit). All three classifications are correct from their respective structural positions. The error would be asserting a single 'true' type independent of perspective. The constraint IS hybrid (Tangled Rope) at the institutional level because institutions have constrained but non-zero exit capacity; they can (at cost) abandon the system. But for the powerless population receiving warnings with no evacuation route, the constraint is unambiguously Snare because exit is zero. The presheaf of perspectives over different structural positions reveals the full reality: the system functions as Rope for those who design it, Tangled Rope for institutions that legitimize it, and Snare for those subjected to it. Theater ratio drift (0.42→0.68) indicates Goodhart's Law in action: as the warning system became institutionalized as 'humanitarian compliance,' the focus shifted from actual civilian protection (function) to documented warnings (proxy metric), allowing the extraction mechanism to persist despite systematic evidence of failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evacuation_infrastructure_sufficiency,
    'Does the grace period between warning and strike provide sufficient time and accessible routes for meaningful evacuation, or is structural capacity inherently insufficient?',
    'Empirical analysis: grace period duration vs evacuation route capacity, border crossing throughput, shelter availability; correlation between warning timing and evacuation success rates',
    'If infrastructure is adequate: warning system functions as genuine precaution (Rope from civilian perspective). If structural insufficiency is systematic: warning is performative, extraction mechanism fully activated (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evacuation_infrastructure_sufficiency, empirical, 'Whether evacuation infrastructure can actually handle warned populations').

omega_variable(
    targeting_doctrine_alternatives,
    'Could alternative targeting protocols (extended pre-strike investigation, drone surveillance, precision timing to minimize civilian presence) replace the warning system without loss of military capability?',
    'Comparative analysis of targeting outcomes under alternative protocols; assessment of intelligence gathering vs strike timing trade-offs; case studies from other military contexts',
    'If alternatives exist with comparable military effect: warning system is choice, not necessity (extraction mechanism confirmed). If alternatives materially reduce military options: policy is true hybrid (Tangled Rope from command perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_doctrine_alternatives, empirical, 'Whether viable alternative targeting protocols exist').

omega_variable(
    humanitarian_institution_capture,
    'Are international humanitarian institutions endorsing the warning system due to genuine assessment of civilian protection or due to institutional capture by military necessity framing?',
    'Documentary analysis of institutional statements; correlation between statements and actual civilian casualty data; institutional response to evidence of evacuation failure',
    'If genuine assessment: Tangled Rope holds (institutions benefit from coordination framework). If captured: Piton classification strengthened (ritualized compliance, theater ≥0.70).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_institution_capture, conceptual, 'Degree of humanitarian institution capture by military framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(idf_knock_on_roof_policy, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(korc_tr_t0, idf_knock_on_roof_policy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(korc_tr_t8, idf_knock_on_roof_policy, theater_ratio, 8, 0.58).
narrative_ontology:measurement(korc_tr_t16, idf_knock_on_roof_policy, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(korc_be_t0, idf_knock_on_roof_policy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(korc_be_t8, idf_knock_on_roof_policy, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(korc_be_t16, idf_knock_on_roof_policy, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(idf_knock_on_roof_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(idf_knock_on_roof_policy, gaza_civilian_casualty_accountability).
narrative_ontology:affects_constraint(idf_knock_on_roof_policy, humanitarian_law_institutional_legitimacy).
narrative_ontology:affects_constraint(idf_knock_on_roof_policy, proportionality_assessment_doctrine).

% DUAL FORMULATION NOTE:
% The 'Knock on the Roof' policy is downstream of IDF targeting doctrine and upstream of broader humanitarian law compliance claims. The policy's ε=0.58 (moderate-high extraction) reflects the specific structural design of warning-as-extraction; upstream constraints (targeting doctrine, military necessity framing) have their own ε values reflecting doctrinal justifications; downstream constraints (humanitarian institution legitimacy) have ε values reflecting institutional capture dynamics. This story focuses on the policy mechanism itself, not the broader doctrinal or institutional justifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(idf_knock_on_roof_policy, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
