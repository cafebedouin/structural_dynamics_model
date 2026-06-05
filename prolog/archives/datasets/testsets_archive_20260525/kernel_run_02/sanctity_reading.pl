% ============================================================================
% CONSTRAINT STORY: sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sanctity_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sanctity_reading
 *   human_readable: Sanctity of Life Reading: Intrinsic Value Prohibition on Intentional Killing
 *   domain: medical_ethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity of life reading represents one distinct normative
 *   interpretation of end-of-life authority. This reading asserts that human
 *   life possesses intrinsic value that cannot be surrendered or overridden
 *   by individual consent, and therefore intentional killing remains
 *   categorically impermissible regardless of the agent's autonomous request.
 *   The constraint exhibits structural properties of a snare from the
 *   perspective of pressured-vulnerable and suffering agents: the prohibition
 *   on intentional killing suppresses their autonomy to choose death, while
 *   institutional actors and doctrine defenders benefit from the clarity and
 *   moral authority the principle provides. The reading coexists with
 *   structurally incompatible siblings — the autonomy reading (which asserts
 *   that individual consent overrides sanctity) and the vulnerability primacy
 *   reading (which prioritizes protection of vulnerable populations but
 *   permits death in autonomy-respecting contexts). Unlike genuine natural
 *   law constraints, this reading requires active institutional enforcement:
 *   medical systems must train physicians to deny requests, manage family
 *   conflict, and document non-compliance. The theater ratio has increased
 *   over the interval (0.38 → 0.55) because actual medical practice has
 *   substantially drifted from the sanctity prohibition (continuous sedation,
 *   withholding aggressive treatment, implicit consent to foregoing
 *   resuscitation) while institutional pronouncements maintain the doctrine.
 *   This drift-without-reclassification is a hallmark of piton dynamics at
 *   the institutional level.
 *
 * KEY AGENTS:
 *   - Pressured-Vulnerable Populations: Primary victim (powerless/trapped) — subject to coercion via family expectation, financial burden, medical authority, or cognitive incapacity; doctrine suppresses their voice
 *   - Suffering Prolonged Agents: Primary victim (moderate/constrained) — competent but embedded in relational/institutional context that suppresses choice to die
 *   - Medical Institutions (Sanctity Doctrine Adopters): Primary beneficiary (institutional/arbitrage) — gain moral authority, externalize responsibility, simplify decision-making via clear principle
 *   - Autonomy-Respecting Physicians: Secondary actor (powerful/mobile) — embedded in system requiring enforcement of sanctity against clinical judgment
 *   - Disability Rights Coalition: Secondary actor (organized/constrained) — benefit from protection against eugenic pressure but constrained by inability to distinguish protection from autonomy suppression
 *   - Religious Institutional Authority: Institutional actor (institutional/arbitrage) — maintains doctrine through inertia while practice drifts; piton dynamics
 *   - Analytical Observer: Civilizational vantage (analytical/analytical) — risks naturalizing the constructed doctrine as unchangeable moral law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sanctity_reading, 0.68).
domain_priors:suppression_score(sanctity_reading, 0.72).
domain_priors:theater_ratio(sanctity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sanctity_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sanctity_reading, snare).
narrative_ontology:human_readable(sanctity_reading, "Sanctity of Life Reading: Intrinsic Value Prohibition on Intentional Killing").
narrative_ontology:topic_domain(sanctity_reading, "medical_ethics/end_of_life_policy").

domain_priors:requires_active_enforcement(sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(sanctity_reading, fixed_text).
narrative_ontology:cs_authority_grounding(sanctity_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(sanctity_reading).
narrative_ontology:cs_kernel_id(sanctity_reading, end_of_life_authority).
narrative_ontology:cs_reading_relation(sanctity_reading, autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation(sanctity_reading, vulnerability_primacy_reading, influences).
narrative_ontology:cs_axiom(sanctity_reading, foundational, intrinsic_value_inviolable).
narrative_ontology:cs_axiom_status(intrinsic_value_inviolable, holdable).
narrative_ontology:cs_axiom_grounding(sanctity_reading, intrinsic_value_inviolable, deontological).
narrative_ontology:cs_axiom(sanctity_reading, foundational, individual_consent_cannot_override_value).
narrative_ontology:cs_axiom_status(individual_consent_cannot_override_value, holdable).
narrative_ontology:cs_axiom_grounding(sanctity_reading, individual_consent_cannot_override_value, deontological).
narrative_ontology:cs_reference_frame(sanctity_reading, sacred_life_irreducible).
narrative_ontology:cs_drift_state(sanctity_reading, contemporary_medical_autonomy_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sanctity_reading, institutional_medical_authority).
narrative_ontology:constraint_beneficiary(sanctity_reading, sanctity_doctrine_defenders).
narrative_ontology:constraint_victim(sanctity_reading, pressured_vulnerable_populations).
narrative_ontology:constraint_victim(sanctity_reading, suffering_prolonged_agents).
narrative_ontology:constraint_victim(sanctity_reading, autonomy_bearing_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESSURED VULNERABLE (SNARE) — Agent subject to coercion via family expectation, financial burden, medical gaslighting, or cognitive incapacity. The prohibition on intentional killing traps this agent in prolonged suffering. No exit option except death denied. Experiences maximum extraction: the constraint forces continuation of a life the agent may not have chosen, while capturing institutional authority's moral claim to have 'protected' the agent. Theater: medical necessity framing obscures the autonomy suppression.
constraint_indexing:constraint_classification(sanctity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUFFERING PROLONGED AGENT (SNARE) — Agent competent to consent but embedded in relational/institutional context that suppresses the choice to die. Family guilt narratives, palliative care messaging, religious institutional pressure, or medical authority's paternalism create high barriers to exercising the choice. The prohibition colludes with suppression mechanisms to deny the agent's chosen exit. High experienced extraction: continued suffering imposed by others' moral framework.
constraint_indexing:constraint_classification(sanctity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEDICAL INSTITUTION / SANCTITY DOCTRINE (ROPE) — Institution benefits from the sanctity reading by outsourcing moral responsibility. The physician is not the agent of death; the principle is. The institution maintains moral authority without engaging complexity of individual cases. Experiences low effective extraction because the reading provides a clear coordination signal: 'Do not kill.' This appears as protection of the commons (medical ethics integrity) rather than institutional capture. Arbitrage exit available: institution can reinterpret principle or switch frameworks.
constraint_indexing:constraint_classification(sanctity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AUTONOMY-RESPECTING PHYSICIAN (TANGLED ROPE) — Powerful institutional actor with mobile exit options (can practice in jurisdictions with different end-of-life regimes, migrate to autonomy-reading systems, retire, specialize). But embedded in a system that requires enforcing the sanctity reading against their best clinical judgment. Experiences mixed extraction: gains moral authority and institutional support from the principle, but is constrained from providing the care they deem most compassionate. Active enforcement required: must deny requests, manage family conflict, document non-compliance. High suppression of alternative practices.
constraint_indexing:constraint_classification(sanctity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DISABILITY RIGHTS COALITION (TANGLED ROPE) — Organized agents with real collective power but constrained by resource barriers and visibility challenges. The sanctity reading offers genuine coordination benefit: protection against eugenic pressure and discriminatory presumptions about disabled life. But the reading also constrains their members' authentic choice to die (for whatever reason — illness burden, not disability per se). High suppression of nuanced distinctions between 'life with disability is worth living' (solidarity) and 'this individual's choice to die should be heard' (autonomy). Active enforcement: institutions cite the sanctity reading to override disabled persons' explicit end-of-life wishes.
constraint_indexing:constraint_classification(sanctity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: RELIGIOUS INSTITUTIONAL AUTHORITY (PITON) — Once-strong doctrine now maintained through institutional inertia. The reading persists in institutional pronouncements, medical oaths, and hospital policy, but actual medical practice in many settings has already shifted: continuous palliative sedation, withholding aggressive treatment, and implicit consent to foregoing resuscitation operate as de facto acceptance of death. The sanctity doctrine is performed in formal statements while bypassed in practice. Theater ratio high: institutions maintain the principle's pronouncements while practice drifts. Piton: the principle is sustained by theatrical adherence, not by functional necessity.
constraint_indexing:constraint_classification(sanctity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the analytical vantage point viewing this reading as grounded in natural law or divine law, the prohibition appears immutable: intrinsic human worth cannot be constructed or destroyed by policy; therefore intentional killing is categorically impermissible. This perspective universalizes the reading and treats it as independent of jurisdiction, cultural variation, or historical contingency. However, the structural data (organized beneficiaries, institutional enforcement, coercive suppression of alternatives) contradicts the mountain classification. This is a false summit: the reading naturalizes what is actually a constructed institutional arrangement grounded in specific theological/philosophical premises, not in discovery of an unchangeable fact.
constraint_indexing:constraint_classification(sanctity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sanctity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sanctity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sanctity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sanctity_reading, TR),
    TR >= 0.70.

:- end_tests(sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The reading extracts prolonged suffering from unwilling subjects whose autonomy is suppressed by the prohibition. The extraction is not maximal (0.72+) because some agents benefit from protection, and institutional actors experience the reading as coordination benefit rather than pure extraction. The value reflects that the primary victims (pressured-vulnerable, suffering agents) bear substantial costs while doctrine defenders gain moral authority. Suppression (0.72): High. Significant barriers include institutional enforcement (medical oaths, hospital policy, legal prohibition), family/relational pressure, medical authority's paternalism, religious doctrine pronouncements, and cognitive/legal barriers to choice. The suppression mechanisms are substantial and reinforcing — the doctrine is enforced at multiple structural levels. Theater ratio (0.55): Moderate-high. The sanctity doctrine is pronounced formally in medical oaths, institutional policies, and religious pronouncements, but actual medical practice has substantially bypassed it: continuous palliative sedation, withholding resuscitation, foregoing aggressive treatment, and de facto acceptance of dying operate as drift from the stated principle. The gap between doctrine and practice reflects that the principle's performative force exceeds its functional necessity — institutions maintain the pronouncement while practice accommodates reality. The theater ratio has increased over the interval as the practice-doctrine gap has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Institutional sanctity doctrine adopters see the reading as coordination and moral clarity (Rope) — a shared principle protecting the vulnerable. The medical institution sees operational efficiency (Piton from degraded perspective) — the doctrine persists through theater while practice drifts. Autonomy-respecting physicians embedded in the system see mixed coordination and constraint (Tangled Rope) — the principle provides moral authority but suppresses their clinical judgment. The disability rights coalition sees mixed protection and constraint (Tangled Rope) — the reading protects them from eugenic pressure but suppresses their members' explicit autonomy. Suffering agents see extraction (Snare) — their autonomy is suppressed by others' moral framework. Pressured-vulnerable populations see maximum extraction (Snare) — the prohibition locks them into prolonged suffering. The analytical observer risks naturalizing the doctrine as immutable moral law (Mountain false summit), revealing the gap between how the reading presents itself (universal, unchangeable) and how it actually functions (constructed, enforced, drifting in practice).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure maps the asymmetric extraction in this reading. Institutional beneficiaries (sanctity doctrine adopters) derive d ≈ 0.10-0.15 from beneficiary status + arbitrage exit → low experienced extraction. Suffering victims (moderate power, constrained exit) derive d ≈ 0.75-0.80 from victim status + constrained exit + embedded suppression → high experienced extraction. Pressured-vulnerable victims (powerless, trapped) derive d ≈ 0.92 from victim status + trapped exit + active enforcement → maximum experienced extraction. The piton institutional authority derives low directionality (arbitrage exit available; can reinterpret doctrine) despite enforcing suppression, because their structural relationship is not victim-like — they benefit from doctrine maintenance through inertia. The analytical observer derives d ≈ 0.72 from the analytical canon (neither beneficiary nor victim, but positioned to see the extraction structure).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via kernel reading frame. The sanctity reading is one legitimate normative interpretation of end-of-life authority, not the only correct classification. Mandatrophy is resolved by recognizing that the classification type (snare) is relative to this specific reading's instantiation, not a claim that sanctity doctrine is universally snare-like. The coexisting autonomy reading would classify the same end-of-life domain as rope or tangled rope (agent choice is coordination value, not extraction). The vulnerability primacy reading would classify as scaffold (protecting vulnerable populations with a sunset toward autonomy as institutions mature). No single type is 'correct' — the kernel permits multiple legitimate readings, each with its own classification. The structural analysis reveals the cost of the sanctity reading: it extracts prolonged suffering from pressured-vulnerable agents while providing moral authority to institutional defenders. This extraction is real and measurable, regardless of whether one endorses the reading's foundational premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_value_foundation,
    'Is the intrinsic value of human life a discovered fact about the world, or a constitutive premise of this particular ethical framework?',
    'Cross-cultural and historical analysis of how end-of-life authority is grounded in different traditions; examination of whether alternative frameworks (autonomy primacy, vulnerability primacy) ground themselves in different foundational claims about value',
    'If discovered fact: the sanctity reading is mountain-like across all frameworks (universal, unchangeable). If constitutive premise: the reading is one legitimate but constructed approach among others (tangled rope or scaffold), and its universalization represents perspectival capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_foundation, conceptual, 'Whether intrinsic human value is a discovered fact or a constitutive framework premise').

omega_variable(
    coercion_vs_protection_boundary,
    'When does the prohibition on intentional killing protect vulnerable populations from coercive pressure versus when does it impose coercive pressure on autonomous agents?',
    'Empirical tracking of: (a) cases where the prohibition prevents coercive death (demonstrable external pressure to die); (b) cases where the prohibition imposes coercive continuation of life (demonstrable wish to die suppressed); (c) cases where protection and coercion are inseparable or ambiguous',
    'If protection cases dominate: snare classification is incorrect; should be scaffolding with protective sunset logic. If coercion cases dominate: snare classification confirmed; the reading extracts suffering from unwilling subjects. If mixed/inseparable: the reading is genuinely tangled rope — protection and extraction are intertwined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_protection_boundary, empirical, 'Empirical boundary between protective and coercive applications of the sanctity prohibition').

omega_variable(
    doctrine_institutional_capture,
    'Does the sanctity reading persist in institutional practice because it reflects genuine moral consensus, or because institutions benefit from the moral authority it provides (externalizing responsibility, simplifying decision-making)?',
    'Comparison of institutional adherence to the sanctity reading with: (a) actual medical practice (continuous palliative sedation, withholding, do-not-resuscitate rates); (b) physician survey data on personal moral views vs institutional policy compliance; (c) family conflict rates and institutional responses to end-of-life requests',
    'If genuine consensus: reading is rope-like (coordination on shared values). If institutional capture: reading is snare-like (doctrine persists because it benefits institutions, not because consensus exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_institutional_capture, empirical, 'Whether sanctity doctrine persistence reflects consensus or institutional capture').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint instantiates the sanctity reading of the end-of-life authority kernel. What is the structural relationship between this reading and the autonomy reading? Do they foreclose each other, coexist, or influence without foreclosing?',
    'Conceptual analysis of the axioms: if both readings are grounded in deontological axioms (inviolable rights), they foreclose each other only if they assign inviolability to incompatible claims. If one is grounded in deontological and the other in instrumental premises, they coexist (different parties can hold both). If both can be held in a single framework with different weights (e.g., ''sanctity is primary, but autonomy can override in specified conditions''), they influence rather than foreclose.',
    'If sanctity forecloses autonomy: the readings are mutually exclusive; a policy regime must choose one. If they coexist: both readings are live across different institutional actors and interpretive traditions. If sanctity influences autonomy: the reading creates structural pressure on autonomy''s scope (e.g., limiting which agents can exercise autonomy) without rendering autonomy incoherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Structural relationship between sanctity and autonomy readings: foreclosure, coexistence, or influence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sanctity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanc_tr_t0, sanctity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sanc_tr_t15, sanctity_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(sanc_tr_t30, sanctity_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(sanc_be_t0, sanctity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sanc_be_t15, sanctity_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(sanc_be_t30, sanctity_reading, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sanctity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sanctity_reading, autonomy_reading).
narrative_ontology:affects_constraint(sanctity_reading, vulnerability_primacy_reading).

% DUAL FORMULATION NOTE:
% The end-of-life authority kernel has three structurally distinct readings: sanctity_reading (this story), autonomy_reading, and vulnerability_primacy_reading. Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different classifications. The three stories are linked via the kernel: they represent competing normative interpretations of the same stabilized commitment (the question of what grounds legitimate end-of-life authority). Do NOT treat this as one constraint with multiple measurements — each reading is a separate constraint story with its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
