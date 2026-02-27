% ============================================================================
% CONSTRAINT STORY: happiness_of_others
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_happiness_of_others, []).

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
 *   constraint_id: happiness_of_others
 *   human_readable: The Social Responsibility for the Happiness of Others
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The norm that individuals are responsible for others' happiness operates
 *   as a hybrid coordination-extraction mechanism. At its coordination
 *   function, it binds people through reciprocal emotional care and genuine
 *   interdependence — the mutual commitment to each other's wellbeing is a
 *   real feature of authentic relationships. At its extraction function, it
 *   creates asymmetric obligations where emotionally empathic or dependent
 *   agents internalize unlimited responsibility for others' emotional states,
 *   generating guilt-based compliance and suppressing autonomous needs. The
 *   constraint has declined in enforceability over the past 30 years as
 *   psychological literacy normalized boundaries and therapeutic frameworks
 *   reframed 'responsibility for your own happiness' as a healthy stance. Yet
 *   it persists through institutional inertia in religious/moral traditions
 *   and through opportunistic enforcement by control-oriented agents. The
 *   theater ratio has increased as the norm becomes more aspirational than
 *   binding — people mouth the responsibility language while behavioral
 *   adoption declines.
 *
 * KEY AGENTS:
 *   - Empathic Dependent: Primary victim (powerless/trapped) — internalized responsibility with no exit mechanism, bears full emotional labor burden
 *   - People-Pleaser: Secondary victim (moderate/constrained) — faces guilt activation if boundaries are set, but can access therapy and boundary-setting strategies
 *   - Control-Oriented Partner: Primary beneficiary (institutional/arbitrage) — uses constraint to deflect emotional responsibility upward while maintaining plausible deniability
 *   - Narcissistic Agent: Secondary beneficiary (institutional/arbitrage) — exploits empathy-based guilt to extract validation and emotional labor
 *   - Therapeutic Movement: Organized challenger (organized/mobile) — building alternative norms (internal emotional responsibility, boundaries) with sunset logic
 *   - Religious/Moral Institution: Institutional maintainer (institutional/arbitrage) — teaches norm through inertia but no longer enforces it effectively
 *   - Analytical Observer: Structural analyst (analytical/analytical) — risks naturalizing a contingent institutional practice as inherent to human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(happiness_of_others, 0.58).
domain_priors:suppression_score(happiness_of_others, 0.68).
domain_priors:theater_ratio(happiness_of_others, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(happiness_of_others, extractiveness, 0.58).
narrative_ontology:constraint_metric(happiness_of_others, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(happiness_of_others, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(happiness_of_others, tangled_rope).
narrative_ontology:human_readable(happiness_of_others, "The Social Responsibility for the Happiness of Others").
narrative_ontology:topic_domain(happiness_of_others, "social/psychological").

domain_priors:requires_active_enforcement(happiness_of_others).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(happiness_of_others, narcissistic_agents).
narrative_ontology:constraint_beneficiary(happiness_of_others, control_oriented_relationship_partners).
narrative_ontology:constraint_victim(happiness_of_others, empathic_individuals).
narrative_ontology:constraint_victim(happiness_of_others, people_pleasers).
narrative_ontology:constraint_victim(happiness_of_others, dependent_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPATHIC DEPENDENT (SNARE) — Internalized responsibility for another's emotional state with no exit mechanism. Cannot refuse without guilt activation. Bears full extraction: emotional labor, suppressed own needs, perpetual monitoring of other's mood. Maximum experienced chi — trapped agent with high d.
constraint_indexing:constraint_classification(happiness_of_others, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RELATIONSHIP PARTICIPANT (TANGLED ROPE) — Experiences both coordination (mutual emotional support enhances bonding) and extraction (unequal emotional labor allocation, guilt-based compliance). Exit is costly but possible (therapy, separation planning). Significant extraction but not total — some agency through therapeutic intervention or boundary-setting.
constraint_indexing:constraint_classification(happiness_of_others, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTROL-ORIENTED AGENT (ROPE) — Benefits from the constraint by establishing leverage: 'your happiness is your responsibility' becomes 'your failure to make me happy is your fault.' Experiences this as pure coordination mechanism — a way to delegate emotional labor upward while maintaining plausible deniability. Arbitrage exit: can always reframe or withdraw demand if directly confronted.
constraint_indexing:constraint_classification(happiness_of_others, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THERAPEUTIC MOVEMENT (SCAFFOLD) — Organized mental health advocates see this as a temporary coordination failure being solved by normalized therapy, boundary-setting education, and cultural shift toward emotional responsibility as internal-only. Sunset logic: as psychological literacy increases, the constraint's enforcement weakens. Theater is declining as alternative norms (self-care, boundaries) gain legitimacy.
constraint_indexing:constraint_classification(happiness_of_others, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RELIGIOUS/MORAL TRADITION (PITON) — Historically maintained the constraint as sacred duty ('love your neighbor,' 'sacrifice for others'). Now largely performative — institutions teach the norm but don't enforce it, and many practitioners ignore it. Theater ratio high because the norm is taught/repeated but functionally degraded (fewer people internalize it fully). Maintained through inertia: institutions benefit from the appearance of moral seriousness without demanding actual enforcement.
constraint_indexing:constraint_classification(happiness_of_others, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW (FALSE SUMMIT) — Claims the constraint is inherent: humans are naturally empathic, interdependent creatures; responsibility for others' happiness is written into our social nature. However, the structural data reveals this as naturalization: the constraint requires active enforcement, benefits specific agents asymmetrically, and is being successfully weakened by cultural norms. This is not a law of nature but a contingent institutional practice.
constraint_indexing:constraint_classification(happiness_of_others, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(happiness_of_others_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(happiness_of_others, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(happiness_of_others, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(happiness_of_others, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(happiness_of_others, TR),
    TR >= 0.70.

:- end_tests(happiness_of_others_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant emotional labor from empathic agents and creates guilt-based compliance mechanisms. However, it is not maximal (0.72+) because the extraction depends on internalization — it requires the victim to accept the responsibility frame. Agents who reject the frame (through therapy, cultural exposure, or personality factors) experience lower extraction. The decline from 0.72 to 0.58 over 30 years reflects increased cultural literacy and therapeutic normalization of boundaries. Suppression (0.68): High. Multiple reinforcement mechanisms: social expectation, religious/moral framing, guilt activation, internalized shame, relationship-threat rhetoric ('if you don't make me happy, you don't love me'). Alternatives are suppressed through relational threat — boundary-setting is reframed as selfishness or abandonment. Exit is costly emotionally and socially. Theater ratio (0.65): Moderate-high. The norm is widely taught and performed but increasingly disconnected from actual enforcement. Religious institutions, parental training, and romantic culture affirm the responsibility frame, but behavioral compliance is declining. The gap between stated norm and actual practice has widened as psychological frameworks provide alternative legitimacy for boundaries.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is enormous. The primary victim (empathic dependent) classifies this as snare — pure extraction with no coordination benefit. The primary beneficiary (control-oriented agent) classifies it as rope — pure coordination for mutual emotional care. The organized therapeutic movement classifies it as scaffold — a temporary coordination problem with sunset through increased psychological literacy. The religious institution classifies it as piton — a once-functional norm now maintained through performative teaching without real enforcement. The false summit perspective sees it as mountain — inherent to human nature, a law of social physics. These are not minor variations; they represent fundamentally different causal stories about the same constraint. The analytical observer can measure this gap: beneficiary-derived d ≈ 0.05 produces χ ≈ -0.12 (they see benefit, low/negative extraction), while victim-derived d ≈ 0.95 produces χ ≈ 1.42 (they experience high extraction). The gap is a diagnostic indicator that the constraint has asymmetric structural properties — it benefits some agents by extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural relationship to the happiness responsibility extraction. Empathic dependents have d ≈ 0.95 (trapped victims): they experience the constraint as imposed externally but have internalized it as their own moral obligation, producing maximum guilt-based compliance. The sigmoid f(d) maps this to high effective extraction chi. Control-oriented beneficiaries have d ≈ 0.05 (full beneficiary with arbitrage): they can reframe, withdraw demand, or claim misunderstanding without consequence, producing low/negative effective extraction chi (they experience benefit, not cost). The therapeutic movement has d ≈ 0.50 (symmetric): they both benefit from and bear costs of the constraint as they work to dismantle it — they must first internalize it to understand it, then teach alternatives. Organized agents (therapeutic/cultural movements) have elevated power relative to powerless individuals, reducing effective chi through coalition capacity and narrative authority. The structural derivation from beneficiary/victim declarations produces these d values automatically: victims with trapped exit get high d; beneficiaries with arbitrage get low d; agents with constrained exit get intermediate d scaled by their power and exit alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint exhibits high extractiveness (0.58) without clear resolution of the ambiguity between coordination (mutual emotional care is real and valuable) and extraction (the responsibility norm enables guilt-based manipulation). The constraint genuinely serves BOTH functions simultaneously in different structural positions: for healthy couples with balanced power, it coordinates mutual care; for power-asymmetric relationships, it extracts emotional labor from the dependent partner. The mandatrophy would be 'resolved' by distinguishing two separate constraints: (1) 'mutual emotional support in relationships' (lower extractiveness, pure rope), and (2) 'asymmetric guilt-based emotional responsibility' (higher extractiveness, snare/tangled rope). However, in actual social practice, agents cannot neatly separate these — the same norm serves both functions, and the beneficiary deliberately conflates them ('if you cared about me, you would make my happiness your responsibility'). The theater ratio increase (0.35 to 0.65) suggests the constraint is degrading toward piton — the norm is increasingly taught as theater (schools teach 'emotional intelligence' and 'empathy') while actual enforcement declines. This degradation may be resolving the mandatrophy: as the constraint becomes more performative and less binding, the extraction function weakens, and what remains is primarily coordination without enforcement cost. The organized therapeutic movement is actively working toward this resolution by establishing competing norms (internal emotional responsibility, boundaries as healthy) that preserve the coordination benefit while eliminating the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empathy_as_constraint_vs_feature,
    'Is empathic resonance with others'' emotional states a constraint on autonomy or a feature that enables authentic relationship?',
    'Longitudinal study comparing relationship satisfaction, autonomy, and mental health outcomes between high-empathy individuals who maintain responsibility boundaries vs those who internalize full responsibility',
    'If feature: the constraint is coordination (higher rope classification). If constraint: the responsibility norm exploits empathy to extract labor, confirming snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empathy_as_constraint_vs_feature, conceptual, 'Whether empathy is inherent feature or exploitable constraint').

omega_variable(
    responsibility_causality_attribution,
    'Do individuals actually believe they can cause another''s happiness, or is this a performative belief maintained for social compliance?',
    'Implicit association testing (IAT) comparing stated beliefs about causality vs actual behavior when told explicitly they cannot influence another''s mood; behavioral tracking of effort allocation when causality is uncertain',
    'If genuinely believed: suppression is high (agents internalize the impossible standard). If performative: suppression is actually lower, and extraction is driven by manipulation rather than internalized guilt.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(responsibility_causality_attribution, empirical, 'Whether responsibility-for-happiness is believed or performed').

omega_variable(
    cultural_variation_in_constraint_binding,
    'Does the happiness responsibility constraint bind with equal force across individualist vs collectivist cultural contexts?',
    'Cross-cultural comparison of empathy-based guilt activation, relationship autonomy norms, and therapeutic uptake rates; linguistic analysis of responsibility framing in different cultural traditions',
    'If universal: suggests structural constraint intrinsic to social bonding. If culturally variable: suggests institutional/historical practice that can be modified. Would affect claimed_type classification across cultural contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_variation_in_constraint_binding, empirical, 'Cultural variation in happiness responsibility binding').

omega_variable(
    agent_power_coalition_threshold,
    'At what point do empathic/dependent agents organize collectively to challenge the constraint, and what shifts their power from ''powerless'' to ''organized''?',
    'Historical tracking of therapy movement, support group formation, and cultural narrative shifts (self-care, boundaries); identification of critical mass thresholds and policy/media tipping points',
    'If organization occurs: powerless perspective shifts toward organized, potentially upgrading classification from snare to tangled rope. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agent_power_coalition_threshold, empirical, 'Coalition power threshold for dependent agents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(happiness_of_others, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(happy_tr_t0, happiness_of_others, theater_ratio, 0, 0.35).
narrative_ontology:measurement(happy_tr_t15, happiness_of_others, theater_ratio, 15, 0.52).
narrative_ontology:measurement(happy_tr_t30, happiness_of_others, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(happy_be_t0, happiness_of_others, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(happy_be_t15, happiness_of_others, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(happy_be_t30, happiness_of_others, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(happiness_of_others, enforcement_mechanism).
narrative_ontology:affects_constraint(happiness_of_others, guilt_activation_social_control).
narrative_ontology:affects_constraint(happiness_of_others, empathy_exploitation_in_relationships).
narrative_ontology:affects_constraint(happiness_of_others, boundary_setting_as_moral_transgression).

% DUAL FORMULATION NOTE:
% The happiness responsibility constraint decomposes into separate structural claims depending on relational context: mutual_emotional_support_coordination (lower extractiveness, rope) and asymmetric_guilt_based_responsibility (higher extractiveness, snare/tangled rope). These are not two measurement perspectives on one constraint; they are two distinct structural mechanisms that operate simultaneously and are deliberately conflated by control-oriented agents. The upstream constraints (guilt_activation, empathy_exploitation) generate the extraction; downstream constraints (boundary_setting_moralization) reinforce it by reframing healthy autonomy as moral failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(happiness_of_others, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
