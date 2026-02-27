% ============================================================================
% CONSTRAINT STORY: biological_curiosity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biological_curiosity, []).

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
 *   constraint_id: biological_curiosity
 *   human_readable: Curiosity (The Information-Seeking Drive)
 *   domain: biological/technological/social
 *
 * SUMMARY:
 *   Curiosity is the innate biological drive to seek information and resolve
 *   uncertainty. While fundamentally a tool for learning and survival, its
 *   compulsive nature can be harnessed, exploited, or suppressed. This
 *   constraint story models the drive itself as a high-cost, high-reward
 *   mechanism. Its classification varies dramatically depending on the
 *   agent's structural relationship to the drive: whether they are its master
 *   (a scientist), its victim (an algorithmically-captured user), its
 *   cultivator (an educator), or its beneficiary (an attention merchant). The
 *   high base metrics reflect the 'Pandora Effect': the drive compels
 *   information-seeking even when the outcome may be costly or harmful,
 *   making it a powerful source of both progress and vulnerability.
 *
 * KEY AGENTS:
 *   - Exploited Media Consumer: Primary victim (powerless/trapped) — their attention is extracted by systems hijacking their innate drive.
 *   - Attention Merchant: Primary beneficiary (institutional/arbitrage) — treats curiosity as a natural resource to be harvested for profit.
 *   - Scientist/Explorer: Mixed role (powerful/mobile) — uses the drive for discovery but also bears its high costs and risks.
 *   - Educator: Cultivator (organized/constrained) — channels the drive toward productive ends via temporary support structures.
 *   - Corporate 'Innovator': Performative actor (institutional/constrained) — engages in the theater of curiosity while suppressing its function.
 *   - Analytical Observer: Sees the full structure, including the 'false summit' of framing a costly drive as a neutral natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biological_curiosity, 0.6).
domain_priors:suppression_score(biological_curiosity, 0.65).
domain_priors:theater_ratio(biological_curiosity, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biological_curiosity, extractiveness, 0.6).
narrative_ontology:constraint_metric(biological_curiosity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(biological_curiosity, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biological_curiosity, tangled_rope).
narrative_ontology:human_readable(biological_curiosity, "Curiosity (The Information-Seeking Drive)").
narrative_ontology:topic_domain(biological_curiosity, "biological/technological/social").

domain_priors:requires_active_enforcement(biological_curiosity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biological_curiosity, attention_merchants).
narrative_ontology:constraint_beneficiary(biological_curiosity, scientists_and_explorers).
narrative_ontology:constraint_beneficiary(biological_curiosity, educators).
narrative_ontology:constraint_victim(biological_curiosity, exploited_media_consumers).
narrative_ontology:constraint_victim(biological_curiosity, risk_taking_explorers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPLOITED CONSUMER (SNARE) — The innate drive to fill information gaps is hijacked by algorithms designed to maximize engagement. The user feels a compulsion they cannot resist, extracting their time and attention for corporate profit. They are trapped by their own biology. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.68.
constraint_indexing:constraint_classification(biological_curiosity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ATTENTION MERCHANT (ROPE) — For a social media platform or publisher, curiosity is a vast, self-renewing natural resource. They experience it as a pure coordination mechanism to direct user behavior toward monetizable ends. As a full beneficiary with arbitrage, their effective extraction is negative. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(biological_curiosity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SCIENTIST (TANGLED ROPE) — Experiences curiosity as a powerful engine for discovery (coordination) but also as a demanding force that requires immense sacrifice of time, resources, and safety (extraction). The drive produces knowledge but extracts a high cost from the individual. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(biological_curiosity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE EDUCATOR (SCAFFOLD) — Views curiosity as the fundamental substrate upon which to build knowledge. The educational system is a temporary structure designed to channel this innate drive until the student achieves intellectual autonomy. χ is low because the educator is a beneficiary. The classification is scaffold due to the implicit sunset clause. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.24. Sunset Rationale: The goal of education is to make the educator obsolete; the scaffold is removed when the student can learn independently.
constraint_indexing:constraint_classification(biological_curiosity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CORPORATE 'INNOVATOR' (PITON) — In a corporate culture that performatively values 'curiosity' but punishes the risk-taking it entails, the drive's function is degraded. It persists as a theatrical value ('innovation workshops', posters) while its true purpose—unpredictable discovery—is suppressed. The high theater_ratio (0.75) and low effective extraction for this beneficiary (χ≈-0.01) trigger the Piton classification.
constraint_indexing:constraint_classification(biological_curiosity, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: PRIMAL ORGANISM (MOUNTAIN) — From the perspective of basic survival, curiosity is not a choice but an immutable law of existence, like gravity. It is a fixed, unchangeable drive to map the environment. This perspective frames the drive as a natural law. However, the high base metrics (ε=0.60, S=0.65) mean the engine will flag this as a 'false summit'—a mischaracterization of a high-cost biological compulsion as a zero-cost natural law.
constraint_indexing:constraint_classification(biological_curiosity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biological_curiosity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biological_curiosity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biological_curiosity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biological_curiosity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biological_curiosity, TR),
    TR >= 0.70.

:- end_tests(biological_curiosity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.60): High. The drive is not neutral; it compels the expenditure of energy, time, and safety resources to close information gaps, regardless of the information's ultimate utility. This represents the biological cost of the 'explore' algorithm. Suppression (S=0.65): High. The internal compulsion to resolve uncertainty is extremely difficult to resist, suppressing other cognitive priorities. This is the mechanism hijacked by clickbait and algorithmic feeds. Theater Ratio (T=0.75): High. In modern social and corporate contexts, the *expression* of curiosity is often performative ('innovation culture') and decoupled from its functional purpose of genuine, risky exploration, which is often penalized.
 *
 * PERSPECTIVAL GAP:
 *   This story is a diagnostic exemplar, showing how a single constraint with fixed properties can manifest as all six types. For the Attention Merchant, it's a beneficial Rope. For the user they exploit, it's a Snare. For the scientist who wrestles with it, it's a Tangled Rope. For the educator who channels it, it's a Scaffold. For the corporation that pays it lip service, it's a Piton. For a primal view of survival, it's a Mountain. The gap is not in the constraint's properties but in the agent's structural position relative to its costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (attention merchants, educators) experience the drive as a resource that enables their function, leading to low or negative effective extraction (Rope, Scaffold). Victims (exploited users) experience the drive as an irresistible compulsion that extracts their resources, leading to high effective extraction (Snare). Agents with a mixed relationship (scientists) experience both the coordinative benefits and extractive costs (Tangled Rope). The directionality `d` is derived from this structural relationship, determining the final classification for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that 'type' is not an intrinsic property of a constraint but an emergent feature of the interaction between a constraint's base properties and an observer's indexed position. The question 'What type is curiosity?' is ill-posed. The correct question is 'From which structural position are you observing curiosity?' The ability to derive all six classifications from a single set of metrics validates the indexical approach and shows that perspectival disagreement is a predictable outcome of structural asymmetry, not a sign of analytical failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drive_teleology,
    'Is curiosity fundamentally a drive to reduce the aversion of an ''information gap'' (deprivation) or a drive to seek the pleasure of discovery (exploration)?',
    'Neurochemical studies differentiating dopamine pathway activation in response to uncertainty reduction vs. novel stimulus presentation.',
    'If deprivation-based, its nature is more extractive (ε is high). If exploration-based, it is more coordinative (ε is lower). This would shift the analytical classification between Tangled Rope and Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drive_teleology, empirical, 'Whether curiosity''s primary driver is aversion-reduction or pleasure-seeking.').

omega_variable(
    supernormal_stimuli_capture,
    'Can technological systems create ''supernormal stimuli'' for curiosity (e.g., algorithmically generated information gaps) that permanently override biological self-regulation?',
    'Longitudinal studies on cognitive function and attention regulation in populations with high vs. low exposure to algorithmic content feeds.',
    'If capture is possible, the ''Snare'' perspective becomes the dominant social reality, and the base suppression metric may be understated. If self-regulation adapts, the ''Snare'' is a temporary state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supernormal_stimuli_capture, empirical, 'The capacity of technology to permanently capture the curiosity drive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biological_curiosity, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biol_tr_t0, biological_curiosity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(biol_tr_t25, biological_curiosity, theater_ratio, 25, 0.6).
narrative_ontology:measurement(biol_tr_t50, biological_curiosity, theater_ratio, 50, 0.75).

% Extraction over time
narrative_ontology:measurement(biol_be_t0, biological_curiosity, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(biol_be_t25, biological_curiosity, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(biol_be_t50, biological_curiosity, base_extractiveness, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biological_curiosity, information_standard).
narrative_ontology:affects_constraint(biological_curiosity, algorithmic_attention_capture).
narrative_ontology:affects_constraint(biological_curiosity, scientific_method).
narrative_ontology:affects_constraint(biological_curiosity, pedagogical_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
