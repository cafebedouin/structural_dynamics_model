% ============================================================================
% CONSTRAINT STORY: ai_edu_decentralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_edu_decentralization, []).

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
 *   constraint_id: ai_edu_decentralization
 *   human_readable: The AI-Education Decoupling
 *   domain: technological/educational/economic
 *
 * SUMMARY:
 *   The deployment of AI as a general-purpose technology in education lowers
 *   the coordination costs for creating and managing small, personalized
 *   learning environments. This fuels a partial decentralization, enabling
 *   the rise of microschools and other alternatives to traditional public
 *   schooling. While this creates new opportunities, it also establishes a
 *   structural tension: a dynamic, innovative private tier for those who can
 *   afford it, and a potentially hollowed-out public tier for those who
 *   cannot. The constraint is the two-tiered system that emerges from this
 *   technological shift.
 *
 * KEY AGENTS:
 *   - EdTech Companies & Microschool Operators: Primary beneficiaries (institutional/arbitrage) - Profit from selling AI platforms and new educational models.
 *   - Affluent Families: Secondary beneficiaries (powerful/mobile) - Can exit the public system for perceived superior, personalized options.
 *   - Low-Income Families: Primary victims (powerless/trapped) - Left in a public system with diminishing resources and peer effects.
 *   - Public Education Systems: Institutional victims (institutional/constrained) - Face student flight, funding loss, and functional degradation, resorting to performative innovation (Piton).
 *   - Education Choice Advocates: Organized agents (organized/mobile) - See the disruption as a temporary tool (Scaffold) to achieve ideological goals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_edu_decentralization, 0.48).
domain_priors:suppression_score(ai_edu_decentralization, 0.62).
domain_priors:theater_ratio(ai_edu_decentralization, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_edu_decentralization, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_edu_decentralization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_edu_decentralization, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_edu_decentralization, tangled_rope).
narrative_ontology:human_readable(ai_edu_decentralization, "The AI-Education Decoupling").
narrative_ontology:topic_domain(ai_edu_decentralization, "technological/educational/economic").

domain_priors:requires_active_enforcement(ai_edu_decentralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_edu_decentralization, edtech_companies).
narrative_ontology:constraint_beneficiary(ai_edu_decentralization, microschool_operators).
narrative_ontology:constraint_beneficiary(ai_edu_decentralization, affluent_families).
narrative_ontology:constraint_victim(ai_edu_decentralization, public_education_systems).
narrative_ontology:constraint_victim(ai_edu_decentralization, low_income_families).
narrative_ontology:constraint_victim(ai_edu_decentralization, teacher_unions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME FAMILIES (SNARE) — Trapped in a declining public system as resources and high-performing peers exit. They bear the full cost of the opportunity gap with no viable exit. For them, this is pure extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(ai_edu_decentralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EDTECH COMPANIES (ROPE) — Experience the constraint as a pure coordination mechanism. Their technology lowers the cost of delivering personalized education, creating new markets and opportunities. They are net beneficiaries. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(ai_edu_decentralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (AI enabling new, efficient educational models) and the severe asymmetric extraction (hollowing out the public commons). This is the system's claimed type.
constraint_indexing:constraint_classification(ai_edu_decentralization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC SCHOOL ADMINISTRATION (PITON) — The function of providing universal, high-quality education is degrading. The administration engages in performative AI adoption ('innovation theater') to appear current, but the core institutional purpose is atrophying due to student/funding flight. The high theater_ratio (0.75) meets the piton gate.
constraint_indexing:constraint_classification(ai_edu_decentralization, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: EDUCATION CHOICE ADVOCATES (SCAFFOLD) — View AI as a temporary lever to break the public school monopoly. They believe this disruption is a necessary, transitional phase (a scaffold) toward a more pluralistic and market-based educational ecosystem, with an ideological sunset on the disruption once the new system is established.
constraint_indexing:constraint_classification(ai_edu_decentralization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: MARKET LIBERTARIAN (MOUNTAIN) — Frames the decoupling as the inevitable, natural law of creative destruction. The failure of a state monopoly when faced with superior technology is seen as an immutable economic principle, not a contingent outcome. The engine will identify this as a false summit, as the base properties (high ε, high suppression) contradict a mountain classification.
constraint_indexing:constraint_classification(ai_edu_decentralization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_edu_decentralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_edu_decentralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_edu_decentralization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_edu_decentralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_edu_decentralization, TR),
    TR >= 0.70.

:- end_tests(ai_edu_decentralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Represents the value siphoned from the public commons. This is not a direct tax, but the social and economic cost of a widening opportunity gap and the degradation of a universal public good. Suppression (0.62): High. Alternatives are suppressed for the victims by lack of resources, and for the legacy system by bureaucratic inertia. The momentum of the new paradigm makes resistance difficult. Theater Ratio (0.75): High. Legacy public systems engage in 'innovation theater' by adopting superficial AI initiatives to appear competitive, while the core function of universal quality education erodes. This high ratio is key to the Piton perspective.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a diagnostic exemplar. The same structural reality is perceived radically differently. For EdTech innovators, it's a Rope solving a coordination problem. For trapped families, it's a Snare extracting their children's future opportunities. For a public school administrator, it's a Piton—a degraded institution they must maintain performatively. For a market-oriented reformer, it's a temporary Scaffold to a better future. For a libertarian ideologue, it's an inevitable Mountain of economic law. The analytical observer sees the whole picture: a Tangled Rope, with both genuine coordination and severe, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (EdTech, affluent families) have arbitrage/mobile exit options, leading to low 'd' values and a Rope classification. Victims (low-income families) are trapped, leading to a high 'd' value and a Snare classification. Institutional actors caught in the middle (public schools) are constrained, leading to intermediate 'd' values and a Piton classification driven by high theater. The analytical view balances these factors to arrive at Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating that a single constraint can legitimately occupy multiple classifications depending on the indexical position of the observer. The question is not 'Is it a Rope or a Snare?' but 'From whose perspective?' The framework's ability to hold all six classifications in superposition, derived from a single set of base properties, prevents the mislabeling of a complex system as either purely beneficial coordination or purely malicious extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_pedagogical_impact,
    'Does AI-driven personalized education produce superior long-term cognitive and non-cognitive outcomes compared to traditional models, or does it optimize for short-term metrics at the expense of creativity and resilience?',
    'Longitudinal studies comparing cohorts from AI-driven microschools and traditional public schools on measures of creativity, problem-solving, and career success.',
    'If outcomes are superior, the coordination aspect is stronger (Rope/Tangled Rope). If outcomes are inferior or merely different, the extractive/performative aspects are stronger (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_pedagogical_impact, empirical, 'Long-term cognitive and social impact of AI-driven education models').

omega_variable(
    public_system_adaptation,
    'Are public education systems structurally capable of integrating AI to provide similar benefits at scale, or are they too constrained by bureaucracy and political economy to adapt effectively?',
    'Case studies of large public districts attempting to deploy personalized AI learning platforms, measuring cost, implementation fidelity, and student outcomes versus private models.',
    'If adaptable, the constraint is a temporary Scaffold. If not, it solidifies as a permanent Snare for those left behind.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_system_adaptation, empirical, 'Capacity of public education systems to adapt to AI-driven decentralization').

omega_variable(
    regulatory_capture_by_edtech,
    'To what extent will the emerging regulatory framework for AI in education be shaped by incumbent public interests versus the lobbying of a concentrated EdTech industry?',
    'Analysis of lobbying expenditures, campaign contributions, and the legislative text of new education technology regulations.',
    'If captured by EdTech, suppression and extractiveness will increase (Snare). If balanced, it may function as a regulatory Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_by_edtech, preference, 'Influence of EdTech industry on future education regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_edu_decentralization, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_e_tr_t0, ai_edu_decentralization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ai_e_tr_t5, ai_edu_decentralization, theater_ratio, 5, 0.65).
narrative_ontology:measurement(ai_e_tr_t10, ai_edu_decentralization, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(ai_e_be_t0, ai_edu_decentralization, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ai_e_be_t5, ai_edu_decentralization, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ai_e_be_t10, ai_edu_decentralization, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_edu_decentralization, resource_allocation).
narrative_ontology:affects_constraint(ai_edu_decentralization, public_good_funding_crisis).
narrative_ontology:affects_constraint(ai_edu_decentralization, credentialing_and_accreditation_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
