% ============================================================================
% CONSTRAINT STORY: grief_coordination_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_grief_coordination_mechanism, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: grief_coordination_mechanism
 *   human_readable: Grief Coordination Mechanism
 *   domain: social/interpersonal/emotional
 *
 * SUMMARY:
 *   Grief is fundamentally a coordination problem: the sudden absence of a
 *   person requires the community to collectively acknowledge the loss,
 *   redistribute relational roles, mark temporal and social transition, and
 *   support those most dependent on the deceased. The grief coordination
 *   mechanism solves this by institutionalizing mourning: prescribing
 *   expression norms, temporal boundaries, ritual participation, and public
 *   acknowledgment. However, the mechanism that coordinates collective grief
 *   also suppresses individualized grief expression, creates extractive
 *   opportunities (commercial funeral industry), and locks bereaved
 *   individuals into identity performances ('the grieving widow,' 'the
 *   grieving parent'). The constraint exhibits simultaneous genuine
 *   coordination function and asymmetric extraction — characteristic of
 *   Tangled Rope. The theater ratio (0.58) reflects that much of the ritual
 *   (viewing, embalming, wake attendance, public display) serves performative
 *   social functions rather than the bereaved individual's emotional
 *   processing. Theater has increased over the observation interval as
 *   commercialization has added performative requirements (memorial videos,
 *   elaborate flowers, viewing logistics) while core coordination functions
 *   (acknowledging loss, role redistribution) remain constant.
 *
 * KEY AGENTS:
 *   - Bereaved Individuals: Primary victims (powerless/identity_locked) — their identity post-loss is constituted through prescribed mourning roles; exit would require abandoning grief identity
 *   - Extended Family Network: Secondary victims and partial beneficiaries (moderate/constrained) — provide mutual support (coordination benefit) but face time and emotional labor costs; some family members suppress divergent grief timelines
 *   - Religious and Ceremonial Institutions: Primary beneficiaries (institutional/arbitrage) — provide genuine coordination value (meaning-making, role redistribution, temporal marking) with minimal extraction; these organizations solve the coordination problem they're designed for
 *   - Commercial Funeral Industry: Secondary beneficiary with extraction mechanism (powerful/arbitrage) — benefits from requirement to publicly ritualize grief; manufactures theater (costly services) that wouldn't exist absent the coordination mechanism; captures vulnerability during high-suppression periods
 *   - Authentic Emotional Expression: Structural victim (powerless/trapped) — the requirement to suppress individualized grief in favor of prescribed expression is the primary extraction mechanism; cannot organize or advocate because suppression is diffuse and internalized
 *   - Grief Support and Hospice Movement: Organized challengers (organized/constrained) — represent alternative coordination pathway with lower theater and lower suppression; have sunset logic (as therapy literacy normalizes, rigid mourning requirements decline)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(grief_coordination_mechanism, 0.32).
domain_priors:suppression_score(grief_coordination_mechanism, 0.48).
domain_priors:theater_ratio(grief_coordination_mechanism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(grief_coordination_mechanism, extractiveness, 0.32).
narrative_ontology:constraint_metric(grief_coordination_mechanism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(grief_coordination_mechanism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(grief_coordination_mechanism, tangled_rope).
narrative_ontology:human_readable(grief_coordination_mechanism, "Grief Coordination Mechanism").
narrative_ontology:topic_domain(grief_coordination_mechanism, "social/interpersonal/emotional").

domain_priors:requires_active_enforcement(grief_coordination_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(grief_coordination_mechanism, social_continuity_institutions).
narrative_ontology:constraint_beneficiary(grief_coordination_mechanism, grief_mediators).
narrative_ontology:constraint_victim(grief_coordination_mechanism, bereaved_individuals).
narrative_ontology:constraint_victim(grief_coordination_mechanism, authentic_emotional_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BEREAVED INDIVIDUAL (SNARE) — The grieving person is identity-locked into prescribed mourning roles. Their identity post-loss is constituted through the constraint: 'the grieving widow,' 'the bereaved parent,' 'the surviving child.' Exit would require abandoning the identity fusion with the lost relationship. Suppression is high: social expectations about appropriate grief expression, timeline, and behavior lock the person into performing grief even when their authentic emotional state diverges. They cannot simply stop grieving because grief has become their primary relational identity to the community.
constraint_indexing:constraint_classification(grief_coordination_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: EXTENDED FAMILY NETWORK (TANGLED ROPE) — Family members face constrained exit from the mourning structure. They both benefit (the ritual coordinates mutual support, provides collective meaning-making, reinforces family bonds) and bear costs (time commitment, emotional labor, expectation to suppress their own divergent grief timelines). Different family members experience different extraction levels based on proximity to the deceased and expected role in the ritual.
constraint_indexing:constraint_classification(grief_coordination_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: RELIGIOUS/CEREMONIAL INSTITUTIONS (ROPE) — Funeral homes, clergy, cultural tradition-keepers experience the constraint as pure coordination. They are solving a genuine problem: how to collectively acknowledge loss, mark transition, redistribute the deceased's relational roles. These institutions benefit from participation (legitimacy, financial sustenance, social authority) but provide genuine coordination value. Low extraction because the beneficiary position aligns with providing coordination service.
constraint_indexing:constraint_classification(grief_coordination_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: COMMERCIAL FUNERAL INDUSTRY (TANGLED ROPE) — For-profit funeral operations coordinate genuine mourning functions while extracting via cost inflation, unnecessary services, and emotional vulnerability exploitation. They benefit from grief's regulatory requirement to publicly mourn: the constraint that authentic private grief must be externalized and ritualized creates captive demand. High theater: embalming, casket selection, memorial photography, and viewing are largely performative costs that wouldn't exist absent the coordination mechanism's theatrical requirements.
constraint_indexing:constraint_classification(grief_coordination_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GRIEF SUPPORT/HOSPICE MOVEMENT (SCAFFOLD) — Organized grief counselors, hospice workers, and peer support groups represent an alternative pathway with explicit sunset logic. They aim to decompose the rigid mourning constraint into individualized grief processing: therapy breaks the theater requirement, allows non-linear emotional timelines, and provides agency over expression. Theater and extraction both lower as grief-processing moves from social ritual (high theater, suppressed authentic emotion) to therapeutic encounter (lower theater, supported authentic expression). Sunset: as grief literacy and counseling normalize, the requirement to perform grief in prescribed social rituals declines.
constraint_indexing:constraint_classification(grief_coordination_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CULTURAL MOURNING EXPECTATIONS (PITON) — Examined from long-term civilizational view, many prescribed mourning rituals are vestigial theatrical practices maintained through inertia rather than current function. Widow's black, sitting shiva duration, wake attendance expectations, public grief display norms persist partly because 'that's what we do' rather than because they optimally serve grieving or community coordination. The ritual maintains itself through identity fusion and social expectation even as its original coordination function (ensuring community acknowledgment of role redistribution, establishing mourning period boundaries) is increasingly provided by alternative mechanisms.
constraint_indexing:constraint_classification(grief_coordination_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE, IDENTITY-LOCKED) — This perspective instantiates the oracle gap (Theorem 4): the analytical observer's identity as 'detached analyst' is itself constituted through the grief coordination mechanism. The analyst's claim to neutrality depends on suppressing their own grief-stricken identity in order to achieve the observational stance. The analytical position cannot fully see that the constraint binds the observer themselves — the requirement to be analytical is the requirement to NOT be grieving. This constitutes an identity lock at the analytical level: the observer cannot shift into fully authentic grieving without losing analytical standing.
constraint_indexing:constraint_classification(grief_coordination_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grief_coordination_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(grief_coordination_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grief_coordination_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(grief_coordination_mechanism, TR),
    TR >= 0.70.

:- end_tests(grief_coordination_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The grief coordination mechanism provides genuine coordination value — collective acknowledgment, role redistribution, temporal marking — that solves a real problem. However, extractive overlays exist: commercial funeral exploitation, mandatory performance of grief regardless of authentic emotional state, identity locking into prescribed mourning roles. The value of 0.32 reflects that the primary function is coordination (which would be Rope at ~0.15-0.35 ε) but with meaningful extraction from the commercial layer and emotional suppression. Suppression (0.48): Moderate-high. Social expectations strongly constrain grief expression: prescribed timeline (roughly 1 year), required behaviors (funeral attendance, public acknowledgment), prohibited behaviors (moving on too quickly, showing insufficient grief), and identity assignment (becoming the bereaved). These are not absolute barriers but substantial costs to deviation. Theater ratio (0.58): Moderate-high. Viewing, embalming, casket selection, memorial services, and public grief display are substantially performative. These rituals coordinate collective acknowledgment and role redistribution but much of their content is theatrical — the same coordination could occur with lower performative overhead. Theater has increased historically as commercialization added layers (memorial photography, viewing logistics, flower displays) while core coordination functions remain constant.
 *
 * PERSPECTIVAL GAP:
 *   The powerless bereaved individual perceives Snare (unchangeable suppression, identity-locked grief role, no exit) while the institutional ceremonial leader perceives Rope (solving coordination problem, providing service, voluntary participation). The commercial funeral industry perceives itself as beneficiary of Rope (solving emotional need) but the bereaved perceives extraction (Snare). The grief support movement perceives Scaffold (temporary mourning suppression with sunset as therapy literacy rises) while traditional institutions perceive Piton (their own rituals degraded but persisting). The analytical observer risks perceiving Mountain (grief suppression as inherent to human social coordination) while the structural data reveals contingency: suppression is overlay, not inherent. The perspectival gap is thus a diagnostic of how much suppression is coordination-necessary versus extractive-overlay.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality flow: (1) Bereaved individual: victim + identity_locked exit + powerless power → d ≈ 0.90 → f(d) ≈ 1.42 → high χ. Structurally mobile in principle (could stop grieving) but identity-locked (cannot imagine non-grieving self). At biographical time, identity_locked returns Rope (changeable in principle), but from the powerless perspective the binding is so complete it appears as Mountain. (2) Ceremonial institution: beneficiary + arbitrage exit + institutional power → d ≈ 0.15 → f(d) ≈ -0.01 → low/negative χ. They experience the constraint as coordination, not extraction. (3) Commercial funeral industry: beneficiary (extract via theater) + arbitrage exit + powerful power → d ≈ 0.65 → f(d) ≈ 1.00 → moderate χ. They extract but claim coordination role, requiring override or explicit marking as extractive beneficiary. (4) Extended family: both victim and beneficiary (mutual support but labor burden) + constrained exit + moderate power → d ≈ 0.55 → f(d) ≈ 0.75 → moderate χ. (5) Grief support movement: beneficiary (expand their domain, pathologize normal grief) + constrained exit + organized power → d ≈ 0.40 → f(d) ≈ 0.40 → moderate χ. They provide genuine alternative but also compete with traditional institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: Is grief coordination fundamentally a collective good requiring suppression as coordination cost (Rope), or is suppression an extractive overlay that could be removed via alternative pathways (Tangled Rope)? The base properties (ε=0.32, suppression=0.48, requires_active_enforcement, beneficiaries + victims) support Tangled Rope. The critical test: can grief coordination occur with lower suppression and lower theater? Grief support movement (Scaffold) and therapy practices suggest yes — individualized grief processing + community acknowledgment can occur without prescribed timelines and identity locking. If true: suppression is extractive overlay, ε would remain ~0.32-0.40 (coordination cost for role redistribution) but suppression would drop to ~0.15-0.25. If false: suppression is coordination-necessary, and the constraint is Rope (~ε≤0.45, ~suppression≤0.30) without extractive overlay. Current classification (Tangled Rope) is conservative: assumes suppression is partly coordination-cost and partly extractive. The mandatrophy would resolve if grief support movement's claims are validated by 10+ year longitudinal data showing equivalent social reintegration with lower suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentic_emotion_vs_social_requirement,
    'Is suppression of individualized grief expression a necessary feature of grief coordination or a contingent institutional overlay?',
    'Comparative ethnographic analysis of grief expression norms across cultures and historical periods; documentation of cases where grief coordination succeeded without suppressing authentic emotion timelines',
    'If necessary: constraint properly classified as mountain (grief coordination fundamentally requires suppression). If contingent: constraint is Tangled Rope/Snare (suppression is extractive overlay, not coordination cost). This is the core mandatrophy question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_emotion_vs_social_requirement, conceptual, 'Whether emotion suppression is intrinsic to grief coordination or an extractive overlay').

omega_variable(
    identity_lock_mechanism_strength,
    'How much of the powerless agent''s inability to exit grief constraint is structural (social shunning, material dependence) versus internalized (identity fusion, cognitive capture)?',
    'Post-exit trajectories: bereaved individuals who reject prescribed mourning norms and how they report psychological autonomy, community standing, and functional recovery over 5-10 years',
    'If primarily structural: classify as trapped (material barriers). If primarily internalized: classify as identity_locked (cognitive barriers). This changes chi calculation: identity_locked at biographical horizon returns Rope (changeable in principle), while trapped returns Mountain (unchangeable from within).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    commercial_extraction_necessity,
    'Do costly funeral services (embalming, caskets, viewings) exist because grief coordination genuinely requires them or because the commercial industry manufactured the requirement?',
    'Historical analysis of grief practices pre-professionalization; comparison of outcomes (family recovery, social reintegration) between high-cost and low-cost funeral practices; cross-national data on industry profit margins versus coordination quality metrics',
    'If necessary: extraction is coordination cost, Tangled Rope with 0.40 < ε < 0.60 is correct classification. If manufactured: extraction is pure overhead, approaching Snare (ε > 0.66). This drives mandatrophy toward distinguishing manufactured requirement from genuine coordination cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_extraction_necessity, empirical, 'Whether funeral service costs are coordination-inherent or industry-manufactured').

omega_variable(
    non_linear_grief_viability,
    'Can grief processing follow non-linear, individualized timelines without degrading social coordination, or does linear prescribed mourning genuinely coordinate collective meaning-making?',
    'Longitudinal data from grief counseling outcomes: social reintegration, family cohesion, community restoration, and psychological recovery metrics comparing clients who follow prescribed grief timelines versus those who pursue self-directed processing',
    'If non-linear is viable: scaffold sunset logic is real — alternative pathways (therapy, peer support) genuinely replace ritualized mourning. If linear is necessary: theater is justified as coordination mechanism. This determines whether grief support movement represents genuine exit or merely parallel extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_linear_grief_viability, empirical, 'Whether individualized grief timelines are compatible with social coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grief_coordination_mechanism, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grief_tr_t0, grief_coordination_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(grief_tr_t2, grief_coordination_mechanism, theater_ratio, 2, 0.48).
narrative_ontology:measurement(grief_tr_t4, grief_coordination_mechanism, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(grief_be_t0, grief_coordination_mechanism, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(grief_be_t2, grief_coordination_mechanism, base_extractiveness, 2, 0.25).
narrative_ontology:measurement(grief_be_t4, grief_coordination_mechanism, base_extractiveness, 4, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(grief_coordination_mechanism, attachment_coordination).
narrative_ontology:boltzmann_floor_override(grief_coordination_mechanism, 0.12).
narrative_ontology:affects_constraint(grief_coordination_mechanism, identity_lock_in_relationships).
narrative_ontology:affects_constraint(grief_coordination_mechanism, ceremonial_institutional_inertia).
narrative_ontology:affects_constraint(grief_coordination_mechanism, vulnerability_exploitation_in_markets).

% DUAL FORMULATION NOTE:
% Grief coordination decomposes into two distinct constraints: (1) collective_grief_acknowledgment (ε~0.15, Rope) — pure coordination problem of marking loss and role redistribution; (2) individualized_emotion_suppression_mechanism (ε~0.55, Snare) — extractive overlay that mandates performance of grief in prescribed forms. These are linked: the coordination function is used to justify and enforce the suppression overlay. Separate stories would clarify which is the primary constraint and whether they're coupled or orthogonal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(grief_coordination_mechanism, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
