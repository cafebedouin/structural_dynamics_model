% ============================================================================
% CONSTRAINT STORY: social_narrative_casting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_narrative_casting, []).

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
 *   constraint_id: social_narrative_casting
 *   human_readable: Social Narrative Casting (Criticism-as-Projection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   Social narrative casting occurs when a critic imposes a specific role
 *   (villain, victim, fool, obstacle) onto a subject through repeated
 *   criticism, positioning the criticism as 'truth-telling' while the
 *   mechanism is actually projection of the critic's internal narrative
 *   needs. The constraint exhibits a fundamental asymmetry: the critic
 *   experiences the casting as a coordination function (organizing their own
 *   identity and worldview), while the subject experiences it as extraction
 *   (loss of autonomy, forced role occupancy, reputational damage). The
 *   constraint is enforced through social visibility — the casting is not
 *   imposed unilaterally but spreads through audience adoption of the
 *   critic's narrative frame. The theater ratio rises over time (0.55 → 0.81)
 *   as the casting becomes self-fulfilling: the subject's defensive responses
 *   are reinterpreted as confirmations of the assigned role, and the critic's
 *   original projection becomes invisible beneath accumulated behavioral
 *   evidence that actually reflects the subject's reaction to being cast.
 *   This is a core mechanism of relational harm in communities, workplaces,
 *   and families where narrative authority concentrates in particular
 *   speakers.
 *
 * KEY AGENTS:
 *   - Critic/Director (institutional/arbitrage): Primary beneficiary. Experiences casting as legitimate narrative coherence and identity maintenance. Achieves psychological closure and social authority.
 *   - Cast Subject (powerless/trapped): Primary victim. Cannot exit the public role or deny the casting without appearing defensive. Faces reputational damage, isolation, and identity distortion as social others adopt the critic's frame.
 *   - Audience/Peer Group (moderate/constrained): Secondary actor. Benefits from narrative clarity (reduced social ambiguity) but also victimized if the casting is false or distorts group understanding. Constrained by conformity pressure and narrative persuasiveness.
 *   - Counter-Narrative Communities (organized/constrained): Secondary actor. Therapists, friends, advocates who explicitly reject the casting and offer alternative frameworks. Possess agency and organized resources to reframe the narrative.
 *   - Psychological Essentialism Culture (institutional/arbitrage): Systemic actor. The cultural belief that criticism reveals 'true character' legitimizes the casting and provides theater for the enforcement mechanism.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective that risks naturalizing the constraint as inherent to human perception rather than contingent on social norms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_narrative_casting, 0.52).
domain_priors:suppression_score(social_narrative_casting, 0.68).
domain_priors:theater_ratio(social_narrative_casting, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_narrative_casting, extractiveness, 0.52).
narrative_ontology:constraint_metric(social_narrative_casting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(social_narrative_casting, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_narrative_casting, tangled_rope).
narrative_ontology:human_readable(social_narrative_casting, "Social Narrative Casting (Criticism-as-Projection)").
narrative_ontology:topic_domain(social_narrative_casting, "social/psychological").

domain_priors:requires_active_enforcement(social_narrative_casting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_narrative_casting, critic_narrative_closure).
narrative_ontology:constraint_beneficiary(social_narrative_casting, critic_identity_maintenance).
narrative_ontology:constraint_victim(social_narrative_casting, subject_autonomy).
narrative_ontology:constraint_victim(social_narrative_casting, social_group_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CAST SUBJECT (SNARE) — The person criticized for occupying a role they did not accept, trapped by social visibility and asymmetric narrative control. Cannot exit the criticism without appearing defensive or accepting the assigned role. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.57. High extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(social_narrative_casting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CRITIC / DIRECTOR (ROPE) — Experiences the casting as coordination of their own narrative coherence and identity maintenance. Criticism performs the functional role of organizing their internal story. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary; sees casting as legitimate meaning-making.
constraint_indexing:constraint_classification(social_narrative_casting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: AUDIENCE / PEER GROUP (TANGLED ROPE) — Constrained by social conformity pressure and narrative persuasiveness; benefits from reduced social ambiguity (the casting resolves uncertainty about the subject's role). Also extraction victims if the casting spreads misinformation or social exclusion. d≈0.60, f(d)≈0.78, σ=0.8 → χ≈0.33. Mixed coordination (clarity) and extraction (suppression of alternative narratives).
constraint_indexing:constraint_classification(social_narrative_casting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: COUNTER-NARRATIVE COMMUNITIES (SCAFFOLD) — Organized agents (friends, therapists, advocacy groups) who explicitly reject the critic's casting and offer alternative frameworks have a sunset logic: as the subject develops self-advocacy and the counter-narrative spreads, the critic's casting loses social force. d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.23. Low effective extraction because organized actors see an exit path (narrative reframing, community building).
constraint_indexing:constraint_classification(social_narrative_casting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PSYCHOLOGICAL TRAIT ESSENTIALISM (PITON) — The cultural narrative that criticism reveals 'true character' is largely performative: trait stability is much weaker than folk psychology assumes, and criticism often projects the critic's unresolved conflicts onto the subject. The essentialism persists through institutional inertia (personality psychology, HR evaluations, gossip norms) despite low predictive validity. theater_ratio≈0.81 satisfies the piton gate (≥0.70). The performative maintenance of 'this criticism is accurate' masks the actual mechanism (projection and narrative closure).
constraint_indexing:constraint_classification(social_narrative_casting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PROJECTION LAW (MOUNTAIN) — From a universal/civilizational perspective, humans are narrative-constructing agents who interpret external behavior through internal template-matching (object relations theory, social cognition). This creates an irreducible gap between the critic's internal casting and the subject's actual behavior — the gap is inherent to human perception, not a choice or policy. However, the structural data (ε=0.52, suppression=0.68, theater=0.81) contradicts the mountain classification. The engine will compute this as a false summit, revealing that while perception always involves interpretation, the degree of coercion and suppression is not inevitable but contingent on social norms that could be changed.
constraint_indexing:constraint_classification(social_narrative_casting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_narrative_casting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_narrative_casting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_narrative_casting, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_narrative_casting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_narrative_casting, TR),
    TR >= 0.70.

:- end_tests(social_narrative_casting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The critic extracts psychological benefits (narrative closure, identity security, social authority) from the subject's assignment to a role. The subject pays significant costs: reputational harm, loss of autonomy in how they are perceived, and often involuntary behavioral adaptation to the assigned role. However, extractiveness is not maximal (≥0.66 for snare) because the constraint operates partly through consent and internalization—some subjects internalize the casting, reducing pure coercion. The measurement trajectory (0.32 → 0.52) reflects the self-fulfilling mechanism: initial projection accumulates behavioral evidence, which increases the critic's extraction capacity. Suppression (0.68): High. The mechanisms suppressing alternatives are: (1) narrative authority concentration (the critic's voice carries disproportionate weight), (2) reputational sunk cost (once the casting spreads, the subject faces social friction if they contradict it), (3) confirmation bias (evidence conforming to the casting is remembered and shared; disconfirming evidence is dismissed as the subject 'trying to look good'), and (4) the subject's own psychological defense mechanisms (denial or rage can reinforce the casting as 'unstable behavior'). Theater ratio (0.81): Very high. The performative component has risen substantially as the casting becomes established. The critic now performs narrative authority ('I know who this person really is') rather than grounding criticism in current behavior. The audience performs belief in the narrative even as evidence contradicts it. The subject performs reactions that confirm the assigned role even if those reactions are responses to being cast. The increasing theater reflects that the casting has become decoupled from behavioral observation and now operates as pure social ritual.
 *
 * PERSPECTIVAL GAP:
 *   The most profound perspectival gap exists between the critic and the subject. The critic experiences criticism as coordination of meaning and identity (Rope) — they are solving a genuine epistemic problem (understanding the subject's character, organizing their own narrative). The subject experiences the same speech act as coercive extraction (Snare) — an imposition of a role they did not accept, enforced through social visibility and reputational damage. The audience occupies a middle position: they benefit from the narrative clarity the casting provides (reduced ambiguity), but they also become extraction victims if the casting is false or distorts their relationships with the subject. The counter-narrative communities see a temporary problem with a sunset (Scaffold) — as the subject gains external validation or the counter-narrative spreads, the critic's casting loses social force. The essentialism frame that underpins the whole mechanism (Piton) performs the work of naturalizing contingent social norms as psychological truth. The analytical observer risks the false summit (Mountain) by seeing projection as an inherent feature of human perception rather than a contingent institutional arrangement that could be changed through norms of narrative humility, collaborative understanding, and role acceptance consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Critic: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Their experience of the constraint is as coordination and identity maintenance. Subject: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction and suppression. They cannot exit without accepting the role or appearing defensive. Audience: Mixed position → d≈0.60, f(d)≈0.78. They are both beneficiaries (narrative clarity) and victims (complicit in potential misinformation). Constrained exit because social conformity pressure makes narrative contradiction costly. Counter-narrative communities: Organized + constrained → d≈0.45, f(d)≈0.50. Lower effective extraction because organization and external resources provide alternative narratives and reduce conformity pressure. Essentialism frame: d≈0.05 (institutional arbitrage). Theater gate (0.81 ≥ 0.70) triggers piton classification despite low chi. Analytical observer: d≈0.72, f(d)≈1.15. False summit risk: the observer naturalizes as inherent what is actually contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that the constraint is genuinely a Tangled Rope — it contains both a coordination function (the critic's legitimate need for narrative coherence) and an asymmetric extraction function (the subject's loss of autonomy). The critic's Rope perspective is real: the act of organizing one's understanding of another person is a genuine cognitive need, and criticism can be a legitimate way to communicate patterns. However, the subject's Snare perspective is also real: without mechanisms of narrative consent and collaborative sense-making, that same act becomes coercive extraction. The constraint resolves mandatrophy by distinguishing between criticism-as-coordination (where the subject participates in meaning-making) and criticism-as-casting (where the subject is positioned without consent into a role selected to serve the critic's narrative needs). The presence of counter-narrative communities and the reversibility of castings when external support increases suggest the constraint is not purely structural but contingent on social norms that privilege the critic's narrative authority. This is why the scaffold perspective is real — a world with strong norms of narrative humility and subject consent to characterization would lower both the theater and the suppression significantly, turning the constraint from Tangled Rope toward Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    projection_vs_accuracy_threshold,
    'What proportion of criticism reflects genuine behavioral pattern observation vs. critic''s projected narrative need?',
    'Multi-rater behavioral coding by independent observers; comparison of critic''s description to subject''s self-report and actual behavioral evidence; temporal stability of criticism after subject changes behavior',
    'If projection > 70%: casting is almost pure extraction (Snare from subject perspective). If projection < 30%: casting contains significant coordination function (Rope from critic perspective is justified). If projection 40-60%: true tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(projection_vs_accuracy_threshold, empirical, 'Proportion of criticism reflecting projection vs. genuine observation').

omega_variable(
    narrative_closure_necessity,
    'Does the critic genuinely require this specific casting for identity coherence, or is the casting opportunistic post-hoc rationalization?',
    'Longitudinal analysis of critic''s narrative consistency; whether the same subject receives different castings in different contexts; whether the critic accepts evidence contradicting the casting or doubles down; existence of alternative narratives that would maintain critic identity',
    'If necessary for coherence: extraction is genuine functional extraction (critic extraction as real cost to them of changing narrative). If opportunistic: extraction is pure control and theater with no functional basis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrative_closure_necessity, conceptual, 'Whether the casting is necessary for the critic''s identity coherence').

omega_variable(
    social_reinforcement_loop_reversibility,
    'Once established, can the critic''s casting be linguistically and socially reversed without the subject performing explicitly opposite behavior?',
    'Experimental: public reframing of the narrative by counter-source; measurement of adoption speed; comparison to castings presented without prior narrative anchoring; longitudinal narrative stability after reframing',
    'If irreversible (sticky): suppression is high and structural (Snare confirmed). If reversible: suppression is contingent on social context, not inherent (suggests lower suppression value).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_reinforcement_loop_reversibility, empirical, 'Whether established narrative castings can be reversed without subject behavior change').

omega_variable(
    subject_collusion_detection,
    'Does the subject sometimes unconsciously or consciously adopt the assigned role to reduce cognitive dissonance or social friction?',
    'Analysis of subject''s self-narrative before/after public casting; behavioral changes that align with the casting; subject''s own investment in the narrative (identity fusion); comparison of subject behavior in presence vs. absence of the critic',
    'If significant collusion: constraint becomes consensual (theater increases, extraction decreases). If minimal: constraint remains coercive (suppression remains high).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_collusion_detection, empirical, 'Degree to which subject internalizes and enacts the assigned narrative role').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_narrative_casting, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(snc_tr_t0, social_narrative_casting, theater_ratio, 0, 0.55).
narrative_ontology:measurement(snc_tr_t3, social_narrative_casting, theater_ratio, 3, 0.68).
narrative_ontology:measurement(snc_tr_t6, social_narrative_casting, theater_ratio, 6, 0.81).

% Extraction over time
narrative_ontology:measurement(snc_be_t0, social_narrative_casting, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(snc_be_t3, social_narrative_casting, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(snc_be_t6, social_narrative_casting, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_narrative_casting, information_standard).
narrative_ontology:affects_constraint(social_narrative_casting, relational_trauma_accumulation).
narrative_ontology:affects_constraint(social_narrative_casting, identity_diffusion_under_persistent_misrepresentation).
narrative_ontology:affects_constraint(social_narrative_casting, narrative_authority_concentration).

% DUAL FORMULATION NOTE:
% Social narrative casting is downstream of individual psychological projection mechanisms but represents a distinct structural constraint at the group/social level. The upstream constraint (individual projection) reflects cognitive mechanisms; this constraint reflects how those mechanisms become institutionalized through social reinforcement, narrative authority, and conformity pressure. The ε values differ: individual projection (ε≈0.25) is a natural cognitive pattern; social casting (ε≈0.52) adds extraction through social enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_narrative_casting, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
