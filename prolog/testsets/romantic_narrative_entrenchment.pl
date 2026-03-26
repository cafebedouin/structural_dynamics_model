% ============================================================================
% CONSTRAINT STORY: romantic_narrative_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_romantic_narrative_entrenchment, []).

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
 *   constraint_id: romantic_narrative_entrenchment
 *   human_readable: Romantic Narrative Entrenchment in Pair Bonding
 *   domain: interpersonal/relationship_dynamics
 *
 * SUMMARY:
 *   Romantic narrative entrenchment operates as a constraint that fuses one
 *   partner's identity with an idealized relationship story, creating
 *   suppression and extraction mechanisms that persist despite contradicting
 *   evidence and causing demonstrable harm. The constraint exhibits high
 *   extractiveness (0.68) and high suppression (0.72) but moderate theater
 *   (0.58), indicating that while some performative maintenance exists
 *   (telling the love story, performing couple identity), much of the
 *   entrenchment operates through internalized cognitive capture rather than
 *   pure theatrical illusion. The constraint appears as a pure snare (maximum
 *   extraction with minimal coordination) from the identity-locked partner's
 *   perspective and from the trapped children's perspective, as a rope
 *   (genuine coordination with benefits) from the narrative beneficiary's
 *   perspective, as a tangled rope (mixed coordination and extraction) from
 *   therapeutic or conflict-aware observers, as a piton (culturally
 *   maintained ritual with degraded function) from the civilizational
 *   perspective, and as a false mountain (naturalized constraint presented as
 *   psychological law) from the analytical observer. The measurement
 *   trajectory shows increasing extractiveness and theater from relationship
 *   formation through entrenchment, with the steepest acceleration occurring
 *   in years 2-5 as identity fusion deepens. The constraint operates through
 *   intermittent reinforcement cycles
 *   (tension-conflict-reconciliation-calm-renewed bonding) that make escape
 *   progressively harder even as the extraction mechanism becomes more
 *   visible.
 *
 * KEY AGENTS:
 *   - Identity-Locked Partner: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with relationship role; bears extraction through subordination of reality perception to partner's narrative
 *   - Narrative Beneficiary Partner: Primary beneficiary (institutional/arbitrage) — extracts emotional labor, narrative primacy, and partner accommodation; benefits from partner's identity lock; experiences constraint as coordination
 *   - Relational Children: Secondary victims (powerless/trapped) — dependent on the dyadic structure that the narrative sustains; develop attachment patterns and bonding expectations that reproduce the constraint intergenerationally
 *   - Therapeutic/Conflict-Aware Observer: Moderate position (moderate/constrained) — sees both genuine coordination (partnership, support) and asymmetric extraction; constrained by the relationship's coordination benefits that make intervention costly
 *   - Cultural Narrative System: Institutional maintenance (institutional/arbitrage) — benefits from the romantic love narrative's reproduction through literature, film, ritual, and social performance; has zero exit cost from perpetuating the frame
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the risk of naturalizing contingent institutional dynamics (gender roles, economic dependency, cultural narratives) as immutable psychological laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(romantic_narrative_entrenchment, 0.68).
domain_priors:suppression_score(romantic_narrative_entrenchment, 0.72).
domain_priors:theater_ratio(romantic_narrative_entrenchment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(romantic_narrative_entrenchment, extractiveness, 0.68).
narrative_ontology:constraint_metric(romantic_narrative_entrenchment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(romantic_narrative_entrenchment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(romantic_narrative_entrenchment, snare).
narrative_ontology:human_readable(romantic_narrative_entrenchment, "Romantic Narrative Entrenchment in Pair Bonding").
narrative_ontology:topic_domain(romantic_narrative_entrenchment, "interpersonal/relationship_dynamics").

domain_priors:requires_active_enforcement(romantic_narrative_entrenchment).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(romantic_narrative_entrenchment, narrative_beneficiary_partner).
narrative_ontology:constraint_victim(romantic_narrative_entrenchment, entrapped_partner).
narrative_ontology:constraint_victim(romantic_narrative_entrenchment, relationship_authenticity).
narrative_ontology:constraint_victim(romantic_narrative_entrenchment, exit_possibility_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-LOCKED PARTNER (SNARE) — Structurally mobile (could leave: has income, social connections, legal standing) but cannot exercise exit because self-concept is fused with the relationship narrative. Exit would require abandoning the identity ('the devoted partner', 'the one who understands them', 'the person who makes this work'). The binding mechanism is cognitive rather than external. Experiences maximal extraction with suppression arising from internalized framing: believes the relationship defines who they are.
constraint_indexing:constraint_classification(romantic_narrative_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: NARRATIVE BENEFICIARY (ROPE) — The partner who benefits from the romantic narrative (receives primary attention, emotional labor, accommodation of inconsistencies, narrative framing of themselves as central). Experiences the constraint as coordination: telling the shared love story, creating meaning through narrative, maintaining relational identity. Exit is costless or beneficial (arbitrage). Net beneficiary with positive chi.
constraint_indexing:constraint_classification(romantic_narrative_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: CONFLICT-AWARE OBSERVER (TANGLED ROPE) — A third perspective (sometimes internalized by one partner, sometimes an external therapist or trusted friend): sees genuine coordination function (emotional support, partnership, shared meaning-making) alongside asymmetric extraction (one partner subordinates reality perception to the other's narrative). High suppression because the relationship's genuine coordination benefits make exit costly psychologically; asymmetric extraction persists because it's embedded in the coordination.
constraint_indexing:constraint_classification(romantic_narrative_entrenchment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: RELATIONAL CHILDREN (SNARE) — From the perspective of children (biological or relational) embedded in the dynamic, the romantic narrative entrenchment becomes a constraint they cannot exit: they are trapped in the family structure that the narrative sustains. Their exit options are genuinely trapped (dependent on parental resources, social identity formed within the family unit). The narrative shapes their own bonding expectations and attachment patterns. Maximum suppression and extraction because they have no structural exit path and the pattern becomes internalized as normal.
constraint_indexing:constraint_classification(romantic_narrative_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: CULTURAL NARRATIVE SYSTEM (PITON) — From a civilizational perspective, romantic love narratives persist as organizing principles for pair bonding through institutional inertia and cultural theater rather than functional necessity. The 'one true love' narrative, 'soulmate' discourse, and 'complete yourself through another person' framing are maintained through repeated cultural performance (literature, film, ritual) despite contradicting evidence about sustainable bonding. The theater maintains the narrative form; the function has atrophied into performative recitation of love story scripts.
constraint_indexing:constraint_classification(romantic_narrative_entrenchment, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: PSYCHOLOGICAL LIMIT VIEW (FALSE SUMMIT) — A perspective that frames romantic narrative entrenchment as an immutable feature of human psychology: pair bonding inherently requires narrative construction; attachment patterns inevitably create asymmetries; love necessarily involves identity fusion. This naturalizes contingent institutional dynamics as unchangeable laws. However, the structural data contradicts this: the suppression and extraction are institutional products (cultural narratives, gender role enforcement, economic dependency structures), not psychological necessities. The mountain classification is a false summit revealing how psychological reframing naturalizes what is actually a contingent constraint.
constraint_indexing:constraint_classification(romantic_narrative_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(romantic_narrative_entrenchment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(romantic_narrative_entrenchment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(romantic_narrative_entrenchment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(romantic_narrative_entrenchment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(romantic_narrative_entrenchment, TR),
    TR >= 0.70.

:- end_tests(romantic_narrative_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The identity-locked partner sacrifices reality perception (their own needs, contradiction signals, outside information) to maintain the romantic narrative. This is substantial extraction. However, it is not at the maximal 0.90+ level because the narrative beneficiary partner still provides some coordination value (emotional support, shared meaning, stability) — the extraction is embedded within a partially functional relationship, not pure predation. The trajectory over 8 years (0.35 → 0.68) reflects how the initial coordination functions gradually become overshadowed by extraction as identity fusion deepens. Suppression (0.72): High. The identity-locked partner faces multiple suppression mechanisms: (1) internalized belief that the relationship defines their identity, (2) fear of identity dissolution outside the relationship, (3) isolation from information contradicting the narrative, (4) social and economic costs of exit, (5) sunk emotional investment. The suppression is primarily internalized (cognitive), but it operates alongside structural barriers (housing dependency, social network centralization around the partner, career patterns interrupted by relationship demands). Theater (0.58): Moderate. The constraint requires some performative maintenance — telling the shared love story, curating couple identity for social observation, reciting affirmations and romantic commitment. But much of the entrenchment operates silently through internalized suppression rather than visible performance. The theater has increased over the measurement interval as the identity lock deepens and more effort goes into maintaining the narrative against contradicting evidence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a maximal perspectival gap between structural position. The narrative beneficiary sees coordination and genuine relationship value (Rope classification) because they benefit from the constraint without paying its suppression cost. The identity-locked partner sees pure extraction (Snare) because they bear the full suppression cost and receive diminishing coordination value as identity fusion increases. A therapeutic observer or conflict-aware third party sees the tangled structure: genuine coordination (emotional support, partnership) exists alongside asymmetric extraction (subordinated reality perception, accommodated inconsistencies, asymmetric emotional labor). The cultural narrative system sees itself as maintaining a benign coordination function (romantic love as partnership principle) while actually enforcing gender roles and economic dependency patterns — the institutional perspective experiences the constraint as rope while the analytical observer recognizes it as piton (culturally maintained theater with atrophied function). The false mountain perspective naturalizes this entire institutional structure as an immutable feature of human psychology and pair bonding, which the structural data contradicts: the suppression and extraction are products of specific narratives, gender roles, and economic structures, not psychological necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity-locked partner's directionality is high (d ≈ 0.88) because they are structurally a full target of the constraint: their power is effectively powerless (cannot override the partner's narrative framing), their exit is blocked by identity lock (not trapped by external barriers alone, but by fused identity that makes exit unthinkable), and they are declared as victims. The sigmoid f(d) applied to d=0.88 yields f(d) ≈ 1.26, amplifying the constraint's experienced extractiveness. The narrative beneficiary's directionality is low (d ≈ 0.12) because they benefit from the constraint: institutional power, arbitrage-level exit options (they can leave costlessly or beneficially), and beneficiary status. Applied sigmoid f(d=0.12) yields f(d) ≈ -0.01, slightly reducing their experienced extraction or even inverting it to coordination gain. The therapeutic observer's directionality is moderate (d ≈ 0.58) because they see both coordination and extraction but are partially bound by the relationship's coordination value and by their own role in the system. The children's directionality is high (d ≈ 0.92) because they are trapped (genuinely powerless to exit the family structure) and victimized by intergenerational pattern reproduction. The cultural narrative system has directionality near zero (d ≈ 0.05, institutional beneficiary with arbitrage exit) — it can maintain or abandon the romantic love frame with no cost, purely through institutional choice. The analytical observer's directionality is moderate-high (d ≈ 0.71) because they bear the burden of seeing the constraint clearly without ability to unilaterally dismantle it; they are victimized by the awareness of a structural problem embedded in the entire cultural-institutional system.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED — Mandatrophy resolved through verification of three snare gates: (1) Extractiveness ≥ 0.46: measured at 0.68. (2) Suppression ≥ 0.60: measured at 0.72. (3) Effective extraction χ ≥ 0.66: computed from ε=0.68, f(d)=1.26 for the primary victim, σ(local)=0.8 yields χ = 0.68 × 1.26 × 0.8 ≈ 0.68 (exceeds threshold). The perspectival analysis confirms snare: the identity-locked partner experiences maximum extraction with no viable exit path. The narrative beneficiary experiences rope (coordination with benefits, no extraction cost). The therapeutic observer experiences tangled rope (mixed coordination and extraction). The constraint cannot be misclassified as pure coordination (Rope) because the asymmetric extraction is structural and the suppression is severe. It cannot be classified as Tangled Rope at the primary victim perspective because the coordination benefit has been substantially displaced by extraction and identity fusion — the victim sees extraction, not mixed function. The false mountain perspective (psychological necessity) is explicitly identified as naturalization of contingent institutional structures. Mandatrophy fully resolved: the snare classification is structural, perspectival readings are diversified, false summitery is detected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_structural_vs_internalized,
    'Is the identity lock structural (externally imposed role expectations and economic dependency creating real barriers) or internalized (the partner has made exit psychologically impossible by fusing identity with the relationship)?',
    'Post-exit trajectory analysis: if the partner shows rapid identity reconstruction and absence of persistent suppression after leaving, the lock was primarily internalized; if suppression persists (continued self-blame, inability to form other attachments, identity fragmentation), the lock involved deep cognitive capture.',
    'If internalized: the constraint''s true suppression is higher than measured (the target carries internalized barriers with them); exit removes external structures but not internal. If structural: alternative structures (reduced economic dependency, different cultural narratives) could enable exit without identity work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_structural_vs_internalized, empirical, 'Whether identity lock is structural or internalized mechanism').

omega_variable(
    narrative_coordination_genuine_function,
    'Does the romantic narrative actually coordinate genuine partnership benefits (emotional support, resource sharing, attachment security) or is it a cover story for asymmetric extraction?',
    'Comparison of relationship outcomes where the narrative frame is removed: does the partnership''s coordination value persist if partners explicitly reject the ''soulmate'' framing? Measurement of support flow: is emotional labor reciprocal under the narrative, or does it flow asymmetrically?',
    'If genuine coordination: classify as Tangled Rope (mixed coordination and extraction) rather than pure Snare; extraction is contingent on the narrative frame, not inherent to pair bonding. If purely extractive: confirm Snare; the narrative is entirely manipulative cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_coordination_genuine_function, empirical, 'Whether romantic narrative provides genuine coordination or mask for extraction').

omega_variable(
    gender_role_entrenchment_magnitude,
    'How much of the identity lock is specifically enforced by gender role narratives versus generic romantic love narratives?',
    'Comparative analysis: same-gender couples with romantic narrative entrenchment vs different-gender couples; measurement of which partner is identity-locked; assessment of whether the lock correlates with traditionally feminine/masculine role expectations.',
    'If gender-role specific: the constraint is partly a manifestation of patriarchal narrative enforcement; could be partially dissolved through gender equality interventions. If generic to romantic love: the constraint is deeper, requiring narrative reconstruction at the civilizational level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_role_entrenchment_magnitude, empirical, 'Degree to which gender roles enforce the identity lock').

omega_variable(
    cyclic_dynamics_intermittent_reinforcement,
    'Does the romantic narrative operate through intermittent reinforcement cycles (tension-reconciliation-calm-tension) that make the extraction mechanism itself reinforcing?',
    'Temporal pattern analysis of relationship episodes: mapping of conflict-resolution-bonding cycles; measurement of whether ''making up'' moments intensify romantic narrative commitment.',
    'If cyclic: the constraint is self-sustaining through operant conditioning; the reconciliation moments create intermittent reinforcement that strengthens the identity lock. This explains persistent entrenchment despite clear harm. If linear accumulation: extraction increases monotonically, eventually exceeding the relationship''s coordination value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cyclic_dynamics_intermittent_reinforcement, empirical, 'Whether cyclic dynamics with intermittent reinforcement sustain entrenchment').

omega_variable(
    exit_cost_magnitude_estimation,
    'What is the objective cost magnitude of exit (loss of housing, income, social standing, shared custody, identity reconstruction)?',
    'Prospective cost assessment for the identity-locked partner: housing costs, income loss from relationship-dependent work patterns, social isolation from partner-centered social network, identity reconstruction effort, possible custody complications.',
    'If total exit cost exceeds annual household income: the lock may involve genuine economic trapping (constrained or trapped exit options) rather than pure identity lock (identity_locked). This changes the classification and the policy interventions required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_magnitude_estimation, empirical, 'Magnitude of objective exit costs beyond identity work').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(romantic_narrative_entrenchment, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rne_tr_t0, romantic_narrative_entrenchment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rne_tr_t2, romantic_narrative_entrenchment, theater_ratio, 2, 0.48).
narrative_ontology:measurement(rne_tr_t5, romantic_narrative_entrenchment, theater_ratio, 5, 0.55).
narrative_ontology:measurement(rne_tr_t8, romantic_narrative_entrenchment, theater_ratio, 8, 0.58).
narrative_ontology:measurement(rne_tr_t1, romantic_narrative_entrenchment, theater_ratio, 1, 0.44).

% Extraction over time
narrative_ontology:measurement(rne_be_t0, romantic_narrative_entrenchment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rne_be_t2, romantic_narrative_entrenchment, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(rne_be_t5, romantic_narrative_entrenchment, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(rne_be_t8, romantic_narrative_entrenchment, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(rne_be_t1, romantic_narrative_entrenchment, base_extractiveness, 1, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(romantic_narrative_entrenchment, attachment_coordination).
narrative_ontology:boltzmann_floor_override(romantic_narrative_entrenchment, 0.12).
narrative_ontology:affects_constraint(romantic_narrative_entrenchment, gender_role_enforcement_asymmetry).
narrative_ontology:affects_constraint(romantic_narrative_entrenchment, economic_dependency_lock_in_relationships).
narrative_ontology:affects_constraint(romantic_narrative_entrenchment, intergenerational_bonding_pattern_reproduction).

% DUAL FORMULATION NOTE:
% Romantic narrative entrenchment is a parent constraint that decomposes into three structurally distinct sub-constraints: (1) Financial coordination within the partnership (which may be purely Rope or Tangled Rope depending on actual economic distribution), (2) Emotional bonding and attachment security (which may be genuine Rope or extracted Snare depending on reciprocity), (3) Identity coordination through shared narrative (which is primarily Snare due to asymmetric identity fusion). This story captures the primary identity coordination mechanism. The financial and emotional sub-constraints should be written as separate stories with their own ε values and linked via network.affects_constraints. Gender role enforcement and economic dependency are upstream institutional constraints that feed into romantic narrative entrenchment at the institutional level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(romantic_narrative_entrenchment, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
