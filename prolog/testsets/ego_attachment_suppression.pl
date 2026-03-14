% ============================================================================
% CONSTRAINT STORY: ego_attachment_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ego_attachment_suppression, []).

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
 *   constraint_id: ego_attachment_suppression
 *   human_readable: Ego Attachment and Self-Suppression Constraint
 *   domain: psychology/interpersonal/identity
 *
 * SUMMARY:
 *   Ego attachment and self-suppression describes the mechanism by which
 *   individuals construct and maintain a defensible self-image through
 *   filtering, editing, and selective expression of authentic impulses. The
 *   constraint operates through internalized threat detection: the ego
 *   perceives authentic self-expression as threatening to identity coherence,
 *   social standing, or relational safety, and suppresses the authentic
 *   response through shame, anxiety, or fragmentation threat. The constraint
 *   is enforced not by external punishment but by the self-protective
 *   function of the ego itself — the primary beneficiary is the identity
 *   system that maintains coherence through suppression. The primary victim
 *   is the authentic self whose expression is blocked, and secondarily the
 *   relational circle that receives a filtered version of the agent rather
 *   than authentic reciprocity. The constraint exhibits all six DR types
 *   depending on perspective: the authentic self experiences it as a snare
 *   (identity-locked, no exit), the ego-identity system experiences it as
 *   pure coordination (rope), close relationships experience it as mixed
 *   (tangled rope), degraded cultural norms around emotional restraint appear
 *   as piton (performative, theater-maintained), a civilizational view risks
 *   naturalizing it as inherent to consciousness (mountain), and therapeutic
 *   interventions provide scaffolded pathways to reduced suppression with a
 *   sunset as internalized safety increases (scaffold). The extraction
 *   mechanism is temporal: early suppression is adaptive (coordination
 *   function dominates), but as the constraint persists, the theater
 *   component rises — authenticity is suppressed not because it actually
 *   threatens current relationships but because the suppression itself has
 *   become the identity, and the ego has internalized the threat. The
 *   measurements show increasing extractiveness over time as suppression
 *   calcifies from a protective response into a characterological pattern,
 *   with rising theater as the functional coordination benefit decays.
 *
 * KEY AGENTS:
 *   - Authentic Self: Primary victim (powerless/identity_locked) — can express without external barriers but cannot do so without ego-threat response; structurally mobile but experientially trapped
 *   - Ego-Identity System: Primary beneficiary (institutional/arbitrage) — maintains coherence and defensibility through selective expression; experiences suppression as coordination, not extraction
 *   - Close Relational Circle: Secondary victim (moderate/constrained) — benefits from relationship stability but bears cost of receiving filtered versions of the agent; exit costs are high (relationship dissolution) but possible
 *   - Cultural Norm Institutions: Institutional actor (powerful/arbitrage) — originally encoded adaptive self-regulation; now maintained through inertia and performative compliance; high theater_ratio indicates degradation
 *   - Therapeutic/Relational Deconditioning Movements: Organized agents (organized/constrained) — provide scaffolded pathways to increased authenticity; sunset occurs as internalized safety increases and protective suppression declines
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent arrangement as inherent to consciousness or human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ego_attachment_suppression, 0.68).
domain_priors:suppression_score(ego_attachment_suppression, 0.75).
domain_priors:theater_ratio(ego_attachment_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ego_attachment_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(ego_attachment_suppression, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ego_attachment_suppression, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ego_attachment_suppression, snare).
narrative_ontology:human_readable(ego_attachment_suppression, "Ego Attachment and Self-Suppression Constraint").
narrative_ontology:topic_domain(ego_attachment_suppression, "psychology/interpersonal/identity").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ego_attachment_suppression, identity_protectors).
narrative_ontology:constraint_beneficiary(ego_attachment_suppression, ego_maintenance_systems).
narrative_ontology:constraint_victim(ego_attachment_suppression, authentic_self_expression).
narrative_ontology:constraint_victim(ego_attachment_suppression, growth_capacity).
narrative_ontology:constraint_victim(ego_attachment_suppression, relational_reciprocity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AUTHENTIC SELF (SNARE) — Identity-locked in ego-constructed self-image. Cannot express authentic impulses without triggering ego-defense mechanisms (shame, anxiety, identity fragmentation). The authentic self is trapped not by external barriers but by internalized threat detection. Self-suppression is experienced as necessary self-protection, not as extraction, which makes escape psychologically inconceivable from within the ego frame.
constraint_indexing:constraint_classification(ego_attachment_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CLOSE RELATIONAL CIRCLE (TANGLED ROPE) — Family, intimate partners, long-term friends. These agents experience mixed coordination and extraction. The suppression benefits relationship stability and reduces interpersonal friction (genuine coordination). But the suppression also extracts cost: they receive filtered, controlled versions of the other agent rather than authentic relating. Exit costs are high (relationship dissolution, identity dissolution for enmeshed partners), but exit is structurally possible.
constraint_indexing:constraint_classification(ego_attachment_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EGO-IDENTITY SYSTEM (ROPE) — The ego's self-protective structures benefit from suppression: they maintain a coherent, defensible self-image through filtering out authenticity that threatens the constructed identity. This is pure coordination from the ego's perspective — managing threat through selective expression. The ego experiences the suppression as necessary, not extractive. Low effective extraction from this position because the constraint serves the ego's core function.
constraint_indexing:constraint_classification(ego_attachment_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: CULTURAL NORM INSTITUTIONS (PITON) — Social structures, professional codes, gender norms, class expectations that originally encoded adaptive suppression (self-regulation, delayed gratification, social coordination) have degraded into performative compliance. The norms persist through institutional inertia despite low functional value — people suppress authenticity not because it enables coordination but because the norm is internalized and theater-maintained (reputation, status, belonging). Piton classification reflects high theater_ratio despite institutional power.
constraint_indexing:constraint_classification(ego_attachment_suppression, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, ego attachment and self-suppression appear as immutable features of consciousness itself: the ego is the self-protective system that all human minds construct; some degree of self-editing is intrinsic to social functioning; the gap between inner experience and outer presentation is a structural feature of language and social life. This perspective risks naturalizing what the structural data suggests is a contingent, extractive arrangement that varies dramatically across psychological health, cultural context, and relational safety.
constraint_indexing:constraint_classification(ego_attachment_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THERAPEUTIC AND DECONDITIONING MOVEMENTS (SCAFFOLD) — Psychotherapy, somatic practices, non-violent communication, attachment-informed relating, and psychological flexibility interventions provide structured pathways to reduced suppression. These represent temporary support systems (scaffolds) that enable the authentic self to be activated and integrated, with a sunset as internalized safety increases and protective suppression decreases. High suppression is tolerated only during the relearning phase; it declines as new relational patterns become safe.
constraint_indexing:constraint_classification(ego_attachment_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ego_attachment_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ego_attachment_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ego_attachment_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ego_attachment_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ego_attachment_suppression, TR),
    TR >= 0.70.

:- end_tests(ego_attachment_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significant subjective authenticity and relational reciprocity. The authentic self cannot express without triggering identity threat; the relational circle receives filtered versions rather than authentic presence; growth and vulnerability are suppressed. The extractiveness is not as extreme as pure predatory extraction (0.85+) because some of the suppression serves genuine coordination functions (managing social friction, enabling group functioning). The measurement trajectory shows increasing extractiveness over time as the protective suppression hardens into characterological pattern — early suppression (0.42) is more adaptive; late suppression (0.72) is more extractive theater. Suppression (0.75): Very high. The primary suppression mechanism is internalized: the ego uses threat detection, shame, and identity fragmentation anxiety to enforce suppression. The agent cannot simply choose to stop suppressing — doing so triggers acute distress (deactivation of the identity) and perceived relational danger (real or imagined). Secondary suppression comes from cultural norms that reinforce emotional restraint and social conformity. Alternatives to suppressed self-expression are deliberately filtered out (cognitive suppression of other identity possibilities). Theater ratio (0.58): Moderate-high and rising. Early suppression (0.35) has genuine protective function — managing real interpersonal friction and enabling social coordination. As the constraint persists, theater increases (0.63) — suppression continues not because it solves coordination problems but because the suppression itself has become identity-protective. The rise in theater from 0.35 to 0.63 indicates degradation: functional suppression is being replaced by performative suppression. At 0.58 overall, the theater is substantial but not fully piton-level — the coordination function hasn't completely atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap reveals the depth of the constraint. From the ego-identity system's position (institutional/arbitrage/rope), suppression is coordination — the ego solves the problem of maintaining a coherent self in a threatening world. From the authentic self's position (powerless/identity_locked/snare), suppression is extraction — authenticity is captured and held to maintain the constructed identity. The relational circle sees tangled rope — they experience both the coordination benefit (reduced interpersonal friction) and the extraction cost (filtered relating). Cultural institutions see piton (performative maintenance of emotional norms that have lost functional value). Therapeutic movements see scaffold (temporary support for authentic reactivation, with sunset as safety increases). The civilizational observer risks seeing mountain (natural law of consciousness and social life) but the structural data contradicts this: suppression levels vary dramatically across cultures, psychological development stages, and relational safety contexts. The falseness of the mountain classification reveals that the constraint is extractive precisely to the degree that it naturalizes itself as inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position. The authentic self is a victim (high d ≈ 0.88) — the suppression extracts its expression and traps it within the ego's constraints. The ego-identity system is a beneficiary (low d ≈ 0.12) — it receives the extracted authenticity and uses it to maintain identity coherence. The relational circle has mixed position (d ≈ 0.52) — they are both beneficiaries (receive coordination benefit of reduced friction) and victims (receive filtered relating). Their exit options (constrained) reflect that leaving the relationship is possible but costly. Cultural institutions have low d (≈ 0.15) as beneficiaries of norm-maintenance, but the piton classification indicates that the suppression they enforce has lost coordination function and persists through inertia. Therapeutic movements have moderate-high d (≈ 0.58) as victims of the suppression from a prospective view — they are working to dissolve the constraint — but also as partial beneficiaries in that therapeutic identity can become identity-locked too (replacing one form of suppression with another). The analytical observer at civilizational scope has high d (≈ 0.73) because the constraint affects the observer's own capacity to see outside the naturalizing frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the temptation to classify this as a mountain (natural law of consciousness) or a rope (necessary coordination mechanism for social life). The structural data reveals snare characteristics from the victim's perspective and piton characteristics from the institutional perspective. The analytical observer must resist the mountain classification by noting that suppression levels are dramatically variable across cultures, psychological health, and relational safety — if it were natural law, we would expect invariance. The snare classification is justified: the authentic self is trapped not by external enforcement but by identity-lock (making exit unthinkable) and by suppression mechanisms (shame, fragmentation anxiety) that function as internalized coercion. The victim cannot exit because exiting would require abandoning the identity that the suppression protects. This is the structural definition of a snare. The tangled rope classification for relational circles is justified because genuine coordination (managing friction, enabling group function) coexists with real extraction (receiving filtered relating, bearing the cost of the other agent's inauthenticity). The suppression does solve a collective action problem — pure authenticity all the time would be socially disruptive — but it also extracts relational reciprocity and genuine presence. The piton classification for cultural institutions reflects that the norms once served genuine functions (teaching impulse control, social consideration) but now persist through theater and inertia. The scaffold classification for therapeutic movements reflects that they provide temporary support structures enabling the authentic self to emerge, with a natural sunset as internalized safety increases and protective suppression declines. No single classification is 'correct' — the presheaf over positions is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_defense_mechanism,
    'Is the observed suppression fundamentally a cognitive identity lock (self-concept depends on the suppression) or a dynamic defense mechanism (protective response that could shift if threat perception changes)?',
    'Longitudinal psychotherapy outcomes; comparison of cognitive restructuring success vs somatic/relational interventions; observation of suppression persistence after threat removal vs rapid adaptation when safety is established',
    'If identity lock dominates: constraint classifies as mountain from identity-locked perspective and rope from therapeutic perspective — large perspectival gap reveals cognitive binding. If defense mechanism dominates: suppression shows rapid decline when safety increases — scaffold perspective becomes dominant and sunset becomes structural feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_defense_mechanism, empirical, 'Whether suppression is identity lock or adaptive defense mechanism').

omega_variable(
    relational_safety_threshold,
    'What level of relational safety (predictability, responsiveness, non-judgment) is sufficient to activate authentic self-expression without threat response?',
    'Experimental observation in therapeutic relationships with graduated safety; measurement of suppression reduction as safety variables increase; identification of threshold beyond which authenticity emerges without coaching',
    'If threshold is low: many relationships could support authenticity but don''t — suppression is over-protective (snare mechanism dominates). If threshold is high: most ordinary relationships provide insufficient safety — suppression is adaptive (rope mechanism dominates). Threshold location determines whether constraint should be treated as extractive pathology or developmental stage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_safety_threshold, empirical, 'Relational safety threshold for authentic self-expression').

omega_variable(
    authentic_expression_cost_real_vs_imagined,
    'To what extent are the costs of authentic expression (shame, rejection, relationship dissolution, identity fragmentation) real structural outcomes vs imagined threats that drive suppression without corresponding external consequences?',
    'Comparison of imagined costs (what the ego predicts) vs actual outcomes when authenticity is gradually increased in established relationships; measurement of cascade vs contained impact; tracking of relationships that survive vs dissolve with increased authenticity',
    'If mostly imagined: suppression persists due to threat simulation rather than real danger — high separability between structural barriers (low) and experienced barriers (high). If mostly real: suppression is adaptive — the constraint protects against genuine costs. This determines whether the snare classification reflects actual extraction or false imprisonment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_expression_cost_real_vs_imagined, empirical, 'Real vs imagined costs of authentic self-expression').

omega_variable(
    cultural_variation_in_suppression_baseline,
    'How much of the observed suppression is culturally variable (norms around emotional expression, self-disclosure, emotional restraint differ across societies) vs intrinsic to ego psychology?',
    'Cross-cultural comparison of suppression levels, authenticity markers, identity flexibility, and relational reciprocity across low-context and high-context cultures; observation of immigrant populations shifting baseline as cultural context changes; intergenerational analysis within immigrant families',
    'If highly variable: the mountain perspective (natural law) is revealed as culturalization of a contingent norm. The constraint becomes a tangled_rope (coordination + extraction) rather than snare. If intrinsic: some baseline suppression is truly inherent to consciousness — the mountain perspective captures a real structural limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_variation_in_suppression_baseline, empirical, 'Cultural variability in suppression baseline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ego_attachment_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ego_attach_tr_t0, ego_attachment_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ego_attach_tr_t3, ego_attachment_suppression, theater_ratio, 3, 0.48).
narrative_ontology:measurement(ego_attach_tr_t6, ego_attachment_suppression, theater_ratio, 6, 0.58).
narrative_ontology:measurement(ego_attach_tr_t9, ego_attachment_suppression, theater_ratio, 9, 0.63).

% Extraction over time
narrative_ontology:measurement(ego_attach_be_t0, ego_attachment_suppression, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ego_attach_be_t3, ego_attachment_suppression, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(ego_attach_be_t6, ego_attachment_suppression, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(ego_attach_be_t9, ego_attachment_suppression, base_extractiveness, 9, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ego_attachment_suppression, identity_coordination).
narrative_ontology:boltzmann_floor_override(ego_attachment_suppression, 0.12).
narrative_ontology:affects_constraint(ego_attachment_suppression, relational_authenticity_deficit).
narrative_ontology:affects_constraint(ego_attachment_suppression, identity_fragmentation_anxiety).
narrative_ontology:affects_constraint(ego_attachment_suppression, social_performance_extraction).

% DUAL FORMULATION NOTE:
% Ego attachment and self-suppression decomposes into three constraint stories with different ε values: (1) identity_coordination suppression (ε=0.42, early stage, rope-dominant) coordinates authentic self across multiple contexts; (2) defensive suppression (ε=0.68, mid-stage, snare-dominant) extracts authenticity through threat simulation; (3) characterological suppression (ε=0.82, late stage, piton-dominant) maintains identity through performative emotional restraint. The three are linked by temporal progression and intervention pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ego_attachment_suppression, institutional, 0.15).
constraint_indexing:directionality_override(ego_attachment_suppression, analytical, 0.73).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
