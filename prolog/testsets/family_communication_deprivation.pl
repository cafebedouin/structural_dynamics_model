% ============================================================================
% CONSTRAINT STORY: family_communication_deprivation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_communication_deprivation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_communication_deprivation
 *   human_readable: Family Communication Deprivation
 *   domain: interpersonal/family_dynamics
 *
 * SUMMARY:
 *   Family communication deprivation is a structural extraction mechanism
 *   where one or more family members (typically a parent or controlling
 *   spouse) systematically restrict another family member's access to
 *   communication with extended family, friends, or support networks. The
 *   constraint operates through a combination of material barriers
 *   (monitoring devices, control of phones/internet, geographic isolation),
 *   authority structures (parental or spousal control), social pressure
 *   (loyalty narratives, shame about family conflict), and psychological
 *   mechanisms (identity fusion with the controlling relationship,
 *   internalized beliefs about external danger). The extracted resource is
 *   the isolated member's autonomy, information access, and relational
 *   identity — the beneficiary gains control, uncontested framing authority,
 *   and reduced risk of external accountability. The suppression is
 *   exceptionally high (0.68) because the target faces compounding barriers:
 *   structural (may depend on housing/income controlled by the beneficiary),
 *   institutional (schools, therapists, courts often defer to
 *   parental/spousal authority), social (family members may pressure return
 *   or compliance), and cognitive (identity-locked perspective prevents the
 *   target from perceiving exit as possible). The extractiveness shows modest
 *   growth over the measurement interval (0.45 to 0.62), reflecting gradual
 *   intensification as the controlling member encounters resistance or as
 *   communication technology improvements force escalation to maintain
 *   control. Theater ratio remains moderate (0.45) and stable, indicating
 *   that while the constraint is enforced through some performative loyalty
 *   narratives, the extraction mechanism is primarily material and
 *   psychological rather than theatrical cover for institutional dysfunction.
 *
 * KEY AGENTS:
 *   - Isolated Family Member: Primary victim (powerless/trapped at biographical horizon, moderate/identity_locked at generational horizon) — bears the extraction of lost relationships, autonomy, information access
 *   - Controlling Family Member: Primary beneficiary (institutional/arbitrage) — gains uncontested framing authority, control over the target's social world, reduced risk of external accountability or alternative influencers
 *   - Extended Family Network: Secondary victim (moderate/constrained) — denied access to family member, burdened with worry and helplessness, threatened with estrangement if they interfere
 *   - Cultural Privacy Narrative: Institutional actor (institutional/arbitrage via piton degradation) — family privacy norms suppress help-seeking and institutional intervention, enabling the constraint to persist
 *   - Analytical Observer: Universal view (analytical/analytical) — assesses the constraint as pure extraction with no genuine coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_communication_deprivation, 0.62).
domain_priors:suppression_score(family_communication_deprivation, 0.68).
domain_priors:theater_ratio(family_communication_deprivation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_communication_deprivation, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_communication_deprivation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(family_communication_deprivation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_communication_deprivation, snare).
narrative_ontology:human_readable(family_communication_deprivation, "Family Communication Deprivation").
narrative_ontology:topic_domain(family_communication_deprivation, "interpersonal/family_dynamics").

domain_priors:requires_active_enforcement(family_communication_deprivation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_communication_deprivation, controlling_family_member).
narrative_ontology:constraint_victim(family_communication_deprivation, isolated_family_members).
narrative_ontology:constraint_victim(family_communication_deprivation, extended_family_network).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED FAMILY MEMBER (SNARE) — Structurally confined by economic dependency, housing insecurity, or parental authority. Communication with extended family is actively blocked through monitoring, interception, geographic isolation, or social narrative control ('they don't understand you,' 'they'll abandon us'). The constraint has high suppression (0.68) because the target cannot exercise alternatives without losing housing, financial support, or parental recognition. No coordination function exists from this agent's perspective — the deprivation serves only extractive control.
constraint_indexing:constraint_classification(family_communication_deprivation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONTROLLING FAMILY MEMBER (ROPE) — Experiences the constraint as coordination of family loyalty and boundary maintenance. From this perspective, restricting outside contact solves the coordination problem of 'maintaining family cohesion' against presumed external threats. The beneficiary views isolation as protective rather than extractive — a genuine (though misframed) coordination function. Has arbitrage exit: can open communication channels without cost to themselves.
constraint_indexing:constraint_classification(family_communication_deprivation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: EXTENDED FAMILY NETWORK (TANGLED ROPE) — Experiences genuine coordination (maintaining family identity and relationships across distance) alongside extraction (denied access to isolated members, burdened with worry and responsibility). Has constrained exit options: can attempt contact at social cost (family conflict, accusations of interference) but cannot compel communication without risking permanent estrangement. Benefits from family connection but bears the cost of deprivation mechanism.
constraint_indexing:constraint_classification(family_communication_deprivation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ISOLATED MEMBER (IDENTITY-LOCKED) — At biographical horizon, the target perceives the constraint as unchangeable (mountain). But at generational horizon, the target has structural mobility (could contact extended family, could leave the household) yet cannot exercise it because their identity is fused with the controlling relationship. The binding mechanism is cognitive: the target has internalized the narrative that extended family relationships are dangerous, that loyalty requires isolation, or that their identity depends on the controlling member's approval. This perspective reveals the identity-lock mechanism — the agent is not trapped by material barriers alone but by a cognitive frame that makes exit unthinkable from within.
constraint_indexing:constraint_classification(family_communication_deprivation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 5: CULTURAL NARRATIVE (PITON) — Family privacy narratives ('what happens in the family stays in the family,' 'don't air dirty laundry') function as institutional cover for the deprivation constraint. The narrative persists through inertia even as its original protective function has atrophied. Modern communication technology (phones, social media, messaging) has made total isolation technically difficult to enforce, yet the cultural narrative maintaining family privacy boundaries continues to suppress help-seeking and disclosure. Theater ratio (0.45) reflects that the performative aspect — maintaining the family's public image as harmonious — is moderately high but not dominant. The piton persists because institutional structures (family court deference to parental authority, therapist-client confidentiality that protects controlling family members, school systems that avoid intervention) actively reinforce it.
constraint_indexing:constraint_classification(family_communication_deprivation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational, universal scope, family communication deprivation is classified as pure extraction with high suppression. The constraint has no genuine coordination function that requires isolation — family cohesion, loyalty, and identity all function better with communication access, not worse. The 'protective' framing offered by the beneficiary is a false coordination justification. The barrier to exit is both material (economic dependency, authority imbalance) and psychological (identity fusion in the identity-locked perspective). High suppression reflects that the target has multiple layers of barriers: structural (housing/financial), social (family pressure), institutional (legal/therapeutic blind spots), and cognitive (internalized narrative). The analytical classification assigns this unambiguously to snare.
constraint_indexing:constraint_classification(family_communication_deprivation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_communication_deprivation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_communication_deprivation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_communication_deprivation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_communication_deprivation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_communication_deprivation, TR),
    TR >= 0.70.

:- end_tests(family_communication_deprivation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The controlling family member captures significant resources: behavioral compliance, uncontested authority over the target's social world, reduced threat of external accountability, and psychological control yielding deference and guilt. This is not modest extraction — it is substantial control over another person's relational life. The measurement growth from 0.45 to 0.62 reflects accumulation: as resistance emerges, the controlling member escalates enforcement mechanisms, increasing the extraction intensity. Suppression (0.68): Very high. The isolated member faces overlapping barriers: (1) Material — dependent on the controlling member for housing, food, money; (2) Structural — lacking independent transportation, communication devices, or privacy; (3) Authority — subject to parental/spousal legal authority in many jurisdictions; (4) Institutional — schools/therapists/police often default to family authority rather than intervening; (5) Social — extended family may pressure compliance or avoid interference due to family loyalty norms; (6) Cognitive — identity fused with the controlling relationship such that exit feels like self-annihilation. Theater ratio (0.45): Moderate, stable. The constraint does not depend primarily on performative loyalty narratives. The controlling member may claim to be 'protecting' or 'preserving family unity,' but these are secondary justifications — the actual mechanism is behavioral control, not narrative maintenance. The stability across time suggests that theater is not escalating even as extractiveness grows, indicating that the controlling member is tightening material enforcement rather than narrative manipulation.
 *
 * PERSPECTIVAL GAP:
 *   The controlling beneficiary sees rope — a coordination mechanism solving the 'problem' of external interference and family loyalty. The isolated victim sees snare — pure extraction with no coordination function. The extended family sees tangled rope — genuine family connection alongside extraction via deprivation. The identity-locked isolated member sees an immutable constraint at biographical horizon (mountain-like) but reveals structural mobility at generational horizon — the gap indicates cognitive capture rather than material impossibility. The cultural narrative perpetuates piton status through institutional inertia: family privacy norms suppress intervention even as modern technology makes total isolation technically difficult. The analytical observer, viewing from global/civilizational scope, classifies unambiguously as snare with false coordination framing — there is no genuine coordination function that requires isolation; family cohesion improves with communication access.
 *
 * DIRECTIONALITY LOGIC:
 *   The controlling family member is the clear beneficiary. They derive d ≈ 0.15 (beneficiary status + arbitrage exit = low directionality). The isolated member is the clear victim. They derive d ≈ 0.90+ (victim status + trapped exit = high directionality, maximum extraction from their perspective). The extended family experiences mixed directionality: they benefit from family connection (if they could access it) but bear the cost of deprivation and separation (constrained exit, d ≈ 0.65). The identity-locked perspective on the isolated member shows a perspectival gap: at biographical time horizon, the trapped exit produces d ≈ 0.95 (mountain-like immutability perception), but at generational time horizon with identity_locked exit, the agent has structural mobility (could contact family, could leave) yet cannot exercise it cognitively, producing d ≈ 0.80 but with a different mechanism. This gap reveals that the binding is cognitive rather than structural — the target is not actually trapped but perceives themselves as trapped because their identity is constituted through the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the identity-locked perspective: The isolated member at biographical time horizon perceives the constraint as immutable (mountain-like perception because they are identity-locked), but at generational time horizon with expanded perspective, they have structural options that their identity frame prevents them from exercising. This is not a false summit in the mountain sense — the constraint is genuinely a snare from the analytical perspective. Rather, the biographical immutability perception reveals a cognitive capture mechanism layered on top of structural barriers. The mandatrophy warns against the controlling beneficiary's coordinate rope classification — communication deprivation does not solve any coordination problem that requires isolation. Family cohesion, loyalty, and identity all function better with communication access. The 'protective' coordination framing is a false natural law — it is a contingent institutional claim that confuses the beneficiary's interest in control with a genuine coordination function. The snare classification is correct and unambiguous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'What proportion of the measured suppression (0.68) is structural (external barriers) versus internalized (cognitive patterns that persist after barrier removal)?',
    'Post-separation trajectory analysis: Do isolated individuals who successfully exit the household continue to avoid family contact? If yes, suppression has significant internalized component. Interview data on reported barriers before vs after exit.',
    'If mostly structural (>70%): constraint is primarily a housing/economic extraction mechanism. If mostly internalized (<50%): identity lock is the primary binding mechanism; exit requires cognitive reframing, not just material change. Mixed cases require decomposition into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Proportion of suppression that is structural vs internalized').

omega_variable(
    coordination_function_authenticity,
    'Does family isolation actually serve any genuine coordination function (maintaining cohesion, preventing harm, preserving family identity), or is the coordination framing purely post-hoc justification?',
    'Comparative analysis: families with communication access vs families with deprivation; measurement of cohesion, loyalty, identity persistence across communication access levels. Historical analysis of the controlling member''s stated rationale vs actual outcomes.',
    'If coordination function is genuine: classify as Tangled Rope (mixed coordination + extraction). If purely extractive: classification remains Snare. This determines whether the constraint can be reformed through better coordination mechanisms or requires full exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_authenticity, conceptual, 'Whether isolation serves genuine family coordination function').

omega_variable(
    exit_option_reality,
    'For an isolated family member in a typical scenario (no legal independence, limited financial resources, social dependence), what is the actual exit cost? Can they realistically contact extended family or exit without losing housing/support?',
    'Case study analysis of successful vs unsuccessful exit attempts; measurement of actual costs (housing instability, income loss, social support disruption) vs perceived costs from within the constraint.',
    'If exit is materially impossible or catastrophic: exit_options should be ''trapped'' not ''identity_locked''. If exit is possible but psychologically blocked: identity_locked classification is correct. If exit is genuinely possible with low cost: exit_options should be ''constrained'' and classification should shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_reality, empirical, 'Actual vs perceived exit feasibility for isolated members').

omega_variable(
    identity_lock_mechanism_specificity,
    'What specific identity-fusion content binds the isolated member? Is the binding through loyalty identity (''being a good family member requires isolation''), role identity (''I am the caregiver/protector''), relational identity (''my worth depends on this person''s approval''), or ideological identity (''outsiders are dangerous'')?',
    'Narrative analysis of the target''s self-descriptions; longitudinal tracking of identity content before, during, and after exit attempts; therapeutic intervention outcomes showing whether identity reframing precedes or follows material exit.',
    'Different identity-fusion mechanisms require different intervention approaches. Loyalty-identity requires challenging the redefinition of loyalty. Role-identity requires role diversification. Relational-identity requires alternative relationships. Ideological-identity requires reality-testing of threat narratives. Specificity improves intervention precision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_specificity, conceptual, 'Specific content and mechanism of identity-lock binding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_communication_deprivation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcd_tr_t0, family_communication_deprivation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fcd_tr_t3, family_communication_deprivation, theater_ratio, 3, 0.41).
narrative_ontology:measurement(fcd_tr_t6, family_communication_deprivation, theater_ratio, 6, 0.44).
narrative_ontology:measurement(fcd_tr_t9, family_communication_deprivation, theater_ratio, 9, 0.45).

% Extraction over time
narrative_ontology:measurement(fcd_be_t0, family_communication_deprivation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fcd_be_t3, family_communication_deprivation, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(fcd_be_t6, family_communication_deprivation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(fcd_be_t9, family_communication_deprivation, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_communication_deprivation, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_communication_deprivation, 0.12).
narrative_ontology:affects_constraint(family_communication_deprivation, intimate_partner_violence_economic_control).
narrative_ontology:affects_constraint(family_communication_deprivation, parental_authority_enforcement).

% DUAL FORMULATION NOTE:
% Family communication deprivation often co-occurs with economic control, child abuse, or intimate partner violence as mechanisms within a larger coercive relationship. The communication constraint is structurally distinct (ε=0.62, Snare) from the economic extraction (typically ε>0.70, Snare) or physical abuse (typically mountain from victim's immediate perspective, snare from analytical), but these constraints reinforce each other in practice. Decomposition enables separate analysis of each mechanism's lifecycle and intervention points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
