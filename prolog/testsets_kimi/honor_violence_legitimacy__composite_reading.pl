% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy â Composite Decline Reading
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models the European aristocratic and military honor
 *   violence legitimacy system (dueling) under the composite reading of its
 *   decline: both external cost escalation (drop) and conceptual redefinition
 *   of honor to exclude violence (contraction) operated simultaneously, with
 *   overlapping but distinct victim sets. The constraint coordinated
 *   aristocratic status reproduction while extracting bodily risk, legal
 *   jeopardy, and material cost from junior and subordinate participants. The
 *   composite reading holds that contraction altered the cost-structure of
 *   drop, making single-mechanism explanations insufficient. The kernel is
 *   honor_violence_legitimacy; this is the composite reading, distinct from
 *   drop-only and contraction-only sibling readings.
 *
 * KEY AGENTS:
 *   - aristocratic_elite: Primary agenda-setter and beneficiary (powerful/mobile) â controls honor code and can transition to non-violent honor forms
 *   - junior_officers: Primary target (moderate/identity_locked) â bears bodily risk and rising external prosecution costs
 *   - traditional_honor_bearers: Secondary target (moderate/identity_locked) â suffers existential invalidation from conceptual redefinition
 *   - duelist_families: Diffuse target (powerless/trapped) â bears death, injury, and legal fallout without agency
 *   - state_authorities: External observer (institutional/analytical) â imposes external costs through prosecution and monopoly assertion
 *   - bourgeois_reformers: Excluded voice (organized/mobile) â drives conceptual redefinition from outside the honor group
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.3).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.22).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy â Composite Decline Reading").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '198d0b88-a43c-48e3-8865-500b1d39ef49').
narrative_ontology:cs_kernel_codification('198d0b88-a43c-48e3-8865-500b1d39ef49', distributed).
narrative_ontology:cs_authority_grounding('198d0b88-a43c-48e3-8865-500b1d39ef49', practice).
narrative_ontology:cs_interpretation_layer_present('198d0b88-a43c-48e3-8865-500b1d39ef49').
narrative_ontology:cs_reading_relation('198d0b88-a43c-48e3-8865-500b1d39ef49', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('198d0b88-a43c-48e3-8865-500b1d39ef49', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('198d0b88-a43c-48e3-8865-500b1d39ef49', foundational, dual_mechanism_necessity).
narrative_ontology:cs_axiom_status(dual_mechanism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('198d0b88-a43c-48e3-8865-500b1d39ef49', dual_mechanism_necessity, empirically_contingent).
narrative_ontology:cs_axiom('198d0b88-a43c-48e3-8865-500b1d39ef49', foundational, contraction_alters_drop_calculus).
narrative_ontology:cs_axiom_status(contraction_alters_drop_calculus, holdable).
narrative_ontology:cs_axiom_grounding('198d0b88-a43c-48e3-8865-500b1d39ef49', contraction_alters_drop_calculus, empirically_contingent).
narrative_ontology:cs_reference_frame('198d0b88-a43c-48e3-8865-500b1d39ef49', reciprocal_violent_honor_practice).
narrative_ontology:cs_drift_state('198d0b88-a43c-48e3-8865-500b1d39ef49', modern_state_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('198d0b88-a43c-48e3-8865-500b1d39ef49', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, aristocratic_elite).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, junior_officers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, traditional_honor_bearers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, duelist_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the honor code, sets challenges, and adjudicates disputes within the aristocratic and military milieu. Benefits from status reproduction and group boundary maintenance. During the interval, gradually shifts toward non-violent honor concepts as external costs rise, retaining status capital while abandoning bodily risk.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, aristocratic_elite, agenda_setter,
    powerful, generational, mobile, continental).

% Must accept challenges and demonstrate martial honor to maintain regimental and social standing. Bears rising bodily risk and, as the interval progresses, escalating external costs in the form of prosecution, cashiering, and imprisonment. Exit is blocked by professional identity fused to the honor-violence nexus.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, junior_officers, payer,
    moderate, biographical, identity_locked, national).

% Older aristocrats and officers whose self-concept is constitutionally bound to violent honor. As honor is conceptually redefined to exclude violence, they suffer status collapse and existential dislocation without necessarily facing legal penalties. Their honor identity becomes obsolete while still psychologically mandatory.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, traditional_honor_bearers, payer,
    moderate, biographical, identity_locked, national).

% Bear the material and emotional fallout of death, injury, and legal persecution that flows from compelled participation. They do not choose the duel but absorb its consequences through loss of breadwinners, medical debt, and social stigma.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, duelist_families, payer,
    powerless, biographical, trapped, national).

% Assert monopoly on legitimate violence through prosecution, military discipline reform, and legal codification. They impose the external cost mechanism (drop) on honor violence practitioners but stand outside the honor group's internal logic, treating the practice as criminal rather than coordinative.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_authorities, observer,
    institutional, generational, analytical, continental).

% Promote conceptual redefinition of honor toward civic, commercial, and Christian-bourgeois virtues that exclude violence. Structurally excluded from aristocratic honor deliberations until late in the interval, when their framing achieves cultural dominance and contracts the legitimacy space for dueling.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, bourgeois_reformers, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social status, masculine identity, and intra-elite dispute resolution within aristocratic and military society by providing a ritualized, self-help mechanism that binds the honor group together and maintains boundary distinctions against bourgeois and lower-class outsiders.
% TRANSFER_FUNCTION: Moves bodily risk, legal jeopardy, and material cost from senior elite and the honor group generally to junior members, reluctant participants, and their families; simultaneously transfers status capital and group membership validation to those who successfully perform violent honor.
% ABSENT_VOICES: Women, clergy, and bourgeois reformers who bore the indirect costs of honor violence were structurally excluded from the honor group's internal deliberations until the contraction phase; their objections entered the discourse only after the conceptual redefinition of honor had begun to erode the constraint from within.
% DISAPPEARANCE_RATIONALE: If the honor violence legitimacy structure vanished overnight, aristocratic and military social organization would lose a primary status-reproduction mechanism; dispute resolution would shift immediately to legal, commercial, or social channels; the identity-fusion of traditional honor bearers would collapse; and the state's monopoly on violence would consolidate without residual institutional competition.
% FOUNDING_PROBLEM: In a decentralized early modern society with weak formal judicial penetration and fragmented sovereignty, honor violence provided aristocratic and military peers with a self-help mechanism for status maintenance, insult redress, and dispute resolution that did not require recourse to state institutions.
% FOUNDING_PROBLEM_CORROBORATION: State legal historians and bourgeois reformers corroborate that the founding problem of weak formal justice was superseded by state consolidation, military bureaucratization, and legal codification. Aristocratic memorialists partially corroborate the shift but dispute its completeness and timing. No corroboration exists from seats entirely outside the benefiting parties without interest in the transition.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).
:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low at interval end (0.30) because the constraint has lost most coercive force by the interval terminus, though residual enforcement persists in isolated military subcultures. Suppression is low (0.22) because the honor group's internal enforcement machinery has collapsed under state pressure. Theater_ratio is high (0.72) because late-phase honor violence became increasingly performative, ritualized, and hollow as its social function atrophied. Accessibility_collapse is low (0.20) because alternatives (legal recourse, bourgeois civility, professional arbitration) are widely available and culturally legitimate by interval end. Resistance is high (0.82) because state and reformist resistance intensified monotonically. The metrics describe a constraint in terminal decline; the claimed_type (tangled_rope) captures its structural form during the active phase. Divergence between claim and late-phase metrics is expected and diagnostic.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocratic agenda-setter seat, the arrangement is traditional coordination of status and dispute resolution that has become unfortunately costly; from the junior officer and traditionalist payer seats, it is enforced extraction of bodily risk and identity-validity that they cannot refuse; from the state observer seat, it is an illegitimate competitor to the monopoly on violence. The engine computes these divergences from the structural data â the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic elite sit near the beneficiary end: they set the terms, control challenges, and can exit to non-violent honor forms as costs rise. Junior officers and traditional honor bearers sit near the target end: their identity is locked to violent honor, and they bear the rising external costs and existential invalidation of conceptual redefinition. Duelist families are full targets: trapped, powerless, and bearing fallout. State authorities and bourgeois reformers are external observers and excluded parties whose resistance drives decline but who do not participate in the constraint's extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy flags the founding problem (weak state justice) as dead, while the disappearance_verdict is world_rearranges, indicating the arrangement persists beyond its function â consistent with the composite reading that overdetermined decline was required because neither external costs nor conceptual shift alone could dissolve the identity-fusion sustaining the constraint. The dual mechanism resolves the mandatrophy by showing that single-cause exhaustion was insufficient; the constraint required simultaneous conceptual and cost-structure dissolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drop_contraction_interaction_empirical,
    'Did external cost escalation and conceptual redefinition operate as independent additive causes, or did they interact such that contraction changed the cost calculus of drop?',
    'Archival analysis of duelist memoirs, regimental correspondence, and court records to determine whether actors cited conceptual change or only costs in their decisions to abandon honor violence.',
    'If additive, the composite reading weakens toward a drop-plus-contraction overlay; if interactive, the composite reading''s structural claim that contraction made drop insufficient is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_contraction_interaction_empirical, empirical, 'Whether the two decline mechanisms were additive or interactive.').

omega_variable(
    victim_set_demographic_distinctness,
    'Do the victim sets of the drop mechanism (legally prosecuted duelists, cashiered officers) and the contraction mechanism (traditionalists suffering identity invalidation) overlap substantially, or are they demographically distinct?',
    'Prosopographical analysis linking prosecuted duelists to traditionalist aristocratic networks, versus reformist or bourgeois-origin officers who abandoned violent honor early.',
    'If overlap is high, the two mechanisms may be empirically indistinguishable in practice; if distinct, the composite reading''s structural delta is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_demographic_distinctness, empirical, 'Demographic overlap between drop and contraction victim sets.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the persistence of honor violence under decline driven by structural enforcement (seconds, regimental culture, formal challenge codes) or by internalized identity fusion that outlasted structural enforcement?',
    'Post-abolition trajectory analysis: if dueling ceased immediately upon removal of seconds and regimental tolerance, suppression was structural; if individuals continued seeking violent redress privately or in extra-institutional contexts, suppression was partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, and late-phase theater_ratio may underestimate residual coercive force; if structural, the decline was genuinely externally driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in late-phase honor violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hono_tr_t15, honor_violence_legitimacy__composite_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(hono_tr_t45, honor_violence_legitimacy__composite_reading, theater_ratio, 45, 0.46).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__composite_reading, theater_ratio, 60, 0.56).
narrative_ontology:measurement(hono_tr_t75, honor_violence_legitimacy__composite_reading, theater_ratio, 75, 0.65).
narrative_ontology:measurement(hono_tr_t90, honor_violence_legitimacy__composite_reading, theater_ratio, 90, 0.72).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(hono_be_t15, honor_violence_legitimacy__composite_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(hono_be_t45, honor_violence_legitimacy__composite_reading, base_extractiveness, 45, 0.51).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__composite_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(hono_be_t75, honor_violence_legitimacy__composite_reading, base_extractiveness, 75, 0.36).
narrative_ontology:measurement(hono_be_t90, honor_violence_legitimacy__composite_reading, base_extractiveness, 90, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(hono_su_t15, honor_violence_legitimacy__composite_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(hono_su_t45, honor_violence_legitimacy__composite_reading, suppression_requirement, 45, 0.41).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__composite_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(hono_su_t75, honor_violence_legitimacy__composite_reading, suppression_requirement, 75, 0.28).
narrative_ontology:measurement(hono_su_t90, honor_violence_legitimacy__composite_reading, suppression_requirement, 90, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
