% ============================================================================
% CONSTRAINT STORY: ai_dignity_safeguarding__imago_dei_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_dignity_safeguarding__imago_dei_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_dignity_safeguarding__imago_dei_reading
 *   human_readable: AI Dignity Safeguarding (Imago Dei Reading)
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'ai_dignity_safeguarding.' The imago Dei reading grounds human dignity in
 *   the inviolable image of the Triune God, insists that dignity is prior to
 *   any capability and equal in all persons, and requires that AI development
 *   remain strictly subordinate to the human person and that enhancement
 *   technologies be rejected if they transgress human nature. This is a
 *   theological commitment framing that competes with secular autonomy-rights
 *   readings and posthuman-continuity readings in contemporary AI governance.
 *   The constraint is CLAIMED as tangled_rope: it coordinates a shared
 *   commitment (all parties bound to subordination and non-transgression)
 *   while asymmetrically extracting from AI developers and potential
 *   enhancement subjects who must forgo research and choices. The theology
 *   frames it as protection; the excluded readings frame it as foreclosure.
 *   The structural asymmetry is irreducible: different stakeholders and
 *   different readings generate different classifications of the same
 *   technological constraint.
 *
 * KEY AGENTS:
 *   - Theological tradition keepers (churches, academies, faith institutions): Set and enforce the definition of transgression and human nature; their institutional authority is the constraint's foundation.
 *   - Human persons as imago bearers (the beneficiary set qua organized): Benefit from constraint that keeps them at the apex and prevents technocratic reduction.
 *   - AI development institutions: Pay the cost of foregone research directions and subordination requirement.
 *   - Enhancement technology researchers: Excluded from authority; their research agenda is directly blocked.
 *   - Persons potentially enhanced (powerless, identity-locked): Barred from choices they might make; their identity is preserved for them by the constraint.
 *   - Secular autonomy frameworks and posthuman advocacy: Excluded from the theological authority frame; their core premises are treated as incoherent by the imago Dei reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_dignity_safeguarding__imago_dei_reading, 0.45).
domain_priors:suppression_score(ai_dignity_safeguarding__imago_dei_reading, 0.62).
domain_priors:theater_ratio(ai_dignity_safeguarding__imago_dei_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__imago_dei_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__imago_dei_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__imago_dei_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_dignity_safeguarding__imago_dei_reading, tangled_rope).
narrative_ontology:human_readable(ai_dignity_safeguarding__imago_dei_reading, "AI Dignity Safeguarding (Imago Dei Reading)").
narrative_ontology:topic_domain(ai_dignity_safeguarding__imago_dei_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(ai_dignity_safeguarding__imago_dei_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_dignity_safeguarding__imago_dei_reading, '8be6d6f3-c3cf-4771-b282-6888f4ad0eca').
narrative_ontology:cs_kernel_codification('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', formalized).
narrative_ontology:cs_authority_grounding('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', lineage).
narrative_ontology:cs_interpretation_layer_present('8be6d6f3-c3cf-4771-b282-6888f4ad0eca').
narrative_ontology:cs_reading_relation('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', ai_dignity_safeguarding__autonomy_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', ai_dignity_safeguarding__posthuman_continuity_reading, forecloses).
narrative_ontology:cs_axiom('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', foundational, imago_dei_unchanging_anthropology).
narrative_ontology:cs_axiom_status(imago_dei_unchanging_anthropology, holdable).
narrative_ontology:cs_axiom_grounding('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', imago_dei_unchanging_anthropology, theological).
narrative_ontology:cs_axiom('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', foundational, enhancement_as_transgression_thesis).
narrative_ontology:cs_axiom_status(enhancement_as_transgression_thesis, holdable).
narrative_ontology:cs_axiom_grounding('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', enhancement_as_transgression_thesis, deontological).
narrative_ontology:cs_reference_frame('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', imago_dei_dignity_framework).
narrative_ontology:cs_drift_state('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', contemporary_pluralist_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8be6d6f3-c3cf-4771-b282-6888f4ad0eca', '').
narrative_ontology:cs_kernel_id(ai_dignity_safeguarding__imago_dei_reading, ai_dignity_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__imago_dei_reading, human_person_as_imago_dei).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__imago_dei_reading, persons_subjected_to_technocratic_reduction).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__imago_dei_reading, subjects_of_enhancement_transgression).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_dignity_safeguarding__imago_dei_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_dignity_safeguarding__imago_dei_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_dignity_safeguarding__imago_dei_reading_tests).
:- end_tests(ai_dignity_safeguarding__imago_dei_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the subordination requirement limits AI development paths and forecloses enhancement research, but it does not completely suppress technology — AI remains permissible in subordinate roles. The constraint is not pure extraction (it genuinely coordinates around shared dignity commitment) but asymmetrically distributes the cost of coordination. Suppression is higher (0.62) because the constraint actively blocks research pathways, controls access to enhancement, and enforces the boundary of transgression through institutional gatekeeping — the suppressive force is not merely discouragement but institutional rejection. Theater is low-moderate (0.28): the constraint maintains genuine theological content (imago Dei doctrine is not merely performative within faith traditions), but the enforcement machinery increasingly involves technological gates, access control, and definitional authority contests that serve the constraint's persistence rather than deepening the theological commitment. The measurement series shows slow extractiveness growth (technology drift and research pressure mount, forcing stronger enforcement) and suppression growth (the active force required to hold the subordination line increases as enhancement pathways proliferate), with a projected slight pullback at t=40 if alternative readings gain enough authority that the single theological frame fractures. Theater ratio remains stable because the theatrical component (definitional gatekeeping, boundary policing) is constant even as underlying pressure rises.
 *
 * PERSPECTIVAL GAP:
 *   The theological tradition-keeper seat and the AI developer seat experience radically different constraint types. From the tradition-keeper seat, the constraint is rope: it preserves a shared commitment to human dignity and prevents technological drift that would dissolve the anthropological frame. From the AI developer and enhancement researcher seats, the constraint is snare: it blocks exit to other jurisdictions (reputational cost, regulatory reach), suppresses research aggressively, and distributes the subordination cost unidirectionally. The analytical seat sees both perspectives as coherent within their respective frames: the constraint IS coordination for those committed to imago Dei anthropology and IS extraction for those committed to autonomy or posthuman frames. This is not a measurement error — it is the structural signature of a reading of a contested kernel. The engine computes per-seat classifications from the structural data and should show this divergence clearly.
 *
 * DIRECTIONALITY LOGIC:
 *   Human persons as imago Dei are the beneficiary set structurally (they receive the protection and dignity preservation); directionality for this set is near 0.0 (full beneficiary). AI development institutions pay the cost of subordination and foregone research (d near 1.0, full target). Enhancement researchers are excluded and their research directly blocked (high d, near-total target, but outside the main stakeholder economy). Potential enhancement subjects are identity-locked — their exit (seeking enhancement elsewhere) carries the cost of being redefined as posthuman, which the constraint treats as a loss of dignity rather than a choice. Their identity-lock means their d is bimodal: if they internalize the imago Dei frame, d is near 0.0 (they 'choose' subordination); if they resist, d is near 1.0 (they are suppressed). The suppression ambiguity omega addresses this directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is technological threat to human dignity and human nature. The founding_problem_status is contested: theological tradition keepers and faith communities attest the problem is live; secular autonomy frameworks and posthuman advocates attest it is misconceived. The disappearance_verdict is world_rearranges: if the constraint vanished, AI would develop along enhancement pathways presently blocked, institutional authority would fragment into competing readings, and the theological frame would lose structural authority. This is not mandatrophy — the constraint's mandate (to protect dignity by enforcing subordination and rejecting transgression) remains live within the imago Dei reading. Mandatrophy would arise if the founding problem dissolved (e.g., if AI development proved harmless to human dignity despite bypassing the constraint, if enhancement proved compatible with imago Dei theology, if the theological frame lost all institutional backing). Currently, the constraint is actively defended because the founding problem remains contested and the tradition-keeping institutions that created it still have authority. The measurement series show stable extractiveness and theater ratio because the constraint is not decaying — it is actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imago_dei_vs_autonomy_foundational,
    'Is human dignity primarily grounded in the inviolable image of God (imago Dei, prior to capability and unchanging), or is it grounded in rationality, autonomy, and the capacity for self-determination (which are capable-dependent and enhancement-compatible)?',
    'This is a conceptual/theological omega, not empirically resolvable. Resolution requires a reading of theological and philosophical tradition and an acceptance of one axiom as foundational. The sibling autonomy_rights_reading adopts the second grounding; this reading adopts the first.',
    'If autonomy is foundational, enhancement becomes potentially dignity-preserving (the enhanced person retains autonomy). If imago Dei is foundational and fixed, enhancement becomes transgression against dignity. Classification hinges entirely on this axiom choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imago_dei_vs_autonomy_foundational, conceptual, 'Foundational source of human dignity: theological anthropology vs. secular autonomy.').

omega_variable(
    human_nature_boundary_ambiguity,
    'What constitutes a transgression of human nature? Where is the boundary between legitimate tool use and illicit enhancement? Is the boundary fixed by theology, discovered by biology, or constructed by convention?',
    'Empirically, cases emerge (cognitive enhancement, lifespan extension, genetic modification, AI integration): the tradition answers ''transgression'' for each case and the answer accumulates a pattern. Theologically, the boundary is debated within tradition — some voices argue enhancement within nature''s implicit teleology is permissible; others argue any alteration is transgression.',
    'A narrower boundary (many things are transgressive) increases suppression and extractiveness; a wider boundary (few things are transgressive) reduces both. The constraint''s practical force depends on where the boundary is drawn, but the theological reading asserts it is drawn by tradition keepers, not by researchers or the enhanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_nature_boundary_ambiguity, empirical, 'The definition and location of the human-nature boundary.').

omega_variable(
    identity_lock_mechanism_in_enhancement_refusal,
    'For persons who might seek enhancement but are barred by the constraint: is the barrier structural (legal, institutional, technological access) or internalized (they believe enhancement would violate their identity as imago Dei)? What happens to the suppression if the structural barrier is removed?',
    'Post-barrier removal trajectory: if persons refuse enhancement because they now understand it as transgression, suppression was internalized and persists even when institutional barriers are gone. If they immediately seek enhancement, the barrier was purely structural.',
    'If internalized, the constraint''s suppression is durable even if legal enforcement weakens — the victim has internalized the violation frame. If structural only, removing barriers would dissolve suppression immediately, and the constraint depends entirely on active institutional enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_enhancement_refusal, empirical, 'Structural vs. internalized suppression mechanism in enhancement refusal.').

omega_variable(
    posthuman_movement_containment,
    'The constraint excludes posthuman and transhumanist voices from authority. As enhancement technologies advance and underground enhancement becomes feasible, can the constraint contain movements that explicitly reject imago Dei anthropology and embrace posthuman identity?',
    'Observational: tracking the growth of posthuman communities, black-market enhancement access, and jurisdictional competition (some jurisdictions permitting enhancement, others enforcing the constraint). If posthuman movements grow despite suppression, the constraint faces an identity-locked resistance problem.',
    'Growing posthuman identity-locked resistance would force the constraint toward more active, more costly suppression — or toward acknowledging a bifurcated human/posthuman split that the constraint cannot prevent but only segregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posthuman_movement_containment, empirical, 'Whether the constraint can contain posthuman movements or whether it creates an uncontrollable underground.').

omega_variable(
    theological_authority_secularization_drift,
    'The constraint''s enforcement depends on theological tradition-keeping institutions maintaining authority over the definition of human nature and transgression. As those institutions secularize and lose authority in pluralist societies, who defines the boundary between legitimate and transgressive enhancement?',
    'Institutional drift: tracking the erosion or persistence of theological authority in AI governance, bioethics, and law; observing whether secular frameworks (autonomy-based, rights-based) or theological frameworks prevail in regulatory regimes.',
    'If theological authority erodes, the constraint''s definition of transgression loses ground to secular and posthuman readings. Enforcement becomes a contest between readings rather than a unified tradition — this is not falsification of the constraint but a shift toward multiplicity that the single-reading framing cannot accommodate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_authority_secularization_drift, empirical, 'Persistence of theological institutional authority in pluralist governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_dignity_safeguarding__imago_dei_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_d_tr_t0, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_d_tr_t5, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(ai_d_tr_t10, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(ai_d_tr_t15, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(ai_d_tr_t20, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(ai_d_tr_t25, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement(ai_d_tr_t30, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(ai_d_tr_t35, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 35, 0.31).
narrative_ontology:measurement(ai_d_tr_t40, ai_dignity_safeguarding__imago_dei_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_d_be_t0, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ai_d_be_t5, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(ai_d_be_t10, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(ai_d_be_t15, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(ai_d_be_t20, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(ai_d_be_t25, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(ai_d_be_t30, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(ai_d_be_t35, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 35, 0.48).
narrative_ontology:measurement(ai_d_be_t40, ai_dignity_safeguarding__imago_dei_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_d_su_t0, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_d_su_t5, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(ai_d_su_t10, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(ai_d_su_t15, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(ai_d_su_t20, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(ai_d_su_t25, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(ai_d_su_t30, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(ai_d_su_t35, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 35, 0.67).
narrative_ontology:measurement(ai_d_su_t40, ai_dignity_safeguarding__imago_dei_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_dignity_safeguarding__imago_dei_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_dignity_safeguarding__imago_dei_reading, 0.12).
narrative_ontology:affects_constraint(ai_dignity_safeguarding__imago_dei_reading, ai_dignity_safeguarding__autonomy_rights_reading).
narrative_ontology:affects_constraint(ai_dignity_safeguarding__imago_dei_reading, ai_dignity_safeguarding__posthuman_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel ai_dignity_safeguarding. The kernel concerns how to protect human dignity in the face of AI and enhancement technology. The imago_dei_reading grounds dignity theologically and treats enhancement as transgression. The autonomy_rights_reading grounds dignity in rationality and democratic choice and permits cautious enhancement within rights limits. The posthuman_continuity_reading treats enhancement as human continuation and grounds dignity in personhood however constituted. Each reading has its own constraint story, its own ε value, its own victim/beneficiary set, and its own classification. They are linked structurally: the autonomy reading influences the imago reading by offering an alternative anthropology; the posthuman reading forecloses the imago reading's fixed-nature premise; the imago reading forecloses the posthuman reading's enhancement-as-fulfillment premise. All three are live positions in contemporary governance, held by different institutional actors and different communities. The decomposition follows the ε-invariance principle: the same natural-language concept ('AI dignity safeguarding') decomposes into structurally distinct constraints with different extractiveness values because the different readings have different victim/beneficiary sets and different notions of what counts as violation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
