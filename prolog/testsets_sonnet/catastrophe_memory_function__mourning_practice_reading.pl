% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av as Mourning-Practice and Boundary-Norm Maintenance (D1/D4 reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the catastrophe_memory_function
 *   kernel: that Tisha B'Av's ritual content is dominantly D1
 *   (mourning-practice) and D4 (boundary-norm maintenance) — the ritual IS
 *   the boundary work, full stop, with no meaningful D5
 *   (survival-competence/adaptive-capacity transmission) component. On this
 *   reading the fast's liturgy, prohibitions, and communal reconvening
 *   function primarily to reconstitute group identity and mark who belongs,
 *   not to teach decentralized institutional adaptation for future
 *   catastrophes. The sibling readings — survival_competence_reading (ritual
 *   as adaptive-capacity transmission) and hybrid_transformation_reading
 *   (both functions at once) — are separate constraints with their own ε
 *   values and are not blended into this one. Extraction here is measured as
 *   low-moderate and slowly rising: the coordination function (sustaining
 *   stateless diaspora identity) is real and substantial, but a genuine cost
 *   is borne disproportionately by members whose life choices the
 *   boundary-norm content implicitly indicts (intermarried families,
 *   assimilation-inclined members) and by youth for whom the memorial content
 *   has not been made experientially present.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority: sets liturgical/halakhic content (institutional/identity_locked) — administers the mourning-boundary reading
 *   - diaspora_communal_cohesion: organized beneficiary — gains reconvening and engagement from the mandatory cycle
 *   - observant_households: primary payers/co-beneficiaries — bear fasting cost, receive belonging
 *   - intermarried_families: powerless payers — indicted by the boundary content they cannot exit without cost
 *   - assimilation_inclined_members: powerless-to-moderate payers — the boundary's intended target
 *   - ritually_disengaged_youth: powerless payers — compliance without transmitted meaning
 *   - comparative_ritual_scholars: analytical observer distinguishing D1/D4 from D5 content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.32).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av as Mourning-Practice and Boundary-Norm Maintenance (D1/D4 reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'd081b2b2-6fe9-4006-8dee-b06a2a7ecfed').
narrative_ontology:cs_kernel_codification('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', fixed_text).
narrative_ontology:cs_authority_grounding('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', lineage).
narrative_ontology:cs_interpretation_layer_present('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed').
narrative_ontology:cs_reading_relation('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', foundational, ritual_content_is_memorial_not_adaptive).
narrative_ontology:cs_axiom_status(ritual_content_is_memorial_not_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', ritual_content_is_memorial_not_adaptive, conventional).
narrative_ontology:cs_axiom('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', foundational, boundary_maintenance_is_the_coordination_function).
narrative_ontology:cs_axiom_status(boundary_maintenance_is_the_coordination_function, holdable).
narrative_ontology:cs_axiom_grounding('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', boundary_maintenance_is_the_coordination_function, instrumental).
narrative_ontology:cs_reference_frame('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', rabbinic_post_temple_mourning_consolidation).
narrative_ontology:cs_drift_state('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', contemporary_diaspora_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d081b2b2-6fe9-4006-8dee-b06a2a7ecfed', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, communal_identity_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, diaspora_communal_cohesion).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, assimilation_inclined_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, intermarried_families).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, ritually_disengaged_youth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_households).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, observant_households).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, collective_memory_requires_ritual_enactment).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, boundary_maintenance_sustains_peoplehood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the halakhic content of the fast, the liturgy, and the specific mourning prohibitions (no leather shoes, no bathing, sitting low). Determines which historical catastrophes get folded into the day's commemorative frame. Its own authority is constituted by successfully transmitting this reading generation to generation — the rabbinate cannot exit the practice without dissolving part of what makes it the rabbinate.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Not an actor but the collective good the ritual is said to produce: a durable sense of peoplehood transmitted across dispersion and centuries without a territorial or state apparatus to anchor it. Listed for completeness since the coordination-function analysis depends on it, but it collects nothing itself — the rabbinate and communal institutions collect the deference this good justifies.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, communal_identity_continuity, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_function__mourning_practice_reading, communal_identity_continuity).

% Synagogues, communal organizations, and lay leadership benefit from the annual mourning cycle as a mechanism that reliably reconvenes dispersed members, reinforces shared calendar and shared grief, and gives communal institutions a recurring occasion to assert relevance and raise engagement. They gain organizational cohesion from the ritual's mandatory character.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, diaspora_communal_cohesion, beneficiary,
    organized, generational, constrained, global).

% Fast for 25 hours, forgo washing and leather and marital relations, sit through lamentation liturgy, and instruct children in the practice. They receive real belonging and meaning from it, but also bear the physical cost of fasting, the social cost of visible difference from the surrounding culture, and the burden of transmitting compliance to children who may not choose it freely.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_households, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, observant_households, beneficiary).

% Want to integrate more fully with surrounding society or reduce the salience of communal difference. The boundary-maintenance function of the fast is aimed precisely at people like them — reminding the community why difference must be preserved. Their preference for a lower-friction identity is treated as the problem the ritual exists to correct, not as a legitimate alternative equilibrium.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, assimilation_inclined_members, payer,
    moderate, biographical, trapped, national).

% Households where the boundary-norm content of the day (endogamy anxiety embedded historically in mourning for communal dissolution) marks them as instances of the very loss being mourned. They cannot participate in the commemorative logic without it implicitly indicting their own family structure, yet leaving the community entirely forfeits other goods of membership.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, intermarried_families, payer,
    powerless, biographical, trapped, national).

% Young community members for whom the fast reads as arbitrary hardship disconnected from lived catastrophe (no personal memory of Temple destruction, pogroms, or expulsion). They are pressured by family and community expectation to comply with mourning practice whose meaning has not been made present to them, and disengagement is treated as a communal failure to be corrected rather than a legitimate response.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritually_disengaged_youth, payer,
    powerless, biographical, constrained, national).

% Study Tisha B'Av alongside other catastrophe-commemoration rituals across cultures to determine whether its content is dominantly memorial/boundary-maintaining (D1/D4) as opposed to transmitting adaptive survival mechanisms (D5). Their classification determines which reading of the kernel best fits the observed liturgical and behavioral content.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, comparative_ritual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fast day coordinates a dispersed, stateless population around a shared calendar of grief, giving diaspora communities without territorial contiguity a recurring, synchronized occasion that reconstitutes group boundaries and affirms continued peoplehood, stated without regard to whether that boundary-maintenance is beneficial to every member equally.
% TRANSFER_FUNCTION: Moves compliance, discomfort, and deference from individual members (especially those least invested in boundary-maintenance — the assimilation-inclined, intermarried, and disengaged) to communal and rabbinic institutions, which receive renewed legitimacy, engagement, and interpretive authority in exchange for administering the commemorative calendar.
% ABSENT_VOICES: Intermarried families and assimilation-inclined members would object that the boundary-norm content treats their life choices as instances of communal loss rather than legitimate adaptation, but the interpretive apparatus that sets the day's content is staffed by those most invested in boundary maintenance, not by those the boundary is drawn against.
% DISAPPEARANCE_RATIONALE: Proponents (rabbinic authorities, communally embedded households) hold that if the fast disappeared, boundary erosion and assimilation would accelerate measurably within a generation — the world rearranges toward dissolution. Assimilation-inclined and disengaged members hold that communal identity persists through other mechanisms (language, food, holidays with lower cost) and that removing this specific high-cost mourning obligation would leave underlying group continuity largely unchanged. The dispute is genuine and unresolved within the tradition itself.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE) and subsequent catastrophes retroactively folded into the same date, the community faced the problem of maintaining cohesive identity and boundary integrity without a central sanctuary, a state, or territorial contiguity to anchor collective memory.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish communal formation (outside rabbinic authority) corroborate that diaspora peoplehood without state apparatus is a genuinely difficult coordination problem that ritual calendars have historically helped solve — this is not merely rabbinic self-justification. However, the SAME historians, along with sociologists of contemporary Jewish identity, report that intermarriage rates, denominational fluidity, and diaspora integration into host societies have proceeded substantially regardless of fast-day observance, suggesting the founding problem's acuteness has diminished even as the practice's enforcement apparatus has not.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 by interval end) because the dominant function under this reading is coordination (sustaining stateless peoplehood) rather than extraction of resources or labor — but it is not zero, because the boundary-maintenance content structurally costs members whose life paths the boundary is drawn against. Suppression is authored moderate and slightly declining (0.40 -> 0.32) reflecting that enforcement was tighter in eras of stronger communal insularity and has softened (though not vanished) as exit options for individual members expanded with modernity and denominational pluralism. Theater ratio rises modestly (0.10 -> 0.22) reflecting that as the literal catastrophes commemorated recede further from lived experience, an increasing share of observance is maintained as identity performance rather than felt mourning — this is exactly the D1/D4-without-D5 signature: memorial obligation persisting as boundary marker even where its adaptive-transmission content (which the sibling reading would measure) is absent by this reading's own claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and organized communal institutions sit near the beneficiary end: they administer the content and gain renewed legitimacy and engagement from the annual cycle, and their own institutional identity is partly constituted by successfully transmitting it (hence identity_locked exit for the rabbinate, not merely institutional). Observant households sit closer to symmetric — real cost, real belonging. Intermarried families and assimilation-inclined members sit near the target end: the boundary-norm content is aimed at correcting exactly their situation, and their exit options are trapped/constrained because communal membership carries other goods they are unwilling to forfeit entirely. Disengaged youth are powerless payers experiencing compliance pressure without transmitted meaning.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless diaspora identity maintenance) is contested as live vs. dead: corroboration from historians outside rabbinic authority confirms the underlying coordination problem was genuine at founding and remains partially live, but the same sources note the practice's enforcement infrastructure (family and communal pressure, halakhic administration) has not eroded in proportion to the diminishing marginal need, which is the classic signature the mandatrophy check exists to catch — a founding problem that has softened while the apparatus built to solve it has not. This story does not resolve the mismatch; it flags founding_problem_status as contested precisely because the benefiting parties (rabbinic authority, embedded households) and the paying parties (intermarried families, disengaged youth) give genuinely different, non-reconcilable answers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    d1_d4_vs_d5_content_boundary,
    'Is the liturgical and behavioral content of Tisha B''Av actually separable into a pure D1/D4 (mourning/boundary) component with no D5 (survival-competence) content, or does close reading of the liturgy (e.g., Eichah''s implicit strategies for surviving displacement, post-Temple rabbinic reorganization narratives embedded in the day''s texts) show adaptive-transmission content this reading denies?',
    'Comparative textual and ethnographic analysis of the day''s full liturgical corpus (Eichah, kinot, associated halakhic literature) coded against a D1/D4/D5 rubric by scholars independent of any single reading''s advocates, cross-checked against the sibling survival_competence_reading and hybrid_transformation_reading stories.',
    'If D5 content is found to be substantial, this story''s claim of pure D1/D4 is undermined and the hybrid_transformation_reading becomes the better-fitting constraint for the same ritual; if D5 content is genuinely negligible, this reading''s ε and classification stand as the accurate account of the mourning-practice function alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(d1_d4_vs_d5_content_boundary, conceptual, 'Whether the ritual''s content is structurally separable into the D1/D4-only claim this reading makes.').

omega_variable(
    boundary_maintenance_natural_vs_constructed,
    'Is group-boundary maintenance through memorial obligation a naturally emergent feature of any stateless, dispersed population''s survival strategy (making the D1/D4 reading closer to an anthropological near-universal), or is the specific boundary content (who counts as loss, who counts as threat to continuity) a constructed choice serving the interests of those who currently administer it?',
    'Cross-cultural comparison with other diaspora/dispersed populations'' mourning-and-boundary rituals (Armenian, Romani, Kurdish, Tibetan) to assess whether the specific content choices (e.g., which historical events get folded in, whose family structures get implicitly indicted) vary with who holds interpretive authority.',
    'If content choices track interpretive-authority interests closely across cases, the constructed reading strengthens and the coordination claim weakens relative to the extraction claim; if content is stable regardless of who administers it, the coordination claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_maintenance_natural_vs_constructed, conceptual, 'Whether the specific boundary content is naturally derived or interest-constructed.').

omega_variable(
    generational_transmission_decay_trajectory,
    'Is the rising theater_ratio trend (observance persisting as identity performance while felt/transmitted meaning declines) a stable long-term trajectory that will continue toward eventual piton status, or a temporary generational dip that self-corrects as diaspora communities re-invest meaning through new pedagogical and experiential practices?',
    'Longitudinal sociological tracking of observance rates, self-reported meaning, and communal investment in commemorative education across the next 1-2 generations.',
    'A continued rise toward piton status would suggest the D1/D4 function is degrading into pure boundary performance without even the identity-cohesion benefit currently claimed; a self-correction would support the coordination-function claim''s durability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_transmission_decay_trajectory, empirical, 'Whether declining transmitted meaning is a stable trend or a correctable generational dip.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t400, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 400, 0.12).
narrative_ontology:measurement_basis(cata_tr_t400, observed).
narrative_ontology:measurement(cata_tr_t800, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 800, 0.14).
narrative_ontology:measurement_basis(cata_tr_t800, observed).
narrative_ontology:measurement(cata_tr_t1200, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1200, 0.16).
narrative_ontology:measurement_basis(cata_tr_t1200, observed).
narrative_ontology:measurement(cata_tr_t1600, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1600, 0.19).
narrative_ontology:measurement_basis(cata_tr_t1600, observed).
narrative_ontology:measurement(cata_tr_t1955, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1955, 0.22).
narrative_ontology:measurement_basis(cata_tr_t1955, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t400, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 400, 0.2).
narrative_ontology:measurement_basis(cata_be_t400, observed).
narrative_ontology:measurement(cata_be_t800, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 800, 0.22).
narrative_ontology:measurement_basis(cata_be_t800, observed).
narrative_ontology:measurement(cata_be_t1200, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1200, 0.24).
narrative_ontology:measurement_basis(cata_be_t1200, observed).
narrative_ontology:measurement(cata_be_t1600, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1600, 0.26).
narrative_ontology:measurement_basis(cata_be_t1600, observed).
narrative_ontology:measurement(cata_be_t1955, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1955, 0.28).
narrative_ontology:measurement_basis(cata_be_t1955, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t400, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 400, 0.38).
narrative_ontology:measurement_basis(cata_su_t400, observed).
narrative_ontology:measurement(cata_su_t800, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 800, 0.36).
narrative_ontology:measurement_basis(cata_su_t800, observed).
narrative_ontology:measurement(cata_su_t1200, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1200, 0.35).
narrative_ontology:measurement_basis(cata_su_t1200, observed).
narrative_ontology:measurement(cata_su_t1600, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1600, 0.33).
narrative_ontology:measurement_basis(cata_su_t1600, observed).
narrative_ontology:measurement(cata_su_t1955, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1955, 0.32).
narrative_ontology:measurement_basis(cata_su_t1955, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_function kernel, decomposed per the epsilon-invariance principle because the natural-language label 'what Tisha B'Av's ritual does' covers structurally distinct claims with different implied ε. mourning_practice_reading (this story) claims pure D1/D4 content (memorial/boundary-maintenance only, ε=0.28, tangled_rope). survival_competence_reading claims the ritual primarily transmits D5 adaptive/institutional-continuity capacity, which is a different coordination function with a different beneficiary/victim structure and a lower expected extraction profile (closer to rope). hybrid_transformation_reading claims both D1/D4 and D5 operate jointly, which yields a different, blended coordination story and a distinct ε from either pure reading. All three are linked via affects_constraints rather than merged, per Rule 1 of the committer frame: each reading is a clean, single-ε constraint, not a parameter of one shared constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
