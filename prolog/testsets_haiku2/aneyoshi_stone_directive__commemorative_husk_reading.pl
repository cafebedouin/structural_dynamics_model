% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Directive as Commemorative Husk
 *   domain: disaster_anthropology/institutional_memory/land-use_governance
 *
 * SUMMARY:
 *   Aneyoshi village in Iwate Prefecture, Japan erected a stone marker
 *   (tei-ishigaki) with an engraved directive instructing residents never to
 *   build below a certain elevation line on the flood plain. The stone was
 *   positioned on high ground as a memorial to a catastrophic ancestral
 *   tsunami and encoded a behavioral rule derived from pre-modern risk
 *   assessment. This constraint story instantiates ONE READING of the
 *   contested kernel: the COMMEMORATIVE HUSK READING. Under this reading, the
 *   stone's original directive retained nominal status for seven decades but
 *   lost behavioral force during the inter-catastrophe period — the long
 *   stretch without a major tsunami event. During this period, development
 *   pressure mounted, the village collective weakened its enforcement
 *   posture, and residents began treating the stone as a memorial artifact
 *   (performative remembrance) rather than a binding land-use constraint. The
 *   stone persists but functions primarily as theater: ceremonies continue,
 *   the narrative is transmitted, yet the constraint no longer structures
 *   settlement patterns. This reading attributes the constraint's persistence
 *   to institutional inertia and the memorial's cultural authority rather
 *   than to continued validation of the original behavioral directive. The
 *   sibling reading (behavioral_competence_reading) contests this by
 *   asserting the stone directive REMAINED BINDING across the 78-year
 *   interval without explicit re-validation — a claim this reading rejects
 *   structurally.
 *
 * KEY AGENTS:
 *   - aneyoshi_village_collective: Nominal agenda-setter; weakened enforcement capacity; performs memorial function but cannot resist development pressure (moderate power, generational horizon)
 *   - coastal_development_interests: Primary beneficiaries; gain access to profitable flood-plain land as the directive's behavioral force decays (organized power, mobile exit)
 *   - downstream_flood_plain_residents: Primary victims; inhabit land originally marked unsafe; cannot relocate; bear tsunami risk (powerless, trapped)
 *   - intergenerational_memory_keepers: Identity-locked payers; transmit the stone's narrative as cultural obligation but lack institutional backing to enforce the directive (powerless, identity-locked exit)
 *   - seismic_research_community: Analytical observer; can validate the original directive's risk assessment but excluded from village-level enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.81).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.79).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Stone Directive as Commemorative Husk").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land-use_governance").

domain_priors:requires_active_enforcement(aneyoshi_stone_directive__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, 'd388a719-c000-4374-9452-f777b27963b6').
narrative_ontology:cs_kernel_codification('d388a719-c000-4374-9452-f777b27963b6', fixed_text).
narrative_ontology:cs_authority_grounding('d388a719-c000-4374-9452-f777b27963b6', lineage).
narrative_ontology:cs_interpretation_layer_present('d388a719-c000-4374-9452-f777b27963b6').
narrative_ontology:cs_reading_relation('d388a719-c000-4374-9452-f777b27963b6', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('d388a719-c000-4374-9452-f777b27963b6', foundational, behavioral_force_decays_without_validation).
narrative_ontology:cs_axiom_status(behavioral_force_decays_without_validation, holdable).
narrative_ontology:cs_axiom_grounding('d388a719-c000-4374-9452-f777b27963b6', behavioral_force_decays_without_validation, empirically_contingent).
narrative_ontology:cs_axiom('d388a719-c000-4374-9452-f777b27963b6', secondary, institutional_inertia_enables_extraction).
narrative_ontology:cs_axiom_status(institutional_inertia_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d388a719-c000-4374-9452-f777b27963b6', institutional_inertia_enables_extraction, instrumental).
narrative_ontology:cs_reference_frame('d388a719-c000-4374-9452-f777b27963b6', protective_settlement_boundary_enforcement).
narrative_ontology:cs_drift_state('d388a719-c000-4374-9452-f777b27963b6', end_of_inter_catastrophe_period, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d388a719-c000-4374-9452-f777b27963b6', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, short_term_land_investors).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, downstream_flood_plain_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_village_collective).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, intergenerational_memory_keepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The village administration maintains the stone and performs periodic civic remembrance ceremonies. They lack resources to enforce the stone's original directive against development pressure and interpret their role as custodians of a memorial rather than enforcers of a binding land-use rule. They retain nominal authority over the constraint but have delegated enforcement responsibility downward to familial and community custom, which has atrophied.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_village_collective, agenda_setter,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_village_collective, payer).

% Real estate developers and agricultural investors who benefit from the weakening of the stone directive. As the directive's enforceability decayed during the inter-catastrophe period (decades without major tsunami), they gained access to profitable land in the flood plain that would otherwise be restricted. They do not publicly contest the memorial; they simply treat its regulatory force as obsolete.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    organized, biographical, mobile, national).

% Live in areas designated unsafe by the original stone directive but now developed due to the directive's loss of behavioral force. They cannot easily relocate; they depend on the land they occupy for housing and livelihood. They bear the catastrophic risk that the stone's original builders sought to prevent, while lacking the resources to challenge development that has proceeded in the inter-catastrophe period.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, downstream_flood_plain_residents, payer,
    powerless, immediate, trapped, local).

% Community members tasked with transmitting the stone's meaning and original behavioral instruction to younger generations. They perform the memorial function and recount the original catastrophe narrative, but lack institutional backing to enforce the directive. Their teaching role persists as theater even as the directive's binding force decays; they are locked into this identity by cultural obligation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, intergenerational_memory_keepers, payer,
    powerless, civilizational, identity_locked, local).

% Modern disaster management agencies and tsunami warning systems were not in place when the stone directive was created. They could, in principle, validate or supersede the stone's geographic and behavioral prescriptions with contemporary science. They are excluded from the village-level enforcement conversation and their authority competes with rather than reinforces the stone directive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, government_disaster_response_apparatus, excluded,
    institutional, generational, constrained, national).

% Scientists studying earthquake and tsunami risk can evaluate whether the stone directive's original prescriptions remain valid. They observe the constraint from outside and note that the directive embodies empirically sound risk assessment, yet has lost practical force due to institutional decay rather than empirical refutation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, seismic_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original stone directive coordinated safe settlement by embedding a pre-modern risk assessment into a durable, culturally-sanctioned marker. It solved the collective action problem of preventing settlement in lethal zones by making the rule a memorial obligation and a sacred boundary. Under THIS READING, the coordination function has DECAYED: the stone now coordinates only memorial practice (remembrance ceremonies, narrative transmission) rather than settlement patterns.
% TRANSFER_FUNCTION: Risk is transferred FROM the village collective (original rule-setters; they lose enforcement authority and cannot prevent development) TO coastal development interests (who gain profitable access to the flood plain). Simultaneously, risk is transferred FROM developers (who would face development constraints under an enforced directive) TO downstream flood-plain residents (who now live in a previously marked-unsafe zone without the protective behavioral rule that originally applied).
% ABSENT_VOICES: Future generations who will inherit the developed flood plain and the still-present tsunami hazard are not party to today's decisions. Pre-modern risk assessors (the stone's original creators) are deceased and cannot defend their original prohibition. Seismic researchers and disaster-management scientists are excluded from the village-level conversation and their authority is subordinated to the village's ceremonial role.
% DISAPPEARANCE_RATIONALE: If the stone were removed entirely and all narrative and institutional memory of the directive were erased, the flood plain would rapidly become fully developed within one generation. The stone's existence — even as theater — preserves a material reminder that this land was once deemed too dangerous. Without it, the last institutional anchor preventing total development would vanish. The coastal economy would rearrange around full utilization of the flood-plain zone.
% FOUNDING_PROBLEM: A catastrophic ancestral tsunami killed villagers and destroyed settlements built on the low flood plain. The stone directive encoded a geographic and behavioral lesson: survival required avoiding this zone.
% FOUNDING_PROBLEM_CORROBORATION: Seismic researchers confirm the physical hazard (tsunami risk to the flood plain) remains unchanged and has not been solved by engineering or evacuation systems alone. However, residents, village administrators, and development interests all attest the FOUNDING PROBLEM as a lived behavioral urgency is now DEAD — no one alive has experienced the catastrophe, and seven decades without a major event have created the perception that the threat is historical rather than current. The inter-catastrophe period itself is the cited reason the problem feels obsolete.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the loss of the directive's behavioral force ENABLES extraction of value from the flood plain by development interests. The constraint becomes extractive because its decay — the suppression of the original behavioral rule that protected the flood plain — benefits a concentrated group while concentrating risk on powerless residents. Suppression remains high (0.79) because the village collective must actively suppress the original directive's behavioral force, maintain ceremony without enforcement, and inhibit distributed memory-keeping that might reignite the directive's power. Theater rises from 0.25 to 0.68 because the constraint's function shifts from behavioral (actually guiding settlement) to memorial (narrating past catastrophe without current force). The suppression_requirement series (t0=0.92 to tn=0.79) shows that maintaining the constraint in husk form requires DECLINING active enforcement — the stone carries enough cultural weight to persist as theater without constant top-down suppression, but the original behavioral rule must be actively kept inert. The leveled coercion grid shows: accessibility_collapse declines across all levels (alternatives to flood-plain development become accessible as the directive weakens); stakes_inflation INCREASES at the individual level (residents move into hazardous zones, raising their personal stakes) while DECREASING at organizational level (developers face no cost to entry); suppression remains high across class and structural levels (the constraint persists as a suppressive device even as its behavioral target vanishes); resistance rises, especially at class level (displaced memory-keepers and vulnerable residents begin to contest the husk's adequacy). All metrics are authored on one shared time grid (t=0,13,26,39,52,65,78) so the engine can compare trends.
 *
 * PERSPECTIVAL GAP:
 *   The village collective and memory-keepers experience the stone as a memorial duty to preserve and perform — they see themselves as custodians of cultural memory rather than enforcers of a land-use rule. Development interests experience the stone as a weakened constraint whose behavioral force has lapsed — they can propose projects without public objection. Downstream residents experience the stone as a failed promise: it marks a boundary that was supposed to protect them, but that boundary is no longer enforced. Seismic researchers experience the stone as an artifact of sound pre-modern risk assessment whose original geographic wisdom is no longer institutionally backed. These seats should compute different classification outcomes: the village collective's seat may compute as rope-like (coordinating memorial practice), while the development interest seat computes as snare-like (extracting value through suppression of an original constraint), and the resident seat computes as snare (bearing concentrated extraction-like risk from a constraint's decay). The engine computes per-seat classification from power and exit; the authored claim is piton (mostly performance, atrophied function), independent of where those per-seat computations land.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests have high mobility (d→0.2: they benefit from the constraint's decay and can redirect capital elsewhere). The village collective is moderate-power constrained (d→0.45: they nominally administer the constraint but lack capacity to enforce it and derive no concentrated benefit from it). Memory-keepers are powerless and identity-locked (d→0.8: they are locked into transmitting the stone's narrative but cannot enforce it; they bear the cost of maintaining a rule no longer backed by collective action). Downstream residents are powerless and trapped (d→0.95: they cannot exit the flood plain, cannot prevent development, and bear catastrophic risk from the constraint's loss of force). The directionality derives from beneficiary/victim position and exit options: development interests are beneficiaries with mobile exit; residents are victims with trapped exit; the village collective is neither systematically beneficiary nor victim, but its moderate power and weakened enforcement capacity give it mid-range directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of the constraint exemplifies Mandatrophy in action. The founding problem (deaths from ancestral tsunami) has been 'solved' by time (no major event in 78 years), creating the illusion that the original directive is obsolete. But Mandatrophy is precisely the condition where a constraint's MANDATE has outlived its function (the mandate: keep people off the flood plain; the function: respond to lethal tsunami risk). The constraint persists because cultural inertia, memorial authority, and identity-fusion have kept it as theater. The READING CHOICE that instantiates this constraint is the declaration that the stone is a HUSK — a constraint whose mandate has become decoupled from actual behavior. The sibling reading (behavioral_competence) would deny Mandatrophy by arguing the directive REMAINED FUNCTIONALLY BINDING even without explicit re-validation. This reading asserts that Mandatrophy is precisely what occurred: institutional decay + theatrical maintenance + beneficiary silence allows extraction to proceed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_lived_vs_institutional,
    'Did the founding problem (lethal tsunami risk to the flood plain) remain a LIVED problem requiring active behavioral response, or did it become an INSTITUTIONAL ARTIFACT whose original urgency faded during the long inter-catastrophe period?',
    'Post-event review: if a major tsunami strikes the flood plain and residents perish, the lived problem was never solved — the directive''s decay was the failure. If no such event occurs for another generation and development proceeds with insurance/evacuation protocols, the institutional reading wins: the problem was transformed from behavioral avoidance to managed risk.',
    'If the founding problem remained lived, this reading is incorrect and the constraint should reclassify as snare (extraction via suppression of a still-necessary protective rule). If the founding problem became institutional, this reading''s piton classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_lived_vs_institutional, empirical, 'Whether the constraint''s founding problem persisted as a lived behavioral challenge or became an institutional-historical artifact.').

omega_variable(
    memorial_function_vs_extraction_cover,
    'Is the stone''s persistence as a memorial GENUINELY SEPARABLE from its loss of behavioral force, or is the memorial function the COVER STORY that enables the behavioral decay?',
    'Counterfactual: if the village collective had actively enforced the directive while also maintaining the memorial, would development interests have been able to suppress the rule? If the memorial was used INSTRUMENTALLY to de-prioritize enforcement, it is a cover story; if the memorial reflects genuine institutional role-shift, it is separable.',
    'If memorial is cover story, the beneficiary structure (development interests using commemoration to shield extraction) is more concentrated and intentional. If separable, the decay is more institutional-inertial. The piton classification holds either way, but the moral/agency structure differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_function_vs_extraction_cover, conceptual, 'Whether the memorial function obscures or genuinely represents the constraint''s actual role.').

omega_variable(
    intergenerational_knowledge_loss_mechanism,
    'Did the inter-catastrophe period itself CAUSE knowledge loss (younger generations never experienced the founding event, weakening belief in the rule), or did institutional decay PRECEDE and ENABLE the knowledge loss?',
    'Historical archive analysis: does village record-keeping show intentional suppression of the directive narrative, or gradual attrition as enforcement resources declined? Do memory-keeper oral histories report explicit decisions to deprioritize transmission, or passive drift?',
    'If knowledge loss was causal, this is a structural vulnerability to long inter-event intervals (a mountain-like feature of human risk assessment). If institutional decay was causal, this is a governance failure and the constraint is more clearly a snare. The piton reading treats both as compatible (theater persists even as knowledge atrophies), but the locus of blame differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_knowledge_loss_mechanism, empirical, 'Whether inter-event time directly caused knowledge loss or whether institutional choices enabled it.').

omega_variable(
    kernel_reading_boundary_memorial_vs_husk,
    'Is the stone directive properly read as a LIVING MEMORIAL (a constraint that now serves primarily commemorative purpose, legitimately co-existing with development) or as a HUSK (a constraint whose behavioral function has been suppressed but whose original mandate is not yet formally superseded)?',
    'Formal institutional action: if the village explicitly rescind the directive and redesignate the land, it becomes a pure memorial. If the village maintains nominal authority over the directive while declining to enforce it, it remains a husk. The distinction is whether the mandate has been formally displaced or merely institutionally inert.',
    'Memorial status would suggest the reading is correctly characterized as piton (theater-heavy). Husk status (unrescinded mandate + no enforcement) supports this reading''s claim that extraction is enabled BY institutional inertia. Either way, the constraint''s extractiveness and suppression are high, but the reading''s structural foundation (mandate decay vs. memorial transition) clarifies what the engine should measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_memorial_vs_husk, conceptual, 'The proper institutional-legal classification of the stone directive''s current status: living memorial or unrescinded but inert mandate.').

omega_variable(
    sibling_reading_boundary_behavioral_competence,
    'Under what conditions would the BEHAVIORAL_COMPETENCE_READING (the sibling reading) be structurally correct — i.e., when does a constraint that has NOT been empirically refuted remain BINDING even without re-validation?',
    'Theoretical: the sibling reading asserts that the directive''s binding force is deontological (grounded in institutional continuity) rather than empirical (grounded in tested risk assessment). This reading counters by saying empirical force (the need for the behavioral rule) is the PRIMARY grounding, and institutional force alone is insufficient. A major tsunami event would empirically validate the sibling''s claim; continued absence validates this husk reading''s claim that the founding problem became merely institutional.',
    'This omega documents the reading contest itself: what structural premise would make the sibling reading correct and this reading incorrect. If found, it routes to the behavioral_competence_reading''s cs_structure.drift_state (the sibling''s empirical grounding remains untested and its axiom persists as holdable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_boundary_behavioral_competence, conceptual, 'The theoretical boundary between this reading (husk/inertial) and its sibling (competence/binding).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(aney_tr_t0, observed).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 13, 0.35).
narrative_ontology:measurement_basis(aney_tr_t13, observed).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 26, 0.45).
narrative_ontology:measurement_basis(aney_tr_t26, observed).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 39, 0.55).
narrative_ontology:measurement_basis(aney_tr_t39, observed).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 52, 0.62).
narrative_ontology:measurement_basis(aney_tr_t52, observed).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 65, 0.67).
narrative_ontology:measurement_basis(aney_tr_t65, observed).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 78, 0.68).
narrative_ontology:measurement_basis(aney_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(aney_be_t0, observed).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 13, 0.85).
narrative_ontology:measurement_basis(aney_be_t13, observed).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 26, 0.82).
narrative_ontology:measurement_basis(aney_be_t26, observed).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 39, 0.79).
narrative_ontology:measurement_basis(aney_be_t39, observed).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 52, 0.81).
narrative_ontology:measurement_basis(aney_be_t52, observed).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 65, 0.81).
narrative_ontology:measurement_basis(aney_be_t65, observed).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 78, 0.81).
narrative_ontology:measurement_basis(aney_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement_basis(aney_su_t0, observed).
narrative_ontology:measurement(aney_su_t13, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 13, 0.88).
narrative_ontology:measurement_basis(aney_su_t13, observed).
narrative_ontology:measurement(aney_su_t26, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 26, 0.84).
narrative_ontology:measurement_basis(aney_su_t26, observed).
narrative_ontology:measurement(aney_su_t39, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 39, 0.81).
narrative_ontology:measurement_basis(aney_su_t39, observed).
narrative_ontology:measurement(aney_su_t52, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 52, 0.79).
narrative_ontology:measurement_basis(aney_su_t52, observed).
narrative_ontology:measurement(aney_su_t65, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 65, 0.79).
narrative_ontology:measurement_basis(aney_su_t65, observed).
narrative_ontology:measurement(aney_su_t78, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 78, 0.79).
narrative_ontology:measurement_basis(aney_su_t78, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=78
narrative_ontology:measurement(aney_grid_01, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(class), 0, 0.82).
narrative_ontology:measurement(aney_grid_02, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(class), 78, 0.45).
narrative_ontology:measurement(aney_grid_03, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(individual), 0, 0.78).
narrative_ontology:measurement(aney_grid_04, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(individual), 78, 0.38).
narrative_ontology:measurement(aney_grid_05, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(organizational), 0, 0.85).
narrative_ontology:measurement(aney_grid_06, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(organizational), 78, 0.52).
narrative_ontology:measurement(aney_grid_07, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(structural), 0, 0.88).
narrative_ontology:measurement(aney_grid_08, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(structural), 78, 0.55).
narrative_ontology:measurement(aney_grid_09, aneyoshi_stone_directive__commemorative_husk_reading, resistance(class), 0, 0.18).
narrative_ontology:measurement(aney_grid_10, aneyoshi_stone_directive__commemorative_husk_reading, resistance(class), 78, 0.42).
narrative_ontology:measurement(aney_grid_11, aneyoshi_stone_directive__commemorative_husk_reading, resistance(individual), 0, 0.22).
narrative_ontology:measurement(aney_grid_12, aneyoshi_stone_directive__commemorative_husk_reading, resistance(individual), 78, 0.35).
narrative_ontology:measurement(aney_grid_13, aneyoshi_stone_directive__commemorative_husk_reading, resistance(organizational), 0, 0.15).
narrative_ontology:measurement(aney_grid_14, aneyoshi_stone_directive__commemorative_husk_reading, resistance(organizational), 78, 0.32).
narrative_ontology:measurement(aney_grid_15, aneyoshi_stone_directive__commemorative_husk_reading, resistance(structural), 0, 0.12).
narrative_ontology:measurement(aney_grid_16, aneyoshi_stone_directive__commemorative_husk_reading, resistance(structural), 78, 0.38).
narrative_ontology:measurement(aney_grid_17, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(class), 0, 0.82).
narrative_ontology:measurement(aney_grid_18, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(class), 78, 0.35).
narrative_ontology:measurement(aney_grid_19, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(individual), 0, 0.92).
narrative_ontology:measurement(aney_grid_20, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(individual), 78, 0.95).
narrative_ontology:measurement(aney_grid_21, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(organizational), 0, 0.65).
narrative_ontology:measurement(aney_grid_22, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(organizational), 78, 0.25).
narrative_ontology:measurement(aney_grid_23, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(structural), 0, 0.88).
narrative_ontology:measurement(aney_grid_24, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(structural), 78, 0.42).
narrative_ontology:measurement(aney_grid_25, aneyoshi_stone_directive__commemorative_husk_reading, suppression(class), 0, 0.95).
narrative_ontology:measurement(aney_grid_26, aneyoshi_stone_directive__commemorative_husk_reading, suppression(class), 78, 0.78).
narrative_ontology:measurement(aney_grid_27, aneyoshi_stone_directive__commemorative_husk_reading, suppression(individual), 0, 0.88).
narrative_ontology:measurement(aney_grid_28, aneyoshi_stone_directive__commemorative_husk_reading, suppression(individual), 78, 0.72).
narrative_ontology:measurement(aney_grid_29, aneyoshi_stone_directive__commemorative_husk_reading, suppression(organizational), 0, 0.82).
narrative_ontology:measurement(aney_grid_30, aneyoshi_stone_directive__commemorative_husk_reading, suppression(organizational), 78, 0.55).
narrative_ontology:measurement(aney_grid_31, aneyoshi_stone_directive__commemorative_husk_reading, suppression(structural), 0, 0.98).
narrative_ontology:measurement(aney_grid_32, aneyoshi_stone_directive__commemorative_husk_reading, suppression(structural), 78, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.25).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_directive kernel decomposes into two constraint stories with divergent ε values and structural claims. COMMEMORATIVE_HUSK_READING (this story) asserts the directive's behavioral force decayed during inter-catastrophe, enabling extraction (~ε 0.81). BEHAVIORAL_COMPETENCE_READING (sibling) asserts the directive remained binding without re-validation (~ε lower, closer to rope/mountain). The readings coexist as live positions held by different institutional actors (village collective + memory-keepers vs. development interests + younger residents). They are NOT merely different framings of a single constraint — they have different beneficiary structures, different suppression mechanisms, and substantially different ε values. Write both as separate constraint stories linked by network.affects_constraints. This story influences the sibling by establishing the counter-thesis: what the sibling reading must defend against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
