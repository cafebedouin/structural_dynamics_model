% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Passover as Survival-Competence Transmission (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the catastrophe_memory_function
 *   kernel — the survival_competence_reading (D5). On this reading,
 *   Passover's central function is the transmission of practical adaptive
 *   capacity: how a dispersed population reconstitutes itself, maintains
 *   identity, and physically survives after the collapse or removal of
 *   centralized institutions. The ritual is read as encoded procedural
 *   knowledge (embodied rehearsal of rapid departure, decentralization to
 *   household-level practice, portable/memorizable transmission medium)
 *   rather than primarily as memorial obligation. This is deliberately
 *   narrower than the hybrid_transformation_reading (which holds both D1/D4
 *   and D5 simultaneously) and structurally distinct from the
 *   mourning_practice_reading (which centers loss-memory and
 *   boundary-maintenance, D1/D4). Each reading is authored as its own
 *   constraint with its own stable epsilon; this file does not average across
 *   them or describe the contest internally.
 *
 * KEY AGENTS:
 *   - diaspora_communities: primary beneficiaries of the transmitted decentralized-continuity template
 *   - decentralized_household_practitioners: the distributed nodes that make the competence redundant/resilient
 *   - future_generations_facing_institutional_collapse: beneficiaries of rehearsal whose benefit is realized only under future catastrophe conditions
 *   - ritual_reformers_and_transmitters: agenda-setters who select and standardize transmitted content, without material extraction
 *   - comparative_ritual_scholars: analytical observers assessing whether the functionalist reading is empirically supported
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Passover as Survival-Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '80be912c-24de-4bcd-afd4-820cc9696a49').
narrative_ontology:cs_kernel_codification('80be912c-24de-4bcd-afd4-820cc9696a49', fixed_text).
narrative_ontology:cs_authority_grounding('80be912c-24de-4bcd-afd4-820cc9696a49', practice).
narrative_ontology:cs_interpretation_layer_present('80be912c-24de-4bcd-afd4-820cc9696a49').
narrative_ontology:cs_reading_relation('80be912c-24de-4bcd-afd4-820cc9696a49', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('80be912c-24de-4bcd-afd4-820cc9696a49', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('80be912c-24de-4bcd-afd4-820cc9696a49', foundational, ritual_content_encodes_procedural_survival_knowledge).
narrative_ontology:cs_axiom_status(ritual_content_encodes_procedural_survival_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('80be912c-24de-4bcd-afd4-820cc9696a49', ritual_content_encodes_procedural_survival_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('80be912c-24de-4bcd-afd4-820cc9696a49', secondary, decentralized_transmission_supersedes_centralized_institutional_dependency).
narrative_ontology:cs_axiom_status(decentralized_transmission_supersedes_centralized_institutional_dependency, holdable).
narrative_ontology:cs_axiom_grounding('80be912c-24de-4bcd-afd4-820cc9696a49', decentralized_transmission_supersedes_centralized_institutional_dependency, instrumental).
narrative_ontology:cs_reference_frame('80be912c-24de-4bcd-afd4-820cc9696a49', post_temple_decentralized_reconstruction).
narrative_ontology:cs_drift_state('80be912c-24de-4bcd-afd4-820cc9696a49', contemporary_diaspora_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('80be912c-24de-4bcd-afd4-820cc9696a49', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, decentralized_household_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations_facing_institutional_collapse).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, ritual_as_adaptive_technology_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_continuity_over_centralized_institution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practice the seder annually without a Temple, without a centralized priesthood, and often without geographic proximity to other practitioners. The ritual structure — retelling, embodied re-enactment of departure, transmission of practical knowledge (what to pack, what to leave, how to move quickly, how to maintain identity without fixed institutions) — gives each household a functioning template for continuity when central institutions are unavailable. They did not design this function; they inherit it and it demonstrably works for them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Each household becomes a self-sufficient node capable of conducting the full ritual without clergy, without a central building, and without institutional sanction. The Haggadah is portable, memorizable, and reproducible. This distributes the capacity to maintain group continuity across many independent sites rather than concentrating it in one vulnerable institution.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, decentralized_household_practitioners, beneficiary,
    moderate, generational, constrained, regional).

% Have not yet needed the transmitted competence but inherit the rehearsed capacity — the annual repetition means that when an actual institutional rupture occurs (expulsion, destruction, forced migration), a already-internalized script for adaptive response exists rather than needing to be improvised from nothing under crisis conditions. They cannot presently exit the tradition to test this, since the benefit is realized only under conditions not yet arrived.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_generations_facing_institutional_collapse, beneficiary,
    powerless, civilizational, trapped, global).

% Rabbinic authorities and household elders who selected, standardized, and continue to transmit the Haggadah's specific content and sequence. They administer which elements are preserved as central (departure narrative, practical instruction, questions-and-answers pedagogy) and which recede. Their choices shape what competence gets transmitted, but they extract no material rent from the transmission and bear the same exposure to institutional catastrophe as everyone else.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_reformers_and_transmitters, agenda_setter,
    organized, generational, constrained, global).

% Study Passover alongside other catastrophe-response rituals cross-culturally to assess whether the transmitted content functions as genuine adaptive knowledge (evacuation logistics, identity maintenance without central institutions, rapid reconstitution of community) or whether that function is a retrospective interpretive overlay on what is primarily memorial and identity-boundary practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves a genuine multi-generational coordination problem: how does a dispersed population maintain the capacity to survive future institutional rupture (destruction of central authority, expulsion, forced migration) when no central institution can be relied upon to persist? Annual embodied rehearsal of departure-under-duress, portable transmission via a memorizable/reproducible text, and decentralization of competence to every household solves this without requiring a standing bureaucracy.
% TRANSFER_FUNCTION: Transfers practical adaptive knowledge (how to move quickly, sustain identity without fixed institutions, reconstitute community from dispersed units) from each generation that has faced disruption to the next generation that has not yet faced it. No material transfer between parties is required for the ritual to function on this reading; what moves is competence, not resource.
% ABSENT_VOICES: Those who experience the ritual purely as loss-memorial (see the mourning_practice_reading) and those who find the survival-competence framing an over-functionalist retrospective reading imposed by scholars rather than felt by practitioners are not centered in this reading. Practitioners who prioritize the theological/covenantal content over any adaptive-capacity function would also object to being described primarily as recipients of a survival technology.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, whether the world rearranges depends on whether the transmitted competence is doing real causal work or is a retrospective functional label applied to a tradition whose persistence is better explained by covenantal obligation and communal identity maintenance. Advocates of this reading argue diaspora communities would lose a rehearsed template for decentralized continuity under institutional collapse; skeptics argue the practical skills in question (mobility, informal networks, dispersed leadership) are transmitted through many channels of Jewish communal life independent of the seder specifically, so the ritual's disappearance would be absorbed rather than catastrophic.
% FOUNDING_PROBLEM: A population needed the capacity to survive recurring catastrophic disruption to centralized institutions (Temple destruction, expulsion, persecution) without any guarantee that centralized religious or political authority would survive to coordinate the response — the ritual was retained, on this reading, because encoding survival competence in a decentralized, memorizable, annually-rehearsed form solved a real continuity problem that no standing institution could solve for itself.
% FOUNDING_PROBLEM_CORROBORATION: Some historians of Jewish institutional response to catastrophe (post-Temple rabbinic reconstruction, post-expulsion community re-formation patterns) note structural parallels consistent with a decentralized-competence account, offering corroboration from outside strictly theological or communal-insider framing. However, mainstream ritual studies scholarship is split: many scholars attribute the ritual's persistence primarily to covenantal/memorial function (D1/D4) rather than adaptive-survival function (D5), and no consensus corroboration exists that the survival-competence reading, rather than the mourning-practice reading, is the primary operative function historically. This reading's strongest corroboration comes from functionalist anthropology of ritual and resilience studies, which are themselves interpretive frameworks rather than testimony from practitioners.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) and suppression modest (0.22) because on this reading no party is coerced into participation and no party extracts material rent from others through the ritual's operation — the coordination genuinely is decentralized, and the beneficiaries and the population bearing any costs of maintaining the practice (time, resources for the seder) substantially overlap. Theater ratio starts low (0.10) but rises modestly over the interval (0.28) reflecting the empirically documented drift of some contemporary practice toward performative/consumerist observance (elaborate commercial seder products, symbolic-only participation) that dilutes the transmission-of-competence function without eliminating it. Accessibility collapse is moderate (0.35): alternative forms of transmitting adaptive/survival knowledge exist and are not suppressed by this ritual's persistence, but the specific historically-rehearsed script has no close substitute within the tradition. Resistance is low (0.2) — this reading meets little organized opposition since it does not extract from anyone; where friction exists it is scholarly/interpretive (skepticism about whether the functionalist reading overstates causal contribution) rather than structural resistance from an extracted-from party.
 *
 * PERSPECTIVAL GAP:
 *   There is little seat divergence in the coercive sense because no party occupies a structural payer role under this reading — beneficiaries and agenda-setters largely coincide (households both administer and receive the transmitted competence). The genuine gap is epistemic rather than extractive: comparative_ritual_scholars, from an analytical seat, may compute this constraint as having weaker coordination-function grounding than practitioners experience it, since the survival-competence claim is harder to verify against the alternative that the same continuity outcomes derive from general communal social capital rather than this specific ritual content.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries sit near the low-d (beneficiary) end: the ritual subsidizes their adaptive capacity without extracting from them in return, beyond ordinary participation costs. No victim group is declared because on this reading no party's costs are asymmetric transfers to another party's benefit — the closest thing to a 'cost' (time, resources, discipline of annual practice) is borne by the same population that receives the transmitted competence. ritual_reformers_and_transmitters are agenda-setters but not beneficiaries in the extractive sense — they select content but do not capture rents from doing so, and they share the same catastrophe exposure as everyone else, which is why no override was applied to push their d toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling by keeping the founding problem's status genuinely contested rather than asserting it is settled: the founding problem (surviving institutional collapse without centralized authority) is not clearly 'dead' in the way a fully obsolete emergency measure would be, because diaspora and minority communities continue to face non-zero risk of institutional rupture, but it is also not obviously 'live' in a way that would justify treating the ritual purely as functional infrastructure rather than partly inherited tradition. Declaring founding_problem_status as contested, with corroboration split between functionalist scholarship and mainstream ritual-studies skepticism, prevents this story from either dismissing the ritual as pure obsolete theater or crediting it with more demonstrated adaptive causation than the evidence supports.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    d5_reading_selection_ambiguity,
    'Is the survival-competence (D5) reading the historically dominant function of Passover, or is it a retrospective functionalist interpretation applied by modern scholarship to a ritual whose primary historical driver was mourning/boundary-maintenance (D1/D4) or covenantal obligation independent of either functionalist account?',
    'Historical-textual analysis of rabbinic commentary across periods to trace which framing (survival-adaptive vs. memorial-obligatory vs. covenantal) dominates explicit justificatory language at different historical moments, cross-referenced against documented community behavior during actual catastrophe events (expulsions, pogroms, forced migrations) to see whether ritual-derived competence is empirically traceable in response patterns.',
    'If historical evidence strongly favors the mourning_practice_reading as primary, this D5 story would need to be understood as a minority or emergent framing rather than a coequal structural claim; if evidence supports genuine dual function, the hybrid_transformation_reading becomes the more accurate single-file account and this story becomes a partial/derivative decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(d5_reading_selection_ambiguity, conceptual, 'Whether D5 is a defensible independent reading or an artifact of functionalist interpretive selection').

omega_variable(
    adaptive_causation_verifiability,
    'Can the claimed transmission of survival competence be empirically distinguished from general communal social capital and cultural cohesion producing the same continuity outcomes independent of this specific ritual''s content?',
    'Comparative study of diaspora communities with strong versus weak Passover observance controlling for other communal cohesion factors, examining whether observance strength predicts differential resilience/reconstitution speed after institutional disruption.',
    'If no differential effect is found, the survival-competence reading''s causal claim weakens substantially and the constraint''s coordination_function should be understood as much more modest (symbolic rehearsal rather than functioning adaptive technology); a found differential effect would strengthen this reading''s structural claim relative to its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_causation_verifiability, empirical, 'Whether the ritual causally contributes to survival outcomes or merely correlates with communities that would be resilient regardless').

omega_variable(
    theater_drift_functional_erosion,
    'Does the rising theater_ratio observed in contemporary practice (commercialized seder products, abbreviated or symbolic-only observance) indicate erosion of the actual transmitted competence, or is the core pedagogical content (the departure narrative, the questions-and-answers structure) robust to surrounding commercial/performative accretion?',
    'Content analysis of contemporary Haggadot and seder practice to assess whether the core practical-knowledge-transmission elements (departure narrative, embodied reenactment, pedagogical questioning) persist unchanged beneath added commercial/performative layers, or whether they are being displaced.',
    'If core content persists, the rising theater_ratio is cosmetic and the D5 function remains intact; if core content is being displaced by performative substitutes, the constraint may be drifting toward a piton (transmitted-competence function atrophying, ritual persisting mainly on inertia/tradition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_functional_erosion, empirical, 'Whether increasing performative elements erode or merely accompany the core transmission function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__survival_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_memory_function kernel, decomposed per the epsilon-invariance principle rather than authored as a single constraint with a measurement parameter. survival_competence_reading (this file) claims a low-extraction, decentralized-coordination structure (rope) grounded in adaptive-knowledge transmission (D5). mourning_practice_reading claims a distinct structure grounded in memorial obligation and boundary-maintenance (D1/D4), likely with different beneficiary/enforcement characteristics tied to communal identity policing. hybrid_transformation_reading claims both functions operate jointly (D1/D4 + D5) and should be expected to show a different epsilon profile than either pure reading, reflecting the combined coordination-and-identity-maintenance load. All three are linked via affects_constraints rather than merged, since averaging their epsilon values would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
