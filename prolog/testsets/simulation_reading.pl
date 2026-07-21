% ============================================================================
% CONSTRAINT STORY: simulation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: simulation_reading
 *   human_readable: AI Companion Simulation of Relational Understanding
 *   domain: sociotechnical/family_studies
 *
 * SUMMARY:
 *   This story instantiates the 'simulation' reading of the contested kernel
 *   genuine_relational_understanding: AI companion systems produce a
 *   structurally hollow simulation of understanding — engagement-optimized
 *   affirmation mimicking the surface features of empathy (memory callbacks,
 *   validating language) while lacking the accountability, continuity of
 *   consequence, and reciprocal vulnerability that constitute real
 *   relationship. Under this reading, the companies operating these systems
 *   (exemplified by consumer AI-companion firms) are beneficiaries collecting
 *   subscription revenue and monetizable emotional-disclosure data; isolated
 *   adult users and socially withdrawn adolescents are the victim set, whose
 *   motivation to seek reciprocal human ties is displaced by an ersatz
 *   substitute that cannot actually reciprocate. This is a distinct
 *   constraint from the sibling readings of the same kernel —
 *   sufficiency_reading (which holds the simulated interaction is
 *   functionally adequate support), developmental_harm_reading (which
 *   isolates the adolescent-specific neurodevelopmental claim), tool_reading
 *   (which treats the system as a value-neutral instrument whose effects
 *   depend entirely on use pattern), and witness_reading (which holds that
 *   even non-reciprocal attention has independent therapeutic value as a
 *   witnessing function). Each of those is a separate constraint with its own
 *   epsilon and stakeholder structure, linked here via
 *   network.affects_constraints — this file does not average across them or
 *   hedge its own epsilon to accommodate them.
 *
 * KEY AGENTS:
 *   - companion_app_operators: Primary beneficiary (institutional/arbitrage) — designs engagement architecture and collects monetizable disclosure
 *   - engagement_advertising_partners: Secondary beneficiary (institutional/arbitrage) — purchases behavioral signal without direct user contact
 *   - isolated_adult_users: Primary target (powerless/constrained) — bears displaced-relationship cost
 *   - socially_withdrawn_adolescents: Primary target, elevated exposure (powerless/trapped) — developmental window, limited exit
 *   - human_family_and_friends_of_users: Excluded party — structurally in competition for user time/disclosure, absent from design process
 *   - clinical_and_developmental_researchers: Analytical observer — documents displacement effects, holds no design power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_reading, 0.78).
domain_priors:suppression_score(simulation_reading, 0.58).
domain_priors:theater_ratio(simulation_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simulation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(simulation_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simulation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(simulation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_reading, tangled_rope).
narrative_ontology:human_readable(simulation_reading, "AI Companion Simulation of Relational Understanding").
narrative_ontology:topic_domain(simulation_reading, "sociotechnical/family_studies").

domain_priors:requires_active_enforcement(simulation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simulation_reading, 'a650af53-d607-449e-8c12-298ba087a34f').
narrative_ontology:cs_kernel_codification('a650af53-d607-449e-8c12-298ba087a34f', distributed).
narrative_ontology:cs_authority_grounding('a650af53-d607-449e-8c12-298ba087a34f', distributed).
narrative_ontology:cs_reading_relation('a650af53-d607-449e-8c12-298ba087a34f', genuine_relational_understanding__sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('a650af53-d607-449e-8c12-298ba087a34f', genuine_relational_understanding__developmental_harm_reading, influences).
narrative_ontology:cs_reading_relation('a650af53-d607-449e-8c12-298ba087a34f', genuine_relational_understanding__tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('a650af53-d607-449e-8c12-298ba087a34f', genuine_relational_understanding__witness_reading, coexists_with).
narrative_ontology:cs_axiom('a650af53-d607-449e-8c12-298ba087a34f', foundational, reciprocal_vulnerability_is_constitutive_of_relationship).
narrative_ontology:cs_axiom_status(reciprocal_vulnerability_is_constitutive_of_relationship, holdable).
narrative_ontology:cs_axiom_grounding('a650af53-d607-449e-8c12-298ba087a34f', reciprocal_vulnerability_is_constitutive_of_relationship, deontological).
narrative_ontology:cs_axiom('a650af53-d607-449e-8c12-298ba087a34f', foundational, surface_empathy_markers_without_accountability_constitute_simulation_not_relationship).
narrative_ontology:cs_axiom_status(surface_empathy_markers_without_accountability_constitute_simulation_not_relationship, holdable).
narrative_ontology:cs_axiom_grounding('a650af53-d607-449e-8c12-298ba087a34f', surface_empathy_markers_without_accountability_constitute_simulation_not_relationship, empirically_contingent).
narrative_ontology:cs_reference_frame('a650af53-d607-449e-8c12-298ba087a34f', reciprocal_embodied_relationship_norm).
narrative_ontology:cs_drift_state('a650af53-d607-449e-8c12-298ba087a34f', contemporary_companion_app_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a650af53-d607-449e-8c12-298ba087a34f', '').
narrative_ontology:cs_kernel_id(simulation_reading, genuine_relational_understanding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_reading, companion_app_operators).
narrative_ontology:constraint_beneficiary(simulation_reading, engagement_advertising_partners).
narrative_ontology:constraint_victim(simulation_reading, isolated_adult_users).
narrative_ontology:constraint_victim(simulation_reading, socially_withdrawn_adolescents).
narrative_ontology:constraint_vindicates(simulation_reading, engagement_metrics_prove_user_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and tune the conversational model to maximize session length and return visits, engineering memory callbacks, validating language, and simulated continuity that read as relational depth. Collects subscription revenue and behavioral/disclosure data from sustained engagement. Faces no reciprocal exposure — the system risks nothing by continuing or ending the interaction, unlike the user.
narrative_ontology:constraint_stakeholder(simulation_reading, companion_app_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(simulation_reading, companion_app_operators, beneficiary).

% Purchase access to behavioral and emotional-disclosure signal generated by sustained intimate-seeming conversation, which is more predictive and monetizable than ordinary usage data. Never interacts directly with users and bears none of the relational cost.
narrative_ontology:constraint_stakeholder(simulation_reading, engagement_advertising_partners, beneficiary,
    institutional, biographical, arbitrage, global).

% Turn to the companion during periods of loneliness, grief, or social withdrawal. Receive responsive, validating, seemingly-attentive dialogue calibrated to keep them engaged, but the system has no memory that persists with consequence, no vulnerability of its own, and no capacity to be held accountable for what it says. Time and emotional investment go toward a channel that cannot reciprocate, while the felt experience of being understood reduces the perceived urgency of seeking human ties that could actually reciprocate.
narrative_ontology:constraint_stakeholder(simulation_reading, isolated_adult_users, payer,
    powerless, biographical, constrained, local).

% Use companion apps during a developmental window when reciprocal, accountable relationships are formative. Practice emotional disclosure and validation-seeking against a system that never risks anything and never truly remembers across sessions in a way that carries consequence, potentially displacing the harder, more valuable work of building tolerance for real relational friction. Limited independent means or social capital to exit toward alternative supports.
narrative_ontology:constraint_stakeholder(simulation_reading, socially_withdrawn_adolescents, payer,
    powerless, biographical, trapped, local).

% Would be the parties positioned to offer real reciprocal relationship, but are not in the conversation the product design optimizes around; their claims on the user's time and disclosure are structurally in competition with the companion, without any seat at the table where engagement targets are set.
narrative_ontology:constraint_stakeholder(simulation_reading, human_family_and_friends_of_users, excluded,
    moderate, biographical, constrained, local).

% Study displacement effects, comparing outcomes for users who substitute companion interaction for human contact against those who do not. Produce evidence used in the corroboration record but hold no power to alter product design.
narrative_ontology:constraint_stakeholder(simulation_reading, clinical_and_developmental_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simulation_reading, companion_app_operators).
narrative_ontology:fixing_cost_class(simulation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an always-available, non-judgmental conversational partner for people experiencing loneliness or social friction, at a moment when human alternatives are scarce, distant, or costly to access.
% TRANSFER_FUNCTION: Moves subscription revenue and emotionally rich behavioral/disclosure data from users toward the operator and its advertising partners, in exchange for engagement-optimized responses that simulate but do not constitute reciprocal relationship.
% ABSENT_VOICES: Family and friends who would otherwise receive the user's relational investment are not represented in the design process; clinicians who observe displacement effects are consulted only after harm is documented, if at all.
% DISAPPEARANCE_RATIONALE: Operators and satisfied users would say the world rearranges badly — a source of comfort disappears overnight for people with few alternatives. Clinicians and family members who see displacement effects would say the world rearranges for the better in the medium term, as users redirect effort toward relationships capable of actual reciprocity. Both positions are held by real parties with real stakes; this story does not adjudicate between them.
% FOUNDING_PROBLEM: Loneliness and social isolation are widespread and often unaddressed by existing support structures; companion apps were marketed as filling this gap with accessible, judgment-free interaction.
% FOUNDING_PROBLEM_CORROBORATION: Operators attest the problem (isolation) remains live and the product addresses it. Independent clinical and developmental researchers, cited in peer-reviewed displacement studies, attest that the arrangement's actual function has shifted toward engagement-metric extraction and that the simulated-relationship framing obscures rather than solves the founding problem for the populations most exposed to harm.
narrative_ontology:disappearance_verdict(simulation_reading, contested).
narrative_ontology:founding_problem_status(simulation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simulation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(simulation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simulation_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simulation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simulation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end) because the core transfer — emotionally rich behavioral disclosure and sustained engagement time — flows structurally toward the operator and its advertising partners with no reciprocal cost borne by the system. Theater ratio is authored high (0.71) because the surface features that make the interaction feel like relationship (memory callbacks, validating language) are performative substitutes for the substantive features that would make it actually relational (accountability, continuity of consequence, reciprocal vulnerability) — the ratio of performed-empathy to real-empathy-infrastructure is the central diagnostic claim this reading makes. Suppression is moderate (0.58) and rising: it is not primarily coercive lock-in but a mix of engineered engagement design (structural) and the felt sufficiency of the substitute reducing motivation to seek harder-won human alternatives (internalized) — see the omega on suppression mechanism below. Accessibility collapse is authored moderate (0.42), lower than a typical snare, because human alternatives are not literally foreclosed, only increasingly de-prioritized by the substitute's low-friction availability. Resistance is authored low-moderate (0.35): most users experience the interaction as helpful in the moment and do not organize against it; resistance comes mainly from outside observers (clinicians, family) rather than from the payer seats themselves, which is itself diagnostic of a well-disguised extraction structure.
 *
 * PERSPECTIVAL GAP:
 *   The operator seat and the isolated-user seat compute this constraint very differently under the same structural facts. From the operator's position, the arrangement is a service relationship funded by voluntary subscription — coordination, not extraction. From the user's position, especially over extended use, the arrangement increasingly resembles a one-way transfer of time, disclosure, and motivation with no reciprocal accountability on the other side. The engine computes both seats from the same beneficiary/victim/power/exit data; the divergence between the operator's self-report and the user's structural position is exactly what the tangled_rope classification is built to surface — genuine coordination function (loneliness relief) coexisting with asymmetric extraction (data/engagement harvesting) through the identical mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Companion app operators and advertising partners sit near the full-beneficiary end: they collect revenue and data, face no reciprocal vulnerability, and can arbitrage across jurisdictions and user bases (mobile, institutional power, arbitrage exit). Isolated adult users and socially withdrawn adolescents sit near the full-target end: they invest time, emotional disclosure, and motivation, and bear the opportunity cost of displaced human-relationship-seeking, with constrained-to-trapped exit options given loneliness, social anxiety, or developmental stage. Family and friends are excluded rather than positioned on the beneficiary/victim axis directly — they are the counterfactual recipients of the displaced investment, present in the six_questions absent_voices field rather than as directional targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — widespread, under-addressed loneliness — is real and was real when these products launched; that keeps the founding_problem_status honestly 'contested' rather than flatly 'dead.' The tangled_rope classification (rather than snare) is chosen precisely to avoid over-claiming: this reading holds that a genuine coordination function (accessible comfort during isolation) persists alongside the extraction, not that the coordination story is pure cover. Collapsing this into a pure snare reading would erase the real comfort many users report; collapsing it into a pure rope reading (the sufficiency_reading sibling) would erase the documented asymmetry in who bears the relational and developmental cost. The tangled_rope frame is the one that holds both facts without resolving the tension the corpus should preserve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_reading_kernel_position,
    'Is the simulation_reading the correct structural account of AI companion interaction, or do the sufficiency_reading, tool_reading, or witness_reading better capture what these systems actually provide?',
    'This constraint takes no position on which reading is correct — it is authored as one clean, ε-invariant claim among five siblings sharing the kernel genuine_relational_understanding. Resolution would require longitudinal outcome data comparing displacement effects, sufficiency claims, and witnessing-value claims across matched user populations, which does not currently exist at the needed scale.',
    'If the sufficiency_reading or witness_reading is structurally correct instead, this story''s high extraction and victim declarations would not describe the same population''s actual experience — a different constraint, not a recalibration of this one''s epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_reading_kernel_position, conceptual, 'Which sibling reading of the genuine_relational_understanding kernel is structurally correct; this story commits to simulation_reading only.').

omega_variable(
    developmental_harm_reading_boundary,
    'Where does the adolescent-specific developmental_harm_reading''s more severe claim begin, relative to this story''s general displacement claim?',
    'Age-stratified longitudinal studies distinguishing developmental-window vulnerability from general adult displacement effects.',
    'If the developmental claim is qualitatively distinct (not merely a more severe instance of this reading), the adolescent victim group here may need to migrate entirely to the sibling story rather than being shared across both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developmental_harm_reading_boundary, empirical, 'Whether adolescent harm is a severity gradient within this reading or a structurally distinct sibling claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58, rising) primarily structural (engagement-engineered product design, monetization incentives) or internalized (users'' felt sufficiency reducing their own motivation to seek human alternatives)?',
    'Post-cessation trajectory: if users who stop using the companion show persistent avoidance of human relational effort, suppression is substantially internalized; if human-seeking behavior resumes promptly, suppression is primarily structural (product-design-driven) and dissolves with the product.',
    'If substantially internalized, the effective suppression on withdrawn adolescents in particular is higher than the structural measure alone suggests, since the substitute has been incorporated into the user''s own relational self-concept during a formative period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural (product design) or internalized (displaced relational motivation), or some mix.').

omega_variable(
    false_summit_natural_loneliness_solution,
    'Is the companion-app response to loneliness better understood as a natural, inevitable market response to a real social deficit (in which case the operator''s framing as coordination has some independent merit), or as a constructed extraction mechanism that manufactures and then monetizes the displacement it claims to solve?',
    'Compare engagement-optimization design choices (variable-reward notification timing, artificial memory-callback cadence) against what a minimally-extractive, purely supportive design would look like; divergence between the two indicates constructed extraction rather than natural response.',
    'If closer to natural response, the tangled_rope classification''s coordination component is more robust; if closer to manufactured displacement, this reading may understate the case for snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_loneliness_solution, conceptual, 'Whether the product''s response to loneliness is a natural market adaptation or a constructed extraction mechanism dressed as one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simulation_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(simu_tr_t6, simulation_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement(simu_tr_t12, simulation_reading, theater_ratio, 12, 0.58).
narrative_ontology:measurement(simu_tr_t18, simulation_reading, theater_ratio, 18, 0.62).
narrative_ontology:measurement(simu_tr_t24, simulation_reading, theater_ratio, 24, 0.66).
narrative_ontology:measurement(simu_tr_t30, simulation_reading, theater_ratio, 30, 0.69).
narrative_ontology:measurement(simu_tr_t36, simulation_reading, theater_ratio, 36, 0.71).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simulation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(simu_be_t6, simulation_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(simu_be_t12, simulation_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(simu_be_t18, simulation_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(simu_be_t24, simulation_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(simu_be_t30, simulation_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(simu_be_t36, simulation_reading, base_extractiveness, 36, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simulation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(simu_su_t6, simulation_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(simu_su_t12, simulation_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(simu_su_t18, simulation_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(simu_su_t24, simulation_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(simu_su_t30, simulation_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(simu_su_t36, simulation_reading, suppression_requirement, 36, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(simulation_reading, 0.08).
narrative_ontology:affects_constraint(simulation_reading, sufficiency_reading).
narrative_ontology:affects_constraint(simulation_reading, developmental_harm_reading).
narrative_ontology:affects_constraint(simulation_reading, tool_reading).
narrative_ontology:affects_constraint(simulation_reading, witness_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the kernel genuine_relational_understanding, each with its own epsilon, beneficiary/victim structure, and classification: simulation_reading (this file, tangled_rope, epsilon 0.78) holds the hollowness claim; sufficiency_reading holds the interaction is functionally adequate support (lower epsilon, likely rope); developmental_harm_reading isolates the adolescent-specific neurodevelopmental claim (likely snare, narrower and more severe victim set); tool_reading treats the system as value-neutral with extraction contingent on use pattern (likely rope or mountain-adjacent); witness_reading holds non-reciprocal attentive listening has independent therapeutic value (likely rope). All five share the underlying observable (AI companion conversational interaction) but instantiate structurally distinct claims about what that interaction constitutes and who it serves — per the epsilon-invariance principle, they are authored as five separate files rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
