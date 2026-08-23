% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocal Obligation: Pharaoh's Duty to Maintain Cosmic Balance
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   The reciprocity reading of Ma'at holds that cosmic order (Ma'at) is
 *   maintained through mutual obligations: Pharaoh must provide justice,
 *   stability, and proper resource distribution (via redistribution, temple
 *   economies, corvée management), and in return the population owes labor,
 *   taxes, and ritual participation. Crucially, Pharaoh is SUBJECT to Ma'at —
 *   failed obligations justify resistance or withdrawal of support. This
 *   creates a moderate extraction ceiling grounded in the reciprocity norm.
 *   The constraint is actively enforced through priesthood interpretation,
 *   bureaucratic administration, and ritual performance. It coordinates
 *   resource allocation and identity (Egyptian cosmic citizenship) while
 *   extracting labor and surplus — a genuine tangled rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.45).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.4).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocal Obligation: Pharaoh's Duty to Maintain Cosmic Balance").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'addcecde-12e7-4541-b364-a78a3a0e949b').
narrative_ontology:cs_kernel_codification('addcecde-12e7-4541-b364-a78a3a0e949b', fixed_text).
narrative_ontology:cs_authority_grounding('addcecde-12e7-4541-b364-a78a3a0e949b', lineage).
narrative_ontology:cs_interpretation_layer_present('addcecde-12e7-4541-b364-a78a3a0e949b').
narrative_ontology:cs_reading_relation('addcecde-12e7-4541-b364-a78a3a0e949b', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('addcecde-12e7-4541-b364-a78a3a0e949b', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('addcecde-12e7-4541-b364-a78a3a0e949b', foundational, pharaoh_subject_to_maat).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_maat, holdable).
narrative_ontology:cs_axiom_grounding('addcecde-12e7-4541-b364-a78a3a0e949b', pharaoh_subject_to_maat, conventional).
narrative_ontology:cs_axiom('addcecde-12e7-4541-b364-a78a3a0e949b', foundational, reciprocity_binds_extraction).
narrative_ontology:cs_axiom_status(reciprocity_binds_extraction, holdable).
narrative_ontology:cs_axiom_grounding('addcecde-12e7-4541-b364-a78a3a0e949b', reciprocity_binds_extraction, conventional).
narrative_ontology:cs_axiom('addcecde-12e7-4541-b364-a78a3a0e949b', secondary, failed_obligation_justifies_resistance).
narrative_ontology:cs_axiom_status(failed_obligation_justifies_resistance, holdable).
narrative_ontology:cs_axiom_grounding('addcecde-12e7-4541-b364-a78a3a0e949b', failed_obligation_justifies_resistance, conventional).
narrative_ontology:cs_reference_frame('addcecde-12e7-4541-b364-a78a3a0e949b', reciprocal_kingship_framework).
narrative_ontology:cs_drift_state('addcecde-12e7-4541-b364-a78a3a0e949b', late_new_kingdom_imperial_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('addcecde-12e7-4541-b364-a78a3a0e949b', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, egyptian_population).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, bureaucracy).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, peasantry).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, conscripted_laborers).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, cosmic_balance_through_reciprocal_obligation).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, pharaonic_legitimacy_conditional_on_maat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Ma'at through law, redistribution, temple endowments, and military defense. Embodies the reciprocal obligation: must provide justice, stability, and proper distribution. Collects surplus via taxation and corvée but is bound by reciprocity norm — over-extraction risks loss of legitimacy, priesthood opposition, and justified resistance. Identity fused with kingship role; exit means cosmic and political death.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).

% Interprets and ritualizes Ma'at; controls temple economies (land, labor, redistribution). Benefits from Pharaoh's endowments and Ma'at-performance. Can constrain Pharaoh through oracular authority and ritual legitimacy. Exit constrained by temple estates and religious identity; could defect to rival cults but loses institutional position.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, priesthood, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, priesthood, agenda_setter).

% Manages taxation, corvée allocation, granaries, and justice administration. Collects rents from position (corruption, patronage). Depends on Pharaonic Ma'at-performance for institutional stability. Exit constrained by specialized skills and institutional embeddedness; could serve foreign powers but loses status.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Provides agricultural surplus, corvée labor, and military service. Receives flood management, grain redistribution in famine, local justice, and cosmic order maintenance. Exit nearly impossible: tied to land, monitored by bureaucracy, cosmic fear of desert (isfet). Resistance takes forms of flight, banditry, tomb robbery, or petition — all high-risk.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Drafted for state projects (pyramids, temples, quarries, military campaigns). Receive rations and cosmic merit but bear extreme physical cost. No exit during service; desertion punished severely. Their labor is the extraction base for monumental Ma'at-performance.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, conscripted_laborers, payer,
    powerless, immediate, trapped, local).

% Serve Egyptian state but stand outside Ma'at reciprocity. Not bound by cosmic obligations, not entitled to Ma'at-justice. Their exclusion defines the boundary of the reciprocal community. Would argue for contractual rather than cosmic obligation if present.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, foreign_mercenaries, excluded,
    moderate, biographical, mobile, regional).

% Sees the full structure: Ma'at as a coordination-extraction system that stabilized Nile civilization for millennia through reciprocal obligation, with a genuine ceiling on extraction that distinguished it from pure despotism, but which degraded as imperial extraction outpaced reciprocity enforcement.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, cosmic_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Nile flood agriculture, surplus redistribution, defense, and cosmic-order maintenance across a unified polity — solving the collective action problem of large-scale hydraulic civilization through a single sacred framework.
% TRANSFER_FUNCTION: Moves agricultural surplus, corvée labor, and ritual participation from population to Pharaoh/state/temples; moves justice, stability, famine relief, cosmic legitimacy, and identity (Egyptian-ness) from Pharaoh/state to population.
% ABSENT_VOICES: Women's voices in Ma'obligation (beyond queen/goddess roles); foreign residents (Nubians, Libyans, Asiatics) subject to extraction but excluded from Ma'at-reciprocity; dissenting theological voices (Atenist interlude, heterodox priesthoods) suppressed by orthodoxy.
% DISAPPEARANCE_RATIONALE: If Ma'at-reciprocity vanished overnight, Pharaonic legitimacy would collapse, redistribution systems would fail, corvée would become pure coercion, priesthood would lose interpretive monopoly, and the polity would fragment into warlordism or foreign conquest — as seen in Intermediate Periods.
% FOUNDING_PROBLEM: Coordinating large-scale hydraulic agriculture, defense, and cosmic-order maintenance in a unified Nile polity required a single framework that bound ruler and ruled in mutual obligation, preventing both tyrannical over-extraction and chaotic under-coordination.
% FOUNDING_PROBLEM_CORROBORATION: Middle Kingdom texts (Prophecy of Neferti, Admonitions of Ipuwer, Loyalist Teaching) attest the reciprocity norm as live and binding. New Kingdom imperial practice and Late Period theological treatises attest its attenuation. Egyptological consensus (Assmann, Hornung, Kemp) corroborates the Middle Kingdom as peak reciprocity articulation; the founding problem's status is contested between 'live but transformed' and 'dead but ritualized' readings.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: Pharaoh extracts substantial surplus (taxes, corvée, temple donations) but reciprocity norm and justified resistance create a ceiling. Suppression 0.4: structural enforcement exists (state monopoly, ideological apparatus) but resistance is normatively licensed, creating exit pressure. Theater 0.3: rituals (Opet Festival, Sed Festival) are functional coordination but increasingly performative in Late Period. Accessibility collapse 0.5: alternatives (foreign mercenary service, banditry, religious heterodoxy) exist but carry cosmic/social death. Resistance 0.55: historical record shows periodic successful resistance (First Intermediate Period, Harem Conspiracy, tomb robberies as economic protest) but high individual cost.
 *
 * PERSPECTIVAL GAP:
 *   From Pharaoh's seat: Ma'at is the coordination mechanism that makes rule possible — genuine rope. From peasant seat: Ma'at is the ideology that makes extraction feel like cosmic duty — snare. From priesthood seat: Ma'at is the source of their interpretive authority — beneficiary. The engine computes this divergence from the structural data; the reciprocity reading's claimed tangled_rope captures the structural truth that all three seats are simultaneously real.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh (agenda_setter, institutional power, identity_locked exit) sits near d=0.3: benefits from extraction but bound by reciprocity norm — constraint subsidizes legitimacy while extracting. Priesthood/bureaucracy (beneficiary, organized power, constrained exit) sit near d=0.25: collect rents from interpretation/administration but depend on Pharaoh's Ma'at-performance. Peasantry/conscripts (payer, powerless, trapped) sit near d=0.85: bear extraction with minimal exit. Population broadly (beneficiary, powerless, constrained) sit near d=0.5: receive justice/distribution but pay for it. The reciprocity norm compresses the d-spread relative to divine_mandate_reading where Pharaoh would be d≈0.1.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating Nile flood agriculture, defense, and cosmic order in a unified polity) remains live but transformed: the reciprocity norm that capped extraction atrophied as Pharaonic power centralized (New Kingdom imperial extraction), then the constraint persisted as piton-theater in Late Period/Ptolemaic when Pharaohs were foreign and Ma'at became purely performative. The reciprocity reading specifically captures the Middle Kingdom articulation where the ceiling was most operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Ma''at kernel, or does it collapse into the divine_mandate_reading in practice?',
    'Textual analysis of Middle Kingdom wisdom literature (e.g., Prophecy of Neferti, Admonitions of Ipuwer) vs. New Kingdom royal inscriptions: do they articulate Pharaoh''s obligation as conditional (reciprocity) or inherent (divine mandate)?',
    'If reciprocal language is rhetorical only and Pharaoh''s Ma''at-embodiment is treated as ontological in practice, the reciprocity_reading collapses into divine_mandate_reading and extraction ceiling vanishes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the reciprocity reading is a live structural alternative or a textual veneer over divine mandate.').

omega_variable(
    extraction_ceiling_enforceability,
    'Does the reciprocity norm (moderate extraction ceiling) have operational teeth, or is resistance/withdrawal of support purely theoretical?',
    'Historical record of successful resistance to Pharaonic over-extraction: Harem Conspiracy, tomb robberies as protest, First Intermediate Period breakdowns, priesthood interventions. Measure whether ''justified resistance'' ever constrained extraction.',
    'If resistance never materially constrained Pharaonic extraction, the ceiling is theoretical and the constraint operates as snare; if resistance did constrain, tangled_rope with genuine reciprocity holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_ceiling_enforceability, empirical, 'Whether the reciprocity norm''s extraction ceiling was enforceable in practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression of alternatives to Ma''at-order structural (state monopoly on violence, ideological monopoly) or internalized (cosmic fear, identity fusion with Egyptian order)?',
    'Compare suppression intensity during periods of strong central state (Old Kingdom, New Kingdom) vs. intermediate periods: if suppression persists without state enforcement, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure suggests; targets carry cosmic-order internalization after exit attempts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cosmic-order constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_reciprocity_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(maat_reciprocity_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(maat_reciprocity_tr_t40, maat_order_principle__reciprocity_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(maat_reciprocity_tr_t60, maat_order_principle__reciprocity_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(maat_reciprocity_tr_t80, maat_order_principle__reciprocity_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(maat_reciprocity_tr_t100, maat_order_principle__reciprocity_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(maat_reciprocity_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maat_reciprocity_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(maat_reciprocity_be_t40, maat_order_principle__reciprocity_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(maat_reciprocity_be_t60, maat_order_principle__reciprocity_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(maat_reciprocity_be_t80, maat_order_principle__reciprocity_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(maat_reciprocity_be_t100, maat_order_principle__reciprocity_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(maat_reciprocity_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maat_reciprocity_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(maat_reciprocity_su_t40, maat_order_principle__reciprocity_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(maat_reciprocity_su_t60, maat_order_principle__reciprocity_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(maat_reciprocity_su_t80, maat_order_principle__reciprocity_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(maat_reciprocity_su_t100, maat_order_principle__reciprocity_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.12).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, pharaonic_taxation).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, temple_economy).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, corvee_labor_system).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, nile_flood_management).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, royal_succession).

% DUAL FORMULATION NOTE:
% Part of maat_order_principle constraint family. This reciprocity_reading extracts the conditional-obligation strand; divine_mandate_reading extracts the ontological-embodiment strand; distributed_maintenance_reading extracts the universal-duty strand. All three share the Ma'at kernel but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, institutional, 0.3).
constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, organized, 0.25).
constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
