% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Folk Syncretistic Divine Legitimacy (Ancient Egypt Reading)
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the 'folk syncretistic' reading of divine
 *   legitimacy in ancient Egypt, where authority flows through household and
 *   village ritual practices that pragmatically incorporate multiple deities.
 *   This reading emphasizes local autonomy, community cohesion, and direct
 *   engagement with the divine, often operating independently of, and
 *   sometimes in quiet resistance to, the centralized state cults and
 *   pharaonic authority. It is one reading of the broader
 *   'divine_legitimacy_substrate' kernel, distinct from the
 *   'amun_polytheistic_reading' and 'atenist_monotheistic_reading'.
 *
 * KEY AGENTS:
 *   - village_communities: Primary beneficiaries and participants (moderate power/identity_locked exit)
 *   - household_units: Fundamental units of practice (powerless/identity_locked exit)
 *   - local_ritual_specialists: Agenda-setters and beneficiaries of local status (moderate power/constrained exit)
 *   - pharaoh: Excluded central authority (institutional power/analytical exit)
 *   - official_priesthoods: Excluded state religious establishment (organized power/constrained exit)
 *   - analytical_historians: Observer (analytical power/analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.2).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Folk Syncretistic Divine Legitimacy (Ancient Egypt Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, 'cb95b2ff-cf87-4151-8696-c2183a940c29').
narrative_ontology:cs_kernel_codification('cb95b2ff-cf87-4151-8696-c2183a940c29', implicit).
narrative_ontology:cs_authority_grounding('cb95b2ff-cf87-4151-8696-c2183a940c29', practice).
narrative_ontology:cs_interpretation_layer_present('cb95b2ff-cf87-4151-8696-c2183a940c29').
narrative_ontology:cs_reading_relation('cb95b2ff-cf87-4151-8696-c2183a940c29', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb95b2ff-cf87-4151-8696-c2183a940c29', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('cb95b2ff-cf87-4151-8696-c2183a940c29', foundational, divine_presence_is_manifest_in_local_ritual).
narrative_ontology:cs_axiom_status(divine_presence_is_manifest_in_local_ritual, holdable).
narrative_ontology:cs_axiom_grounding('cb95b2ff-cf87-4151-8696-c2183a940c29', divine_presence_is_manifest_in_local_ritual, theological).
narrative_ontology:cs_axiom('cb95b2ff-cf87-4151-8696-c2183a940c29', secondary, pragmatic_pluralism_of_deities_is_effective).
narrative_ontology:cs_axiom_status(pragmatic_pluralism_of_deities_is_effective, holdable).
narrative_ontology:cs_axiom_grounding('cb95b2ff-cf87-4151-8696-c2183a940c29', pragmatic_pluralism_of_deities_is_effective, conventional).
narrative_ontology:cs_reference_frame('cb95b2ff-cf87-4151-8696-c2183a940c29', ancestral_ritual_continuity).
narrative_ontology:cs_drift_state('cb95b2ff-cf87-4151-8696-c2183a940c29', atenist_interregnum, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb95b2ff-cf87-4151-8696-c2183a940c29', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_units).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, household_units).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, local_autonomy_of_worship).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, pragmatic_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary participants and beneficiaries of the ritual practices. They invest time, resources, and social effort into maintaining these traditions, which in turn provide social cohesion, moral order, and a sense of cosmic belonging. Their identity is deeply intertwined with these shared practices.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities, payer).

% The fundamental unit of ritual practice, performing daily offerings and maintaining household shrines. They derive personal and familial meaning, protection, and continuity from these rituals, which are integral to their daily life and self-conception.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_units, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, household_units, payer).

% Individuals (e.g., elders, healers, diviners) who guide and facilitate local rituals, interpret omens, and mediate with deities. They derive social status, influence, and often material support from their role, which is grounded in community acceptance and traditional knowledge rather than state appointment.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists, beneficiary).

% The distant central authority, whose legitimacy is theoretically divine but practically derived from state cults and military power. This folk reading of divine legitimacy operates largely independently of, and often in quiet defiance of, pharaonic claims to exclusive mediation with the gods. The pharaoh's attempts to impose religious uniformity (e.g., Atenism) met significant resistance from these deeply embedded local practices.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh, excluded,
    institutional, civilizational, analytical, national).

% The established priestly class, primarily serving state cults (e.g., Amun). They view folk practices as unsophisticated or potentially heterodox, but generally tolerate them as long as they don't challenge central authority. Their own legitimacy is not derived from these folk practices, and they have limited direct influence over them.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, official_priesthoods, excluded,
    organized, generational, constrained, national).

% Modern scholars who reconstruct and analyze ancient Egyptian religious practices from archaeological, textual, and anthropological evidence. They observe the structural dynamics of this constraint without being subject to its operation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, diffuse).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__folk_syncretistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable local communities and households to maintain social cohesion, moral order, and a sense of cosmic belonging through shared ritual practices, independent of distant central authority.
% TRANSFER_FUNCTION: Transfers social meaning, collective identity, spiritual comfort, and a sense of continuity to participants; transfers time, effort, and material resources into ritual practice and offerings.
% ABSENT_VOICES: The pharaoh and official priesthoods are largely absent from the direct operation of this constraint. If present, they would assert their exclusive authority over divine legitimacy and attempt to centralize or standardize religious practice, which would be resisted by the folk.
% DISAPPEARANCE_RATIONALE: If these deeply embedded household and village ritual practices vanished overnight, the entire social and moral order of local communities would collapse. Social cohesion would erode, traditional identities would fragment, and the primary means of coping with life's uncertainties would be lost, leading to profound societal disorganization at the local level.
% FOUNDING_PROBLEM: How to maintain social cohesion, explain natural phenomena, provide meaning in daily life, and secure divine favor at the local level, often in contexts where central state religion was distant, inaccessible, or irrelevant to daily concerns.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of similar folk religions in other cultures, archaeological evidence of numerous household shrines and local cult sites, and the historical persistence of these practices despite attempts at top-down religious reform (e.g., Atenism) corroborate that these problems are perennial for local communities and that this constraint addresses them effectively from the folk perspective.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it genuinely solves a collective-action problem (social cohesion, meaning-making) for local communities with minimal coercive overhead from within the folk system. Extractiveness is low (0.15) as the 'costs' (time, effort, offerings) are largely self-imposed and directly tied to perceived benefits. Suppression is low (0.2) because adherence is driven by social norms and identity, not active external enforcement. Theater ratio is low (0.1) as the practices are deeply meaningful and functional for participants, not performative for external audiences. Accessibility collapse is high (0.8) because for the folk, these practices are the primary and often only accessible means of engaging with the divine. Resistance is moderate (0.5) reflecting the folk's resilience against external attempts to alter or suppress their traditions, particularly during periods like the Atenist interregnum (c. 1350-1330 BCE), where suppression requirements briefly increased but the folk practices largely remained stable.
 *
 * PERSPECTIVAL GAP:
 *   The folk communities and local ritual specialists experience this constraint as a beneficial, self-organizing Rope, essential for their social and spiritual well-being. In contrast, the pharaoh and official priesthoods, from their excluded positions, would likely view these practices as either irrelevant, potentially heterodox, or at best, a diffuse substrate that implicitly supports state stability without being under their direct control. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Village communities, household units, and local ritual specialists are beneficiaries, as they directly gain social cohesion, meaning, and status from the practices. They also bear the costs (time, effort), making their directionality near symmetric or slightly beneficiary. The pharaoh and official priesthoods are excluded from the direct operation and benefit of this specific reading, though they might indirectly benefit from the social stability it provides. There are no identifiable 'victims' within this reading, as participation is largely voluntary and beneficial from the folk perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_concentration_ambiguity,
    'Is the benefit of these folk practices truly diffuse across the community, or does it concentrate disproportionately on local ritual specialists (e.g., through enhanced status, material support, or control over social narratives)?',
    'Detailed ethnographic studies of analogous contemporary folk religious systems, or more granular archaeological evidence of resource distribution within ancient villages.',
    'If benefits are found to be significantly concentrated, the constraint''s extractiveness would be re-evaluated upward, potentially shifting its classification towards a Tangled Rope for the broader community, with local specialists as beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration_ambiguity, empirical, 'Ambiguity regarding the distribution of benefits from folk religious practices.').

omega_variable(
    implicit_state_legitimation,
    'To what extent did the social stability and moral order provided by these folk practices implicitly legitimize or support the broader pharaonic state, even when operating outside its direct control?',
    'Comparative historical analysis of societies where folk religion was actively suppressed versus tolerated, examining long-term state stability and resistance levels.',
    'If significant implicit legitimation is found, the pharaoh and official priesthoods, though excluded from direct operation, could be re-classified as indirect beneficiaries, altering the overall beneficiary structure and potentially the constraint''s systemic role.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_state_legitimation, conceptual, 'Whether folk religious practices, by providing social stability, implicitly legitimized the state.').

omega_variable(
    resistance_effectiveness_measurement,
    'How effectively did the ''resistance'' of folk practices (measured at 0.5) actually prevent top-down religious reforms from altering daily life, versus merely delaying or deflecting them?',
    'More precise archaeological and textual analysis of post-Atenist religious life at the village and household level, quantifying the degree of return to pre-Atenist norms versus lasting changes.',
    'If resistance was less effective than assumed, the suppression metric from central authorities might be higher, and the folk''s exit options more ''constrained'' than ''identity_locked'' in the face of state power, potentially shifting the constraint''s classification towards a Snare during periods of state imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_effectiveness_measurement, empirical, 'The true effectiveness of folk resistance to external religious imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 1500, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1500, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(divi_tr_t1400, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(divi_tr_t1350, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1350, 0.08).
narrative_ontology:measurement(divi_tr_t1300, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(divi_tr_t1100, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1100, 0.1).
narrative_ontology:measurement(divi_tr_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1000, 0.1).

% Extraction over time
narrative_ontology:measurement(divi_be_t1500, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(divi_be_t1400, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1400, 0.16).
narrative_ontology:measurement(divi_be_t1350, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1350, 0.14).
narrative_ontology:measurement(divi_be_t1300, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1300, 0.15).
narrative_ontology:measurement(divi_be_t1100, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1100, 0.15).
narrative_ontology:measurement(divi_be_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1500, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1500, 0.2).
narrative_ontology:measurement(divi_su_t1400, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1400, 0.2).
narrative_ontology:measurement(divi_su_t1350, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1350, 0.25).
narrative_ontology:measurement(divi_su_t1300, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1300, 0.2).
narrative_ontology:measurement(divi_su_t1100, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1100, 0.2).
narrative_ontology:measurement(divi_su_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1000, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_legitimacy_substrate' kernel, representing the local, pragmatic, and pluralistic folk religious practices. It coexists with and influences the more centralized Amun polytheistic and Atenist monotheistic readings by providing a resilient, alternative source of divine legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
