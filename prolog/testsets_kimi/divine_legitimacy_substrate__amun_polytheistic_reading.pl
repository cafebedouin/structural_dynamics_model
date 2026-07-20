% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Polytheistic Priesthood Legitimacy Constraint
 *   domain: ancient_history/religious_studies/political_economy
 *
 * SUMMARY:
 *   In the New Kingdom Egyptian political economy, divine legitimacy is
 *   mediated through the priestly interpretation of a multi-deity cosmos with
 *   Amun-Ra as chief patron. This reading of the divine_legitimacy_substrate
 *   kernel distributes interpretive authority across the Amun priesthood
 *   while accommodating regional variation. Pharaoh requires priestly
 *   validation to rule legitimately, temple economies accumulate surplus, and
 *   rural cultivators bear the cost. The constraint is claimed by the
 *   priesthood as necessary cosmic order but operates with substantial
 *   asymmetric extraction.
 *
 * KEY AGENTS:
 *   - amun_priesthood: Agenda-setter and beneficiary (institutional/identity_locked) â controls interpretation and legitimacy
 *   - temple_economies: Beneficiary (institutional/constrained) â accumulates surplus
 *   - pharaoh: Payer/beneficiary (powerful/constrained) â receives legitimacy, loses autonomy
 *   - rural_cultivators: Payer (powerless/trapped) â provides taxes and labor
 *   - folk_practitioners: Excluded (powerless/trapped) â practices marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.72).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.65).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Polytheistic Priesthood Legitimacy Constraint").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, 'd8a48062-92fa-4d60-b05c-07d614221cdb').
narrative_ontology:cs_kernel_codification('d8a48062-92fa-4d60-b05c-07d614221cdb', formalized).
narrative_ontology:cs_authority_grounding('d8a48062-92fa-4d60-b05c-07d614221cdb', lineage).
narrative_ontology:cs_interpretation_layer_present('d8a48062-92fa-4d60-b05c-07d614221cdb').
narrative_ontology:cs_reading_relation('d8a48062-92fa-4d60-b05c-07d614221cdb', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('d8a48062-92fa-4d60-b05c-07d614221cdb', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('d8a48062-92fa-4d60-b05c-07d614221cdb', foundational, amun_ra_supremacy_within_divine_council).
narrative_ontology:cs_axiom_status(amun_ra_supremacy_within_divine_council, holdable).
narrative_ontology:cs_axiom_grounding('d8a48062-92fa-4d60-b05c-07d614221cdb', amun_ra_supremacy_within_divine_council, theological).
narrative_ontology:cs_axiom('d8a48062-92fa-4d60-b05c-07d614221cdb', foundational, priestly_mediation_required_for_legitimacy).
narrative_ontology:cs_axiom_status(priestly_mediation_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d8a48062-92fa-4d60-b05c-07d614221cdb', priestly_mediation_required_for_legitimacy, conventional).
narrative_ontology:cs_reference_frame('d8a48062-92fa-4d60-b05c-07d614221cdb', amun_theocratic_order).
narrative_ontology:cs_drift_state('d8a48062-92fa-4d60-b05c-07d614221cdb', amarna_period, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d8a48062-92fa-4d60-b05c-07d614221cdb', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_priesthoods).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, rural_cultivators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, multi_deity_cosmology).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, amun_ra_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls theological interpretation, coronation rituals, and oracle consultations. Validates pharaonic legitimacy through Amun-Ra. Manages temple lands and labor. Identity is fused with the cosmic order they administer; exit means abandoning sacred role and economic base.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, beneficiary).

% Accumulate agricultural surplus, craft production, and corvÃ©e labor through tax and dedications. Provide redistribution and ritual services. Deeply embedded in the political economy; cannot exit the legitimacy system without losing their land and labor grants.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies, beneficiary,
    institutional, generational, constrained, national).

% Maintain local cults under the Amun-Ra umbrella. Benefit from inclusion in the national temple network and shared legitimacy. Constrained because autonomy risks exclusion or suppression if they challenge Amun supremacy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_priesthoods, beneficiary,
    moderate, generational, constrained, regional).

% Receives divine legitimacy through priestly coronation and oracle, but is structurally constrained to defer to the Amun priesthood for validation. Must dedicate resources to temples and perform prescribed rituals. Cannot unilaterally redefine legitimacy without risking deposition or civil conflict.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary).

% Pay temple taxes and provide corvÃ©e labor. Receive ritual assurance of cosmic order and agricultural fertility. Geographically bound to temple estates; no practical exit from the religious-political tax system.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, rural_cultivators, payer,
    powerless, biographical, trapped, local).

% Practice household and village rituals outside formal temple doctrine. Marginalized in official legitimacy discourse but persist in daily life. Trapped because their practices are invisible to the high theological framework and offer no alternative path to political legitimacy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, folk_practitioners, excluded,
    powerless, biographical, trapped, local).

narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates regional cults and political authority under a unified multi-deity cosmology, providing shared agricultural calendars, ritual schedules, and a mechanism for legitimate succession that reduces dynastic violence.
% TRANSFER_FUNCTION: Moves agricultural surplus and labor from rural cultivators to temple economies; moves legitimizing authority from priestly interpretation to pharaoh in exchange for material support and doctrinal deference.
% ABSENT_VOICES: Folk practitioners and non-Amun local cults are structurally excluded from official theological discourse; they would argue for decentralized, pragmatic ritual but are invisible to the legitimacy mechanism.
% DISAPPEARANCE_RATIONALE: Without priestly interpretation of Amun-Ra supremacy, pharaonic legitimacy loses its primary channel, temple economies collapse, regional cults proliferate, and the political order fragments into competing local theologies.
% FOUNDING_PROBLEM: Political fragmentation and lack of centralized legitimacy following periods of foreign rule and competing regional nomes; need for a unifying cosmological framework that accommodates local variation while subordinating it.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historiography and archaeology corroborate the integration problem, but dispute whether priestly monopoly was the necessary solution; no source outside the theological framework corroborates the divine ontology itself.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the substantial diversion of agricultural surplus to temples and the structural subordination of pharaonic power. Suppression (0.65) captures the active marginalization of Atenist and folk alternatives while allowing regional syncretism under Amun supremacy. Theater ratio (0.48) acknowledges that rituals have genuine coordinating functions (calendar, redistribution) but also serve to performatively maintain priestly authority. Accessibility collapse (0.60) indicates that while folk alternatives persist locally, no alternative legitimacy mechanism exists at the national scale. Resistance (0.55) reflects recurring pharaonic challenges (most severely the Amarna period) and persistent folk practice.
 *
 * PERSPECTIVAL GAP:
 *   The priesthood experiences the constraint as cosmic order maintenance; the pharaoh experiences it as a necessary but costly validation mechanism that constrains autonomous action; the cultivator experiences it as taxation and labor obligation with diffuse ritual benefit. The engine computes these divergences from structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Amun priesthood and temple economies are full beneficiaries (low d). Regional priesthoods are partial beneficiaries (low-moderate d). Pharaoh occupies a mixed position: beneficiary of legitimacy but payer of autonomy and resources; declared in victims with constrained exit pushes d toward target. Rural cultivators are full targets (high d). Folk practitioners are excluded targets (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling: the constraint is not a mountain (it is actively maintained by priests, not a natural law), not a pure rope (temples extract asymmetric surplus and constrain the pharaoh), and not a pure snare (the coordination functionâpolitical integration, agricultural scheduling, legitimacy reduction of violenceâis structurally real and would need replacement if removed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priesthood_extraction_or_coordination,
    'Does the temple economy''s accumulation of land and labor represent the necessary cost of maintaining cosmic-order coordination, or is it extractive surplus captured by an institutional class?',
    'Comparative analysis of temple redistributive efficiency versus secular administration in periods of priestly weakness; archaeological evidence of standard-of-living differentials.',
    'If redistributive efficiency is high and surplus is modest, the constraint moves toward rope; if surplus is large and redistribution is minimal, it moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priesthood_extraction_or_coordination, empirical, 'Whether temple enrichment is coordination cost or extraction').

omega_variable(
    pharaoh_constraint_vs_collusion,
    'Is the pharaoh''s deference to Amun priesthood a genuine structural constraint on royal power, or a performative collusion that obscures shared extraction from cultivators?',
    'Analysis of pharaonic land grants to temples versus independent military/administrative capacity; instances of pharaohs overriding priests without deposition.',
    'If pharaoh can override priests at low cost, the constraint is more theatrical and collusive (piton/snare); if override reliably triggers crisis, the constraint is genuinely binding (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_constraint_vs_collusion, conceptual, 'Whether pharaoh constraint is structural or performative').

omega_variable(
    kernel_reading_boundary,
    'Is the Amun-polytheistic reading best understood as a sincere theological commitment, or as an institutional extraction mechanism whose theological language is a stabilizing kernel?',
    'Comparative institutional analysis across different kernel readings: do the same priests behave differently when the reading changes, holding material interests constant?',
    'If behavior is invariant to reading, the constraint is better classified as extraction using a kernel; if behavior tracks theological content, the classification as tangled rope stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Theological sincerity versus institutional extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(divi_tr_t5, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(divi_be_t5, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(divi_su_t5, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(divi_su_t10, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(divi_su_t20, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel decomposes into three structurally distinct readings: the Amun-polytheistic reading (formalized, lineage-grounded, extractive-coordinative), the Atenist-monotheistic reading (pharaonic revelation, exclusive), and the folk-syncretistic reading (distributed, practice-grounded). Each reading has a different epsilon, beneficiary structure, and institutional footprint. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
