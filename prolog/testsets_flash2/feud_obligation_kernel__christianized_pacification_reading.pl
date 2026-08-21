% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification Reading of Blood-Feud Obligations
 *   domain: legal_anthropology/medieval_history/political_systems
 *
 * SUMMARY:
 *   This constraint represents the 'Christianized Pacification' reading of
 *   blood-feud obligations, prevalent in medieval Europe. It frames
 *   blood-feuds as a violation of divine law and asserts that legitimate
 *   authority for violence and justice resides solely with God, delegated to
 *   ecclesiastical and royal institutions. This reading seeks to suppress
 *   traditional kin-based vengeance through spiritual and temporal penalties,
 *   expanding the jurisdictional reach and moral authority of the Church and
 *   Crown. The structural delta for this reading is that all feud
 *   participants are victims (facing spiritual peril and institutional
 *   punishment), while the Church and Crown are beneficiaries (gaining
 *   interpretive monopoly and expanded power).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.85).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.9).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification Reading of Blood-Feud Obligations").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '934741d0-0291-4f01-9f08-ee6e18fb3c61').
narrative_ontology:cs_kernel_codification('934741d0-0291-4f01-9f08-ee6e18fb3c61', formalized).
narrative_ontology:cs_authority_grounding('934741d0-0291-4f01-9f08-ee6e18fb3c61', lineage).
narrative_ontology:cs_interpretation_layer_present('934741d0-0291-4f01-9f08-ee6e18fb3c61').
narrative_ontology:cs_reading_relation('934741d0-0291-4f01-9f08-ee6e18fb3c61', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('934741d0-0291-4f01-9f08-ee6e18fb3c61', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('934741d0-0291-4f01-9f08-ee6e18fb3c61', foundational, divine_monopoly_on_vengeance).
narrative_ontology:cs_axiom_status(divine_monopoly_on_vengeance, holdable).
narrative_ontology:cs_axiom_grounding('934741d0-0291-4f01-9f08-ee6e18fb3c61', divine_monopoly_on_vengeance, theological).
narrative_ontology:cs_axiom('934741d0-0291-4f01-9f08-ee6e18fb3c61', secondary, ecclesiastical_royal_delegation).
narrative_ontology:cs_axiom_status(ecclesiastical_royal_delegation, holdable).
narrative_ontology:cs_axiom_grounding('934741d0-0291-4f01-9f08-ee6e18fb3c61', ecclesiastical_royal_delegation, conventional).
narrative_ontology:cs_reference_frame('934741d0-0291-4f01-9f08-ee6e18fb3c61', divinely_ordained_peace).
narrative_ontology:cs_drift_state('934741d0-0291-4f01-9f08-ee6e18fb3c61', late_medieval_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('934741d0-0291-4f01-9f08-ee6e18fb3c61', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feuding_families).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts divine authority to prohibit vengeance and establish peace. Benefits from expanded spiritual and temporal jurisdiction, increased tithes, and moral authority over populations. Actively enforces prohibitions through excommunication and penitential discipline.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).

% Benefits from the Church's pacification efforts, which reduce internal conflict and strengthen royal control over territories. Gains legitimacy by aligning with divine law and can delegate enforcement to secular courts, expanding its own legal reach.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, beneficiary,
    institutional, generational, mobile, national).

% Are the primary targets of pacification efforts. They face spiritual penalties (excommunication, denial of sacraments) and temporal punishments (fines, imprisonment) for continuing feuds. Their identity is often tied to honor and vengeance, making exit from the feud cycle difficult despite the costs.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feuding_families, payer,
    moderate, biographical, identity_locked, local).

% Suffer the direct violence and instability of feuds, but also bear the burden of ecclesiastical and royal interventions (e.g., providing resources for peace-keeping, participating in courts). They benefit from reduced violence but pay through increased institutional control and loss of traditional dispute resolution.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_communities, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, local_communities, beneficiary).

% Historically mediated feuds and maintained customary law. Their authority is undermined by the assertion of divine and royal monopoly on justice. They would argue for the legitimacy of traditional mechanisms but are increasingly marginalized.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, traditional_elders, excluded,
    moderate, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral and legal framework that prohibits private vengeance, aiming to coordinate society towards peace and order under a single, divinely sanctioned authority for justice.
% TRANSFER_FUNCTION: Transfers the right to legitimate violence and dispute resolution from kin groups to ecclesiastical and royal institutions, along with the associated social control and resources (e.g., fines, tithes, judicial fees).
% ABSENT_VOICES: Traditional kin-based justice systems and their proponents are excluded; they would argue for the efficacy and legitimacy of customary law in maintaining social order, but their claims are dismissed as 'barbaric' or 'un-Christian'.
% DISAPPEARANCE_RATIONALE: If this Christianized pacification framework vanished, society would revert to more localized, kin-based forms of justice, including blood-feuds, as the centralized authority for violence would be delegitimized. The power balance between Church, Crown, and local communities would fundamentally shift.
% FOUNDING_PROBLEM: Widespread blood-feuds and private vengeance destabilized society, hindered economic development, and challenged the emerging authority of both Church and Crown.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical and royal chronicles consistently depict feuds as a persistent problem requiring continuous intervention. Modern historians and legal anthropologists corroborate the historical prevalence and disruptive nature of feuds, supporting the claim that the problem was, and to some extent remains, a live concern for centralized authorities.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because traditional kin groups lose their autonomy in dispute resolution and face severe penalties, while ecclesiastical and royal institutions gain significant power and resources. Suppression is very high due to the combined spiritual (excommunication, damnation) and temporal (fines, imprisonment, loss of land) coercion applied. Theater ratio is low because the pacification efforts were genuinely aimed at establishing a new order, though some performative aspects existed to reinforce divine authority. Resistance is high because feuding was deeply ingrained in social structures and identity, leading to continuous, often violent, challenges to the new authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of feuding families, the constraint is a snare, forcibly extracting their traditional rights and imposing alien forms of justice. From the Church's perspective, it is a necessary rope or even a mountain, aligning human law with divine will for the common good. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions are the primary agenda-setters and beneficiaries, actively shaping and enforcing the constraint to expand their moral and jurisdictional authority. Royal authority is a strong beneficiary, gaining stability and legitimacy through alignment with the Church's efforts. Feuding families are clear payers, facing direct penalties and identity-locked exit options due to deeply held cultural norms. Local communities are also payers, bearing the costs of institutional intervention, but also beneficiaries of reduced violence. Traditional elders are excluded, their customary authority undermined.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (pacification) remains live, but its justification has shifted from purely divine command to also include state-building and social order. The high extractiveness and suppression indicate it's not a simple coordination mechanism, but a forceful re-ordering of society that benefits centralized powers. The 'snare' classification prevents mislabeling this as a benign 'rope' of social order, acknowledging the coercive transfer of authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_secular_authority,
    'To what extent is the authority for prohibiting vengeance truly divine, versus a strategic assertion by ecclesiastical and royal powers to consolidate their own temporal control?',
    'Comparative analysis of legal systems in different religious contexts and historical periods, examining the correlation between religious claims and state-building processes.',
    'If primarily a strategic assertion, the constraint''s ''divine law'' aspect becomes a cover story for institutional extraction, potentially reclassifying it as a more overt snare. If genuinely divine, it reinforces the mountain-like aspect of the underlying moral claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_vs_secular_authority, conceptual, 'Ambiguity between theological grounding and institutional power grab.').

omega_variable(
    internalized_spiritual_suppression,
    'How much of the suppression of feuds is due to internalized spiritual fear (e.g., fear of damnation, excommunication) versus external temporal enforcement (fines, imprisonment)?',
    'Analysis of penitential literature, confessional records, and legal archives to gauge the relative impact of spiritual vs. temporal penalties on individual behavior and community norms.',
    'If spiritual suppression is dominant, the effective suppression is higher and more pervasive than purely structural measures suggest, as individuals carry the enforcement within themselves. This would amplify the snare-like qualities, as exit from spiritual identity is ''identity_locked''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_spiritual_suppression, empirical, 'Structural vs. internalized suppression mechanism in religious contexts.').

omega_variable(
    pacification_efficacy_vs_cost,
    'Did the Christianized pacification efforts genuinely reduce overall violence and improve social stability, or did they primarily shift the locus of violence and impose new forms of extraction?',
    'Quantitative historical analysis of violence rates (e.g., homicides, feuds) before and after pacification efforts, alongside economic and social indicators of well-being for different social strata.',
    'If pacification was largely ineffective or merely shifted costs, the ''coordination function'' claim is weakened, reinforcing the ''snare'' classification. If highly effective, it might suggest a stronger ''tangled_rope'' element, where genuine coordination benefits coexist with extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pacification_efficacy_vs_cost, empirical, 'Effectiveness of pacification vs. its extractive costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 80, 0.89).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
