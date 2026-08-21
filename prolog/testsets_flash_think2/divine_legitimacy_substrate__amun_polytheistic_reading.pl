% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Polytheistic Interpretation of Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the system of divine legitimacy in ancient
 *   Egypt, specifically through the lens of the Amun-Ra polytheistic
 *   cosmology and its interpretation by the powerful Amun priesthood during
 *   the New Kingdom. It establishes a cosmic order and legitimizes the
 *   pharaoh's rule, but also enables significant extraction by the priestly
 *   class. The constraint is claimed as a 'rope' by its beneficiaries (the
 *   priesthood and pharaoh) due to its coordination function, but the
 *   authored metrics reflect its substantially extractive and actively
 *   enforced nature, leading to a computed 'tangled_rope' classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.65).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.75).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Polytheistic Interpretation of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '7cdaafff-47e7-44dc-b4da-03a10fa467e3').
narrative_ontology:cs_kernel_codification('7cdaafff-47e7-44dc-b4da-03a10fa467e3', formalized).
narrative_ontology:cs_authority_grounding('7cdaafff-47e7-44dc-b4da-03a10fa467e3', lineage).
narrative_ontology:cs_interpretation_layer_present('7cdaafff-47e7-44dc-b4da-03a10fa467e3').
narrative_ontology:cs_reading_relation('7cdaafff-47e7-44dc-b4da-03a10fa467e3', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('7cdaafff-47e7-44dc-b4da-03a10fa467e3', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('7cdaafff-47e7-44dc-b4da-03a10fa467e3', foundational, amun_ra_chief_patron).
narrative_ontology:cs_axiom_status(amun_ra_chief_patron, holdable).
narrative_ontology:cs_axiom_grounding('7cdaafff-47e7-44dc-b4da-03a10fa467e3', amun_ra_chief_patron, theological).
narrative_ontology:cs_axiom('7cdaafff-47e7-44dc-b4da-03a10fa467e3', foundational, priestly_interpretive_authority).
narrative_ontology:cs_axiom_status(priestly_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7cdaafff-47e7-44dc-b4da-03a10fa467e3', priestly_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('7cdaafff-47e7-44dc-b4da-03a10fa467e3', maatian_cosmic_order).
narrative_ontology:cs_drift_state('7cdaafff-47e7-44dc-b4da-03a10fa467e3', atenist_heresy_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7cdaafff-47e7-44dc-b4da-03a10fa467e3', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cults).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The established religious authority that interprets divine will, performs essential rituals, and manages vast temple estates. They validate the pharaoh's rule and benefit significantly from tithes, offerings, and political influence.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Derives divine legitimacy and social order from the Amun-Ra cosmology as interpreted by the priesthood. However, they are also constrained by priestly counsel and must allocate substantial state resources to maintain the temples and priesthood.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer).

% Participates in state-sanctioned rituals, pays tithes and offerings, and provides labor for temple construction and maintenance. They receive a coherent cosmic order, social cohesion, and spiritual guidance, but bear significant material costs.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace, payer,
    powerless, biographical, trapped, local).

% Local religious practices and deities are tolerated but are structurally subordinated to the dominant Amun-Ra cosmology. They indirectly 'pay' by having their autonomy and influence limited, and their resources often flow upwards to the central temples.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cults, excluded,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cults, payer).

% Adherents of the Atenist monotheistic belief system, who fundamentally reject the Amun-Ra cosmology and priestly authority. They are actively suppressed or marginalized by the established system and have no legitimate voice within it.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_reformers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a coherent cosmic order and social hierarchy, providing stability and meaning for the populace, and legitimizing the pharaoh's rule through divine sanction.
% TRANSFER_FUNCTION: Moves wealth (tithes, offerings, labor) from the common populace and state resources from the pharaoh to the Amun priesthood and temple economies, in exchange for spiritual services, social order, and political legitimacy.
% ABSENT_VOICES: Adherents of alternative or suppressed cosmologies, such as the Atenist reformers, would object to the exclusivity and extractive nature of the Amun priesthood's authority, advocating for different forms of divine connection and social organization.
% DISAPPEARANCE_RATIONALE: If the established priestly interpretation and its enforcement vanished, the pharaoh's divine legitimacy would collapse, leading to political instability. The vast temple economies, which were central to the state's administration and resource distribution, would cease to function, causing widespread social and economic upheaval.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned social and political order in a complex polytheistic society, providing cosmic meaning and legitimizing centralized rule after periods of fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, archaeological evidence of temple construction and administration, and contemporary accounts from non-priestly scribes or foreign observers attest to the system's function in maintaining social order and state power, particularly after the Second Intermediate Period.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is moderate-high, reflecting the significant wealth and resources channeled to the Amun priesthood and temples. Suppression (0.75) is high, as the system actively marginalized or suppressed alternative religious practices and challenges (e.g., Atenism). The peak in extractiveness and suppression around 1350 BCE reflects the period immediately preceding and following the Amarna heresy, where the Amun priesthood's power was at its zenith and then reasserted. Theater ratio (0.40) is moderate; while rituals and interpretations were genuinely believed, some performative aspects served to reinforce priestly authority and maintain the status quo. The claimed type is 'tangled_rope' because it genuinely coordinates social and political order but does so with significant asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Amun priesthood, this system is a necessary 'rope' that maintains cosmic balance (Ma'at) and social order. From the perspective of the common populace or suppressed alternative cults, it functions as a 'snare' or 'tangled_rope,' extracting resources and suppressing dissent under the guise of divine will. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood acts as the primary agenda-setter and beneficiary, directly collecting wealth and wielding immense political influence. The pharaoh is also a beneficiary, gaining divine legitimacy for their rule, but is simultaneously a payer, constrained by priestly authority and obligated to fund the vast temple system. The common populace are clear payers, contributing labor and resources. Regional cults and Atenist reformers are excluded and bear costs through marginalization and suppression.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_pharaonic_constraint,
    'To what extent did the Amun priesthood''s interpretive authority genuinely constrain the pharaoh''s political and religious decisions, beyond mere ritual adherence?',
    'Detailed historical analysis of specific pharaonic decrees, temple inscriptions, and administrative records, particularly during periods of tension between the crown and the priesthood.',
    'If the constraint was primarily ritualistic, the pharaoh''s ''payer'' role is minimal; if it involved significant policy influence, the ''payer'' role and associated extraction are higher, potentially shifting the pharaoh''s effective directionality closer to a target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_pharaonic_constraint, empirical, 'The actual power balance between pharaoh and priesthood.').

omega_variable(
    separability_of_cosmic_order_from_extraction,
    'Is the social cohesion and cosmic order provided by the Amun-Ra cosmology structurally inseparable from the high extraction and centralized authority of the Amun priesthood?',
    'Comparative historical analysis of other ancient societies with similar coordination problems but different religious-economic structures, or counterfactual analysis of the Amarna period''s long-term viability had it persisted.',
    'If separable, the extraction is pure rent-seeking riding on a genuine coordination function; if inseparable, a portion of the measured extraction is a necessary cost of the coordination itself, potentially shifting the classification closer to a ''rope'' for some seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(separability_of_cosmic_order_from_extraction, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of regional cults and alternative beliefs primarily structural (state-backed enforcement, economic incentives) or internalized (deeply held popular belief in Amun-Ra''s supremacy)?',
    'Archaeological evidence of clandestine cult practices, analysis of popular religious texts, and records of state intervention against non-sanctioned worship.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher and more resilient than structural measures suggest; if primarily structural, removing state enforcement would lead to rapid diversification of religious practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for religious belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 1550, 1070).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1550, 0.3).
narrative_ontology:measurement(divi_tr_t1450, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1450, 0.35).
narrative_ontology:measurement(divi_tr_t1350, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1350, 0.45).
narrative_ontology:measurement(divi_tr_t1250, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1250, 0.42).
narrative_ontology:measurement(divi_tr_t1150, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1150, 0.41).
narrative_ontology:measurement(divi_tr_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1070, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1550, 0.55).
narrative_ontology:measurement(divi_be_t1450, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1450, 0.6).
narrative_ontology:measurement(divi_be_t1350, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1350, 0.68).
narrative_ontology:measurement(divi_be_t1250, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1250, 0.7).
narrative_ontology:measurement(divi_be_t1150, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1150, 0.67).
narrative_ontology:measurement(divi_be_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1070, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1550, 0.65).
narrative_ontology:measurement(divi_su_t1450, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1450, 0.7).
narrative_ontology:measurement(divi_su_t1350, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1350, 0.85).
narrative_ontology:measurement(divi_su_t1250, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1250, 0.8).
narrative_ontology:measurement(divi_su_t1150, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1150, 0.78).
narrative_ontology:measurement(divi_su_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1070, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
