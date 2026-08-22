% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Indigenous Stewardship Reading of the Cultural Property Legal Corpus
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint story authors the indigenous_stewardship_reading of the
 *   cultural_property_legal_corpus kernel. The standing arrangement under
 *   contest is the international legal and institutional framework that vests
 *   custody, title, and interpretive authority over indigenous cultural and
 *   sacred artifacts in colonial-era museums and successor states. From this
 *   reading's perspective, the arrangement extracts cultural, spiritual, and
 *   economic value from source communities to benefit holding institutions
 *   and states, while presenting a coordination story of preservation and
 *   universal access. The authored metrics reflect this reading's assessment
 *   of the current arrangement; the claimed type is tangled_rope because the
 *   constraint simultaneously coordinates preservation and access and
 *   asymmetrically extracts from communities.
 *
 * KEY AGENTS:
 *   - source_communities: Primary target (organized/trapped) â bear extraction through dispossession of sacred objects and blocked repatriation.
 *   - colonial_museums: Primary beneficiary and agenda-setter (institutional/constrained) â collect prestige, revenue, and research control.
 *   - successor_states: Secondary beneficiary and agenda-setter (institutional/arbitrage) â benefit from national identity narratives and territorial sovereignty over artifacts.
 *   - international_law_scholars: Analytical observer â sees the structural gap between UN declarations and domestic practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.82).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Indigenous Stewardship Reading of the Cultural Property Legal Corpus").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, 'bfd5440a-cea0-447b-9f9a-b209dd0d2602').
narrative_ontology:cs_kernel_codification('bfd5440a-cea0-447b-9f9a-b209dd0d2602', formalized).
narrative_ontology:cs_authority_grounding('bfd5440a-cea0-447b-9f9a-b209dd0d2602', lineage).
narrative_ontology:cs_interpretation_layer_present('bfd5440a-cea0-447b-9f9a-b209dd0d2602').
narrative_ontology:cs_reading_relation('bfd5440a-cea0-447b-9f9a-b209dd0d2602', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfd5440a-cea0-447b-9f9a-b209dd0d2602', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_axiom('bfd5440a-cea0-447b-9f9a-b209dd0d2602', foundational, cultural_continuity_as_legitimacy_source).
narrative_ontology:cs_axiom_status(cultural_continuity_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('bfd5440a-cea0-447b-9f9a-b209dd0d2602', cultural_continuity_as_legitimacy_source, deontological).
narrative_ontology:cs_axiom('bfd5440a-cea0-447b-9f9a-b209dd0d2602', foundational, communal_sacred_property_status).
narrative_ontology:cs_axiom_status(communal_sacred_property_status, holdable).
narrative_ontology:cs_axiom_grounding('bfd5440a-cea0-447b-9f9a-b209dd0d2602', communal_sacred_property_status, deontological).
narrative_ontology:cs_reference_frame('bfd5440a-cea0-447b-9f9a-b209dd0d2602', indigenous_community_authority).
narrative_ontology:cs_drift_state('bfd5440a-cea0-447b-9f9a-b209dd0d2602', contemporary_international_legal_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bfd5440a-cea0-447b-9f9a-b209dd0d2602', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, source_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sacred and ceremonial objects necessary for cultural and spiritual practice are held in foreign museums or claimed by successor states. Legal claims for repatriation are blocked by statutes of limitations, lack of legal standing, or state sovereignty doctrines. Community members are denied access for ceremony and are required to request permission from institutions that do not recognize communal sacred authority.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, source_communities, payer,
    organized, generational, trapped, global).

% Maintain physical custody of artifacts acquired during colonial periods. Control access, display, and research permissions. Derive institutional prestige, research funding, and visitor revenue. Resist deaccessioning through internal policies and legal defenses based on colonial acquisition titles and donor intent.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_museums, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_museums, beneficiary).

% Assert territorial sovereignty over cultural artifacts within their borders. Enact national patrimony laws that classify indigenous cultural material as state property. Benefit from national identity narratives and tourism revenue. Block or delay repatriation to indigenous communities through state immunity and domestic jurisdiction arguments.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, beneficiary).

% Analyze the conflict between state sovereignty, museum ethics, and indigenous rights under international law. Document the systematic gap between UN declarations and domestic legal practice. Provide independent structural assessments of title chains and repatriation barriers.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under its own framing, the international legal corpus coordinates the preservation, display, and inter-state transfer of cultural artifacts to prevent destruction and looting, and to organize shared human heritage access through accredited institutions.
% TRANSFER_FUNCTION: Moves physical custody and interpretive authority over sacred and cultural objects from source communities to colonial museums and successor states, transferring prestige, research access, tourism revenue, and sovereign identity-claims to the holders.
% ABSENT_VOICES: Source community spiritual leaders and cultural practitioners are structurally excluded from legal proceedings and museum governance; their testimony on the sacred status and ceremonial necessity of artifacts is treated as non-legal or anthropological data rather than authoritative ownership claims.
% DISAPPEARANCE_RATIONALE: If the legal doctrines and enforcement that sustain museum and state custody disappeared, source communities would reclaim sacred objects, museum collections would reorganize around consent-based loan and stewardship agreements, and successor state cultural property laws would be rewritten to recognize communal authority.
% FOUNDING_PROBLEM: Colonial-era acquisition and subsequent state succession created an interstate legal framework to manage disputes over cultural objects and prevent looting, but it was built on the assumption that communities lacked juridical personality to hold title or custody.
% FOUNDING_PROBLEM_CORROBORATION: International heritage bodies and museum associations attest the preservation and anti-looting problem is still live. Indigenous rights organizations and UN special rapporteurs attest the problem is misframed and the arrangement persists as colonial extraction. Independent legal historians corroborate that the framework was designed for state-to-state and museum-property coordination, not community authority.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.88 at interval end) because communities are deprived of objects essential to cultural and spiritual continuity. Suppression is high (0.82) because legal standing is systematically denied through state immunity, statutes of limitations, and national patrimony laws. Theater ratio (0.45) reflects performative decolonization discourse and advisory committees that do not transfer authority. Accessibility collapse (0.75) is high because community custody alternatives are structurally blocked by state sovereignty and museum title doctrines. Resistance (0.68) is substantial and growing through indigenous rights movements and UN mechanisms. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (museums, successor states) experience the constraint as legitimate property management and national patrimony; the payer seat (source communities) experiences it as ongoing colonial extraction. The engine computes this divergence from the structural data: museums have institutional power and constrained exit (they choose to retain), while communities have organized identity but trapped exit (no legal standing). The claimed type does not adjudicate this divergence; the metrics do.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (colonial_museums, successor_states) derive directionality near the full-beneficiary end: they collect prestige, revenue, research access, and sovereign identity from the constraint. Victims (source_communities) derive directionality near the full-target end: they bear the costs of dispossession, denied ceremonial access, and cultural discontinuity. The asymmetry is reinforced by spatial scope (national/global for holders vs local community scale for sources) and power (institutional vs organized).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents misreading the genuine preservation and research coordination as pure extraction, while capturing the asymmetric beneficiary structure. If the coordination function atrophied and only possession remained, the constraint would degrade toward snare or piton. The founding problem status is contested because beneficiary parties claim the preservation problem is still live, while external observers corroborate that the arrangement persists as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'This constraint is the indigenous_stewardship_reading of kernel cultural_property_legal_corpus. Sibling readings assign authority to successor states (sovereign_repatriation_reading) or to universal-access institutions (universal_heritage_reading). Where exactly do the readings diverge structurally?',
    'Compare the beneficiary/victim structures and epsilon values of the sibling constraints once authored as separate stories.',
    'The divergence is located in the authority-assignment axiom: this reading makes indigenous communities the rightful authority and current holders the extractors, while sibling readings legitimate holder authority and shift victimhood elsewhere.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Committing omega for kernel reading position and sibling structural deltas').

omega_variable(
    coordination_extraction_separability,
    'Can the preservation and research coordination provided by museums be separated from the extractive custody arrangement, or are they structurally fused under the current legal corpus?',
    'Natural experiments of community-controlled stewardship with institutional research partnerships; comparative analysis of repatriation outcomes where custody transferred but access preserved.',
    'If separable, the constraint is a tangled_rope with a genuine coordination component; if inseparable, the extraction is inherent to the institutional form and the constraint leans toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether preservation coordination and extractive custody are structurally separable').

omega_variable(
    colonial_continuity_legitimacy,
    'Does the successor state''s legal continuity with colonial acquisition constitute a legitimate title under international law, or is it an unbroken chain of extraction?',
    'Comparative legal analysis of title laundering, UN repatriation principles, and international tribunal rulings on cultural property.',
    'If colonial continuity is illegitimate, the constraint''s beneficiary structure collapses into pure extraction; if legitimate, the extraction is partially masked as inherited state right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_continuity_legitimacy, conceptual, 'Whether colonial legal continuity is a legitimate title or extraction chain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(cult_su_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the cultural_property_legal_corpus kernel. The epsilon-invariance principle requires separate stories for each reading because they assign legitimate authority to different agents and produce different epsilon values. Each reading has its own constraint_id, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
