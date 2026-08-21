% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Treaty of Waitangi: Crown Sovereignty Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint represents the 'Crown Sovereignty' reading of the Treaty
 *   of Waitangi, where English Article I is interpreted as a complete cession
 *   of Māori sovereignty to the British Crown, establishing Westminster
 *   parliamentary supremacy in New Zealand. This reading underpins the
 *   historical and ongoing exercise of plenary legislative power by the
 *   Crown, often without requiring Māori consent, and has led to unilateral
 *   resource allocation and the subordination of Māori interests to
 *   parliamentary will. This is one reading of the
 *   'waitangi_sovereignty_allocation' kernel, with sibling readings
 *   'partnership_reading' and 'rangatiratanga_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.85).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.75).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, snare).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Treaty of Waitangi: Crown Sovereignty Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '1ef5ebc1-0e8c-426f-8f93-bf71dffa267a').
narrative_ontology:cs_kernel_codification('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', fixed_text).
narrative_ontology:cs_authority_grounding('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', lineage).
narrative_ontology:cs_interpretation_layer_present('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a').
narrative_ontology:cs_reading_relation('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_reading_relation('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', foundational, parliamentary_supremacy_is_plenary).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_is_plenary, holdable).
narrative_ontology:cs_axiom_grounding('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', parliamentary_supremacy_is_plenary, conventional).
narrative_ontology:cs_axiom('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', foundational, english_text_is_authoritative).
narrative_ontology:cs_axiom_status(english_text_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', english_text_is_authoritative, conventional).
narrative_ontology:cs_reference_frame('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', unilateral_crown_authority).
narrative_ontology:cs_drift_state('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', contemporary_post_treaty_settlements_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1ef5ebc1-0e8c-426f-8f93-bf71dffa267a', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, pakeha_settlers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Treaty as ceding full sovereignty, enabling unilateral legislative power and resource allocation. Benefits from unchallenged authority and control over national resources. Exit options are to maintain the status quo or concede to alternative interpretations, which would entail significant loss of power and resources.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the Crown's plenary power, which facilitated land acquisition, resource exploitation, and the establishment of a Westminster-style government. Their interests are generally aligned with the Crown sovereignty reading, as it underpins their historical and contemporary claims to land and political dominance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, pakeha_settlers, beneficiary,
    organized, generational, mobile, national).

% Bear the costs of lost sovereignty, land, resources, and cultural autonomy. Their traditional authority (tino rangatiratanga) is subordinated to parliamentary supremacy. Exit is identity-locked, as their identity and well-being are inextricably linked to their ancestral lands and cultural practices, which are governed by the Crown's interpretation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    powerless, civilizational, identity_locked, local).

% Experience the effects of Crown sovereignty through legislation and policy that often marginalizes Māori interests. They are subject to the same laws as other citizens but often face systemic disadvantages stemming from the historical interpretation of the Treaty. Their options are to advocate for change within the existing system or engage in protest.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens, payer,
    moderate, biographical, constrained, national).

% Monitor New Zealand's compliance with international indigenous rights standards. They provide critical analysis and recommendations but lack direct enforcement power over the Crown's domestic interpretation of the Treaty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unified legal and political system under the authority of the British Crown, facilitating the orderly settlement and governance of New Zealand.
% TRANSFER_FUNCTION: Transfers ultimate legislative and executive authority over all of New Zealand from Māori chiefs (as interpreted by the Crown) to the British Crown, enabling the Crown to allocate land and resources unilaterally.
% ABSENT_VOICES: The full scope of Māori rangatiratanga (self-determination) as understood by Māori signatories is absent from this reading; their understanding of retained authority would fundamentally challenge the Crown's claim to plenary sovereignty.
% DISAPPEARANCE_RATIONALE: If the Crown sovereignty reading vanished, the entire constitutional and legal framework of New Zealand would be destabilized. Māori claims to self-determination and resource ownership would gain immediate legal and political force, necessitating a fundamental renegotiation of power and governance structures.
% FOUNDING_PROBLEM: The British Crown sought to establish legitimate authority over New Zealand to protect British settlers, regulate trade, and prevent other colonial powers from claiming the territory, while also managing relations with Māori.
% FOUNDING_PROBLEM_CORROBORATION: The Crown maintains that its sovereignty is essential for national unity and effective governance. Māori leaders and scholars, supported by historical analysis and international legal principles, corroborate that the problem of reconciling Crown authority with Māori self-determination remains live and unresolved, but dispute the Crown's unilateral interpretation.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading enables the Crown to appropriate vast resources and exercise unchallenged authority, largely at the expense of Māori self-determination and resource control. Suppression (0.75) is significant, as the Crown has historically used legal and coercive means to enforce this interpretation and suppress Māori resistance. Theater ratio (0.20) is relatively low, as the Crown's actions are largely consistent with its declared interpretation, though some performative gestures towards 'partnership' exist. The historical measurements show an increase in extractiveness and suppression as the Crown consolidated power, with a slight decline in recent decades due to increased Māori activism and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective, this reading provides a clear, stable basis for governance. From Māori perspectives, it is a fundamentally unjust and extractive interpretation that denies their inherent rights and the true intent of the Treaty. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown and Pakeha settlers are the primary beneficiaries, gaining political control, land, and resources. Māori iwi and hapu, along with Māori citizens, are the primary victims, experiencing loss of sovereignty, land, and cultural rights. International human rights bodies act as observers, providing external critique without direct enforcement power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_ambiguity,
    'Is the English Article I''s ''cession of sovereignty'' truly plenary, or is it ambiguous enough to accommodate Māori understandings of retained authority?',
    'Comparative textual analysis of the English and Māori versions of the Treaty, alongside historical records of negotiations and contemporary international legal principles regarding indigenous treaties.',
    'If the text is found to be genuinely ambiguous or to support a more limited cession, the Crown sovereignty reading''s legitimacy would be undermined, potentially shifting the constraint towards a Tangled Rope or even Snare from the Crown''s perspective, as its coordination function would be revealed as cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_text_ambiguity, conceptual, 'Ambiguity in the Treaty text regarding the extent of sovereignty cession.').

omega_variable(
    international_law_impact,
    'To what extent does evolving international law on indigenous rights (e.g., UNDRIP) challenge the domestic legal validity and moral legitimacy of the Crown sovereignty reading?',
    'Judicial review incorporating international legal norms, or legislative action to align domestic law with international standards.',
    'If international law is deemed to have significant domestic legal or moral force, the Crown sovereignty reading could be reclassified as a Snare due to its suppression of indigenous rights, or face increasing external pressure for resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_impact, empirical, 'Influence of international indigenous rights law on the Crown''s interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(wait_tr_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(wait_tr_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.7).
narrative_ontology:measurement(wait_be_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1880, 0.8).
narrative_ontology:measurement(wait_be_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1920, 0.88).
narrative_ontology:measurement(wait_be_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1960, 0.92).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(wait_su_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1880, 0.75).
narrative_ontology:measurement(wait_su_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(wait_su_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_land_claims_tribunal).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_fisheries_settlement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'waitangi_sovereignty_allocation' kernel. This 'Crown Sovereignty' reading directly influences the operational space and contestation of the 'partnership_reading' and 'rangatiratanga_reading' by asserting a foundational claim of plenary Crown authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
