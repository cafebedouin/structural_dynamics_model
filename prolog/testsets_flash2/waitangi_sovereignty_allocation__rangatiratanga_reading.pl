% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi: Māori Tino Rangatiratanga (Full Authority) Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint represents the 'tino rangatiratanga' reading of Article
 *   II of the Māori text of the Treaty of Waitangi, which asserts that Māori
 *   retained full authority over their lands, resources, and taonga
 *   (treasures), with the Crown gaining only kāwanatanga (governorship) over
 *   its own settlers. This reading emphasizes Māori self-determination and
 *   inherent sovereignty, limiting the Crown's jurisdiction. The metrics
 *   reflect the historical suppression of this reading and its partial
 *   resurgence in contemporary times, leading to a 'rope' classification from
 *   this perspective, as it genuinely coordinates co-existence while
 *   requiring active defense against competing interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.25).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.4).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi: Māori Tino Rangatiratanga (Full Authority) Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, 'a9bd0d0d-4fca-4d56-b29f-50fa389f85a5').
narrative_ontology:cs_kernel_codification('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', fixed_text).
narrative_ontology:cs_authority_grounding('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', lineage).
narrative_ontology:cs_interpretation_layer_present('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5').
narrative_ontology:cs_reading_relation('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', foundational, maori_tino_rangatiratanga_inherent).
narrative_ontology:cs_axiom_status(maori_tino_rangatiratanga_inherent, holdable).
narrative_ontology:cs_axiom_grounding('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', maori_tino_rangatiratanga_inherent, deontological).
narrative_ontology:cs_axiom('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', foundational, crown_kawanatanga_limited_to_settlers).
narrative_ontology:cs_axiom_status(crown_kawanatanga_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', crown_kawanatanga_limited_to_settlers, conventional).
narrative_ontology:cs_reference_frame('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', maori_text_original_intent).
narrative_ontology:cs_drift_state('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', contemporary_era_post_waitangi_tribunal, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a9bd0d0d-4fca-4d56-b29f-50fa389f85a5', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_cultural_institutions).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government_agencies).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the original signatories and inheritors of tino rangatiratanga, they retain full authority over their lands, resources, and cultural treasures. Their identity is deeply tied to this authority, making exit from the claim unthinkable, but their power is organized and growing.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, beneficiary,
    organized, generational, identity_locked, national).

% Under this reading, Crown agencies' jurisdiction is limited to kāwanatanga (governorship) over non-Māori populations and must defer to Māori authority over lands and taonga. This requires significant restructuring and relinquishing of assumed powers, incurring political and administrative costs.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government_agencies, payer,
    institutional, generational, constrained, national).

% Their land titles and resource access, often acquired under Crown sovereignty claims, become subject to Māori tino rangatiratanga. This creates uncertainty and potential for renegotiation or restitution, imposing costs and challenging established property rights.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_landowners, payer,
    moderate, biographical, constrained, local).

% Benefit from the recognition and active protection of taonga (treasures), including language, knowledge, and cultural practices. This reading empowers them to exercise self-determination in cultural matters, securing resources and autonomy.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_cultural_institutions, beneficiary,
    organized, generational, identity_locked, national).

% Interprets the Treaty and its implications for modern law. While not a direct beneficiary or victim, its rulings can significantly shift the balance of power and resource allocation, acting as an agenda-setter for the constraint's practical application.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Monitor New Zealand's compliance with indigenous rights under international law. Their observations and recommendations provide external pressure and legitimacy to the rangatiratanga reading, influencing domestic policy and judicial interpretation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for co-existence and governance in Aotearoa New Zealand by defining the respective spheres of authority for Māori (tino rangatiratanga) and the Crown (kāwanatanga over settlers), aiming to prevent conflict over land and resources.
% TRANSFER_FUNCTION: Transfers ultimate authority over Māori lands, resources, and taonga to Māori iwi and hapū, while limiting Crown governmental power to non-Māori populations and administrative functions. This implies a transfer of control and decision-making power.
% ABSENT_VOICES: Early colonial settlers who believed in absolute Crown sovereignty and the doctrine of terra nullius are absent from this reading's framing, as their claims are directly contradicted by the retention of tino rangatiratanga. Their descendants, who benefit from historical land acquisitions, often resist this reading.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished, Māori claims to self-determination and resource control would lose their foundational legal and moral grounding, leading to renewed conflict over land, resources, and governance. The entire constitutional and social fabric of New Zealand would be fundamentally altered, likely reverting to a more Crown-centric, extractive model.
% FOUNDING_PROBLEM: To establish a basis for British settlement in Aotearoa New Zealand while securing Māori authority and preventing uncontrolled land alienation, ensuring peace and mutual benefit between Māori and the Crown.
% FOUNDING_PROBLEM_CORROBORATION: Māori iwi and hapū consistently attest that the founding problem of securing their authority and resources remains live, given ongoing challenges to their rangatiratanga. International indigenous rights experts and some legal scholars outside the Crown's direct influence corroborate the continued relevance of this problem and the need for the rangatiratanga reading to address it.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is relatively low from this reading's perspective because it asserts Māori retention of authority, meaning the constraint itself is not designed to extract from Māori. However, the historical suppression (0.40) has been significant, as the Crown's actions often contradicted this reading. Resistance (0.70) is high, reflecting continuous Māori advocacy. Accessibility collapse (0.60) is moderate, as the Crown's competing claims have always presented an alternative, albeit one resisted by Māori. The claimed type is 'rope' because, from this perspective, the Treaty's Māori text genuinely coordinates a framework for co-existence and mutual recognition of authority, even if that coordination has been historically undermined by other readings. The temporal measurements show a period of increasing extractiveness and suppression as the Crown asserted its sovereignty, followed by a decrease as Māori resistance led to greater recognition of rangatiratanga.
 *
 * PERSPECTIVAL GAP:
 *   The 'tino rangatiratanga' reading fundamentally diverges from the 'Crown sovereignty' reading, which would classify the Crown as the primary beneficiary and Māori as victims. The 'partnership' reading would likely show a more balanced, but still contested, distribution of benefits and costs. The engine's per-seat classification will highlight these divergences, showing how the same historical document is experienced as a different constraint depending on the interpretive frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi and hapū are the primary beneficiaries, as the reading secures their authority and resources (low directionality). Crown government agencies and settler landowners are payers, as they must cede or share authority and resources (high directionality). The New Zealand judiciary acts as an agenda-setter, interpreting and applying the Treaty, which can either uphold or undermine this reading. International human rights bodies serve as observers, providing external validation and pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (securing Māori authority) remains live and actively contested. The challenge is not that the mandate has atrophied, but that it has been actively suppressed and reinterpreted by competing claims. The classification as 'rope' from this reading's perspective prevents mislabeling it as a 'snare' (which it would be from a Crown sovereignty reading's perspective for Māori) or a 'piton' (as it is actively defended and its function is vital).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crown_sovereignty_vs_rangatiratanga,
    'Is the Treaty of Waitangi a cession of full sovereignty to the Crown (Crown sovereignty reading) or a retention of full Māori authority (tino rangatiratanga reading)?',
    'Ongoing legal and political contestation, potentially through international arbitration or a constitutional re-founding process that explicitly clarifies the relationship.',
    'If the Crown sovereignty reading prevails, this constraint would be reclassified as a ''snare'' for Māori, with high extractiveness and suppression. If the rangatiratanga reading is fully implemented, it would function as a ''rope'' or even a ''mountain'' for Māori, with minimal extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crown_sovereignty_vs_rangatiratanga, conceptual, 'Fundamental interpretive conflict over the Treaty''s core meaning regarding sovereignty.').

omega_variable(
    kawanatanga_scope_ambiguity,
    'What is the precise scope and limit of kāwanatanga (governorship) granted to the Crown under the rangatiratanga reading?',
    'Detailed co-governance agreements, legislative clarification, and judicial precedent that delineate specific areas of Crown authority versus Māori authority.',
    'A narrow interpretation of kāwanatanga would further empower Māori and reduce Crown overreach, reinforcing the ''rope'' classification. A broad interpretation, even within this reading, could introduce more extractiveness and suppression, pushing it towards a ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_scope_ambiguity, empirical, 'Ambiguity regarding the practical limits of Crown authority under Māori sovereignty.').

omega_variable(
    natural_law_vs_constructed_rangatiratanga,
    'Is tino rangatiratanga an inherent, pre-existing natural right of Māori, or a right recognized and affirmed by the Treaty as a constructed legal instrument?',
    'Philosophical and legal debate within Māori jurisprudence and international indigenous rights discourse. This is a conceptual distinction that may not have a single empirical resolution.',
    'If inherent, the ''rope'' classification is more robust, as the Treaty merely acknowledges a pre-existing ''mountain'' of Māori authority. If purely constructed by the Treaty, its persistence is more vulnerable to legal reinterpretation and political will, making it a more fragile ''rope'' or even a ''tangled_rope'' if enforcement becomes asymmetric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rangatiratanga, conceptual, 'Whether Māori tino rangatiratanga is an inherent right or a Treaty-derived right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(wait_tr_t1880, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(wait_tr_t1920, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(wait_tr_t1960, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.2).
narrative_ontology:measurement(wait_be_t1880, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1880, 0.3).
narrative_ontology:measurement(wait_be_t1920, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(wait_be_t1960, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(wait_su_t1880, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1880, 0.5).
narrative_ontology:measurement(wait_su_t1920, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(wait_su_t1960, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, identity_coordination).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_rights_legislation).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, resource_management_act_interpretation).

% DUAL FORMULATION NOTE:
% This constraint is one of three primary readings of the Treaty of Waitangi's sovereignty allocation kernel. It directly influences and is influenced by the Crown sovereignty and partnership readings, as their interpretations compete for legal and political dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
