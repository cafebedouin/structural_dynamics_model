% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: Near-Term Harms Priority Reading of AI Alignment
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint instantiates the nearterm_harms_reading of the contested
 *   kernel ai_alignment_priority: the claim that AI alignment means
 *   preventing present discriminatory and extractive harms from deployed
 *   systems, with priority given to justice for marginalized populations. The
 *   constraint models the institutionalized governance arrangement that
 *   enacts this reading through sociotechnical audit methodologies, fairness
 *   metric regimes, and resource flows to bias mitigation infrastructure. The
 *   reading competes with an existential_risk_reading (alignment as
 *   preventing catastrophic loss of control) and an integrated_reading (both
 *   as complementary priorities). From the authoring seat, this is a Tangled
 *   Rope: there is genuine coordination in standardizing harm detection, but
 *   the arrangement asymmetrically extracts legitimacy for tech corporations
 *   and career paths for the audit industry while marginalized communities
 *   bear ongoing harms and audit participation costs.
 *
 * KEY AGENTS:
 *   - ai_ethics_audit_sector: Primary agenda-setter (organized/constrained) â defines audit methodology, administers fairness evaluations, captures resource flows
 *   - tech_corporations: Agenda-setter and beneficiary (institutional/arbitrage) â deploys systems, commissions audits, captures legitimacy
 *   - present_vulnerable_populations: Named beneficiary (powerless/trapped) â receives selective harm reduction without controlling audit priorities
 *   - marginalized_groups: Primary target/payer (powerless/trapped) â bears ongoing harms and audit extraction labor
 *   - x_risk_research_community: Excluded actor (organized/constrained) â deprioritized by near-term framing dominance
 *   - integrated_alignment_researchers: Excluded actor (organized/constrained) â complementary framing marginalized by priority claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.78).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.65).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Near-Term Harms Priority Reading of AI Alignment").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '3d6d8c02-8a52-492a-8c87-5bdedbe2b25f').
narrative_ontology:cs_kernel_codification('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', distributed).
narrative_ontology:cs_authority_grounding('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', distributed).
narrative_ontology:cs_reading_relation('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', ai_alignment_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', foundational, alignment_scope_present_harms).
narrative_ontology:cs_axiom_status(alignment_scope_present_harms, holdable).
narrative_ontology:cs_axiom_grounding('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', alignment_scope_present_harms, empirically_contingent).
narrative_ontology:cs_axiom('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', foundational, justice_priority_over_speculative_safety).
narrative_ontology:cs_axiom_status(justice_priority_over_speculative_safety, holdable).
narrative_ontology:cs_axiom_grounding('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', justice_priority_over_speculative_safety, deontological).
narrative_ontology:cs_reference_frame('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', present_harm_mitigation_framework).
narrative_ontology:cs_drift_state('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', corporate_audit_dominance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d6d8c02-8a52-492a-8c87-5bdedbe2b25f', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ai_ethics_audit_sector).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, tech_corporations).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers sociotechnical audit methodologies for deployed AI systems. Receives institutional funding, contracts, and career pathways for bias assessments, fairness tool development, and compliance reporting. Research agendas and professional trajectories are tied to the continued dominance of near-term harm framing in AI governance.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_ethics_audit_sector, agenda_setter,
    organized, biographical, constrained, global).

% Deploy AI systems and commission or conduct internal audits under the near-term harms framing. Uses audit results and fairness certifications to legitimate continued deployment, deflect structural regulation, and demonstrate accountability to regulators and the public. Can pivot messaging or geographic jurisdiction if governance winds shift.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, tech_corporations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, tech_corporations, beneficiary).

% Are the named beneficiaries of bias mitigation and fairness audit programs. Receive selective protection when audits successfully identify and force remediation of discriminatory system behavior. Cannot opt out of the AI systems that affect them and do not control the audit priorities, methodologies, or remediation timelines.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, national).

% Bear the ongoing discriminatory and extractive impacts of deployed AI systems that audits fail to prevent or that are legitimized as 'fair enough' by audit theater. Provide data, feedback labor, and participation in fairness evaluations without receiving structural power over system design, ownership, or governance. Experience the constraint as ongoing extraction dressed in the language of protection.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_groups, payer,
    powerless, immediate, trapped, national).

% Would advocate for existential risk as the primary alignment priority but is structurally excluded from funding streams, policy discourse, and institutional priority-setting dominated by the near-term harms framing. Their research agenda is treated as speculative or secondary within institutions that have adopted this reading.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, x_risk_research_community, excluded,
    organized, generational, constrained, global).

% Advocate for treating catastrophic and present harms as complementary priorities rather than competitors. Are marginalized in funding and institutional priority-setting when the near-term harms reading dominates exclusively, forcing them to justify integrated approaches through near-term framing to access resources and attention.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, integrated_alignment_researchers, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the identification and remediation of discriminatory outputs in deployed AI systems through standardized sociotechnical audit methodologies, creating shared frameworks for measuring bias across platforms and deployment contexts.
% TRANSFER_FUNCTION: Moves financial and human resources to bias mitigation infrastructure and audit operations; moves legitimacy and regulatory forbearance to tech corporations via fairness certifications; moves the costs of ongoing system harms, data extraction, and participation labor onto marginalized communities.
% ABSENT_VOICES: Existential risk researchers and integrated researchers are excluded from priority-setting discourse; community organizers who reject corporate audit frameworks in favor of structural power redistribution and community governance are absent from institutional methodology design.
% DISAPPEARANCE_RATIONALE: If the near-term harms priority framework vanished, resources would shift away from audit infrastructure toward other governance mechanisms or toward sibling readings; tech corporations would lose a primary legitimacy shield against structural regulation; marginalized communities would face unmediated harms but might gain leverage for demands outside the audit paradigm.
% FOUNDING_PROBLEM: Deployed AI systems were producing measurable discriminatory outcomes against protected groups without standardized accountability mechanisms or reliable evaluation methods.
% FOUNDING_PROBLEM_CORROBORATION: Affected communities and independent civil society researchers attest that discriminatory harms persist and are inadequately addressed by current audit methodologies. Tech corporations and the audit industry attest that the problem is being actively managed through their programs. Academic critics and some regulators attest that while the founding harm is real, the current arrangement has shifted toward legitimizing continued deployment rather than preventing extraction.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the audit infrastructure extracts substantial resources and legitimacy while delivering incomplete protection; suppression is moderate-high (0.65) because the near-term framing actively displaces alternative alignment vocabularies in funding and policy discourse; theater_ratio is moderate-high (0.55) because a growing share of audit activity functions as performative legitimacy for continued deployment rather than structural harm prevention. Accessibility_collapse is moderate (0.50) because alternatives (community-led governance, structural reform, x-risk research) persist at the margins despite institutional marginalization. Resistance is moderate (0.45) because excluded researchers and affected communities actively contest the framework. The temporal series show monotonic increases in extractiveness and theater from 2016-2026 as the audit industry matured and corporatized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (marginalized_groups) experiences this constraint as ongoing extraction dressed in protective language; the agenda-setter seat (ai_ethics_audit_sector) experiences it as legitimate coordination solving a real problem; the beneficiary seat (present_vulnerable_populations) experiences mixed effects â selective protection alongside continued vulnerability. The engine should compute these seats differently: low directionality for audit sector and tech corporations, high directionality for marginalized_groups, and near-symmetric for vulnerable populations who receive both benefits and costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to concentrated capture: the ai_ethics_audit_sector receives dedicated funding streams and institutional positioning; tech_corporations capture legitimacy and regulatory deflection; present_vulnerable_populations receive diffuse coordination benefits. Victim declarations map to structural extraction: marginalized_groups bear ongoing discriminatory impacts, provide uncompensated data and feedback labor, and suffer audit theater that legitimizes continued deployment. The divergence between declared beneficiaries and actual extraction drives the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â discriminatory AI outputs without accountability â remains live, which prevents pure snare classification. However, the coordination function has atrophied into corporate legitimacy extraction: audits increasingly certify systems as 'fair enough' to continue deployment rather than preventing harms. Because the founding problem is contested and the audit machinery persists beyond its protective efficacy, the constraint is not a pure rope. Tangled Rope captures the hybrid: genuine coordination infrastructure co-opted for asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alignment_kernel_reading_contest,
    'This constraint is the nearterm_harms_reading of kernel ai_alignment_priority; the existential_risk_reading would reallocate priority to catastrophic loss-of-control scenarios, while the integrated_reading would treat both as complementary. Does this reading''s priority claim structurally suppress sibling readings, or can they coexist in resource allocation?',
    'Comparative funding and publication trajectory analysis across the three readings; tracking whether dominance of near-term framing correlates with absolute resource decline in x-risk research or merely relative priority shift.',
    'If structural suppression is demonstrated, the constraint reads more strongly as snare; if mere coexistence with priority difference, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_kernel_reading_contest, conceptual, 'Committing uncertainty about this reading''s relationship to sibling readings in the alignment priority kernel').

omega_variable(
    corporate_legitimacy_extraction,
    'Does the deployed system audit framework primarily extract legitimacy for tech corporations while delivering thin coordination benefits to vulnerable populations?',
    'Independent outcome measurement comparing audit frequency and fairness metric publication rates against actual discriminatory impact reduction in targeted deployment domains.',
    'High extraction with thin coordination would confirm tangled_rope leaning toward snare; measurable sustained protection would support stronger coordination classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_legitimacy_extraction, empirical, 'Whether audit infrastructure serves corporate legitimacy or community protection').

omega_variable(
    beneficiary_victim_overlap_ambiguity,
    'Present vulnerable populations and specific marginalized groups overlap substantially; is the constraint''s extraction falling on the same groups it claims to coordinate, or are there distinct capturer and bearer classes?',
    'Demographic and resource-flow analysis tracking who receives audit contracts, who experiences post-audit system impacts, and whether protected groups see net harm reduction or net extraction.',
    'If extraction and coordination map to the same agents, the constraint may read as rope from the beneficiary seat and snare from the payer seat; distinct classes would clarify asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_overlap_ambiguity, empirical, 'Overlap between beneficiary and victim populations in nearterm harm alignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 6, 0.46).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.74).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel ai_alignment_priority. The nearterm_harms_reading models alignment as present harm prevention with justice priority; sibling readings model existential risk and integrated framings. Per the epsilon-invariance principle, each reading requires a separate constraint story because they have distinct beneficiary/victim structures, different epsilon values, and incompatible priority claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
