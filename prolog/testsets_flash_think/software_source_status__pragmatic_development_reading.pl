% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Pragmatic Development Superiority
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic development' reading of
 *   software source status, asserting that open source is a superior
 *   development methodology because freedom (transparency, modifiability) is
 *   instrumental to quality. It emphasizes the practical benefits like peer
 *   review, bug detection, and innovation velocity. While claimed as a 'rope'
 *   (a beneficial coordination mechanism), the authored metrics reflect the
 *   implicit costs of non-conformity and the subtle suppression of
 *   alternative models through discursive framing, leading to a divergence
 *   between the claimed type and the computed classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.45).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.55).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Pragmatic Development Superiority").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'a495b3e8-df76-489a-93e3-7d305a4d1f07').
narrative_ontology:cs_kernel_codification('a495b3e8-df76-489a-93e3-7d305a4d1f07', implicit).
narrative_ontology:cs_authority_grounding('a495b3e8-df76-489a-93e3-7d305a4d1f07', practice).
narrative_ontology:cs_interpretation_layer_present('a495b3e8-df76-489a-93e3-7d305a4d1f07').
narrative_ontology:cs_reading_relation('a495b3e8-df76-489a-93e3-7d305a4d1f07', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('a495b3e8-df76-489a-93e3-7d305a4d1f07', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('a495b3e8-df76-489a-93e3-7d305a4d1f07', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a495b3e8-df76-489a-93e3-7d305a4d1f07', foundational, transparency_drives_quality).
narrative_ontology:cs_axiom_status(transparency_drives_quality, holdable).
narrative_ontology:cs_axiom_grounding('a495b3e8-df76-489a-93e3-7d305a4d1f07', transparency_drives_quality, empirically_contingent).
narrative_ontology:cs_axiom('a495b3e8-df76-489a-93e3-7d305a4d1f07', foundational, collaboration_accelerates_innovation).
narrative_ontology:cs_axiom_status(collaboration_accelerates_innovation, holdable).
narrative_ontology:cs_axiom_grounding('a495b3e8-df76-489a-93e3-7d305a4d1f07', collaboration_accelerates_innovation, empirically_contingent).
narrative_ontology:cs_reference_frame('a495b3e8-df76-489a-93e3-7d305a4d1f07', collaborative_innovation_paradigm).
narrative_ontology:cs_drift_state('a495b3e8-df76-489a-93e3-7d305a4d1f07', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a495b3e8-df76-489a-93e3-7d305a4d1f07', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, tech_companies_using_oss).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, users_of_oss).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, academic_researchers).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively contribute to and benefit from the collaborative development model, gaining reputation, skills, and access to high-quality tools. They are the primary producers and consumers of the methodology.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_developers, beneficiary,
    moderate, biographical, mobile, global).

% Leverage open source software for their products and services, benefiting from lower development costs, shared maintenance, and a large talent pool. They contribute strategically to maintain the ecosystem.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, tech_companies_using_oss, beneficiary,
    organized, generational, arbitrage, global).

% Benefit from access to high-quality, often free software, with greater transparency and security due to peer review. Their choices are influenced by the perceived superiority of open source solutions.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, users_of_oss, beneficiary,
    moderate, biographical, mobile, global).

% Bear the cost of competing with a development model framed as superior, facing pressure to adopt open source practices or justify their closed approach. Their market share and legitimacy are challenged by this narrative.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    powerful, generational, constrained, global).

% Study the efficacy, security, and economic impact of open source development, often providing empirical evidence that reinforces or challenges the claims of superiority. They benefit from the open data and collaborative nature of the field.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, academic_researchers, observer,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, academic_researchers, beneficiary).

% Administer and promote open source licenses and best practices, shaping the norms and legal framework that underpin the pragmatic development model. They advocate for its adoption and defend its principles.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_foundations_licensing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, diffuse).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates large-scale, distributed software development, peer review, and collaborative innovation by emphasizing transparency, shared access to source code, and community-driven improvement.
% TRANSFER_FUNCTION: Transfers knowledge, code, and development effort among participants, leading to shared, higher-quality, and more secure software. It also implicitly transfers market share and legitimacy from proprietary models to open source by framing the latter as superior.
% ABSENT_VOICES: Proprietary software advocates who argue that their model is equally valid or superior for specific contexts (e.g., highly specialized, mission-critical, or consumer-facing applications requiring strict control), but whose arguments are often marginalized within the dominant discourse of open source's pragmatic superiority.
% DISAPPEARANCE_RATIONALE: If the belief in open source's pragmatic superiority vanished overnight, the collaborative development paradigm would fragment. Software development would likely revert to more proprietary, siloed approaches, innovation velocity would slow, and the current ecosystem of shared tools and knowledge would largely collapse, reorganizing around closed, competitive models.
% FOUNDING_PROBLEM: The inefficiencies, bugs, security vulnerabilities, and lack of transparency inherent in traditional proprietary software development, leading to vendor lock-in, slower innovation cycles, and limited user control.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of open source, including foundations and many developers, attest that these problems are still live and that open source continues to offer the best solutions. Independent academic studies on software quality, security, and innovation rates often provide corroborating evidence for the benefits of open source, though the degree of 'superiority' can be contested by proprietary vendors and some economic analyses.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'rope' because proponents genuinely believe it offers superior coordination and benefits. However, the metrics reflect a more complex reality: 'extractiveness' (0.45) captures the opportunity costs for those who don't fully participate or conform, and the implicit 'tax' of adhering to community norms. 'Suppression' (0.55) arises not from direct coercion, but from the strong social and professional pressure to adopt open source, and the framing of proprietary alternatives as inherently inferior or less innovative, which limits their perceived legitimacy and market access. 'Theater ratio' (0.2) is low but acknowledges some performative aspects in the promotion of 'openness' even when practical benefits might be context-dependent. The time series shows a gradual increase in extractiveness and suppression as the open source paradigm gains dominance, making non-participation more costly.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of open source proponents, this is a clear 'rope' – a superior way to coordinate development that benefits all. From the perspective of proprietary vendors, it's a constraint that implicitly extracts market share and legitimacy, forcing them to adapt or be marginalized. The engine's computation will highlight this divergence between the claimed 'rope' and the metrics' indication of higher extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Open source developers, companies using OSS, and users are beneficiaries, gaining from shared resources and quality. Proprietary software vendors are payers, bearing the cost of competition and the challenge to their business model's legitimacy. Open source foundations act as agenda-setters, promoting and enforcing the norms of this development methodology. The 'superiority' claim implicitly directs benefits to those who conform and costs to those who do not.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_strength,
    'How robust and generalizable is the empirical evidence for open source''s pragmatic superiority (quality, security, innovation velocity) across all software domains and contexts?',
    'Comprehensive meta-analysis of independent studies comparing open and closed source projects across diverse metrics and application types, controlling for project size, funding, and developer expertise.',
    'If evidence for universal superiority is weak or highly context-dependent, the ''superiority'' claim becomes more theatrical, increasing the constraint''s effective theater_ratio and potentially reclassifying it towards a ''tangled_rope'' or ''snare'' for those implicitly forced to conform without realizing the claimed benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_strength, empirical, 'The strength and generalizability of empirical claims underpinning open source''s pragmatic superiority.').

omega_variable(
    cost_of_participation_ambiguity,
    'What are the true, unacknowledged costs of participation in open source development (e.g., time, learning curve, community politics, maintenance burden for downstream users), and how do they affect the net benefit for different stakeholders?',
    'Detailed economic and sociological studies of open source project participation, including opportunity costs, hidden labor, and the distribution of benefits and burdens among contributors and users.',
    'If unacknowledged costs are substantial and disproportionately borne by certain groups (e.g., smaller developers, less resourced companies), the constraint''s effective extractiveness would be higher, pushing it closer to a ''tangled_rope'' by revealing hidden asymmetric transfers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_participation_ambiguity, empirical, 'The hidden costs and asymmetric distribution of burdens in open source participation.').

omega_variable(
    discursive_suppression_mechanism,
    'To what extent does the discourse of ''open source superiority'' actively suppress the development or adoption of proprietary alternatives, beyond mere competitive advantage?',
    'Content analysis of industry publications, academic papers, and developer forums to identify rhetorical strategies that delegitimize proprietary models, combined with market analysis of proprietary software''s decline in areas where open source is dominant, controlling for other competitive factors.',
    'If discursive suppression is a significant factor, the constraint''s effective suppression would be higher, indicating that the ''superiority'' claim functions as a mechanism to limit exit options and reinforce the dominant paradigm, pushing it towards a ''tangled_rope'' or ''snare'' for proprietary vendors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discursive_suppression_mechanism, conceptual, 'Whether the ''superiority'' claim functions as a discursive suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1995, software_source_status__pragmatic_development_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__pragmatic_development_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(soft_tr_t2005, software_source_status__pragmatic_development_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__pragmatic_development_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(soft_tr_t2015, software_source_status__pragmatic_development_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(soft_tr_t2020, software_source_status__pragmatic_development_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(soft_tr_t2025, software_source_status__pragmatic_development_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(soft_be_t1995, software_source_status__pragmatic_development_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(soft_be_t2000, software_source_status__pragmatic_development_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(soft_be_t2005, software_source_status__pragmatic_development_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(soft_be_t2010, software_source_status__pragmatic_development_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(soft_be_t2015, software_source_status__pragmatic_development_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(soft_be_t2020, software_source_status__pragmatic_development_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(soft_be_t2025, software_source_status__pragmatic_development_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1995, software_source_status__pragmatic_development_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(soft_su_t2000, software_source_status__pragmatic_development_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(soft_su_t2005, software_source_status__pragmatic_development_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(soft_su_t2010, software_source_status__pragmatic_development_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(soft_su_t2015, software_source_status__pragmatic_development_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(soft_su_t2020, software_source_status__pragmatic_development_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(soft_su_t2025, software_source_status__pragmatic_development_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'software_source_status' kernel, focusing on the pragmatic benefits of open development (quality, security, innovation) as opposed to ethical imperatives or property rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
