% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Property Rights Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint models the 'property rights' reading of software control,
 *   where software is treated as intellectual property, granting creators
 *   exclusive rights to control its use, modification, and distribution. This
 *   reading emphasizes the protection of investment and commercial
 *   sustainability as the primary justification for these restrictions. It is
 *   one of several competing readings of the 'software_control_legitimacy'
 *   kernel. The claimed type is 'tangled_rope' because it genuinely
 *   coordinates commercial software development while extracting value from
 *   users and FOSS advocates through restrictions.
 *
 * KEY AGENTS:
 *   - software_vendors: Agenda-setter (institutional/mobile) — enforce property rights
 *   - software_investors: Beneficiary (powerful/arbitrage) — profit from property rights
 *   - foss_advocates: Payer (organized/constrained) — bear costs of restricted freedom
 *   - users_seeking_modification: Payer (moderate/constrained) — denied ability to modify software
 *   - intellectual_property_lawyers: Beneficiary (institutional/mobile) — profit from IP enforcement
 *   - legal_systems: Agenda-setter (institutional/analytical) — provide and enforce IP framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.45).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.6).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'f49ba4cb-db2a-4caa-9ca5-4b83197f5973').
narrative_ontology:cs_kernel_codification('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', formalized).
narrative_ontology:cs_authority_grounding('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', lineage).
narrative_ontology:cs_interpretation_layer_present('f49ba4cb-db2a-4caa-9ca5-4b83197f5973').
narrative_ontology:cs_reading_relation('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', foundational, software_is_intellectual_property).
narrative_ontology:cs_axiom_status(software_is_intellectual_property, holdable).
narrative_ontology:cs_axiom_grounding('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', software_is_intellectual_property, conventional).
narrative_ontology:cs_axiom('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', foundational, investment_requires_exclusive_rights).
narrative_ontology:cs_axiom_status(investment_requires_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', investment_requires_exclusive_rights, instrumental).
narrative_ontology:cs_reference_frame('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', classical_intellectual_property_framework).
narrative_ontology:cs_drift_state('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', contemporary_digital_commons_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f49ba4cb-db2a-4caa-9ca5-4b83197f5973', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, users_seeking_modification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, intellectual_property_lawyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell proprietary software, relying on intellectual property laws to protect their investment and revenue streams. They actively enforce licensing agreements and advocate for strong copyright protections.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, biographical, mobile, global).

% Provide capital to software companies, expecting returns based on the ability of those companies to control and monetize their software. They benefit directly from strong property rights in software.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Believe in the free use, modification, and distribution of software. They bear the cost of proprietary restrictions by being denied the ability to freely adapt and share software, which they view as a fundamental right or public good.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, constrained, global).

% Desire to modify software for personal use, customization, or to fix bugs, but are prevented by licensing terms and technical restrictions. Their options are limited to using alternative (often less functional) software or violating terms of service.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, users_seeking_modification, payer,
    moderate, immediate, constrained, local).

% Specialize in enforcing software copyrights and patents, benefiting from the complexity and litigation arising from property rights claims in software. They are key enforcers of this constraint.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, intellectual_property_lawyers, beneficiary,
    institutional, biographical, mobile, national).

% Provide the framework for intellectual property rights, adjudicating disputes and enforcing judgments that uphold software control as a property right. They are the ultimate enforcers of this constraint.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment and innovation in software development by providing a legal framework that assures creators they can monetize their work, thereby incentivizing commercial software production.
% TRANSFER_FUNCTION: Transfers control over software use, modification, and distribution from users and the public domain to software creators and their investors, in exchange for the creation of commercial software products.
% ABSENT_VOICES: Advocates for a 'digital commons' or 'information wants to be free' perspective, who would argue that software is fundamentally different from physical property and should not be subject to the same restrictive rights. They are largely excluded from the legislative and judicial processes that define IP law.
% DISAPPEARANCE_RATIONALE: If software property rights vanished overnight, the commercial software industry as we know it would collapse, leading to a massive shift towards open-source or alternative funding models. Investment in proprietary software would cease, and the digital economy would fundamentally reorganize.
% FOUNDING_PROBLEM: How to incentivize the creation of complex software when it is easily copied and distributed, ensuring creators can recoup their significant development costs and make a profit.
% FOUNDING_PROBLEM_CORROBORATION: Software vendors and investors universally attest that the problem is live, citing high development costs and the ease of digital piracy. While FOSS advocates offer alternative models, the commercial sector's reliance on IP protection is widely acknowledged by economic observers and legal scholars as a primary driver for proprietary software development.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).
:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while it enables commercial activity, it restricts fundamental freedoms of use and modification that many believe should be inherent to software. Suppression (0.6) is significant due to legal enforcement mechanisms (copyright, patents, EULAs) and technical measures (DRM) that actively prevent unauthorized use or modification. Theater ratio (0.1) is low, as the enforcement is largely functional, directly serving the goal of protecting commercial interests. Accessibility collapse (0.4) is moderate; alternatives (FOSS) exist but often lack the market penetration or specific features of proprietary software. Resistance (0.5) is also moderate, driven by ongoing FOSS advocacy, legal challenges, and user communities seeking workarounds.
 *
 * PERSPECTIVAL GAP:
 *   Software vendors and investors perceive this as a legitimate and necessary framework for innovation, experiencing it as a 'rope' or even a 'mountain' of economic reality. FOSS advocates and users seeking modification experience it as a 'snare' or 'tangled rope' that restricts their freedoms and extracts value. The engine's per-seat classification will reflect these divergences based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and investors are clear beneficiaries (low d) as the constraint directly enables their business model and returns. FOSS advocates and users seeking modification are targets (high d) as they bear the costs of restricted access and control. Intellectual property lawyers also benefit (low d) from the enforcement complexity. Legal systems act as agenda-setters, enforcing the constraint, with a more neutral directionality (d=0.5) as they are meant to balance interests, though their operation often favors established property rights.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (incentivizing software creation) is still live, but its persistence is increasingly contested by those who argue that alternative models (like FOSS) can also drive innovation without the same extractive costs. The classification as 'tangled_rope' reflects this hybrid nature: a genuine coordination function (incentivizing commercial software) coupled with asymmetric extraction (restricting user freedoms and benefiting specific commercial entities). It prevents mislabeling as a pure 'rope' by acknowledging the victims, or as a pure 'snare' by recognizing the legitimate coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    software_nature_ambiguity,
    'Is software fundamentally a form of property analogous to physical goods, or is it a unique informational artifact that requires a different legal framework?',
    'A philosophical and legal consensus shift, potentially driven by new technological paradigms (e.g., AI-generated code, decentralized autonomous organizations).',
    'If software is reclassified as non-property, the entire basis for this constraint collapses, leading to a ''freedom_imperative_reading'' or ''commons_reading'' becoming dominant. If affirmed as property, the constraint''s legitimacy is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(software_nature_ambiguity, conceptual, 'The fundamental ontological status of software as property.').

omega_variable(
    innovation_incentive_efficacy,
    'To what extent do strong software property rights genuinely incentivize innovation, versus merely enabling rent-seeking or stifling follow-on innovation?',
    'Longitudinal economic studies comparing innovation rates and market concentration in jurisdictions with varying IP regimes, or across different software sectors (e.g., proprietary vs. open source).',
    'If strong IP is shown to stifle innovation or primarily enable rent-seeking, the ''pragmatic_openness_reading'' or ''commons_reading'' gains empirical support, weakening the justification for this constraint''s extractiveness. If it strongly correlates with innovation, the constraint''s ''rope'' aspect is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_efficacy, empirical, 'Empirical effectiveness of IP in driving software innovation.').

omega_variable(
    reading_legitimacy_contest,
    'Given the existence of multiple coherent readings of software control, which reading''s framing is most widely accepted as legitimate by the broader public and policymakers?',
    'Public opinion surveys, legislative debates, and judicial rulings over time, tracking the prevalence and influence of each reading in policy and law.',
    'If the ''property_rights_reading'' loses legitimacy, its enforcement becomes more difficult and costly, increasing resistance and potentially shifting its classification towards a ''snare'' or ''piton'' if its coordination function atrophies. If it gains legitimacy, its ''rope'' aspects are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, preference, 'The relative legitimacy of the ''property_rights_reading'' compared to its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_control_legitimacy__property_rights_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__property_rights_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__property_rights_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__property_rights_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__property_rights_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_control_legitimacy__property_rights_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__property_rights_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__property_rights_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__property_rights_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__property_rights_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_control_legitimacy__property_rights_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__property_rights_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__property_rights_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__property_rights_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__property_rights_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, digital_rights_management_enforcement).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, patent_troll_litigation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, open_source_licensing_compliance).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel. Each reading represents a distinct structural claim about the nature and purpose of software control, with different beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
