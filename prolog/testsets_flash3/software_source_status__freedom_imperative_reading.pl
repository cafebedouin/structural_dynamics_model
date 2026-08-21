% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software as Injustice (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint story represents the 'freedom imperative' reading of
 *   software source status, where proprietary software is viewed as an
 *   ethical injustice. From this perspective, the lack of access to source
 *   code, and the restrictions on its use, modification, and distribution,
 *   constitute a fundamental violation of user freedom and a mechanism for
 *   extraction by vendors. The constraint is the proprietary licensing model
 *   itself, actively enforced through legal and technical means. The claimed
 *   type is 'snare' because the coordination story (vendor innovation,
 *   quality control) is seen as a cover for pure extraction and control, with
 *   identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.95).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.88).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software as Injustice (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0').
narrative_ontology:cs_kernel_codification('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', fixed_text).
narrative_ontology:cs_authority_grounding('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', extraction).
narrative_ontology:cs_interpretation_layer_present('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0').
narrative_ontology:cs_reading_relation('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', foundational, software_freedom_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(software_freedom_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', software_freedom_is_a_moral_imperative, deontological).
narrative_ontology:cs_axiom('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', foundational, proprietary_software_is_an_injustice).
narrative_ontology:cs_axiom_status(proprietary_software_is_an_injustice, holdable).
narrative_ontology:cs_axiom_grounding('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', proprietary_software_is_an_injustice, deontological).
narrative_ontology:cs_reference_frame('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', universal_software_freedom).
narrative_ontology:cs_drift_state('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', contemporary_digital_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9f4488ea-db40-4d99-b9cd-c5a3d5cec1b0', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, academic_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users are denied fundamental freedoms to inspect, modify, and share the software that runs their lives. They are forced to accept terms that restrict their autonomy and subject them to the control of vendors. Their identity as 'users' is defined by this lack of control.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users, payer,
    powerless, biographical, identity_locked, global).

% Vendors profit by restricting access to software source code, controlling distribution, and dictating terms of use. They actively enforce these restrictions through legal means (licenses) and technical means (DRM, obfuscation). They frame this as protecting intellectual property and funding innovation.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Developers are prevented from building upon, learning from, or contributing to a vast body of software due to proprietary restrictions. This limits their creative freedom and forces them into dependent relationships with vendors. Their ability to innovate is constrained by closed ecosystems.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Researchers are hindered in their ability to study, verify, and improve software systems critical to society. Proprietary barriers prevent scientific scrutiny and open collaboration, undermining the integrity of research and public knowledge.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, academic_researchers, payer,
    organized, generational, constrained, global).

% Advocates articulate the ethical imperative of software freedom and expose the injustices of proprietary models. They work to educate the public, develop free alternatives, and influence policy, but face immense institutional power from proprietary interests.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the proprietary vendor's perspective, it coordinates software development, distribution, and support under a single entity, ensuring quality control and a clear revenue model. From this reading's perspective, it coordinates control and extraction.
% TRANSFER_FUNCTION: Transfers control over software, the ability to modify it, and the knowledge embedded within its source code from users and the public domain to proprietary software vendors. It also transfers economic value (license fees, subscription revenue) to vendors.
% ABSENT_VOICES: The 'public' as a collective entity with a right to universal access to knowledge and tools is largely absent from the legal and economic frameworks that define software licensing. Future generations, whose digital heritage is being enclosed, also lack a voice.
% DISAPPEARANCE_RATIONALE: If proprietary software vanished overnight, the entire digital economy would undergo a radical transformation. Users would gain unprecedented control, new collaborative development models would emerge, and the power dynamics between software creators and users would fundamentally shift. It would be a profound re-ordering of digital society.
% FOUNDING_PROBLEM: The founding problem, from the perspective of proprietary software, was how to monetize software development and protect intellectual investment in a digital medium where copying is trivial.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary vendors claim the problem is still live, citing the need for continued funding for innovation. Free software advocates and some economists argue that alternative funding models exist and that the 'problem' has been reframed to justify ongoing extraction, with corroboration from the success of open-source ecosystems and public-funded software initiatives.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the core 'value' of software freedom is entirely denied to users, and significant economic value is transferred to vendors. Suppression is also very high (0.88) due to the combination of legal enforcement (copyright, EULAs) and technical enforcement (DRM, obfuscation) that actively prevents users from exercising control over their software. Theater ratio is low (0.1) because the enforcement is genuinely functional in maintaining proprietary control, not merely performative. Accessibility collapse is high (0.75) as alternatives (free software) are often marginalized or require significant effort to adopt, and resistance is high (0.8) reflecting the ongoing struggle by free software movements.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary software vendors perceive their licensing model as a legitimate 'rope' for coordinating development and protecting investment. However, from the 'freedom imperative' reading, this same structure is a 'snare' that systematically extracts freedom and control from users. The engine's classification will highlight this divergence based on the high extractiveness and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors are the clear beneficiaries and agenda-setters, as they design and enforce the terms that generate their revenue and control. Software users, independent developers, and academic researchers are the primary victims, bearing the costs of restricted access and control. Free software advocates act as analytical observers, exposing the structural dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a system of control and extraction as mere 'coordination' or 'property protection.' By identifying proprietary software as a snare, it highlights that its persistence relies on active suppression of alternatives and the suppression of user freedoms, rather than a genuine, mutually beneficial coordination function. The 'founding problem' of monetizing software is seen as having been 'solved' in ways that create new injustices, rather than a neutral technical challenge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_vs_economic_priority,
    'Is software primarily an ethical domain (freedom, justice) or an economic one (property, innovation incentives)?',
    'Societal consensus shift or legal redefinition of software''s fundamental nature. If software is legally reclassified as a public good or a fundamental right, the ethical framing gains legal force.',
    'If software is primarily ethical, the ''freedom imperative'' reading''s classification as a snare is strengthened, leading to calls for radical policy changes. If primarily economic, the ''property rights'' or ''utilitarian'' readings gain legitimacy, potentially reclassifying proprietary software as a tangled rope or even a rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethical_vs_economic_priority, conceptual, 'The fundamental framing of software''s nature.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/technical barriers) or internalized (users'' learned helplessness/acceptance of proprietary norms)?',
    'Post-exit suppression trajectory: if users continue to avoid free software even after legal/technical barriers are removed (e.g., through education and advocacy), reclassify as partially internalized. Surveys on user perception of control and agency.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — users carry the suppression with them after exit, making the snare more insidious. If purely structural, removing barriers would immediately empower users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in software use.').

omega_variable(
    kernel_reading_impact_on_property_rights,
    'How would the widespread adoption of the ''freedom imperative'' reading structurally impact the ''property_rights_reading'' of software source status?',
    'Analysis of legal and policy changes in jurisdictions where software freedom principles gain traction. Observation of shifts in industry norms and business models.',
    'If the ''freedom imperative'' reading gains dominance, it would likely ''foreclose'' the ''property_rights_reading'' within the same legal framework, leading to a redefinition of intellectual property rights for software. This would fundamentally alter the legal landscape for software development and distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_impact_on_property_rights, conceptual, 'Impact of this reading on the property rights framing of software.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_source_status__freedom_imperative_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__freedom_imperative_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__freedom_imperative_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__freedom_imperative_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(soft_tr_t2020, software_source_status__freedom_imperative_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__freedom_imperative_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_source_status__freedom_imperative_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(soft_be_t1990, software_source_status__freedom_imperative_reading, base_extractiveness, 1990, 0.85).
narrative_ontology:measurement(soft_be_t2000, software_source_status__freedom_imperative_reading, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(soft_be_t2010, software_source_status__freedom_imperative_reading, base_extractiveness, 2010, 0.93).
narrative_ontology:measurement(soft_be_t2020, software_source_status__freedom_imperative_reading, base_extractiveness, 2020, 0.95).
narrative_ontology:measurement(soft_be_t2024, software_source_status__freedom_imperative_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_source_status__freedom_imperative_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(soft_su_t1990, software_source_status__freedom_imperative_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(soft_su_t2000, software_source_status__freedom_imperative_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(soft_su_t2010, software_source_status__freedom_imperative_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(soft_su_t2020, software_source_status__freedom_imperative_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(soft_su_t2024, software_source_status__freedom_imperative_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, digital_rights_management_systems).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, intellectual_property_law_software).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, open_source_licensing_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
