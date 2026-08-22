% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Derivative Work Trigger (Broad Copyleft Reading)
 *   domain: software_licensing/copyright_law
 *
 * SUMMARY:
 *   This constraint captures the broad copyleft reading of the GPL's
 *   derivative-work clause: the act of dynamically linking GPL-licensed code
 *   into any other code creates a derivative work, triggering source
 *   disclosure obligations for the entire linked product. This reading
 *   interprets the GPL as a strong commons-protection mechanism that pulls
 *   dependent codebases into GPL territory, ensuring that improvements and
 *   derivative works remain available to downstream users. The reading is one
 *   of three competing interpretations of the same GPL kernel: the
 *   narrow-linking-permissive reading treats linking as aggregation, not
 *   derivation; the interface-boundary reading permits tight coupling across
 *   clean API boundaries without derivative-work triggering. This story
 *   instantiates ONLY the broad reading, as a complete and self-contained
 *   constraint.
 *
 * KEY AGENTS:
 *   - gpl_commons_projects: beneficiaries of the broad interpretation; their copyleft reach depends on derivative-work scope being expansive
 *   - downstream_users: beneficiaries; guaranteed source access and modification rights for entire work under this reading
 *   - proprietary_vendors_integrating_gpl: payers; face compliance burden and strategic constraints on product design
 *   - proprietary_libraries_linked_to_gpl: payers; risk GPL contamination and loss of proprietary control
 *   - standards_bodies_and_advocates: observers; FSF, SFC, courts produce authoritative readings and precedent
 *   - proprietary_software_ecosystem: excluded; positioned as potential violators rather than participants in GPL governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.71).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Derivative Work Trigger (Broad Copyleft Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, 'f7ce9498-6086-42d9-b9bc-3d1ac76e4ede').
narrative_ontology:cs_kernel_codification('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', fixed_text).
narrative_ontology:cs_authority_grounding('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', lineage).
narrative_ontology:cs_interpretation_layer_present('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede').
narrative_ontology:cs_reading_relation('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', foundational, derivative_work_expansive_linking_trigger).
narrative_ontology:cs_axiom_status(derivative_work_expansive_linking_trigger, holdable).
narrative_ontology:cs_axiom_grounding('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', derivative_work_expansive_linking_trigger, deontological).
narrative_ontology:cs_axiom('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', secondary, copyleft_binary_choice_architecture).
narrative_ontology:cs_axiom_status(copyleft_binary_choice_architecture, holdable).
narrative_ontology:cs_axiom_grounding('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', copyleft_binary_choice_architecture, instrumental).
narrative_ontology:cs_reference_frame('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', copyleft_commons_protection_mandate).
narrative_ontology:cs_drift_state('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', contemporary_jurisdictional_divergence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7ce9498-6086-42d9-b9bc-3d1ac76e4ede', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, commons_dependent_projects).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors_integrating_gpl).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_libraries_linked_to_gpl).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_commons_projects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Projects that distribute GPL-licensed code benefit from this reading: any code dynamically linked to their codebase becomes GPL-obligated, pulling dependent codebases into the commons. They gain access to improvements and ensure their licensing terms remain effective at controlling derivative work behavior. Their strategic interest is in maintaining copyleft reach.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_commons_projects, beneficiary,
    organized, generational, arbitrage, global).

% Users who receive software incorporating GPL code gain source access and freedom to modify and redistribute, protected by the copyleft mechanism. Under this reading, they are guaranteed source disclosure for the entire work because linking triggers derivative-work status. Their exit option is to use proprietary closed-source software exclusively, but many popular tools now incorporate GPL code, making exit impractical.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users, beneficiary,
    powerless, biographical, trapped, global).

% Commercial software vendors who want to incorporate GPL libraries into their proprietary products face a compliance burden under this reading: if dynamic linking triggers derivative-work status, they must either release their entire codebase as GPL, abandon the GPL component, or redesign around it. They argue that linking is aggregation, not derivation, and should not trigger source disclosure. Their options are expensive: redesigning, paying for proprietary licenses, or relocating to permissive-licensed alternatives (which may not exist or may lag in features).
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors_integrating_gpl, payer,
    powerful, biographical, constrained, global).

% Proprietary library publishers whose code is dynamically linked to GPL applications face GPL contamination risk. Under this reading, their code becomes part of a derivative work and they must either relicense under GPL (losing proprietary control), refactor their distribution model, or accept that their library triggers GPL obligations downstream. Their exit is market repositioning: publish as permissive-licensed or proprietary-but-incompatible.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_libraries_linked_to_gpl, payer,
    organized, generational, constrained, global).

% Free software foundations, copyright lawyers, and governance bodies (FSF, Software Freedom Conservancy, courts) interpret the GPL's language and scope. They produce the authoritative readings of what constitutes a derivative work. Under this reading they have endorsed a broad interpretation of derivative work to protect commons; they also produce counterreadings and precedents that others cite against this view.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, standards_bodies_and_advocates, observer,
    institutional, generational, analytical, global).

% The broader proprietary software industry is excluded from the decision-making authority that interprets GPL derivative-work scope. They respond through licensing strategy, lobbying for permissive alternatives, and sometimes litigation; but they are not seated at the table where the GPL's meaning is authoritatively determined. Their exclusion is enforced by the license text itself: only the licensor (GPL author) and downstream recipients can invoke it; proprietary integrators are positioned as potential violators, not participants in a cooperative framework.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_ecosystem, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_commons_projects).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a distributed cooperative agreement: any code linked to GPL code becomes subject to GPL terms, ensuring that improvements and derivative works remain available to the commons rather than being enclosed in proprietary software. Solves the commons-protection problem: how to prevent tragedy of the commons where GPL code is incorporated into proprietary products without obligation to contribute back.
% TRANSFER_FUNCTION: Transfers source code access rights and modification freedoms from proprietary vendors to downstream users and the commons community. Vendors who want to use GPL code must transfer their modified or dependent code into the GPL sphere (or find alternatives), creating a one-way ratchet that continuously expands the commons at the expense of proprietary enclosure.
% ABSENT_VOICES: The proprietary software vendors most affected are excluded from the initial GPL authorship and interpretation process. Standards bodies and courts interpret the scope retrospectively, but proprietary vendors cannot directly shape the reading of what 'derivative work' means before the constraint is applied to them. The narrow-reading and interface-boundary advocates are present in legal disputes but structurally disadvantaged in norm-setting forums.
% DISAPPEARANCE_RATIONALE: If this reading were overturned and derivative-work triggering no longer applied to linked code, proprietary software vendors could freely link GPL libraries into closed-source products without obligation. The commons would lose its primary enforcement mechanism; GPL code would still be open-source, but vendors would incorporate improvements from it without contributing back. The market for GPL-compatible proprietary software would expand dramatically, and the power dynamics between commons projects and commercial software would shift toward enclosure.
% FOUNDING_PROBLEM: In the 1980s–1990s, free software was vulnerable to enclosure: a vendor could take GPL code, link it into a proprietary product, and distribute the result without contributing source back. The GPL's copyleft mechanism was designed to prevent this by making the entire derived work subject to GPL terms, ensuring commons continuity.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and copyright scholars outside the proprietary industry attest that enclosure remains a live threat; courts in multiple jurisdictions have enforced broad derivative-work interpretations (e.g., Jacobsen v. Katzer, 2008). Proprietary vendors and permissive-license advocates attest that the founding problem is overstated or has been solved by market segmentation; they cite economic arguments that proprietary and open-source can coexist without GPL contamination. Academic literature from both sides exists; the contest is genuine and unresolved in consensus.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end, reflecting a moderate-to-high extraction from proprietary vendors: the broad reading extracts compliance cost and strategic limitation (vendors must either open-source, refactor, or abandon GPL components). It is not pure snare (the commons projects genuinely benefit from collaborative commons access, not just rent collection), and it is not pure rope (the extraction is asymmetric: beneficiaries gain without paying; payers lose strategic optionality). The reading sits at the rope-to-tangled-rope boundary: genuine coordination function (commons protection, commons collaboration) paired with asymmetric extraction (vendors forced to choose costly alternatives). Suppression is 0.71: the constraint's persistence depends on active enforcement through licensing compliance mechanisms, litigation, and norm enforcement in package managers and procurement processes — alternative interpretations (narrow reading, interface boundary) are actively suppressed by forum authority and community governance. Theater ratio is 0.28, moderate: enforcement activity is mostly real (actual license compliance checking, legal action against violators), but some activity is performative (organizations that adopt GPL more for signal value than compliance, communities that cite GPL strength without understanding linking semantics). Accessibility collapse is 0.62: alternatives exist (permissive licenses, proprietary libraries, redesign around the constraint) but require substantial costly effort, especially for large proprietary codebases already using GPL libraries. Resistance is 0.58: the constraint meets real resistance from vendors (legal challenges, permissive-license advocacy, pressure for narrow readings), but the commons projects and standards bodies maintain the broad reading despite opposition.
 *
 * PERSPECTIVAL GAP:
 *   The commons-project seat and the proprietary-vendor seat should compute drastically different types from this structural data. For the commons seat: this is genuine coordination (pull improvements back, solve commons-enclosure problem) with distributed benefit — they should compute as rope or coordinate-dominant. For the proprietary-vendor seat: this is forced-choice extraction (lose strategic optionality, bear compliance cost, face legal risk) — they should compute as tangled-rope or snare. The engine's per-seat classification will capture this gap precisely: the same constraint produces different types when viewed from the beneficiary vs. payer seat, because directionality (d) is structurally asymmetric. The commentary should not predict the computed type; rather, it explains why the structural asymmetry exists and what causes the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Commons projects occupy the beneficiary seat (d near 0.0): they benefit from the broad interpretation without bearing direct compliance cost. They have high exit optionality — they can adopt permissive licenses if strategic conditions change. Downstream users also occupy a beneficiary seat (d near 0.2): they benefit from guaranteed source access, but they face a constrained exit (GPL code is popular; avoiding it means abandoning valuable tools). Proprietary vendors are the payers (d near 0.85): they bear compliance costs (redesign, licensing negotiation, legal risk) and face constrained exit (GPL libraries dominate certain domains; redesign or relicense are costly). Proprietary libraries linked to GPL code occupy a payer seat (d near 0.80): they lose proprietary control if they become part of a derived work. The standards-body seat is analytical; it does not collect extraction value. The excluded proprietary ecosystem sits outside the derivation chain: it experiences pressure but does not have a seat at the table where the GPL meaning is authoritatively set.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has not experienced mandate obsolescence. The founding problem (preventing GPL code enclosure in proprietary products) remains live and is actively contested; the standards bodies continue to enforce the broad reading despite ongoing legal and advocacy pressure to narrow it. The extracted value (compliance cost, strategic limitation on vendors) is not diffusely paid but concentrated on vendors who integrate GPL code. The constraint persists because the commons projects and standards bodies maintain it, not because of institutional inertia. Therefore, mandatrophy is not triggered: the coordinate function and extraction function remain coupled, and the beneficiaries continue to actively defend the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'Does the GPL derivative-work clause''s text, authorial intent, and precedent establish a bright-line test that unambiguously includes dynamic linking, or is the boundary fundamentally contested and context-dependent?',
    'Close textual analysis of GPL language, FSF guidance documents, and court precedent (Jacobsen v. Katzer, Gpl.net cases, EU directives on software copyright). Empirical examination of what linking scenarios courts have actually treated as derivative work vs. aggregation.',
    'A bright-line rule favoring the broad reading would strengthen mandatrophy resolution and clarify vendor compliance; a genuinely ambiguous boundary would leave the constraint partially unenforceable and vulnerable to narrow-reading pressure from courts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, conceptual, 'Whether GPL derivative-work scope is textually determinate or fundamentally contestable').

omega_variable(
    market_substitution_and_enclosure_risk,
    'If the broad reading is narrowed or overturned, does the GPL lose its primary commons-protection mechanism, or can other mechanisms (permissive-license adoption, community governance, trademark) preserve commons continuity without copyleft enforcement?',
    'Scenario analysis from jurisdictions that have narrowed GPL enforcement (e.g., Germany''s rulings favoring narrow linking interpretations). Monitor market dynamics: do proprietary integrations of GPL code increase significantly when broad-reading enforcement weakens? Do commons projects remain viable and resilient without copyleft reach?',
    'If enclosure risk is real and market substitution occurs, the founding problem remains live and mandatrophy is not triggered; if commons projects remain robust under permissive governance, the broad reading''s enforcement importance diminishes and the constraint risks becoming piton-class (maintained theatrically but not functionally necessary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_substitution_and_enclosure_risk, empirical, 'Whether narrowing the broad reading would cause commons enclosure or whether alternative mechanisms can protect the commons').

omega_variable(
    reading_coexistence_and_jurisdiction_arbitrage,
    'Do different readings coexist as genuinely live positions in different jurisdictions (US courts broad, EU courts narrow), enabling vendors to forum-shop and select favorable interpretations, thereby undermining the broad reading''s global enforceability?',
    'Map out case law and precedent by jurisdiction. Observe whether vendors strategically locate development and distribution in narrow-reading jurisdictions while selling globally. Track attempts to harmonize international copyright law standards (WIPO, WTO negotiations).',
    'If jurisdiction arbitrage is systematic and effective, the broad reading becomes locally strong but globally leaky; the constraint would compute as moderate extraction at global scope due to enforcement gaps, not high extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_and_jurisdiction_arbitrage, empirical, 'Whether reading divergence across jurisdictions enables enforcement evasion via jurisdiction arbitrage').

omega_variable(
    axiom_clash_in_reading_incompatibility,
    'Does the broad reading''s foundational axiom (derivative work is expansively defined to protect commons) logically foreclose the narrow reading''s axiom (linking is aggregation unless source modification occurs), or are both axioms defensible within different legal frameworks?',
    'Formal analysis of the axiom claims in the context of copyright law theory, GPL intent, and software architecture. Determine whether one axiom logically entails the negation of the other, or whether both can hold consistently in different doctrinal frameworks.',
    'If the axioms foreclose each other, the sibling readings should be marked forecloses; if both are consistent in different frameworks, they coexist. This affects the engine''s computation of reading conflict and the likelihood of long-term jurisdictional resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_clash_in_reading_incompatibility, conceptual, 'Whether the broad reading''s axioms logically foreclose the sibling readings'' axioms or whether both can coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_broad_deriv_tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl_broad_deriv_tr_t5, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gpl_broad_deriv_tr_t10, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(gpl_broad_deriv_tr_t15, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(gpl_broad_deriv_tr_t20, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gpl_broad_deriv_tr_t25, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(gpl_broad_deriv_tr_t35, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl_broad_deriv_be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gpl_broad_deriv_be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(gpl_broad_deriv_be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(gpl_broad_deriv_be_t15, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(gpl_broad_deriv_be_t20, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(gpl_broad_deriv_be_t25, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(gpl_broad_deriv_be_t35, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl_broad_deriv_su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpl_broad_deriv_su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(gpl_broad_deriv_su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(gpl_broad_deriv_su_t15, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(gpl_broad_deriv_su_t20, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(gpl_broad_deriv_su_t25, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(gpl_broad_deriv_su_t35, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.18).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, software_license_compliance_enforcement).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendor_enclosure_defense).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested gpl_derivative_work_trigger kernel. It instantiates the broad-copyleft interpretation where dynamic linking triggers derivative-work status. The narrow_linking_permissive_reading and interface_boundary_reading are sibling constraints in the same kernel family. All three readings share the same founding problem (GPL commons protection vs. vendor integration flexibility) but instantiate different ε values and victim/beneficiary structures due to their different interpretations of what constitutes derivative work. The three stories should be linked via network.affects_constraints as a constraint family, with each story documenting its reading relations and axioms in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
