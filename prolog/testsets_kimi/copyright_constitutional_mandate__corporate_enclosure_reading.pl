% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Corporate Enclosure Reading of the Copyright Clause
 *   domain: legal/political_economic
 *
 * SUMMARY:
 *   This constraint instantiates the corporate_enclosure_reading of the
 *   copyright_constitutional_mandate kernel. The kernel is the U.S.
 *   Constitution's Copyright Clause ('limited Times' / 'Authors'). This
 *   reading interprets 'limited Times' as permitting maximal extension short
 *   of explicit perpetuity, and treats copyright as a full property right
 *   requiring maximal protection rather than a temporary statutory monopoly.
 *   The reading is materially advanced by corporate incumbents (Disney, RIAA,
 *   MPAA) through legislative capture, producing high extractiveness through
 *   term extension, criminalized circumvention (DMCA Â§1201), and fair-use
 *   restriction. Derivative creators, educators, and archivists bear the
 *   costs. Sibling readings include the public_scaffold_reading (temporary
 *   monopoly as means to public-domain end) and the
 *   judicial_ambiguity_reading (zone of legislative discretion with
 *   rational-basis deference). This constraint is structurally distinct from
 *   those siblings: its epsilon is high, its beneficiary set is concentrated
 *   corporate, and its victim set is diffuse cultural-sector actors.
 *
 * KEY AGENTS:
 *   - corporate_incumbents: Primary beneficiary/agenda-setter (institutional/arbitrage) â captures legislative process to extend monopoly rents
 *   - derivative_creators: Primary target (moderate/constrained) â chilled by statutory damages and licensing friction
 *   - educators: Secondary target (organized/constrained) â bears compliance costs and permission-seeking burdens
 *   - archivists: Secondary target (organized/constrained) â blocked by anti-circumvention from preserving digital culture
 *   - public_domain_advocates: Excluded voice (moderate/constrained) â structurally outspent in legislative debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.84).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.79).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Corporate Enclosure Reading of the Copyright Clause").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "legal/political_economic").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '02192a78-4884-4939-a963-e12f600efd10').
narrative_ontology:cs_kernel_codification('02192a78-4884-4939-a963-e12f600efd10', fixed_text).
narrative_ontology:cs_authority_grounding('02192a78-4884-4939-a963-e12f600efd10', extraction).
narrative_ontology:cs_interpretation_layer_present('02192a78-4884-4939-a963-e12f600efd10').
narrative_ontology:cs_reading_relation('02192a78-4884-4939-a963-e12f600efd10', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('02192a78-4884-4939-a963-e12f600efd10', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('02192a78-4884-4939-a963-e12f600efd10', foundational, maximal_property_protection_mandate).
narrative_ontology:cs_axiom_status(maximal_property_protection_mandate, holdable).
narrative_ontology:cs_axiom_grounding('02192a78-4884-4939-a963-e12f600efd10', maximal_property_protection_mandate, deontological).
narrative_ontology:cs_axiom('02192a78-4884-4939-a963-e12f600efd10', foundational, limited_times_means_non_perpetuity_only).
narrative_ontology:cs_axiom_status(limited_times_means_non_perpetuity_only, holdable).
narrative_ontology:cs_axiom_grounding('02192a78-4884-4939-a963-e12f600efd10', limited_times_means_non_perpetuity_only, conventional).
narrative_ontology:cs_reference_frame('02192a78-4884-4939-a963-e12f600efd10', maximal_property_enclosure).
narrative_ontology:cs_drift_state('02192a78-4884-4939-a963-e12f600efd10', post_term_extension_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('02192a78-4884-4939-a963-e12f600efd10', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major content conglomerates and their trade associations (e.g., Disney, RIAA, MPAA) that fund legislative lobbying to extend copyright terms, criminalize circumvention, and narrow fair use. They treat back-catalog works as perpetual revenue assets and set the statutory agenda through campaign finance, revolving doors, and litigation infrastructure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, beneficiary).

% Remix artists, fan creators, documentary filmmakers, and transformative writers who rely on twentieth-century cultural source material. They face statutory damages, licensing friction, and chilling effects that prevent or penalize adaptation, even where fair use might theoretically apply.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, national).

% Teachers, professors, and educational institutions who excerpt, display, and discuss copyrighted works in pedagogy. They bear licensing fees, permission delays, and administrative risk aversion driven by narrow fair-use guidelines and aggressive rights-holder enforcement.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    organized, biographical, constrained, national).

% Cultural preservationists and librarians who are prohibited from circumventing access controls to preserve obsolete digital formats. DMCA section 1201 blocks preservation and format migration even when works are out of commercial distribution and the copyright owner cannot be located.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    organized, generational, constrained, national).

% Legal scholars, civil society organizations, and open-access advocates who argue for the original constitutional design of limited terms and a robust public domain. They are systematically outspent in legislative drafting and excluded from statutory negotiation rooms dominated by industry counsel.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates, excluded,
    moderate, civilizational, constrained, national).

% Academic researchers and public-interest lawyers who document the historical drift from limited monopoly to corporate property enclosure. They publish critical histories and file amicus briefs but lack the institutional authority to alter statutory text.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legal_scholars, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to solve the underproduction of expressive works by treating creative output as alienable property whose exclusive control generates market incentives for production and distribution.
% TRANSFER_FUNCTION: Moves monopoly rents and derivative-control rights from future creators, educators, and preservationists to incumbent corporate rightsholders; moves legislative capacity from public-interest balancing to term-extension and enforcement expansion.
% ABSENT_VOICES: Future creators who would remix or adapt twentieth-century works locked in extended terms; the general public as intended beneficiary of the public domain; economists who would set term length at the welfare-maximizing point. Excluded because legislative drafting is captured by incumbent lobbying and because the property framing marginalizes utilitarian cost-benefit analysis.
% DISAPPEARANCE_RATIONALE: If the maximal-protection reading vanished overnight, statutory damages would shrink, anti-circumvention penalties would lift, fair use would expand, and the public domain would rapidly accumulate twentieth-century works. The creative economy would restructure around open adaptation, educational access would widen, and incumbent back-catalog revenue would collapse.
% FOUNDING_PROBLEM: The Framers sought to promote the progress of science and useful arts by securing to authors, for limited times, the exclusive right to their writingsâaddressing market failure in creative production where non-excludability discourages investment.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians such as Boyle and Party, publishing from outside the content industry, attest that the current regime serves rent extraction on dead and corporate-authored catalogs rather than marginal creation; no independent empirical study corroborates life-plus-seventy as the welfare-maximizing term, while industry-funded studies are self-asserted by the beneficiary set.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84) because the constraint transfers vast back-catalog value to incumbents while suppressing derivative creation that would occur under shorter terms. Suppression (0.79) is high due to criminalized circumvention, statutory damages, and treaty lock-in via TRIPS/Berne. Theater ratio (0.45) reflects that term-extension debates are framed as creator incentives but functionally protect corporate assets (Mickey Mouse curve). Accessibility collapse (0.68) is substantial: fair use and the public domain are theoretically available but have collapsed in practice due to legal chill, automated enforcement, and orphan works. Resistance (0.42) is moderate: there is sustained scholarly and civil-society opposition, but it is systematically outspent and loses legislative battles.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (corporate incumbents) experiences the constraint as legitimate property defense and necessary market incentive. The payer seats (derivative creators, educators, archivists) experience it as an artificial barrier to cultural participation and preservation. The engine will compute divergent per-seat types from this structural asymmetry: the incumbent seat may compute as rope/tangled_rope (defending 'its' property) while the creator seat computes as snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are declared beneficiaries with global scope and arbitrage-grade exit (can forum-shop, shape legislation, vertically integrate). Their derived directionality sits near the full-beneficiary end (d â 0.1). Derivative creators, educators, and archivists are declared victims with constrained exit and national scope; their directionality sits near the full-target end (d â 0.85â0.9). The engine will therefore compute high effective extraction for the victim seats and low/negative extraction for the beneficiary seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by distinguishing the corporate_enclosure_reading from the public_scaffold_reading. A naive analysis might classify copyright generically as scaffold (temporary means to public end). By decomposing the kernel into readings, we isolate the corporate_enclosure reading's dead founding problem (the Framers' limited-term incentive structure has been replaced by perpetual-adjacent rent extraction) and its concentrated beneficiary set. The mandatrophy flag is triggered because the founding problem is dead while the arrangement persists as world_rearrangesâa signature of capture rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corporate_enclosure_kernel_legitimacy,
    'Is the corporate_enclosure_reading a plausible interpretation of the Copyright Clause kernel, or an extraction narrative grafted onto constitutional text?',
    'Originalist and purposivist doctrinal review of ''limited Times'' and ''Authors''; empirical analysis of legislative timing showing term extensions correlate with imminent public-domain entry of valuable corporate assets rather than welfare-maximizing policy.',
    'If the reading is extra-constitutional graft, its authority_grounding shifts from lineage to extraction, reclassifying the commitment-system pattern and triggering false-summit evaluation of any mountain-like claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_enclosure_kernel_legitimacy, conceptual, 'Whether the corporate enclosure reading derives from the constitutional kernel or captures it.').

omega_variable(
    sibling_public_scaffold_alternative,
    'What would change structurally if the public_scaffold_reading replaced the corporate_enclosure_reading as the operative interpretation?',
    'Cross-jurisdictional comparison of regimes with shorter terms or robust fair use; measurement of derivative output, educational access costs, and preservation rates.',
    'Would invert beneficiary/victim structure, lower epsilon substantially, and potentially reclassify the constraint as scaffold or rope with corporate incumbents as payers losing back-catalog rents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_public_scaffold_alternative, conceptual, 'Structural delta under public-scaffold sibling reading.').

omega_variable(
    marginal_incentive_empirical_status,
    'Does term extension beyond life-plus-fifty generate marginal creative works that would not have been produced under shorter terms?',
    'Econometric analysis of creative output across jurisdictions with varying term lengths, controlling for GDP and cultural industry size; direct survey of creators on production motivation.',
    'If null or negative, the coordination story is cover and the snare classification is reinforced; if strongly positive, the constraint might shift toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_incentive_empirical_status, empirical, 'Empirical status of the incentive justification for term extension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corp_enc_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corp_enc_tr_t8, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(corp_enc_tr_t16, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(corp_enc_tr_t24, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(corp_enc_tr_t32, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(corp_enc_tr_t40, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(corp_enc_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(corp_enc_be_t8, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(corp_enc_be_t16, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(corp_enc_be_t24, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(corp_enc_be_t32, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 32, 0.79).
narrative_ontology:measurement(corp_enc_be_t40, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 40, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(corp_enc_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(corp_enc_su_t8, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(corp_enc_su_t16, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(corp_enc_su_t24, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(corp_enc_su_t32, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(corp_enc_su_t40, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the corporate_enclosure_reading of the copyright_constitutional_mandate kernel, decomposed per the epsilon-invariance principle because the three sibling readings produce structurally distinct epsilon values, beneficiary sets, and enforcement patterns. The public_scaffold_reading carries low epsilon and a public-domain beneficiary set; the judicial_ambiguity_reading carries moderate epsilon and diffused institutional authority; this reading carries high epsilon with concentrated corporate beneficiaries and diffuse cultural-sector victims. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
