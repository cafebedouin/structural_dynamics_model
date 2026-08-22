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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   domain: legal/intellectual_property/open_source
 *
 * SUMMARY:
 *   The GPL is a contested kernel, grounded in a fixed text (the license
 *   itself) that multiple parties read differently. The BROAD COPYLEFT
 *   READING interprets linking—including dynamic linking—as creating a
 *   derivative work under copyright law, thereby triggering GPL's
 *   source-disclosure obligation for all linked code. This reading vindicates
 *   a copyleft ideology: it pulls proprietary code into the commons and
 *   prevents vendors from extracting value without contributing back. The
 *   sibling readings (narrow_linking_permissive_reading,
 *   interface_boundary_reading) interpret the same GPL text as applying only
 *   to modifications, not linking, or as recognizing interface boundaries as
 *   non-derivative aggregation. This constraint story models ONLY the broad
 *   copyleft reading; the sibling readings are separate constraint stories
 *   with different ε, different beneficiaries/victims, and different
 *   classifications. The kernel is the GPL license text; the reading-specific
 *   extraction (ε=0.68) reflects the broad interpretation's enforcement cost
 *   on proprietary vendors.
 *
 * KEY AGENTS:
 *   - open_source_commons_users: benefit from copyleft enforcement; gain source access to linked proprietary code
 *   - GPL_community: sets and advocates the broad copyleft reading; maintains GPL projects and enforces the interpretation
 *   - proprietary_software_vendors: face disclosure obligations; must refactor, release code, or avoid GPL libraries
 *   - closed_source_integrators: caught between GPL compliance costs and ecosystem benefits
 *   - courts_and_regulators: adjudicate GPL interpretation disputes; their rulings validate or constrain the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Derivative Work Trigger (Broad Copyleft Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "legal/intellectual_property/open_source").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '11ab4f8b-d91f-43ae-937e-ba9f3970b35d').
narrative_ontology:cs_kernel_codification('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', fixed_text).
narrative_ontology:cs_authority_grounding('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', lineage).
narrative_ontology:cs_interpretation_layer_present('11ab4f8b-d91f-43ae-937e-ba9f3970b35d').
narrative_ontology:cs_reading_relation('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', foundational, linking_constitutes_derivation).
narrative_ontology:cs_axiom_status(linking_constitutes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', linking_constitutes_derivation, deontological).
narrative_ontology:cs_axiom('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', foundational, copyleft_maximalism_prevents_capture).
narrative_ontology:cs_axiom_status(copyleft_maximalism_prevents_capture, holdable).
narrative_ontology:cs_axiom_grounding('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', copyleft_maximalism_prevents_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', copyleft_commons_integrity).
narrative_ontology:cs_drift_state('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', contemporary_proprietary_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11ab4f8b-d91f-43ae-937e-ba9f3970b35d', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_community).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_integrators).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_virtue).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, software_freedom_maximalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to source code of linked proprietary software under GPL terms. They benefit from the copyleft mechanism which, under this reading, ensures downstream derivatives remain available as source. They can fork, modify, and redistribute; the constraint funds their freedom by pulling dependent code into the commons.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_users, beneficiary,
    organized, generational, mobile, global).

% Gains a unified commons of software where linking is treated as creating a derivative work, preventing proprietary capture of the ecosystem. The reading vindicates their normative claim that broad copyleft prevents 'embrace, extend, extinguish' strategies. They maintain and advocate for this interpretation through legal theory, licensing, and litigation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_community, beneficiary,
    organized, generational, mobile, global).

% Face disclosure obligations if they link to GPL code, even dynamically. They must either: release proprietary code under GPL (loses competitive advantage), isolate functionality in a separate process (increases complexity and cost), or avoid GPL libraries entirely (constrains functionality and locks them out of the free-software ecosystem). Exit options are constrained by the reading's broad definition of derivation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Build products atop GPL libraries without originally intending to disclose source. Under the broad reading, even aggregation through linking triggers derivative-work status and forces disclosure. They face compliance costs (legal review, refactoring to isolate GPL code) or must release proprietary modules. Their exit—using non-GPL alternatives—is available but costly (ecosystem value, feature parity).
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_integrators, payer,
    moderate, biographical, constrained, global).

% Write and maintain the GPL license text and interpret its terms. Under the broad reading they (e.g., FSF) declare that linking—even dynamic linking—constitutes a derivative work, triggering copyleft obligations. They enforce this interpretation through license disputes, legal opinions, and coordination with GPL projects. They set the boundaries of what counts as derivation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_license_authors, agenda_setter,
    institutional, generational, analytical, global).

% Would advocate for narrower derivation boundaries (interface-based, not linking-based) to enable seamless integration without disclosure obligations. They are not at the table in GPL drafting or enforcement; their perspective—that overly broad derivative definitions impede beneficial interoperability—is structurally absent from the reading's production.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_integrators_seeking_interop, excluded,
    moderate, biographical, trapped, global).

% Hold patents on algorithms or techniques embedded in proprietary software. The broad copyleft reading forces their code into GPL, which GPL's patent clause (GPLv2 section 7, GPLv3) weakens their enforcement hand—GPL distributes patent grants to all downstream users. They are structurally absent: no seat at the license-boundary discussion, their interests not represented.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, patent_holders_in_proprietary_stack, excluded,
    powerful, biographical, identity_locked, global).

% Analyze GPL from a narrower constructionist view: linking does not inherently make a derivative work; only modifications to GPL code itself do. They publish opposing legal analysis, file amici in litigation, and advise clients on risk. They see the broad reading as over-reaching and incompatible with copyright law's historical definition of derivation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, legal_scholars_narrow_reading, observer,
    analytical, generational, analytical, global).

% Adjudicate disputes over GPL interpretation. They hear arguments from all sides and render verdicts on whether specific linking patterns create derivative works under copyright law. Their rulings either validate or constrain the broad copyleft reading's enforceability.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_community).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified commons of software where proprietary and open-source code cannot be seamlessly integrated without source disclosure, forcing all downstream derivatives to remain available as source. This coordinates the entire ecosystem around a single copyleft principle: anything built atop GPL code must remain free.
% TRANSFER_FUNCTION: Transfers proprietary code from vendors' exclusive control into a commons accessible to all downstream users under GPL. The transfer is coercive: vendors who link to GPL code must either release source code or refactor their architecture. The constraint moves control and knowledge from vendor to commons.
% ABSENT_VOICES: Software integrators seeking narrow API-based interoperability and patent-holding firms whose IP is pulled into GPL are structurally excluded. They would argue that copyleft should stop at interface boundaries, not extend through linking; that overly broad derivation definitions impede beneficial integration. They have no seat in GPL license drafting or enforcement structures.
% DISAPPEARANCE_RATIONALE: If the broad copyleft reading vanished (replaced by a narrower interpretation), proprietary vendors would freely link to GPL libraries without disclosure, the commons would shrink (vendors extract value without contributing back), and the ecosystem would reorganize around proprietary-driven integration patterns. The constraint's disappearance would collapse the GPL commons' enforceability.
% FOUNDING_PROBLEM: Early free software faced 'embrace, extend, extinguish' tactics: vendors would use free software as a foundation, add proprietary layers, and lock downstream users into the proprietary product while extracting value from the free-software work. The GPL was designed to prevent this by ensuring linked code remains free.
% FOUNDING_PROBLEM_CORROBORATION: The GPL community and free-software advocates attest the founding problem remains live: vendors continue seeking to integrate GPL code without disclosure. Proprietary vendors and narrower legal scholars attest the problem is overstated or solved by other means (separate processes, modular design); they cite cases where courts have not definitively validated the broad reading and empirical evidence of successful closed-source/open-source integration without GPL violations. The dispute is live in litigation and policy discourse; no neutral external authority has definitively resolved it.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-to-high (0.68) because the constraint imposes substantial compliance burden on proprietary vendors—they must either disclose proprietary code, incur refactoring costs, or exit the ecosystem. However, it is not maximal (not 0.85+) because alternatives exist: (1) vendors can isolate GPL code in separate processes (eliminates the linking chain), (2) use permissive licenses instead, (3) litigate the derivation boundary. Suppression is moderate (0.42) because the constraint relies on legal interpretation, not technical mechanism—it is suppressive in that vendors face litigation risk, but enforcement is not automatic or physical. Theater is low-to-moderate (0.28): there is genuine copyleft function (ensuring commons remains free), but a portion of enforcement activity is spent on boundary disputes (what counts as linking, what counts as derivation) rather than implementing the core coordination. Accessibility collapse is high (0.72): once the broad copyleft reading is understood, alternatives (narrow reading, interface-boundary reading) collapse as options within a single jurisdictional framework—vendors in GPL-enforcing territories face binary choice (comply or avoid), with the middle ground of 'permissive linking' legally foreclosed if the reading holds. Resistance is high (0.71): proprietary vendors actively resist the reading through litigation, licensing alternatives (Apache, MIT), and lobbying for narrower interpretations. The measurement series show slight upward drift in extractiveness and theater (as the reading becomes institutionalized through case law and FSF enforcement) and stable suppression (the legal mechanism does not intensify; enforcement remains litigation-based).
 *
 * PERSPECTIVAL GAP:
 *   From the GPL community's seat, the broad copyleft reading is ROPE: genuine coordination (commons preservation), with beneficiaries (downstream users gain source access). From proprietary vendors' seat, it is SNARE: extraction (forced disclosure or refactoring cost) with suppression (litigation risk, legal uncertainty). The engine computes this seat-specific divergence from the structural data: beneficiaries (GPL community, open-source users) derive low directionality; victims (vendors) derive high directionality. The divergence is intentional and is exactly what the framework measures—a rope from one seat, a snare from another, depending on power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The GPL community and open-source users are structural beneficiaries: they benefit from the reading's enforceability—every linked proprietary module comes into the commons. Proprietary vendors and closed-source integrators are structural targets: the reading forces them to either disclose or refactor. The reading's enforcement depends on legal authority (courts, FSF as copyright-holder representatives); they have high institutional power. The relationship is asymmetric: the GPL community sets the interpretation; the vendors must navigate it. Directionality for beneficiaries (GPL community) is near 0.1–0.2 (full beneficiary); for victims (vendors) is near 0.8–0.9 (full target).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (embrace, extend, extinguish) is CONTESTED: the GPL community attests it is live and requires the broad copyleft reading to prevent. Proprietary vendors and narrower legal scholars attest the problem is overstated or solved by other mechanisms (separate processes, modular architecture, licensing diversification). The disappearance verdict is WORLD_REARRANGES: if the reading disappeared, proprietary vendors would freely integrate GPL code without disclosure, and the commons would shrink. This mismatch (live + world_rearranges) is structurally coherent—the reading persists because it defends against the founding problem; if the reading failed, the problem would recur. No mandatrophy signal arises from the five-questions interview. However, omega variables document the irreducible uncertainty: is the founding problem really live, or has the software industry evolved past the embrace-extend-extinguish risk through modularization, API standards, and licensing maturity?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivation_boundary_contested,
    'Is linking (including dynamic linking) inherently a form of derivation under copyright law, or is the broad copyleft reading an over-extension of copyright doctrine to software integration?',
    'Authoritative court ruling on GPL enforceability against linked proprietary code, or legislative clarification of copyright doctrine applied to software. Alternatively, empirical analysis of GPL litigation outcomes and settlements.',
    'If courts validate broad linking-as-derivation, the reading becomes doctrine. If courts restrict derivation to modifications (narrow reading), copyleft loses enforceability over linking. If courts recognize interface boundaries as non-derivative aggregation (interface boundary reading), the reading is superseded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivation_boundary_contested, empirical, 'Contested legal boundary between linking (aggregation) and derivation.').

omega_variable(
    embrace_extend_extinguish_vitality,
    'Is the embrace-extend-extinguish threat still live in software markets, or has modularization, API standards, and licensing diversity made the threat historical?',
    'Empirical study of vendor integration patterns post-2010: do closed-source vendors still seek to integrate open-source code and lock downstream users? Or have they pivoted to service models, proprietary extensions, or separate-process architectures that avoid direct linking?',
    'If the threat is no longer live, the founding problem has been solved by market evolution, and the broad copyleft reading may be persisting as theater (enforcing a defunct boundary) rather than genuine coordination. If the threat is live, the reading''s enforcement remains functionally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embrace_extend_extinguish_vitality, empirical, 'Whether the GPL copyleft reading''s founding problem persists in contemporary markets.').

omega_variable(
    reading_foreclusion_coherence,
    'Does the broad copyleft reading logically foreclose the interface-boundary reading, or can both readings coexist in different enforcement regimes (jurisdictions, licensing variants)?',
    'Textual analysis of GPL language: does ''derivative work'' linguistically foreclose ''aggregation across clean interfaces''? Or can both interpretations coexist by applying different weight to API boundaries? Alternatively, natural experiment: jurisdictions with explicit GPL fork narrowing (e.g., AGPL''s network trigger vs. GPLv2''s linking trigger) reveal whether the readings genuinely exclude or merely differ in scope.',
    'If the readings logically foreclose one another, the relationship in cs_structure.reading_relations should be ''forecloses''. If they can coexist, the relationship is ''coexists_with''. If one reading creates structural pressure on the other (e.g., broad copyleft makes interface-boundary narrower by raising vendors'' compliance cost), the relationship is ''influences''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclusion_coherence, conceptual, 'Logical relationship between broad copyleft reading and interface-boundary reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the constraint''s suppression of proprietary integration mechanically structural (litigation risk, legal uncertainty) or partly internalized (vendors internalize fear of copyleft and self-censor even when legal risk is low)?',
    'Post-exit suppression trajectory: if vendors who successfully defend against copyleft claims still refrain from linking (ongoing self-censorship despite legal victory), suppression has internalized. If they resume linking, suppression is primarily structural (mechanical legal barrier).',
    'If internalized, the measured suppression (0.42) understates the constraint''s effective suppression on vendor behavior—they carry the suppression with them even when legal risk recedes. If structural, the measured suppression accurately reflects the legal mechanism''s force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized mechanisms of suppression on proprietary linking behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t22, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 22, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t22, projected).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t22, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 22, 0.67).
narrative_ontology:measurement_basis(gpl__be_t22, projected).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(gpl__be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t22, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 22, 0.41).
narrative_ontology:measurement_basis(gpl__su_t22, projected).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(gpl__su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% The GPL derivative-work boundary is a single contested kernel (gpl_derivative_work_trigger) instantiated by three distinct readings. Each reading emits a different constraint with distinct ε, beneficiaries/victims, and classification. The broad_copyleft_reading (this story) pulls linked proprietary code into GPL commons; the narrow_linking_permissive_reading restricts obligations to modifications; the interface_boundary_reading recognizes clean API aggregation as non-derivative. The three stories form a constraint family linked by network.affects_constraints. The broad reading influences both siblings by raising the enforcement cost of proprietary integration, creating structural pressure toward narrower interpretations. Courts' rulings on this kernel will reclassify all three stories simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
