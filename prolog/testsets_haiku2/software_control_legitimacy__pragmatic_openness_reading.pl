% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Software Control as Pragmatic Methodology Choice (Coexistence Reading)
 *   domain: software_engineering/intellectual_property/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the pragmatic-openness reading of the
 *   software-control legitimacy kernel. It asserts that software control —
 *   the choice between open source (transparent, collaborative,
 *   community-governed) and proprietary (closed, privately-governed,
 *   market-tested) models — is fundamentally a development methodology
 *   choice, not a question of categorical rights or moral imperatives. Both
 *   models produce valuable software; both are legitimate; developers and
 *   users benefit from ecosystem diversity. The reading explicitly rejects
 *   the sibling readings' claims: it denies the freedom imperative's
 *   assertion that proprietary software is categorically unethical, denies
 *   the property-rights reading's claim that the regime should prioritize
 *   capital recovery above all, and denies the commons reading's claim that
 *   software is shared infrastructure requiring negotiated collective
 *   governance above market/community choice. Yet it coexists with all three
 *   readings in actual software practice — different developers,
 *   organizations, and users adopt different readings simultaneously. The
 *   constraint is the very acceptance of coexistence and the empirical
 *   examination of tradeoffs.
 *
 * KEY AGENTS:
 *   - Software developers: choose control models based on project needs, community, and business model. Both choices yield career and technical legitimacy.
 *   - Software users: benefit from ecosystem diversity; can switch tools, fork open source, or negotiate proprietary licenses based on needs. No single control model serves all use cases.
 *   - Open source communities: demonstrate collaborative development works at scale; their success is empirical, not stipulated by the reading.
 *   - Proprietary vendors: demonstrate capital-backed investment in software features users value; their success is also empirical.
 *   - Freedom advocates (excluded): hold that software is a human-rights issue and proprietary control is illegitimate; their frame is valid but not inside this reading.
 *   - Commons advocates (excluded): hold that software is shared infrastructure; their frame would reorient the entire constraint.
 *   - IP regime (observer): Copyright and patent law are the substrate; the reading takes them as background, not as the primary question.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.28).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Software Control as Pragmatic Methodology Choice (Coexistence Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/intellectual_property/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'fe146ab9-ecdb-449b-8949-e370998e0893').
narrative_ontology:cs_kernel_codification('fe146ab9-ecdb-449b-8949-e370998e0893', distributed).
narrative_ontology:cs_authority_grounding('fe146ab9-ecdb-449b-8949-e370998e0893', distributed).
narrative_ontology:cs_reading_relation('fe146ab9-ecdb-449b-8949-e370998e0893', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe146ab9-ecdb-449b-8949-e370998e0893', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe146ab9-ecdb-449b-8949-e370998e0893', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('fe146ab9-ecdb-449b-8949-e370998e0893', foundational, software_control_is_methodology_choice).
narrative_ontology:cs_axiom_status(software_control_is_methodology_choice, holdable).
narrative_ontology:cs_axiom_grounding('fe146ab9-ecdb-449b-8949-e370998e0893', software_control_is_methodology_choice, instrumental).
narrative_ontology:cs_axiom('fe146ab9-ecdb-449b-8949-e370998e0893', foundational, both_models_produce_valuable_software).
narrative_ontology:cs_axiom_status(both_models_produce_valuable_software, holdable).
narrative_ontology:cs_axiom_grounding('fe146ab9-ecdb-449b-8949-e370998e0893', both_models_produce_valuable_software, empirically_contingent).
narrative_ontology:cs_reference_frame('fe146ab9-ecdb-449b-8949-e370998e0893', coexistence_of_methodologies).
narrative_ontology:cs_drift_state('fe146ab9-ecdb-449b-8949-e370998e0893', contemporary_software_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fe146ab9-ecdb-449b-8949-e370998e0893', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, collaborative_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can choose development methodology based on project goals, community, and business model. Both choices yield career and technical legitimacy. Open source offers code review, reputation, community contribution, and independence; proprietary offers investment backing, commercial revenue, and focused design goals. The choice is genuinely available to most developers.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_developers, beneficiary,
    organized, generational, mobile, global).

% Benefit from ecosystem diversity. Critical infrastructure and scientific tools predominantly open source (peer review, transparency, maintenance breadth); consumer products and enterprise software predominantly proprietary (integrated experience, long-term support contracts, rapid feature iteration on funded priorities). Users can switch tools, request features, or fork open source. No single control model serves all use cases.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    moderate, biographical, constrained, global).

% Open source projects depend on decentralized contribution, distributed peer review, and voluntary labor. The pragmatic reading validates this as a legitimate development model that produces high-quality infrastructure (Linux, Apache, GNU tools, scientific libraries). Success is empirically demonstrated, not stipulated.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, collaborative_projects, beneficiary,
    organized, generational, mobile, global).

% Proprietary control enables capital recovery through licensing, supports R&D investment in features and polish that market demands (user interface, ecosystem consistency, long-term support contracts). The pragmatic reading validates this as a legitimate development model that produces high-quality consumer and enterprise software. Success is market-tested.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors, beneficiary,
    powerful, generational, mobile, global).

% Hold that software is fundamentally a commons and proprietary control denies users ethical agency and computing autonomy. They are excluded from the pragmatic reading's framework because that reading treats software control as a pragmatic choice rather than a rights issue. Their objection is heard in parallel discourses but not inside the methodology frame.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, freedom_imperative_advocates, excluded,
    organized, generational, constrained, global).

% Argue software is a shared digital infrastructure requiring negotiated collective governance, not a binary open/proprietary choice where control rests with either volunteers or private vendors. They are excluded from the pragmatic reading because that reading treats both models as legitimate design choices rather than as partial solutions to an underlying commons problem.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, commons_governance_advocates, excluded,
    organized, generational, constrained, global).

% Argue software control should prioritize the creator's property rights and ability to monetize work. They are excluded from the pragmatic reading because that reading accepts both property-based (proprietary) and gift/commons-based (open source) as equally legitimate, not as a hierarchy where property claims are paramount.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, property_rights_advocates, excluded,
    organized, generational, constrained, global).

% Copyright and patent law are the legal substrate enabling both models to operate. The pragmatic reading takes the IP regime as background condition; it does not make the regime itself the primary question (unlike the property-rights reading, which argues the regime should be strong, or the freedom reading, which argues it should not apply to software).
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, intellectual_property_regime, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables software ecosystems to function with multiple legitimate control models coexisting: open source and proprietary software serve different use cases, and developers/users benefit from having both available and from the competitive pressure each model exerts on the other. Solves the real problem that no single control strategy optimizes for all software types.
% TRANSFER_FUNCTION: No net transfer: the pragmatic reading explicitly denies that one model extracts from the other. Open source projects receive volunteer labor and community resources; proprietary vendors receive market revenue. Both are voluntary transactions (developers choose to contribute or to license; users choose tools matching their needs). The constraint coordinates methodology pluralism rather than redistributing value.
% ABSENT_VOICES: Advocates for the freedom imperative (software as a human-rights issue, not a methodology choice) and commons-governance framers (software as shared infrastructure requiring negotiated collective stewardship) are not represented in this reading's frame. They would object that treating software control as a mere pragmatic choice erases the ethical and political dimensions of code authorship and digital autonomy. Their objections are valid within their frames but do not apply inside the pragmatic reading.
% DISAPPEARANCE_RATIONALE: If the pragmatic acceptance of both models evaporated and one reading (freedom imperative, property rights, or commons governance) became hegemonic, the software ecosystem would reorganize: if property rights won, open source would become a subordinate gift-economy alongside proprietary; if freedom won, proprietary software would be treated as illegitimate; if commons governance won, all software would be subject to negotiated collective licensing. The disappearance of pragmatic coexistence would rearrange the landscape because the constraint is the very acceptance of both as legitimate.
% FOUNDING_PROBLEM: Early software was predominantly proprietary (mainframes, system software), enforced by computer manufacturers' legal and technical control. By the 1980s–1990s, technologists (Stallman, Raymond, and open-source advocates) demonstrated that decentralized, transparent, collaborative development produced high-quality software (Linux, Apache, GCC) and could sustain complex infrastructure. Simultaneously, proprietary models continued producing valuable software (Windows, macOS, commercial databases). The founding problem: what is the right relationship between code visibility, control, and development quality? Both empirical and normative claims competed. The pragmatic reading solved this by accepting both as legitimate and examining methodology choice empirically.
% FOUNDING_PROBLEM_CORROBORATION: The pragmatic reading is corroborated by: (1) empirical evidence from outside benefiting parties — comparative software-quality studies find both models produce high-quality software, with quality correlating to specific features of each (open source excels at infrastructure, security review, and maintenance breadth; proprietary excels at integrated user experience, long-term support guarantees, and rapid feature iteration on funded priorities); (2) technology historians and systems researchers document the actual success of both models in different domains; (3) enterprise IT organizations and public-sector bodies testify that their software stacks depend on both; (4) developer testimony across the industry is that the choice between models is genuinely a methodological/business decision, not a moral imperative in either direction. No outside corroboration claims open source is categorically superior or that property rights in software are inherently illegitimate — both claims remain contested.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the reading denies that either model extracts from the other — both are voluntary (developers and users make deliberate choices matching their circumstances). Suppression is very low (0.15) because the constraint does not depend on coercing acceptance of either model; practitioners accept pragmatic coexistence because it is empirically true that both work. Theater is minimal (0.12) because the constraint's function is not to maintain a false appearance; methodology debates are transparent, and the tradeoffs are openly discussed. Accessibility collapse is moderate (0.35) because practitioners can and do switch models, but switching costs exist (rewrites, community transitions, license compatibility questions). Resistance is moderate (0.42) because sibling readings actively contest the pragmatic frame — freedom advocates push back against accepting proprietary legitimacy, property-rights advocates push back against treating property as merely instrumental, commons advocates argue the frame misses the shared-infrastructure problem. The measurement series are nearly flat because the pragmatic acceptance has been stable in practice since the late 1990s, with only gradual increases as proprietary vendors acknowledge open source's legitimacy and as open source projects mature into long-term infrastructure. No major regime change is measured.
 *
 * PERSPECTIVAL GAP:
 *   The freedom-imperative seat and the pragmatic seat compute very differently. From the freedom seat, proprietary software constrains user autonomy and denies agency — it should compute as snare or tangled-rope. From the pragmatic seat, proprietary software is a legitimate choice that enables different development models — it computes as rope or even neutral coordination. This divergence is not a measurement error; it is the core of the kernel contest. The engine should compute the freedom reading as high-extraction and the pragmatic reading as low-extraction, applied to the same constraint — software control itself. Each reading sees the same phenomenon (closed code) through different normative lenses and arrives at different ε values. This is exactly what the kernel-reading apparatus is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   All beneficiary stakeholders (developers, users, collaborative projects, proprietary vendors) sit near the symmetric or beneficiary end of directionality because the pragmatic reading frames both models as enabling rather than extractive. Developers get choice and tools; users get ecosystem diversity. Neither model systematically targets a powerless group; both function in competitive markets where switching is possible (even if costly). Excluded stakeholders (freedom advocates, commons advocates) do not appear in directionality calculations because their objection is to the reading frame itself, not to a particular model's implementation. Their absence is not an oversight — they are genuinely excluded by the pragmatic reading, which defines the problem as methodology choice rather than rights or commons governance.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic reading's founding problem remains live and empirically addressed: the question of what relationship holds between code visibility, control, and development quality has not been resolved by declaring one model superior. Instead, the pragmatic reading accepts that both models answer the question differently depending on context (infrastructure vs. consumer product, volunteer labor vs. capital-backed teams, transparency needs vs. competitive advantage). Mandatrophy — the state where a constraint's founding problem has died but the constraint persists — would occur if the pragmatic reading stopped examining empirical tradeoffs and simply asserted 'both models are equally good in all contexts' as dogma. That is not the current state; the reading remains grounded in method-comparative analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_quality_correlation,
    'Does software development methodology (open vs. proprietary) causally determine quality, or is quality determined by other factors (team skill, resources, maintenance culture) that happen to correlate with methodology choice?',
    'Comparative studies isolating methodology from confounds: controlled projects rewritten in both models, or matched-pair analyses where team size, domain, and maturity are held constant. Longitudinal tracking of identical codebases under different governance regimes.',
    'If methodology is causal, the pragmatic reading is justified by direct tradeoff analysis. If quality is orthogonal to methodology, both models are even more pragmatically neutral and the choice becomes purely about business/community fit, not technical outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_quality_correlation, empirical, 'Whether software quality causally depends on control methodology or is determined by independent factors.').

omega_variable(
    legitimacy_frame_choice,
    'Is software control a legitimate choice to be examined pragmatically, or is it fundamentally a question of rights (freedom reading), property (property-rights reading), or commons governance (commons reading)?',
    'No empirical resolution possible. This is a frame-choice question: the pragmatic reading''s entire coherence depends on denying the sibling readings'' claims that software control is fundamentally a *categorical* question rather than a *pragmatic* choice. Different parties adopt different frames; none is falsifiable by evidence.',
    'If the freedom/property/commons frames are correct, software control is not pragmatically neutral — one reading should dominate, and coexistence is unstable. If the pragmatic frame is correct, coexistence is sustainable and method-choice is real. The kernel contest is precisely this disagreement about frame-legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_frame_choice, conceptual, 'Whether software control is fundamentally a pragmatic methodology choice or a categorical normative question.').

omega_variable(
    hidden_extraction_in_proprietary_models,
    'Does the proprietary model extract value from users/developers through lock-in, licensing terms, or intentional incompatibility, making it extractive rather than coordinative even if framed as legitimate choice?',
    'Structural analysis of licensing terms, switching costs, and network effects in proprietary ecosystems. Comparison with open-source switching costs. Post-exit trajectories of developers moving between models.',
    'If substantial extraction is demonstrated (lock-in, predatory licensing), the proprietary model is snare-like, and the pragmatic frame erases that by accepting the model as legitimate. If switching is genuinely available and terms are clear, the coordinate frame holds and extraction is not hidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_extraction_in_proprietary_models, empirical, 'Whether the legitimacy-accepting frame masks hidden extraction in proprietary models.').

omega_variable(
    sibling_reading_foreclosure_risk,
    'Does widespread acceptance of the pragmatic reading (coexistence as legitimate) foreclose the freedom-imperative and commons-governance readings, or do they remain live alternatives?',
    'Observe whether freedom and commons advocates continue to contest the pragmatic frame in practice, or whether pragmatic coexistence becomes hegemonic and the alternative frames are marginalized. Track regulatory and activist discourse over next 20 years.',
    'If pragmatic coexistence becomes hegemonic, this reading successfully influences (but does not foreclose) the sibling readings by making them appear optional rather than foundational. If the siblings remain actively contested, true coexistence holds and the kernel remains unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_risk, empirical, 'Whether pragmatic acceptance of both models forecloses the normative alternative readings or sustains genuine kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(soft_tr_t30, observed).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 25, 0.27).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(soft_be_t30, observed).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement_basis(soft_su_t30, observed).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the software-control-legitimacy kernel. The kernel is the contested claim about the relationship between code transparency, control authority, and software quality. The pragmatic reading accepts both open-source and proprietary models as legitimate alternatives suited to different contexts and examines their tradeoffs empirically. The freedom-imperative reading rejects proprietary models as unethical; the property-rights reading prioritizes capital recovery; the commons reading frames software as shared infrastructure. All four readings apply to the same phenomenon (software control) but instantiate different constraints with different ε values, victim sets, and beneficiary structures. They are linked by network.affects_constraints to enable comparative analysis of how the same kernel is read differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
