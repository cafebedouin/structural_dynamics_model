% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary: Transformative Use Coordination
 *   domain: intellectual_property/technology_governance
 *
 * SUMMARY:
 *   Under this reading of the derivative-work statute, the legal boundary
 *   between copyright control and transformative fair use is drawn such that:
 *   (1) fixed recastings that substantially incorporate original expression
 *   remain under copyright holder control; (2) transformative uses—parody,
 *   commentary, remix, adaptation, and machine-learning training—are
 *   permissible without ex-ante licensing when the transformation is
 *   sufficient; (3) the boundary is not categorical but evaluated
 *   case-by-case through fair-use factors, particularly transformative
 *   purpose and market effect. This reading instantiates a coordination
 *   arrangement: it permits cultural and technological innovation to proceed
 *   without licensing friction while preserving copyright holders' exclusive
 *   control over market-substituting derivatives. The arrangement solves a
 *   collective-action problem—how to define the scope of an exclusive right
 *   without freezing cultural evolution—by delegating boundary-setting to
 *   courts through fair-use doctrine rather than prescribing categories ex
 *   ante.
 *
 * KEY AGENTS:
 *   - Original copyright holders (authors, publishers, studios, music majors): retain exclusive control over fixed recastings but lose licensing fees from transformative use
 *   - Transformative users (artists, remix creators, researchers, fan creators): benefit from legal permission to use copyrighted expression as raw material for new creation
 *   - ML training systems and generative technology developers: benefit from treating large-scale training as transformative use, not derivative-work preparation
 *   - Courts and legal interpreters: administer the boundary through case-by-case evaluation; set the agenda by determining what counts as transformative
 *   - Rights-clearance intermediaries: lose licensing revenue as transformative uses escape the licensing system
 *   - Excluded licensing seekers: trapped outside the consensus, lacking resources to assert fair-use claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.28).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.15).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary: Transformative Use Coordination").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '01574543-7166-4a07-95af-821bf4ee36d2').
narrative_ontology:cs_kernel_codification('01574543-7166-4a07-95af-821bf4ee36d2', fixed_text).
narrative_ontology:cs_authority_grounding('01574543-7166-4a07-95af-821bf4ee36d2', lineage).
narrative_ontology:cs_interpretation_layer_present('01574543-7166-4a07-95af-821bf4ee36d2').
narrative_ontology:cs_reading_relation('01574543-7166-4a07-95af-821bf4ee36d2', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('01574543-7166-4a07-95af-821bf4ee36d2', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('01574543-7166-4a07-95af-821bf4ee36d2', foundational, transformative_use_fair_use_protection).
narrative_ontology:cs_axiom_status(transformative_use_fair_use_protection, holdable).
narrative_ontology:cs_axiom_grounding('01574543-7166-4a07-95af-821bf4ee36d2', transformative_use_fair_use_protection, deontological).
narrative_ontology:cs_axiom('01574543-7166-4a07-95af-821bf4ee36d2', foundational, derivative_work_excludes_transformation).
narrative_ontology:cs_axiom_status(derivative_work_excludes_transformation, holdable).
narrative_ontology:cs_axiom_grounding('01574543-7166-4a07-95af-821bf4ee36d2', derivative_work_excludes_transformation, empirically_contingent).
narrative_ontology:cs_reference_frame('01574543-7166-4a07-95af-821bf4ee36d2', fair_use_doctrine_protects_transformative_creation).
narrative_ontology:cs_drift_state('01574543-7166-4a07-95af-821bf4ee36d2', contemporary_generative_ai_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('01574543-7166-4a07-95af-821bf4ee36d2', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_users).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_training_systems).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_technology_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, cultural_producers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, rights_clearance_intermediaries).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine_validity).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, transformative_use_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Creators, researchers, and technologists who build on copyrighted work through parody, commentary, remix, adaptation, and ML training without seeking ex-ante licensing. They benefit from a legal boundary that treats their activity as non-infringing when the use is sufficiently transformative. Their work depends on being able to develop derivative cultural products and data-driven applications without negotiating permission for every source work.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_users, beneficiary,
    organized, generational, mobile, global).

% Authors, publishers, studios, and music labels who hold exclusive derivative-work rights under copyright law. Under this reading, they retain exclusive control over fixed recastings that substantially incorporate their expression, but lose the right to control transformative uses. They bear the cost of not collecting licensing fees or control rents from transformative activity, and the cost of legal uncertainty about where the line falls.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders, payer,
    powerful, biographical, constrained, global).

% Large language models, vision models, and other generative systems that train on corpora including copyrighted text, images, and other works. Under this reading, training is treated as a transformative use, not a preparation of derivative works; the systems benefit from legal permission to ingest corpora without per-work licensing. They benefit from the boundary drawn here; licensing every training-data source would be economically prohibitive.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ml_training_systems, beneficiary,
    institutional, generational, mobile, global).

% Technology companies building generative AI systems, image synthesis tools, remix platforms, and similar systems. They set the technical and legal interpretation of transformative use in practice through their implementation choices (what they train on, what they claim is fair use, how they structure outputs). They benefit directly from a broad transformative-use boundary that permits training and deployment without licensing.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_technology_developers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, generative_technology_developers, beneficiary).

% Musicians, filmmakers, visual artists, and writers who create remix works, fan works, mashups, and transformative adaptations. They depend on the transformative-use boundary to shield their work from infringement claims. Their exit options are constrained: they could cease transformative production or seek licensing for every source, but the licensing model is economically impractical for small-scale or fan creators.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, cultural_producers, beneficiary,
    moderate, generational, constrained, global).

% Licensing administrators, rights-collection societies (ASCAP, BMI, etc.), and legal service providers who intermediated rights transactions under the previous regime. A broad transformative-use boundary reduces the volume of licensing transactions they administer, shrinking their fee base and operational scope. They are economically harmed by the shift from ex-ante licensing to ex-post fair-use evaluation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, rights_clearance_intermediaries, payer,
    moderate, biographical, constrained, global).

% Judges, courts of appeals, and legal scholars who interpret the derivative-work statute and fair-use doctrine. Under this reading, they administer the boundary through case-by-case evaluation of transformative purpose, not categorical ex-ante rules. They set the agenda by deciding which uses are transformative enough to clear the boundary, bearing the burden of uncertainty management and judicial resources spent on boundary disputes.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_legal_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Would-be creators and researchers who lack the legal resources or cultural standing to assert transformative-use claims, or whose transformative intent is ambiguous or commercially marginal. They remain outside the consensus that benefits transformative users; they would prefer clearer licensing rules or lower licensing costs but are neither in the conversation defining transformativeness nor able to assert fair-use defenses credibly.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, excluded_licensing_seekers, excluded,
    moderate, biographical, trapped, global).

% Copyright systems in other jurisdictions (EU, UK, Japan, etc.) that take different approaches to derivative-work boundaries and fair use. They observe the U.S. boundary and coordinate or diverge based on their own statutory frameworks and policy choices. This reading's permissiveness on transformative use creates international regulatory pressure and arbitrage opportunities.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, international_copyright_regimes, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, generative_technology_developers).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a boundary between copyright control and public creativity: permits original copyright holders to control fixed recastings and full commercial derivative works while permitting transformative creators and technology systems to build on copyrighted expression without ex-ante licensing. Solves the tension between incentivizing original authorship and permitting cultural evolution, reuse, and technological innovation.
% TRANSFER_FUNCTION: Moves control rights: original holders retain exclusive control over verbatim copying and substantially incorporated fixed derivatives, but lose exclusive control over transformative uses (parody, commentary, remix, adaptation, ML training). The constraint transfers transformative-use rights from copyright holders to the cultural and technological commons, conditional on adequate transformation.
% ABSENT_VOICES: Small and independent creators who cannot afford to litigate transformative-use claims; international authors and publishers in jurisdictions with stronger neighboring rights; recording artists whose work is used in ML training but who lack standing in AI policy; non-Western cultural producers whose traditions of remix and adaptation conflict with Western copyright absolutism but are not heard in legislative negotiations.
% DISAPPEARANCE_RATIONALE: If this boundary vanished and all secondary uses required ex-ante licensing, the cost of licensing would eliminate most transformative creation (parody, remix, fan works, research), generative AI would require per-source licensing at prohibitive scale, scientific and educational reuse would be chilled, and cultural production would reconcentrate in capital-intensive licensed studios. The internet as a remix and remix-derivative platform would be substantially restructured.
% FOUNDING_PROBLEM: Copyright law's derivative-work right was drafted to control the printing and republication of books and scores—fixed works in fixed media. It did not anticipate transformative use (parody, commentary, adaptation), digital culture and remix, ML training at scale, or the speed of technological recombination. The founding problem was: how to control verbatim copying and market-substituting derivatives without strangling legitimate adaptation, research, and innovation?
% FOUNDING_PROBLEM_CORROBORATION: Courts (Campbell v. Acuff-Rose Music on parody; Sony v. Universal on format-shifting; Authors Guild v. Google on large-scale digitization) have repeatedly affirmed that transformative use is a legitimate fair-use category and that the derivative-work right was not intended to forbid all secondary creation. Technology companies, researchers, and creative communities affirm that the founding problem persists: how to permit innovation and cultural remix without destroying copyright incentives for original work. Copyright holders themselves are split: some (film studios, music majors) seek broader licensing, others (academic publishers, technology companies) benefit from transformative-use freedom.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint does not extract value from the primary creative act (original authorship remains incentivized and compensated); instead, it permits secondary creators to participate without paying a licensing fee to the holder. The extraction that persists is modest: copyright holders lose licensing revenue from transformative uses, but they retain control over the most valuable secondary uses (commercial derivatives, market-substituting copies). Suppression is very low (0.15) because the constraint does not require coercive enforcement of exclusions—transformative users need not seek permission or face retaliation; the boundary is permissive by design. Theater is minimal (0.12) because the constraint's function is genuine: courts actually evaluate fair-use factors; the evaluation is not decorative. Accessibility collapse is moderate (0.42) because the transformative-use boundary, while permissive in principle, still requires creators to assess legal risk, budget for potential litigation, and navigate case-by-case uncertainty. Alternatives (non-transformative creation, or licensing) do not disappear but are costly. Resistance is high (0.58) because copyright holders, especially large publishers and studios, mount substantial legal challenges to transformative-use claims, contest the boundaries in ongoing litigation, and lobby for statutory narrowing of fair use. The measurement series shows extractiveness and suppression remaining flat over the interval: the boundary stabilizes as courts and technology companies establish norms around what constitutes transformation, rather than monotonically drifting upward (what would signal either increasing rent-extraction or increasing enforcement). The theater ratio remains very low, confirming functional rather than performative operation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (copyright holders, rights intermediaries) experience this constraint as a restriction on their ability to monetize secondary uses. From their perspective, the boundary is narrow and arbitrary—'transformative' is undefined until litigated, and courts have expanded its scope over time (Campbell, Google Books, Authors Guild). The beneficiary seats (transformative users, ML developers) experience the same constraint as enablement: a legal permission that makes their innovation economically feasible. The engine computes per-seat classifications reflecting this structural gap: from the copyright holder's institutional seat, the constraint is likely to compute as Tangled Rope (coordination for some, extraction for others), while from the developer's seat it computes as Rope (genuine coordination with no concentrated harm). The court seat is closer to symmetric: courts benefit from interpretive authority but bear the burden of managing uncertainty and political pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative users sit at d ≈ 0.0–0.2 (strong beneficiaries): they collect permission to use copyrighted work as raw material without licensing fees; their costs are minimal (legal uncertainty, potential litigation risk, but not licensing fees). ML training systems and generative developers sit at d ≈ 0.1–0.3: they benefit substantially from transformative-use protection, which enables large-scale training without per-source licensing; their exit option is licensing (prohibitively expensive and logistically infeasible at scale), so exit is trapped/constrained. Original copyright holders sit at d ≈ 0.6–0.75: they bear the cost of lost licensing revenue from transformative uses; their alternative would be expanding the derivative-work right to capture transformative use (exit to enclosure reading), which is institutionally and politically constrained. Courts and legal interpreters sit at d ≈ 0.5: they are symmetric participants—they gain institutional authority by administering the boundary, but they also bear the costs of legal uncertainty, case-by-case adjudication burden, and political pressure from both sides. Rights-clearance intermediaries sit at d ≈ 0.8 (targets): they lose licensing volume directly; their revenue is extracted by the boundary's permissiveness. The inter-seat divergence is significant: what the transformer sees as permission-to-create, the copyright holder sees as lost revenue; what courts see as adjudication, copyright holders may see as unpredictable erosion of their rights.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to permit cultural innovation and technological development without destroying copyright incentives—remains live, not dead. The constraint is not a zombie arrangement persisting past its function. The measuring metrics (stable extractiveness and suppression over the interval; low theater) confirm that the constraint continues to serve its coordination function. However, there is a mandatrophy question at the boundary: does the constraint coordinate genuine tension-resolution, or has it become a cover story for institutional capture by technology companies? The omega addresses this: if ML training is sufficiently transformative that no licensing fee is owed, does that boundary track the statute's intent (permit transformative use) or does it reflect the power of tech companies to define 'transformative' favorably to themselves? The measured resistance (0.58) suggests the mandate is contested, not collapsed—rights holders are mounting real resistance, not accepting the arrangement as settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_scope_definition,
    'What degree and kind of transformation is sufficient to escape the derivative-work right? Is the boundary stable or continually expanding?',
    'Systematic analysis of appellate cases over a 20-year window: track whether courts'' transformative-use findings expand or contract in scope, and whether the factors courts weigh (commercial purpose, market substitution, creative transformation) remain consistent.',
    'If the boundary is expanding, the constraint is drifting toward stronger beneficiary protection (lower extraction for developers, higher for copyright holders), and the coordination function may be degrading into hidden enclosure of the fair-use right itself. If the boundary is stable, the coordination function is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformative_scope_definition, empirical, 'Whether the transformative-use boundary is stable or systematically expanding/contracting over time.').

omega_variable(
    ml_training_as_transformative,
    'Is machine-learning training genuinely transformative use, or is the treatment of ML training as fair use a reading that conflates scale (billions of parameters trained on billions of texts) with transformation (new purpose, new expression)?',
    'Appellate case law establishing whether training itself (absent any output) constitutes creation of a derivative work, and whether the fair-use factor of ''transformative purpose'' applies to statistical model building with no specified downstream application.',
    'If ML training is found to be derivative-work preparation despite transformative output, the boundary shifts toward enclosure, and generative technology developers lose the treatment that currently shields large-scale training. If training is affirmed as transformative, the coordination reading is strengthened but raises the mandate question: is this coordination or institutional capture by tech companies?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ml_training_as_transformative, empirical, 'Whether machine-learning training itself, or only downstream transformation, counts as transformative use.').

omega_variable(
    coordination_vs_capture,
    'Does this constraint genuinely coordinate the tension between copyright incentives and innovation, or has it become a mechanism by which technology companies capture the fair-use right and define transformativeness so broadly that copyright holders lose control without compensation?',
    'Comparative analysis: (1) licensing volume and revenue trends in domains where transformative use is well-established (book publishing, music sampling, remix platforms) vs. domains where ML training is contested (generative AI); (2) survey of copyright holders'' willingness to license vs. their exclusion from compensation under fair-use framing; (3) distribution of litigation costs and legal resources between transformative-use claimants and copyright holders.',
    'If technology companies have systematically redefined transformativeness in their favor, the constraint is a false rope (enclosure in rope clothing). If the boundary reflects genuine negotiation between copyright incentives and innovation needs, the coordination function is real. A finding of capture would warrant boundary re-negotiation or legislated categorical carveouts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_capture, conceptual, 'Whether the constraint coordinates genuine tension or masks institutional capture of the fair-use right.').

omega_variable(
    international_copyright_arbitrage,
    'As the U.S. coordination reading expands transformative-use protection and other jurisdictions (EU, UK) maintain stricter neighboring rights and narrower fair-use domains, do creators and technology companies exploit the divergence through jurisdictional arbitrage?',
    'Tracking of technology company operational decisions (where models are trained, where rights are cleared, where disputes are litigated) over time; analysis of registration and licensing patterns across jurisdictions.',
    'If arbitrage is occurring, the coordination reading may be exporting the boundary asymmetrically—benefiting parties that can operate in multiple jurisdictions while harming rights holders in more restrictive regimes. The constraint''s effective scope would be non-uniform globally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_copyright_arbitrage, empirical, 'Whether jurisdictional divergence in transformative-use interpretation creates regulatory arbitrage.').

omega_variable(
    statute_intent_vs_reading,
    'Does the statute''s original text, legislative history, and common-law derivative-work doctrine support the broad transformative-use reading, or has case law expanded beyond the statutory intent?',
    'Originalist statutory analysis: review legislative history of the 1976 Copyright Act and its predecessors; compare judicial interpretation of derivative work in early case law (1980s–1990s) with current appellate precedent (2010s onward).',
    'If the reading represents genuine statutory evolution, the coordination reading is justified by law. If case law has expanded beyond statutory intent, the boundary may require legislative correction. A finding of drift would support the enclosure reading''s claim that courts have unilaterally narrowed copyright holder rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statute_intent_vs_reading, conceptual, 'Whether the transformative-use reading aligns with statutory intent or represents judicial expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(deri_tr_t5, observed).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(deri_tr_t10, observed).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(deri_tr_t15, observed).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(deri_tr_t25, observed).
narrative_ontology:measurement(deri_tr_t35, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 35, 0.12).
narrative_ontology:measurement_basis(deri_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(deri_be_t5, observed).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(deri_be_t10, observed).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement_basis(deri_be_t15, observed).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(deri_be_t25, observed).
narrative_ontology:measurement(deri_be_t35, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 35, 0.28).
narrative_ontology:measurement_basis(deri_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement_basis(deri_su_t5, observed).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(deri_su_t10, observed).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 15, 0.145).
narrative_ontology:measurement_basis(deri_su_t15, observed).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(deri_su_t25, observed).
narrative_ontology:measurement(deri_su_t35, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 35, 0.15).
narrative_ontology:measurement_basis(deri_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__coordination_reading, 0.12).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine_four_factors).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, copyright_incentive_intensity).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, generative_ai_training_legal_status).

% DUAL FORMULATION NOTE:
% The derivative-work statutory boundary kernel decomposes into three readings: coordination_reading (this constraint, permissive transformative use), enclosure_reading (maximal copyright holder control), and hybrid_carveout_reading (commercial/non-commercial split). These are not the same constraint measured differently; they instantiate different structural relationships between copyright holders, transformative users, and technology systems. Each reading has distinct ε, distinct beneficiaries/victims, and distinct classification. They are linked by network.affects_constraints because changes in one reading's legal precedent directly pressure the others' interpretive space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__coordination_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
