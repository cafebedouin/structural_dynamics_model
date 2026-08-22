% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout
 *   domain: intellectual_property/technology_governance
 *
 * SUMMARY:
 *   The hybrid-carveout reading instantiates a derivative-work boundary that
 *   separates commercial from non-commercial uses. Non-commercial
 *   transformative works (scholarship, commentary, remix art, documentary
 *   clips) are permitted without authorization; commercial derivative
 *   creation requires licensing. This reading is ONE OF THREE interpretations
 *   of the derivative-work statutory kernel. The enclosure reading treats ALL
 *   incorporation of copyrighted expression as derivative-work infringement
 *   (highest extraction, weakest carveout). The coordination reading treats
 *   only substantially-similar recastings as derivative works, leaving
 *   transformative and intermediate uses non-infringing (lowest extraction,
 *   broadest permission). This reading sits between: it preserves a licensing
 *   market for commercial adaptation while exempting non-commercial
 *   transformation. The authored metrics (0.62 extractiveness, 0.68
 *   suppression) describe the hybrid-carveout reading's actual operation —
 *   commercially viable derivative creators face real licensing costs and
 *   legal compliance overhead; non-commercial researchers and artists operate
 *   freely. The claimed type is tangled_rope because the same statutory
 *   mechanism coordinates the licensing market (benefiting copyright holders,
 *   enabling orderly adaptation) while extracting from commercial downstream
 *   developers who have no exit except paying, litigating, or abandoning
 *   commercial derivative projects.
 *
 * KEY AGENTS:
 *   - copyright_holders: beneficiary/agenda-setter (institutional); control the licensing gate; profit from commercial derivative licensing
 *   - commercial_derivative_creators: payer (powerful to powerless by sub-category); bear licensing costs; constrained exit
 *   - non_commercial_transformative_users: beneficiary (moderate); operate freely within carveout; mobile exit
 *   - technology_platforms: payer/beneficiary dual (institutional); ambiguous zone between user-content carveout and commercial AI training
 *   - independent_artists: payer (powerless); identity-locked exit; face prohibitive licensing costs relative to revenue
 *   - open_culture_advocates: excluded; would contest the carveout's size and commercial boundary definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.68).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '17b2a140-6a32-4266-b1b3-1d86e9a706d2').
narrative_ontology:cs_kernel_codification('17b2a140-6a32-4266-b1b3-1d86e9a706d2', fixed_text).
narrative_ontology:cs_authority_grounding('17b2a140-6a32-4266-b1b3-1d86e9a706d2', lineage).
narrative_ontology:cs_interpretation_layer_present('17b2a140-6a32-4266-b1b3-1d86e9a706d2').
narrative_ontology:cs_reading_relation('17b2a140-6a32-4266-b1b3-1d86e9a706d2', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('17b2a140-6a32-4266-b1b3-1d86e9a706d2', derivative_work_statutory_boundary__coordination_reading, influences).
narrative_ontology:cs_axiom('17b2a140-6a32-4266-b1b3-1d86e9a706d2', foundational, commercial_use_requires_authorization).
narrative_ontology:cs_axiom_status(commercial_use_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('17b2a140-6a32-4266-b1b3-1d86e9a706d2', commercial_use_requires_authorization, empirically_contingent).
narrative_ontology:cs_axiom('17b2a140-6a32-4266-b1b3-1d86e9a706d2', foundational, non_commercial_transformation_exempted).
narrative_ontology:cs_axiom_status(non_commercial_transformation_exempted, holdable).
narrative_ontology:cs_axiom_grounding('17b2a140-6a32-4266-b1b3-1d86e9a706d2', non_commercial_transformation_exempted, deontological).
narrative_ontology:cs_reference_frame('17b2a140-6a32-4266-b1b3-1d86e9a706d2', author_exclusive_right_to_derivatives).
narrative_ontology:cs_drift_state('17b2a140-6a32-4266-b1b3-1d86e9a706d2', ai_training_era_ambiguity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('17b2a140-6a32-4266-b1b3-1d86e9a706d2', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, technology_platforms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, technology_platforms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, independent_artists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Original work creators and their assignees (studios, publishers, record labels) benefit from the right to control and monetize derivative works. They set the boundary rule by lobbying legislation and establishing licensing frameworks. They control the authorization gate for commercial uses, extracting licensing fees from commercial derivative creators while non-commercial uses escape their control entirely.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, agenda_setter).

% Publishers, studios, game developers, AI model trainers, and other commercial entities that incorporate copyrighted expression into new works must negotiate licenses or face infringement liability. They bear licensing costs, legal compliance overhead, and restricted access to source material. Their exit options are absorbing the cost, licensing at high rates, settling disputes, or finding alternative (non-copyrighted) source material, all costly.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_creators, payer,
    powerful, biographical, constrained, global).

% Academic researchers, artists, documentary filmmakers, journalists, and remix creators using copyrighted works in non-commercial transformative contexts operate largely outside the licensing gate. They benefit from the carveout: they can engage source material for research, criticism, commentary, and artistic transformation without negotiating rights. Their exit is complete — they can operate freely within the non-commercial frame.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users, beneficiary,
    moderate, biographical, mobile, global).

% Social media platforms, search engines, and AI service providers operate in an ambiguous zone: they host derivative content (user remixes, commentary, thumbnails with clips) non-commercially on their platforms while monetizing the platform overall through advertising and data. They benefit from the non-commercial carveout when defending user-generated content but face licensing pressure when their machine-learning systems (trained on large copyrighted corpora) are commercialized. They have substantial resources to negotiate or litigate but face rising compliance costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, technology_platforms, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, technology_platforms, beneficiary).

% Musicians, visual artists, and writers working alone or in small groups who incorporate samples, references, or transformative elements from copyrighted works face the highest effective constraint. They lack the resources to negotiate licenses or defend against cease-and-desist letters. The boundary rule effectively traps them: they cannot commercialize (licensing costs prohibitive); non-commercial work does not sustain a career. Many self-identify as artists, making exit identity-dissolution.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, independent_artists, payer,
    powerless, biographical, identity_locked, local).

% Would argue for broader transformative-use exemptions, shortened copyright terms, and mandatory licensing for derivative works. They advocate for public-domain expansion and compulsory licensing to lower barriers to creation. They are structurally excluded from the negotiation and legislative process relative to copyright holder lobbying power.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, open_culture_advocates, excluded,
    moderate, generational, constrained, global).

% Courts interpret the statutory boundary and fair-use exceptions; regulators consider competition implications of licensing cartels and technological protection measures (DMCA). They adjudicate disputes when the non-commercial/commercial boundary is contested and can impose remedies that shift the enforcement line.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system for originating authors to control commercial reuse of their expression while permitting non-commercial transformation and commentary. Creates a licensing market for derivative rights, enabling authors to monetize adaptation rights without losing attribution or facing unlimited reuse.
% TRANSFER_FUNCTION: Moves licensing fees from commercial derivative creators to copyright holders (or their representatives) for the right to use copyrighted expression in new commercial works. Non-commercial users pay zero and receive full access within the carveout. The commercial/non-commercial boundary sorts payers from beneficiaries.
% ABSENT_VOICES: Open-culture advocates, software commons practitioners, and remix communities argue for broader transformation exemptions and shorter copyright terms. They would contest the size of the commercial boundary and the exclusion of certain derivative uses (mashups, academic AI training) from the carveout. They have limited standing in copyright legislative processes dominated by publisher and studio lobbying.
% DISAPPEARANCE_RATIONALE: If the derivative-work licensing requirement and its commercial/non-commercial carveout vanished, derivative creation would explode: every adaptation would become non-infringing by default (pending other statutory limits like fair use). Licensing fees would cease, copyright holders would lose a primary revenue stream, and the incentive structure for controlling derivative adaptation would collapse. Commercial derivative markets would reorganize around either all-permissive licensing or contractual arrangements separate from copyright.
% FOUNDING_PROBLEM: Unauthorized derivative works—adaptations, sequels, translations, abridgements incorporating substantial copyrighted expression—compete with official versions and can harm the original's market and reputation. Authors need a right to control and profit from adaptations of their work.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and publishers attest the problem is live, citing unauthorized remakes, deepfakes, and model training as ongoing threats. Technology platforms and open-culture advocates attest the founding problem is largely solved by technological measures (watermarking, authentication) and market abundance, and the licensing requirement persists as rent collection and gatekeeping. Courts have acknowledged both framings in contemporary fair-use and DMCA rulings without resolving the contest.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint extracts licensing fees from commercial users while exempting non-commercial users — a categorical split that creates two directionalities. The commercial/non-commercial boundary determines d: commercial actors face high d (toward target, constrained exit), non-commercial actors face low d (beneficiary or mobile), copyright holders face low d (beneficiary, arbitrage exit). Suppression is substantial (0.68) because enforcement depends on active gatekeeping: copyright holders must identify unauthorized commercial uses, issue takedowns, defend against fair-use claims, and maintain licensing infrastructure. Theater ratio rises from 0.25 to 0.44 over the interval because enforcement increasingly focuses on defending the commercial boundary line itself (DMCA anti-circumvention provisions, licensing contract terms) rather than on controlling actual infringement — the theatrical component is the maintenance of the gate even when substantial transformation has occurred. Resistance is moderate (0.58) because commercial users push back through fair-use litigation and licensing boycotts, while non-commercial users largely comply. The measurements show extractiveness rising gradually as copyright holders add sublicense conditions and extend the commercial boundary (e.g., treating AI model training as commercial derivative work even when the resulting model is freely available). This reading sits structurally between full enclosure (all uses derivative, highest extraction) and coordination framing (transformation-preserving, negligible extraction).
 *
 * PERSPECTIVAL GAP:
 *   From the copyright-holder and publisher seat, the hybrid carveout is a justified coordination mechanism: it permits legitimate transformation (scholarship, remix, satire) while enabling monetization of commercial adaptation, balancing incentives for origination with access for downstream creation. From the commercial-derivative-creator seat, the same boundary appears as arbitrary gatekeeping: the commercial/non-commercial distinction is porous (a film shot on a budget can become commercial after festival success), licensing fees bear no relation to actual copying (AI models trained on copyrighted text may produce entirely original output), and the threat of takedown and infringement liability creates a chilling effect on legitimate transformation. From the non-commercial-user seat, the carveout is beneficial but precarious — one commercial transaction (licensing the work for small compensation) reclassifies the entire project as derivative-work-requiring and destroys the carveout. The engine computes these seated experiences from the structural data; the committer frame does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint has fundamentally asymmetric directionality by design. Copyright holders (beneficiary) have d near 0.0: they control the licensing gate, arbitrage between enforcing and permitting uses, and collect fees with minimal compliance cost. Commercial derivative creators (payer) have d near 0.85–0.95 depending on scale: they face licensing costs, legal overhead, and strong constraints on free operation. The commercial/non-commercial carveout creates a second tier: non-commercial users have d near 0.1–0.2 (beneficiary, mobile exit), while small independent artists have d near 0.9 (identity-locked, powerless, trapped). Technology platforms occupy an intermediate zone: their platforms benefit from the non-commercial carveout (d ~0.3), but their commercial AI services face licensing pressure (d ~0.7 for that component). This reading's per-seat classification should diverge sharply: copyright holders compute as receiving net subsidy; commercial users compute as heavily extracted from; non-commercial users compute as beneficiaries or recipients of subsidy. The engine computes this divergence from the structural data — the committer frame does not preset the seat classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unauthorized derivative works harming original's market and reputation) is alive but contested. Copyright holders attest the problem persists: unauthorized sequels, deepfakes, and machine-learning model training on copyrighted works remain concerns. Open-culture advocates and courts increasingly attest the problem is managed through other mechanisms (technological watermarking, authentication systems, market competition) and the licensing requirement persists as gatekeeping beyond the founding problem's scope. The constraint avoids pure mandatrophy by maintaining a genuine licensing market that copyright holders actively use, but theater_ratio rising toward 0.44 signals increasing performative enforcement (defending the boundary itself) as the underlying adaptation need shifts toward AI model training and automated transformation. The divergence between founding problem (preventing unauthorized adaptation) and actual enforcement (controlling commercial exploitation of any copyrighted material) suggests drift toward extraction without coordination — a mandatrophy candidate in the 20–30 year range. The omega variable captures this contest: if courts expand fair-use exception beyond transformation-preserving uses, or if AI training is reclassified as non-commercial, the licensing extraction collapses and mandatrophy resolves. If the commercial/non-commercial boundary holds and licensing becomes compulsory across AI training, the constraint hardens into snare-territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_boundary_instability,
    'Is the commercial/non-commercial boundary stable and unambiguous, or does it collapse under pressure from emerging derivative-creation technologies (AI model training, procedural generation, etc.)?',
    'Tracking court decisions on boundary cases: is AI training commercial or non-commercial? Is a free app funded by ad revenue commercial? Is a research model with eventual commercialization non-commercial during development? Longitudinal survey of enforcement disputes at the boundary.',
    'If the boundary remains stable, the constraint maintains its tangled_rope character: genuine carveout for non-commercial users, functioning licensing market for commercial users. If the boundary collapses (AI training reclassified as universally commercial, or conversely as non-commercial research), the constraint bifurcates: either all uses become licensed (snare), or all transformation becomes permitted (rope). The effective extraction for commercial actors depends entirely on boundary stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commercial_boundary_instability, empirical, 'Whether the commercial/non-commercial distinction can survive AI and procedural-generation technologies').

omega_variable(
    fair_use_vs_licensing_market_tension,
    'Does the non-commercial carveout for transformative use coexist coherently with the licensing market, or does fair-use doctrine progressively expand to absorb licensing categories, collapsing the market?',
    'Analysis of fair-use case law over 15+ years: are successful fair-use defenses expanding into categories that licensing would traditionally cover (e.g., criticism, commentary, remix)? Do copyright holders increasingly fail to enforce against transformative uses even in commercial contexts?',
    'If fair use expands, licensing revenue declines for marginal categories, extraction falls, and the constraint shifts toward rope or coordination. If fair use remains narrowly construed, licensing market persists, extraction holds, and tangled_rope persists. The committer tension between coordination (fair use recognizing transformation) and extraction (licensing market maintaining fees) is unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_vs_licensing_market_tension, empirical, 'Whether fair-use doctrine will expand to subsume licensing market').

omega_variable(
    licensing_cost_vs_creation_barrier,
    'For independent artists and small creators, does the licensing cost function as the binding constraint on commercial derivative creation, or are other barriers (access to source material, platform gatekeeping, market discovery) more decisive?',
    'Counterfactual: if licensing were free or compulsory, would independent derivative-creation volume increase substantially? Surveys and interviews with powerless commercial creators about actual decision-factors.',
    'If licensing cost is the binding constraint, the hybrid carveout creates high extraction from powerless creators (effective suppression rises toward 0.8+), and the constraint''s identification as tangled_rope holds. If licensing cost is secondary and other barriers dominate, the constraint''s extractiveness falls and classification shifts toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_cost_vs_creation_barrier, empirical, 'Whether licensing cost is the effective barrier to independent commercial derivative creation').

omega_variable(
    reading_foreclosure_via_axiom_collision,
    'Does the hybrid-carveout reading''s foundational axiom (commercial uses require authorization; non-commercial transformations are permitted) logically foreclose the enclosure reading (all expression incorporation is derivative), or do the axioms coexist as institutional choices?',
    'Jurisprudential analysis: does the hybrid axiom claim to deny the enclosure axiom''s core premise, or merely to adjudicate it differently? Can a copyright authority adopt the hybrid reading and still acknowledge the enclosure reading''s logical possibility? Case law and statutory history.',
    'If the readings coexist (coexists_with relation), the kernel remains contested and each reading can be instantiated in different jurisdictions or traditions. If the hybrid axiom forecloses enclosure (forecloses relation), the classification system must mark that epistemic incompatibility. The committer structure depends on this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_collision, conceptual, 'Whether the hybrid-carveout axiom forecloses the enclosure axiom or merely disputes its application').

omega_variable(
    non_commercial_carveout_as_fiction,
    'Is the non-commercial user carveout a genuine exemption addressing legitimate transformation needs, or does it function as a suppression mechanism by rendering small creators unable to professionalize their work (trapped identity-locked state)?',
    'Ethnographic study of remix and derivative-creator communities: what fraction transition from non-commercial to commercial work? At what point does non-commercial identity foreclose commercial viability? Is the carveout experienced as permission or as forced amateurization?',
    'If genuine exemption, the non-commercial users are true beneficiaries and the constraint''s tangled_rope character holds: coordination + partial extraction. If suppression mechanism, non-commercial users are actually payers (forced into non-commercial identity) and the constraint''s extraction class rises — potential reclassification toward snare. This determines whether the non_commercial_transformative_users stakeholder is classified as beneficiary or as payer misidentified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_commercial_carveout_as_fiction, conceptual, 'Whether the non-commercial carveout is a genuine exemption or enforced amateurization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.18).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_fair_use_doctrine).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, dmca_anti_circumvention_provision).

% DUAL FORMULATION NOTE:
% The derivative_work_statutory_boundary kernel decomposes into three structurally distinct constraint stories: enclosure_reading (highest extraction, all incorporation derivative), coordination_reading (lowest extraction, transformation-preserving), and hybrid_carveout_reading (moderate extraction, commercial/non-commercial split). The three readings have different beneficiary structures, different per-seat classifications, and different ε values. This story instantiates the hybrid reading: it authorizes a licensing market (coordination) while extracting from commercial actors (asymmetric extraction). The sibling readings are separate constraint stories with their own omegas, measurements, and committer structure. All three readings apply the same statutory kernel; the divergence is in interpretation and institutional choice. They are linked via network.affects_constraints and share kernel_id but have distinct ε, claimed_type, and stakeholder profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
