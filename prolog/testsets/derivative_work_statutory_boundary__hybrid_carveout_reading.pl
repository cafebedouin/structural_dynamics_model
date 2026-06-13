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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Statutory Boundary (Hybrid Carveout Reading)
 *   domain: intellectual_property/technology_governance
 *
 * SUMMARY:
 *   The derivative work statutory boundary operates through a hybrid carveout
 *   that permits non-commercial transformative use while requiring commercial
 *   users to obtain authorization. This reading frames the boundary as a
 *   negotiated coordination mechanism: copyright holders retain control over
 *   lucrative commercial adaptation, non-commercial creators operate under an
 *   exemption, and intermediaries facilitate licensing markets. The
 *   constraint is claimed as tangled_rope because it requires active
 *   enforcement (distinguishing commercial from non-commercial intent),
 *   generates asymmetric extraction (commercial users pay, non-commercial
 *   users exempt), and coordinates authorization pathways for some users
 *   while foreclosing them for others. The kernel contest involves three
 *   structurally distinct readings: the coordination reading treats all
 *   transformative uses as non-infringing; the enclosure reading treats all
 *   derivative works as requiring authorization regardless of commerciality;
 *   this hybrid carveout reading treats the boundary itself as the legitimate
 *   axis—placing the transformation rule inside the scope of commercial
 *   intent determination.
 *
 * KEY AGENTS:
 *   - original_copyright_holders — agenda-setter and beneficiary, controls licensing authority and extracts licensing fees from commercial users
 *   - commercial_derivative_creators — payer (must license), faces licensing costs and uncertainty about boundary application
 *   - non_commercial_transformative_users — payer (enforcement exposure), benefits from carveout but faces constant reclassification risk
 *   - educational_institutions — payer with identity-lock (accreditation liability), ambiguously positioned between institutional funding and non-commercial intent
 *   - licensing_intermediaries — beneficiary, extract transaction fees from licensing requirement
 *   - enforcement_authorities — observer, interpret the commercial/non-commercial boundary through litigation
 *   - cultural_commons_advocates — excluded, would eliminate the commercial axis entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Statutory Boundary (Hybrid Carveout Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '808c37ee-1782-4b83-9f23-511586415fec').
narrative_ontology:cs_kernel_codification('808c37ee-1782-4b83-9f23-511586415fec', formalized).
narrative_ontology:cs_authority_grounding('808c37ee-1782-4b83-9f23-511586415fec', lineage).
narrative_ontology:cs_interpretation_layer_present('808c37ee-1782-4b83-9f23-511586415fec').
narrative_ontology:cs_reading_relation('808c37ee-1782-4b83-9f23-511586415fec', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('808c37ee-1782-4b83-9f23-511586415fec', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_axiom('808c37ee-1782-4b83-9f23-511586415fec', foundational, commerciality_determines_authorization_requirement).
narrative_ontology:cs_axiom_status(commerciality_determines_authorization_requirement, holdable).
narrative_ontology:cs_axiom_grounding('808c37ee-1782-4b83-9f23-511586415fec', commerciality_determines_authorization_requirement, conventional).
narrative_ontology:cs_axiom('808c37ee-1782-4b83-9f23-511586415fec', foundational, transformative_intent_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(transformative_intent_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('808c37ee-1782-4b83-9f23-511586415fec', transformative_intent_necessary_but_insufficient, deontological).
narrative_ontology:cs_reference_frame('808c37ee-1782-4b83-9f23-511586415fec', statutory_derivative_work_right_with_commercial_boundary).
narrative_ontology:cs_drift_state('808c37ee-1782-4b83-9f23-511586415fec', contemporary_blended_monetization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('808c37ee-1782-4b83-9f23-511586415fec', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, original_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, educational_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, open_source_and_fan_communities).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_creators).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, transformative_use_doctrine_partial_validity).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, market_based_licensing_coordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive rights to authorize derivative works. Under this reading, they retain licensing authority over commercial exploitations while ceding enforcement against non-commercial uses, creating a two-tier system. They can selectively enforce or license commercial adaptations, extracting licensing fees from commercial users while maintaining goodwill through permissive non-commercial policy.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, original_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, original_copyright_holders, beneficiary).

% Must obtain explicit license to create derivative works for commercial purposes. They benefit from the clarity the boundary provides (knowing which uses require authorization) but bear the licensing cost and negotiation overhead. Their alternatives are creating non-commercial works, seeking licenses, or avoiding the protected material—all costly relative to unrestricted adaptation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_creators, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_creators, beneficiary).

% Can create transformative derivative works without authorization if non-commercial. They nominally benefit from this carveout but face constant legal uncertainty: what counts as 'non-commercial,' whether their work might incidentally monetize (ads, donations, institutional hosting), and the risk that copyright holders contest the classification. The exemption is conditional and reversible by enforcement discretion.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users, payer,
    moderate, biographical, constrained, global).

% Occupy an ambiguous position: teaching materials may incorporate derivative uses that are technically non-commercial but institutionally funded, creating liability risk. They are locked into copyright compliance by accreditation requirements and legal liability exposure. The carveout is narrow (non-commercial) while educational use is institutionally structured around budgets and organizational liability, creating constant compliance uncertainty.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, educational_institutions, payer,
    organized, generational, identity_locked, national).

% Benefit from the non-commercial carveout, creating substantial derivative ecosystems (fan fiction, mods, remixes, creative commons adaptations) without licensing fees. They operate at the margin of the exemption, maintaining non-commercial status through community norms and technical choices. Their collective output is substantial and politically visible, shaping public perception of 'fair use' and transformative rights.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, open_source_and_fan_communities, beneficiary,
    organized, biographical, mobile, global).

% Operate licensing clearinghouses and rights-management platforms that emerged specifically because derivative work authorization is legally required for commercial uses. They benefit from the licensing bottleneck: without the requirement, their services would be less valuable. They extract transaction fees and information rents from the authorization requirement.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries, beneficiary,
    moderate, biographical, mobile, global).

% Courts and legislatures interpret and apply the commercial/non-commercial boundary through litigation and statutory revision. They adjudicate disputes about what counts as derivative, when commercial intent applies retroactively, and whether the carveout has swallowed the rule. Their interpretive choices reshape the effective boundary continuously.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, enforcement_authorities, observer,
    institutional, generational, analytical, national).

% Would argue for broader derivative use rights, asserting that all non-infringing transformations should be permitted regardless of commerciality. They are excluded from the baseline rule-setting (which foregrounds the commercial boundary as the legitimate axis) but do contest it through legislative advocacy and litigation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, cultural_commons_advocates, excluded,
    moderate, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, original_copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for authorizing derivative adaptations: copyright holders can selectively license commercial derivatives, creating a market mechanism for adaptation rights rather than a blanket prohibition. Non-commercial users benefit from predictable exemption. Intermediaries provide transaction infrastructure (licensing platforms) that would be unnecessary under blanket prohibition.
% TRANSFER_FUNCTION: Moves licensing fees from commercial derivative creators to original copyright holders; moves information-rent extraction from licensing intermediaries; exempts non-commercial users but exposes them to enforcement discretion and boundary-testing costs.
% ABSENT_VOICES: Creators whose work becomes locked behind licensing requirements because it is commercially successful speak through litigation and legislative advocacy but are not seated at the rule-making table. Cultural commons advocates who would eliminate the commercial boundary entirely are absent from copyright-holder and commercial-user negotiation frames.
% DISAPPEARANCE_RATIONALE: If the commercial/non-commercial boundary evaporated and all derivative use required authorization, commercial adaptation industries would contract, licensing negotiation costs would rise, and non-commercial communities would face legal exposure. If conversely all derivative use became permissible, copyright holders would lose licensing revenue and the incentive structure for original creation would shift. The constraint structures who negotiates with whom and under what conditions.
% FOUNDING_PROBLEM: Original copyright law treated derivative works as categorically infringing preparation, but courts and scholars argued that purely transformative uses (parody, remix, educational adaptation) should not require authorization. The hybrid carveout emerged as a compromise: permit transformative non-commercial use, but require licensing for commercial derivatives—balancing original-creator incentives with cultural adaptation rights.
% FOUNDING_PROBLEM_CORROBORATION: Copyright scholars (including critics of the status quo) acknowledge the foundational problem: tension between derivative-work protection and transformative-use doctrine. However, whether the founding problem remains 'live' is contested: copyright holders argue that licensing markets function well and incentives remain strong; adaptation advocates argue that the problem has inverted—the commercial boundary now imposes licensing costs that suppress beneficial adaptation. Legislative records from derivative-work reform efforts (DMCA Safe Harbor, EU Directive harmonization) show both positions represented.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).

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
 *   Extractiveness measures 0.58 at interval end because licensing fees are collected from commercial users without direct proportionality to service cost; the boundary itself creates rents by restricting who can adapt without authorization. Suppression is moderately high (0.62) because enforcement requires active policing of the non-commercial/commercial boundary: copyright holders monitor for boundary violations, intermediaries track licensing compliance, and courts adjudicate contested cases. Theater is moderate (0.28) because the carveout's non-commercial exemption is real but narrow—much activity falls into ambiguous territory (institutional funding with public benefit, fan communities with merchandise ties, educational platforms with embedded advertising). The measurement series shows slight extraction and suppression increase over the interval, indicating tightening boundary enforcement as digital distribution makes commercial/non-commercial intent harder to maintain cleanly (blended monetization models, platform-mediated distribution, institutional boundaries). Accessibility collapse is moderate (0.48) because alternatives exist: creating original works, seeking licenses, negotiating blanket licenses, or operating outside copyright regimes (public domain, unprotected works, independent creation). Resistance is high (0.71) because non-commercial communities actively resist boundary tightening through platform advocacy, legislative advocacy for broader carveouts (EU DSM Directive exception-expansion debates), and technological workarounds that maintain non-commercial appearance.
 *
 * PERSPECTIVAL GAP:
 *   Commercial derivative creators and copyright holders experience the constraint differently from non-commercial users and educational institutions. Copyright holders see the commercial boundary as a legitimate coordination mechanism that preserves incentives and licensing revenue; commercial creators see it as a licensing tax that competitors might avoid through non-commercial framing; non-commercial users see it as conditional permission that could be revoked through aggressive enforcement; educational institutions see it as creating liability exposure through ambiguous classification. From enforcement authorities' seat, the boundary is inherently contestable because 'commercial intent' is legal metaphor, not natural fact: the same work (educational platform, fan collection, institutional remix) can be reclassified by different parties depending on monetization structure and institutional backing. The engine computes directionality from beneficiary/victim declarations and exit options: copyright holders and intermediaries have low d (beneficiaries with arbitrage exit); commercial creators have high d (payers with constrained exit); non-commercial users sit between (nominal exemption but identity-lock into compliance).
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders (institutional power, beneficiary role) derive d near 0.0: they set the rules, control authorization, and extract licensing fees. Commercial creators (powerful power, payer role, constrained exit) derive d near 0.85: they must negotiate, pay fees, and cannot exit to unrestricted derivative adaptation. Non-commercial users (moderate power, payer role via enforcement exposure, constrained exit via identity-lock into 'non-commercial' classification) derive d near 0.60: they face enforcement risk and boundary-testing costs but have the nominal carveout as partial protection. Licensing intermediaries (moderate power, beneficiary role, mobile exit) derive d near 0.15: they profit from the licensing bottleneck but could migrate to other rent-extraction services if derivative-work licensing collapsed. Educational institutions require a directionality override: their formal role is 'payer' (enforcement exposure, institutional liability) with 'identity_locked' exit (accreditation requirements), but their structural relationship to the constraint is more target-like (d ~0.70) than the moderate power atom would suggest, because the identity-lock is institutional rather than individual—they cannot exit without organizational dissolution. Override: educational_institutions d=0.70.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tension between derivative-work exclusivity and transformative-use doctrine) remains contested rather than resolved. Under the coordination reading, mandatrophy would trigger if the problem were dead (derivative licenses became unnecessary, transformative use doctrine fully absorbed derivative-work protection). Under the enclosure reading, mandatrophy would trigger if all commercial derivatives required authorization but licensing markets didn't form or worked poorly. Under this hybrid carveout reading, mandatrophy would trigger if the commercial/non-commercial boundary itself became unenforceable or universally gamed—if the exemption swallowed the rule. Current evidence suggests the boundary persists: licensing markets function (intermediaries exist, licensing is negotiated), enforcement occurs (copyright litigation continues, DMCA takedowns target derivative platforms), and the carveout is real but narrow. However, digital distribution is creating mandatrophy pressure: blended monetization (ads in fan spaces, institutional platforms with embedded commerce, creator economies where 'non-commercial' is unstable) makes the boundary harder to police cleanly. The theater increase (0.18 → 0.28) over the interval reflects rising performative boundary-maintenance: copyright holders make public commitments to non-commercial tolerance while aggressively enforcing against anything with commercial ambiguity, creating the appearance of carveout expansion while extractiveness stabilizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commerciality_boundary_stability,
    'As digital distribution creates blended monetization models (ads, institutional funding, creator economics, platform revenue sharing), can the commercial/non-commercial boundary remain stable and enforceable, or will it dissolve into contested classification?',
    'Empirical tracking of copyright litigation trends: if commercial/non-commercial boundary disputes are rising as a fraction of derivative-work litigation, the boundary is destabilizing. Platform enforcement patterns: if copyright holders shift to aggressive automated enforcement of any potential commercial adjacency (educational platforms with ads, fan communities with Patreon integration, open-source projects with commercial deployment), the exemption shrinks in practice.',
    'If the boundary becomes unenforceable or universally gamed, the carveout reading collapses into the enclosure reading (all uses require authorization) or the coordination reading (all transformative uses are non-infringing). The classification would shift from tangled_rope (hybrid with asymmetric payoffs) to snare (pure extraction) or rope (pure coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commerciality_boundary_stability, empirical, 'Whether the commercial/non-commercial distinction can survive blended monetization and institutional funding complexity.').

omega_variable(
    transformative_use_doctrine_scope,
    'Does the non-commercial carveout operate as a genuine safe harbor (predictable exemption for defined categories) or as a discretionary equitable doctrine (fair use, transformative use, with enforcement contingent on case-by-case adjudication)?',
    'Statutory vs. case-law evolution: if legislatures codify clear non-commercial categories (education, commentary, parody, remix), the carveout hardens into statutory safe harbor. If courts continue to apply transformative-use doctrine on a case-by-case basis, the exemption remains discretionary. European and national approaches differ: EU DSM Directive (2019) codified text and data mining exception; US law relies on fair-use case law. Tracking statutory reform trends would indicate which trajectory dominates.',
    'A genuine safe harbor would support the coordination reading and reduce extraction (users know they are protected). Discretionary doctrine would support the snare reading and increase theater (users must convince courts their use is protected; copyright holders retain enforcement discretion). The measured extractiveness and theater_ratio are calibrated to the current mixed state (partially codified, partially discretionary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_doctrine_scope, conceptual, 'Whether the non-commercial exemption is a structural safe harbor or an equitable case-by-case doctrine.').

omega_variable(
    institutional_vs_individual_non_commercial,
    'Can institutional non-commercial use (educational platforms, public broadcasting, open-source software supported by institutional funding) be coherently distinguished from commercial use, or does institutional scale inevitably collapse the distinction into ''organized entity with resources''?',
    'Empirical: educational institution litigation trends—if schools, libraries, and public broadcasters face increased licensing demands despite non-commercial mission, the institutional exception is failing. Statutory: if legislative reforms explicitly carve out institutional education and public good uses, the distinction holds; if they treat all institutional funding as ''commercial scale,'' the distinction collapses.',
    'If institutions face escalating licensing costs, they become de facto payers (victims) despite non-commercial mission, and educational access becomes extractive from institutional budgets. The directionality override for educational_institutions (d=0.70) assumes this partial collapse is already happening; if complete collapse occurs, institutions would be fully victimized (d→0.85) and the constraint shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_individual_non_commercial, empirical, 'Whether institutional non-commercial uses remain protected or collapse into commercial-scale licensing requirements.').

omega_variable(
    kernel_reading_contest_instantiation,
    'Is this reading structurally coherent as a distinct instantiation of the kernel, or does the commercial/non-commercial boundary collapse when pressed—revealing that the true contest is binary (transformation sufficient or not) rather than ternary (transformation+commerciality)?',
    'Case-law analysis: if courts applying this reading consistently treat commerciality as an independent element (transformation + non-commercial intent required), the reading holds as a distinct coherent position. If courts collapse it into either the coordination reading (transformation sufficient, commerciality irrelevant) or enclosure reading (all derivative works require authorization, transformation irrelevant), the boundary is a false trichotomy.',
    'If the binary collapse occurs, this reading is not a stable position but a temporary compromise zone between two antagonistic readings. Classification would revert to one of the sibling readings, and the entire constraint story would need to be re-attributed to the true dominant reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_instantiation, conceptual, 'Whether the hybrid carveout reading is a stable third position or a transitory compromise zone between binary opposites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(deri_tr_t5, observed).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(deri_tr_t10, observed).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(deri_tr_t15, observed).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(deri_tr_t20, observed).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(deri_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(deri_be_t5, observed).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(deri_be_t10, observed).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(deri_be_t15, observed).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(deri_be_t20, observed).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(deri_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(deri_su_t5, observed).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(deri_su_t10, observed).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(deri_su_t15, observed).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(deri_su_t20, observed).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(deri_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.12).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, fair_use_doctrine_parody_exception).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_licensing_market_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the derivative_work_statutory_boundary kernel. The coordination_reading treats all transformative uses as non-infringing. The enclosure_reading treats all derivative works as requiring authorization. This hybrid_carveout_reading makes the commercial/non-commercial boundary the central axis of distinction. All three readings must be generated as separate constraint stories with distinct ε values and stakeholder structures. The network edges create a constraint family: changes to enforcement of the commercial boundary in this reading would affect the empirical support for the coordination and enclosure readings, and vice versa (empirical evidence that the boundary is unstable would shift relative belief-mass toward the binary readings). See audits/2026-06-12_derivative_work_kernel_family/DECOMPOSITION.md for the full ε-invariance justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
