% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope — Enforcement Vacuum Reading (Interpretive Plurality)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint models the GPL Section 2(b) derivative-work boundary not
 *   as either the strong or narrow reading resolved, but as the enforcement
 *   vacuum ITSELF — the structural fact that no definitive judicial precedent
 *   exists, so both readings remain simultaneously licensed and the actual
 *   operative rule in any given case tracks which interpretive community has
 *   the standing, resources, and will to enforce its preferred reading in
 *   that context. FSF-aligned projects effectively enact the strong-copyleft
 *   reading where Conservancy or FSF litigation reaches; industry-dominated
 *   ecosystems effectively enact the narrow-scope reading where their
 *   internal legal departments and resource advantage make challenge unlikely
 *   to succeed. This is a genuinely distinct constraint from either sibling
 *   reading: its ε is not the ε of strong copyleft (which would measure the
 *   burden of mandatory relicensing) nor the ε of narrow scope (which would
 *   measure the risk of under-compliance) but the transaction-cost and
 *   risk-allocation burden of not knowing which regime applies, borne
 *   disproportionately by parties who lack the resources to arbitrage the
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - fsf_and_conservancy_enforcers: organized enforcement capacity, enacts strong reading within reach
 *   - industry_dominated_consortia: institutional resource advantage, enacts narrow reading within reach
 *   - pragmatic_hybrid_adopters: exploit the gap deliberately for flexibility
 *   - clarity_seeking_small_developers: bear elevated transaction costs from irreducible uncertainty
 *   - courts_and_legislatures: structurally absent resolvers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Enforcement Vacuum Reading (Interpretive Plurality)").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '841fb6dc-4eea-40b5-8a1a-77b4044aaa17').
narrative_ontology:cs_kernel_codification('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', distributed).
narrative_ontology:cs_authority_grounding('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', distributed).
narrative_ontology:cs_reading_relation('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', foundational, enforcement_capacity_determines_operative_rule).
narrative_ontology:cs_axiom_status(enforcement_capacity_determines_operative_rule, holdable).
narrative_ontology:cs_axiom_grounding('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', enforcement_capacity_determines_operative_rule, empirically_contingent).
narrative_ontology:cs_axiom('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', secondary, absence_of_precedent_is_itself_a_governing_structural_fact).
narrative_ontology:cs_axiom_status(absence_of_precedent_is_itself_a_governing_structural_fact, holdable).
narrative_ontology:cs_axiom_grounding('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', absence_of_precedent_is_itself_a_governing_structural_fact, conventional).
narrative_ontology:cs_reference_frame('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', textual_underdetermination_at_drafting).
narrative_ontology:cs_drift_state('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', contemporary_multi_architecture_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('841fb6dc-4eea-40b5-8a1a-77b4044aaa17', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_hybrid_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, large_industry_integrators).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_small_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, downstream_redistributors_facing_dual_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_consortia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sponsors and litigates compliance actions against violators within its ecosystem, effectively enacting the strong-copyleft reading wherever it has standing and resources to enforce. Its enforcement capacity, not any court ruling, is what makes the strong reading operative for FSF-aligned projects. Can walk away from any single dispute without existential risk; funds its enforcement program through donations and settlements.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_and_conservancy_enforcers, agenda_setter,
    organized, generational, mobile, global).

% Large firms and foundations they control set internal legal guidance that adopts the narrow-scope reading for their own linking and packaging practices, and rarely face enforcement because they have the legal resources to contest any claim and because the plaintiff community lacks parallel enforcement reach into their ecosystems. They effectively license their own interpretation into their supply chains.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_consortia, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_consortia, beneficiary).

% Companies and developers who deliberately exploit the interpretive gap — shipping dynamically linked or plugin-based combinations under whichever reading is locally survivable, switching architecture or jurisdiction if challenged. The ambiguity itself is a resource: it lets them capture proprietary value from GPL-adjacent code without committing to either camp's compliance costs.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_hybrid_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Ship products combining GPL components under favorable internal interpretations, backed by legal departments capable of outlasting any enforcement effort brought against them. Benefit directly from the absence of binding precedent because it lets in-house counsel set the operative rule for their own products.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, large_industry_integrators, beneficiary,
    institutional, generational, arbitrage, global).

% Solo developers and small shops who want a definite answer about whether their linking arrangement triggers copyleft obligations, but must instead pay for legal review, insurance-style caution (over-complying by relicensing more than necessary), or accept unquantified litigation risk. They cannot self-insure against ambiguity the way large firms can, so the uncertainty is a real transaction cost with no offsetting benefit.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_small_developers, payer,
    powerless, biographical, constrained, national).

% Distributors who repackage software built by others must satisfy whichever reading the upstream community that supplied the code is prepared to enforce, but cannot know in advance which community that will be for any given component. They bear compliance costs calibrated to the stricter possible reading without any certainty it will actually be the one applied to them.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, downstream_redistributors_facing_dual_risk, payer,
    moderate, biographical, constrained, global).

% Have not issued the definitive derivative-work ruling that would resolve which reading governs; the handful of settled cases were resolved by settlement rather than a reasoned decision on the merits, so no binding precedent exists. Their absence from the resolution process is precisely what sustains the plurality — they would settle the question if a case were litigated to judgment, but no party with standing has forced that outcome.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts_and_legislatures, excluded,
    institutional, civilizational, analytical, national).

% Document and critique the split between readings, publish risk-assessment frameworks for clients, and track enforcement patterns by community. They have no enforcement power themselves but their analysis shapes how adopters allocate legal budget under the uncertainty.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_scholars_and_open_source_lawyers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits a large, heterogeneous ecosystem of licensors and adopters to keep operating under one nominal license text without forcing a single, universally binding interpretation that would require either mass relicensing or mass non-compliance — coordination is achieved by tolerating plural, locally-enforced readings rather than by settling on one.
% TRANSFER_FUNCTION: Moves legal and transaction-cost risk from parties with enforcement capacity and legal resources (FSF-aligned enforcers acting within their reach, industry consortia acting within theirs) onto parties without it — small developers and redistributors who cannot predict, insure against, or contest whichever reading ends up applied to them.
% ABSENT_VOICES: Courts and legislatures that could resolve the scope question are structurally absent — no party with an interest in ambiguity has forced a case to a reasoned judgment, and no party harmed by the ambiguity (small developers) has standing or resources to do so either.
% DISAPPEARANCE_RATIONALE: If a definitive precedent settled the derivative-work boundary tomorrow, pragmatic hybrid adopters would lose their exploitable middle ground, industry integrators would face a real compliance cliff if the strong reading won or a real liability relief if the narrow reading won, small developers would gain a knowable compliance target, and FSF-aligned enforcement would either be vindicated wholesale or forced into a much narrower enforcement posture. Legal practice, licensing checklists, and vendor risk assessments across the ecosystem would all be rewritten.
% FOUNDING_PROBLEM: GPL Section 2(b)'s derivative-work language was drafted before dynamic linking, plugin architectures, and network-service composition existed in their current forms, leaving genuine textual underdetermination about which forms of code coupling trigger copyleft.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and open-source practitioners outside both the FSF community and industry consortia (e.g., independent open-source licensing counsel publishing risk-assessment literature) attest that the textual ambiguity is real and unresolved by courts, not merely asserted by either interpretive camp to suit its own enforcement interests.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored modestly (0.38, rising slowly) because the enforcement-vacuum constraint's primary cost is not direct rent extraction but a diffuse transaction-cost tax imposed on parties who must resolve ambiguity themselves — legal review, over-compliance, or unquantified risk-bearing. This is lower than either sibling reading's likely ε because it is a coordination-cost story, not a direct-transfer story, though it rises over time as more code-coupling architectures (containers, service meshes, WASM modules) create novel coupling forms the 1991-era text never anticipated, widening rather than narrowing the ambiguity. Suppression (0.42) reflects that neither camp can compel the other's adopters to conform — each enforces only within its own reach — so no single suppressive apparatus exists; what suppression exists is fragmented and localized. Theater ratio (0.30) captures that a meaningful share of 'compliance' activity in this space is legal-opinion theater — memos asserting a defensible position rather than genuine resolution of the underlying ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are agents with the resources to exploit or arbitrage the ambiguity: pragmatic hybrid adopters who choose their architecture and posture opportunistically, and large industry integrators whose legal departments can simply outlast challenge. Victims are agents who need a single defendable answer and cannot get one cheaply: small developers who must either over-comply or accept unbounded risk, and redistributors who inherit uncertainty from every upstream component without being able to resolve it themselves. The two enforcing agenda-setters (FSF-aligned enforcers, industry consortia) are themselves NOT simple beneficiaries — they invest real resources sustaining their respective readings — but they are structurally advantaged relative to powerless adopters because they at least control which reading applies within their own domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine textual underdetermination about code coupling at GPL's drafting — remains live; new coupling architectures continue to generate cases the text does not clearly address, so this is not a zombie mandate coasting on inertia after its problem resolved. What has drifted is not the founding problem's liveness but the resolution mechanism: courts were expected (eventually) to settle scope questions through litigation, but settlements have consistently mooted cases before judgment, so the vacuum has become a permanent structural feature rather than a transitional gap awaiting a ruling. Classifying this as tangled_rope rather than mountain prevents mislabeling a contestable, resource-dependent enforcement asymmetry as an inevitable natural fact about copyright law; classifying it as tangled_rope rather than pure snare prevents ignoring that plural interpretation genuinely does let a heterogeneous ecosystem keep functioning without a costly, potentially ecosystem-fracturing forced resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vacuum_permanence_vs_transitional_gap,
    'Is the enforcement vacuum a stable equilibrium that will persist indefinitely because no party benefits enough from litigating to judgment, or a transitional gap that a sufficiently high-stakes case will eventually close?',
    'Track whether any pending or future GPL derivative-work dispute proceeds to a reasoned appellate judgment rather than settling; a single binding appellate ruling on dynamic linking would substantially resolve this question.',
    'If permanent, this constraint should be understood as a stable structural feature of open-source governance requiring its own risk-management institutions; if transitional, current risk-allocation patterns are a temporary artifact that will be superseded once precedent forms, and long-term commentary and licensing guidance built around the vacuum will need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vacuum_permanence_vs_transitional_gap, empirical, 'Whether the enforcement vacuum is a stable or transitional structural feature.').

omega_variable(
    committer_structure_which_reading_operative_where,
    'This constraint is one reading (enforcement_vacuum_reading) of the gpl_copyleft_scope kernel; the sibling readings (strong_copyleft_reading, narrow_scope_reading) each assert a definite substantive derivative-work boundary. Where is the actual disagreement located — is it a genuine textual/doctrinal disagreement about copyright''s derivative-work test, or a disagreement about which community''s enforcement practice should be treated as authoritative?',
    'Compare doctrinal legal analysis (would a court applying ordinary derivative-work doctrine agree with either camp) against sociological analysis of enforcement patterns (which reading is actually enforced in which ecosystem, independent of doctrinal merit).',
    'If the disagreement is primarily doctrinal, courts resolving a test case would settle it regardless of enforcement patterns, and this enforcement_vacuum_reading would collapse into whichever sibling reading turns out doctrinally correct. If the disagreement is primarily about enforcement-community authority, a doctrinal ruling might not actually change practical outcomes in ecosystems the ruling''s plaintiff class cannot reach — the enforcement_vacuum_reading would persist even after nominal legal resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_which_reading_operative_where, conceptual, 'Whether the kernel''s three readings are separated by doctrine or by enforcement sociology.').

omega_variable(
    small_developer_coalition_capacity,
    'Could clarity-seeking small developers and downstream redistributors, who individually lack enforcement or arbitrage capacity, form a coalition (industry association, standardized compliance clearinghouse, collective legal defense fund) to reduce their transaction-cost burden without requiring judicial resolution?',
    'Examine whether analogous coordination has emerged in other underdetermined-licensing contexts (e.g., patent pools, open-source foundations offering compliance-as-a-service) and whether comparable structures exist or are forming for GPL scope risk specifically.',
    'If coalition capacity exists or is buildable, the victim-side transaction-cost burden this constraint measures could be substantially reduced without waiting on courts, suggesting this reading''s classification as tangled_rope (rather than snare) is durable — the coordination path remains genuinely open. If no such coalition is feasible, the asymmetry between resourced and unresourced adopters is more structurally locked in.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_developer_coalition_capacity, preference, 'Whether powerless adopters have a coalition path to reduce enforcement-vacuum transaction costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 25, 0.3).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This constraint is the third member of the gpl_copyleft_scope kernel family, alongside strong_copyleft_reading and narrow_scope_reading. Where those two readings each assert a definite substantive derivative-work boundary (and would each classify the resulting compliance regime differently), this reading asserts that the SOCIAL FACT of enforcement plurality is the operative constraint, independent of which substantive boundary is doctrinally correct. Its ε (0.38) sits below what either sibling reading would likely author for its own preferred regime's compliance burden, because this reading's cost is a transaction/uncertainty cost rather than a direct compliance-obligation cost. The three stories should be read together as a decomposition of the colloquial phrase 'GPL Section 2(b) scope' into structurally distinct claims per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
