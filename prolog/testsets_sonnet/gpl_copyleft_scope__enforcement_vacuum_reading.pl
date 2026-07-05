% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: GPL Copyleft Scope — Enforcement Vacuum (Interpretive Plurality)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   Software projects and companies operate under GPL-family licenses whose
 *   Section 2(b) scope — what counts as a 'derivative work' triggering
 *   copyleft obligations for combined, linked, or aggregated code — has never
 *   been definitively settled by a controlling court in a way that resolves
 *   the question across jurisdictions and technical architectures. In the
 *   absence of that ruling, two readings circulate as live legal positions:
 *   the FSF's traditionally broader reading (strong_copyleft_reading) and
 *   industry's traditionally narrower reading (narrow_scope_reading). This
 *   story is about the enforcement-vacuum structure itself: the fact that no
 *   one has been forced to choose, and that the choice that actually governs
 *   a given piece of software depends on which interpretive community can
 *   practically enforce its reading in that specific ecosystem.
 *
 * KEY AGENTS:
 *   - fsf_aligned_projects: enforces strong reading locally, no capacity to bind industry ecosystems
 *   - sophisticated_corporate_adopters: exploits the ambiguity as strategic optionality
 *   - dual_licensing_vendors: sells resolution of uncertainty their own vendor identity depends on preserving
 *   - small_downstream_developers: bears the transaction cost of guessing under both readings
 *   - clarity_seeking_startups: pays real legal fees for a definite answer the market cannot supply
 *   - industry_dominated_ecosystems: enforces narrow reading locally through norm-setting rather than litigation
 *   - courts_and_legislatures: structurally absent because settlement dynamics prevent precedent formation
 *   - software_freedom_conservancy_and_enforcers: documents the plurality without power to resolve it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.32).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.28).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Enforcement Vacuum (Interpretive Plurality)").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '3357a857-2a79-4824-8e5e-9628e4cd4822').
narrative_ontology:cs_kernel_codification('3357a857-2a79-4824-8e5e-9628e4cd4822', distributed).
narrative_ontology:cs_authority_grounding('3357a857-2a79-4824-8e5e-9628e4cd4822', distributed).
narrative_ontology:cs_reading_relation('3357a857-2a79-4824-8e5e-9628e4cd4822', gpl_copyleft_scope__strong_copyleft_reading, influences).
narrative_ontology:cs_reading_relation('3357a857-2a79-4824-8e5e-9628e4cd4822', gpl_copyleft_scope__narrow_scope_reading, influences).
narrative_ontology:cs_axiom('3357a857-2a79-4824-8e5e-9628e4cd4822', foundational, coexistence_without_adjudication_is_licensed_plurality).
narrative_ontology:cs_axiom_status(coexistence_without_adjudication_is_licensed_plurality, holdable).
narrative_ontology:cs_axiom_grounding('3357a857-2a79-4824-8e5e-9628e4cd4822', coexistence_without_adjudication_is_licensed_plurality, conventional).
narrative_ontology:cs_axiom('3357a857-2a79-4824-8e5e-9628e4cd4822', foundational, enforcement_capacity_not_legal_merit_determines_operative_reading).
narrative_ontology:cs_axiom_status(enforcement_capacity_not_legal_merit_determines_operative_reading, holdable).
narrative_ontology:cs_axiom_grounding('3357a857-2a79-4824-8e5e-9628e4cd4822', enforcement_capacity_not_legal_merit_determines_operative_reading, empirically_contingent).
narrative_ontology:cs_reference_frame('3357a857-2a79-4824-8e5e-9628e4cd4822', gpl_v2_drafting_era_derivative_work_doctrine).
narrative_ontology:cs_drift_state('3357a857-2a79-4824-8e5e-9628e4cd4822', contemporary_plugin_and_containerization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3357a857-2a79-4824-8e5e-9628e4cd4822', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, sophisticated_corporate_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, dual_licensing_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, small_downstream_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_startups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__enforcement_vacuum_reading, copyleft_scope_remains_judicially_unsettled).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the strong-copyleft reading within its own ecosystem through compliance letters, community pressure, and occasional litigation threats. Has enforcement capacity where it maintains community standing but no capacity to bind industry-dominated ecosystems that never accepted its interpretive authority. Benefits from the ambiguity persisting because it lets FSF-aligned communities enforce the broad reading locally without a definitive adverse ruling foreclosing that reading everywhere.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, agenda_setter,
    organized, civilizational, arbitrage, global).

% Maintains in-house counsel and licensing specialists who read the ambiguity as room to maneuver: adopts the narrow reading where profitable, cites the strong reading when it suits a competitor dispute, and structures architecture (plugin boundaries, IPC, dynamic linking) to sit in the interpretive gray zone deliberately. The uncertainty is a resource it can afford to exploit and others cannot.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, sophisticated_corporate_adopters, beneficiary,
    institutional, generational, arbitrage, global).

% Sells proprietary licenses specifically because customers fear the strong-copyleft reading might apply to their combined work. The vacuum is the vendor's product: if courts settled the scope question definitively in either direction, either the fear disappears (narrow ruling) or compliance becomes non-negotiable and cheaper to just comply with (strong ruling) — both outcomes erode the vendor's arbitrage niche.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, dual_licensing_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Cannot afford to commission a legal opinion on whether their linking architecture triggers copyleft obligations. Must guess, and guesses wrong in both directions: some comply unnecessarily (giving away code that narrow-scope reading would not have required), others under-comply and face takedown or compliance demands years later when a well-funded enforcer picks their specific case as a test.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, small_downstream_developers, payer,
    powerless, biographical, constrained, national).

% Needs a definite answer for due diligence, acquisition, or investment purposes and cannot get one — burns real transaction costs (outside counsel opinions, defensive re-architecture, license audits) precisely because the constraint itself is 'no one knows which reading a court would apply here.' Pays in legal fees and delayed deals for uncertainty it did not create and cannot resolve unilaterally.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_startups, payer,
    moderate, biographical, constrained, national).

% Operates de facto under the narrow-scope reading within its own sphere (permissive linking norms, foundation-blessed interpretations, corporate CLA structures) and has the resources to make that reading stick locally through norm-setting, model licensing language, and legal defense funds — without needing a court to ratify it. Its enforcement capacity substitutes for judicial precedent within its domain.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, beneficiary).

% Has not been forced to rule definitively on GPL Section 2(b) scope for dynamic linking and aggregation because most disputes settle before trial (defendants with resources prefer negotiated compliance or licensing to a published adverse ruling; defendants without resources cannot afford to litigate to a ruling at all). Their absence from the conversation is structural, not incidental — the vacuum persists partly because no party with both standing and resources wants a precedent-setting loss.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts_and_legislatures, excluded,
    institutional, civilizational, analytical, national).

% Documents compliance patterns across both interpretive communities, brings occasional enforcement actions, and tracks where practice diverges from either reading. Has genuine analytical visibility into the plurality but limited capacity to resolve it — enforcement actions settle individual cases without producing precedent binding the other community.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, software_freedom_conservancy_and_enforcers, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, software_freedom_conservancy_and_enforcers, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large, heterogeneous population of licensors and licensees around a shared license text without requiring universal agreement on its most contested clause's boundary — each interpretive community can proceed coherently within its own enforcement sphere rather than the entire ecosystem stalling pending a single global adjudication.
% TRANSFER_FUNCTION: Moves legal-risk-bearing capacity from parties who can absorb ambiguity (institutional adopters, dual-licensing vendors with counsel and architecture flexibility) to parties who cannot (small developers and startups who must guess and bear the downside of guessing wrong), while moving strategic optionality from a settled-law baseline to whichever community has local enforcement capacity.
% ABSENT_VOICES: Courts and legislatures that could resolve the scope question are structurally absent — not excluded by rule, but by the settlement dynamics that keep well-resourced disputes from reaching a published ruling. Small developers who bear the transaction-cost burden of the ambiguity have no forum in which to demand resolution; they are not organized enough to litigate a test case and not represented in the standards conversations where informal norms harden.
% DISAPPEARANCE_RATIONALE: If a definitive judicial ruling settled the Section 2(b) scope question tomorrow — in either direction — dual-licensing vendors lose their core value proposition, sophisticated adopters lose the architectural arbitrage the ambiguity currently permits, FSF-aligned enforcement either gains a powerful precedent or loses its broadest enforcement claims, and small developers gain a clear compliance target that eliminates most of their current legal uncertainty costs. The interpretive-plurality structure itself, not just enforcement intensity, would disappear.
% FOUNDING_PROBLEM: GPL Section 2(b)'s language ('based on the Program,' derivative-work coupling) was drafted in an era before plugin architectures, dynamic linking, containerization, and network-service composition existed in their current forms; the clause was meant to prevent proprietary capture of copylefted code but was never tested against the full range of technical coupling mechanisms that emerged afterward.
% FOUNDING_PROBLEM_CORROBORATION: FSF-aligned commentators attest the founding problem (preventing proprietary capture via technical coupling) remains fully live and the strong reading is the correct extension of original intent. Industry legal departments and independent academic license scholars (e.g. commentary from software law academics outside both the FSF and major corporate legal departments) attest the founding problem was narrower — preventing wholesale proprietary redistribution of copylefted source — and that extending it to all forms of dynamic coupling is a later interpretive expansion contested even among original drafters' contemporaries. No single source uncontested by an interested party corroborates either reading as definitively the original design intent.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.32 at interval end) because the enforcement-vacuum structure itself extracts primarily through elevated transaction costs on clarity-seeking parties, not through direct rent capture — this is a low-epsilon tangled_rope as specified, not a high-extraction snare. Suppression is low-moderate (0.28) because no single party actively suppresses alternatives; rather, the absence of a forcing mechanism (a binding precedent) is what sustains the plurality. Theater ratio is modest (0.22) and rising slowly: some genuine compliance activity, some performative signaling by both interpretive communities asserting their reading is 'the' reading when neither has been adjudicated. Accessibility collapse is moderate (0.35) — alternatives (seeking declaratory judgment, adopting more permissive licenses, restructuring architecture to avoid the ambiguous zone) remain genuinely available, just costly. Resistance is elevated (0.55) because both interpretive communities actively contest the other's claim to authority, and clarity-seeking parties actively resist the uncertainty by burning resources to route around it.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a sophisticated adopter with in-house counsel, the vacuum looks like a rope: genuine flexibility, no one forced into a single costly reading, room to negotiate architecture case by case. From the seat of a small developer or clarity-seeking startup, the same vacuum looks closer to extractive: real money and real risk absorbed for a legal question that a court could resolve but that no well-resourced party wants resolved against itself, so it never gets resolved. The tangled_rope classification holds both seats' experience as structurally real rather than adjudicating between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Sophisticated corporate adopters and dual-licensing vendors sit near the beneficiary end: they have the resources to treat the ambiguity as optionality (architect around it, insure against it, sell resolution of it) rather than as risk they must absorb. Small downstream developers and clarity-seeking startups sit near the target end: they cannot afford legal opinions, cannot architect defensively at scale, and pay in unnecessary compliance, under-compliance risk, or transaction costs for a definite answer the ambiguity structurally prevents anyone from supplying cheaply. FSF-aligned projects and industry-dominated ecosystems both hold agenda_setter roles because each enforces its preferred reading within its own sphere — they are not victims of the vacuum, they are its co-administrators, each benefiting from local enforcement capacity that the other cannot override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing proprietary capture of copylefted code — remains partially live, which is why this is not simply a piton. But the enforcement-vacuum structure has itself become semi-institutionalized: dual-licensing business models, corporate legal risk practices, and FSF enforcement strategy have all adapted to expect the ambiguity to persist rather than to push toward resolving it. Some actors now have an interest in the vacuum's continuation that is independent of the original copyleft-protection purpose — that is the seam where genuine coordination (a workable license ecosystem without universal litigation) shades into extraction (uncertainty monetized by those positioned to exploit it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_vacuum_is_the_kernel_contest_itself,
    'Is the enforcement-vacuum reading a genuinely distinct constraint from strong_copyleft_reading and narrow_scope_reading, or is it simply a description of the fact that those two readings are contested — i.e., is it a meta-level observation rather than a third structurally independent constraint?',
    'Test whether the enforcement-vacuum reading''s beneficiary/victim structure and ε would change if either sibling reading were judicially settled tomorrow. If settling either sibling reading collapses the enforcement-vacuum reading''s distinct beneficiaries (dual-licensing vendors, arbitrage-seeking adopters) to zero, that confirms enforcement-vacuum is parasitic on the coexistence of the other two rather than an independent constraint with its own stable ε.',
    'If parasitic, this story should be understood as documenting a second-order effect of the underlying kernel contest rather than a fourth sibling with true structural independence — but the ε-invariance test is satisfied regardless, since this story''s ε (transaction-cost extraction from uncertainty) is measurably different from either sibling''s ε (extraction from a settled substantive reading), which is the operational test the framework applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vacuum_is_the_kernel_contest_itself, conceptual, 'Whether the enforcement-vacuum reading is a genuine third constraint or a meta-level restatement of the strong/narrow contest.').

omega_variable(
    which_community_actually_has_enforcement_capacity_where,
    'In any given technical context (a specific plugin architecture, a specific dynamic-linking pattern, a specific industry sector), which interpretive community — FSF-aligned enforcers or industry-dominated ecosystem norms — actually has practical enforcement capacity, and how stable is that allocation over time?',
    'Empirical survey of actual compliance enforcement actions (Conservancy litigation history, corporate CLA enforcement patterns, informal community pressure campaigns) mapped against technical architecture types and industry sectors, tracked over a multi-year window to detect whether capacity is shifting toward one community.',
    'If FSF-aligned capacity is systematically weakening relative to industry-dominated norms (fewer resources, fewer test cases, declining community enforcement participation), the practical effect of the vacuum increasingly resembles the narrow_scope_reading regardless of what a court might eventually hold — the enforcement-vacuum reading would be drifting toward absorption by narrow_scope_reading as a matter of practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_community_actually_has_enforcement_capacity_where, empirical, 'Whether enforcement capacity between the two interpretive communities is stable, or drifting toward one reading''s de facto dominance.').

omega_variable(
    settlement_avoidance_as_deliberate_strategy,
    'Do well-resourced parties on both sides of copyleft disputes deliberately settle before trial specifically to avoid creating precedent, i.e., is the absence of judicial resolution a strategic outcome rather than mere happenstance?',
    'Analysis of settlement terms and timing in GPL enforcement disputes that reached litigation but settled before judgment; interviews with counsel on both sides about settlement motivations relative to precedent risk.',
    'If settlement avoidance is deliberate and coordinated (even tacitly) across both interpretive communities, the enforcement vacuum is not a natural byproduct of legal complexity but an actively maintained structural feature — which would push this constraint''s classification toward higher requires_active_enforcement weight and potentially toward a different type than tangled_rope if the deliberate maintenance is found to be primarily extractive rather than coordination-preserving.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settlement_avoidance_as_deliberate_strategy, empirical, 'Whether the lack of precedent is incidental or a jointly (if tacitly) maintained strategic outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 25, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 25, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes the natural-language concept 'GPL Section 2(b) derivative work scope' into a third member of a constraint family alongside strong_copyleft_reading and narrow_scope_reading. Those two siblings each claim a specific substantive legal boundary is correct and measure the extraction that flows FROM that substantive reading being enforced. This story measures a structurally different phenomenon: the extraction that flows from NEITHER reading being definitively settled, which produces its own beneficiaries (parties who profit from ambiguity) and victims (parties who need certainty and cannot get it) distinct from either sibling's beneficiary/victim sets. All three stories must be read together to understand the full kernel; none alone captures it, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
