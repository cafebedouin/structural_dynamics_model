% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: Near-Term AI Harms Governance Priority
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This constraint instantiates the near_term_harms_reading of the contested
 *   kernel ai_risk_governance_priority. It asserts that AI governance must
 *   prioritize demonstrated present harmsâalgorithmic bias, misinformation,
 *   labor displacement, and surveillanceâaffecting marginalized populations
 *   over speculative superintelligence scenarios. The governance framework
 *   coordinates genuine present-harm mitigation (fairness audits, regulatory
 *   oversight) while simultaneously creating an extractive dynamic:
 *   technology companies benefit from a narrow, manageable compliance
 *   surface, and marginalized communities remain structurally harmed despite
 *   nominal priority. The claim/metric gap is deliberate: claimed as
 *   tangled_rope because both coordination and extraction are structurally
 *   present, while metrics describe high extractiveness and substantial
 *   theater.
 *
 * KEY AGENTS:
 *   - global_south_populations: Primary target (powerless/trapped) â bear externalized algorithmic harms without proportional representation in governance forums.
 *   - marginalized_groups: Primary target (powerless/trapped) â subject to bias and surveillance nominally prioritized but inadequately addressed.
 *   - displaced_workers: Target (powerless/constrained) â bear labor market disruption without adequate transition support from the governance framework.
 *   - technology_companies: Structural beneficiary (institutional/arbitrage) â capture near-term regulatory processes and benefit from manageable compliance scope relative to existential-risk regimes.
 *   - fairness_audit_orgs: Secondary beneficiary (moderate/constrained) â receive funding and mandate from the governance framework but operate within industry-tolerable boundaries.
 *   - multilateral_governance_bodies: Agenda setter (institutional/constrained) â administer the priority framework with limited leverage over global technology capital.
 *   - x_risk_research_institutes: Excluded voice (organized/constrained) â structurally sidelined in resource allocation when present harms take priority.
 *   - civil_society_observers: Analytical observer (organized/analytical) â document present harms and advocate for this reading from outside the benefiting set.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.72).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.68).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "Near-Term AI Harms Governance Priority").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '924cf5b6-d915-4d8c-8006-f8485466017b').
narrative_ontology:cs_kernel_codification('924cf5b6-d915-4d8c-8006-f8485466017b', distributed).
narrative_ontology:cs_authority_grounding('924cf5b6-d915-4d8c-8006-f8485466017b', distributed).
narrative_ontology:cs_reading_relation('924cf5b6-d915-4d8c-8006-f8485466017b', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('924cf5b6-d915-4d8c-8006-f8485466017b', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('924cf5b6-d915-4d8c-8006-f8485466017b', foundational, demonstrated_present_harms_take_regulatory_priority).
narrative_ontology:cs_axiom_status(demonstrated_present_harms_take_regulatory_priority, holdable).
narrative_ontology:cs_axiom_grounding('924cf5b6-d915-4d8c-8006-f8485466017b', demonstrated_present_harms_take_regulatory_priority, empirically_contingent).
narrative_ontology:cs_axiom('924cf5b6-d915-4d8c-8006-f8485466017b', secondary, speculative_risks_cannot_override_known_injustices).
narrative_ontology:cs_axiom_status(speculative_risks_cannot_override_known_injustices, holdable).
narrative_ontology:cs_axiom_grounding('924cf5b6-d915-4d8c-8006-f8485466017b', speculative_risks_cannot_override_known_injustices, deontological).
narrative_ontology:cs_reference_frame('924cf5b6-d915-4d8c-8006-f8485466017b', present_harm_justice_priority).
narrative_ontology:cs_drift_state('924cf5b6-d915-4d8c-8006-f8485466017b', generative_ai_scale_up_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('924cf5b6-d915-4d8c-8006-f8485466017b', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, fairness_audit_orgs).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the externalized costs of AI systems trained on extracted data and deployed without local governance input; subject to algorithmic discrimination, surveillance, and labor exploitation with minimal recourse in international standards bodies where priorities are set.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, immediate, trapped, global).

% Face demonstrated algorithmic harms including discriminatory classification, misinformation targeting, and surveillance that the governance framework nominally prioritizes but inadequately addresses due to structural power imbalances in implementation and enforcement.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups, payer,
    powerless, immediate, trapped, national).

% Bear labor market disruption from automation without adequate transition support or binding retraining mandates; the governance priority names their harms but resource flows and protective mechanisms remain insufficient relative to scale.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, displaced_workers, payer,
    powerless, immediate, constrained, national).

% Benefit from governance attention focused on narrow, manageable present-harm compliance rather than structural constraints or existential-risk governance that might threaten core business models; actively shape regulatory processes through lobbying and standards-setting to maintain this equilibrium.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Receive funding and mandate from governance frameworks prioritizing present harms; conduct genuine bias and fairness assessments but operate within boundaries tolerable to industry sponsors and contracting bodies.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, fairness_audit_orgs, beneficiary,
    moderate, biographical, constrained, national).

% Administer the governance priority framework, convening standards bodies and issuing guidelines; possess limited enforcement leverage against global technology capital and depend on member-state cooperation for binding action.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, multilateral_governance_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Structurally sidelined in priority-setting forums when present harms dominate the governance agenda; argue that long-term catastrophic risks require sustained attention and funding that near-term prioritization displaces.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, x_risk_research_institutes, excluded,
    organized, civilizational, constrained, global).

% Document present harms and advocate for marginalized community inclusion in AI governance; provide external corroboration of the founding problem from outside the benefiting audit industry and technology sector.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_observers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addressing demonstrated algorithmic harms including bias, misinformation, labor displacement, and surveillance through institutionalized governance frameworks, fairness audits, regulatory oversight, and procedural inclusion of affected communities.
% TRANSFER_FUNCTION: Moves regulatory attention, research funding, and compliance obligations from speculative long-term risk assessment toward present-harm mitigation and audit infrastructure; compliance costs flow to deployed AI system operators while marginalized communities receive nominal protection and procedural inclusion.
% ABSENT_VOICES: Existential risk researchers and long-termist advocates are structurally sidelined in priority-setting forums; Global South governance delegations are underrepresented in standards bodies; affected workers lack binding bargaining power in automation governance.
% DISAPPEARANCE_RATIONALE: If the near-term priority constraint vanished, regulatory attention and funding would shift toward frontier model safety and speculative risk research, present-harm audit infrastructures would lose mandate and collapse, and marginalized communities would lose their current procedural foothold in AI governanceâthough many would argue the practical protection was already insufficient to prevent rearrangement of harms.
% FOUNDING_PROBLEM: AI systems were being deployed at scale causing documented harms of bias, discrimination, surveillance, and labor displacement without adequate accountability mechanisms, oversight, or governance attention proportional to affected populations.
% FOUNDING_PROBLEM_CORROBORATION: Documented by civil society organizations, affected communities, and academic researchers outside the benefiting audit industry; corroborated by UN human rights rapporteurs and Global South advocacy networks attesting that present harms continue unabated despite governance prioritization.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the governance framework channels resources toward narrow, performative compliance (bias audits, disclosure) without altering underlying power asymmetries. Suppression (0.68) reflects active marginalization of existential-risk and alternative framings in priority-setting forums. Theater ratio (0.45) indicates substantial performative activity relative to structural interventionâaudits and guidelines substitute for binding constraints. Accessibility collapse (0.50) shows alternative framings remain conceptually available but politically collapsed within this priority regime. Resistance (0.55) from x-risk advocates and industry lobbying maintains contested dynamics. Temporal measurements show monotonic increases across the interval as the framework institutionalizes and compliance capture deepens.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (technology companies) experiences this constraint as a navigable compliance environment that preempts more threatening governance. The victim seats (global south populations, marginalized groups, displaced workers) experience it as performative prioritization that performs protection without delivering structural safety. The fairness audit seat experiences it as genuine, constrained work that delivers partial coordination. The engine computes these divergent seat types from the structural dataâno single authored claim resolves the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies are declared beneficiaries (low directionality, subsidized by manageable compliance). Global south populations, marginalized groups, and displaced workers are declared victims (high directionality, extraction concentrated on powerless agents at large scope). Fairness audit organizations are secondary beneficiaries with constrained exit. Multilateral governance bodies administer enforcement. The structural asymmetry is stark: the same framework that nominally protects the powerless delivers concentrated benefits to the powerful.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents misreading this constraint as either pure coordination (rope/scaffold) or pure extraction (snare). The genuine coordination functionâpresent-harm audits, bias mitigation frameworks, and procedural inclusionâis real and documented. However, the asymmetric extractionâcontinued structural harm to marginalized communities, regulatory capture by technology companies, and suppression of alternativesâis equally real. A scaffold classification would fail because there is no sunset clause and the justification is the steady state, not transition. A snare classification would miss the genuine mitigation that does occur. Tangled_rope captures the hybrid accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_ambiguity,
    'Does the present-harms governance framework structurally benefit technology companies by creating a narrow, manageable compliance surface that avoids deeper constraints on business models?',
    'Comparative analysis of regulatory stringency across jurisdictions with near-term versus existential-risk prioritization; measuring whether present-harm regimes produce more or less structural industry constraint.',
    'If capture is dominant, effective extraction is higher than nominal and the constraint leans toward snare; if genuine constraint dominates, it remains tangled_rope with real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_ambiguity, empirical, 'Whether present-harm governance is captured by industry interests.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of existential-risk and alternative framings structural (resource allocation and forum exclusion) or internalized (narrative dominance making alternatives seem illegitimate)?',
    'Track whether existential-risk research funding and representation recovers when structural barriers are removed; if not, internalization is present.',
    'Internalized suppression increases effective suppression beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of alternative governance framings.').

omega_variable(
    near_term_vs_xrisk_zero_sum,
    'Are present-harm mitigation and existential risk preparation genuinely zero-sum in resource allocation, or can they be structurally separated?',
    'Economic analysis of AI governance budgets showing fungibility or separability of present-harm and long-term risk funding streams.',
    'If zero-sum, this reading genuinely extracts from x-risk institutes; if separable, the extraction is partially illusory and the constraint is less asymmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(near_term_vs_xrisk_zero_sum, conceptual, 'Whether resource competition between framings is structurally necessary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(ai_r_tr_t40, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(ai_r_tr_t50, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(ai_r_be_t40, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(ai_r_be_t50, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(ai_r_su_t40, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(ai_r_su_t50, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_risk_governance_priority kernel. It is linked to sibling readings that instantiate competing priority claims from the same contested governance space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
