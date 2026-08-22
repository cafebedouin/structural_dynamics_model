% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Severity Carve-Out: Critical Systems Exemption
 *   domain: technology_law/consumer_protection/product_liability
 *
 * SUMMARY:
 *   This constraint instantiates the 'severity carve-out' reading of the
 *   contested beta-designation kernel: a legal and regulatory doctrine that
 *   categorically prohibits software vendors from using beta designation to
 *   disclaim liability when code is deployed in life-safety, financial, or
 *   other critical-infrastructure domains. The reading asserts that
 *   domain-specific physical constraints (the irreversibility of harm in
 *   medical and financial systems) override the general contractual freedom
 *   to allocate liability via beta disclosure. This is one of three
 *   structurally distinct readings of the beta-designation kernel; the other
 *   readings (expansive shield, narrow warning) have different ε values,
 *   different victim/beneficiary structures, and are authored as separate
 *   constraint stories. The claim/metric gap is deliberate and models the
 *   actual doctrinal contest: the carve-out is claimed as tangled_rope
 *   (genuine coordination against catastrophic externalization, with
 *   mandatory vendor participation) while the metrics describe moderate
 *   extraction and low suppression — lower than a pure snare because the
 *   coordination necessity is real and the beneficiary set (vulnerable
 *   populations, regulatory authorities) genuinely prevents harm rather than
 *   purely extracting rents.
 *
 * KEY AGENTS:
 *   - Safety advocates (organized coalitions): establish and defend the carve-out via litigation, testimony, standards-setting, and knowledge production about failure modes
 *   - Regulatory authorities (FDA, SEC, financial and power-grid regulators): enforce the carve-out via device/system clearance, compliance audits, and penalty authority
 *   - Vulnerable populations (patients, elderly, low-income users, Global South populations): benefit materially from liability protection they cannot negotiate for themselves
 *   - Established vendors in critical domains: bear compliance costs but have integrated them into business models; can exit via divestment or can comply
 *   - Venture-backed startups in health/fintech: identity-locked to rapid iteration; forced to choose between expensive compliance, delayed entry, or domain exit
 *   - Industry advocates for shield expansion (venture capital, software associations): oppose the carve-out via legislative campaigns and litigation but are excluded from the regulatory processes that codified it
 *   - Product liability insurers: observe and price the carve-out's effect; benefit from clarity but absorb higher claims
 *   - Comparative jurisdictions (EU, UK, Japan regulators): observe outcomes and adjust their own doctrinal choices, creating downstream influence on global norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.38).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.22).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Severity Carve-Out: Critical Systems Exemption").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/consumer_protection/product_liability").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '3aab2a3d-79ee-415e-b574-b32d3176b869').
narrative_ontology:cs_kernel_codification('3aab2a3d-79ee-415e-b574-b32d3176b869', formalized).
narrative_ontology:cs_authority_grounding('3aab2a3d-79ee-415e-b574-b32d3176b869', extraction).
narrative_ontology:cs_interpretation_layer_present('3aab2a3d-79ee-415e-b574-b32d3176b869').
narrative_ontology:cs_reading_relation('3aab2a3d-79ee-415e-b574-b32d3176b869', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('3aab2a3d-79ee-415e-b574-b32d3176b869', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('3aab2a3d-79ee-415e-b574-b32d3176b869', foundational, harm_severity_overrides_contractual_freedom).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_freedom, holdable).
narrative_ontology:cs_axiom_grounding('3aab2a3d-79ee-415e-b574-b32d3176b869', harm_severity_overrides_contractual_freedom, deontological).
narrative_ontology:cs_axiom('3aab2a3d-79ee-415e-b574-b32d3176b869', foundational, domain_specific_liability_carve_out).
narrative_ontology:cs_axiom_status(domain_specific_liability_carve_out, holdable).
narrative_ontology:cs_axiom_grounding('3aab2a3d-79ee-415e-b574-b32d3176b869', domain_specific_liability_carve_out, empirically_contingent).
narrative_ontology:cs_reference_frame('3aab2a3d-79ee-415e-b574-b32d3176b869', categorical_beta_prohibition_critical_domains).
narrative_ontology:cs_drift_state('3aab2a3d-79ee-415e-b574-b32d3176b869', contemporary_health_finance_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3aab2a3d-79ee-415e-b574-b32d3176b869', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, safety_advocates).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors_in_critical_domains).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, venture_funded_startups_healthcare_finance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, venture_startups_health_finance).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, established_vendors_critical_domains).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, venture_startups_health_finance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized coalitions (patient safety organizations, consumer advocates, some academic liability scholars) that advance the position that beta designation cannot shield vendors from liability in life-safety and critical domains. They testify in regulatory proceedings, file amicus briefs in litigation, publish research documenting failure patterns, and coordinate international standard-setting. Their authority derives from demonstrated expertise in harm identification and risk communication.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, safety_advocates, agenda_setter,
    organized, civilizational, analytical, global).

% Domain-specific regulators (FDA for medical devices, SEC for trading systems, power-grid authorities) mandated by statute to prevent catastrophic harm. They enforce the carve-out by refusing to approve or permit critical systems operating under beta designation, revoking clearances for vendors who attempt liability disclaimers on known defects, and prosecuting material misrepresentations. Their enforcement carries legal penalties and reputational consequences.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Patients using implantable medical devices, elderly users of financial services, populations in low-income countries with limited insurance. They depend on critical systems they cannot choose or negotiate with vendors about. The carve-out prevents vendors from disclaiming liability for failures they cause, but the users cannot perceive or exercise this protection directly — it works through upstream vendor decisions.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% Large, established software vendors in medical devices, financial infrastructure, and aerospace who have integrated carve-out compliance into their business models. They maintain insurance, conduct comprehensive premarket testing, and employ regulatory-affairs staff. The carve-out's cost is predictable and priced into their margins. They can exit by divesting critical-domain units or can comply — both are expensive but calculable.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, established_vendors_critical_domains, payer,
    powerful, generational, mobile, global).

% Early-stage ventures in healthcare tech and fintech whose founding mission is rapid iteration in regulated domains. The carve-out prevents them from using beta designation as a liability escape hatch, forcing them to carry insurance, undergo premarket review, or delay market entry until product maturity. Many are funded on a thesis of 'move fast and break things' — the carve-out is incompatible with that identity. Those that stay absorb expensive compliance; those that exit the critical domain lose their market thesis.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, venture_startups_health_finance, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, venture_startups_health_finance, beneficiary).

% Software industry groups and venture capital organizations opposing the carve-out via legislative campaigns and litigation. They argue the carve-out stifles innovation and imposes compliance barriers that favor incumbents. They are excluded from the regulatory proceedings and advocacy coalitions that codified the carve-out, though they mount ongoing legal and political opposition. Their exclusion is structural: they have opposed the carve-out since its inception and are not admitted to regulatory rulemaking by consensus.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, industry_advocates_shield_expansion, excluded,
    organized, biographical, trapped, global).

% Insurance carriers underwriting errors-and-omissions and product liability for software vendors. They observe and price the carve-out: vendors in critical domains face higher premiums because they cannot disclaim liability via beta, increasing insurable exposure. Insurers benefit from clarity and risk quantification but absorb higher claim frequencies from vendors who previously used beta as a liability hedge.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, product_liability_insurers, observer,
    institutional, generational, constrained, global).

% EU, UK, Japan, and other regulatory bodies observing how the carve-out affects vendor behavior, entry timing, and safety outcomes. They use this evidence to inform their own doctrinal choices. The carve-out's influence on international standard-setting creates downstream pressure on the global norm and shapes what constitutes 'reasonable care' in critical-domain software.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, comparative_regulatory_regimes, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents cost-shifting of catastrophic harm from vendors to end-users and taxpayers in domains where failure consequences are irreversible or systemic. Coordinates expected safety investment across vendors by denying the liability-waiver exit and forcing vendors to internalize expected damages.
% TRANSFER_FUNCTION: Transfers expected liability from powerless end-users (patients, elderly, low-income populations) and taxpayers (who absorb systemic failures) to vendors who deploy code in critical domains. The mechanism is denial of beta as a liability-disclaimer vehicle: vendors must absorb expected damages via insurance, higher testing spend, or delayed market entry.
% ABSENT_VOICES: Vendors in critical domains who are prohibited from participating in the regulatory rulemaking that constrains them; startup founders whose business model depends on rapid iteration under beta shields; end-users in jurisdictions with weak consumer protections who are forced to pay higher prices because vendors price compliance costs into products; countries in the Global South where the carve-out is enforced by treaty pressure but not politically legitimated by local rulemaking.
% DISAPPEARANCE_RATIONALE: If the carve-out vanished overnight, vendors in critical domains would revert to beta designation and broad liability disclaimers within weeks. Insurance carriers would exit or raise premiums drastically. End-users would absorb uncompensated failures, and regulatory agencies would face political pressure to rebuild the carve-out or mandate premarket review as a substitute. The carve-out prevents a cost-shifting equilibrium; its removal triggers rapid return to that equilibrium and downstream political conflict.
% FOUNDING_PROBLEM: Early 2000s–2010s: vendors deployed minimally-tested code in medical devices, financial systems, and critical infrastructure under blanket beta-period liability waivers; high-profile failures (Therac-25 class device incidents, trading-system outages, power-grid vulnerabilities) revealed identifiable negligence with uncompensated catastrophic harm to patients and end-users; regulatory bodies lacked tools to override contractual liability disclaimers.
% FOUNDING_PROBLEM_CORROBORATION: FDA regulatory records and device-failure case studies (insulin-pump firmware defects, pacemaker software vulnerabilities) document the pattern. SEC enforcement actions against trading-system vendors provide financial-domain evidence. Academic liability scholarship and Government Accountability Office (GAO) reports corroborate the failure scope. Venture capital and software-industry groups contest the frequency, attributing cases to isolated bad actors and bad oversight; they do not contest the existence of the pattern, only its generality.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).
:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at interval end, rising from 0.28 over 25 years) reflects the carve-out's real compliance costs on vendors — they must absorb expected liability via insurance, testing, or delayed entry — but the extraction is justified by a genuine coordination problem (preventing cost-shifting to powerless end-users in critical domains). The measurement series shows extraction rising steeply early (0–10 years) as the carve-out is codified and enforcement machinery is built, then plateauing (15–25 years) as compliance becomes normal practice and vendors price it into expectations. Suppression is low (0.22) because vendors have exit options: they can comply (expensive but calculable), they can shift to non-critical domains, or they can litigate. The barrier is high but not opaque or identity-fusing for established vendors; for startups it is identity-locking because their founding thesis is incompatible with the carve-out's constraints. Theater is very low (0.18) because enforcement is straightforward regulatory gatekeeping, not performative activity — FDA does not approve untested medical software under beta designation; regulators simply deny clearance or revoke permits. There is minimal theater except in the political realm (the ongoing legislative battles and litigation that keep the carve-out visible despite its institutional solidity). The claim (tangled_rope) reflects the tension: genuine coordination necessity + mandatory vendor participation for the beneficiary set, but the extraction intensity and enforcement burden are lower than a pure snare because the coordination solves a real coordination problem (preventing uncompensated catastrophic harm) rather than purely extracting rents.
 *
 * PERSPECTIVAL GAP:
 *   From the safety-advocate and regulatory seat (the author's seat for the claim): the constraint is rope-like (genuine coordination against harm externalization) with mandatory vendor participation justified by the severity of failure consequences. From the established vendor seat: the constraint is routine rope (compliance is expensive but predictable and priced in). From the startup vendor seat: the constraint is snare-like or near-snare (forced internalization of expected liability on an asymmetric cost basis; identity-locked to domain; no exit except failure or strategic pivot). From the powerless end-user seat: the constraint is mostly invisible (the user depends on vendor compliance decisions upstream) but materially protective. The engine's per-seat calculation should reveal this: safety advocates compute to beneficiary (d ~0.08); regulators compute to beneficiary-agenda-setter (d ~0.15); established vendors compute to moderate payer (d ~0.65–0.75); startups compute to high-target (d ~0.85–0.95); end-users compute to slight beneficiary (d ~0.25) despite powerlessness. The perspectival gap is structural: the constraint looks like coordination to the seats that set it, like extraction to the seats that are forced to internalize costs, and like invisible protection to the seats that benefit but cannot perceive the mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety advocates and regulatory authorities are beneficiaries (d near 0.0–0.2): they impose the constraint, benefit from its operation (harm prevention), and bear minimal direct costs. The directionality override for 'organized' power_atom to d=0.08 reflects the safety-advocate coalition's role as beneficiary-agenda-setter: they set the agenda and benefit, but they are not an institution with captured rents — their benefit is the public good of harm prevention. Vulnerable populations are slight beneficiaries (d ~0.15–0.25) despite powerlessness: they benefit materially from liability protection but do not control the constraint; they cannot exit it (they depend on critical systems) but the constraint's operation is in their interest. Established vendors are moderate-to-high payers (d ~0.65–0.75): they bear compliance costs but have negotiating power and can price costs into margins. Venture startups are high-target victims (d ~0.85–0.95): they are identity-locked (cannot exit without abandoning their domain thesis), bear asymmetric costs, and lack the negotiating power and insurance access of established vendors. The industry advocates opposing the carve-out are NOT victims (they do not operate under the carve-out, they oppose it) and NOT beneficiaries (they do not benefit from its operation); they are excluded from the rulemaking process. The structural derivation (beneficiaries, victims, exit options, power distribution) correctly captures these relationships without needing overrides. The override for organized advocates (d=0.08) is used only to ensure the beneficiary coalition's low directionality is explicit in the Prolog output, not derived from a default.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early 2000s–2010s: untested code deployed under blanket beta waivers in critical domains, externality of catastrophic uncompensated harm) is LIVE at the measurement intervals 0–25. Enforcement has not become theatrical: regulatory gatekeeping still actively denies approvals for vendors attempting beta designation in critical domains, and the enforcement burden remains real. The carve-out persists because (1) the problem it solves is live, (2) the coordination necessity is permanent (vendors will always have incentives to externalize liability if permitted), and (3) enforcement is not declining. This is NOT mandatrophy. However, there is a secondary question about doctrinal substitution (is the carve-out being replaced by a finer-grained liability-allocation regime that accomplishes the same safety goal with less enforcement overhead?) — this belongs in an omega variable and is not yet resolved empirically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_boundary_definition,
    'What constitutes a ''critical domain'' or ''life-safety system'' subject to the carve-out? Where is the boundary between systems whose beta status must be prohibited and systems where beta disclosure suffices?',
    'Regulatory rulemakings, case-law evolution, and international standard-setting bodies (ISO, IEC, FDA guidance documents) that define domain lists and harm-severity thresholds. A system crosses the carve-out boundary if its failure mode carries irreversible harm or systemic consequences.',
    'If the boundary is narrow (only medical devices and flight-critical systems), the carve-out''s bite is limited and most software remains under beta-friendly rules. If the boundary expands (IoT, autonomous vehicles, supply-chain management), the carve-out''s scope and extraction intensity both rise. Classification can shift with threat perception and new failure case studies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_boundary_definition, conceptual, 'Operationalization of ''critical domain'' determines the carve-out''s scope and the number of vendors affected.').

omega_variable(
    substitution_doctrine_emergence,
    'Is the categorical carve-out (beta prohibition in critical domains) being replaced by a finer-grained doctrine (domain-specific risk tiers, contingent beta shields, revocable beta status) that achieves similar safety objectives with less total enforcement overhead?',
    'Regulatory and case-law analysis: if newer doctrines (tiered beta, conditional liability disclaimers, revocable-during-pilot schemes) become dominant and the original categorical prohibition shifts to a background default, the original carve-out enters theater (it is maintained for historical legitimacy but the actual allocation is driven by the newer tool).',
    'If the carve-out is substituted by a more flexible doctrine, its classification could shift from tangled_rope (genuine coordination with forced vendor participation) to piton (inertial maintenance while the actual coordination happens elsewhere). The extractiveness measured by the carve-out alone would remain constant, but its functional role would degrade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_doctrine_emergence, empirical, 'Whether the original carve-out is being superseded by a more sophisticated liability-allocation regime.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (vendors'' inability to use beta in critical domains) structural (the regulatory prohibition is external and costly to circumvent) or internalized (vendors have come to accept the prohibition as legitimate and do not attempt to circumvent it)?',
    'Behavioral evidence: litigation statistics on vendors challenging the carve-out; lobbying expenditure and legislative amendment proposals; post-exit interviews with vendors who left critical domains. If litigation and lobbying decline while vendors stay in compliance, suppression has shifted from structural to internalized legitimation.',
    'If suppression is structural, the constraint''s persistence depends on continued enforcement (regulatory gatekeeping, liability exposure). If internalized, vendors maintain compliance even without external enforcement pressure, which could allow regulators to reduce gatekeeping intensity while maintaining compliance — a transition from rope (active coordination with external enforcement) to rope-with-voluntary-compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether vendor compliance is sustained by external enforcement or internalized legitimation.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the severity carve-out reading logically foreclose the expansive-shield reading, or do they coexist as contested claims held by different parties in ongoing dispute?',
    'Doctrinal analysis: if the carve-out is framed as a categorical exception to the shield (beta is generally a liability waiver, except in critical domains), the readings coexist. If the carve-out is framed as negating the entire shield premise (beta cannot allocate liability in any domain where harm severity is high), it forecloses the shield. Legislative language and case-law framing determine which structure is actual.',
    'If coexistence, the two readings can be held simultaneously by different parties and both remain live positions — this models the actual regulatory landscape. If foreclosure, one reading''s core premise directly contradicts the other''s, which would require judicial or legislative resolution. The impact on classification is modest (both trajectories keep the constraint type as tangled_rope or snare), but the stability prediction differs: coexistence invites ongoing jurisdictional variation; foreclosure predicts eventual doctrinal consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the two readings of the beta-designation kernel are logically incompatible or can coexist as different parties'' positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(beta_tr_t0, observed).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(beta_tr_t5, observed).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(beta_tr_t10, observed).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(beta_tr_t15, observed).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(beta_tr_t20, observed).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(beta_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(beta_be_t0, observed).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(beta_be_t5, observed).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(beta_be_t10, observed).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(beta_be_t15, observed).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(beta_be_t20, observed).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(beta_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(beta_su_t0, observed).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement_basis(beta_su_t5, observed).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement_basis(beta_su_t10, observed).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement_basis(beta_su_t15, observed).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(beta_su_t20, observed).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(beta_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__severity_carve_out_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% The beta-designation kernel admits three structurally distinct readings, each with its own constraint story, ε value, beneficiary/victim structure, and type classification. The severity carve-out reading (this story) asserts domain-specific safety constraints override contractual liability allocation; it forecloses or influences (depending on doctrine framing) the expansive-shield reading but coexists with the narrow-warning reading. All three stories link via network.affects_constraints to model the kernel family; the engine's comparison of their types and ε values reveals the doctrinal contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__severity_carve_out_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
