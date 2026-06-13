% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Judicial Activism: Treaty Mandate Exceeding Through Interpretive Drift
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint instantiates the JUDICIAL ACTIVISM READING of the WTO DSB
 *   authority kernel. Under this reading, DSB panels have progressively
 *   exceeded their treaty mandate by reinterpreting WTO agreements to expand
 *   scope beyond what member states negotiated. The panels frame these
 *   expansions as applying the 'object and purpose' of the treaty and
 *   interpreting it in light of 'contemporary practice,' but the effect is to
 *   create new obligations in domains (labor, environment, cultural goods)
 *   that were deliberately excluded from mandatory coverage in the original
 *   negotiated texts. The reading asserts that this interpretive drift
 *   constitutes illegitimate judicial legislation: panels are making new
 *   policy rules, not applying agreed rules. This reading is contested by two
 *   siblings (binding_referee_reading and advisory_coordination_reading),
 *   which hold that panel interpretation is either legitimate judicial
 *   application of treaty law (binding) or that panels are merely providing
 *   advisory guidance (advisory). The judicial_activism_reading claims the
 *   panels exceed legitimate bounds.
 *
 * KEY AGENTS:
 *   - DSB panels: institutional agenda-setters interpreting treaty scope and issuing binding rulings; operate with quasi-judicial independence and frame interpretive drift as treaty fidelity
 *   - Developing member states: structural payers bearing the cost of expanding obligations they did not negotiate; constrained exit (WTO withdrawal prohibitively costly)
 *   - Developed member states: structural beneficiaries; their existing regulatory standards already meet expansive interpretations, so DSB rulings lock in their regulatory advantage
 *   - Treaty negotiators (historical): excluded voice; their documented intent to bound DSB scope is reinterpreted as non-binding; cannot object from the past
 *   - Domestic legislatures: payers facing obligations that override democratically-enacted law without requiring new treaty ratification
 *   - Retaliation authority: moderate-power payer; threatened with coerced enforcement of obligations it does not consider legitimate
 *   - Legal scholars (observer): analytical seat documenting whether DSB jurisprudence has systematically exceeded textual bounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism: Treaty Mandate Exceeding Through Interpretive Drift").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '2dc2c741-7a21-4160-bbed-bb8769f8d2f0').
narrative_ontology:cs_kernel_codification('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', fixed_text).
narrative_ontology:cs_authority_grounding('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', extraction).
narrative_ontology:cs_interpretation_layer_present('2dc2c741-7a21-4160-bbed-bb8769f8d2f0').
narrative_ontology:cs_reading_relation('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_axiom('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', foundational, panel_authority_bounded_by_text).
narrative_ontology:cs_axiom_status(panel_authority_bounded_by_text, holdable).
narrative_ontology:cs_axiom_grounding('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', panel_authority_bounded_by_text, deontological).
narrative_ontology:cs_axiom('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', foundational, negotiated_exclusions_binding).
narrative_ontology:cs_axiom_status(negotiated_exclusions_binding, holdable).
narrative_ontology:cs_axiom_grounding('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', negotiated_exclusions_binding, conventional).
narrative_ontology:cs_reference_frame('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', bounded_textual_dispute_resolution).
narrative_ontology:cs_drift_state('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', contemporary_expanded_jurisdiction, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2dc2c741-7a21-4160-bbed-bb8769f8d2f0', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_policy_autonomy).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, treaty_negotiated_balance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, developed_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_legislatures).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, retaliation_authorization_seat).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, strict_textualism_treaty_interpretation).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, member_state_sovereignty_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret WTO agreements in reported disputes. Frame interpretations as applying treaty 'object and purpose' and 'contemporary practice.' Have progressively expanded the domains covered by DSB authority (labor, environment, food safety, cultural goods) beyond the textual scope negotiated by member states. Operate with quasi-judicial independence; appellate review is limited to legal reasoning, not panel discretion. Control the framing of what disputes fall within DSB jurisdiction.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_panels, agenda_setter,
    institutional, generational, analytical, global).

% Negotiated narrow WTO scope to preserve domestic policy flexibility in labor, environmental, and cultural domains. Face progressively expanding DSB rulings that impose domestic policy changes in those domains. Cannot exit WTO without losing market access and negotiating power. Domestic constituencies demand policy autonomy in these areas, creating pressure to resist DSB rulings, but resistance carries retaliation risk. Their capacity to shape panel interpretation is minimal (underrepresented on panels, weak in dispute resources).
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_member_states, payer,
    moderate, biographical, constrained, global).

% Their regulatory baselines (labor, environment, food safety) are already stringent. DSB rulings that expand scope to cover these domains do not require them to change course but simultaneously lock in their regulatory standards as global minimum, blocking developing states from using regulatory flexibility as competitive advantage. Overrepresented on DSB panels through legal resources and institutional networks. Can absorb compliance costs; their exit options include forum-shopping or regulatory arbitrage across jurisdictions.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developed_member_states, beneficiary,
    powerful, generational, arbitrage, global).

% The diplomats who negotiated the Uruguay Round and founded the WTO deliberately excluded labor, environment, and cultural goods from mandatory DSB coverage, treating them as non-traded domains. Their documented intent is being reinterpreted through panels' expansive reading of 'object and purpose.' Cannot object; their intent is dismissed as 'original understanding' that gives way to living interpretation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, treaty_negotiators_original, excluded,
    institutional, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(wto_dsb_authority__judicial_activism_reading, treaty_negotiators_original).

% Ratified WTO treaties on the understanding that DSB scope was bounded to negotiated domains; DSB interpretive expansion imposes new obligations that override domestic law without requiring treaty amendment or legislative re-ratification. Cannot amend DSB mandate unilaterally; require consensus of all member states (politically impossible). Face constituent pressure from constituencies harmed by expanding DSB obligations (trade unions, environmental groups, cultural workers).
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_legislatures, payer,
    moderate, biographical, constrained, national).

% The Dispute Settlement Understanding authorizes member states to retaliate against non-compliance with DSB rulings. In the judicial_activism_reading, the retaliation authorization itself becomes problematic: if the ruling exceeds DSB mandate, then authorizing retaliation to enforce it makes the retaliation itself a coerced transfer of something not negotiated. Smaller states cannot retaliate without harming their own trade; the retaliation threat is what enforces DSB compliance behavior, creating structural suppression.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, retaliation_authorization_seat, payer,
    moderate, generational, constrained, global).

% Analyze DSB jurisprudence to document whether panels have expanded scope beyond treaty text and negotiating intent. Provide evidence for the contested question of whether DSB interpretation is faithful to mandate or exceeds it. Hold no institutional position in DSB or member-state governments; provide independent assessment of the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, legal_scholars_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, developed_member_states).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original mandate was to provide neutral, expert dispute resolution applying negotiated WTO rules to member-state compliance disputes. A bounded DSB prevents races-to-the-bottom in trade barriers and provides a rules-based alternative to bilateral power negotiation, protecting smaller states from being bullied by larger trading partners.
% TRANSFER_FUNCTION: The constraint transfers policy autonomy from member states (especially developing states) to DSB panels. States that negotiated narrow scope lose the ability to regulate in excluded domains; panels reinterpret scope through 'object and purpose' reasoning, expanding what falls under DSB coverage. The transfer is enforced through retaliation threat: member states that resist compliance face trade sanctions authorized by the Dispute Settlement Understanding.
% ABSENT_VOICES: Treaty negotiators from the Uruguay Round and earlier GATT rounds are not represented in current DSB proceedings; their documented intent to bound scope is reinterpreted as non-binding. Domestic labor movements, environmental constituencies, and cultural workers harmed by expansive DSB rulings have no standing in DSB proceedings. Smaller member states' trade negotiators are present but underrepresented in panel composition and dispute resources. NGOs and civil society are excluded.
% DISAPPEARANCE_RATIONALE: If DSB panels ceased to issue binding rulings expanding scope beyond negotiated text and reverted to strictly bounded interpretation, member states would immediately reassert domestic policy autonomy in labor, environment, and cultural domains. Developing states would restructure their regulatory postures to use flexibility as competitive advantage. The power asymmetry between developed and developing states would shift. Trade negotiations would likely stall as developing states would demand renegotiation of the DSB scope boundary. The current institutional arrangement structurally constrains what is politically possible for member states.
% FOUNDING_PROBLEM: Early trade disputes were resolved through bilateral negotiation (slow, favor-dependent, vulnerable to power asymmetry) or unilateral retaliation (destructive). The founding problem was: how to create a neutral, rule-based system for dispute resolution so that smaller states have access to justice and are not bullied, and so that rule certainty attracts investment?
% FOUNDING_PROBLEM_CORROBORATION: The DSB and developed member states claim the founding problem remains live — rules-based enforcement prevents chaos, protects smaller states, and attracts investment. Developing states, trade union movements, legal scholars studying DSB jurisprudence, and Brazil's appellate mechanism shutdown all document that the DSB has systematically expanded beyond the negotiated mandate and now imposes obligations not textually supported, converting 'neutral arbitration' into 'judicial policy creation.' Congressional testimony from the U.S. and India, and academic studies from Oxford, Yale, and UNCTAD scholars (outside the DSB apparatus) support the activist-reading view that scope has exceeded bounds.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 at interval end (rising from 0.35 in 1995) because the constraint transfers policy autonomy from negotiating member states to panels without explicit mandate in treaty text — the transfer is sustained by reinterpreting treaty language, not by negotiated consent. Suppression is higher (0.72) because the constraint's persistence depends on enforcing compliance via retaliation threats: member states that resist the interpretation face trade sanctions, creating active suppression. Theater ratio (0.41, rising steeply) reflects the mechanism of the constraint: panels frame interpretive drift as textual fidelity and 'living interpretation,' when the measured effect is policy creation — the theatrical justification is what makes the drift coercible. Accessibility_collapse (0.62) is moderate because alternatives (treaty renegotiation, DSB withdrawal, dispute non-participation) remain formally possible but carry prohibitive costs, especially for developing states. Resistance (0.74) is high because developing states, trade unions, and legal scholars actively contest the interpretation; member-state withdrawal from DSB (Australia, India, Brazil at various points) and calls for appellate reform reflect this resistance. The measurement series documents monotonic drift: extractiveness, suppression, and theater all increase across the interval, consistent with the claim that interpretive creep is ongoing and accumulating.
 *
 * PERSPECTIVAL GAP:
 *   From the DSB panels' seat (institutional power, analytical exit): they are interpreting the treaty faithfully, applying evolved doctrine, and serving the treaty's ultimate purpose — promoting rules-based trade. From the developing-state seat (moderate power, constrained exit): panels are rewriting the agreement and imposing obligations without consent, acting as unaccountable legislators. From the observer seat: the evidence of whether panels have exceeded bounds is empirical (court jurisprudence analysis, negotiating-history comparison, doctrine mapping) but contested because panels control the interpretive framework itself. The engine computes per-seat type classification from this structural data; the claim (snare) and the metrics diverge from the binding_referee_reading perspective (which would claim rope or coordination) but align with the judicial_activism_reading itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The DSB panels sit at institutional power with analytical exit (they can change their interpretation framework but face no external constraint forcing them to; they are the interpreters, not subjects of interpretation). They are the agenda-setter, explicitly non-payer. Developing member states sit at moderate power with constrained exit: they negotiated bounded treaty scope, but cannot exit the system without catastrophic cost, so they absorb the expanding obligations. They are high-d targets. Developed member states are powerful beneficiaries: their exit_options are arbitrage (they can shop regulatory strategies across jurisdictions and their compliance costs are low because their baselines already exceed the constraints). They benefit from the DSB's expansive interpretation locking in their advantage. Domestic legislatures are moderate-power payers: they lose policy sovereignty (high d, toward target end) but sit at the negotiating table (moderate power, not powerless). The retaliation authority is a moderate-power payer coerced to enforce obligations it does not consider legitimate. Treaty negotiators are excluded from current decisions but were powerful at founding; this is captured by the excluded role and the 'buried intent' theme in the omega. No directionality override is needed; the structural data produces the correct d vector.
 *
 * MANDATROPHY ANALYSIS:
 *   The judicial_activism_reading names the founding problem ('neutral, rule-based dispute resolution to protect smaller states') as CONTESTED / DEAD. It claims the DSB has converted the rule-based system into a vehicle for policy creation that undermines the textual boundaries member states negotiated. The constraint persists not because the founding coordination problem requires it (developing states claim the problem is solved at bounded scope), but because enforcement machinery (retaliation authorization) and institutional inertia keep it alive. The constraint is not formally mandated to do what it now does, but does so anyway through interpretive drift. This is a mandatrophy candidate: the founding purpose (bounded arbitration) has atrophied and the institution now pursues its own mandate expansion. The theater ratio rising from 0.08 to 0.41 is diagnostic: increasingly, DSB operations are theatrical justification of the new scope, not functional dispute resolution under agreed rules.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_vs_evolutionary_interpretation,
    'Is DSB interpretation constrained by treaty text and negotiating history, or does the treaty legitimately evolve to meet contemporary needs, with panels as legitimate interpreters of that evolution?',
    'Historical analysis of negotiating records (Uruguay Round travaux préparatoires) comparing original intent to current rulings; systematic jurisprudence analysis documenting whether panels cite text/history or substitute ''object and purpose'' and ''contemporary practice''; member-state statements on ratification.',
    'If interpretation has strayed far from text/history and panels claim evolutionary authority, the constraint is an extractive institution disguised as interpreter. If panels cite strong textual support for expansions, the reading collapses toward binding_referee_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_vs_evolutionary_interpretation, empirical, 'Whether DSB interpretation is bounded by negotiated text or legitimately evolutionary.').

omega_variable(
    member_state_consent_retraction,
    'When member states ratified the WTO, did they consent to DSB authority to expand scope through interpretation, or consent only to bounded arbitration of negotiated text?',
    'Legislative history of ratifying countries; statements at the ministerial conferences; evidence of whether states expected the DSB could reinterpret scope through case law or only apply fixed rules.',
    'Clear evidence that ratifying legislatures did NOT consent to interpretive scope expansion would reframe the constraint from disagreement-about-interpretation to unilateral authority-assumption (stronger snare claim). Clear evidence that scope expansion was anticipated would move toward binding_referee_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_consent_retraction, empirical, 'Whether member-state ratification consent covered DSB interpretive mandate expansion.').

omega_variable(
    retaliation_legitimacy_cascade,
    'If DSB rulings themselves exceed treaty mandate, is the retaliation that enforces them a legitimate exercise of treaty-authorized enforcement, or an illegitimate coerced transfer?',
    'Legal analysis of retaliation authorization''s scope and conditions; member-state practice in authorizing retaliation for contested vs. uncontested rulings; evidence of whether retaliation threatens are modified when panel jurisdiction is disputed.',
    'If retaliation is authorized only for rulings within DSB legitimate scope, then retaliation enforcing out-of-scope rulings is ultra vires (exceeds authority), strengthening the snare classification. If retaliation is authorized for all DSB rulings regardless of jurisdiction disputes, then enforcement becomes doubly problematic — it enforces rules that the DSB made up, using coercion the member states thought was bounded to agreed disputes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_legitimacy_cascade, empirical, 'Whether retaliation enforcement is legitimate when the ruling being enforced is disputed as exceeding mandate.').

omega_variable(
    kernel_reading_contest_structure,
    'Are the three readings (judicial_activism, binding_referee, advisory_coordination) genuinely coexisting as live positions held by different parties, or does one reading foreclose the others?',
    'Mapping which member states, DSB insiders, and legal traditions hold which reading; testing whether each reading''s core premise logically contradicts the others or merely emphasizes different aspects of the same institutional reality.',
    'If readings coexist (parties hold different ones simultaneously), then the kernel is genuinely contested and the machinery is Ω_C (conceptual irreducibility). If one reading logically forecloses others (e.g., if the binding_referee reading asserts DSB panels ARE courts, then the advisory_coordination reading that says they are NOT courts is logically foreclosed), then the contest is about material facts, not interpretation. This affects how the constraint family is structured and whether reconciliation is possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the three DSB authority readings coexist or foreclose each other.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (retaliation threats, institutional barriers to exit) or internalized (states believing they morally/legally ought to comply, even if they dispute jurisdiction)?',
    'Analyzing state resistance patterns: if states resist interpretation but comply out of fear of retaliation, suppression is structural; if states resist interpretation but comply out of belief in rule-of-law, suppression is partly internalized; if states resist interpretation AND challenge compliance through countermeasures/non-participation, suppression is failing and structural.',
    'If suppression is mostly structural, the constraint relies on coercion and is a stronger snare. If suppression is partly internalized (a norm of compliance with international institutions), the constraint carries an ideological component that could shift if the legitimacy frame breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of non-compliance is enforced through retaliation threats (structural) or through internalized rule-of-law norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement_basis(wto__tr_t2000, observed).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(wto__tr_t2005, observed).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement_basis(wto__tr_t2010, observed).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement_basis(wto__tr_t2015, observed).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement_basis(wto__tr_t2020, observed).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(wto__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(wto__be_t2000, observed).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement_basis(wto__be_t2005, observed).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(wto__be_t2010, observed).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(wto__be_t2015, observed).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(wto__be_t2020, observed).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(wto__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement_basis(wto__su_t2000, observed).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement_basis(wto__su_t2005, observed).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement_basis(wto__su_t2010, observed).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement_basis(wto__su_t2015, observed).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(wto__su_t2020, observed).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(wto__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__judicial_activism_reading, 0.18).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_autonomy__trade_constraint).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, developing_state_policy_capture__wto_mechanism).

% DUAL FORMULATION NOTE:
% The WTO DSB authority kernel is decomposed into three structurally distinct readings: (1) binding_referee_reading (panels issue legitimate law; extraction is coordination cost), (2) advisory_coordination_reading (panels advise; member states retain discretion; no extraction), (3) judicial_activism_reading (panels exceed mandate; illegitimate policy creation; this file). Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and type. The three readings coexist as live positions held by different member states, legal traditions, and institutional actors. They are linked via affects_constraints because the legitimacy of DSB authority structures what constraints it can impose; the reading adopted determines whether DSB rulings count as coordination (advisory/binding) or extraction (activism). All three readings are needed to model the kernel contest; no single constraint captures the full institutional reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
