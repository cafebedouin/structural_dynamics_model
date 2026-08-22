% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Medical Mandate Coercion (Bodily Autonomy Primacy Reading)
 *   domain: constitutional/bioethical/public_health
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily-autonomy-primacy reading of
 *   the vaccine-mandate-legitimacy kernel. Under this reading, medical
 *   self-sovereignty is categorically inviolable; state coercion to enforce
 *   vaccination is per se impermissible regardless of epidemiological
 *   outcome, disease severity, or collective risk. The constraint models what
 *   the world looks like when this reading governs legitimacy: coercive
 *   enforcement mechanisms (employment conditioning, educational access
 *   conditioning, travel restriction) persist and intensify, driven by public
 *   health authorities' assertion of mandate authority. Vaccine-hesitant
 *   populations face escalating suppression. Liberty advocacy movements
 *   benefit from the constraint's existence as a moral rallying point.
 *   Immunocompromised populations paradoxically enter the victim set under
 *   this reading because they bear the externality of reduced vaccination
 *   rates caused by resistance to coercive enforcement, despite being
 *   protected by higher aggregate vaccination coverage. The claimed type is
 *   snare because the constraint's persistence depends entirely on the
 *   state's coercive capacity and suppresses genuine alternatives
 *   (trust-based persuasion, risk-stratified approaches, voluntary
 *   incentives) in favor of one enforcement mechanism. The reading is
 *   outcome-independent: epidemiological evidence that mandates prevent
 *   disease does not change the legitimacy judgment under this reading.
 *
 * KEY AGENTS:
 *   - public_health_authorities: institutional agenda-setter, controls coercive apparatus
 *   - vaccine_hesitant_individuals: powerless payers, trapped in employment/education conditions
 *   - medical_professionals: organized payers with constrained exit, forced into compliance enforcement
 *   - immunocompromised_bearing_externality: paradoxical victims—protected by mandate-driven rates but exposed to enforcement-resistance consequences
 *   - liberty_advocacy_movements: organized beneficiaries, gain moral and resource authority from constraint persistence
 *   - epidemiological_evidence_base: excluded non-agent, structurally barred from legitimacy calculation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.82).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.79).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, snare).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Medical Mandate Coercion (Bodily Autonomy Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "constitutional/bioethical/public_health").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '23999e18-51b1-4048-8864-f384285dc182').
narrative_ontology:cs_kernel_codification('23999e18-51b1-4048-8864-f384285dc182', fixed_text).
narrative_ontology:cs_authority_grounding('23999e18-51b1-4048-8864-f384285dc182', lineage).
narrative_ontology:cs_interpretation_layer_present('23999e18-51b1-4048-8864-f384285dc182').
narrative_ontology:cs_reading_relation('23999e18-51b1-4048-8864-f384285dc182', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('23999e18-51b1-4048-8864-f384285dc182', vaccine_mandate_legitimacy__risk_stratification_reading, coexists_with).
narrative_ontology:cs_axiom('23999e18-51b1-4048-8864-f384285dc182', foundational, bodily_integrity_inviolability).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('23999e18-51b1-4048-8864-f384285dc182', bodily_integrity_inviolability, deontological).
narrative_ontology:cs_axiom('23999e18-51b1-4048-8864-f384285dc182', foundational, coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('23999e18-51b1-4048-8864-f384285dc182', coercion_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('23999e18-51b1-4048-8864-f384285dc182', individual_bodily_sovereignty_inviolable).
narrative_ontology:cs_drift_state('23999e18-51b1-4048-8864-f384285dc182', contemporary_pandemic_response_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('23999e18-51b1-4048-8864-f384285dc182', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_risk_mitigators).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_exception_seekers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_bearing_externality).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_bearing_externality).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face employment loss, educational exclusion, travel restriction, and healthcare access barriers conditioned on vaccination status. Under this reading, these conditions constitute per se violations of bodily autonomy. Their situation is structurally trapped: accepting vaccination to preserve economic access concedes the autonomy principle; refusing vaccination to maintain principle loses material security. No genuine choice exists; the appearance of choice is theater.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_individuals, payer,
    powerless, biographical, trapped, national).

% Seek medical or philosophical exemptions but face gatekeeping by physicians and authorities who control legitimacy determination. Their professional identity and reputation depend on presenting as medically legitimate to authorities, which constrains honest disclosure of concerns and locks them into status-seeking behavior. The process of seeking exception becomes identity-constituting.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_exception_seekers, payer,
    moderate, biographical, identity_locked, national).

% Control policy formation, enforcement mechanism design, and definition of legitimate dissent. They deploy employment conditioning, educational access restrictions, travel barriers, and professional licensing as enforcement mechanisms. They benefit from the constraint through expanded regulatory authority and control over medical decision-making. They have full exit: they could shift to persuasion-based approaches anytime.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear high exposure risk from unvaccinated circulation, which increases when enforcement-driven resistance depresses vaccination uptake. This reading paradoxically treats their autonomy as secondary to the general bodily autonomy principle, leaving them trapped: they cannot exit unvaccinated circulation, cannot compel others' vaccination (that would be coercive), and depend on herd immunity thresholds that the autonomy-absolute reading undermines. They are victims of the reading's internal logic.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_bearing_externality, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_bearing_externality, beneficiary).

% Are required to administer, recommend, and enforce vaccination policy under threat of licensing revocation. Those who comply become complicit in the coercive apparatus this reading identifies as illegitimate. Those who refuse lose career stability. They occupy a dual position: they are agents of the constraint (setting policy, counseling patients) and payers of the constraint (careers depend on compliance).
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_professionals, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_professionals, agenda_setter).

% Gain organizational resources, legal authority, recruitment, fundraising, and moral standing from the constraint's existence and intensification. They benefit from every enforcement measure that validates their autonomy argument. They can exit the constraint's dynamics at any time by shifting resources, but do not, because the constraint generates their primary resource base and constituency. Their beneficiary position is functionally parasitic on the constraint's persistence.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, mobile, national).

% This reading's outcome-independence means epidemiological evidence about vaccine effectiveness, safety, and prevention capacity is structurally excluded from legitimacy calculation. The reading does not dispute the evidence; it rejects that evidence could ever justify coercion. Evidence accumulation does not change the axiom.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, epidemiological_evidence_base, excluded,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, epidemiological_evidence_base).

% Those who would prefer vaccination but lack systemic access (transportation, childcare, medical distrust from discrimination history, language barriers, prior negative healthcare experiences) are excluded from the bodily-autonomy reading's protections. The reading assumes a choice set they do not have. Their autonomy is nominal, not real—but the reading does not adjudicate the distinction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, competing_individual_risk_acceptors, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None declared. This reading rejects the premise that collective disease prevention constitutes a coordination problem that justifies individual coercion. The reading's position is that no coordination outcome can override bodily autonomy—so no coordination function is recognized as legitimate.
% TRANSFER_FUNCTION: Transfers bodily decision-making authority from individuals to state agents. Individuals relinquish the right to refuse medical intervention; states collect the regulatory authority to condition civic, economic, and social participation on medical compliance. Liberty advocacy movements receive organizational resources, constituency, and moral authority in proportion to the constraint's enforcement intensity.
% ABSENT_VOICES: Immunocompromised populations seeking near-universal vaccination coverage would object that the reading deprioritizes their protection. Epidemiologists and public health practitioners would object that outcome-independence forecloses disease prevention as a legitimate consideration. Constitutional scholars endorsing collective-action or public-goods theories would object to the reading's individualism. Disabled populations unable to vaccinate would object that autonomy framing leaves them isolated without protection. Victims of infectious disease would object if alive to testify. The reading structurally excludes all voices that would weigh collective outcomes against individual choice.
% DISAPPEARANCE_RATIONALE: If the coercive enforcement apparatus vanished, vaccination uptake would decline in some populations, remain stable or increase in others (through restored trust-based approaches). Epidemic dynamics would shift. Immunocompromised protection would depend on voluntary herd immunity rather than enforcement-driven compliance. Public health would shift institutional modalities from coercion to negotiation, persuasion, risk-stratification, and consent-based approaches. The entire architecture of conditional civic participation (employment, education, travel) would become decoupled from vaccination status. The constraint enables a specific governance modality (enforcement-based); its disappearance would reorganize public-health decision-making around different mechanisms.
% FOUNDING_PROBLEM: The founding problem is the state's claimed authority to override individual bodily autonomy through coercive power. The reading treats this as an authority question, not a disease-prevention question. Whether this authority is legitimate is what the constraint is about.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional libertarian scholarship, disability-rights bioethics (some streams), and bodily autonomy advocacy movements consistently assert this as a live problem. CORROBORATION FROM OUTSIDE BENEFICIARY SEATS: some mainstream bioethicists and constitutional scholars acknowledge bodily autonomy as a live normative concern even while disagreeing that it should override all collective-action justifications. The problem is live in that serious institutional and scholarly voices actively dispute state coercion authority; it is not dead or resolved. However, corroboration from epidemiologists and public health institutions is absent—they do not recognize this as the founding problem; they frame it as a disease-prevention problem. The reading's founding problem and the public-health-primacy reading's founding problem are incommensurable: they define what the constraint is fundamentally about.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) reflects sustained coercive transfer of bodily decision rights from individuals to state authorities—the constraint's core mechanism. Suppression (0.79) is high because enforcement depends on active denial of exit: employment loss, educational exclusion, travel restriction all function as coercive mechanisms maintaining compliance. Theater (0.28) is moderate-low because the constraint does actually accomplish disease prevention through vaccination rate elevation, even though this reading rejects that justification as legitimate. The measurement trajectory shows rising extractiveness and suppression through the interval (points 0–30), then plateauing (point 36), reflecting the constraint reaching maximum enforcement intensity and stabilizing at that level. Accessibility collapse (0.71) reflects that alternatives to state coercion (trust-based persuasion, risk-stratified mandates) become materially harder to pursue once the enforcement apparatus is operationalized and normalized. Resistance (0.73) reflects substantial organized pushback from bodily-autonomy advocates, medical-freedom movements, and civil-rights organizations—the constraint does NOT operate frictionlessly. The temporal measurements trace enforcement hardening: suppression-requirement climbs as alternative enforcement channels open (school mandates, healthcare worker mandates, travel mandates); extractiveness climbs as the state claims broader jurisdictional authority over medical conditions. The plateau at t=36 reflects a state of maximum institutional capture: every coercive mechanism is deployed, no further intensification is institutionally feasible without explicit authoritarian expansion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (public health authorities) experience this as legitimate disease prevention—genuine coordination solving an epidemic problem. From their seat, the constraint solves a public goods problem (vaccination as a non-excludable benefit whose uptake requires coordination). The payer seats (vaccine-hesitant individuals, immunocompromised bearing externality, medical professionals) experience this as coercive extraction of bodily autonomy, regardless of disease-prevention success. This reading's structural asymmetry is irreducible: the two seats have fundamentally different legitimacy frames. The engine computes from the structural data (coercive enforcement, suppression of alternatives, exclusion of epidemiological reasoning) and should generate snare classifications for the payer seats and a more favorable (rope or piton) classification for the agenda-setter, revealing the per-seat divergence that the reading itself instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities sit as agenda-setter with institutional power and arbitrage-level exit (they control whether enforcement continues); d is near the beneficiary end, approaching full subsidy. Vaccine-hesitant individuals are trapped (cannot exit employment, education, travel without vaccination status), powerless, and forced to bear costs; d is at the target end (1.0). Medical professionals are organized but constrained (career depends on compliance); d sits in the high-extraction zone (0.7–0.85). Immunocompromised populations are paradoxically positioned: they are victims of reduced vaccination uptake (caused by enforcement resistance), but under this reading they are supposed to win autonomy protection—the contradiction is the structural trap this reading creates. Liberty advocacy movements occupy beneficiary position despite being described as advocates: they gain organizational resources, moral authority, recruitment, and funding from the constraint's persistence. The reading's own framing makes them beneficiaries of the coercion they claim to oppose. This is the reading's core extractive contradiction: to save bodily autonomy from state coercion, the reading generates conditions that benefit the advocacy movements opposing the constraint, creating a perverse incentive against resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state authority to override bodily autonomy) remains live as a constitutional question. The disappearance verdict (world-rearranges) confirms the constraint has real structural effects—it is not vestigial. Theater is low enough (0.28) that the constraint does accomplish something functionally real (disease prevention), not merely perform legitimacy. However, the constraint exhibits extraction-accumulation (base_extractiveness rises 0.68→0.82 over the interval) and suppression-hardening (suppression-requirement rises 0.62→0.79), suggesting institutional mission creep: the original mandate authority expands to cover more conditions, more professions, broader populations. This is a mandatrophy candidate if the founding problem (preventing a specific disease outbreak) becomes dead (the outbreak is controlled) but the constraint persists (the mandate continues for endemic disease or different disease). The measurement plateau at t=36 suggests the constraint has reached a stability point where further extraction requires explicit institutional expansion, not incremental mission creep—a sign of approaching piton conditions if the extractiveness-benefit ratio begins to degrade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_persuasion_boundary,
    'Is the measured suppression (0.79) a result of state-imposed coercive force, or of persuasion-resistant populations whose medical hesitancy would persist even without legal enforcement?',
    'Natural experiment: jurisdiction that removes employment/educational conditioning but maintains public health messaging and voluntary incentives, observing vaccination uptake trajectory.',
    'If uptake stays near-current levels without coercion, suppression is structural (inherent to the population). If uptake drops substantially, suppression is iatrogenic (caused by enforcement resistance dynamics). Classification changes from snare (extraction through coercion) to rope (coordination with distributed resistance) if suppression is population-driven rather than enforcement-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_persuasion_boundary, empirical, 'Attribution of measured suppression to state coercion versus population medical hesitancy.').

omega_variable(
    immunocompromised_paradox,
    'Are immunocompromised populations genuinely victims under this reading, or are they collateral damage the reading accepts as the price of absolute bodily autonomy for others?',
    'Explicit advocacy from immunocompromised organizations: do they endorse this reading or reject it as sacrificing their protection?',
    'If rejected: the reading''s victim set is incomplete and incoherent (the most vulnerable are excluded from protection). The reading should shift to acknowledge immunocompromised bearing externality costs. If endorsed: the reading''s authors are willing to accept immunocompromised harm as acceptable cost of autonomy principle, which reframes the ethical logic from universalist autonomy to autonomy-for-those-able-to-exercise-it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunocompromised_paradox, empirical, 'Whether the bodily autonomy reading aligns with or contradicts immunocompromised-advocacy positions.').

omega_variable(
    advocacy_movement_incentive_capture,
    'Do liberty advocacy movements have structural incentives to perpetuate the constraint (constraint = fundraising, moral authority, recruitment) rather than resolve it?',
    'Examine funding trajectories and organizational expansion during periods of maximum enforcement versus periods of voluntary take-up.',
    'If incentive capture is demonstrated, the beneficiary classification for liberty_advocacy_movements becomes evidence of parasitic extraction: they benefit from the constraint without bearing its enforcement costs, creating perverse resistance to resolution pathways that would preserve autonomy without coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_movement_incentive_capture, empirical, 'Structural alignment of advocacy-movement interests with constraint persistence.').

omega_variable(
    alternative_coordination_feasibility,
    'Are trust-based persuasion, risk-stratified mandates, or voluntary incentive systems materially feasible for disease prevention, or is coercive enforcement the only mechanism that achieves public-health thresholds?',
    'Implementation trials in different jurisdictions; comparison of epidemiological outcomes across enforcement-heavy and persuasion-heavy regimes.',
    'If alternatives achieve similar public-health thresholds: the coercive constraint is pure extraction, its suppression is unjustified, and the snare classification is definitive. If alternatives fail: the constraint accomplishes something materially necessary, shifting classification toward tangled_rope (genuine coordination + asymmetric extraction) or rope (pure coordination with distribution friction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Whether coercive enforcement is the only mechanism capable of achieving public-health vaccination thresholds.').

omega_variable(
    axiom_overriding_trajectory,
    'As epidemiological evidence accumulates showing long-term vaccine safety and effectiveness, does the deontological axiom (bodily_integrity_inviolability) remain holdable or move toward overridden status within its own tradition?',
    'Track jurisprudential, ethical, and constitutional scholarship: is the axiom being refined, qualified, or abandoned as absolute?',
    'If overridden: the reading itself transitions from holdable to historically superseded, and a different reading (public_health_primacy or risk_stratification) becomes the institutional standard. If holdable: the axiom persists as a live position that rejects empirical refutation on principle (maintaining outcome-independence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_overriding_trajectory, conceptual, 'Long-term status of bodily_integrity_inviolability axiom within constitutional and bioethical traditions.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.79) structural (external barriers: employment loss, travel restriction) or internalized (individuals have absorbed the belief that vaccination refusal is morally transgressive or medically dangerous)?',
    'Post-enforcement-removal trajectory: if suppression persists in cohorts removed from coercive conditions, reclassify as partially internalized.',
    'If internalized: the constraint''s effective suppressive force is higher than structural barriers alone account for; it has installed itself in target populations'' self-concepts. The payers carry suppression with them even if exit becomes possible. If structural: removal of coercive mechanisms would substantially reduce suppression, making resolution possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Suppression mechanism: structural coercion versus internalized belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 6, 0.71).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 18, 0.78).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 36, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 18, 0.74).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 36, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vaccine-mandate-legitimacy kernel. It instantiates the bodily-autonomy-primacy position: medical self-sovereignty is categorically inviolable; state coercion is per se impermissible. Sibling readings instantiate alternative positions: public_health_primacy_reading asserts state duty to prevent collective harm justifies individual burden; risk_stratification_reading asserts legitimacy contingent on actuarial proportionality. Each reading generates a different constraint with different beneficiaries, victims, and classifications. They are not perspectives on one constraint but structurally distinct constraints grounded in different commitments to the same kernel. The three stories form a constraint family linked by network.affects_constraints edges. The decomposition is required by ε-invariance: the three readings produce substantively different ε values (this reading: 0.82; public_health_primacy: much lower ~0.35–0.45; risk_stratification: intermediate ~0.55–0.65) because they disagree on what counts as extraction. No single ε can model all three readings—each gets its own story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, powerless, 1.0).
constraint_indexing:directionality_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, institutional, 0.05).
constraint_indexing:directionality_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
