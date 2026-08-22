% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy: Bodily Autonomy Primacy Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates one reading of the vaccine mandate
 *   legitimacy kernel: the bodily autonomy primacy reading, which holds that
 *   medical self-sovereignty is categorically inviolable regardless of public
 *   health outcomes. The constraint is a SNARE because (1) the coercive
 *   mandate is justified through public health framing, but (2) the
 *   extraction—loss of bodily autonomy, livelihood, and liberty—persists
 *   regardless of whether the public health objective succeeds, (3)
 *   suppression is high because enforcement depends on active exclusion from
 *   employment and public life, and (4) alternatives (conscientious
 *   objection, risk-stratified exemption, alternative protection mechanisms)
 *   are structurally foreclosed. The beneficiaries are advocacy movements
 *   that weaponize the mandate to advance their broader anti-coercion agenda,
 *   not the unvaccinated themselves or the vulnerable populations the mandate
 *   purports to protect. This is a kernel reading: the same underlying
 *   mandate commitment can be read as a snare (bodily autonomy primacy), as
 *   legitimate rope (public health primacy), or as a valid but over-broad
 *   scaffold (risk stratification). The reading declared here is the
 *   ε-invariant instantiation of the bodily autonomy primacy framing—what
 *   that reading sees when it looks at the mandate structure.
 *
 * KEY AGENTS:
 *   - unvaccinated_individuals: primary target (bodily autonomy confiscation)
 *   - immunocompromised_persons_denied_alternatives: secondary target (caught between vulnerability and coercion)
 *   - conscientious_objectors: target (forced conscience violation or economic exclusion)
 *   - public_health_authorities: agenda setter (enforces mandate)
 *   - medical_autonomy_advocates: beneficiary (advancing bodily autonomy as supreme principle)
 *   - libertarian_advocacy_movements: beneficiary (instrumentalizing mandate for anti-coercion messaging)
 *   - medical_professionals: excluded (proportionality expertise shut out of policy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.82).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.91).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, snare).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Vaccine Mandate Legitimacy: Bodily Autonomy Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'c3e62059-1805-48bb-866a-069514d7249d').
narrative_ontology:cs_kernel_codification('c3e62059-1805-48bb-866a-069514d7249d', fixed_text).
narrative_ontology:cs_authority_grounding('c3e62059-1805-48bb-866a-069514d7249d', extraction).
narrative_ontology:cs_interpretation_layer_present('c3e62059-1805-48bb-866a-069514d7249d').
narrative_ontology:cs_reading_relation('c3e62059-1805-48bb-866a-069514d7249d', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c3e62059-1805-48bb-866a-069514d7249d', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('c3e62059-1805-48bb-866a-069514d7249d', foundational, bodily_autonomy_inviolable_categorical).
narrative_ontology:cs_axiom_status(bodily_autonomy_inviolable_categorical, holdable).
narrative_ontology:cs_axiom_grounding('c3e62059-1805-48bb-866a-069514d7249d', bodily_autonomy_inviolable_categorical, deontological).
narrative_ontology:cs_axiom('c3e62059-1805-48bb-866a-069514d7249d', foundational, informed_consent_non_delegable_to_state).
narrative_ontology:cs_axiom_status(informed_consent_non_delegable_to_state, holdable).
narrative_ontology:cs_axiom_grounding('c3e62059-1805-48bb-866a-069514d7249d', informed_consent_non_delegable_to_state, deontological).
narrative_ontology:cs_reference_frame('c3e62059-1805-48bb-866a-069514d7249d', constitutional_bodily_integrity).
narrative_ontology:cs_drift_state('c3e62059-1805-48bb-866a-069514d7249d', contemporary_pandemic_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c3e62059-1805-48bb-866a-069514d7249d', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_autonomy_advocates).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, libertarian_advocacy_movements).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_persons_denied_alternatives).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, conscientious_objectors).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_integrity_inalienable).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, informed_consent_non_delegable).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_coercion_categorically_impermissible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face legal and employment mandates to vaccinate or exit employment, education, or public life. Their bodily autonomy claim is the constraint's central referent. They bear the extraction: loss of livelihood, social exclusion, and forced medical intervention. Exit options collapse to bare alternatives: vaccinate or lose economic security.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_individuals, payer,
    powerless, biographical, trapped, national).

% Experience heightened transmission risk from vaccine-hesitant populations while mandates are framed as protecting them. If mandates fail, their genuine vulnerability (not rhetorical) goes unaddressed; if mandates succeed via coercion, they benefit incidentally but at cost of the coercive principle's establishment. Bear costs either way: vulnerability if mandates fail; complicity in coercion if mandates succeed.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_persons_denied_alternatives, payer,
    powerless, immediate, trapped, national).

% Hold sincere objections to vaccination grounded in religious, philosophical, or ethical conviction. Mandates do not distinguish their objections from instrumental vaccine-hesitancy; they face the same enforcement. Their cost is either conscience violation or economic/social exclusion.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, conscientious_objectors, payer,
    moderate, biographical, constrained, national).

% Mobilize opposition to mandates as the vehicle for establishing bodily autonomy as a non-negotiable constitutional principle. They benefit from the constraint through elevated attention, resource flow, and institutional legitimacy for their core claim—that coerced medical intervention is categorically impermissible. They do not themselves bear the mandate's enforcement costs; the unvaccinated do.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_autonomy_advocates, beneficiary,
    organized, generational, arbitrage, national).

% Frame mandates as exemplar of state overreach, mobilizing broader anti-coercion messaging. They benefit from the constraint's contestation by advancing their broader political agenda around minimal state authority. They do not bear the mandate's enforcement costs directly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, libertarian_advocacy_movements, beneficiary,
    organized, generational, arbitrage, national).

% Set and enforce mandate policy, justified by collective harm prevention and disease control. They claim the mandate solves a genuine coordination problem (controlling transmission); they bear the cost of enforcement infrastructure and legal contestation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Would articulate proportionality-based objections to blanket mandates and advocate for individualized risk assessment; they are largely excluded from setting mandate policy despite their expertise. Their absence from the constraint-setting negotiation is structural—mandates are political/legal decisions, not medical ones, and clinicians' voices advocating alternatives are not in the room.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_professionals, excluded,
    organized, biographical, constrained, national).

% The academic consensus on medical coercion (in bioethics, public health ethics, and medical jurisprudence) that informed consent is non-waivable and that state coercion in medicine requires extraordinary justification—the foundational claim this reading is instantiating.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_ethics_consensus, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_ethics_consensus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_autonomy_advocates).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function exists in this reading. The mandate is framed as solving a collective-action problem (preventing transmission), but that frame is precisely what this reading contests—it substitutes state coercion for individual choice and masks extraction as public health.
% TRANSFER_FUNCTION: Moves bodily autonomy (a non-fungible right) from individuals to the state-medical apparatus, in exchange for collective disease-risk reduction that accrues diffusely. The payers are the unvaccinated and conscientious objectors; the beneficiary is state capacity to enforce medical compliance; the secondary beneficiary is advocacy movements that instrumentalize the mandate to advance anti-coercion narratives.
% ABSENT_VOICES: Medical professionals advocating proportionality-based alternatives are structurally excluded—mandate policy is set by public health and political authorities, not clinicians. Individual vaccine-hesitant persons, immunocompromised patients at genuine risk of vaccine side effects, and conscientious objectors with developed ethical frameworks (not mere opinion) are not represented in the negotiation that produces the mandate.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, transmission patterns would shift (unvaccinated populations would show higher disease incidence), and public health authorities would lose a tool for rapid compliance. But the question of whether disease control would substantially degrade or merely shift to different risk distributions is itself contested—different empirical framings (transmission reduction ≈ 30–60% in most epidemiological models) lead to different verdicts. The bodily autonomy reading insists the answer is irrelevant—even full disease prevention does not justify categorical coercion—so the verdict is 'contested' in the sense that the causal question does not resolve the normative question.
% FOUNDING_PROBLEM: An emerging infectious disease with significant mortality in vulnerable populations, where vaccination is available and effective, but voluntary uptake is insufficient to prevent healthcare system overload and death clusters in high-transmission communities. The founding problem is the collective-action problem of coordination on disease control when individual incentives are misaligned with population outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities (beneficiaries) attest the founding problem is live and that mandates are necessary to solve it. Epidemiologists provide correlational evidence that mandate-adjacent compliance increases and disease burden declines in mandate jurisdictions. However, medical ethicists and public health ethics consensus bodies (outside the mandate-setting authority) attest that the founding problem is real but contestable in severity and that mandates represent an unjustifiable escalation beyond alternative interventions (targeted protection, voluntary incentives, information campaigns, risk-stratified approaches). The most direct corroboration of the founding problem comes from physicians and epidemiologists observing actual transmission waves; the most direct contestation comes from the same professional bodies on whether that problem justifies the coercive solution.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high and rising (0.65→0.82) because the mandate transfers bodily autonomy to the state-medical apparatus without compensation, and the transfer persists even where empirical disease control is achieved—the extraction is not contingent on failure. Suppression requirement is high and persistent (0.72→0.91) because the mandate's enforcement depends on active exclusion (job loss, school exclusion, travel restrictions), not on participant acceptance. Theater ratio is low (0.12→0.18) because the public health justification is substantive—disease control genuinely occurs—but a growing share of enforcement effort is devoted to punishing noncompliance rather than achieving disease control targets, and exemptions are rarely granted even on grounds the mandate itself nominally permits (medical contraindication, prior infection, temporal deferral). The measurement series show monotonic intensification of suppression and plateau of extractiveness at t=28, indicating enforcement infrastructure hardened and escalated through the interval, while the core extraction (autonomy loss) reached a stable plateau—the constraint matured into its equilibrium form. The shared time grid ensures every metric is authored at every examined time point (t=0, 4, 8, 12, 20, 28, 36), avoiding the OQ-105 misalignment trap.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority's seat, the mandate is legitimate emergency coordination: it solves a real collective-action problem (transmission control), the beneficiaries are the entire vulnerable population, and the cost to the unvaccinated is proportional to the benefit to the many. From the unvaccinated individual's seat, the mandate is categorical coercion: their bodily autonomy is non-negotiable, the public health benefit is uncertain and conditional on empirical claims, and no outcome justifies the precedent of state-enforced medical intervention. From the immunocompromised person's seat, the constraint is incoherent: they are nominally protected but their actual risk increases if mandates are enforced via social exclusion (the vaccinated are less cautious), and they did not consent to benefiting from coercion. From the advocacy movement's seat, the mandate is exactly the wedge needed to advance the broader claim that bodily autonomy cannot be overridden—they benefit from the mandate's existence precisely because they can contest it on principle. The engine computes per-seat directionality from the structural beneficiary/victim declarations: the public health authority and advocacy movements are beneficiaries (they gain from the constraint's operation); the unvaccinated and conscientious objectors are victims (they bear the extraction). Scope is national and suppression is structural (employment and legal exclusion), so directionality for the payer seats is near the target end; directionality for the beneficiary seats is near the beneficiary end. The gap is not an error—it is the signature of a snare: the same constraint is experienced as legitimate coordination from the agenda-setter's seat and as categorical coercion from the target's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals and conscientious objectors have trapped exit (vaccine or lose livelihood; no genuine alternatives). They are the constraint's primary targets, so their directionality is near 1.0 (full target end). Immunocompromised persons have constrained exit (vulnerable either way) and are nominally beneficiaries but actually vulnerable to mandate enforcement; their directionality is ambiguous—the overrides section does not correct for this because the structure itself is ambiguous: they gain protection incidentally but at the cost of the coercive precedent, which harms them if extended. Public health authorities are the agenda-setter (institutional power, set the rules); they benefit from the mandate's operation and have arbitrage-level exit (they can change the policy). Libertarian advocacy movements have organized power and arbitrage exit (they are not subject to the mandate, they benefit from contesting it, and they can redirect their organizing energy). Medical autonomy advocates are the same class: organized power, arbitrage exit, direct benefit from the constraint's operation. All beneficiaries land near the beneficiary end (d near 0.0). The power atom distribution is crucial: powerless unvaccinated (trapped) vs. institutional public health authorities creates maximum directionality divergence, which explains why the per-seat classifications diverge starkly from the story-level claimed type.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and real—collective transmission in the early pandemic did create mortality clusters that could be partially addressed by rapid vaccination uptake. But the founding problem status is contested because (1) mandates were implemented after vaccination was widely available and voluntary uptake was substantial, so the problem was already partially solved; (2) the core claim—that mandates increase net vaccination relative to alternatives like incentives and information campaigns—is itself contested by economists and political scientists; (3) the mandate persisted long after the founding problem's acute phase (t=0–12) and plateaued at a high enforcement level (t=28–36) even as the founding problem's urgency declined. This plateau is the mandatrophy signature: enforcement infrastructure persists and calcifies, the original justification becomes ceremonial, and the constraint remains because the authority has no incentive to dismantle it. The snare classification prevents mislabeling this as rope (pure coordination) by insisting on the asymmetry: the unvaccinated and conscientious objectors bear the cost regardless of whether the founding problem is solved; the advocates and authorities benefit regardless. If the founding problem were truly the driving rationale, mandates would sunset as disease burden declined—they did not. This is the mandatrophy signal: the constraint persists because it benefits the agenda-setter and advocacy movements, not because the founding problem requires it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_mandate_efficacy,
    'What is the actual counterfactual reduction in disease transmission attributable to mandate-driven vaccination, net of voluntary uptake trajectories?',
    'Controlled comparison between mandate and non-mandate jurisdictions with similar baseline vaccination rates, holding other variables constant (lockdown policies, variant prevalence, healthcare capacity). Econometric identification of mandate effects using quasi-experimental variation in implementation timing and stringency.',
    'If efficacy is substantial (>30% transmission reduction), the public health primacy reading gains structural support. If efficacy is marginal (<10%), the public health justification weakens and the snare classification is affirmed. This does NOT resolve the normative question (whether outcome justifies coercion) but establishes the empirical ground for the mandate''s claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_mandate_efficacy, empirical, 'Counterfactual disease reduction from mandate-driven compliance.').

omega_variable(
    coercion_vs_coordination_boundary,
    'Is the mandate logically foreclosed by the bodily autonomy primacy axiom, or is it merely disfavored?',
    'Axiom testing: if bodily autonomy is held to be categorically inalienable, then ANY state-enforced medical intervention (regardless of outcome) violates it, and the reading forecloses coercive approaches entirely. If bodily autonomy is held to be extremely weighty but defeasible by sufficiently grave emergencies, then the reading coexists with public health primacy and the dispute is over thresholds, not principles.',
    'Full foreclosure supports a strong snare classification and eliminates negotiation space with the public health primacy reading. Defeasibility-under-extreme-conditions permits coexistence and shifts the constraint''s type from snare to contested tangled rope (asymmetric extraction from legitimate coordination). This is the boundary between readings that truly conflict and readings that dispute thresholds within a shared framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_coordination_boundary, conceptual, 'Whether bodily autonomy primacy forecloses all coercion or merely disfavors it.').

omega_variable(
    identity_fusion_in_advocacy,
    'To what extent have medical autonomy advocates and libertarian movements fused their organizational identity with opposition to mandates, such that a mandate reversal would constitute an identity crisis rather than a policy victory?',
    'Discourse analysis of advocacy messaging and organizational positioning: if messaging pivots rapidly from specific mandate opposition to broader anti-coercion principles as mandates decline, fusion is partial; if the organization dissolves or radically shrinks as mandate contestation winds down, fusion is near-complete.',
    'High fusion indicates that the constraint''s beneficiaries have identity-locked their interest in the constraint''s persistence—they would bear a material cost (organizational legitimacy loss) from mandate reversal. This deepens the snare classification: not only does the constraint extract from the unvaccinated, but its beneficiaries are structurally committed to perpetuating it. Conversely, if advocacy movements successfully pivot to new anti-coercion campaigns, the fusion was instrumental and the extraction was pure rather than institutionally rooted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_advocacy, empirical, 'Whether advocacy movements have identity-locked themselves to mandate opposition.').

omega_variable(
    kernel_foreclosure_vs_coexistence,
    'Do the bodily autonomy primacy axiom and the public health primacy axiom logically foreclose each other in a single framework, or can a coherent authority structure hold both with different priority weightings?',
    'Examine actual constitutional and jurisprudential precedent (U.S. Constitutional law, international human rights frameworks) to determine whether courts/authorities treat bodily autonomy and public health authority as competing but both-legitimate interests (coexistence) or as fundamentally irreconcilable (foreclosure).',
    'True foreclosure (each reading rules out the other) means this reading is a sibling in the readings_relations sense—coexists_with applies. Precedent showing bodily autonomy as supreme-when-not-overridden suggests the axioms coexist. If precedent is split or evolution has shifted, the reading_relations edge and axiom_status may need revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_vs_coexistence, conceptual, 'Whether the axioms genuinely foreclose or merely compete for priority.').

omega_variable(
    immunocompromised_harm_asymmetry,
    'When immunocompromised persons are included in the victim set, is the extraction symmetrically distributed, or do they suffer disproportionate harms from mandate enforcement (social exclusion of caregivers, loss of alternative protection strategies) relative to their gain from increased population vaccination?',
    'Epidemiological and sociological data on health outcomes for immunocompromised persons in high-mandate vs. low-mandate jurisdictions, controlled for disease severity and healthcare access; interview-based evidence from immunocompromised persons on their actual risk perception and preference for mandate vs. alternatives.',
    'If immunocompromised persons show net negative outcomes from mandates (infection risk from disinhibited vaccinated contacts + psychological/social costs of mandate enforcement > protection gained), they should not be in the beneficiary set at all, and the snare classification is strengthened. If they show net positive outcomes, the extraction from them is lower than from unvaccinated, and directionality varies by agent—the constraint is more precisely a targeted snare than a blanket snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_harm_asymmetry, empirical, 'Whether immunocompromised persons bear net harms from mandate enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement_basis(vacc_tr_t4, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).
narrative_ontology:measurement(vacc_tr_t28, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 28, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t28, observed).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 4, 0.71).
narrative_ontology:measurement_basis(vacc_be_t4, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 8, 0.76).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(vacc_be_t20, observed).
narrative_ontology:measurement(vacc_be_t28, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 28, 0.82).
narrative_ontology:measurement_basis(vacc_be_t28, observed).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 36, 0.82).
narrative_ontology:measurement_basis(vacc_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 4, 0.78).
narrative_ontology:measurement_basis(vacc_su_t4, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 8, 0.84).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.88).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement_basis(vacc_su_t20, observed).
narrative_ontology:measurement(vacc_su_t28, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 28, 0.91).
narrative_ontology:measurement_basis(vacc_su_t28, observed).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 36, 0.91).
narrative_ontology:measurement_basis(vacc_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.25).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is a member of the vaccine_mandate_legitimacy constraint family. The kernel is the standing commitment to vaccine mandates in response to pandemic transmission. The bodily_autonomy_primacy_reading (this constraint) instantiates the kernel under the axiom that bodily autonomy is categorically inviolable. The public_health_primacy_reading instantiates the same kernel under the axiom that collective harm prevention justifies state medical authority. The risk_stratification_reading instantiates the kernel under proportionality constraints that permit targeted but not blanket mandates. Each reading yields a different ε, different beneficiary/victim structure, and different computed type. The three constraints are siblings linked by network.affects_constraints (each affects the others) and distinguished by their cs_structure.axioms and reading_relations. The sibling readings compete in real institutions—different courts, jurisdictions, and advocacy coalitions hold different readings. This file contains ONLY the bodily autonomy primacy reading; do not fold the siblings into this story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
