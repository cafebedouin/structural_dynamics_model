% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Substance Prohibition as Moral-Social Coordination (Prohibition Reading)
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of a contested kernel: the
 *   prohibition_reading of the substance_control_kernel. The kernel is the
 *   standing societal commitment to regulate substance use; this reading
 *   frames that regulation as punishment for moral transgression requiring
 *   state coercion to protect social order. Alternative readings
 *   (harm_reduction_reading, legalization_reading) instantiate different
 *   constraint structures from the same kernel commitment. This constraint
 *   story is ONLY this reading; the others are separate JSON files linked via
 *   network.affects_constraints. Do NOT conflate the three readings into one
 *   constraint — each reading produces a different ε, different victim sets,
 *   and different beneficiary structures. The prohibition reading's ε is high
 *   (0.81) because state punishment is the primary mechanism; the harm
 *   reduction reading's ε would be lower (punishment is not the mechanism);
 *   the legalization reading's ε would differ again (individual liberty with
 *   externality capture). The claimed_type (tangled_rope) reflects the
 *   prohibition reading's own logic: genuine coordination around moral order
 *   (coordination function) coupled with asymmetric extraction from substance
 *   users and marginalized communities (extraction function). The measurement
 *   series (1970-2026) tracks intensification of extractiveness and theater
 *   ratio over the prohibition era, indicating increasing disconnect between
 *   the moral-order framing and the constraint's actual operation as
 *   institutional rent-seeking and racialized enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.87).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Prohibition as Moral-Social Coordination (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'd3778122-568a-4a2a-91d0-da01de7dbda3').
narrative_ontology:cs_kernel_codification('d3778122-568a-4a2a-91d0-da01de7dbda3', formalized).
narrative_ontology:cs_authority_grounding('d3778122-568a-4a2a-91d0-da01de7dbda3', extraction).
narrative_ontology:cs_interpretation_layer_present('d3778122-568a-4a2a-91d0-da01de7dbda3').
narrative_ontology:cs_reading_relation('d3778122-568a-4a2a-91d0-da01de7dbda3', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('d3778122-568a-4a2a-91d0-da01de7dbda3', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('d3778122-568a-4a2a-91d0-da01de7dbda3', foundational, substance_use_as_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_as_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('d3778122-568a-4a2a-91d0-da01de7dbda3', substance_use_as_moral_transgression, deontological).
narrative_ontology:cs_axiom('d3778122-568a-4a2a-91d0-da01de7dbda3', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('d3778122-568a-4a2a-91d0-da01de7dbda3', state_punishment_protects_social_order, deontological).
narrative_ontology:cs_reference_frame('d3778122-568a-4a2a-91d0-da01de7dbda3', moral_order_preservation_mandate).
narrative_ontology:cs_drift_state('d3778122-568a-4a2a-91d0-da01de7dbda3', contemporary_decriminalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d3778122-568a-4a2a-91d0-da01de7dbda3', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, criminal_justice_institutions).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, affected_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, moral_conservatives).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, commercial_drug_trafficking_organizations).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, commercial_drug_trafficking_organizations).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, pharmaceutical_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face criminalization, incarceration, collateral sanctions (employment, housing, family custody loss), and social stigma. Under the prohibition reading, their use is framed as moral transgression rather than health condition, making exit through treatment participation structurally different from harm reduction framing — treatment is reframed as punishment/reformation rather than medical intervention. Trapped by neurobiological dependence, legal consequences, and the constraint's framing that defines use itself as criminal act.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, biographical, trapped, national).

% Sets and enforces prohibition rules, determines enforcement priorities, administers arrest and prosecution authority. Collects institutional resources, budgets, political authority, and operational scope from enforcement activity. Can choose escalation or de-escalation of enforcement; maintains the constraint by active exclusion of alternative framings (harm reduction, treatment-first models). Benefits from the constraint's persistence through organizational expansion and resource allocation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Courts, corrections systems, and prosecutorial apparatus derive operational mandate, budgets, institutional prestige, and employment from prohibition enforcement. Collects revenue streams (fines, asset forfeiture, incarceration bed occupancy where private corrections operate), political legitimacy, and structural authority from the moral transgression framing. Can shift enforcement intensity but cannot unilaterally reframe substance use without institutional mandate loss.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, criminal_justice_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, criminal_justice_institutions, agenda_setter).

% Experience disproportionate enforcement targeting, higher arrest rates for equivalent use, longer sentences, and accumulated intergenerational incarceration burden. Bear diffuse costs of black market violence, neighborhood destabilization, and family separation. Lack resources to contest framing or exit enforcement spatially; trapped by geography, economic dependency, and racialized enforcement patterns.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, marginalized_communities, payer,
    powerless, biographical, trapped, national).

% Bear costs of family member incarceration (income loss, custody disruption, social stigma, emotional toll). Framed under prohibition reading as bearing collateral consequences of moral transgression, rather than as co-victims of enforcement policy. Constrained by economic dependency and social structure; excluded from policy-making seats even though the constraint's operation directly shapes their material conditions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, affected_families, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, affected_families, excluded).

% Receive vindication of moral transgression framing and social-order protection narrative. Benefit from the constraint's operation as enforcement of their normative claims about substance use as moral failing. Possess political power and institutional voice to maintain the framing against alternative readings. Can shift enforcement intensity without reframing the core moral premise.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, moral_conservatives, beneficiary,
    powerful, generational, arbitrage, national).

% Are structurally excluded from primary policy-making roles under the prohibition reading. Their evidence base (overdose prevention, disease transmission reduction, cost-effectiveness) contradicts the moral transgression framing and is treated as undermining social order protection. Can operate in constrained spaces (syringe exchange, medication-assisted treatment) only where prohibition framework permits, and must justify services through criminalization premises rather than health outcomes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, national).

% Pay enforcement costs (interdiction, prosecution, incarceration of lower-level operatives) but structurally benefit from prohibition via market power, pricing authority, violence-backed enforcement of supply monopolies, and the absence of regulated legitimate competition. Trapped by the constraint's creation of criminal markets with supernormal profits and dependence on violence rather than contract law for dispute resolution.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, commercial_drug_trafficking_organizations, payer,
    powerful, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, commercial_drug_trafficking_organizations, beneficiary).

% Enacts and maintains prohibition statutes, sets penalties, and can redefine the constraint's boundaries and enforcement intensity. Derives political legitimacy from social-order protection framing and moral transgression doctrine. Faces pressure from constituents bearing enforcement costs but maintains the framing against alternative readings through statute and resource allocation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, legislative_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Are excluded from substance market production by prohibition (with exceptions for opioid manufacturing under different regulatory frames that create parallel extraction). Experience both constraint enforcement (prosecution of diverted medications) and constraint benefits (protection of legitimate prescription market from low-cost generic drug competition). Mobile through regulatory arbitrage and can shift production to jurisdictions with different readings.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, pharmaceutical_interests, excluded,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, pharmaceutical_interests, payer).

% The prohibition reading claims that 'society' and 'social order' benefit from enforcement of the moral transgression doctrine. This is a vindicated proposition rather than a real actor; the constraint benefits named agents (law enforcement, criminal justice, moral conservatives) at the expense of others. Social order itself is not an agent that collects rents.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, society_as_enforcer, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(substance_control_kernel__prohibition_reading, society_as_enforcer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the prohibition reading, the constraint solves a coordination problem: how to maintain public commitment to a shared moral order that defines substance use as transgression and deems enforcement necessary to protect that order from erosion. The coordination is among those who hold the moral transgression premise; it requires state enforcement to maintain the shared framing against competing readings and to punish violators as a signal of commitment.
% TRANSFER_FUNCTION: Moves freedom, income, family stability, and bodily autonomy from substance users and marginalized communities to law enforcement and criminal justice institutions (in the form of operational budgets, political authority, and institutional expansion). Also transfers market power and pricing authority to criminal trafficking organizations (who benefit from prohibition's supply reduction and pricing effect). Transfers legitimacy claims to moral conservatives and social-order frameworks.
% ABSENT_VOICES: Harm reduction practitioners and public health epidemiologists are structurally excluded — their evidence base contradicts the moral transgression framing and is suppressed as incoherent with social-order protection. Substance users themselves are excluded from policy-making roles, their experience reframed as evidence of moral failing rather than as testimony about the constraint's effects. Communities bearing disproportionate enforcement costs (marginalized populations) are excluded from primary policy seats.
% DISAPPEARANCE_RATIONALE: If the prohibition constraint and its enforcement vanished, substance use patterns would reorganize substantially — some toward treatment and harm reduction (now framed as health service rather than moral failure), some toward regulated legalization with taxation and age controls, some toward intermediate models. Criminal trafficking organizations would lose the monopoly pricing power prohibition creates. Law enforcement resource allocation would shift. The moral transgression framing would lose state enforcement backing and would compete openly with alternative framings rather than holding hegemonic status.
% FOUNDING_PROBLEM: The founding problem under this reading is the preservation of social moral order against erosion by individual transgression. Stated by the reading's own lights: substance use is framed as a moral transgression that, if unpunished, would signal to others that transgression is tolerable and would erode collective commitment to shared moral norms. The state's role is to enforce those norms through criminal punishment, making the cost of transgression higher than the benefit and thereby protecting the social order.
% FOUNDING_PROBLEM_CORROBORATION: The reading's own adherents (moral conservatives, criminal justice agencies, legislative authorities) attest the founding problem is live. However, public health authorities, comparative public policy research from decriminalization jurisdictions (Portugal, Switzerland), and harm reduction practitioners attest the founding problem is either misidentified or superseded — that substance use epidemiology responds to availability, poverty, untreated trauma, and healthcare access far more than to punishment certainty, and that the moral order has not eroded in jurisdictions with decriminalized approaches. The competing readings (harm reduction, legalization) are attested by these outside voices as more accurately describing the structural dynamics.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising from 0.55 in 1970. This trajectory reflects: (1) expansion of the carceral apparatus (incarceration rates rose 400%+ over the interval); (2) accumulation of collateral sanctions (employment restrictions, housing exclusions, family law impacts); (3) intensification of enforcement targeting marginalized communities (racialized disparities in arrest and sentencing); (4) creation of supernormal profits for criminal trafficking organizations. Suppression is higher (0.87) than extractiveness because maintaining the moral transgression framing requires active suppression of competing framings (harm reduction evidence, international decriminalization models, lived experience testimony from affected communities). Theater ratio (0.42, rising from 0.18) indicates increasing disconnect: criminal justice agencies justify operations through moral-order protection and social-safety rhetoric while actually enforcing institutional expansion, asset forfeiture, and racialized targeting. The rising theater ratio is diagnostic of constraint degradation toward piton-like operation — institutional maintenance through performance rather than genuine coordination. The measurement grid is authored on one shared timeline so every metric is assessed at every time point; this alignment enables the temporal analysis systems to detect that extraction and theater are co-rising, a signature of degradation.
 *
 * PERSPECTIVAL GAP:
 *   From the law enforcement and criminal justice seat, this constraint is a genuine coordination mechanism (maintaining moral order) that they conscientiously administer and for which they deserve institutional resources. From the substance-user and marginalized-community seats, the same constraint operates as coercive extraction dressed in moral language — the 'moral order' is a cover story for institutional expansion and racialized targeting. Neither seat is lying; they are observing the same constraint through different structural positions. The engine's per-seat classification is designed to capture this: what computes as rope (beneficial coordination) from the enforcement seat should compute as snare or tangled_rope from the user seat because the structural asymmetry (who benefits, who is trapped) is encoded in the authored power/exit/beneficiary data. The theater ratio rising over time indicates that from both seats, the performative maintenance of the moral-order justification has come to exceed the actual coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges radically across seats. Law enforcement (institutional power, arbitrage exit) has d near 0.0 (full beneficiary): the constraint expands their budgets, authority, and operational scope; they can escalate or de-escalate while maintaining the constraint's basic frame. Substance users (powerless, trapped exit) have d near 1.0 (full target): they bear criminalization, incarceration, collateral sanctions, and their exit options are foreclosed by legal jeopardy and neurobiological dependence. Moral conservatives (powerful, arbitrage exit) have d near 0.2 (beneficiary): they collect vindication of their normative framing and political legitimacy from enforcement, and can modify enforcement intensity without losing the moral premise. Marginalized communities (powerless, trapped exit) have d above 1.0 structurally — higher extraction intensity than substance users due to racialized enforcement disparities, compounded by geographic trapping and economic dependency that preclude spatial exit. The engine computes these divergent directionalities from the authored beneficiary/victim/exit declarations; the story does not adjudicate them but supplies the structural data that generates the per-seat classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows clear mandatrophy dynamics. The founding_problem (preservation of moral order against erosion by substance transgression) is contested as to whether it is still live. Evidence: (1) public health authorities argue substance use epidemiology is driven by availability and untreated trauma, not moral enforcement; (2) decriminalization jurisdictions (Portugal since 2001, Switzerland harm-reduction pilot, Netherlands policy shift) report stable or declining use rates despite reduced punishment, contradicting the mandate's enforcement justification; (3) arrest rates for drug possession have stabilized or declined in many US jurisdictions since 2012 despite rising incarceration for distribution, suggesting enforcement focus has shifted away from the foundational mandate; (4) rising theater ratio indicates institutional operation increasingly divorced from the stated purpose. Mandatrophy resolution: the constraint persists because it benefits institutional actors (law enforcement, criminal justice) who can maintain enforcement intensity even if the founding problem is no longer live. A mandatrophied constraint that should have sunset or reformulated (shifted to harm reduction framing) instead persists through institutional inertia and deliberate reframing of institutional expansion as social-order protection. The constraint should be classified as piton-candidate (persists by performance and institutional inertia rather than genuine coordination function) or as tangled_rope degrading toward snare (asymmetric extraction increasing as theater increases).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_transgression_vs_medical_condition,
    'Is substance use fundamentally a moral transgression, a medical condition, or a complex phenomenon spanning both framings?',
    'Comparative epidemiology: do jurisdictions with high enforcement of moral-transgression framing show different prevalence, severity, or recovery outcomes than jurisdictions with health-condition framing? Do neurobiological markers, genetics, trauma history, and socioeconomic factors explain more variance in use onset than moral agency? Do punishment certainty predict abstinence independent of treatment access?',
    'If substance use is primarily health condition: the entire constraint''s moral transgression premise is foreclosed, and the beneficiary/victim structure reverses (users become treated as patients rather than criminals, enforcement apparatus loses mandate, harm reduction becomes primary function). If primarily moral transgression: the prohibition reading''s core axiom holds, and the extracted rents are justified as social-order maintenance costs. This is the most consequential omega for the reading''s validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_transgression_vs_medical_condition, empirical, 'Fundamental framing question for the reading''s core axiom.').

omega_variable(
    enforcement_efficacy_vs_cost,
    'Does prohibition enforcement reduce population substance use rates (net of harm), or does it increase incarceration and collateral harm while use persists independent of enforcement intensity?',
    'Causal inference from jurisdictional variation: do US states with harsher penalties and higher enforcement show lower per-capita use than permissive jurisdictions, controlling for treatment access, poverty, and opioid supply? Do international comparisons (Portugal decriminalized in 2001; Netherlands harm-reduction dominant; US prohibition-heavy) show predicted divergence in use rates?',
    'If enforcement reduces net harm: the theater ratio rise becomes misinterpretation (performance justified by efficacy), and the extraction may be necessary cost. If enforcement increases net harm: the rising theater ratio is diagnostic of mandatrophy (institutional operation persists despite failed founding problem), and the constraint should be reclassified as piton or snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_efficacy_vs_cost, empirical, 'Whether the constraint achieves its stated founding problem or persists through institutional inertia.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of competing readings (harm reduction, legalization) structural (law enforcement actively excludes alternatives via prosecution and policy capture) or internalized (populations absorbed the moral-transgression framing as common sense)?',
    'Historical analysis of policy discourse: did harm-reduction frameworks emerge in academic/medical spaces first and then face active suppression by law enforcement and moral-conservative actors (structural suppression), or were they always absent from public discourse (internalized suppression of the alternative entirely)? Post-exit suppression trajectory: when individuals move from prohibition-heavy to harm-reduction-dominant jurisdictions, do their framing shifts follow legal change or lag it?',
    'If structural: the suppression (0.87) is artificially elevated by active exclusion mechanisms; relaxing enforcement would lower suppression below the measured value. If internalized: suppression persists even after law relaxes, indicating deeper identity-fusion or institutional capture. This affects exit-option classification for affected constituencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of competing framings is active institutional exclusion or internalized normalization.').

omega_variable(
    reading_foreclosure_boundary,
    'Does the prohibition_reading''s core premise (substance use IS moral transgression requiring state punishment) logically foreclose the harm_reduction_reading''s premise (substance use is health condition), or can both framings coexist in a single policy framework?',
    'Logical analysis: if substance use is moral transgression, can one simultaneously treat it as health condition without logical contradiction? Or does the moral framing necessarily entail punishment as the primary state response, foreclosing treatment-first, harm-reduction-centered approaches?',
    'If foreclosed (they cannot coexist): the two readings are distinct constraints in a family where the prohibition reading FORECLOSES the harm_reduction reading—adoption of one precludes the other. If coexist (both can hold): different policy actors can simultaneously hold the moral transgression view while supporting harm reduction pragmatically, making them compatible positions within a single framework. This determines the network relation from this reading to harm_reduction_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Core question for the reading_relations axiom structure.').

omega_variable(
    institutional_capture_of_mandate,
    'Does law enforcement and criminal justice institutional inertia explain the persistence of the prohibition constraint despite contested founding_problem status, or does the moral transgression axiom remain a live shared value in the political constituency?',
    'Political economy analysis: if the constraint were reframed (toward harm reduction or legalization), which constituencies would actively resist and which would accept? Do law enforcement budget increases correlate with substance-use prevalence (expected if efficacy drives resource allocation) or with political pressure from moral conservatives (expected if mandate justifies budgets independent of efficacy)?',
    'If institutional capture: the constraint should be classified as piton (inertial, theatrically maintained). If moral consensus: it remains tangled_rope (genuine coordination on moral order, coupled with extraction). This affects classification and the mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_mandate, empirical, 'Whether the constraint persists due to institutional inertia or genuine political consensus on the moral axiom.').

omega_variable(
    kernel_reading_contest_frame,
    'Is the substance_control_kernel itself contested and under-specified (multiple readings are genuinely live and competing), or is the prohibition_reading the currently hegemonic instantiation with alternatives marginalized in policy discourse?',
    'Meta-analysis of policy discourse: are harm_reduction and legalization readings live positions in legislative debates, judicial proceedings, and executive policy-making, or are they marginalized as fringe alternatives suppressed by prohibition hegemony? Do different jurisdictions genuinely instantiate different readings (Portugal''s decriminalization = harm_reduction instantiation, Switzerland = hybrid instantiation, US = prohibition instantiation)?',
    'If genuinely contested: the kernel is truly under-determined, and each reading is a distinct constraint family member. If hegemonic: the prohibition reading is THE constraint, and alternatives are not yet full readings but counter-framings being suppressed. This affects how the reading_relations edges are drawn and whether the siblings are peer constraints or subordinate counter-proposals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_frame, conceptual, 'Whether the substance_control_kernel is genuinely multi-reading or prohibition-hegemonic with suppressed alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_kernel__prohibition_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(subs_tr_t1985, substance_control_kernel__prohibition_reading, theater_ratio, 1985, 0.24).
narrative_ontology:measurement(subs_tr_t2000, substance_control_kernel__prohibition_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(subs_tr_t2012, substance_control_kernel__prohibition_reading, theater_ratio, 2012, 0.38).
narrative_ontology:measurement(subs_tr_t2020, substance_control_kernel__prohibition_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(subs_tr_t2026, substance_control_kernel__prohibition_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_kernel__prohibition_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(subs_be_t1985, substance_control_kernel__prohibition_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(subs_be_t2000, substance_control_kernel__prohibition_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement(subs_be_t2012, substance_control_kernel__prohibition_reading, base_extractiveness, 2012, 0.79).
narrative_ontology:measurement(subs_be_t2020, substance_control_kernel__prohibition_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(subs_be_t2026, substance_control_kernel__prohibition_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_kernel__prohibition_reading, suppression_requirement, 1970, 0.62).
narrative_ontology:measurement(subs_su_t1985, substance_control_kernel__prohibition_reading, suppression_requirement, 1985, 0.71).
narrative_ontology:measurement(subs_su_t2000, substance_control_kernel__prohibition_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(subs_su_t2012, substance_control_kernel__prohibition_reading, suppression_requirement, 2012, 0.83).
narrative_ontology:measurement(subs_su_t2020, substance_control_kernel__prohibition_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(subs_su_t2026, substance_control_kernel__prohibition_reading, suppression_requirement, 2026, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, black_market_violence_externality).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, incarceration_collateral_sanctions).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, drug_trafficking_organization_market_power).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_kernel. The kernel represents the standing societal commitment to regulate substance use; this reading interprets that regulation as punishment for moral transgression. The harm_reduction_reading and legalization_reading instantiate the same kernel under different normative commitments, producing different constraint structures, beneficiary/victim sets, and extractiveness values. The three readings are NOT one constraint viewed from different angles—they are three structurally distinct constraints that share a contested kernel. Each has its own ε, its own stakeholder configuration, and its own type classification. The prohibition_reading's ε (0.81, high extraction via punishment) differs sharply from harm_reduction_reading's ε (lower, because health intervention is the mechanism, not punishment) and legalization_reading's ε (distinct again, because market regulation is the mechanism). The readings are linked via network.affects_constraints to enable contamination analysis: if decriminalization (legalization_reading) gains institutional traction, it creates structural pressure on the prohibition_reading by changing both the political legitimacy conditions and the enforcement resource availability for punishment-based systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
