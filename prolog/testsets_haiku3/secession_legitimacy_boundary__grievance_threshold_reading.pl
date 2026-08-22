% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy via Grievance Threshold (Structural Injustice Reading)
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   The grievance-threshold reading of secession legitimacy claims that
 *   unilateral provincial exit becomes morally and politically legitimate
 *   when federal actions cross a threshold of structural injustice,
 *   regardless of what the written constitution says. This reading competes
 *   within a contested kernel: the same federation that other readings defend
 *   via constitutional text, popular sovereignty within provincial
 *   boundaries, or indigenous treaty authority is here reframed as
 *   conditionally legitimate only if its federal actions remain below the
 *   injustice threshold. The reading provides a principled framework (not
 *   just naked preference) for exit: there is a criterion, not arbitrary
 *   will. But the criterion itself is ambiguous — what counts as structural
 *   injustice, who decides, and how is the threshold measured? The constraint
 *   coordinates a coalition of aggrieved populations around the promise that
 *   legitimate exit is possible if they can demonstrate threshold-crossing;
 *   it extracts from federal authority (which loses its territorial integrity
 *   shield once the threshold narrative takes hold) and from inter-provincial
 *   solidarity (which dissolves if exit becomes individually rational). The
 *   enforcement machinery is substantial: movements must sustain the
 *   threshold narrative against federal contestation, international arbiters
 *   must evaluate threshold-crossing claims, and federal authorities must
 *   either reform or suppress the exit attempt.
 *
 * KEY AGENTS:
 *   - provincial_independence_movement_leadership: agenda-setter, institutional power, sets and frames the threshold narrative
 *   - aggrieved_regional_populations: beneficiary and payer, organized power, experience the coordination of exit legitimacy while bearing political/economic costs
 *   - federal_authority_holders: payer, institutional power, lose territorial integrity claims once threshold narrative takes hold
 *   - inter_provincial_solidarity_coalitions: payer, organized power, exit incentives fractured by threshold-crossing pressure
 *   - international_arbiters: agenda-setter and observer, institutional power, evaluate threshold-crossing claims and issue or withhold recognition
 *   - constitutional_literalists: excluded, organized power, contest the reading's foundational premise
 *   - indigenous_peoples_and_treaty_holders: excluded, powerless, territorial and sovereignty interests may be overridden by threshold dynamics without their voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy via Grievance Threshold (Structural Injustice Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26').
narrative_ontology:cs_kernel_codification('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', formalized).
narrative_ontology:cs_authority_grounding('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', distributed).
narrative_ontology:cs_reading_relation('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', secession_legitimacy_boundary__popular_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', foundational, structural_injustice_overrides_constitutional_form).
narrative_ontology:cs_axiom_status(structural_injustice_overrides_constitutional_form, holdable).
narrative_ontology:cs_axiom_grounding('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', structural_injustice_overrides_constitutional_form, deontological).
narrative_ontology:cs_axiom('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', foundational, legitimacy_conditional_on_federal_compliance_threshold).
narrative_ontology:cs_axiom_status(legitimacy_conditional_on_federal_compliance_threshold, holdable).
narrative_ontology:cs_axiom_grounding('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', legitimacy_conditional_on_federal_compliance_threshold, instrumental).
narrative_ontology:cs_reference_frame('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', federal_legitimacy_contingent_on_justice).
narrative_ontology:cs_drift_state('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', contemporary_post_civil_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae9ecf6a-f37f-420c-9b1c-f6e1acd5aa26', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, provincial_independence_movements).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regional_populations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, inter_provincial_solidarity_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regional_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frames and articulates grievances against federal authority; organizes political, legal, and cultural machinery for secession; claims legitimacy threshold has been crossed. Bears the constraint's enforcement costs (legal challenges, federal suppression, international diplomatic isolation). Sets the narrative of what constitutes 'structural injustice' and when the threshold has been reached.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_independence_movement_leadership, agenda_setter,
    institutional, generational, trapped, national).

% Those who experience federal policies as extractive or oppressive: discriminatory resource allocation, linguistic/cultural suppression, fiscal exploitation, or subordination in federal decision-making. Benefit from the legitimacy claim that secession becomes permissible when grievances reach threshold severity. Also bear costs: political polarization, economic disruption risk, inter-community violence potential during secession crisis. The constraint coordinates their collective exit narrative while exacting enforcement and conflict costs.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regional_populations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regional_populations, payer).

% Central government institutions and their defenders. The constraint's operation extracts legitimacy and authority: if federal actions cross the threshold, the constraint assigns secession moral and political permission, undercutting federal territorial integrity claims. They must either reform (absorbing the grievances the threshold describes) or suppress the exit attempt (reinforcing the narrative of federal injustice). No clean exit from the dilemma once the threshold narrative takes hold.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority_holders, payer,
    institutional, generational, constrained, global).

% Other provincial or regional populations who might benefit from federal reforms but are harmed by secession fragmentation. If a province exits via the grievance threshold, it breaks the solidarity coalition and may trigger competitive grievance-claiming by other regions. The constraint extracts from their coordination by converting inter-regional common cause into provincial competitive exit positioning.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, inter_provincial_solidarity_coalitions, payer,
    organized, biographical, constrained, national).

% International legal bodies, regional organizations, and peer states that evaluate secession legitimacy claims. Under this reading, they must assess whether federal actions crossed an 'objective' threshold of structural injustice — a high-friction judgment that makes them de facto arbiters of internal political boundaries. They enforce or withhold recognition based on perceived threshold attainment, which shapes the constraint's credibility.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_arbiters, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, international_arbiters, observer).

% Those who hold that constitutional text forecloses unilateral secession entirely, regardless of grievance severity. They would argue the threshold reading replaces law with grievance politics and destabilizes the federation. Structurally excluded from the grievance threshold framing because their foundational premise (constitutional form constrains legitimate exit) is what this reading explicitly overrides.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_literalists, excluded,
    organized, generational, trapped, national).

% Indigenous nations with treaty rights or inherent sovereignty that predate the federal structure. Under this reading, they are excluded: the grievance threshold operates on the logic of provincial/regional populations within the federal frame, not on treaty authority. Their exclusion from the threshold framing means the constraint may override their territorial and sovereignty interests without their consent or voice in threshold determination.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_peoples_and_treaty_holders, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, provincial_independence_movement_leadership).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled framework for when unilateral exit becomes legitimate: a federal arrangement that systematically extracts from or oppresses a region loses its moral claim to territorial integrity. The coordination problem is how to make territorial exit admissible without making the federation incoherent — the answer offered is: structure it on threshold-crossing, not permanent right.
% TRANSFER_FUNCTION: Transfers political legitimacy and moral standing for secession from federal authority (the constitutional monopoly on territorial integrity) to the aggrieved regional population (only when structural injustice threshold is demonstrably crossed). Also transfers enforcement costs: those seeking to invoke the threshold must sustain burden-of-proof for structural injustice; federal actors bear suppression costs; inter-provincial coalitions lose internal solidarity.
% ABSENT_VOICES: Constitutional literalists and indigenous treaty holders are structurally excluded. Constitutional literalists would argue this reading replaces law with grievance politics; treaty holders would claim the threshold framework ignores their prior territorial and sovereignty claims. Both are kept out by the logic of the reading itself — one because it contests the reading's foundational premise, the other because the reading operates inside the federal frame without recognizing non-federal legitimacy sources.
% DISAPPEARANCE_RATIONALE: If this constraint (the grievance-threshold legitimacy claim) disappeared overnight, federal territorial integrity would return to constitutional text as the sole legitimacy anchor — secession would be constitutionally impermissible regardless of federal overreach. Regions experiencing structural injustice would lose their primary political leverage for exit negotiation. The federal arrangement would either calcify (locked in by text) or democratize (constitutional amendment becomes the only route). Inter-provincial dynamics would shift from competitive exit positioning to coalition-based reform pressure.
% FOUNDING_PROBLEM: Federations built on constitutional text that forecloses unilateral exit leave oppressed or systematically exploited regions with no legitimate exit option short of revolutionary rupture. The problem is the mismatch: constitutional form assumes federal legitimacy flows from consent, but offers no consent-withdrawal mechanism when consent is withdrawn by majority vote of a constituent population.
% FOUNDING_PROBLEM_CORROBORATION: Aggrieved regional populations and independence movements attest the founding problem is live and unresolved. Constitutional scholars outside the independence movement (and critical of federal policy) acknowledge the mismatch between federalism-as-consent theory and text-based exit foreclosure. Federal authorities contest the premise, arguing constitutional amendment is the existing consent-withdrawal mechanism. International law scholars are divided: some cite the problem as real, others defend text-based boundaries. No consensus corroboration exists — the founding problem itself is contested terrain.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68) reflects the reading's core mechanism: it transfers legitimacy from federal constitutional text to the grievance narrative, which is a high-stakes transfer benefiting secessionist movements at federal expense. Suppression is higher (0.71) because the constraint's persistence depends on active enforcement: federal authorities must suppress secessionist organizing to prevent threshold-crossing, movements must sustain organizational coherence against federal pressure, international arbiters must withstand diplomatic pressure to recognize or deny exit. Theater is moderate (0.42) because the reading does articulate a real procedural claim — threshold-crossing is alleged as objective, not merely preferential — but this theatricality is structural to the reading's operation: once the threshold narrative is established, even purely theatrical invocation of it shifts legitimacy dynamics. The measurement series show rising extractiveness (0.45 to 0.68 over the interval) as the reading's influence spreads from theoretical framing to lived political pressure; theater and suppression rise in parallel. This suggests the constraint's extraction intensifies as movements gain organizational capacity and international attention, requiring correspondingly higher federal suppression to contain it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and beneficiary seats compute this constraint as opposite types: from the movement's position, it is coordination (principled framework for exit, collective action enabled). From the federal seat, it is pure extraction (territorial authority stripped away by external threshold judgment). From the inter-provincial seat, it is a snare (the coordination mechanism — common federal reform — is replaced by competitive exit pressure, trapping each region in individual-rationality dynamics). The engine's per-seat classification should diverge sharply: tangled-rope or rope from the movement seat (coordination + some asymmetry), snare or pure-extraction from the federal seat. These divergences are constitutive of the reading, not errors to be reconciled away.
 *
 * DIRECTIONALITY LOGIC:
 *   The seated differences are stark: from the provincial movement's seat, the constraint coordinates legitimate exit and represents justice against federal overreach (low d, beneficiary directionality). From the federal authority seat, the same constraint is an existential threat to territorial integrity and the rule of law (high d, full target). From the inter-provincial coalition seat, it is a commons-tragedy mechanism that converts collective federal reform efforts into individual exit races (high d, extraction by Schelling-point concentration). From the international arbiter seat, it is a legitimacy-judgment framework they are now forced to operate — high administrative burden, liability for recognition errors (moderate-to-high d). The constitutional literalist and indigenous peoples seats are excluded entirely from the directionality computation because they are not in the conversation the constraint coordinates — they are structurally outside its frame. The derivation chain produces these divergences directly: beneficiaries have mobile exit (can leave if threshold is crossed) and organized power (movements), yielding low d; victims (federal authority, inter-provincial coalitions) have constrained exit (cannot avoid the threshold claim's political force) and institutional power (but now weakened by the legitimacy transfer), yielding high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that constitutional text forecloses exit even under systematic federal injustice, trapping oppressed populations — is live in jurisdictions with secessionist movements or indigenous sovereignty claims. The mandate of the grievance-threshold reading is to provide a legitimacy path around that trap. However, as the constraint operationalizes (movements invoke it, international attention grows, federal resistance hardens), the reading's original mandated function (enabling principled exit) may atrophy into pure political positioning and federal extraction, where movements invoke the threshold not to achieve exit but to extract concessions, and federal authorities preemptively reform or suppress to avoid the legitimacy loss. The theater ratio (0.42) suggests this is already underway: a growing share of the constraint's enforcement machinery is maintaining the threshold narrative itself rather than enabling actual exits. The mandatrophy hypothesis: the founding problem is never fully solved (threshold-crossing is still contested), but the constraint persists because it benefits institutional actors (movements extract political leverage; federal authorities extract reform demands under threat of threshold invocation) while actual secessions remain rare. The mechanism shifts from 'legitimacy gate for exit' to 'negotiation leverage in federal-provincial bargaining,' which is mandatrophic — the founding problem is not resolved, merely managed theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_objectivity_ambiguity,
    'What constitutes ''structural injustice'' objectively, and who measures whether the threshold has been crossed?',
    'Establish prior agreement on metrics (resource allocation disparity, representation ratios, policy veto frequency, fiscal extraction rate) that operationalize structural injustice. Compare pre-announced thresholds against measured federal behavior. If threshold-crossing turns on inter-party judgment rather than pre-set metrics, the ambiguity persists.',
    'If objectivity cannot be operationalized, the threshold becomes a post-hoc justification for secession (gutting the reading''s principled character). If objectivity IS established, federal actors can point to compliance and undercut the legitimacy claim. The reading''s coherence depends on resolving this tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_objectivity_ambiguity, conceptual, 'Whether structural injustice can be measured objectively or becomes a contested political judgment.').

omega_variable(
    federal_reform_route_preemption,
    'Does the grievance-threshold reading preempt federal reform as a legitimate path, or do aggrieved regions bear a burden to exhaust reform before invoking the threshold?',
    'Explicit articulation of sequencing: must reform efforts fail demonstrably before threshold is presumed crossed? If so, what counts as ''exhaustion''? If not, regions can invoke the threshold immediately upon identifying structural injustice, which accelerates secession dynamics.',
    'If reform preemption exists, the constraint preserves federal coherence by requiring internal first-pass. If it does not, the constraint becomes a shortcut to secession for movements that prefer exit to negotiation, decoupling legitimacy from remediation attempts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_reform_route_preemption, conceptual, 'Whether threshold invocation requires prior exhaustion of federal reform mechanisms.').

omega_variable(
    reading_kernel_boundary_ambiguity,
    'Is this reading truly distinct from the popular-sovereignty reading, or does ''structural injustice threshold'' collapse into ''whatever the regional democratic majority decides''?',
    'Test whether the threshold reading can reject a secession referendum result. If the regional majority votes for exit but federal analysis shows no structural injustice, does the threshold reading deny legitimacy? If yes, it is distinct. If it defers to the referendum regardless, it has become the popular-sovereignty reading in disguise.',
    'If the readings collapse into one another, the kernel decomposition was premature — the grievance threshold is epiphenomenal. If they remain distinct, the threshold reading claims to impose an objective filter on democratic preference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_boundary_ambiguity, conceptual, 'Whether the grievance threshold is a genuine constraint on democratic secession or merely a narrative wrapper around popular sovereignty.').

omega_variable(
    inter_provincial_cascade_risk,
    'Does successful invocation of the threshold by one region create demonstrable pressure for other regions to claim threshold-crossing and exit, even absent comparable structural injustice?',
    'Post-secession analysis: track whether other regions dramatically increase independence movement activity and threshold-crossing rhetoric. Compare to baseline. If activity spikes, the reading has a cascade dynamic that transforms from constraint (legitimacy gate) to incentive (race to exit).',
    'If cascade occurs, the constraint''s long-term effect may be federation dissolution rather than calibrated exit for objectively aggrieved populations. The reading''s sustainability depends on threshold credibility withstanding cascade pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_provincial_cascade_risk, empirical, 'Whether successful threshold-crossing by one region triggers competitive exit pressure in others.').

omega_variable(
    indigenous_authority_void,
    'How does the grievance-threshold reading interact with indigenous treaty rights and pre-federal sovereignty claims that the reading does not acknowledge?',
    'Explicit engagement with indigenous territorial and sovereign authority: does structural injustice threshold apply equally to indigenous nations within federal boundaries? Can indigenous majority territories invoke it? If yes, does federal authority still bind them? If no, the reading implicitly subordinates indigenous sovereignty to the federal frame.',
    'If the void is unresolved, the reading may override indigenous interests without their consent, embedding a second-order extraction (federal authority over indigenous nations). If the reading explicitly incorporates indigenous authority recognition, its structure changes substantially — it becomes not just a secession legitimacy theory but a multi-sovereignal renegotiation theory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_authority_void, conceptual, 'Whether the grievance-threshold reading accounts for indigenous sovereignty or implicitly subordinates it to the federal frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__grievance_threshold_reading, 0.14).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority_legitimacy_base).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, inter_provincial_solidarity_mechanism).

% DUAL FORMULATION NOTE:
% This story is one reading of the secession_legitimacy_boundary kernel. The other readings (constitutional_impossibility, popular_sovereignty, treaty_primacy) are separate constraint stories with different ε values, different beneficiary/victim structures, and different computed per-seat types. The kernel decomposition is governed by ε-invariance: this reading's ε (0.68) measures extraction under the grievance-threshold framing; a sibling reading's ε would measure extraction under its framing of the same federal arrangement. They are not the same constraint viewed from different angles — they are different constraints whose empirical referent is the same federal structure. Link them via network.affects_constraints to enable cross-reading contamination analysis and kernel-stability assessment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__grievance_threshold_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
