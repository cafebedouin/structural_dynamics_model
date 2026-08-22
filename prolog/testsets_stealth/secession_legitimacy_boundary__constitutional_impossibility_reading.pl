% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession — Amendment-Only Exit
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   This story instantiates the constitutional-impossibility reading of
 *   federal secession law: a unilateral declaration of exit by a constituent
 *   region is void, and the only legitimate path out of the federation is a
 *   negotiated constitutional amendment commanding supermajority consent. The
 *   setting is a resource-politics federation in which one region is a
 *   persistent net fiscal contributor through resource royalties and
 *   taxation, and in which transfer-receiving provinces each hold a consent
 *   role in the amendment gate. From this reading's seat the constraint is
 *   claimed as a structural feature of federal sovereignty — a federation
 *   whose members could leave unilaterally would not have enforceable mutual
 *   obligations — and it is actively maintained: the apex court voids
 *   extra-amendment exit procedures, and the federal government controls the
 *   amendment agenda. The claim and the metrics are independent authored
 *   facts: the metrics below describe a contested, actively enforced
 *   arrangement whose sanctioned exit channel carries a rising performative
 *   share; the engine computes per-seat classifications from the structural
 *   data and measures any divergence from the claim.
 *
 * KEY AGENTS:
 *   - federal_government: primary beneficiary and political agenda-setter (institutional/arbitrage) — secures territorial integrity, revenue base, and resource authority; controls the amendment agenda
 *   - apex_constitutional_court: doctrinal agenda-setter (institutional/arbitrage) — authors and maintains the exclusivity doctrine; every escalation arrives as a reference question
 *   - transfer_recipient_provinces: secondary beneficiaries (organized/constrained) — transfer flows preserved, amendment veto held
 *   - resource_export_industry: dual-positioned beneficiary/payer (powerful/mobile) — market access secured, regulatory burden borne, capital partly mobile
 *   - secessionist_region_residents: principal cost-bearing seat (moderate/constrained) — collective exit foreclosed short of supermajority consent
 *   - secessionist_provincial_government: organized cost-bearing seat (organized/constrained) — mandate foreclosed, escalates politically
 *   - treaty_holder_nations: excluded seat (organized/trapped) — consent claim outside the gate's coalition math
 *   - comparative_federalism_scholars: analytical observer (analytical/analytical) — outside attestation on the founding problem and its status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.2).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.42).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession — Amendment-Only Exit").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).
domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '6714f96b-fd12-41f9-b2de-098f6bffed6b').
narrative_ontology:cs_kernel_codification('6714f96b-fd12-41f9-b2de-098f6bffed6b', fixed_text).
narrative_ontology:cs_authority_grounding('6714f96b-fd12-41f9-b2de-098f6bffed6b', lineage).
narrative_ontology:cs_interpretation_layer_present('6714f96b-fd12-41f9-b2de-098f6bffed6b').
narrative_ontology:cs_reading_relation('6714f96b-fd12-41f9-b2de-098f6bffed6b', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('6714f96b-fd12-41f9-b2de-098f6bffed6b', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('6714f96b-fd12-41f9-b2de-098f6bffed6b', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('6714f96b-fd12-41f9-b2de-098f6bffed6b', foundational, unilateral_secession_constitutionally_void).
narrative_ontology:cs_axiom_status(unilateral_secession_constitutionally_void, holdable).
narrative_ontology:cs_axiom_grounding('6714f96b-fd12-41f9-b2de-098f6bffed6b', unilateral_secession_constitutionally_void, conventional).
narrative_ontology:cs_axiom('6714f96b-fd12-41f9-b2de-098f6bffed6b', foundational, amendment_channel_sole_legitimate_exit).
narrative_ontology:cs_axiom_status(amendment_channel_sole_legitimate_exit, holdable).
narrative_ontology:cs_axiom_grounding('6714f96b-fd12-41f9-b2de-098f6bffed6b', amendment_channel_sole_legitimate_exit, conventional).
narrative_ontology:cs_reference_frame('6714f96b-fd12-41f9-b2de-098f6bffed6b', constitutional_supremacy_amendment_exclusivity).
narrative_ontology:cs_drift_state('6714f96b-fd12-41f9-b2de-098f6bffed6b', contemporary_secessionist_mobilization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6714f96b-fd12-41f9-b2de-098f6bffed6b', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, transfer_recipient_provinces).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_export_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_export_industry).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_region_residents).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_provincial_government).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, negotiated_amendment_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order and controls the amendment agenda: it drafts amendment resolutions, litigates secession references, and enjoins or prosecutes exit procedures attempted outside the sanctioned channel. It collects the taxes and resource royalties whose continued flow the prohibition secures, and funds the transfers the recipient provinces receive. Its way out of the constraint is restructuring the federation on terms it sets — devolution, fiscal concessions, new amendment packages — rather than permitting unilateral dissolution.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, beneficiary).

% Interprets the constitutional text and has authored the doctrine that a unilateral declaration of exit is void and that exit runs only through a negotiated amendment commanding supermajority consent. Its authority rests on the order it interprets; it can revisit doctrine at the cost of precedent and institutional legitimacy, and every secessionist escalation arrives at its door as a reference question.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, apex_constitutional_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive equalization and program transfers partly funded by the net-contributing region whose exit the prohibition forecloses. Each holds a consent role in the amendment gate, giving every one of them a veto over the region's exit. Their alternative to the arrangement is a smaller transfer pool and renegotiated federation terms.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, transfer_recipient_provinces, beneficiary,
    organized, generational, constrained, regional).

% Produces and moves the region's resource output through federally regulated pipelines into a national market with a common currency; the prohibition preserves that market access. It bears federal regulatory and climate-policy costs it cannot escape by regional exit, though it can partially relocate head offices and routing — a mobility its residents lack. It lobbies both orders of government and funds constitutional litigation on both sides.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_export_industry, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_export_industry, payer).

% Live in the region whose collective exit the constraint forecloses short of supermajority consent. They vote in referendums, fund the secessionist movement, and bear the gap between their region's expressed preference and the outcome the sanctioned channel can deliver. Individual emigration is available to some and dissolves rather than satisfies the claim; collective exit requires consent from the federation being exited.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_region_residents, payer,
    moderate, biographical, constrained, regional).

% Governs the region and carries exit as its central mandate: it organizes referendums, litigates the boundary's content, and petitions for amendment. Each escalation meets the court's doctrine and the federal government's control of the amendment agenda. Its alternatives are political escalation ending in legal defeat, or accommodation within the federation it campaigns to leave.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_provincial_government, payer,
    organized, biographical, constrained, regional).

% Hold pre-constitutional treaty relationships and assert that their consent conditions any change to the region's boundaries or status. The amendment gate this constraint authorizes does not list their consent among its required components. They cannot exit the constitutional order that asserts jurisdiction over them, and their claim to have predated it finds no register in the gate's coalition math.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, treaty_holder_nations, excluded,
    organized, civilizational, trapped, regional).

% Study which federal arrangements persist, how amendment gates behave under exit pressure, and what the fiscal and legal record shows about negotiated versus unilateral dissolutions. Both the court's references and the secessionist movement's briefs cite their work; they hold no stake in the outcome, and their attestations are the constraint's main outside check.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, comparative_federalism_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, diffuse).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single settled channel for changing the federation's composition: any exit must be negotiated into a constitutional amendment commanding supermajority consent, so that debt allocation, borders, currency, treaty claims, and resource royalties are settled cooperatively rather than unilaterally.
% TRANSFER_FUNCTION: Moves exit-decision authority from provincial majorities to the supermajority amendment gate — in practice, from the secessionist region to the federation as a whole — and thereby secures the continued flow of the region's resource revenues and fiscal contributions into the federal framework.
% ABSENT_VOICES: Treaty-holder nations would condition any boundary change on adjudication of their pre-constitutional claims; their consent is not a component of the amendment gate this reading authorizes. Within the region, residents whose exit preference cannot aggregate to the required supermajority are present at the ballot box but absent from the outcome set. Both seats sit outside the amendment coalition this constraint recognizes.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, the region's unilateral exit would become legally operative: fiscal transfers, debt allocation, currency arrangements, resource-royalty regimes, and the amendment veto structure would all be renegotiated under duress, and the federation's other members would race to secure their claims before exit completed. The arrangement's participants exist and their arrangements depend on it.
% FOUNDING_PROBLEM: The constitution's framers faced a union whose mutual obligations — shared debt, common defense, a single currency, an integrated market — would be unenforceable if any member could dissolve its ties unilaterally. The founding problem was to make the union's composition a matter of collective constitutional decision rather than unilateral withdrawal.
% FOUNDING_PROBLEM_CORROBORATION: Comparative federalism scholarship and the constitutional practice of peer federations corroborate that the continuity problem is real and unresolved: no federation with a unilateral-exit right has persisted, and negotiated dissolutions required exactly the agreement the gate exists to produce. The secessionist region's government contests both the problem's liveness and the gate's adequacy, and treaty-holder nations dispute that the founding settlement ever bound them; both attestations come from outside the benefiting parties and are recorded as dissent, not corroboration.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.20) because this reading assesses the standing arrangement as reciprocal coordination: the region receives currency, defense, market access, and transfers, and the reading holds the extraction claim against it invalid. Suppression (0.42) records the enforcement machinery that does exist — judicial voiding of unilateral declarations, prosecution posture toward extra-legal referendum machinery, federal control of the amendment agenda — short of the pervasive coercion a fully suppressive arrangement would show, because a sanctioned channel remains open. Theater_ratio (0.28) is the honest descriptive trend the reading must explain rather than endorse: the amendment gate functions for ordinary amendments but has never opened for exit, so the performative share of the 'negotiated exit is available' framing has risen across the interval. Accessibility_collapse (0.78) is high because once the doctrine is understood, unilateral alternatives collapse as legal options and only the gate remains. Resistance (0.58) records sustained secessionist organizing, referendums, and litigation — descriptively true even though the reading holds that resistance categorically unsuccessful. All three series share one time grid (T=0,5,10,15,20,25,30). Suppression_requirement is authored because the story specifically tracks enforcement-capacity change: a U-shaped trajectory (post-referendum hardening, concession-era relaxation, re-hardening under renewed resource-politics alienation) driven by the crisis-concession-relaxation-accumulation cycle rather than monotonic drift; base_extractiveness and theater_ratio drift gently upward across the same grid as the region's net contribution grows and the gate's unopened history lengthens.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute differently from the same structural facts. From the federal government and the court, the constraint is the constitutional order itself — the thing that makes mutual obligations enforceable — and its costs are invisible because they are constitutive. From the region's residents and its government, the same structure is a gate their majority preference cannot pass, and the gate's supermajority requirement includes the very federation being exited. The recipient provinces occupy a third position: pure veto-holders whose consent is courted but whose costs are diffuse. The excluded treaty-holder seat experiences the constraint as bypass — a boundary process that proceeds without the consent they hold to be a precondition. The engine computes these per-seat divergences from power, exit, and role data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared where the constraint's operation demonstrably preserves flows: the federal government (revenue base, resource authority, territorial integrity), transfer-receiving provinces (transfer pool plus amendment veto), and the resource-export industry (integrated market, common currency). These declarations derive low directionality for those seats. The region's residents and its provincial government are declared payers — they bear the constraint's operative cost, the foreclosed unilateral exit — and derive high directionality. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the national scope at which the gate operates. The gain_flow field is authored 'diffuse' as an affirmative, checked claim of this reading: every named seat was examined, and none captures the constraint's gains, because the reading holds there is no extraction to capture — the flows the prohibition secures are lawful revenues and reciprocal transfers within its framework, and its benefits are held to reach every member including the region. That diffuse claim is precisely what the rival framings contest; the concentration question is routed to the omegas rather than resolved here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making a union's mutual obligations enforceable against unilateral withdrawal — is authored live, and the disappearance verdict is world_rearranges, so the R5 mismatch check (dead status plus world_rearranges) does not fire. The classification work this story does is boundary-keeping in both directions: a pure-extraction computation driven by the payer seats' high directionality would import an extraction claim this reading's framework explicitly rejects — it holds the flows lawful and reciprocal — so the low authored epsilon and the absent victim set are the load-bearing declarations. Conversely, the false-summit probe stays armed: this is a mountain claim with declared beneficiaries, and the naturality omega keeps the constructed-constraint alternative open rather than letting the categorical framing certify itself. The rising theater series is monitored as the early mandatrophy signal: if the amendment gate's performative share keeps climbing while no exit ever passes it, the 'negotiated exit' half of the constraint drifts toward maintained performance, and the constraint's classification would migrate even while the categorical half holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_vs_structural_prohibition,
    'Is the categorical prohibition on unilateral secession a structural feature of any durable federation (mountain-like), or a constructed constraint that benefits identifiable agents — the federal government, transfer-receiving provinces — at the secessionist region''s expense?',
    'Comparative federation analysis: examine whether any federation permitting unilateral member exit has persisted, controlling for the confound that voluntary unions are self-selected on exit costs; test whether the prohibition survives in counterfactual federation designs without concentrated beneficiaries.',
    'If constructed, the false-summit signature reclassifies this reading''s mountain claim toward a hybrid coordination/extraction type and the beneficiary set becomes load-bearing; if structural, the mountain claim stands and the declared beneficiaries are legitimate co-beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_structural_prohibition, conceptual, 'Whether the prohibition is natural federal structure or a constructed constraint with identifiable beneficiaries.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the secession_legitimacy_boundary kernel — the constitutional_impossibility_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'No intra-framework resolution exists: the readings are rival completeness claims about the source of exit legitimacy. Resolution occurs politically — amendment success or failure, court composition, referendum outcomes — not analytically within this story.',
    'Adopting the popular_sovereignty_reading or the grievance_threshold_reading would convert this story''s beneficiary set into a victim set and raise epsilon sharply; adopting the treaty_primacy_reading would add a consent condition this reading''s exclusivity axiom cannot accommodate. This story''s epsilon (0.20) and absent victim set are reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a four-reading kernel; the disagreement is located at the source of exit legitimacy.').

omega_variable(
    amendment_gate_reachability,
    'Is the amendment channel a genuine exit path or practically unreachable, given that it requires supermajority consent including the federation being exited — a collective-action bind?',
    'Comparative amendment history: identify any federation in which the amendment gate has actually opened for member exit, and measure the threshold''s effective probability against observed amendment success rates for ordinary matters.',
    'If unreachable, the theater_ratio is understated and the constraint operates as pure prohibition with a nominal exit — moving computed types toward extraction-flavor for the payer seats; if genuine, the coordination framing holds and the reading''s low-epsilon assessment is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gate_reachability, empirical, 'Whether the sanctioned exit channel is real or nominal.').

omega_variable(
    fiscal_net_position_ambiguity,
    'Does the secessionist region''s net fiscal position actually involve outflow (which would make the prohibition extractive in effect), or is the net position ambiguous once services, debt, currency, and defense are counted?',
    'Independent fiscal-balance accounting: per-capita revenue versus expenditure, debt attribution, capital flows, and valuation of the federal services the region receives.',
    'A robust net outflow would raise effective extraction for the region''s residents and substantiate the rival framings'' victim claims against this reading''s no-victim structure; a balanced or inflow position supports this reading''s beneficiary-only declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_net_position_ambiguity, empirical, 'The region''s net fiscal position under the standing arrangement.').

omega_variable(
    internalized_prohibition_stability,
    'Is the prohibition''s stability maintained by enforcement machinery, or by internalized constitutional identity — the region''s own political elites treating unilateral action as categorically unavailable?',
    'Post-referendum behavior analysis: track whether secessionist elites pursue the amendment channel after defeats (internalization) or shift to extra-legal instruments when enforcement capacity relaxes (enforcement-dependence).',
    'If internalized, suppression persists even as enforcement relaxes, and the falling middle of the suppression series overstates the constraint''s erosion; if enforcement-dependent, the U-shaped enforcement series is the load-bearing variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_prohibition_stability, empirical, 'Structural versus internalized maintenance of the prohibition.').

omega_variable(
    treaty_consent_blind_spot,
    'Does the constraint''s operation bypass treaty-holder consent claims in a way this reading''s framework cannot register, and would registering it break the exclusivity axiom?',
    'Litigation and amendment practice: test whether amendment processes touching the region trigger a duty to consult treaty holders, and whether any court reads treaty obligations as conditions on exit.',
    'Recognition would add a consent condition the amendment-exclusivity axiom cannot accommodate without revision — the reading would either absorb the condition into the gate (axiom survives, narrowed) or confront its first genuine counter-instance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_consent_blind_spot, conceptual, 'Whether treaty consent is a structural blind spot of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 25, 0.19).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'secession legitimacy' decomposes into at least four structurally distinct constraints — this file plus the three sibling readings of the secession_legitimacy_boundary kernel — each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates the constitutional-impossibility reading only: the constitutional text as the exclusive source of exit legitimacy. The sibling files are linked here as one kernel family; epsilon differs across members because each reading assesses the same standing arrangement by different lights, and this reading's exclusivity premise is the structural element on which all three siblings disagree.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
