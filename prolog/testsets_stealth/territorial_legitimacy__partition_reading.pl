% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Partition-Legitimacy Framework for Territorial Sovereignty (UN 181 / Green Line Reading)
 *   domain: political/international_law
 *
 * SUMMARY:
 *   The partition-legitimacy framework holds that sovereign legitimacy in the
 *   contested territory flows from international legal partition and mutual
 *   state recognition: UN Resolution 181 as founding text, the 1949 armistice
 *   lines as default borders, construction beyond them as illegitimate, and a
 *   two-state consummation as the framework's promised endpoint. This file
 *   instantiates ONE reading of the territorial_legitimacy kernel; the
 *   security-necessity and indigenous-continuity readings are separate
 *   constraints with their own epsilon and victim sets, linked via
 *   network.affects_constraints. The epsilon referent is the standing
 *   arrangement under contest — the recognition architecture itself —
 *   assessed by this reading's own lights: the framework confers real goods
 *   (treaties, recognition, a statehood path) while pricing specific claims
 *   (refugee return, settler tenure, rejectionist participation) as its
 *   operating costs. Claimed type and metrics are authored independently: the
 *   claim is tangled_rope because the structure demonstrably coordinates AND
 *   asymmetrically charges; the metrics describe eight decades of widening
 *   delivery-gap and accumulating process-theater. KEY AGENTS (by structural
 *   relationship): - un_system_bodies: agenda-setting custodian
 *   (institutional/constrained) — administers the framework's declaratory
 *   machinery - us_eu_major_powers: enforcement principals
 *   (institutional/mobile) — condition aid and standing on framework
 *   adherence - palestinian_authority_establishment: recognition-dependent
 *   beneficiary (organized/trapped) - israeli_state_establishment:
 *   dual-positioned beneficiary-payer (institutional/constrained) -
 *   international_mediation_establishment: concentrated recipient of the
 *   framework's operational rents (organized/constrained) -
 *   post_1967_settler_communities: primary territorial payer
 *   (organized/trapped) - refugee_descendant_communities: claim-subordination
 *   payer (powerless/trapped) - rejectionist_factions: excluded challengers
 *   (organized/identity_locked) - binational_civilian_populations: diffuse
 *   payers and prospective beneficiaries (moderate/constrained) -
 *   international_law_academy: analytical observer (moderate/analytical)
 *
 * KEY AGENTS:
 *   - un_system_bodies: agenda-setting custodian (institutional/constrained)
 *   - us_eu_major_powers: enforcement principal (institutional/mobile)
 *   - palestinian_authority_establishment: recognition-dependent beneficiary (organized/trapped)
 *   - israeli_state_establishment: dual-positioned beneficiary-payer (institutional/constrained)
 *   - international_mediation_establishment: concentrated rent recipient (organized/constrained)
 *   - post_1967_settler_communities: primary territorial payer (organized/trapped)
 *   - refugee_descendant_communities: claim-subordination payer (powerless/trapped)
 *   - rejectionist_factions: excluded challenger (organized/identity_locked)
 *   - binational_civilian_populations: diffuse payer and prospective beneficiary (moderate/constrained)
 *   - international_law_academy: analytical observer (moderate/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.66).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Partition-Legitimacy Framework for Territorial Sovereignty (UN 181 / Green Line Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/international_law").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, 'a368e2cf-7197-4dfe-a110-cbd11bf20239').
narrative_ontology:cs_kernel_codification('a368e2cf-7197-4dfe-a110-cbd11bf20239', fixed_text).
narrative_ontology:cs_authority_grounding('a368e2cf-7197-4dfe-a110-cbd11bf20239', lineage).
narrative_ontology:cs_interpretation_layer_present('a368e2cf-7197-4dfe-a110-cbd11bf20239').
narrative_ontology:cs_reading_relation('a368e2cf-7197-4dfe-a110-cbd11bf20239', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_reading_relation('a368e2cf-7197-4dfe-a110-cbd11bf20239', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a368e2cf-7197-4dfe-a110-cbd11bf20239', foundational, recognized_partition_confers_sovereignty).
narrative_ontology:cs_axiom_status(recognized_partition_confers_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('a368e2cf-7197-4dfe-a110-cbd11bf20239', recognized_partition_confers_sovereignty, conventional).
narrative_ontology:cs_axiom('a368e2cf-7197-4dfe-a110-cbd11bf20239', secondary, green_line_default_border_presumption).
narrative_ontology:cs_axiom_status(green_line_default_border_presumption, holdable).
narrative_ontology:cs_axiom_grounding('a368e2cf-7197-4dfe-a110-cbd11bf20239', green_line_default_border_presumption, conventional).
narrative_ontology:cs_reference_frame('a368e2cf-7197-4dfe-a110-cbd11bf20239', partition_recognition_baseline).
narrative_ontology:cs_drift_state('a368e2cf-7197-4dfe-a110-cbd11bf20239', contemporary_post_oslo_stagnation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a368e2cf-7197-4dfe-a110-cbd11bf20239', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_authority_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_mediation_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, post_1967_settler_communities).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, refugee_descendant_communities).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, rejectionist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, un_system_bodies).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, us_eu_major_powers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, binational_civilian_populations).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_authority_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_state_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, binational_civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passes the resolutions that reaffirm the partition-and-recognition formula, convenes negotiation conferences, and runs mandates (refugee relief, special coordinators) premised on an eventual two-state consummation. It cannot compel member states; its instruments are declaratory texts, budgets, and convening power. Stepping off the formula would mean repudiating the organization's own self-determination commitments, so its posture is locked even as implementation stalls.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_system_bodies, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, un_system_bodies, beneficiary).

% Condition military aid, reconstruction funding, and diplomatic standing on adherence to the recognition formula, and periodically waive enforcement for allies. They alternate between upholding the 1967-line presumption and tolerating deviation, and retain the option of shifting weight to rival legitimacy doctrines — a lever they have pulled repeatedly across administrations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, us_eu_major_powers, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, us_eu_major_powers, beneficiary).

% Governs fragments of the occupied territory under the framework's grant of recognition, donor budget support, and treaty access. Its payroll, security coordination, and international standing all route through continued framing of the conflict as negotiable partition. Renouncing the formula would forfeit its external lifeline; delivering on it has become implausible, which corrodes its domestic standing with each passing year.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_authority_establishment, beneficiary,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_authority_establishment, payer).

% Holds the framework's largest conferred good: recognition of statehood within the 1949 lines, embedded in treaties with two neighbors. It simultaneously carries the framework's charges — censure resolutions, settlement-illegality rulings, conditionality threats, and litigation exposure for institutions beyond the lines. Its governing coalitions increasingly rest on voters who reject the formula the state formally affirms.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_state_establishment, payer).

% A professional ecosystem of special envoys, quartet staff, donor secretariats, track-two institutes, and peace-oriented NGOs whose funding cycles, career ladders, and institutional mandates are pegged to the formula remaining the operative plan. Output concentrates in conferences, roadmaps, and reports issued at anniversaries and crises; it holds no independent enforcement capacity, and its production volume rises as outcomes recede.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_mediation_establishment, beneficiary,
    organized, biographical, constrained, global).

% Live in towns and outposts built beyond the lines the formula treats as default borders. Homes, schools, and infrastructure there are classified by the framework as removable obstacles; any consummation entails evacuation or transfer to the other state. They respond with deep political entrenchment inside the Israeli system and steady construction on the ground, and cannot relocate without losing both assets and the ideological community that constitutes their life.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, post_1967_settler_communities, payer,
    organized, generational, trapped, regional).

% Descendants of the 1947-49 displaced hold registered claims to return that the formula sequences behind border agreement — a queue now in its eighth decade. Host-state legal regimes vary from citizenship to statelessness; the claim category is preserved administratively while its exercise is deferred indefinitely. Their redress exists only inside a framework that prices it as negotiable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, refugee_descendant_communities, payer,
    powerless, generational, trapped, global).

% Armed and electoral movements that deny any partition's validity hold governing power in Gaza and influence across the diaspora. The negotiation architecture bars them by its own admission test — participation requires prior acceptance of mutual recognition — so they address the framework only from outside, through veto-by-force. Their refusal is constitutive: abandoning it dissolves the movement's reason for being.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, rejectionist_factions, excluded,
    organized, generational, identity_locked, regional).

% Israelis and Palestinians alike absorb the conflict's recurring violence, conscription, closure economics, and war-risk premiums, regardless of anyone's position on the formula. A consummated settlement would hand them recognized borders, normalized movement, and regional integration; its absence hands them the next round. Polling in both societies shows declining belief that the promised endpoint will arrive in their lifetimes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, binational_civilian_populations, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, binational_civilian_populations, beneficiary).

% Scholars and practitioners who test the formula's doctrinal coherence, document the gap between resolution texts and ground facts, and supply opinions to courts and commissions. They neither collect the framework's goods nor bear its charges; their professional standing tracks the intensity of the disputes they analyze.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_law_academy, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, international_mediation_establishment).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a zero-sum sovereignty dispute between two national movements into a bounded border-allocation problem that third parties can recognize, fund, and mediate: each side renounces exclusive claim in exchange for recognized statehood within defined lines. Secondarily, stabilizes the general international rule that legitimate territory derives from law and recognition rather than conquest.
% TRANSFER_FUNCTION: Moves legitimacy goods — recognition, aid access, treaty capacity, diplomatic standing — to parties accepting the partition formula; moves renunciation costs onto parties whose claims exceed it: refugee return subordinated to border agreement, settlement projects exposed to evacuation, rejectionist movements denied standing.
% ABSENT_VOICES: Rejectionist armed factions are barred by the framework's own admission test (prior acceptance of mutual recognition); refugee communities sit outside the PLO's representation monopoly in practice; settler communities appear only as objects of removal provisions. Each would object from a different ground: no partition is legitimate; partition prices our return as negotiable; the lines drawn cut through our homes.
% DISAPPEARANCE_RATIONALE: Recognition decisions, aid disbursement formulas, Security Council resolution language, ICJ dockets, and both parties' diplomatic strategies all presuppose the framework. Overnight removal would force every actor onto a sibling ground — security necessity or indigenous continuity — with immediate consequences: annexation claims would lose their counter-baseline, refugee claims would lose their procedural home, and the mediation architecture would lose its mandate.
% FOUNDING_PROBLEM: Terminate the British Mandate without perpetual communal war by allocating sovereignty between two national movements claiming the same territory — UN Resolution 181's partition was the great-power answer proposed in 1947.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside all benefiting parties (Israeli, Palestinian, and international academies) corroborate that the Mandate-termination problem was real and that 181 was the proposed answer to it. On current status: rejectionist factions attest the allocation problem is unresolved while denying the framework addresses it; ICJ proceedings and General Assembly voting records independently document that the dispute persists. No party outside the framework's beneficiaries attests that the ORIGINAL problem (Mandate termination) remains live — that form is dead; the successor allocation problem is contested.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 at interval end) reflects the widening gap between the framework's promised goods (two recognized states) and its delivered ones, while its costs — subordinated refugee claims, evacuation-exposed settlements, gated participation — compound annually. Suppression (0.66) is the framework's enforcement intensity: non-recognition doctrine, aid conditionality, litigation exposure; it is authored as a raw structural property and is NOT scaled by power or scope — the engine owns that arithmetic. Theater (0.60) tracks Goodhart drift: anniversary diplomacy, unimplemented resolutions, and roadmap production substitute for outcomes. Accessibility collapse is low (0.40) because rival legitimacy grounds remain live — this is precisely a contested kernel, and the sibling readings persist as usable alternatives. Resistance (0.65) is sustained: settlement construction directly defies the 1967-line presumption, and rejectionist movements refuse the framework's admission test. The three measurement series share one ten-point grid (1947-2025); the Oslo-era dips in extraction and theater mark the interval's one period of visible delivery, and the post-2000 rise marks its loss. The suppression_requirement series is authored because enforcement-capacity change IS the traced dynamic here: the framework built its conditionality and litigation machinery progressively as delivery stalled. Coordination type is identity_coordination: the framework's dominant function is coordinating the boundary of legitimate statehood — membership claims adjudicated against evolving criteria — whose failure would most directly reproduce the original coordination problem (every territorial claim reverting to war).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (UN bodies, major powers) the framework computes as coordination they operate and fund; from the payer seats (settlers, refugees, rejectionists) the same structure computes as extraction with their claims as the priced input; the dual-positioned seats (Israel, the PA) straddle. The engine derives these per-seat classifications from the structural data; the divergence between the mediator seat's coordination experience and the refugee seat's extraction experience is the perspectival fact this story encodes, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the PA, the Israeli state establishment, and the mediation establishment; victim declarations drive high d for settler communities, refugee descendants, and rejectionist factions. No directionality_overrides are authored: the schema keys overrides by power atom, and the organized-power seats span both victim (settlers, rejectionists) and beneficiary (PA, mediation establishment) positions, so a per-atom override would corrupt one side or the other. The dual-positioned seats need no correction anyway — each is a NET beneficiary (core legitimacy and funding flows exceed compliance costs), so beneficiary-derived low d is approximately right even though each also pays. Binational civilians sit near symmetric: they pay the frozen conflict and would collect the settlement. Refugee descendants combine victim status, powerless power, and trapped exit — the derivation should place them nearest the full-target end; rejectionists add identity_lock, which pushes their effective extraction higher than their organized power alone would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Reading the framework as pure coordination ignores that refugee claims and settler tenure are the priced inputs — a rope verdict would erase identifiable payers. Reading it as pure extraction ignores delivered goods: the Egyptian and Jordanian treaties, mutual recognition between the parties, and the PA's international existence are real coordination outputs a snare verdict would deny. Tangled rope holds both. On obsolescence: the founding problem's original form (Mandate termination) is dead, but its successor (sovereignty allocation) is contested-live, so founding_problem_status is contested rather than dead — no zombie mismatch arises. Rising theater_ratio documents process-outcome substitution without yet rendering the framework inertial: recognition decisions still move material goods, as the 2024-25 recognition wave showed. The irreversibility_threshold omega marks the watchpoint where this would tip toward piton dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_ground_contestation,
    'This constraint is one reading of the territorial_legitimacy kernel (partition_reading). Would instantiating a sibling ground — security necessity or indigenous continuity — change the victim set and epsilon?',
    'Comparative compile of the sibling stories: align their stakeholder surfaces and epsilon referents (all three take the standing territorial arrangement as referent) and diff the victim sets and effective-extraction profiles.',
    'Under the security-necessity reading, settler communities drop out of the victim set and border-adjacent civilians enter it; under the indigenous-continuity reading, refugee descendants move toward the beneficiary pole and the state establishments toward the target pole. This file''s own classification is unaffected; cross-kernel comparison is invalid without the diff.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_ground_contestation, conceptual, 'Kernel-level ambiguity: which ground of territorial legitimacy governs, and how the victim set shifts across readings.').

omega_variable(
    refugee_subordination_share,
    'What fraction of the framework''s measured extraction consists of subordinating refugee return claims, versus the shares borne by settlers and rejectionists?',
    'Claim-level coding of negotiation archives (Camp David 2000, Annapolis 2008 parameters) to price the return-clause concessions demanded relative to border and security clauses.',
    'If refugee subordination dominates, extraction concentrates on the powerless-trapped seat and coalition potential drops; if settler exposure dominates, extraction concentrates on an organized seat with real political leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_subordination_share, empirical, 'Distribution of extraction across the three victim seats.').

omega_variable(
    enforcement_delivery_coupling,
    'Is the rising suppression_requirement series enforcement maturing toward effectiveness, or escalating compensation for delivery failure?',
    'Compare enforcement intensity against outcome indicators across periods: Oslo-era conditionality coincided with visible transfers; post-2000 intensification coincided with none. Regress enforcement effort on delivery milestones.',
    'If compensation dominates, the framework is ratcheting coercion to hold a receding promise — supporting drift toward harder types; if maturation, enforcement may yet close the practice gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_delivery_coupling, empirical, 'Whether enforcement growth signals capability or desperation.').

omega_variable(
    irreversibility_threshold,
    'At what density of settlement facts does practice drift become codification collapse — the point where the two-state consummation is no longer implementable by any compensation package?',
    'Settlement build-rate and demographic projections against available swap-land and evacuation-compensation envelopes; structured expert elicitation on remaining two-state feasibility space.',
    'Past the threshold, the framework''s reference frame becomes formally affirmed and practically void — theater_ratio approaches unity and the reading survives only as ritual, pushing lifecycle detection toward inertial dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_threshold, empirical, 'Location of the implementability cliff for the partition endpoint.').

omega_variable(
    hybrid_actor_stability,
    'How many consequential actors hold this reading purely, versus hybridizing it with sibling grounds (partition rhetoric plus security practice; partition diplomacy plus indigenous narrative)?',
    'Longitudinal coding of party platforms, coalition agreements, and UN voting records for pure-versus-hybrid ground invocation.',
    'Widespread hybridization stabilizes the coexists_with relation structure but blurs each reading''s victim set in practice; a return to pure holdings would sharpen inter-reading conflict and raise the kernel''s overall contest temperature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_actor_stability, conceptual, 'Prevalence and stability of hybrid legitimacy holdings across actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlpr_tr_t1947, territorial_legitimacy__partition_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(tlpr_tr_t1955, territorial_legitimacy__partition_reading, theater_ratio, 1955, 0.22).
narrative_ontology:measurement(tlpr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.34).
narrative_ontology:measurement(tlpr_tr_t1978, territorial_legitimacy__partition_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(tlpr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(tlpr_tr_t2000, territorial_legitimacy__partition_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(tlpr_tr_t2004, territorial_legitimacy__partition_reading, theater_ratio, 2004, 0.44).
narrative_ontology:measurement(tlpr_tr_t2011, territorial_legitimacy__partition_reading, theater_ratio, 2011, 0.5).
narrative_ontology:measurement(tlpr_tr_t2017, territorial_legitimacy__partition_reading, theater_ratio, 2017, 0.55).
narrative_ontology:measurement(tlpr_tr_t2025, territorial_legitimacy__partition_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(tlpr_be_t1947, territorial_legitimacy__partition_reading, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement(tlpr_be_t1955, territorial_legitimacy__partition_reading, base_extractiveness, 1955, 0.4).
narrative_ontology:measurement(tlpr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(tlpr_be_t1978, territorial_legitimacy__partition_reading, base_extractiveness, 1978, 0.52).
narrative_ontology:measurement(tlpr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.46).
narrative_ontology:measurement(tlpr_be_t2000, territorial_legitimacy__partition_reading, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement(tlpr_be_t2004, territorial_legitimacy__partition_reading, base_extractiveness, 2004, 0.61).
narrative_ontology:measurement(tlpr_be_t2011, territorial_legitimacy__partition_reading, base_extractiveness, 2011, 0.59).
narrative_ontology:measurement(tlpr_be_t2017, territorial_legitimacy__partition_reading, base_extractiveness, 2017, 0.63).
narrative_ontology:measurement(tlpr_be_t2025, territorial_legitimacy__partition_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tlpr_su_t1947, territorial_legitimacy__partition_reading, suppression_requirement, 1947, 0.15).
narrative_ontology:measurement(tlpr_su_t1955, territorial_legitimacy__partition_reading, suppression_requirement, 1955, 0.2).
narrative_ontology:measurement(tlpr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.32).
narrative_ontology:measurement(tlpr_su_t1978, territorial_legitimacy__partition_reading, suppression_requirement, 1978, 0.38).
narrative_ontology:measurement(tlpr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.47).
narrative_ontology:measurement(tlpr_su_t2000, territorial_legitimacy__partition_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(tlpr_su_t2004, territorial_legitimacy__partition_reading, suppression_requirement, 2004, 0.56).
narrative_ontology:measurement(tlpr_su_t2011, territorial_legitimacy__partition_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement(tlpr_su_t2017, territorial_legitimacy__partition_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement(tlpr_su_t2025, territorial_legitimacy__partition_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate borders in Israel/Palestine' covers three structurally distinct claims with different epsilons and victim sets: partition-recognition (this file), security-necessity, and indigenous-continuity. They are modeled as one kernel with three reading-stories linked by affects_constraints, per the epsilon-invariance principle — measuring legitimacy by recognition yields a different constraint than measuring it by control or habitation, and forcing one story to span all three would average away exactly the divergence the corpus exists to measure. The upstream/downstream structure differs by pair: the partition baseline exerts structural pressure on security claims (which must argue against a recognized-border default), while partition and indigenous-continuity compete as rival grounds with no causal ordering.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
