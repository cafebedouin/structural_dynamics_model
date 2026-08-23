% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Theory of Statehood — Recognition as the Gate to Legal Personality
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The constitutive theory of statehood holds that a polity acquires legal
 *   personality only when the existing community of states recognizes it:
 *   statehood is conferred by the gate, not found in facts. This story
 *   authors that reading as the standing arrangement under contest — the
 *   recognition gate as it actually operates. Existing recognized states,
 *   with discretion concentrated in the great powers, decide which polities
 *   count; unrecognized de facto polities (Somaliland, Taiwan, Kosovo in
 *   part, Northern Cyprus, Transnistria, Abkhazia, South Ossetia, the Sahrawi
 *   Arab Democratic Republic) and their populations bear the costs: no treaty
 *   capacity, no institutional membership, restricted access to international
 *   finance and markets, and documents that do not travel. The arrangement
 *   carries a real coordination function — a decentralized system needs
 *   determinate counterparties — and a real veto whose value to its holders
 *   is the exclusion it imposes. This is ONE reading of the
 *   montevideo_statehood_criteria kernel; the declaratory and hybrid readings
 *   are separate constraints with their own epsilon values: the declaratory
 *   sibling carries near-zero epsilon because no party's standing depends on
 *   another's consent, while the hybrid sibling re-keys the gate to
 *   legitimacy and carries intermediate epsilon over a re-composed victim
 *   set. The claimed type is authored from structure; the metrics from
 *   observed operation; the engine computes each seat's classification, and
 *   any divergence between claim and computed type is the measurement the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - recognized_great_powers: agenda-setting seat — holds the operative admission veto, extends or withholds recognition as a foreign-policy instrument, not bound by the gate in any way it cannot waive (institutional power, arbitrage position)
 *   - established_recognized_states: beneficiary seat — collect club stability and predictable counterparties without running the gate; retain a voice but not the decisive vote (institutional power, mobile position)
 *   - unrecognized_de_facto_polities: primary target — hold effective territory and functioning government but no legal personality; no self-help route to standing (moderate power, trapped)
 *   - populations_of_unrecognized_polities: diffuse target — carry the exclusion in documents, travel, finance, and representation (powerless, trapped)
 *   - united_nations_membership_apparatus: administering seat — applies the gate through the Charter membership procedure; gains a determinate list, absorbs the recurring cost of contested cases (institutional power, constrained)
 *   - stateless_nations_without_territorial_control: excluded voice — would object that the gate entrenches existing borders; holds no seat in the conversation (powerless, trapped)
 *   - international_law_community: analytical observer — authors the competing readings, split between them, invested in the consent-based foundations of the field (analytical power, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.62).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.7).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Theory of Statehood — Recognition as the Gate to Legal Personality").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, 'f7311548-cc31-406b-9464-4abe6cd71677').
narrative_ontology:cs_kernel_codification('f7311548-cc31-406b-9464-4abe6cd71677', fixed_text).
narrative_ontology:cs_authority_grounding('f7311548-cc31-406b-9464-4abe6cd71677', practice).
narrative_ontology:cs_interpretation_layer_present('f7311548-cc31-406b-9464-4abe6cd71677').
narrative_ontology:cs_reading_relation('f7311548-cc31-406b-9464-4abe6cd71677', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('f7311548-cc31-406b-9464-4abe6cd71677', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('f7311548-cc31-406b-9464-4abe6cd71677', foundational, recognition_constitutes_legal_personality).
narrative_ontology:cs_axiom_status(recognition_constitutes_legal_personality, holdable).
narrative_ontology:cs_axiom_grounding('f7311548-cc31-406b-9464-4abe6cd71677', recognition_constitutes_legal_personality, conventional).
narrative_ontology:cs_axiom('f7311548-cc31-406b-9464-4abe6cd71677', foundational, international_legal_order_is_state_consent_based).
narrative_ontology:cs_axiom_status(international_legal_order_is_state_consent_based, holdable).
narrative_ontology:cs_axiom_grounding('f7311548-cc31-406b-9464-4abe6cd71677', international_legal_order_is_state_consent_based, conventional).
narrative_ontology:cs_axiom('f7311548-cc31-406b-9464-4abe6cd71677', secondary, unrecognized_entities_lack_treaty_capacity).
narrative_ontology:cs_axiom_status(unrecognized_entities_lack_treaty_capacity, holdable).
narrative_ontology:cs_axiom_grounding('f7311548-cc31-406b-9464-4abe6cd71677', unrecognized_entities_lack_treaty_capacity, conventional).
narrative_ontology:cs_reference_frame('f7311548-cc31-406b-9464-4abe6cd71677', state_consent_constitutive_order).
narrative_ontology:cs_drift_state('f7311548-cc31-406b-9464-4abe6cd71677', contemporary_state_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f7311548-cc31-406b-9464-4abe6cd71677', '2026-08-05T14:22:31Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, recognized_great_powers).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, established_recognized_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_de_facto_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_of_unrecognized_polities).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, legal_positivist_consent_doctrine).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, sovereign_equality_of_existing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which new polities are admitted to full participation in international life, through bilateral recognition decisions, Security Council votes on membership, and coordinated non-recognition campaigns. They extend or withhold recognition as an instrument of foreign policy — the same criteria facts produce recognition for one polity and refusal for another depending on the recognizer's alignment. They are not bound by the gate in any way they cannot waive: they act on facts when convenient while denying others' facts. Leaving the arrangement would mean surrendering an instrument they currently monopolize.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, recognized_great_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, recognized_great_powers, beneficiary).

% Hold full membership in the society of states and collect its benefits: treaty networks, institutional seats, diplomatic protection. They gain from a membership list they did not have to fight to define and from the stability of predictable counterparties. They retain a voice in admission votes but not the decisive one; some extend recognition to contested polities against great-power preference, which costs them little. Their stake is the club's stability rather than the gate's discretion.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, established_recognized_states, beneficiary,
    institutional, generational, mobile, global).

% Control territory, maintain functioning governments, and in several cases meet every published criterion for statehood, yet hold no legal personality: they cannot join the United Nations, accede to most treaties, borrow from international financial institutions, or appear as parties before many tribunals. They pursue recognition through lobbying, quasi-diplomatic missions, and litigation strategies, and some have run these campaigns for over three decades without result. No action of their own confers standing — the decision belongs entirely to others.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_de_facto_polities, payer,
    moderate, generational, trapped, regional).

% Carry the exclusion in daily life: passports that many states do not accept, travel that requires third-country workarounds, limited access to international finance, remittance channels, and markets, and no representation in the forums where decisions about their region are made. They cannot exit by leaving — their citizenship documents follow them — and cannot exit by staying, since the exclusion attaches to the polity they live in.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_of_unrecognized_polities, payer,
    powerless, biographical, trapped, regional).

% Administers admission through the Charter's membership articles and the Security Council recommendation procedure, and applies the recognition standard in every membership, agency-admission, and observer-status decision. It gains a determinate membership list but absorbs the recurring institutional cost of contested cases: decade-long membership blocks, damaged universality claims, and member-state fights that consume agenda time. It is bound by its own founding rules and cannot unilaterally change the admission standard.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, united_nations_membership_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Nations with substantial populations and political movements but no effective territorial government — dispersed or under the jurisdiction of existing states. The admission gate is built and operated entirely without them: they hold no vote, no observer seat in the relevant forums, and no path to being considered. Their claim is that the arrangement freezes the map at the moment of their exclusion and prices their political aspiration beyond reach; the conversation that would hear that claim includes no seat for them.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, stateless_nations_without_territorial_control, excluded,
    powerless, generational, trapped, regional).

% Scholars, courts, and commissions that produce the doctrine: they author the competing accounts of statehood, advise recognizing states, and supply the advisory opinions through which contested cases are argued. The discipline is split — a majority professes the criteria-based account while documenting that practice runs on recognition — and its own authority is invested in the consent-based foundations of the field. It sees the full structure and adjudicates nothing.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_law_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, recognized_great_powers).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In a decentralized system with no central registry of legal persons, the arrangement solves the counterparty-identification problem: it produces a shared, enforceable answer to which entities can sign treaties, exchange ambassadors, join organizations, and hold international rights and duties. Recognition practice is the mechanism by which the society of states maintains a determinate membership list.
% TRANSFER_FUNCTION: Moves legal standing and access: treaty capacity, diplomatic representation, membership in international organizations, access to international financial institutions and markets, and the protections of international law flow to recognized states and are denied to unrecognized polities and their populations. The arrangement also concentrates admission discretion itself in the existing community of states, operationally in the great powers.
% ABSENT_VOICES: Unrecognized de facto polities and their populations are the paradigmatic absent voices: they are the objects of every recognition decision and hold no seat in any forum where those decisions are made — they can petition, lobby, and litigate at the margins but cannot vote. Stateless nations without territorial control are absent entirely; their objection — that the gate converts political aspiration into legal impossibility and entrenches existing borders — is never entered into the record the gate administers.
% DISAPPEARANCE_RATIONALE: If the constitutive rule vanished overnight, Somaliland, Taiwan, Kosovo, Palestine, Northern Cyprus, and Transnistria would assert legal personality immediately; UN membership and specialized-agency admission would flood with contested applications; bilateral recognition would degrade into courtesy rather than constitutive act; and the great powers would lose a standing instrument of foreign policy. Treaty relations, diplomatic practice, and the membership of every international organization would reorganize around whatever replaced the gate.
% FOUNDING_PROBLEM: The 19th-century problem of determining, in a system with no central authority, which entities count as states — how to manage admission to the society of states after imperial collapse and revolution without endless competing personality claims, and how to ground international law itself in a system without a world legislature (the positivist answer: law exists by state consent, so legal personality must exist by state consent).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: public international law scholarship across the constitutive/declaratory split agrees the counterparty-identification problem is real; the ICJ's jurisprudence presupposes the need for determinate legal personality; and the conduct of unrecognized polities themselves — Somaliland's three-decade recognition campaign, Kosovo's advisory-opinion strategy, Palestine's incremental treaty accession — attests that the identification problem is live. No party, including the gate's victims, disputes that the problem exists; the contest is over whether community consent is the necessary solution.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62: the gate's value to its holders is precisely the standing it withholds — treaty capacity, institutional membership, financial access — and that value has accumulated as contested cases multiplied (Kosovo, Somaliland, Taiwan, the annexation non-recognition policies). Suppression is 0.70 and is the load-bearing number: the arrangement does not persist by consensus but by active enforcement — coordinated non-recognition campaigns, pressure on third states considering recognition, Security Council gatekeeping of membership, and the maintenance of strategic-ambiguity doctrines. Theater is 0.38: recognition ceremonies, non-recognition policy statements, and ambiguity doctrines are a growing performative share of activity, but the underlying counterparty-identification function is real work the system would otherwise lack. Accessibility collapse is 0.58: under this reading a polity that meets every published criterion still lacks personality, so self-help routes collapse; the declaratory account survives as a doctrinal alternative some states invoke, which keeps collapse short of total. Resistance is 0.60 and organized: Somaliland's three-decade quasi-diplomatic campaign, Kosovo's advisory-opinion strategy, Palestine's incremental treaty-accession path, and sustained scholarly contestation. Identity-lock note: the doctrinal community's attachment is professional and disciplinary — international law's self-conception is that legal obligation exists by state consent, so abandoning the constitutive reading would unsettle the field's foundational account of itself, not merely one doctrine within it. The measurement series run on one shared time grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the great-power seat the arrangement is the orderly admission of members to a society of sovereign equals and a legitimate instrument of policy; from the unrecognized polity's seat the same structure is a locked door that converts political achievement into legal nullity regardless of what is achieved; from the UN seat it is simultaneously the source of a determinate membership list and a recurring institutional wound (decade-long membership blocks, damaged universality claims). The payer and agenda-setter seats compute differently because their structural positions differ — arbitrage against trapped — not because they observe different facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared at the structural layer: recognized great powers and established recognized states. Great powers sit nearest the beneficiary end — they collect the veto's discretion and can arbitrage recognition selectively, recognizing one criteria-identical polity and refusing another on alignment alone. Established states benefit from club stability but hold no decisive vote and occasionally act against the gate's preference at little cost, so their d sits above the great powers' while remaining on the beneficiary side. Unrecognized de facto polities and their populations are declared victims and sit near the full-target end, amplified by trapped exit: nothing they do confers standing, so what the gate withholds cannot be exited around. The UN membership apparatus is deliberately NOT declared a beneficiary: it administers the gate and gains a determinate list but absorbs the recurring cost of contested cases, placing it near symmetric. Stateless nations without territorial control are excluded rather than seated — targets-in-waiting whose path is foreclosed before it opens; they are not currently governed parties and carry no declared d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — determinate counterparty identification in a decentralized system — is live, so no mandatrophy resolution is declared: the arrangement has not outlived its function. The classification work runs in both directions. Reading the arrangement as pure coordination would erase the veto: the declaratory alternative shows the identification function does not require community consent as its mechanism, so the consent requirement is doing separable work — exclusion. Reading it as pure extraction would erase the function: the system genuinely needs a determinate membership list, the world rearranges without it, and the declaratory alternative carries its own costs (competing personality claims everywhere, no settled counterparties). The tangled_rope classification holds both facts: genuine coordination function and asymmetric extraction through the same gate. If an objective criteria-based registry ever delivered the identification function, the veto would stand exposed as rent and the classification would drift toward snare — that is the trajectory the omega variables track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story is the constitutive_reading of the montevideo_statehood_criteria kernel; what would each sibling reading change structurally?',
    'Compare the three readings'' compiled stories: the declaratory_reading empties the victim set (legal personality established by meeting the four objective criteria, recognition merely acknowledges); the hybrid_reading re-keys the gate to normative legitimacy rather than community consent. The disagreement is located at the constitutive premise itself.',
    'Under the declaratory reading the victim set dissolves and measured extraction collapses toward coordination cost; under the hybrid reading the victim set persists but re-composes around legitimacy-deficient entities. This story''s classification holds only for the constitutive reading''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; siblings are separate constraints with their own epsilon values.').

omega_variable(
    veto_necessity_vs_privilege,
    'Is the community-consent veto a structural necessity of a decentralized legal order, or a constructed privilege that an objective criteria-based registry would render unnecessary?',
    'Comparative institutional analysis: whether a criteria-based registry could deliver the determinate counterparty identification the system needs, using criteria-meeting-but-unrecognized polities (Somaliland as the cleanest test case) as the discriminating instances.',
    'If a registry suffices, the veto is exclusion riding on a real identification function and the classification drifts toward snare; if consent is genuinely necessary to the identification function, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_necessity_vs_privilege, empirical, 'Whether the admission veto is coordination cost or rent.').

omega_variable(
    doctrine_vs_interest_driver,
    'Is recognition practice governed by the constitutive doctrine or by great-power interest with the doctrine as post-hoc rationalization?',
    'Code recognition and non-recognition decisions against doctrinal criteria versus sponsor alignment; the Kosovo versus Abkhazia/South Ossetia mirror-image pattern (criteria-identical facts, opposite recognition outcomes tracking recognizer alignment) is the cleanest test set.',
    'If interest-driven, the arrangement is an instrument of policy wearing doctrine as cover and its enforcement is better read as interest maintenance; if doctrine-governed, the arrangement is more coordination-like than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_interest_driver, empirical, 'What actually drives the gate''s recognition and non-recognition decisions.').

omega_variable(
    professed_declaratory_operative_constitutive_gap,
    'Most states and most scholarship profess the declaratory account of statehood while operative practice runs on recognition (non-recognition policies, membership vetoes, document non-acceptance); which is the operative arrangement?',
    'Predictive test: which account actually forecasts recognition and access outcomes across a coded case set; where the accounts diverge in their predictions, the operative arrangement is the one that predicts.',
    'If operative practice is constitutive, this story''s epsilon stands and the declaratory sibling is a near-zero-extraction dead letter; if practice has genuinely shifted declaratory, this story''s extraction is over-measured and the declaratory sibling carries the live arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professed_declaratory_operative_constitutive_gap, empirical, 'The gap between professed doctrine and operative practice in the statehood regime.').

omega_variable(
    recognition_gate_trajectory,
    'Is the admission veto hardening or dissolving over the coming decade (Somaliland recognition momentum, Taiwan''s shrinking recognition set, coordinated non-recognition of annexations)?',
    'Track recognition events, membership votes, and enforcement expenditure 2025-2035; rising political conditionality and enforcement spending indicate hardening; mass recognition waves or criteria-based admission indicate dissolution.',
    'Hardening supports the rising base_extractiveness trajectory as accumulation; dissolution would date a transition away from the current classification and re-date the measurement series.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recognition_gate_trajectory, empirical, 'Direction of drift for the recognition gate over the next decade.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1920, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1920, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1920, 0.24).
narrative_ontology:measurement_basis(mont_tr_t1920, observed).
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1945, 0.26).
narrative_ontology:measurement_basis(mont_tr_t1945, observed).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement_basis(mont_tr_t1960, observed).
narrative_ontology:measurement(mont_tr_t1971, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement_basis(mont_tr_t1971, observed).
narrative_ontology:measurement(mont_tr_t1991, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1991, 0.31).
narrative_ontology:measurement_basis(mont_tr_t1991, observed).
narrative_ontology:measurement(mont_tr_t2008, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement_basis(mont_tr_t2008, observed).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(mont_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mont_be_t1920, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement_basis(mont_be_t1920, observed).
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1945, 0.46).
narrative_ontology:measurement_basis(mont_be_t1945, observed).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1960, 0.36).
narrative_ontology:measurement_basis(mont_be_t1960, observed).
narrative_ontology:measurement(mont_be_t1971, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1971, 0.44).
narrative_ontology:measurement_basis(mont_be_t1971, observed).
narrative_ontology:measurement(mont_be_t1991, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1991, 0.52).
narrative_ontology:measurement_basis(mont_be_t1991, observed).
narrative_ontology:measurement(mont_be_t2008, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement_basis(mont_be_t2008, observed).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(mont_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1920, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1920, 0.52).
narrative_ontology:measurement_basis(mont_su_t1920, observed).
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement_basis(mont_su_t1945, observed).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement_basis(mont_su_t1960, observed).
narrative_ontology:measurement(mont_su_t1971, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement_basis(mont_su_t1971, observed).
narrative_ontology:measurement(mont_su_t1991, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1991, 0.6).
narrative_ontology:measurement_basis(mont_su_t1991, observed).
narrative_ontology:measurement(mont_su_t2008, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2008, 0.66).
narrative_ontology:measurement_basis(mont_su_t2008, observed).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(mont_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, territorial_integrity_norm).

% DUAL FORMULATION NOTE:
% The colloquial label 'Montevideo statehood criteria' covers three structurally distinct claims about what constitutes statehood. This story authors the constitutive reading only: recognition by the existing community constitutes legal personality, with its distinctive victim set (unrecognized de facto polities) and its distinctive extraction (the admission veto). The declaratory reading — criteria-meeting establishes statehood as fact, recognition merely acknowledges — carries near-zero epsilon because no party's standing depends on another's consent. The hybrid reading re-keys the gate to normative legitimacy and carries intermediate epsilon over a re-composed victim set. Per the epsilon-invariance principle these are separate stories linked through this network edge, not one story with a measurement parameter. The constitutive reading also structurally reinforces the territorial integrity norm: making secession contingent on community recognition raises the cost of border change everywhere.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
