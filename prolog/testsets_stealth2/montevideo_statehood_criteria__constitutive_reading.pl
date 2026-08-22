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
 *   human_readable: Constitutive Theory of Statehood (Recognition-Gated Legal Personality)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The constitutive theory holds that statehood is not a factual status but
 *   a conferred one: a polity becomes a state when the existing community of
 *   states acknowledges it. The doctrine solves a real problem -
 *   international law needs a determinate roster of legal persons before
 *   treaties, embassies, loans, and adjudication can proceed - but it also
 *   hands every existing state, and above all the Security Council's
 *   permanent members, a structural veto over new entrants. Polities that
 *   govern territory and population effectively (Somaliland; Taiwan for
 *   decades; Abkhazia; Transnistria) remain outside treaty frameworks,
 *   international financial institutions, and collective security, while
 *   their populations absorb the human costs of legal invisibility.
 *   Enforcement is active and costly: recognition counter-campaigns,
 *   admission vetoes, and economic coercion of would-be recognizers.
 *   CONSTRAINT-FAMILY NOTE: the colloquial label 'statehood requires
 *   recognition' decomposes per the epsilon-invariance principle into three
 *   structurally distinct claims. This file authors the constitutive branch
 *   (epsilon 0.68; victim set = unrecognized polities and their populations;
 *   incumbents hold the veto). The declaratory sibling authors near-zero
 *   extraction (criteria self-execute; recognition merely evidences). The
 *   hybrid sibling authors a gate retained but redirected at normatively
 *   deficient polities. The branches differ in epsilon, victim set, and
 *   failure modes; they are linked via network.affects_constraints, not
 *   merged. KEY AGENTS (by structural relationship): - p5_permanent_members:
 *   Agenda setter (institutional/arbitrage) - holds the admission veto and
 *   recognition leverage - established_recognizing_states: Primary
 *   beneficiary (organized/constrained) - collects club protections; pays
 *   coercion costs when deviating - unrecognized_polities: Primary target
 *   (moderate/trapped) - bears the extraction -
 *   populations_of_unrecognized_territories: Primary target
 *   (powerless/trapped) - bears the human costs -
 *   aspiring_stateless_movements: Excluded voice (powerless/trapped) -
 *   outside the conversation that sets the gate -
 *   international_court_of_justice: Analytical observer
 *   (institutional/analytical) - shifts doctrinal legitimacy without
 *   administering the gate
 *
 * KEY AGENTS:
 *   - p5_permanent_members: agenda setter (institutional/arbitrage) - admission veto, recognition leverage, counter-campaign direction
 *   - established_recognizing_states: primary beneficiary (organized/constrained) with secondary payer exposure to coercion
 *   - unrecognized_polities: primary target (moderate/trapped) - effective governance without legal personality
 *   - populations_of_unrecognized_territories: primary target (powerless/trapped) - bear diffuse human costs of limbo
 *   - aspiring_stateless_movements: excluded voice (powerless/trapped) - stateless nations outside the deliberation
 *   - international_court_of_justice: analytical observer (institutional/analytical) - advisory jurisdiction over status questions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.72).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Theory of Statehood (Recognition-Gated Legal Personality)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, 'c05e38a3-567d-4cd8-8bf9-7a50adbb5a84').
narrative_ontology:cs_kernel_codification('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', formalized).
narrative_ontology:cs_authority_grounding('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', practice).
narrative_ontology:cs_interpretation_layer_present('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84').
narrative_ontology:cs_reading_relation('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', foundational, recognition_constitutes_statehood).
narrative_ontology:cs_axiom_status(recognition_constitutes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', recognition_constitutes_statehood, conventional).
narrative_ontology:cs_axiom('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', secondary, nonrecognition_denies_treaty_capacity).
narrative_ontology:cs_axiom_status(nonrecognition_denies_treaty_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', nonrecognition_denies_treaty_capacity, conventional).
narrative_ontology:cs_reference_frame('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', recognition_constituted_statehood).
narrative_ontology:cs_drift_state('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', contemporary_post_montevideo_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c05e38a3-567d-4cd8-8bf9-7a50adbb5a84', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, p5_permanent_members).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, established_recognizing_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_of_unrecognized_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, established_recognizing_states).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, constitutive_theory_of_statehood).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, stimson_nonrecognition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over United Nations membership applications - the hard gate through which recognition converts into full international participation. Decide which recognition campaigns succeed by extending or withholding recognition and by pressuring other states' recognition choices. Their recognition decisions are constrained by no higher authority; they can extend, withhold, or withdraw recognition as geopolitical instruments, and they direct the counter-campaigns that keep contested cases frozen.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Enjoy the recognized club's protections: a sovereignty shield against absorption, equal legal personality regardless of size, and access to the treaty network, international courts, and financial institutions. In exchange they must take positions on every recognition dispute; those that deviate from great-power preferences pay coercion costs - trade restrictions, aid withdrawal, diplomatic isolation - as several states have over Taiwan-related engagements. Exiting the club is not an option; the protections and the obligations arrive together.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, established_recognizing_states, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, established_recognizing_states, payer).

% Govern territory and population effectively and meet the factual criteria for statehood, yet cannot convert effectiveness into legal personality. They pay in blocked market access, exclusion from the IMF and World Bank, unenforceable contracts abroad, travel documents that foreign banks question, and no collective-security backstop. Exit would mean dissolution into a neighboring state or acceptance of autonomy arrangements - either ends the statehood project they exist to pursue.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    moderate, biographical, trapped, regional).

% Bear the human costs of legal limbo: citizenship documents not honored abroad, narrowed migration and remittance channels, property and inheritance claims unenforceable internationally, and no recourse to international courts or human-rights bodies that require statehood for standing. They did not choose the status question and cannot relocate out of it; the constraint binds them across generations.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_of_unrecognized_territories, payer,
    powerless, generational, trapped, regional).

% Nations without states - Kurdish self-determination advocates, Sahrawi independence organizers, and comparable movements - watch the gate from outside. Recognition practice is set entirely among existing states, and their eligibility is debated without them. Their object lesson is visible in every frozen case: declaring statehood without great-power sponsorship invites non-recognition plus retaliation, so the rational move is to stay quiet until a sponsor appears.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, aspiring_stateless_movements, excluded,
    powerless, generational, trapped, regional).

% Asked to adjudicate questions that turn on statehood, most prominently the Kosovo Advisory Opinion of 2010. Declined to endorse either the constitutive or the declaratory account, holding recognition irrelevant to the narrow question posed. Its pronouncements shift doctrinal legitimacy among scholars and foreign ministries without administering the gate itself; it collects no flow from the arrangement and bears none of its costs.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, p5_permanent_members).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single authoritative roster of subjects of international law so that treaty-making, diplomatic exchange, adjudication, and international-organization membership all have determinate counterparties; resolves, once per candidate, the otherwise chaotic question of who may sign, sue, borrow, and be represented.
% TRANSFER_FUNCTION: Moves legal personality, treaty capacity, market and credit access, and collective-security coverage from the pool of unrecognized polities to the recognized club, allocated by existing states' recognition decisions; moves corresponding deference obligations - withholding recognition, honoring counter-campaign commitments - onto third states.
% ABSENT_VOICES: Unrecognized polities and stateless nations have no seat where recognition practice is set: Security Council chambers, General Assembly admission votes, and great-power contact groups deliberate their status entirely among gateholders. Somaliland-style applicants, Sahrawi and Kurdish self-determination advocates, and comparable movements would object to criteria-free discretionary gating but are structurally outside the conversation.
% DISAPPEARANCE_RATIONALE: If the recognition requirement vanished overnight, every de facto regime meeting the factual criteria would assert full legal personality at once; treaty registries, financial institutions, and diplomatic protocols would face a flood of new counterparties; and every frozen territorial dispute would reopen as claimants re-priced their options. The state system would reorganize around a much larger, ungated roster.
% FOUNDING_PROBLEM: After dynastic legitimacy collapsed, the post-Westphalian order needed a determinate answer to which governments and territories counted as sovereign equals entitled to sign treaties and exchange ambassadors - revolutionary governments, new nations, and colonial entities all claimed standing, and bilateral relations could not proceed without a settled answer.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: the Montevideo Convention's own Article 3 (adopted by regional states in 1933) declares political existence independent of recognition, attesting that contemporaries believed the counterparty problem solvable without a constitutive gate; the ICJ's Kosovo Advisory Opinion (2010) and the Badinter Commission's criteria-based opinions document practice drifting from the constitutive frame; treatise scholarship (Crawford, The Creation of States) records declaratory dominance in day-to-day practice. Caveat stated plainly: the corroborating institutions are themselves state-constituted, so no fully external attestor exists - the closest available testimony is the doctrinal record itself, which cuts against the constitutive reading's own tradition.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type tangled_rope is authored from structure, independently of the metrics: the arrangement possesses a genuine coordination function (a single roster of legal persons - without it, every bilateral relation re-litigates counterparty validity), asymmetric extraction (incumbents collect positional rents; unrecognized polities pay in market access, credit, and security), and active enforcement (admission vetoes, counter-recognition campaigns, coercion of recognizers). Metrics describe operation as descriptively true: extractiveness 0.68 - severe for targets (decades-long exclusion of effective governments) but uneven, since some de facto polities adapt; suppression 0.72 - persistence depends on suppressing both recognition bids and would-be recognizers, not on participant preference; theater_ratio 0.38 - real legal work (embassies, treaties, admission procedures) coexists with openly geopolitical recognition statements that assess power rather than criteria; accessibility_collapse 0.65 - once the rule is understood, self-help entry collapses (declaration without a sponsor fails) though de facto persistence remains possible, so not mountain-grade; resistance 0.55 - recognition lobbies, occasional defiant recognizers, and the scholarly declaratory movement. The three measurement series share one grid (points 0-90 map approximately to 1933-2023): extraction and enforcement dipped during the decolonization wave of near-automatic recognition, then ratcheted upward as contested secessions made recognition a geopolitical instrument again. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change (counter-campaign buildout), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (p5_permanent_members) the arrangement reads as responsible gatekeeping: managed enlargement, systemic stability, leverage over revisionist projects. From the payer seats the identical structure reads as an arbitrary veto over existence - effectiveness in governing territory counts for nothing without a sponsor. Same-level divergence inside the club: established_recognizing_states enjoy the roster's protection but pay coercion costs when they deviate from great-power preferences, so their computed seat should sit between the P5's near-pure benefit and the polities' near-pure burden. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   p5_permanent_members sit nearest the beneficiary pole: they set the terms, hold the admission veto, and face no higher authority (arbitrage-grade exit from any particular recognition fight). established_recognizing_states derive low d from their beneficiary declaration, elevated somewhat by the coercion payments recorded in their secondary payer role. unrecognized_polities and populations_of_unrecognized_territories sit near the full-target pole: victim declarations combined with trapped exit amplify effective extraction. aspiring_stateless_movements carry high d but stand outside enforcement reach; international_court_of_justice is analytical and collects no flow. Scope amplification applies modestly: the regime is global in reach, raising verification difficulty and thus effective extraction on the target seats. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - settling who counts as a treaty counterparty after dynastic legitimacy collapsed - has been partially superseded by objective criteria (Montevideo) and near-universal membership norms, yet the gate persists and has been re-weaponized (counter-recognition campaigns, admission vetoes). Classifying as tangled_rope rather than snare preserves the real coordination service (determinate counterparties) that a pure-extraction reading would erase; refusing rope preserves the veto extraction and suppressed exits that a pure-coordination reading would excuse. The contested founding_problem_status paired with the world_rearranges verdict signals a live-but-transformed mandate, not a zombie: the mismatch consumer sees status=contested rather than dead, so no capture/zombie flag fires - appropriate, since the roster function still performs even as its justification has shifted from necessity to incumbency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the constitutive_reading of the montevideo_statehood_criteria kernel; how would the victim set, veto structure, and classification change under the sibling readings (declaratory_reading, hybrid_reading)?',
    'Author the sibling stories and compare: the declaratory reading removes the victim set (polities meeting the four criteria count as states regardless of recognition, dissolving the veto''s bite); the hybrid reading keeps a gate but redistributes extraction onto normatively deficient polities rather than all unrecognized ones.',
    'Under the declaratory sibling this constraint''s extraction collapses toward coordination cost (rope-like); under the hybrid sibling the victim set changes composition while the gate structure persists. Cross-reading comparison is the corpus-level test of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of the statehood-criteria kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    natural_law_vs_incumbent_gate,
    'Is the recognition requirement an inherent feature of legal personality (any legal order requires communal acknowledgment of its subjects, as with domestic corporate personality) or a constructed gate that identifiable incumbents maintain for their own benefit?',
    'Comparative institutional analogy: if every functioning legal system necessarily requires discretionary communal acknowledgment for subjecthood, the requirement is structural; if systems demonstrably operate on declaratory registration without incumbent discretion (as company registries do), the discretionary-veto form is constructed.',
    'If inherent, part of the measured extraction is the irreducible price of legal order (Boltzmann-floor side); if constructed, the full epsilon is incumbent rent and false-summit analysis applies to the naturality framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_incumbent_gate, conceptual, 'Whether recognition-constituted statehood is a natural feature of legal systems or an incumbent-constructed gate.').

omega_variable(
    de_facto_adaptation_neutralization,
    'Do informal adaptation channels (unofficial trade offices, parallel currency arrangements, functional working relations) neutralize most of the measured extraction from unrecognized polities?',
    'Matched comparison of welfare, trade volume, and institutional access between long-unrecognized polities (Somaliland, Taiwan pre-1971, Abkhazia) and recognized peers of similar endowment and history.',
    'If harms are largely neutralized, effective extraction is materially lower than authored and the constraint sits nearer rope; if not, the authored epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_adaptation_neutralization, empirical, 'Whether informal channels offset the costs of non-recognition.').

omega_variable(
    doctrine_vs_admission_procedure,
    'Is the operative gate the constitutive doctrine itself or the Security Council admission procedure layered above it, making the doctrine partly theatrical in its current operation?',
    'Trace actual blocking events: if nearly all exclusions run through Security Council veto or pre-clearance politics rather than doctrine-driven non-recognition, the procedural layer is the binding constraint and the doctrine''s share of enforcement is smaller than claimed.',
    'If the admission procedure is the real gate, theater_ratio is understated and the doctrine drifts toward inertial maintenance; if doctrine drives practice independently, the authored ratio stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_admission_procedure, empirical, 'Whether the binding gate is the doctrine or the UN admission procedure.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the measured suppression of unrecognized polities purely structural (veto threats, coercion of would-be recognizers) or partly internalized (de facto elites self-censor recognition bids as futile, having watched prior bids fail)?',
    'Post-change trajectory: if polities that suddenly gain a great-power sponsor bid immediately and successfully, prior restraint was structural; if they continue to hesitate absent external encouragement, restraint is internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint would persist briefly even if the veto machinery weakened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural versus internalized suppression in non-recognition restraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(mont_tr_t0, observed).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(mont_tr_t15, observed).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(mont_tr_t30, observed).
narrative_ontology:measurement(mont_tr_t45, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement_basis(mont_tr_t45, observed).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(mont_tr_t60, observed).
narrative_ontology:measurement(mont_tr_t75, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement_basis(mont_tr_t75, observed).
narrative_ontology:measurement(mont_tr_t90, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement_basis(mont_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(mont_be_t0, observed).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement_basis(mont_be_t15, observed).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(mont_be_t30, observed).
narrative_ontology:measurement(mont_be_t45, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 45, 0.44).
narrative_ontology:measurement_basis(mont_be_t45, observed).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(mont_be_t60, observed).
narrative_ontology:measurement(mont_be_t75, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement_basis(mont_be_t75, observed).
narrative_ontology:measurement(mont_be_t90, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 90, 0.68).
narrative_ontology:measurement_basis(mont_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(mont_su_t0, observed).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(mont_su_t15, observed).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(mont_su_t30, observed).
narrative_ontology:measurement(mont_su_t45, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement_basis(mont_su_t45, observed).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(mont_su_t60, observed).
narrative_ontology:measurement(mont_su_t75, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 75, 0.66).
narrative_ontology:measurement_basis(mont_su_t75, observed).
narrative_ontology:measurement(mont_su_t90, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 90, 0.72).
narrative_ontology:measurement_basis(mont_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'statehood requires recognition' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story is the constitutive branch (substantial epsilon; victim set = unrecognized polities and their populations; incumbents hold a structural veto). The declaratory sibling authors near-zero epsilon (criteria self-execute; no victim set beyond edge cases). The hybrid sibling authors intermediate epsilon with a redistributed victim set (normatively deficient polities). Upstream/downstream structure: declaratory codification (Montevideo Article 3) and ICJ practice are cited AGAINST the constitutive frame, while constitutive recognition politics supplies the enforcement substrate on which any hybrid normative gate would ride. Each member links to the others via affects_constraints; none merges the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
