% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Partial Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   United Nations Security Council Resolution 242 (1967) establishes the
 *   framework for resolving territorial disputes arising from military
 *   occupation. The resolution's withdrawal clause uses an indefinite English
 *   article ('from territories occupied') that permits interpretation as
 *   requiring withdrawal from all occupied territories or only some
 *   territories deemed to meet specified conditions. This constraint
 *   instantiates the partial-withdrawal reading: the indefinite scope permits
 *   the occupying military power to retain strategic territories while
 *   claiming compliance with the resolution. The reading converts textual
 *   ambiguity into negotiating leverage for both the occupying power (which
 *   controls withdrawal scope) and mediating diplomatic authorities (which
 *   broker phased agreements). Territorial claimants lack a fixed enforcement
 *   line and experience the constraint as indefinite postponement of
 *   restoration. This is ONE reading of the contested kernel
 *   unsc_242_withdrawal_clause; the maximal-withdrawal reading and
 *   interpretive-authority-structure reading are siblings instantiating
 *   different structural logics from the same text.
 *
 * KEY AGENTS:
 *   - occupying_military_power: Holds territory, controls withdrawal pace and scope, benefits from textual ambiguity — institutional power, arbitrage exit
 *   - territorial_claimant_states: Claim sovereignty, seek full restoration, identity-locked to the claim — organized power, constrained/identity-locked exit
 *   - mediating_diplomatic_authority: Brokers agreements, benefits from indefinite scope that perpetuates mediation function — institutional power, mobile exit
 *   - displaced_civilian_populations: Bear costs of ongoing occupation, have no negotiating seat, no exit — powerless, trapped
 *   - international_court_of_justice: Observes but lacks enforcement authority, competing claims to interpretive authority — institutional, analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause — Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'c4c392b2-4584-4d0e-b04e-f719057046e9').
narrative_ontology:cs_kernel_codification('c4c392b2-4584-4d0e-b04e-f719057046e9', fixed_text).
narrative_ontology:cs_authority_grounding('c4c392b2-4584-4d0e-b04e-f719057046e9', distributed).
narrative_ontology:cs_reading_relation('c4c392b2-4584-4d0e-b04e-f719057046e9', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4c392b2-4584-4d0e-b04e-f719057046e9', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('c4c392b2-4584-4d0e-b04e-f719057046e9', foundational, secure_boundaries_doctrine).
narrative_ontology:cs_axiom_status(secure_boundaries_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c4c392b2-4584-4d0e-b04e-f719057046e9', secure_boundaries_doctrine, instrumental).
narrative_ontology:cs_axiom('c4c392b2-4584-4d0e-b04e-f719057046e9', foundational, indefinite_article_encodes_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_encodes_discretion, holdable).
narrative_ontology:cs_axiom_grounding('c4c392b2-4584-4d0e-b04e-f719057046e9', indefinite_article_encodes_discretion, empirically_contingent).
narrative_ontology:cs_reference_frame('c4c392b2-4584-4d0e-b04e-f719057046e9', phased_withdrawal_security_accommodation).
narrative_ontology:cs_drift_state('c4c392b2-4584-4d0e-b04e-f719057046e9', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4c392b2-4584-4d0e-b04e-f719057046e9', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_military_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_diplomatic_authority).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimant_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls occupied territories and sets the pace and scope of any withdrawal. Interprets the indefinite English article ('from territories') as permitting retention of strategic areas deemed necessary for security. Holds that the constraint permits phased, partial withdrawal negotiated via intermediaries. Directly benefits from the textual ambiguity that defers a fixed withdrawal boundary. Can shift negotiating positions, invoke security exceptions, and pivot between diplomatic tracks — arbitrage-grade exit permits flexibility even within the constraint's operation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_military_power, agenda_setter,
    institutional, generational, arbitrage, regional).

% Claim sovereignty over the occupied territories and seek full withdrawal to the pre-1967 boundaries. They argue the indefinite article is a translation artifact, that the French definite article ('des territoires') controls, and that UNSC 242 mandates complete territorial restoration. Their exit option is constrained by identity fusion: territorial claim is constitutive of state identity and cannot be abandoned without dissolving the state's founding legitimacy. No fixed enforcement line exists for withdrawal scope — they are locked into perpetual negotiation. Bear the cost of indefinitely postponed restoration.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimant_states, payer,
    organized, generational, identity_locked, regional).

% Residents of occupied territories who bear the ongoing costs of military occupation: restricted movement, administrative control by occupying authority, resource allocation determined externally, uncertainty about future status and property rights. They have no direct negotiating seat and no exit — citizenship, property, and livelihood are bound to the territory. They are not named in diplomatic negotiations and experience the phased-withdrawal framing as indefinite postponement of return to normalcy.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_civilian_populations, payer,
    powerless, biographical, trapped, local).

% International actors (UN mediation bodies, regional powers, international mediators) who control the interpretation and implementation framework for Resolution 242. They benefit from the textual ambiguity because it keeps the dispute open and perpetuates their mediation function and relevance. The indefinite scope permits them to broker 'phased withdrawal agreements' and claim procedural success while the substance remains contested. They have sufficient exit options (other disputes, other mandates, redeployment to different regions) to exit this particular constraint if required, but they have no incentive to do so given the perpetual mediation revenue stream.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_diplomatic_authority, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_diplomatic_authority, agenda_setter).

% Tasked with resolving treaty interpretation disputes but faces competing authority claims from drafting states and the occupying power. The court observes the constraint's operation but has no enforcement mechanism to compel a particular reading and can only issue advisory opinions (non-binding). Sits as an observer because its authority to interpret is itself contested by the parties; has analytical exit (can decline to engage) but remains engaged through the international legal system's architecture.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_military_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a diplomatic framework for resolving territorial disputes through negotiated, phased withdrawal rather than immediate full restoration (which might destabilize the region) or indefinite occupation (which violates the principle of temporary possession). Permits step-by-step agreements that can accommodate security concerns, regional stability considerations, and the practical complexity of military withdrawal, enabling all parties to negotiate rather than unilaterally impose terms.
% TRANSFER_FUNCTION: Transfers negotiating authority and temporal discretion from automatic resolution (Charter-based territorial integrity principle which would mandate immediate full restoration) to mediated, contingent agreement. The occupying power retains discretion over withdrawal scope, pace, and conditions; mediators retain discretion over frame-setting, agreement brokerage, and implementation verification; territorial claimants retain only the right to negotiate. This transfer is asymmetric: occupying power gains the most discretion, claimants lose the most certainty.
% ABSENT_VOICES: Displaced civilian populations have no direct diplomatic seat; they are named in humanitarian frameworks but not in treaty interpretation and withdrawal-scope negotiations. Indigenous prior residents and historical populations (if any) are similarly absent. International legal scholars who dispute the partial-withdrawal reading are also absent — the constraint operates through state practice and diplomatic negotiations, not through scholarly consensus or judicial review. International human-rights bodies are marginally present but not empowered in the core negotiating structure.
% DISAPPEARANCE_RATIONALE: If this particular reading of Resolution 242 disappeared and the maximal-withdrawal reading controlled instead, territorial claims would advance toward full restoration and occupying power would face compulsory rather than negotiated withdrawal — the occupying state would need to exit the territory entirely rather than retaining strategic areas, and mediators would lose their perpetual mediation function. If the interpretive-authority-structure reading gained control instead, the ICJ would gain binding authority over interpretation, the ambiguity would be resolved by external judicial authority rather than negotiation, and occupying power's discretion would be constrained by judicial reasoning. The world clearly rearranges under any alternative reading, but which reading becomes operative is contested among the parties — territorial claimants and human-rights bodies favor maximal withdrawal; occupying power and mediators favor the partial reading; some international legal authorities favor interpretive-structure (judicial resolution).
% FOUNDING_PROBLEM: UNSC 242 (1967) was drafted to resolve the aftermath of military occupation following the Six-Day War. The founding problem was to establish a framework permitting withdrawal of military forces from occupied territories while respecting the principle of durable peace and avoiding destabilization. The immediate problem was acute: a newly-occupied territory, humanitarian pressure for withdrawal, security concerns about re-invasion, and need for international legitimacy of any settlement. The indefinite English article was a deliberate compromise negotiated between drafts calling for complete withdrawal and those permitting strategic retention — a textual bridge permitting both interpretations.
% FOUNDING_PROBLEM_CORROBORATION: The occupying power and diplomatic mediators attest the founding problem is still live: security concerns and regional instability justify ongoing retention of certain strategic areas, full withdrawal would destabilize the region, and phased negotiation is the appropriate mechanism. Territorial claimants and international legal scholars outside the benefiting parties attest the founding problem is substantially resolved by 2026 (55+ years have passed, security situations have evolved, international law has clarified) and the partial-withdrawal reading has become a mechanism for indefinite postponement rather than a temporary accommodation. Declassified drafting records and contemporary scholarly analysis from neutral sources (Swiss archives, Swedish mediation records, academic historical analysis published by non-aligned institutions) support the deliberate-compromise reading of the indefinite article, suggesting the founding problem was real and the compromise was intentional — but contemporary analysis also shows the compromise has been weaponized into indefinite discretion rather than used as intended.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 endpoint) because the constraint operates through conditional, phased agreements rather than outright rent extraction: the occupying power gains time and discretion, but not unlimited resources. Suppression is high (0.72) because maintaining the indefinite scope requires active enforcement against competing interpretations (the maximal-withdrawal reading, court attempts at binding interpretation). Theater ratio is moderate-high (0.41) and rising: diplomatic processes produce visible 'agreements' and 'frameworks' that claim progress while the substance (actual withdrawal scope) remains contested. The measurement series shows theater ratio rising from 0.18 (early pure-uncertainty phase) to 0.42 (mature phase where process becomes the performance), indicating the constraint's function has migrated toward procedural legitimacy rather than substantive resolution. Accessibility of alternatives collapses only partially (0.48) because the maximal-withdrawal reading remains a live legal position and the interpretive-authority-structure reading offers institutional exit routes. Resistance is substantial (0.62) precisely because territorial claimants and their allies actively contest the partial-withdrawal reading through counter-interpretations and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   The occupying military power perceives this constraint as legitimate coordination that balances security and territorial restoration through phased negotiation. From their structural position (agenda-setter, arbitrage exit), they compute the constraint as permitting and protecting their own discretion — a rope-like coordination function. The territorial claimant states perceive the same constraint as pure extraction masquerading as negotiation: the indefinite scope locks them into identity-bound claiming with no fixed endpoint, while the occupying power retains all discretion. From their position (payer, identity-locked exit), they compute it as a snare. Mediating authorities perceive it as successful coordination: phased agreements are negotiated, frameworks are established, the international system functions — but they benefit from the indefinite scope because it perpetuates their mediating role, making them partly complicit in the asymmetry. The engine computes per-seat directionality from these structural differences: the occupying power's d approaches 0 (beneficiary), claimants' d approaches 1 (target), mediators sit near 0.5 (symmetric, but tilted toward benefit). The claim is tangled_rope (both coordination function and extraction), which requires all three components: beneficiaries (occupying power, mediators), victims (claimants), and active enforcement (competing interpretations must be suppressed). The metrics are authored independently of the claim — the divergence is what the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (d near 0): The occupying military power benefits from the indefinite scope (retains strategic territories indefinitely, postpones costly full withdrawal) and has arbitrage exit (can shift interpretation-compliance strategies, negotiate terms). Mediating diplomatic authority benefits from perpetuated mediation function and has mobile exit (can pivot to other disputes). Victim directionality (d near 1): Territorial claimants are structurally targeted (indefinite postponement of restoration, no fixed enforcement line) and have identity-locked exit (cannot exit the territorial claim without dissolving state identity). Displaced civilian populations are structurally targeted (ongoing occupation burden) and have trapped exit (citizenship and residence are bound to the territory). The engine derives d from these beneficiary/victim declarations and exit-option asymmetries without tuning.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits incipient mandatrophy: the founding problem (establish withdrawal framework after 1967 occupation) is substantially solved in form (multiple withdrawal agreements exist, phased frameworks are negotiated) but the substance (actual full territorial restoration) remains blocked. The theater_ratio trajectory (rising from 0.18 to 0.42) documents this drift: the proportion of activity that is procedural performance (diplomatic meetings, framework-setting, agreement-signings) is growing while the substantive output (actual territorial restoration) is stalled. However, mandatrophy is not yet complete because the founding problem's status is contested: occupying power and mediators attest the problem remains live (security concerns, regional instability), and this attestation blocks the full mandatrophy verdict. An omega variable captures this irreducible ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafters_intent_vs_textual_clarity,
    'Was the indefinite English article in Resolution 242''s withdrawal clause a deliberate encoding of discretionary withdrawal scope, or an artifact of translation negotiation that obscures the true intent (which may be in the French definite article)?',
    'Declassified drafting records, authenticated testimony from negotiators, comparative historical analysis of other multilingual UN resolutions and their interpretation patterns. NGO Legal research archives and academic historical analysis.',
    'If deliberate encoding, the partial-withdrawal reading''s axiom (secure_boundaries_doctrine) is strengthened and the constraint''s legitimacy is higher. If an artifact, the maximal_withdrawal_reading gains ground and the occupying power''s discretion contracts. The classification could shift from tangled_rope toward snare if intent is confirmed as maximal withdrawal masked by translation ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drafters_intent_vs_textual_clarity, empirical, 'Whether textual indefiniteness was intentional or accidental.').

omega_variable(
    security_boundary_retention_necessity,
    'Is retention of strategic territories genuinely necessary for the occupying power''s security, or is it a pretext for indefinite occupation and territorial expansion?',
    'Post-withdrawal security analysis from independent international bodies (UN fact-finding missions, International Crisis Group assessments), comparative security outcomes in similar phased-withdrawal cases, declassified occupying-power security assessments. Expert testimony from security analysts outside the occupying state.',
    'If necessary, the constraint''s coordination function (balancing security and restoration) is real and the tangled_rope classification holds. If pretextual, the constraint is pure extraction riding on a coordination cover story, shifting classification toward snare. The secure_boundaries_doctrine axiom is core to this reading; if necessity fails, the axiom becomes overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_boundary_retention_necessity, empirical, 'Whether secure-boundary retention is a genuine security requirement or a mechanism for indefinite occupation.').

omega_variable(
    reading_authority_legitimacy,
    'Which authority structure should legitimately control interpretation of Resolution 242: drafting state intent, ICJ judicial reasoning, occupying-state practice, or mediator-negotiated consensus?',
    'International legal scholarship, state practice voting patterns in UN bodies, ICJ advisory opinions on treaty interpretation authority, regional peace agreement acceptance and implementation patterns. This is a conceptual and preference question — no empirical resolution fully settles it.',
    'If drafting-state intent controls, this partial-withdrawal reading is weakened (intent may be maximal). If ICJ authority is established, the reading becomes subject to judicial override (the interpretive-authority-structure reading gains force). If occupying-state practice controls, this reading is strengthened. If mediator consensus controls, the reading persists indefinitely as long as mediation functions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_authority_legitimacy, conceptual, 'Which authority structure should govern treaty interpretation — a fundamental issue unresolved in international law.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (external barriers preventing alternative readings from gaining diplomatic traction) or internalized (claimant states have absorbed the indefinite-scope framing into their own reasoning and abandoned maximal-withdrawal as a live negotiating position)?',
    'Comparative track-record of claimant-state demands before and after sustained exposure to the partial-withdrawal framing; post-breakpoint analysis if the reading is formally superseded (do claimants re-assert maximal withdrawal or remain locked into phased-withdrawal framing); interviews/diplomatic records from claimant negotiators.',
    'If structural, suppression persists only while the occupying power and mediators actively enforce the partial-withdrawal reading; if it is superseded, the constraint collapses. If internalized, claimants may continue framing demands as phased-withdrawal negotiation even after the reading''s enforcement evaporates — the constraint becomes self-perpetuating through cognitive capture. This affects the long-term stability of the classification and the piton-trajectory probability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of the maximal-withdrawal reading is structural or internalized in claimant states.').

omega_variable(
    temporal_indefiniteness_as_mechanism,
    'Does the partial-withdrawal reading''s indefinite withdrawal scope function as a deliberate extraction mechanism (keep-the-parties-negotiating indefinitely, collect rents from the ambiguity), or as a genuine accommodation of security-boundary complexity?',
    'Pattern analysis of withdrawal timelines: do agreed-withdrawal phases consistently slip or get renegotiated? Do mediators consistently restart negotiations rather than enforcing agreed endpoints? Do occupying-power security concerns demonstrate genuine time-sensitivity or persistent re-emergence? Historical comparison with other phased-withdrawal agreements and their completion timelines.',
    'If indefiniteness is mechanism, the constraint is primarily extractive and should be classified as snare (pure extraction with a coordination cover). If genuine accommodation, the classification holds as tangled_rope. The theater_ratio trajectory (rising to 0.42) suggests growing mechanistic function, but the trend is not determinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_indefiniteness_as_mechanism, empirical, 'Whether indefinite withdrawal scope is a designed extraction mechanism or a legitimate accommodation of complexity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1995, 0.34).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(unsc_tr_t2020, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(unsc_tr_t2026, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1995, 0.54).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(unsc_be_t2020, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(unsc_be_t2026, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.52).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1980, 0.61).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(unsc_su_t2020, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(unsc_su_t2026, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_integrity_default_charter_article_2_4).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, phased_withdrawal_precedent_framework).

% DUAL FORMULATION NOTE:
% The constraint family unsc_242_withdrawal_clause consists of three structurally distinct readings of the same text. The partial_withdrawal_reading (this constraint, ε=0.58) interprets indefinite English article as permitting discretionary scope; the maximal_withdrawal_reading (ε≈0.25) interprets definite French article as mandating full restoration; the interpretive_authority_structure reading (ε≈0.45) contests authority rather than scope. Each reading has distinct beneficiary/victim structure and directionality profiles. They are NOT the same constraint viewed from different angles — the ε values differ significantly, indicating genuinely distinct structural claims about what the text requires. Each instantiates a different set of beneficiaries, victims, and enforcement patterns. Links are bidirectional: maximal reading influences this partial reading (provides alternative legal position); interpretive reading influences both (contests the authority that permits either to claim legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
