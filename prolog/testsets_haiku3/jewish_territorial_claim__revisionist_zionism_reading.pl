% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionism: Maximalist Territorial Claim (Both Banks) via Iron Wall
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   The revisionist Zionism reading of the Jewish territorial claim posits
 *   that Jewish sovereignty requires both banks of the Jordan River,
 *   immediate seizure of this territory, and explicit rejection of Arab
 *   consent as a prerequisite. The 'Iron Wall' doctrine (articulated by
 *   Vladimir Jabotinsky and the Revisionist movement) frames military force
 *   as the primary and necessary mechanism to compel Arab acceptance of
 *   Jewish territorial demands. This reading explicitly refuses negotiation
 *   frameworks that condition Jewish claims on Arab agreement, treats Arab
 *   resistance as an obstacle to be overcome through superior coercive force
 *   rather than as a voice in settlement, and asserts that sustainable Jewish
 *   sovereignty can only rest on military dominance sufficient to foreclose
 *   Arab alternatives. The constraint story captures THIS reading's standing
 *   arrangement — the claim and its justificatory machinery — not the
 *   political/labor/cultural alternatives or any endpoint consensus.
 *
 * KEY AGENTS:
 *   - Revisionist Zionist leadership: ideological architects and political organizers claiming maximalist territory and Iron Wall doctrine
 *   - Settler colonists: agents implementing territorial claim through settlement, military action, and institutional consolidation
 *   - Arab Palestinian population: primary targets whose displacement and dispossession the constraint operationalizes
 *   - Arab state actors: secondary targets whose military capacity must be neutralized or overwhelmed
 *   - International observers (Western powers, League of Nations): analytical seats watching the claim's trajectory and enforcement
 *   - Competing Zionist readings (political, labor, cultural): alternative frameworks for Jewish national aspiration that reject or modify the maximalist claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.89).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.87).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionism: Maximalist Territorial Claim (Both Banks) via Iron Wall").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '7d035b74-2efc-4574-ae20-ff211aa05d1c').
narrative_ontology:cs_kernel_codification('7d035b74-2efc-4574-ae20-ff211aa05d1c', formalized).
narrative_ontology:cs_authority_grounding('7d035b74-2efc-4574-ae20-ff211aa05d1c', extraction).
narrative_ontology:cs_interpretation_layer_present('7d035b74-2efc-4574-ae20-ff211aa05d1c').
narrative_ontology:cs_reading_relation('7d035b74-2efc-4574-ae20-ff211aa05d1c', jewish_territorial_claim__political_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('7d035b74-2efc-4574-ae20-ff211aa05d1c', jewish_territorial_claim__labor_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('7d035b74-2efc-4574-ae20-ff211aa05d1c', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('7d035b74-2efc-4574-ae20-ff211aa05d1c', foundational, arab_consent_unnecessary_to_jewish_claim).
narrative_ontology:cs_axiom_status(arab_consent_unnecessary_to_jewish_claim, holdable).
narrative_ontology:cs_axiom_grounding('7d035b74-2efc-4574-ae20-ff211aa05d1c', arab_consent_unnecessary_to_jewish_claim, deontological).
narrative_ontology:cs_axiom('7d035b74-2efc-4574-ae20-ff211aa05d1c', foundational, coercive_force_necessary_to_overcome_arab_resistance).
narrative_ontology:cs_axiom_status(coercive_force_necessary_to_overcome_arab_resistance, holdable).
narrative_ontology:cs_axiom_grounding('7d035b74-2efc-4574-ae20-ff211aa05d1c', coercive_force_necessary_to_overcome_arab_resistance, empirically_contingent).
narrative_ontology:cs_axiom('7d035b74-2efc-4574-ae20-ff211aa05d1c', secondary, jewish_security_requires_perpetual_military_dominance).
narrative_ontology:cs_axiom_status(jewish_security_requires_perpetual_military_dominance, overridden).
narrative_ontology:cs_axiom_grounding('7d035b74-2efc-4574-ae20-ff211aa05d1c', jewish_security_requires_perpetual_military_dominance, empirically_contingent).
narrative_ontology:cs_reference_frame('7d035b74-2efc-4574-ae20-ff211aa05d1c', jewish_historical_displacement_and_vulnerability).
narrative_ontology:cs_drift_state('7d035b74-2efc-4574-ae20-ff211aa05d1c', post_state_establishment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7d035b74-2efc-4574-ae20-ff211aa05d1c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, settler_colonists).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_palestinian_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_cultural_and_religious_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, settler_colonists).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_state_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates the maximalist territorial claim (both banks of Jordan) and the Iron Wall doctrine as the ideological and strategic foundation for Jewish state establishment. Sets the non-negotiable parameters: immediate sovereignty, rejection of Arab consent as prerequisite, coercive enforcement. Commands the movement's institutional apparatus, articulates justifications, and directs settlement and military strategy. Could theoretically cede territory or accept negotiated boundaries, but the ideology forecloses this choice.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Agents implementing the territorial claim through settlement, military service, and institutional consolidation. They gain property, political rights, demographic majority status, and institutional power in the claimed territory. They also bear security costs and military service burdens. Their exit is identity-locked: they have internalized the settler colonial project as their future, their property, their security, their claim to the land. Leaving means abandoning everything the ideology has promised them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, settler_colonists, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, settler_colonists, payer).

% Primary victims of the territorial claim. Displaced from land, dispossessed of property, denied political rights and self-determination in claimed territory. Confined to shrinking enclaves or refugee camps. Their only options are: remain in place under settler rule without rights, flee as refugees, or mount armed resistance that is militarily overwhelmed. They cannot negotiate the claim away; alternatives are foreclosed by superior force. Suppression is both structural (military occupation, legal disabilities) and partially internalized (generational trauma, adaptation to permanent subordination).
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_palestinian_population, payer,
    powerless, generational, trapped, regional).

% Secondary targets whose military capacity must be neutralized, overwhelmed, or contained to secure the maximalist territorial claim. They bear the costs of military competition and strategic failure, diplomatic isolation, and the obligation to support Palestinian resistance. Their exit is constrained: they cannot abandon Palestinian kinship without domestic legitimacy collapse; they cannot match the military force arrayed against them; negotiation on the maximalist claim's terms means accepting subordination.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_state_actors, payer,
    powerful, generational, constrained, regional).

% Western powers, League of Nations, later United Nations observe and partially enable the territorial claim's implementation. They provide military aid, diplomatic recognition, diplomatic cover for enforcement, and tacit or explicit acceptance of the Iron Wall doctrine. They do not drive the constraint but their choices modulate its operational environment. Their position allows them to potentially alter the constraint's terms, but they do not exercise this power in the revisionist reading's favor.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_observers, observer,
    institutional, generational, analytical, global).

% Palestinian national, political, and armed movements that would negotiate alternative territorial arrangements, demand right-of-return, or assert Palestinian statehood — all foreclosed by the revisionist reading's non-negotiable maximalism. They mount substantial resistance but lack military capacity to fundamentally alter the constraint's operation. Their exclusion from the agenda-setting seat is structural: the constraint exists precisely to foreclose their alternatives.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_resistance_movements, excluded,
    moderate, biographical, trapped, regional).

% Jewish diaspora communities benefit from the establishment of a Jewish state and the territorial security it promises, though they may disagree with the revisionist reading's methods and ideology. Many would prefer the political or labor readings' softer approaches, but they support the state's existence and its territorial defense once established. Their identity-lock is partial: they can theoretically critique the revisionist framing but cannot fully exit Jewish national interests.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_cultural_and_religious_communities, beneficiary,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes sovereign Jewish territorial control and political self-determination in response to historical Jewish displacement and vulnerability; creates a unified political and military apparatus for collective Jewish security and national regeneration.
% TRANSFER_FUNCTION: Transfers land, property, political rights, and demographic majority status from Arab Palestinian and Arab state populations to Jewish settler colonists and the Jewish state apparatus. The transfer is accomplished through displacement, dispossession, and legal/military subjugation of the victim populations.
% ABSENT_VOICES: Arab Palestinian political leadership, Arab national movements, and the broader Arab state system whose consent is explicitly rejected by this reading. They would argue for Palestinian self-determination, right-of-return, and negotiated territorial settlement — all foreclosed by the revisionist maximalism. Also absent: Jewish anti-zionist and non-zionist voices, who would argue for diaspora alternatives or non-territorial Jewish renewal.
% DISAPPEARANCE_RATIONALE: If the revisionist reading and its Iron Wall enforcement vanished overnight, the territorial claim would revert to negotiation, Arab populations could reclaim dispersed land and property, Palestinian refugees could exercise right-of-return, and the regional power balance would fundamentally shift. The entire subsequent geopolitical order (Israeli state structure, Palestinian displacement, Arab-Israeli military competitions, regional alliance patterns) rides on this constraint's persistence.
% FOUNDING_PROBLEM: Jewish historical displacement, persecution, and vulnerability to pogroms and genocidal violence; the need for a sovereign Jewish state with sufficient military power to ensure Jewish security and prevent recurrence of diaspora victimization.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist leadership attests the founding problem is permanently live: Jewish security remains contingent on perpetual military dominance and territorial control, even after statehood is established. International observers (Holocaust survivors, Jewish organizations) corroborate the historical basis of the founding problem but increasingly contest the assertion that permanent maximal territorial claims and rejection of Arab negotiation remain necessary. Palestinian and Arab sources explicitly reject the founding problem's framing as justification for their dispossession. Post-establishment analysis shows that the constraint's operative function has shifted from addressing historical vulnerability (which statehood substantially addressed) to maintaining settler colonial dominance and preventing victim alternatives — a classic mandatrophy signature.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the constraint's operation involves total displacement of the victim population from claimed territory — this is not surplus extraction but appropriation of fundamental rights and land. Suppression is equally high (0.87) because the 'Iron Wall' is precisely the coercive apparatus preventing victim exit, alternatives, or consent-based renegotiation; the constraint's persistence depends entirely on this suppressive force. Theater is low (0.22) at the interval endpoint because the ideological claim is nakedly extractive — the justificatory narrative of security, regeneration, or historical right is minimal relative to the bald assertion of military-backed territorial seizure. Accessibility collapse is high (0.76): once the maximalist claim is articulated and the Iron Wall enforced, Arab alternatives (remaining in place, negotiating status, appealing to law) are structurally closed. Resistance is high (0.82): the victims mount substantial armed, political, and diplomatic resistance throughout, but the constraint persists because the coercive capacity is overwhelming. The measurement series tracks increasing consolidation: extractiveness and suppression both rise as settlement deepens and military dominance is established, while theater remains minimal — the justificatory effort does not increase because the claim needs no narrative cover once enforcement is assured.
 *
 * PERSPECTIVAL GAP:
 *   From the revisionist leadership seat, the arrangement is the necessary assertion of Jewish territorial and political will against historical displacement; from the Arab population seats, it is an imposed, non-negotiable seizure of land and sovereignty backed by force they cannot match. The engine computes these as radically different d values and likely different type classifications at the per-seat level: revisionist leadership might frame the same structural data as rope (justified territorial recovery) while the victim seats unambiguously experience snare (coerced dispossession with no exit). The authored claim (snare) and the metrics reflect the victim-seat and analytical-observer perspective; the gap itself is the political fact the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Revisionist leadership are beneficiaries with high power (institutional) and arbitrage-grade exit (they can negotiate terms or cede territory, though the ideology forecloses that choice). They anchor the constraint and collect its gains (territorial control, sovereignty, demographic majority in claimed lands). Settler colonists are both beneficiaries (access to claimed land, property rights, institutional power) and partial payers (they bear military costs, security risks); their exit is constrained by identity lock-in (they have internalized the settler colonial project as their future). Arab Palestinian population are full targets with powerless-to-moderate power and trapped exit (they cannot leave without abandoning land and kin; they cannot negotiate the claim away; resistance is met with overwhelming force). Arab states are partial targets with moderate-to-powerful power but identity-locked and constrained exit (they cannot abandon Palestinian kinship without domestic legitimacy collapse; their military options are foreclosed by superior force). International observers are analytical seats. The directionality chain: beneficiaries (d ≈ 0.05-0.15) get near-full subsidy from the constraint's operation; targets (d ≈ 0.85-0.95) bear its full extractive weight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Jewish displacement and vulnerability to persecution — was live when the revisionist reading emerged (early 20th century). The problem may be argued as partially dead or contested by the interval endpoint (the state has been established, the victim displacement is completed). Yet the constraint persists and is reinforced: this is a classic mandatrophy signature. The revisionist reading's stated justification (Jewish security and territorial regeneration) achieved its stated goal (Jewish statehood, territorial control), but the constraint's operative function has shifted to maintaining settler colonial dominance and preventing victim alternatives. A mandatrophy-resolved reading would acknowledge that the founding problem no longer justifies the constraint's ongoing operation, but the revisionist framing denies this: it asserts that Jewish security remains contingent on perpetual military dominance and territorial control — moving the justification from the founding problem to the permanent condition, a drift typical of extractive constraints that have outlived their coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_coercion_vs_consent,
    'Does the ''Iron Wall'' mechanism (coercive military superiority) constitute the core innovation of this reading, or is it merely a tactical implementation choice?',
    'Genealogical analysis of founding texts (Jabotinsky''s Iron Wall essay, Betar doctrine, Revisionist platform documents) combined with comparative analysis of how this reading versus political/labor readings treat the Arab consent question at the theoretical level.',
    'If coercion is merely tactical, the reading might coexist with consent-seeking alternatives; if coercion is foundational to the territorial claim''s justification, it forecloses consent-based readings within a single coherent framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_coercion_vs_consent, conceptual, 'Whether coercion is the core premise of revisionist reading or a contingent tactic.').

omega_variable(
    kernel_reading_vs_implementation_constraint,
    'Is this JSON capturing a reading of the contested kernel ''Jewish territorial claim'' (a normative position on what Zionism should demand), or is it capturing the implementation constraint that actually emerged (the military machinery required to realize any maximalist claim)?',
    'Distinguish the normative reading (what revisionist ideology posited as the claim and justification) from the structural constraint (what mechanisms were required operationally). The two may have different ε values and different victim sets.',
    'If the constraint is the normative reading, the referent is the demand-structure itself; if the constraint is the operational machinery, the referent is the military enforcement apparatus. Different ε, different type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_implementation_constraint, conceptual, 'Whether the story describes the ideological reading or the operational constraint it generated.').

omega_variable(
    maximalism_geographic_referent,
    'Does ''both banks of Jordan'' refer to the Transjordan territory (east bank) as a territorial claim, or is it a negotiating position whose real referent is the West Bank and Israeli side only?',
    'Historical analysis of which revisionist leaders claimed Transjordan as actual settlement territory versus negotiating maximalism, and whether the claim was ever operationalized or remained aspirational.',
    'If Transjordan was genuine claim, the victim set and territorial scope differ materially; if it was negotiating theater, the actual constraint is narrower and the theater_ratio assessment changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximalism_geographic_referent, empirical, 'Geographic scope ambiguity in the maximalist territorial claim.').

omega_variable(
    iron_wall_suppression_internalized_vs_structural,
    'Is the suppression created by the Iron Wall mechanism structural (external barriers to exit, economic dependency, military occupation) or internalized (Arab populations internalize the claim as inevitable, revise expectations downward, cease resistance)?',
    'Post-armistice/post-establishment trajectory analysis: if suppression persists after the coercive mechanism is nominally removed or stabilized, reclassify as partially internalized. If it decays when enforcement overhead drops, classify as structural.',
    'If internalized, the measured suppression (0.87) understates the constraint''s effective hold on resistance — the target carries the suppression beyond the enforcement mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_suppression_internalized_vs_structural, empirical, 'Suppression mechanism: structural or internalized colonization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t5, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(jewi_tr_t5, observed).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(jewi_tr_t10, observed).
narrative_ontology:measurement(jewi_tr_t15, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(jewi_tr_t15, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t25, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t5, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement_basis(jewi_be_t5, observed).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement_basis(jewi_be_t10, observed).
narrative_ontology:measurement(jewi_be_t15, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement_basis(jewi_be_t15, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t25, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 25, 0.89).
narrative_ontology:measurement_basis(jewi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t5, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement_basis(jewi_su_t5, observed).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement_basis(jewi_su_t10, observed).
narrative_ontology:measurement(jewi_su_t15, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement_basis(jewi_su_t15, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t25, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(jewi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__revisionist_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_displacement_and_refugee_status).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, arab_military_capacity_constraint).

% DUAL FORMULATION NOTE:
% The Jewish territorial claim kernel decomposes into four structurally distinct readings: cultural (no necessary political sovereignty), labor (socialist transformation + facts-on-ground), political (statehood via negotiation and international recognition), revisionist (maximal territory via coercive force). These are not the same constraint viewed from different angles; they have materially different ε values, different beneficiary/victim structures, different justificatory mechanisms, and different relationships to Arab participation. Each reading instantiates a distinct constraint with its own type classification. The four stories form a constraint family linked by network.affects_constraints; each member also records its sibling readings in cs_structure.reading_relations and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, powerless, 0.92).
constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
