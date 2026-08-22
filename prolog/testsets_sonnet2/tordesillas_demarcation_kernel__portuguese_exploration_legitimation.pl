% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Line as Papal Confirmation of Portuguese Exploration Priority and Exclusion of Rivals East of the Meridian
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This story authors ONE reading of the Tordesillas kernel: the Portuguese
 *   exploration-legitimation reading, under which the papal-confirmed line
 *   functions primarily as a mechanism to consolidate and protect the eastern
 *   sea route to India and the Estado da Índia's trading-post monopoly
 *   against rival European crowns, rather than as a license for territorial
 *   conquest and subjugation of populations (that is the sibling reading,
 *   spanish_conquest_legitimation, authored as a separate constraint). Under
 *   this reading the coordination function (avoiding renewed Iberian war over
 *   overlapping claims) is real, and the extraction runs through trade-route
 *   exclusivity and customs/licensing revenue directed against competing
 *   European powers — English, French, and later Dutch traders — rather than
 *   through land seizure or forced labor of indigenous populations, which
 *   sits outside this reading's primary victim set (though Asian coastal
 *   polities appear here as excluded non-parties whose waters are treated as
 *   adjudicable by Europeans).
 *
 * KEY AGENTS:
 *   - portuguese_crown: primary beneficiary and co-agenda-setter — institutional/arbitrage
 *   - estado_da_india: administrative beneficiary collecting monopoly trade revenue — institutional/arbitrage
 *   - spanish_crown_east_of_line: co-signatory turned constrained party in the east — institutional/constrained
 *   - english_crown, french_crown, dutch_merchant_interests: excluded rival powers bearing the exclusion cost — powerful-organized/constrained
 *   - pope_alexander_vi_curia: agenda-setting authority claiming universal temporal jurisdiction — institutional/analytical
 *   - asian_coastal_polities: excluded non-parties whose ports are treated as within the adjudicated zone — moderate/trapped
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.58).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.58).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Line as Papal Confirmation of Portuguese Exploration Priority and Exclusion of Rivals East of the Meridian").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '5cc95968-1c9a-4f68-bfd3-1c2be098945c').
narrative_ontology:cs_kernel_codification('5cc95968-1c9a-4f68-bfd3-1c2be098945c', formalized).
narrative_ontology:cs_authority_grounding('5cc95968-1c9a-4f68-bfd3-1c2be098945c', lineage).
narrative_ontology:cs_interpretation_layer_present('5cc95968-1c9a-4f68-bfd3-1c2be098945c').
narrative_ontology:cs_reading_relation('5cc95968-1c9a-4f68-bfd3-1c2be098945c', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('5cc95968-1c9a-4f68-bfd3-1c2be098945c', foundational, prior_navigation_confers_exclusive_trade_priority).
narrative_ontology:cs_axiom_status(prior_navigation_confers_exclusive_trade_priority, holdable).
narrative_ontology:cs_axiom_grounding('5cc95968-1c9a-4f68-bfd3-1c2be098945c', prior_navigation_confers_exclusive_trade_priority, conventional).
narrative_ontology:cs_axiom('5cc95968-1c9a-4f68-bfd3-1c2be098945c', secondary, papal_confirmation_settles_rival_claims_among_catholic_crowns).
narrative_ontology:cs_axiom_status(papal_confirmation_settles_rival_claims_among_catholic_crowns, overridden).
narrative_ontology:cs_axiom_grounding('5cc95968-1c9a-4f68-bfd3-1c2be098945c', papal_confirmation_settles_rival_claims_among_catholic_crowns, theological).
narrative_ontology:cs_reference_frame('5cc95968-1c9a-4f68-bfd3-1c2be098945c', papal_universal_temporal_jurisdiction_over_discovery).
narrative_ontology:cs_drift_state('5cc95968-1c9a-4f68-bfd3-1c2be098945c', grotian_freedom_of_seas_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5cc95968-1c9a-4f68-bfd3-1c2be098945c', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_crown_east_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_merchant_interests).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_temporal_authority_over_discovery).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, priority_of_prior_navigation_as_title_ground).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated Tordesillas after Alcáçovas and papal bulls (Inter Caetera, Aeterni Regis) already recognized its prior navigation along the African coast and into the Indian Ocean. Uses the treaty line to lock in exclusive rights to the sea route to India and the Estado da Índia's trading posts, and to bar rival crowns from the eastern hemisphere without needing to contest each voyage individually.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary).

% The administrative and commercial apparatus that collects customs, licensing fees (cartazes), and monopoly trade revenue along the Indian Ocean routes the treaty's eastern hemisphere assigns to Portugal. Its fortified trading post network depends on the treaty-backed claim to exclude competing European fleets from the same waters.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india, beneficiary,
    institutional, generational, arbitrage, global).

% Bound by the same papal-treaty framework it used to claim the west; when its own expeditions (notably Magellan-Elcano) reached the Moluccas from the east, it found Portugal's claim already asserted there and had to negotiate (eventually selling its claim at Zaragoza in 1529) rather than simply compete on the water. The treaty it helped establish now constrains its own eastward reach.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_crown_east_of_line, payer,
    institutional, generational, constrained, global).

% Has no standing under the papal grant and is treated by Portugal and Spain as an interloper east of the line. Its early voyages toward Asia are diplomatically and sometimes militarily contested on the ground that the papal division already settled the matter; England's practical response over the following century is simply to reject the treaty's authority since it never subscribed to papal jurisdiction.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown, payer,
    powerful, generational, constrained, global).

% Also excluded as a non-signatory. Francis I's remark demanding to see the clause in Adam's will granting the New World to Spain and Portugal captures the structural objection: the treaty's authority runs only as far as the other Catholic powers' willingness to recognize papal temporal jurisdiction, which France increasingly does not extend.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown, payer,
    powerful, generational, constrained, global).

% As Protestant merchants organizing later into chartered companies, they reject papal authority outright and treat the Portuguese monopoly claim east of the line as an obstacle to be broken by force and competing trade routes rather than a legal boundary to be respected. Their eventual displacement of Portuguese posts in Asia is the practical answer to the treaty's exclusionary claim.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_merchant_interests, payer,
    organized, generational, constrained, global).

% Issues and mediates the arbitration on the claimed basis of universal temporal authority over newly discovered lands and peoples, treating the line as confirming rather than creating rights already established by prior Portuguese and Spanish navigation and settlement.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, pope_alexander_vi_curia, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, pope_alexander_vi_curia, observer).

% Kingdoms and trading cities along the Indian Ocean littoral (Malacca, Hormuz, Kilwa, Calicut) whose ports and trade routes the treaty's eastern hemisphere assumes as within Portugal's zone. They are never party to the treaty and have no voice in a European document that European powers nonetheless use to arbitrate access to their own waters and harbors.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, asian_coastal_polities, excluded,
    moderate, generational, trapped, regional).

% Assess the treaty's later status as the foundation of, and eventual casualty to, the doctrine of freedom of the seas (mare liberum) that Grotius would articulate partly in response to the Portuguese monopoly claim it enabled.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, maritime_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, mutually recognized line of demarcation so the two Iberian crowns that had already been contesting overlapping exploration and trading claims (Guinea coast, Atlantic islands, the sea route to India) would not have to resolve every new discovery by separate negotiation or naval confrontation with each other.
% TRANSFER_FUNCTION: Confirms and consolidates Portugal's exclusive right to license, tax, and militarily enforce trade along the eastern sea routes and the Indian Ocean littoral, transferring the cost of exclusion onto any other European crown attempting to compete there — the treaty does not itself seize land or goods but licenses the enforcement apparatus that does.
% ABSENT_VOICES: England, France, and later Dutch and other Protestant powers were never signatories and reject papal jurisdiction outright; Asian polities whose ports and waters are being carved into a 'hemisphere' had no representation whatsoever in either the papal bulls or the treaty negotiations at Tordesillas.
% DISAPPEARANCE_RATIONALE: Without the treaty's confirmation of prior Portuguese priority, other European crowns would have had to contest Indian Ocean access through open competition or separate bilateral arrangements from the outset rather than treating Portuguese exclusivity as a legally settled baseline to be worked around or broken; the treaty measurably shaped a century of naval policy, licensing systems (cartazes), and eventual doctrinal counter-arguments (mare liberum) that would not have taken the same form.
% FOUNDING_PROBLEM: Two Iberian Catholic monarchies were on a collision course over overlapping claims to newly explored Atlantic and prospective eastern trade routes, and needed a mechanism to divide the world between themselves without recourse to war between the two Catholic powers most capable of policing the seas.
% FOUNDING_PROBLEM_CORROBORATION: Non-Iberian European powers (England's Francis I era diplomats, French crown officials, later Dutch legal theorists including Grotius) attest from outside the beneficiary set that the original bilateral Iberian rivalry the treaty solved has no bearing on their own claims, treating the papal-authority premise itself as having no force absent voluntary Catholic recognition — corroborating that the founding problem was narrowly bilateral and is long resolved or moot, while the exclusionary claim persisted well past that resolution.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored at moderate-tangled-rope level, reflecting trade-monopoly rents and exclusionary licensing rather than the higher extraction of direct territorial conquest (which belongs to the sibling reading). It rises through the sixteenth century as the Estado da Índia's cartaz system matures and falls after 1600 as Dutch and English competition erodes actual Portuguese enforcement capacity even as the treaty's nominal claim persists. Theater ratio rises steadily (0.12 to 0.55) precisely because the treaty's real enforcement power decays over the period while its ceremonial and legal invocation persists — by the mid-1600s the line is cited more than it is defended. Suppression tracks the Estado da Índia's actual naval enforcement capacity, peaking mid-period and declining as Dutch and English power grows.
 *
 * DIRECTIONALITY LOGIC:
 *   Portugal (crown and Estado da Índia) sits near the full-beneficiary end: it collects the trade revenue and the treaty's exclusionary logic runs in its favor east of the line. Spain, having negotiated the treaty as a co-equal party, is a partial payer once its own eastward ambitions (Moluccas) run into a line it helped draw — this is the treaty's internal tension. England, France, and Dutch merchant interests are structural targets: never signatories, no standing under the papal grant, and treated as illegitimate interlopers whose exclusion is the treaty's enforcement object east of the line. Asian coastal polities are excluded rather than positioned as either beneficiary or payer in the treaty's own terms — they are simply omitted from a framework that nonetheless governs access to their waters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (averting Iberian war over overlapping Atlantic and eastern claims) is authored as dead by 1663 — the two crowns' rivalry had long been overtaken by Dutch and English naval supremacy in the Indian Ocean — yet the treaty's exclusionary claim and papal-authority premise persisted in Portuguese diplomatic and legal argument well past that point (rising theater_ratio captures this). Classifying this as tangled_rope rather than snare or pure piton preserves the genuine original coordination function (a real bilateral war-avoidance mechanism) while registering that its late-period persistence functioned mostly as legal theater defending a monopoly claim increasingly unenforceable in fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_naturalness_ambiguity,
    'Is papal temporal authority over discovery a genuinely recognized universal legal order (as the Portuguese and Spanish crowns treated it) or a constructed claim that benefited only the two crowns willing to seek and honor it?',
    'Comparative analysis of contemporaneous non-Catholic legal traditions and later doctrinal developments (Grotius''s mare liberum, Vitoria''s critique of the papal grant) that treat the authority claim as never having been universally binding.',
    'If the authority is genuinely constructed rather than naturally binding, the coordination function reduces to a private bilateral agreement between two crowns dressed in universal legal language, sharpening the tangled_rope reading toward the extractive pole for non-signatory powers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_naturalness_ambiguity, conceptual, 'Whether papal temporal authority over discovery was a real universal order or a constructed convenience for the two claiming crowns.').

omega_variable(
    reading_decomposition_boundary,
    'Where exactly does the exploration-legitimation reading''s victim set (rival European crowns) stop and the conquest-legitimation reading''s victim set (indigenous populations) begin, given that both readings draw on the same 1494 line and the same papal bulls?',
    'Track how each crown''s own administrative and legal apparatus (Estado da Índia licensing records versus Spanish American encomienda and repartimiento records) invoked the treaty differently in each hemisphere — the eastern apparatus is commercial-licensing in character, the western apparatus is land-and-labor-grant in character.',
    'Confirms that the two readings are structurally distinct constraints (different extraction mechanisms, different victims, different epsilon) rather than one constraint viewed from two angles — supporting the decomposition into separate stories linked by network.affects_constraints rather than a single story with a measurement parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_decomposition_boundary, conceptual, 'Where the two kernel readings'' distinct victim sets and extraction mechanisms actually diverge in the historical record.').

omega_variable(
    asian_polity_exclusion_severity,
    'Should Asian coastal polities be treated merely as excluded non-parties (as authored here) or as an unmeasured second victim tier whose exclusion from the treaty''s own framework constitutes a distinct extractive relationship not captured by the rival-European-crown victim set?',
    'Assess Estado da Índia cartaz licensing enforcement against non-European shipping in the Indian Ocean — if Portuguese enforcement extracted tribute or restricted movement of Asian merchant shipping under color of the treaty''s claim, this would establish a second, currently unauthored victim relationship.',
    'If corroborated, this reading''s victim set would need to expand beyond rival European crowns, raising extractiveness and complicating the clean structural delta (rival powers only) this reading was authored to establish relative to the conquest-legitimation sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asian_polity_exclusion_severity, empirical, 'Whether Asian coastal polities constitute an unauthored second victim tier under Portuguese cartaz enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1663).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.12).
narrative_ontology:measurement(tord_tr_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1520, 0.18).
narrative_ontology:measurement(tord_tr_t1545, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1545, 0.25).
narrative_ontology:measurement(tord_tr_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1580, 0.32).
narrative_ontology:measurement(tord_tr_t1610, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1610, 0.42).
narrative_ontology:measurement(tord_tr_t1663, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1663, 0.55).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.42).
narrative_ontology:measurement(tord_be_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1520, 0.5).
narrative_ontology:measurement(tord_be_t1545, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1545, 0.58).
narrative_ontology:measurement(tord_be_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1580, 0.6).
narrative_ontology:measurement(tord_be_t1610, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1610, 0.55).
narrative_ontology:measurement(tord_be_t1663, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1663, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.45).
narrative_ontology:measurement(tord_su_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1520, 0.55).
narrative_ontology:measurement(tord_su_t1545, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1545, 0.62).
narrative_ontology:measurement(tord_su_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1580, 0.65).
narrative_ontology:measurement(tord_su_t1610, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1610, 0.58).
narrative_ontology:measurement(tord_su_t1663, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1663, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This story and tordesillas_demarcation_kernel__spanish_conquest_legitimation are sibling readings of a single kernel (the 1494 Treaty of Tordesillas and its underlying papal bulls). They are authored as separate constraints because their victim sets, extraction mechanisms, and epsilon values diverge sharply: this reading's extraction runs through trade-monopoly licensing against rival European crowns (moderate epsilon, tangled_rope), while the sibling reading's extraction runs through territorial conquest and forced labor against indigenous American populations (expected substantially higher epsilon, likely snare-adjacent). Per the ε-invariance principle, a single story cannot honestly carry both profiles; each reading gets its own file, linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
