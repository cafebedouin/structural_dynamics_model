% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Tordesillas Demarcation â Portuguese Exploration Legitimation
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1494 Treaty of Tordesillas and the papal bulls that underwrote it
 *   divided the non-European world between Portugal and Castile along a
 *   meridian. This constraint story captures the Portuguese
 *   exploration-legitimation reading of that kernel: the demarcation is read
 *   not primarily as a license for indigenous subjugation but as a
 *   confirmation of prior Portuguese discovery rights and a legal barrier
 *   excluding rival European powers from eastern commerce. The Estado da
 *   Ãndia captured the trade rents; rival crowns bore the cost of exclusion;
 *   indigenous populations were affected but are not the primary victim set
 *   in this reading. The claim is tangled_rope because the arrangement had a
 *   genuine coordination function (preventing Iberian war and financing
 *   long-distance navigation) while also operating as an asymmetric
 *   extraction mechanism (monopoly rents enforced by naval power and papal
 *   authority).
 *
 * KEY AGENTS:
 *   - Portuguese Estado da Ãndia (institutional/beneficiary) â captures eastern trade monopoly rents
 *   - Rival European powers (powerful/payer) â bear costs of exclusion from Asian commerce
 *   - Portuguese Crown (institutional/agenda_setter) â procured papal confirmation and set enforcement policy
 *   - Papal Curia (institutional/observer) â supplied the legal-theological framework without material extraction
 *   - Indigenous peoples east of the line (powerless/excluded) â materially affected but absent from legitimating discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.72).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Demarcation â Portuguese Exploration Legitimation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'be23eb9f-350f-4ffb-8b0d-310a019e1334').
narrative_ontology:cs_kernel_codification('be23eb9f-350f-4ffb-8b0d-310a019e1334', formalized).
narrative_ontology:cs_authority_grounding('be23eb9f-350f-4ffb-8b0d-310a019e1334', lineage).
narrative_ontology:cs_interpretation_layer_present('be23eb9f-350f-4ffb-8b0d-310a019e1334').
narrative_ontology:cs_reading_relation('be23eb9f-350f-4ffb-8b0d-310a019e1334', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('be23eb9f-350f-4ffb-8b0d-310a019e1334', foundational, prior_discovery_confirms_eastern_commercial_monopoly).
narrative_ontology:cs_axiom_status(prior_discovery_confirms_eastern_commercial_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('be23eb9f-350f-4ffb-8b0d-310a019e1334', prior_discovery_confirms_eastern_commercial_monopoly, conventional).
narrative_ontology:cs_axiom('be23eb9f-350f-4ffb-8b0d-310a019e1334', foundational, papal_grant_as_commerce_exclusion_instrument).
narrative_ontology:cs_axiom_status(papal_grant_as_commerce_exclusion_instrument, holdable).
narrative_ontology:cs_axiom_grounding('be23eb9f-350f-4ffb-8b0d-310a019e1334', papal_grant_as_commerce_exclusion_instrument, conventional).
narrative_ontology:cs_reference_frame('be23eb9f-350f-4ffb-8b0d-310a019e1334', papal_universal_jurisdiction_framework).
narrative_ontology:cs_drift_state('be23eb9f-350f-4ffb-8b0d-310a019e1334', post_reformation_naval_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be23eb9f-350f-4ffb-8b0d-310a019e1334', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_universal_temporal_jurisdiction).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, mare_clausum_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered Portuguese monopoly over eastern trade routes and colonial possessions from Goa; collected customs and spice-trade rents; benefited from the papal demarcation which excluded other European crowns from the Indian Ocean commercial basin.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    institutional, generational, arbitrage, global).

% French, English, Dutch and other European crowns were formally barred by papal grant and Iberian naval enforcement from trading east of the Tordesillas line; bore the cost of exclusion from Asian spice commerce and had to choose between accepting second-mover status, covertly violating the line, or mounting expensive naval challenges.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, generational, constrained, continental).

% Negotiated the 1494 demarcation with Castile and procured papal bulls to confirm prior Portuguese Atlantic and Indian Ocean exploration; set the legal and naval policy that the Estado da Ãndia enforced; retained ultimate authority to alter or abandon the monopoly but chose to maintain it for dynastic revenue.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary).

% Issued the bulls Inter Caetera and related decrees that established the demarcation line; supplied the theological-legal framework of papal donation and universal jurisdiction over non-Christian lands; did not extract material rents from the arrangement but vindicated the Church's claimed authority to allocate newly discovered territories.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, observer,
    institutional, civilizational, analytical, universal).

% Populations of coastal Africa, India, and maritime Asia whose lands and waters were traversed and claimed under the Portuguese monopoly; were not parties to the treaty negotiations and were structurally absent from its legitimating discourse, though materially affected by the trade exclusion and fortified presence it enabled.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_peoples_east, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divided the world outside Europe between Iberian crowns to prevent intra-Catholic war over discovery claims, and to legitimize a single-crown monopoly structure for financing and protecting long-distance maritime commerce to Asia.
% TRANSFER_FUNCTION: Transferred exclusive rights to navigate, trade, and establish fortified factories east of the demarcation line from the broader community of European powers to the Portuguese Crown and its Estado da Ãndia, underwritten by papal authority and enforced by naval power.
% ABSENT_VOICES: Indigenous peoples of Africa and maritime Asia were not consulted and had no seat in the negotiations; Protestant and northern European merchants were materially affected but initially lacked diplomatic leverage to challenge the papal grant within the Catholic legal framework.
% DISAPPEARANCE_RATIONALE: If the demarcation and its papal confirmation had vanished in the early sixteenth century, Portuguese monopoly over the Cape Route would have lost its primary legal shield; rival European powers would have entered Asian trade decades earlier; the institutional pattern of Iberian commercial exclusivity would not have structured the first century of European expansion in the Indian Ocean.
% FOUNDING_PROBLEM: Preventing war between the Crowns of Portugal and Castile over competing claims to newly discovered non-Christian lands, and securing a recognized legal framework that would protect Portuguese maritime investment in the Atlantic and Indian Oceans.
% FOUNDING_PROBLEM_CORROBORATION: Portuguese royal correspondence and chronicles attest the immediate fear of Castilian rivalry. Modern historians (Boxer, Diffie, Winius) corroborate that intra-Iberian competition was the proximate motive. Northern European diplomatic archives and Protestant legal theorists attest from outside the beneficiary set that by the mid-sixteenth century the original rivalry was superseded by Dutch, English, and French entry, rendering the founding problem obsolete.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is moderate-to-high because the Portuguese Crown and Estado da Ãndia collected spice-trade rents substantially above the cost of coordination. Suppression (0.72) is higher than extraction because the constraint's persistence depended on active naval enforcement in the Indian Ocean and the diplomatic suppression of rival claims, not merely on voluntary compliance. Theater_ratio (0.52) is elevated: by the end of the interval the papal legal framework had become increasingly performative, masking raw naval power and habitual monopoly. Accessibility_collapse (0.48) reflects that alternative European trading arrangements were partly but not fully closed â rivals could and eventually did circumvent the line. Resistance (0.58) captures sustained diplomatic and naval opposition from French, Dutch, and English actors.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese Crown's seat the demarcation was a legitimate coordination mechanism that prevented war with Castile and amortized the massive cost of Atlantic and Indian Ocean navigation. From the seat of rival European powers the same structure was an enforced exclusion from lucrative commerce, maintained by papal fiction and Iberian naval coercion. The engine computes this divergence from the same structural facts: low directionality for the Crown and Estado (beneficiaries with arbitrage-grade exit), high directionality for rival crowns (payers with constrained exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Portuguese Estado da Ãndia, Crown) sit near the full-beneficiary end: the constraint subsidizes their monopoly and they control its enforcement. Victims (rival European powers) sit near the full-target end: the constraint extracts from them the opportunity cost of excluded trade and forces them into costly circumvention or subordination. Indigenous peoples are excluded rather than directly targeted in this reading; their directionality is not the axis that drives classification here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing Iberian war over discoveries â was substantially solved by the mid-sixteenth century and became moot after the Iberian Union of 1580. Yet the monopoly structure persisted because the Estado da Ãndia and the Crown continued to extract rents from it. The divergence between founding_problem_status (dead) and disappearance_verdict (world_rearranges) flags the mandatrophy: the constraint outlived its coordinating justification and persisted as extraction. Without the R5 genealogy interview, this would be misread as a still-functional coordination device; with it, the engine detects the zombie transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope,
    'Is the Tordesillas demarcation kernel best read as a trade-monopoly device (Portuguese exploration legitimation) or as a territorial-conquest license (Spanish conquest legitimation)?',
    'Comparative analysis of primary enforcement costs: naval patrols and customs posts in the Indian Ocean versus encomienda and territorial administration in the Americas; examination of papal-royal correspondence to see which beneficiary structure dominated the legal imagination.',
    'If the territorial-conquest reading is primary, the constraint family should be re-centered on land dispossession and indigenous victimhood; if the trade-monopoly reading holds, this constraint remains the structurally dominant one for the eastern hemisphere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Sibling reading ambiguity between trade monopoly and territorial conquest as the kernel''s primary function.').

omega_variable(
    papal_authority_binding_force,
    'Was papal authority over newly discovered lands a enforceable legal constraint on non-Catholic powers, or merely a Catholic coordination fiction?',
    'Diplomatic records of French, English, and Dutch responses to Iberian monopoly claims; frequency of naval confrontation versus diplomatic acknowledgment of the line.',
    'If non-Catholic powers never recognized the constraint, suppression was lower than Iberian sources suggest and the constraint''s persistence relied more on naval power than on legal legitimacy; this would shift effective extraction toward raw coercion rather than legitimated coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_binding_force, empirical, 'Whether papal jurisdiction bound non-Catholic European actors.').

omega_variable(
    monopoly_rent_vs_coordination_cost,
    'Did the Portuguese monopoly over eastern routes extract rents above the coordination cost of preventing intra-European naval war?',
    'Economic comparison of Portuguese spice markups against plausible competitive-market prices; analysis of naval expenditure required for route patrolling versus monopoly profit.',
    'A large rent-cost gap would confirm asymmetric extraction in the tangled rope; a narrow gap would suggest the constraint was closer to a genuine coordination mechanism for collective exploration financing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monopoly_rent_vs_coordination_cost, empirical, 'Whether Portuguese extraction exceeded the inherent cost of coordinating long-distance maritime expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tordesillas_port_tr_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tordesillas_port_tr_t10, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 10, 0.24).
narrative_ontology:measurement(tordesillas_port_tr_t20, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tordesillas_port_tr_t30, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 30, 0.38).
narrative_ontology:measurement(tordesillas_port_tr_t45, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 45, 0.48).
narrative_ontology:measurement(tordesillas_port_tr_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(tordesillas_port_be_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(tordesillas_port_be_t10, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(tordesillas_port_be_t20, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(tordesillas_port_be_t30, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(tordesillas_port_be_t45, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(tordesillas_port_be_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tordesillas_port_su_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tordesillas_port_su_t10, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(tordesillas_port_su_t20, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(tordesillas_port_su_t30, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(tordesillas_port_su_t45, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 45, 0.76).
narrative_ontology:measurement(tordesillas_port_su_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% The Tordesillas demarcation kernel decomposes into two structurally distinct constraints: a Portuguese reading centered on commercial monopoly and European rival exclusion east of the line (this file), and a Spanish reading centered on territorial conquest and indigenous subjugation west of the line. They share the same papal bulls and treaty text but have different epsilon values, different victim sets, and different primary extraction modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
