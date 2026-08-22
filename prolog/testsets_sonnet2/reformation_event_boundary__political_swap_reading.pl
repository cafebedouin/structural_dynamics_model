% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Princely Asset Seizure Under Theological Cover (Political Swap Reading)
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested 'Reformation event
 *   boundary' kernel: the political-swap reading, which holds that the
 *   Reformation's structural driver was secular rulers' exploitation of
 *   theological controversy to break papal jurisdiction and seize
 *   ecclesiastical assets, with theology functioning as legitimating scaffold
 *   rather than causal engine. On this reading, the event's true
 *   periodization runs from Luther's 1517 challenge to the 1648 Peace of
 *   Westphalia, when the territorial-sovereignty settlement (not any
 *   doctrinal resolution) stabilizes. The sibling readings —
 *   theological_climb_reading (doctrine as genuine causal breakthrough) and
 *   composite_overdetermination_reading (irreducible multi-causal event) —
 *   are separate constraint stories with their own ε and stakeholder
 *   structures; this file does not average across them or hedge its ε to
 *   accommodate them, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - secular_princes: Primary beneficiary and agenda-setter (institutional/arbitrage) — converts doctrinal dispute into legal instrument for asset and jurisdiction transfer
 *   - roman_curia: Primary victim (institutional/trapped) — loses land, revenue, and appellate authority with no effective recourse
 *   - monastic_orders: Direct victim (powerless/trapped) — dissolved and confiscated by princely decree
 *   - reformed_theologians: Structurally instrumentalized beneficiary (moderate/constrained) — genuine doctrine deployed as legitimating scaffold beyond their control
 *   - diplomatic_historians: Analytical observer — reconstructs the settlement record independent of confessional narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.72).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.68).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Princely Asset Seizure Under Theological Cover (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '0167c969-30e1-449a-8c9b-ccbdcf20e793').
narrative_ontology:cs_kernel_codification('0167c969-30e1-449a-8c9b-ccbdcf20e793', distributed).
narrative_ontology:cs_authority_grounding('0167c969-30e1-449a-8c9b-ccbdcf20e793', extraction).
narrative_ontology:cs_interpretation_layer_present('0167c969-30e1-449a-8c9b-ccbdcf20e793').
narrative_ontology:cs_reading_relation('0167c969-30e1-449a-8c9b-ccbdcf20e793', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('0167c969-30e1-449a-8c9b-ccbdcf20e793', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('0167c969-30e1-449a-8c9b-ccbdcf20e793', foundational, political_interest_precedes_doctrinal_content).
narrative_ontology:cs_axiom_status(political_interest_precedes_doctrinal_content, holdable).
narrative_ontology:cs_axiom_grounding('0167c969-30e1-449a-8c9b-ccbdcf20e793', political_interest_precedes_doctrinal_content, empirically_contingent).
narrative_ontology:cs_axiom('0167c969-30e1-449a-8c9b-ccbdcf20e793', secondary, territorial_sovereignty_supersedes_ecclesiastical_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_sovereignty_supersedes_ecclesiastical_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('0167c969-30e1-449a-8c9b-ccbdcf20e793', territorial_sovereignty_supersedes_ecclesiastical_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('0167c969-30e1-449a-8c9b-ccbdcf20e793', papal_universal_jurisdiction_framework).
narrative_ontology:cs_drift_state('0167c969-30e1-449a-8c9b-ccbdcf20e793', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0167c969-30e1-449a-8c9b-ccbdcf20e793', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, english_crown).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, roman_curia).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_orders).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, local_parish_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, reformed_theologians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_populations).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, territorial_populations).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, state_supremacy_over_ecclesiastical_property).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, territorial_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German territorial rulers, Scandinavian monarchs, and eventually the English crown adopt reformed confessions as legal instruments, converting the theological dispute into cover for annexing monastic land, tithe revenue, and judicial authority previously reserved to Rome. They negotiate settlements (Augsburg, later Westphalia) that formalize the transfer of ecclesiastical assets and jurisdiction into princely hands under cuius regio, eius religio.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_princes, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, secular_princes, beneficiary).

% Loses direct taxation rights, appellate jurisdiction, and vast landholdings across northern and central Europe as territories declare for reform. Cannot militarily compel restitution and is reduced to diplomatic and propagandistic resistance; the papacy's temporal authority over affected territories is permanently curtailed by the political settlements that follow.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, roman_curia, payer,
    institutional, civilizational, trapped, continental).

% Monasteries and convents are dissolved by princely decree; their land, buildings, and endowments are confiscated and redistributed to nobility or state treasuries. Individual monastics have no legal recourse and are typically pensioned off or absorbed into secular life at the ruler's discretion.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, monastic_orders, payer,
    powerless, biographical, trapped, regional).

% Compelled to adopt the confession of their territorial ruler or face expulsion, loss of benefice, or exile, regardless of personal theological conviction. Their income, appointment, and doctrinal content are now administered by the prince's consistory rather than the bishop's curia.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, local_parish_clergy, payer,
    powerless, biographical, constrained, local).

% Provide the doctrinal architecture (justification by faith, priesthood of all believers, sola scriptura) that princes invoke to legitimate the asset transfer, and receive patronage, university chairs, and protection in return — but have limited say over how their arguments are deployed once rulers control the settlement terms. On the political-swap reading their genuine theological convictions function structurally as legitimating scaffold rather than as the causal driver of events.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, reformed_theologians, excluded,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, reformed_theologians, beneficiary).

% Subject populations have their confession, and thus much of their social and legal life, determined by their ruler's political calculation. Some gain vernacular liturgy and reduced tithe burden; many bear the costs of confessional wars (Schmalkaldic War, Thirty Years' War) fought substantially over the territorial and property settlements the theological dispute enabled.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, territorial_populations, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, territorial_populations, beneficiary).

% Reconstruct the sequence from Diet negotiations, land-transfer records, and treaty texts (Augsburg 1555, Westphalia 1648) to assess whether the political settlements, not the doctrinal disputes, are the load-bearing structure of the event's actual outcomes.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, diplomatic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_princes).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides secular rulers a legally coherent, popularly legible justification for asserting territorial sovereignty over church courts, land, and revenue that had previously answered to a transnational papal authority — solving princes' genuine problem of consolidating fragmented sovereign authority against a rival jurisdiction.
% TRANSFER_FUNCTION: Moves ecclesiastical land, tithe revenue, judicial authority, and clerical appointment power from the Roman curia and monastic institutions to territorial princes and monarchs, using theological legitimation as the transfer mechanism and confessional war/settlement as the enforcement instrument.
% ABSENT_VOICES: The Roman curia's own claim that doctrinal continuity, not property loss, is the central stake is treated on this reading as self-interested obscurantism rather than evidence; ordinary parishioners forced to convert have no documented voice in the princely settlements that determine their confession.
% DISAPPEARANCE_RATIONALE: Remove the property-and-jurisdiction transfer mechanism and the theological dispute plausibly remains a doctrinal controversy contained within the existing ecclesiastical-political order (as earlier heresies like the Hussite and Waldensian movements largely were) rather than producing a permanent redrawing of sovereign territorial control across northern Europe.
% FOUNDING_PROBLEM: Territorial rulers faced a structural problem: papal fiscal exactions (annates, indulgence revenue), extraterritorial appellate jurisdiction, and independent monastic landholding constrained emerging state sovereignty and drained resources rulers wanted for their own treasuries and wars.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal and administrative historians working from state treasury and land-registry archives (outside both the reformed theological tradition and the curia) attest that the sovereignty and revenue problem the settlements solved was substantially resolved by Westphalia in 1648; the confessional framing that persisted afterward is corroborated by these archival sources as serving legitimation rather than continuing to solve an active jurisdictional problem.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply from 1517 (0.35, dispute is still primarily doctrinal) to 1555 (0.65, Peace of Augsburg formalizes territorial confessional control and property transfer) and peaks near 1618-1648 as confessional war becomes the enforcement mechanism for consolidating and re-litigating the property settlements (0.75 at the outbreak of the Thirty Years' War), settling slightly by 1648 (0.72) once Westphalia locks in the transfer permanently. Theater ratio tracks the widening gap between the theological rhetoric deployed in public disputation and the land-registry/treasury reality of asset transfer — by the 1580s the doctrinal argument is substantially serving legitimation of an already-completed transfer rather than driving further genuine doctrinal contest. Suppression (enforcement of confessional conformity, cuius regio eius religio, war) rises steadily and peaks during the Thirty Years' War (0.78 at 1618) before settling at Westphalia's negotiated toleration provisions (0.68).
 *
 * PERSPECTIVAL GAP:
 *   From the secular princes' seat this reads as legitimate assertion of territorial sovereignty against an overreaching foreign jurisdiction — coordination that solves a genuine governance problem. From the Roman curia's and monastic orders' seats it is straightforward asset seizure conducted under theological pretext, with no meaningful voice in the settlement terms. The engine should compute divergent per-seat types from this same structural data: agenda-setter seats trend toward coordination readings, payer seats toward extraction readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular princes and the English crown sit near the full-beneficiary end: they set the settlement terms, collect the transferred assets, and retain arbitrage-grade exit (they can recalibrate confessional alignment as political interest shifts, as England's own mid-Tudor confessional whiplash demonstrates). The Roman curia and monastic orders sit near the full-target end: trapped, institutional or powerless, bearing the transfer with no comparable recourse. Reformed theologians occupy an intermediate position — genuine beneficiaries of patronage and protection, but without control over how their doctrine is instrumentalized once princely power takes over the settlement process; this is why their situation notes the scaffold function explicitly rather than treating them as straightforward victims or straightforward beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — princely subordination to papal fiscal and jurisdictional authority — is authored as dead by 1648: the territorial sovereignty settlement fully resolves it. But the confessional-political apparatus (state churches, established religion, confession-linked civil rights restrictions) persists for centuries after the jurisdictional problem it solved has been settled, which is the signature the political-swap reading treats as diagnostic: the theological framing continued to do legitimating work long after its stated doctrinal stakes had been resolved through property and treaty law, not through theological consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_as_cause_or_cover,
    'Did Luther''s doctrine of justification by faith alone function as a genuine independent causal force that would have produced institutional rupture regardless of princely interest, or did it function primarily as available legitimating material that princes selected and deployed because it served pre-existing sovereignty and fiscal interests?',
    'Comparative analysis of cases where theological dissent arose without matching princely fiscal/jurisdictional interest (e.g., earlier heresies suppressed without political sponsorship) versus cases where doctrine and princely interest aligned (German territories, England, Scandinavia) and produced lasting institutional rupture; convergence toward the aligned cases supports the political-swap reading.',
    'If theology functioned as a genuinely independent causal driver, this reading''s classification collapses toward the theological_climb_reading''s structure and its extraction/beneficiary attributions would need revision; if theology functioned as available legitimating material, the tangled_rope classification with princes as primary beneficiaries stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_as_cause_or_cover, conceptual, 'Whether Reformation theology was causally load-bearing or instrumentally selected cover.').

omega_variable(
    periodization_terminus_choice,
    'Is 1648 (Westphalia, political-territorial settlement) the correct terminus for ''the Reformation as event,'' or does a theologically-indexed terminus (e.g., Trent''s close in 1563, or the stabilization of confessional catechisms) better capture when the phenomenon actually concluded?',
    'Compare institutional stability metrics (confessional switching rate, land-title litigation volume, treaty renegotiation frequency) after each candidate terminus date to determine which marks the actual stabilization point of the outcomes this reading tracks.',
    'Choosing a different terminus would shift the measured extraction and suppression trajectory shape and could alter where the theater_ratio peak is dated, though the qualitative claim (political settlement, not doctrinal consensus, is the stabilizing event) is robust across plausible terminus choices in the 1555-1648 range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_terminus_choice, conceptual, 'Whether Westphalia or an earlier theological milestone better periodizes the event on this reading''s own terms.').

omega_variable(
    curia_self_interest_discount,
    'How much should the Roman curia''s own historical complaints about asset seizure be discounted as self-interested testimony from the losing party, versus treated as accurate first-hand accounting of what was actually transferred?',
    'Cross-reference curial complaint records against independent land-registry, tax-roll, and treasury documentation from the receiving territories to establish whether the scale of transfer claimed matches the scale documented by neutral administrative sources.',
    'If independent administrative records substantially corroborate curial claims of scale, the victim characterization is strongly evidenced; if they diverge significantly, the extraction magnitude authored here may be overstated relative to the political-swap reading''s own evidentiary standards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curia_self_interest_discount, empirical, 'Whether the scale of asset transfer is independently corroborated beyond the losing party''s own account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.3).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__political_swap_reading, theater_ratio, 1530, 0.42).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.55).
narrative_ontology:measurement(refo_tr_t1580, reformation_event_boundary__political_swap_reading, theater_ratio, 1580, 0.6).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.62).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.58).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__political_swap_reading, base_extractiveness, 1530, 0.52).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.65).
narrative_ontology:measurement(refo_be_t1580, reformation_event_boundary__political_swap_reading, base_extractiveness, 1580, 0.7).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.75).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__political_swap_reading, suppression_requirement, 1530, 0.55).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.6).
narrative_ontology:measurement(refo_su_t1580, reformation_event_boundary__political_swap_reading, suppression_requirement, 1580, 0.65).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.78).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__political_swap_reading, 0.1).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the reformation_event_boundary kernel, each authored as a structurally distinct constraint with its own ε per the ε-invariance principle. political_swap_reading (this file): tangled_rope, high extraction (~0.72), princes as beneficiaries, curia/monasteries as victims, periodized to 1648. theological_climb_reading: authors doctrine as genuine causal breakthrough, plausibly lower extraction and different beneficiary/victim structure (theologians and reformed communities as primary agents rather than instrumentalized scaffold). composite_overdetermination_reading: refuses a single dominant driver and would author an irreducibly mixed structure resisting simple beneficiary/victim assignment. All three are linked bidirectionally through network.affects_constraints as members of one contested kernel; none averages or hedges against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
