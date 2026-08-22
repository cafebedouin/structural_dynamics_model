% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Exclusive-Isaac Covenant Transmission Boundary (Genesis 17:19-21)
 *   domain: religious/institutional-authority
 *
 * SUMMARY:
 *   Within the rabbinic transmission of the Abrahamic covenant, Genesis
 *   17:19-21 is read to fix covenant succession in Isaac alone: the promise
 *   is established with Isaac, while Ishmael — though blessed, fruitful, and
 *   father of twelve princes — stands outside the covenant line. This reading
 *   operates as an identity boundary: it defines who may count within the
 *   covenant community, grounds descent and conversion adjudication, and has
 *   carried a dispersed minority's coherence for two millennia. Its operation
 *   imposes a categorical cost on those who claim inheritance through
 *   Ishmael, including the later tradition that reads the succession as
 *   continuing through him. Per the epsilon-invariance principle, the
 *   colloquial label 'the Abrahamic covenant' decomposes into separate
 *   constraint stories: this file instantiates ONLY the exclusive-Isaac
 *   reading, with its own epsilon, beneficiary set, and victim set; the
 *   ishmael_covenant_reading and christian_supersessionist_reading are
 *   separate stories linked through network.affects_constraints, as is the
 *   territorial component (land_promise_constraint). The claimed type and the
 *   metrics below are independent authored facts: the claim states what this
 *   reading is structurally; the metrics describe how it operates.
 *
 * KEY AGENTS:
 *   - - rabbinic_authority_institutions: Agenda-setter (institutional/identity_locked) — administers the boundary, collects interpretive authority from its stability
 *   - - organized_jewish_communities: Primary beneficiary (organized/identity_locked) — receives identity continuity; bears intercommunal friction secondarily
 *   - - ishmaelite_lineage_claimants: Primary target (powerless/trapped) — bears categorical exclusion, holds no seat in the adjudication
 *   - - islamic_abrahamic_succession_tradition: Secondary target (institutional/identity_locked) — rival succession claim rendered void within this framework
 *   - - comparative_religion_scholars: Analytical observer (analytical/analytical) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.7).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.6).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Exclusive-Isaac Covenant Transmission Boundary (Genesis 17:19-21)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/institutional-authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '99c44da7-671a-4ee2-bbe3-d72921de4bd4').
narrative_ontology:cs_kernel_codification('99c44da7-671a-4ee2-bbe3-d72921de4bd4', fixed_text).
narrative_ontology:cs_authority_grounding('99c44da7-671a-4ee2-bbe3-d72921de4bd4', lineage).
narrative_ontology:cs_interpretation_layer_present('99c44da7-671a-4ee2-bbe3-d72921de4bd4').
narrative_ontology:cs_reading_relation('99c44da7-671a-4ee2-bbe3-d72921de4bd4', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('99c44da7-671a-4ee2-bbe3-d72921de4bd4', abrahamic_covenant__christian_supersessionist_reading, influences).
narrative_ontology:cs_axiom('99c44da7-671a-4ee2-bbe3-d72921de4bd4', foundational, covenant_exclusive_to_isaac_line).
narrative_ontology:cs_axiom_status(covenant_exclusive_to_isaac_line, holdable).
narrative_ontology:cs_axiom_grounding('99c44da7-671a-4ee2-bbe3-d72921de4bd4', covenant_exclusive_to_isaac_line, theological).
narrative_ontology:cs_axiom('99c44da7-671a-4ee2-bbe3-d72921de4bd4', foundational, ishmael_blessed_but_outside_covenant).
narrative_ontology:cs_axiom_status(ishmael_blessed_but_outside_covenant, holdable).
narrative_ontology:cs_axiom_grounding('99c44da7-671a-4ee2-bbe3-d72921de4bd4', ishmael_blessed_but_outside_covenant, theological).
narrative_ontology:cs_reference_frame('99c44da7-671a-4ee2-bbe3-d72921de4bd4', isaac_exclusive_election_charter).
narrative_ontology:cs_drift_state('99c44da7-671a-4ee2-bbe3-d72921de4bd4', contemporary_pluralist_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('99c44da7-671a-4ee2-bbe3-d72921de4bd4', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority_institutions).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, organized_jewish_communities).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_abrahamic_succession_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, organized_jewish_communities).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, lineage_election_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, genesis_17_exclusive_limiting_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret, transmit, and adjudicate the covenant-transmission rule: they certify descent and conversion, decide who may be counted within the covenant community, and teach the Genesis 17 reading that fixes succession in Isaac. Their office rests on an unbroken chain of transmission that certifies this reading; revising it would unsettle the ground on which their own authority stands.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Live inside a fixed membership charter that has kept a dispersed minority coherent across expulsions, conversion pressure, and assimilation pressure for two millennia. The charter gives them durable boundaries and inherited identity; it also generates standing friction with neighboring traditions that honor the same ancestors under different succession claims, a friction their members absorb in daily intercommunal life.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, organized_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, organized_jewish_communities, payer).

% Descendant communities tracing inheritance through Abraham's firstborn son. Under this reading their claim is categorically void: the verses they also revere name Ishmael blessed and father of a great nation, yet establish the covenant elsewhere. They were not party to the interpretive adjudication that fixed the exclusion and hold no seat in the institutions that maintain it.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants, excluded).

% A civilization-spanning tradition that reads the same patriarchal narratives as culminating in a succession through Ishmael to Muhammad. The exclusive reading renders its succession claim void within the rabbinic framework, anchoring a standing legitimacy dispute between the two traditions that neither side's texts resolve for the other.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_abrahamic_succession_tradition, payer,
    institutional, civilizational, identity_locked, global).

% Study the composition history of Genesis and the institutional career of the exclusive reading. They document that the text itself preserves parallel threads — Ishmael's blessing, nationhood, and twelve princes alongside Isaac's election — and that the exclusive synthesis is one adjudication among several the material permits. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, comparative_religion_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority_institutions).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the membership-definition problem for a small, dispersed community: a fixed genealogical charter tells members and neighbors alike who carries the promise, stabilizing identity across exile, intermarriage pressure, and the absence of territorial anchors.
% TRANSFER_FUNCTION: Moves recognized covenant standing — the legitimate claim to Abrahamic election — exclusively along Isaac's line, and correspondingly withholds recognition from Ishmael's descendants and from later traditions claiming succession through him. The transferred good is status and legitimacy, not wealth.
% ABSENT_VOICES: Ishmael and his descendants are spoken about in the governing text but were never party to the adjudication that excluded them; the exclusion was authored and maintained entirely within Isaac-line interpretive institutions. Contemporary Muslim hermeneuts, who read the same verses to the opposite effect, likewise stand outside the rabbinic conversation — their objection registers only as the rival claim this reading denies.
% DISAPPEARANCE_RATIONALE: If the exclusive-Isaac boundary vanished overnight, the membership architecture built on it — descent certification, conversion thresholds, the community's self-understanding as a distinct covenant people — would require immediate reconstruction, and the legitimacy dispute with Ishmaelite succession claims would lose its fixed terms. Arrangements in both traditions depend on the boundary's existence, even where they contest its content.
% FOUNDING_PROBLEM: Abraham's household presented the promise with an unresolved succession problem: multiple sons, competing mothers, and a divine promise whose carrier the narrative must fix. The reading resolves the ambiguity by establishing the covenant with Isaac alone (Genesis 17:19-21), converting genealogical open-endedness into a single determinate line of transmission.
% FOUNDING_PROBLEM_CORROBORATION: Academic biblical scholarship — outside every benefiting party — corroborates that Genesis preserves competing succession threads requiring adjudication. Islamic tradition corroborates that the succession question remains live, while answering it oppositely. No corroboration exists for the exclusivity answer from outside the benefiting parties; the corroboration attaches to the problem, not to this reading's resolution of it.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.70: the boundary's principal product for outsiders is categorical status denial, and the expected structural delta for this reading specifies high epsilon for the exclusion. Suppression is 0.60, authored as a raw structural property — the hermeneutical closure by which the framework admits no inclusive alternative — and is NOT scaled by power or scope; the engine scales only extractiveness. Theater is low (0.20): the boundary function is operative in every conversion and descent ruling, not merely performed, though liturgical rehearsal of election adds a performative layer. Accessibility collapse is 0.55: within the framework the inclusive alternative collapses once the reading is granted, but the sibling readings remain live outside it, so alternatives do not vanish. Resistance is 0.60: the reading meets sustained counter-claim from the Islamic succession tradition and critical scrutiny from academic scholarship. All three tracked metrics run on one shared six-point grid (t=0..100) so no metric row borrows another's endpoints; the extractiveness trajectory tracks the hardening of the boundary as the rival succession claim grew from local presence to civilization-scale competitor.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat, the arrangement is faithful transmission: the institutions experience the boundary as obedience to a received text, and their identity_locked position means the reading and their authority are one fabric. From the trapped payer seat, the same verses operate as categorical status denial issued by a process that never admitted them. The Islamic-tradition seat, institutionally powerful and identity_locked, experiences the reading as a standing challenge to its own succession charter rather than as a cost it can exit. The engine computes these divergent per-seat classifications from power, exit, and directional data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic institutions sit nearest the beneficiary end: they administer the boundary and collect interpretive authority from its stability. Organized communities also sit low — they receive identity continuity — though their secondary exposure to intercommunal friction lifts them slightly off the floor. Ishmaelite claimants sit near the full-target end: powerless within the framework, categorically excluded, with no exit into membership. The Islamic succession tradition is also target-side despite institutional power: its identity_locked position means the denial strikes its constitutive claim, and its scale purchases no exit. Global scope for the maintaining institutions and the rival tradition amplifies effective extraction on the target side through verification difficulty at scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fixing an indeterminate succession — remains live for the community that maintains the reading: every conversion ruling and descent adjudication re-exercises it. This is not a mandate outliving its function, and the story declares no resolved mandatrophy. The classification matters in both directions: reading the arrangement as pure extraction erases the identity-coordination work that carried a stateless minority through two millennia of dispersion; reading it as pure coordination erases the categorical, unchosen cost the boundary imposes on excluded claimant populations. The tangled-rope claim keeps both facts on the table while the engine computes per-seat types from the structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_multiplicity,
    'This constraint is one reading of the abrahamic_covenant kernel; the same transmitted text instantiates structurally different constraints under the ishmael_covenant_reading and christian_supersessionist_reading siblings — which reading a community adopts determines the entire beneficiary/victim structure.',
    'Comparative analysis across the sibling stories: each reading''s beneficiary/victim arrays and epsilon over the shared referent; adoption is tracked by which community''s interpretive authority governs membership in practice.',
    'If the ishmael_covenant_reading were adopted instead, the victim set inverts (Isaac-line exclusivity becomes the denied position) and the coordination function shifts from boundary-maintenance-for-minority-continuity to universal-succession-legitimation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'Kernel membership: one text, multiple readings, structurally distinct constraints.').

omega_variable(
    exclusivity_hermeneutical_location,
    'Where in the text does the disagreement live: does Genesis 17:19 (''my covenant I will establish with Isaac'') together with 17:21 function as exclusive limitation, or does 17:20''s blessing of Ishmael (twelve princes, a great nation) make verse 21 additive emphasis rather than exclusion?',
    'Philological analysis of the verb forms and the force of the contrast in verses 19-21 across Masoretic, Samaritan, Septuagint, and targumic witnesses, together with the documented adjudication history in rabbinic and Islamic exegesis.',
    'An exclusive-limiting finding sustains this constraint with its current victim set; an additive-emphasis finding collapses the exclusion and merges this story''s structure toward the ishmael_covenant_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_hermeneutical_location, conceptual, 'Whether the textual warrant supports exclusivity or merely emphasis.').

omega_variable(
    status_denial_extraction_question,
    'Does denial of covenant status impose a cost on the excluded (making the boundary extractive), or is non-membership a neutral state the covenant was never obliged to confer?',
    'Not resolvable by data alone: it depends on whether election is modeled as a distributable good (then denial extracts) or a particular vocation owing nothing universal (then denial is mere definition). Track how excluded claimant communities themselves experience and litigate the denial.',
    'If neutral, effective extraction drops toward coordination-cost levels and the reading classifies nearer rope; if a status good, the measured extraction stands and the tangled-rope/snare tension sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_denial_extraction_question, preference, 'Whether covenant status is a good whose denial extracts from the excluded.').

omega_variable(
    enforcement_mechanism_genealogy,
    'Is the boundary''s current enforcement machinery (descent adjudication, conversion procedure) continuous with the founding genealogical rule, or a later rabbinic construction retrojected onto the text?',
    'Historical-philological dating of the matrilineal principle and conversion procedures against the biblical patrilineal household structure they claim to administer.',
    'If discontinuous, part of the measured suppression is later institutional accretion rather than the founding constraint''s own force, shifting attribution of enforcement costs across the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_genealogy, empirical, 'Continuity between the founding rule and the machinery that enforces it today.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(abra_tr_t0, observed).
narrative_ontology:measurement(abra_tr_t20, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(abra_tr_t20, observed).
narrative_ontology:measurement(abra_tr_t40, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(abra_tr_t40, observed).
narrative_ontology:measurement(abra_tr_t60, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(abra_tr_t60, observed).
narrative_ontology:measurement(abra_tr_t80, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement_basis(abra_tr_t80, observed).
narrative_ontology:measurement(abra_tr_t100, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement_basis(abra_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(abra_be_t0, observed).
narrative_ontology:measurement(abra_be_t20, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(abra_be_t20, observed).
narrative_ontology:measurement(abra_be_t40, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(abra_be_t40, observed).
narrative_ontology:measurement(abra_be_t60, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(abra_be_t60, observed).
narrative_ontology:measurement(abra_be_t80, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(abra_be_t80, observed).
narrative_ontology:measurement(abra_be_t100, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 100, 0.7).
narrative_ontology:measurement_basis(abra_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(abra_su_t0, observed).
narrative_ontology:measurement(abra_su_t20, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(abra_su_t20, observed).
narrative_ontology:measurement(abra_su_t40, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement_basis(abra_su_t40, observed).
narrative_ontology:measurement(abra_su_t60, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(abra_su_t60, observed).
narrative_ontology:measurement(abra_su_t80, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 80, 0.59).
narrative_ontology:measurement_basis(abra_su_t80, observed).
narrative_ontology:measurement(abra_su_t100, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement_basis(abra_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Abrahamic covenant' covers structurally distinct claims and is decomposed per the epsilon-invariance principle. This story instantiates only the exclusive-Isaac transmission reading, with its own epsilon (0.70), beneficiary set (rabbinic institutions, organized communities), and victim set (Ishmaelite claimants, Islamic succession tradition). The ishmael_covenant_reading inverts the victim structure from the same kernel text; the christian_supersessionist_reading relocates transmission away from lineage entirely; land_promise_constraint decomposes the territorial-grant component, whose conditionality disputes map onto a separate conflict surface. All four are linked through affects_constraints; upstream textual-establishment confidence flows downstream into the more contested readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
