% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority â Composite Overdetermination Reading
 *   domain: ecclesiological/institutional
 *
 * SUMMARY:
 *   This constraint is the composite_overdetermination reading of the Vatican
 *   II magisterial authority kernel. It treats the Second Vatican Council not
 *   as a single coherent ecclesiological event but as an overdetermined
 *   composite: the conciliar texts were deliberately crafted with ambiguous
 *   compromise formulations to secure supermajority votes, encoding
 *   incompatible continuity and rupture visions simultaneously. The real
 *   locus of authority therefore shifted from the texts themselves to the
 *   hermeneutical apparatus that controls their interpretation. The Roman
 *   Curia administers this ambiguity, enforcing a continuity hermeneutic
 *   while the text's rupture elements remain structurally present.
 *   Implementation divergence across local churches is a designed feature,
 *   not a bug, and the persistent 10â12 percent rejection votes signal
 *   unresolved theological incompatibility embedded in the final texts.
 *
 * KEY AGENTS:
 *   - Roman Curia (institutional/identity_locked) â sets and enforces the authorized hermeneutic, collects centralized interpretive authority
 *   - Continuity faction clergy (powerful/identity_locked) â institutional beneficiaries of the authorized reading
 *   - Traditionalist dissenters (organized/identity_locked) â pay by canonical irregularity and marginalization for rejecting ambiguous authority
 *   - Progressive reformers (moderate/constrained) â pay by suppression when pushing rupture elements beyond Roman tolerance
 *   - Local bishops (powerful/constrained) â pay by loss of autonomous teaching authority to centralized interpretation
 *   - Conciliar historians (analytical/analytical) â document the overdetermined drafting from outside the beneficiary set
 *   - Catholic laity (powerless/identity_locked) â excluded from hermeneutical debate, identity shaped by the Church's coherence claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.56).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority â Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiological/institutional").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'd8f19363-6907-4806-a7db-f511e39a1934').
narrative_ontology:cs_kernel_codification('d8f19363-6907-4806-a7db-f511e39a1934', fixed_text).
narrative_ontology:cs_authority_grounding('d8f19363-6907-4806-a7db-f511e39a1934', lineage).
narrative_ontology:cs_interpretation_layer_present('d8f19363-6907-4806-a7db-f511e39a1934').
narrative_ontology:cs_reading_relation('d8f19363-6907-4806-a7db-f511e39a1934', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8f19363-6907-4806-a7db-f511e39a1934', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('d8f19363-6907-4806-a7db-f511e39a1934', foundational, conciliar_texts_encode_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('d8f19363-6907-4806-a7db-f511e39a1934', conciliar_texts_encode_incompatible_visions, empirically_contingent).
narrative_ontology:cs_axiom('d8f19363-6907-4806-a7db-f511e39a1934', foundational, hermeneutical_control_as_authority_locus).
narrative_ontology:cs_axiom_status(hermeneutical_control_as_authority_locus, holdable).
narrative_ontology:cs_axiom_grounding('d8f19363-6907-4806-a7db-f511e39a1934', hermeneutical_control_as_authority_locus, conventional).
narrative_ontology:cs_reference_frame('d8f19363-6907-4806-a7db-f511e39a1934', textual_overdetermination_as_constitutive).
narrative_ontology:cs_drift_state('d8f19363-6907-4806-a7db-f511e39a1934', post_conciliar_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d8f19363-6907-4806-a7db-f511e39a1934', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, continuity_faction_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_dissenters).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reformers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_bishops).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the authoritative interpretation of conciliar texts through the Congregation for the Doctrine of the Faith and the papal magisterium. Controls which theological readings receive institutional approval and which are censured. Its exit would require abandoning the universal teaching office and the claim to definitive hermeneutical authority.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Bishops, theologians, and clergy whose careers and positions align with the official hermeneutic of continuity. They receive institutional preferment, academic appointments, and canonical security. They experience the constraint as vindication of their theological stance and protection of tradition.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, continuity_faction_clergy, beneficiary,
    powerful, generational, identity_locked, global).

% Clergy and laity in communities such as the SSPX and allied groups who reject the council's authority because they perceive doctrinal rupture. They bear canonical irregularity, restricted sacramental access, and institutional marginalization. Their separation from the official Church was forced by refusal to accept ambiguous authority as binding.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_dissenters, payer,
    organized, generational, identity_locked, global).

% Theologians, clergy, and lay movements who emphasize rupture-leaning elements in the conciliar texts and advocate structural reform. They face doctrinal investigations, revoked teaching licenses, and exclusion from official platforms when their reading exceeds Roman tolerance. They remain inside the institution hoping the text's ambiguity will eventually authorize their vision.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reformers, payer,
    moderate, biographical, constrained, global).

% Diocesan bishops whose traditional teaching authority has been progressively absorbed by Roman curial oversight of conciliar interpretation. They experience the constraint as a loss of autonomous magisterial voice, required to implement directives whose meaning is contested. Some leverage the ambiguity for local flexibility; others experience it as disabling uncertainty.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_bishops, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_bishops, beneficiary).

% Academic historians who have reconstructed the council's drafting processes, voting records, and theological compromises from archival sources. They document the overdetermined character of the texts but lack magisterial authority; their research corroborates the composite reading from outside the beneficiary set.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_historians, observer,
    analytical, biographical, analytical, global).

% Ordinary Catholic believers largely excluded from hermeneutical debates. They receive the council's effects through liturgy, catechesis, and parish life. Their religious identity is shaped by the Church's claim to coherent authority, yet they have minimal voice in interpreting the council's contested meaning.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_laity, excluded,
    powerless, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional unity of the global Catholic Church across incompatible theological visions by allowing all conciliar factions to find their preferences partially encoded in the final texts, thereby avoiding immediate schism during and after the council.
% TRANSFER_FUNCTION: Transfers interpretive authority from local bishops and theologians to the centralized Roman magisterium, which controls the authorized hermeneutic; transfers theological clarity away from all parties into a managed ambiguity that requires a central arbiter.
% ABSENT_VOICES: The minority who voted non placet (10â12 percent) and whose theological objections were overridden by ambiguous drafting; lay theologians without curial appointment; conciliar historians whose archival work reveals the compositional compromises but who lack magisterial voice.
% DISAPPEARANCE_RATIONALE: If the overdetermined authority structure vanished, the Church would either be forced to reconcile the incompatible visions formallyâproducing clarity and likely schismâor decentralize interpretive authority back to local churches. The current managed ambiguity would unravel, and the institutional function of the Roman Curia as hermeneutical gatekeeper would collapse.
% FOUNDING_PROBLEM: The threat of schism during the council if either continuity or rupture visions prevailed outright; the need to secure supermajority votes on contentious documents by crafting language acceptable to incompatible theological parties.
% FOUNDING_PROBLEM_CORROBORATION: Conciliar periti and historians present at the council attest to the drafting compromises in commission records and floor debates; independent ecclesiastical historians outside the curial beneficiary set corroborate the strategic use of ambiguity. The Roman Curia itself does not corroborate the dead status, asserting instead that the council's unity is substantive rather than tactical.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the substantial transfer of interpretive authority from local churches and theologians to the Roman Curia under cover of an ambiguous text. Suppression (0.56) captures the active enforcement of the continuity hermeneutic against traditionalist and progressive dissent, moderated by the ambiguity that permits some local variation. Theater ratio (0.52) registers the widening gap between the magisterium's performance of textual coherence and the historical record of conciliar compromise. Accessibility collapse (0.48) is moderate because exit options (schism, local resistance, alternative communities) remain structurally available though canonically and socially costly. Resistance (0.52) reflects sustained pushback from organized traditionalists and diffuse progressive networks. The temporal series trace the maturation of the hermeneutical control apparatus from the council's close (1965) to the present.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Roman Curia) experiences the constraint as necessary guardianship of unity through authoritative interpretation; the same structure appears to payer seats (traditionalists, progressives, local bishops) as the suppression of their preferred readings and the concentration of power in a central interpreter. The engine computes this divergence from identical structural data via directionality and scope scaling.
 *
 * DIRECTIONALITY LOGIC:
 *   The Roman Curia is declared beneficiary and agenda-setter: it collects hermeneutical authority and enforcement power, placing it near the full-beneficiary end. Traditionalist dissenters and progressive reformers are declared victims/payers: they bear the costs of suppressed readings, disciplinary sanctions, and irregular status. Local bishops are victims because the constraint extracts their traditional teaching autonomy by subordinating diocesan interpretation to Roman oversight. Continuity faction clergy are beneficiaries because their reading is institutionally subsidized and canonically protected.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents false classification as a pure snare by preserving a genuine coordination function: the ambiguous text prevented immediate schism in 1965 and continues to allow diverse Catholic factions to share institutional space. It prevents false classification as a pure rope by documenting asymmetric extraction: the Roman Curia coordinates the whole but captures disproportionate authority, while identifiable parties (traditionalists, progressives, local bishops) bear concentrated costs. The temporal measurements show extraction and theater intensifying as the original coordination problem (avoiding conciliar schism) receded into history, yet the structure has not decayed into pure piton because the coordination benefit of institutional unity remains partially real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_intentionality,
    'Were the ambiguous formulations in the conciliar texts intentionally designed to secure supermajority votes, or do they reflect genuine unresolved theological disagreement among the Council Fathers that could not be overcome?',
    'Archival analysis of conciliar commission drafting records, periti memoirs, and floor-debate transcripts to determine whether ambiguity was a deliberate political strategy or an emergent byproduct of theological pluralism.',
    'If intentionality is established, the constraint''s extraction is more clearly a designed transfer of authority to a future interpreter; if emergent, the extraction is better characterized as institutional opportunism exploiting accidental ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_intentionality, empirical, 'Whether textual ambiguity was intentional drafting strategy or emergent disagreement').

omega_variable(
    hermeneutical_authority_legitimacy,
    'Does the concentration of interpretive authority in the Roman Curia represent necessary guardianship of ecclesial unity, or exploitative control of an ambiguous text that reserves resolution to a self-interested party?',
    'Comparative analysis of decentralized versus centralized interpretive models in other communions, combined with assessment of whether Curial interpretations show systematic bias toward institutional self-preservation.',
    'Resolution would shift classification between tangled_rope (genuine coordination with asymmetric extraction) and snare (the coordination story is cover for centralized power accumulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_authority_legitimacy, conceptual, 'Whether centralized hermeneutical authority is guardianship or extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the enforcement of the continuity hermeneutic a doctrinal necessity for Catholic identity, or a structural suppression of the text''s rupture elements that maintains the beneficiary structure?',
    'Pattern analysis of disciplinary cases: if sanctions fall disproportionately on those who expose the textual compromises rather than on those who merely dissent theologically, suppression is structural maintenance of the overdetermination.',
    'If structural maintenance, the effective suppression is higher than the doctrinal-necessity framing suggests, amplifying extraction for the Curia and continuity faction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Whether enforcement is doctrinal necessity or structural maintenance of ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is the composite_overdetermination reading of the vatican_ii_magisterial_authority kernel. It decomposes the colloquial label 'Vatican II magisterial authority' into three structurally distinct claims: continuity (the council as coherent organic development), rupture (the council as revolutionary break), and composite (the council as intentionally overdetermined compromise). The epsilon values differ because the referents differ: continuity treats the text as semantically coherent, rupture treats it as encoding a new ecclesiology, and composite treats it as encoding incompatible ecclesiologies simultaneously. Each story links to its siblings in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
