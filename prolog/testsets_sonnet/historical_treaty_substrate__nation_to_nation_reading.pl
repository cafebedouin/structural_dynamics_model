% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Treaties Read as Nation-to-Nation International Agreements Requiring Ongoing Consent
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the nation-to-nation reading of the historical
 *   treaty substrate kernel: the claim that historical treaties between
 *   Indigenous nations and settler states were, and remain, agreements
 *   between sovereign equals subject to ongoing consent and modern treaty law
 *   principles, rather than completed property transactions (the
 *   extinguishment reading) or non-sovereignty-ceding relational pacts (the
 *   stewardship reading). Under this reading specifically, Indigenous nations
 *   enter the beneficiary structure as co-equal sovereigns holding consent
 *   rights over territorial and resource changes, and the settler state
 *   becomes bound by ongoing international-law-style treaty obligations
 *   rather than holding unilateral post-cession authority. Historically,
 *   courts and legislatures largely operated as though the extinguishment
 *   reading controlled, while diplomatic and advocacy uses of the treaties
 *   increasingly invoke the nation-to-nation frame — producing declining but
 *   still substantial theater ratio and suppression requirement over the
 *   interval as the reading gains partial doctrinal traction without full
 *   enforcement. ε does not change across this decomposition; only this
 *   reading's own structural profile is authored here. The sibling readings
 *   (extinguishment_reading, stewardship_reading) are separate constraint
 *   files with their own ε, beneficiary/victim sets, and classifications,
 *   linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - indigenous_treaty_nations: co-equal sovereign beneficiary/payer (organized/constrained) — holds consent rights, bears enforcement cost
 *   - settler_state: agenda_setter (institutional/constrained) — administers systems this reading would subordinate to consent
 *   - domestic_courts: agenda_setter/observer (institutional/analytical) — determines which reading has legal force in practice
 *   - resource_extraction_dependent_settler_industries: payer (powerful/constrained) — bears exposure from consent requirement
 *   - international_treaty_law_bodies: observer (institutional/analytical) — assesses formal treaty-law criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.58).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.71).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaties Read as Nation-to-Nation International Agreements Requiring Ongoing Consent").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '11dbb72d-3545-4839-9fae-fd6e6fbe60ac').
narrative_ontology:cs_kernel_codification('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', distributed).
narrative_ontology:cs_authority_grounding('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', distributed).
narrative_ontology:cs_reading_relation('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', foundational, indigenous_nations_retained_international_legal_personality).
narrative_ontology:cs_axiom_status(indigenous_nations_retained_international_legal_personality, holdable).
narrative_ontology:cs_axiom_grounding('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', indigenous_nations_retained_international_legal_personality, empirically_contingent).
narrative_ontology:cs_axiom('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', foundational, territorial_and_jurisdictional_change_requires_ongoing_consent).
narrative_ontology:cs_axiom_status(territorial_and_jurisdictional_change_requires_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', territorial_and_jurisdictional_change_requires_ongoing_consent, conventional).
narrative_ontology:cs_reference_frame('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', sovereign_equality_diplomatic_protocol).
narrative_ontology:cs_drift_state('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', post_undrip_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('11dbb72d-3545-4839-9fae-fd6e6fbe60ac', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_legitimacy_claim).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_dependent_settler_industries).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, sovereign_equality_of_treaty_parties).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, consent_based_international_legal_personality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the co-equal sovereign position under this reading: territorial changes, resource extraction, and jurisdictional assertions by the settler state require their ongoing consent, not a one-time historical cession. They benefit structurally from the reading's recognition of continuing sovereignty, but they pay the cost of litigating and defending that recognition against a settler state and courts that frequently do not operate as if it were true — the reading names an entitlement whose enforcement is contested in every instance.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, payer).

% Administers courts, land title systems, and resource permitting that this reading would subordinate to ongoing consent requirements. Retains practical control over enforcement machinery and can choose how far to implement the reading's implications; bears reputational and legal exposure from international and domestic litigation when it does not.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state, agenda_setter,
    institutional, generational, constrained, national).

% The settler state's own claim to lawful founding benefits from a reading that frames the historical treaties as valid international instruments between sovereigns, rather than as instruments of unilateral conquest — even though the same reading constrains present-day state action. This is a non-agent legitimacy interest, not an actor with independent standing.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_legitimacy_claim, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__nation_to_nation_reading, settler_state_legitimacy_claim).

% Hold permits, leases, and capital investments premised on settled title and unilateral state jurisdiction over treaty territory. Under this reading, extraction projects without renewed Indigenous consent become treaty violations, exposing sunk investment to injunction, revocation, or renegotiated terms.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_dependent_settler_industries, payer,
    powerful, biographical, constrained, national).

% Comparative international law scholarship and tribunals assess whether the historical instruments meet the formal criteria of treaties between sovereigns under the Vienna Convention framework and customary international law, informing but not binding domestic courts on this reading's applicability.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_treaty_law_bodies, observer,
    institutional, civilizational, analytical, global).

% Adjudicate which reading of the treaty substrate governs a given dispute. Their doctrinal choices determine in practice whether the nation-to-nation reading has legal force or remains an aspirational academic and diplomatic claim.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, domestic_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, domestic_courts, observer).

% Not party to the treaties and rarely consulted on which reading governs, yet affected through land use, taxation, and resource pricing consequences of whichever reading prevails. Their preferences are treated as downstream of the legal question, not a factor in resolving it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, non_treaty_settler_public, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal-diplomatic framework in which the settler state and Indigenous nations can coordinate ongoing territorial and jurisdictional decisions through negotiated consent rather than unilateral assertion, using the formal apparatus of international treaty law (mutual obligation, ongoing consent, remedies for breach).
% TRANSFER_FUNCTION: Under this reading, decision-making authority and veto power over territorial and resource changes flow toward Indigenous nations and away from unilateral settler-state and industry action; conversely, legitimacy and a lawful-founding narrative flow toward the settler state that adopts the reading.
% ABSENT_VOICES: Non-treaty settler publics and downstream resource-dependent communities are not parties to the interpretive contest and are rarely heard in the courts or diplomatic fora where the reading is adjudicated, despite bearing consequences of the outcome.
% DISAPPEARANCE_RATIONALE: If the nation-to-nation reading were abandoned entirely in favor of a purely extinguishment framing, Indigenous nations would lose the primary doctrinal basis for consent-based challenges to resource extraction and jurisdictional assertions currently being litigated; conversely, if it were universally adopted and enforced, settler-state resource permitting and land title systems would require wholesale renegotiation with treaty nations as co-sovereigns.
% FOUNDING_PROBLEM: The historical treaties were negotiated, in substantial part, using the diplomatic forms and vocabulary of agreements between sovereign nations — flags, gift exchange, oratory recognizing mutual nationhood, and negotiated terms — creating an unresolved question about what legal status those instruments actually carry today.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars outside both the settler state and the treaty nations' own advocacy bodies have examined the original instruments against Vienna Convention criteria and customary international law standards, and disagree among themselves about whether Indigenous nations at the time possessed the requisite international legal personality — this is independent corroboration that the status is genuinely contested, not merely asserted by either side of the current dispute.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored as moderate-high and declining: under this reading, the settler state's historical unilateral assertions of jurisdiction constitute an ongoing extraction from Indigenous nations' sovereign prerogatives, but the reading's growing doctrinal traction (through international law scholarship, some domestic court decisions, and diplomatic practice) is gradually constraining that extraction. Suppression (0.71) remains high because domestic courts, land title systems, and resource permitting regimes still substantially operate on non-consent-based assumptions, actively suppressing full recognition of this reading's implications. Theater ratio (0.44) reflects genuine but partial performative adoption — governments increasingly use nation-to-nation language in ceremony and diplomacy while enforcement mechanisms lag behind the rhetoric. Accessibility collapse is moderate (0.40): the reading has not fully displaced competing readings, and litigation, diplomacy, and legislative reform remain live alternative paths. Resistance is high (0.78): both settler-state institutions defending existing title systems and, at times, factions within treaty nations skeptical of international-law framing (preferring stewardship or self-determination-based accounts) actively contest this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the settler state's own institutional seat, adopting the nation-to-nation reading functions as a Tangled Rope: it coordinates a more legitimate, internationally defensible founding narrative while simultaneously constraining unilateral action — a real cost the state bears for a real legitimacy benefit. From the seat of resource-extraction-dependent industries, the same reading looks like Snare-adjacent extraction of their sunk investment through retroactively imposed consent requirements. From the treaty nations' own seat, the reading is closer to a Rope struggling against active suppression — the coordination function (formal consent-based mutual obligation) is genuine, but its enforcement is systematically resisted by the party that would otherwise be bound. The engine's per-seat computation should reflect this divergence rather than collapsing it into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty nations are declared both beneficiary (of the sovereign-equality recognition) and payer (of the litigation and diplomatic cost required to make that recognition operative) — this dual role is intentional and drives a directionality nearer the target end than the beneficiary label alone would suggest, because the benefit is largely unrealized without continuous, costly assertion. The settler state is agenda_setter with constrained exit: it authored and administers the systems the reading would bind, and cannot exit the reading's implications without abandoning the legitimacy claim the reading also supplies it. Resource industries are pure payers with concentrated, powerful but constrained position: sunk capital cannot easily relocate. The settler_state_legitimacy_claim is marked as a non-agent beneficiary (agent: false) because it is a legitimacy interest, not an actor capable of collecting rents itself — it is included for completeness per the schema's non-agent guidance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine ambiguity in the diplomatic form of the original negotiations — is contested rather than dead: the corroborating international law scholarship confirms the ambiguity is real, not manufactured by either side's advocacy. This prevents the reading from being dismissed as mere retrospective reinterpretation (mandatrophy would require the founding problem to be dead while the arrangement persists for other reasons); here the interpretive question itself remains live, which is why disappearance_verdict is world_rearranges rather than world_unchanged — courts, industries, and treaty nations would all have to reorganize around whichever reading prevailed if this one vanished from the interpretive field.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_status_at_signing,
    'Did Indigenous nations possess the international legal personality required for the historical instruments to qualify as treaties between sovereign equals under contemporaneous or modern international law standards?',
    'Comparative historical-legal analysis of the negotiating record (protocols, gift exchange, mutual recognition language) against Vienna Convention and customary international law criteria for sovereign treaty-making capacity, cross-checked against how comparable historical agreements between colonial powers and non-European polities have been classified.',
    'If sovereign status is affirmed, the nation-to-nation reading gains strong doctrinal footing and the settler state''s ongoing consent obligations become more legally enforceable, sharply reducing suppression over time. If denied, the reading remains primarily diplomatic and aspirational, and the extinguishment_reading retains greater practical force despite this story''s authored metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_status_at_signing, empirical, 'Whether Indigenous nations held the sovereign capacity this reading presupposes.').

omega_variable(
    reading_selection_mechanism,
    'Which institutional actor''s choice of reading actually controls outcomes in a given dispute — legislative reform, judicial doctrine, international arbitration, or direct negotiation — and does that selection mechanism itself favor one reading systematically?',
    'Track a sample of resource-extraction and jurisdictional disputes across multiple forums (domestic courts, international tribunals, negotiated settlements) and code which reading''s premises the deciding body implicitly or explicitly adopted.',
    'If domestic courts systematically default to extinguishment-reading premises regardless of international law scholarship, the nation-to-nation reading''s beneficiary status for Indigenous nations is largely symbolic until legislative or constitutional reform changes the selection mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_mechanism, empirical, 'Which forum''s default premises actually govern outcomes.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the choice among nation_to_nation, extinguishment, and stewardship readings itself underdetermined by the historical record, such that no single reading can claim to be the ''correct'' recovery of original intent — and if so, does that underdetermination favor the settler state (which can select whichever reading is most convenient in a given dispute) by default?',
    'Systematic review of the full negotiating record across multiple treaty instruments and regions to assess whether the record supports one reading more consistently than the others, versus supporting different readings in different specific treaties.',
    'If genuinely underdetermined across the board, the kernel''s three readings should be understood as competing normative-legal claims rather than competing historical-factual claims, which would affect how courts should treat interpretive deference and which party should bear the burden of interpretive uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three-reading contest reflects genuine historical ambiguity or motivated selection among available framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(hist_tr_t8, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement(hist_tr_t16, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 16, 0.53).
narrative_ontology:measurement(hist_tr_t24, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(hist_tr_t32, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(hist_be_t8, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(hist_be_t16, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(hist_be_t24, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(hist_be_t32, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(hist_su_t8, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 8, 0.81).
narrative_ontology:measurement(hist_su_t16, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(hist_su_t24, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(hist_su_t32, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, stewardship_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the historical_treaty_substrate kernel. extinguishment_reading treats the same historical instruments as completed property transactions with much lower ongoing settler-state obligation and much higher accessibility_collapse (alternatives to unilateral state authority are treated as closed). stewardship_reading treats the instruments as non-sovereignty-ceding relational pacts, producing a different beneficiary structure emphasizing mutual coexistence duties rather than sovereign consent rights. All three share the same underlying historical instruments but diverge in claimed_type, ε, and stakeholder structure because they instantiate structurally distinct legal claims about what the instruments are. Each carries its own ε; none should be treated as an observable-selection variant of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
