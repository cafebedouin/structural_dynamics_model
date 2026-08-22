% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Historical Treaties Read as Nation-to-Nation International Agreements Between Sovereign Equals
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel: the historical
 *   treaty substrate between settler states and Indigenous nations, read as
 *   agreements between co-equal sovereigns subject to modern treaty law
 *   principles requiring ongoing consent. Under this reading, the original
 *   bargain was never extinguished by signature but persists as a live
 *   international-law-adjacent obligation, meaning unilateral resource
 *   permitting or territorial administration by the settler state constitutes
 *   an ongoing treaty violation rather than an exercise of settled domestic
 *   sovereignty. This is a Tangled Rope: it genuinely coordinates a durable
 *   framework for negotiating shared territorial governance (the coordination
 *   function), while simultaneously extracting litigation cost, delay, and
 *   unresolved risk from Indigenous nations who must continuously reassert
 *   the doctrine against an enforcement apparatus that resists full
 *   recognition, and from resource industry and fiscal authorities who bear
 *   the cost of unresolved consent claims. Two sibling readings of the same
 *   historical instruments — the extinguishment reading (treaties as
 *   completed property transactions) and the stewardship reading (treaties as
 *   relational pacts with no sovereignty cession at all) — are NOT part of
 *   this constraint; they are separate stories with their own epsilon values,
 *   linked here only through the kernel network.
 *
 * KEY AGENTS:
 *   - indigenous_treaty_nations: co-equal sovereign under this reading, bears cost of ongoing assertion
 *   - settler_state_executive_and_legislature: agenda-setter administering recognition machinery
 *   - resource_extraction_industry: mobile capital bearing transaction-cost risk from contested consent
 *   - settler_state_fiscal_authorities: institutional payer of settlement and compensation risk
 *   - international_law_advocacy_bodies: analytical beneficiary collecting doctrinal capital
 *   - domestic_courts: observer seat adjudicating which reading prevails case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.72).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.68).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaties Read as Nation-to-Nation International Agreements Between Sovereign Equals").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '32e8e86d-29ae-4afc-be5d-f6c6cc784170').
narrative_ontology:cs_kernel_codification('32e8e86d-29ae-4afc-be5d-f6c6cc784170', distributed).
narrative_ontology:cs_authority_grounding('32e8e86d-29ae-4afc-be5d-f6c6cc784170', distributed).
narrative_ontology:cs_reading_relation('32e8e86d-29ae-4afc-be5d-f6c6cc784170', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('32e8e86d-29ae-4afc-be5d-f6c6cc784170', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('32e8e86d-29ae-4afc-be5d-f6c6cc784170', foundational, treaty_consent_is_ongoing_not_terminal).
narrative_ontology:cs_axiom_status(treaty_consent_is_ongoing_not_terminal, holdable).
narrative_ontology:cs_axiom_grounding('32e8e86d-29ae-4afc-be5d-f6c6cc784170', treaty_consent_is_ongoing_not_terminal, conventional).
narrative_ontology:cs_axiom('32e8e86d-29ae-4afc-be5d-f6c6cc784170', foundational, indigenous_nations_retain_sovereign_standing).
narrative_ontology:cs_axiom_status(indigenous_nations_retain_sovereign_standing, holdable).
narrative_ontology:cs_axiom_grounding('32e8e86d-29ae-4afc-be5d-f6c6cc784170', indigenous_nations_retain_sovereign_standing, deontological).
narrative_ontology:cs_reference_frame('32e8e86d-29ae-4afc-be5d-f6c6cc784170', sovereign_equals_negotiating_protocol).
narrative_ontology:cs_drift_state('32e8e86d-29ae-4afc-be5d-f6c6cc784170', post_1970s_indigenous_rights_jurisprudence, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('32e8e86d-29ae-4afc-be5d-f6c6cc784170', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, international_law_advocacy_bodies).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industry).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_fiscal_authorities).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, consent_based_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, ongoing_treaty_obligation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold original treaty-making authority under this reading and are entitled to ongoing consent rights over territorial and resource decisions on treaty lands. They benefit from the doctrine when it is enforced by courts or negotiated recognition, but bear enormous cost and delay litigating for that recognition against a settler state that resists full nation-to-nation implementation; their sovereignty claim is real but its practical realization remains perpetually contested and under-resourced.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, payer).

% Administers the machinery that decides how much of the nation-to-nation reading to formally recognize — treaty rights litigation frameworks, consultation duties, comprehensive claims processes. Retains ultimate legislative supremacy and can narrow or expand recognition through policy, making it both the enforcer of whichever reading prevails in a given era and the party most constrained if the nation-to-nation reading is judicially entrenched.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_executive_and_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Operates mines, pipelines, and forestry concessions on treaty territory under permits issued by the settler state. Under the nation-to-nation reading, prior unilateral permitting becomes a treaty violation requiring renegotiated consent, which raises transaction costs, project timelines, and the risk of injunction. Firms can relocate capital to other jurisdictions more easily than Indigenous nations can relocate their territorial claim, so their exit option is comparatively strong.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industry, payer,
    powerful, biographical, mobile, national).

% Bear the budgetary consequence of the nation-to-nation reading: compensation settlements, revenue-sharing agreements, and the fiscal risk of stalled resource royalties when consent is withheld or contested. Cannot exit the obligation once courts recognize it as live international-law-adjacent doctrine; can only manage its cost through negotiation or delay.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_fiscal_authorities, payer,
    institutional, generational, constrained, national).

% NGOs, UN rapporteurs, and academic international-law communities that cite the nation-to-nation reading as evidence that domestic treaty law is converging with international sovereign-equality norms. They collect reputational and doctrinal capital from the reading's advancement without bearing the settlement costs or litigation risk themselves.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_law_advocacy_bodies, beneficiary,
    organized, civilizational, analytical, global).

% Private landholders and municipalities whose property or infrastructure sits on treaty territory are rarely party to treaty renegotiation processes even though the nation-to-nation reading's practical implementation (land return, co-management, revenue sharing) can directly affect their tenure and local governance. They are not consulted as a class distinct from the settler state itself.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, third_party_settler_landholders, excluded,
    moderate, biographical, trapped, regional).

% Adjudicate which reading of the historical treaty substrate governs in specific disputes, drawing on evolving doctrines of the honour of the crown, fiduciary duty, and international comparative law. Their rulings determine, case by case, how much of the nation-to-nation reading becomes binding domestic law rather than aspirational doctrine.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, domestic_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reading the historical treaties as agreements between sovereign equals solves the problem of how to govern a shared territory whose original bargain was never a simple property cession: it provides a framework for ongoing negotiation over resource use, land management, and jurisdiction that can adapt as circumstances change, rather than freezing the relationship at the moment of signing.
% TRANSFER_FUNCTION: Where enforced, the reading moves consent authority, veto power over resource projects, and negotiated compensation from the settler state and its licensees toward the Indigenous treaty nations; where under-enforced, it moves litigation cost and delay onto the Indigenous nations who must repeatedly assert the doctrine against institutional resistance.
% ABSENT_VOICES: Third-party settler landholders and municipalities are structurally outside the nation-to-nation negotiating table even though implementation directly touches their tenure; proponents of the stewardship reading argue the nation-to-nation frame still imports a Westphalian sovereignty concept foreign to the original relational intent, but that critique rarely reaches courts applying international-law analogies.
% DISAPPEARANCE_RATIONALE: If the nation-to-nation reading were abandoned entirely in favor of the extinguishment reading, decades of litigation strategy, consultation frameworks, comprehensive claims processes, and international advocacy built on sovereign-equals doctrine would collapse; resource permitting would revert to unilateral settler-state authority and consent requirements would lose their doctrinal foundation.
% FOUNDING_PROBLEM: The original treaties were negotiated, in many documented instances, using nation-to-nation protocol, wampum and pipe ceremonies, and reciprocal diplomatic language — the founding problem this reading addresses is the mismatch between that historical negotiating posture and a domestic legal system that later treated the same instruments as simple land cessions.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous nations and international law scholars attest the sovereign-equals framing from oral history, treaty commissioner records, and comparative diplomatic protocol analysis; settler state legal historians and some domestic court decisions dispute that the historical negotiating posture entailed permanent international-law-grade sovereignty, arguing instead for domestic constitutional characterization — corroboration exists on both sides from outside the immediate litigating parties, which is precisely why the reading remains contested rather than settled.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.72 by interval end because, even where the nation-to-none reading is judicially recognized, its practical implementation remains slow, underfunded, and repeatedly re-litigated — the doctrine's promise of consent authority is real but its delivery is extracted through decades of Indigenous-nation-funded litigation. Suppression starts high (0.80) reflecting the historical near-total judicial and legislative refusal to entertain the sovereign-equals framing, and declines modestly (to 0.68) as international law norms and comparative jurisprudence have made the reading harder to dismiss outright, though far from fully enforced. Theater ratio starts high (0.65) — early recognition of the reading was largely symbolic (apologies, commissions, non-binding declarations) — and falls as some jurisdictions have moved toward binding consultation and consent frameworks, though substantial performative recognition persists alongside genuine change.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty nations occupy the paradoxical dual seat characteristic of a Tangled Rope: they are the reading's intended beneficiary (the doctrine exists to recognize their sovereignty) and simultaneously its primary payer (the cost of asserting, litigating, and enforcing that recognition falls almost entirely on them, against a settler state with vastly greater institutional resources). Resource extraction industry and settler fiscal authorities are payers with different exit profiles: industry can relocate capital, fiscal authorities cannot escape the obligation once courts entrench it. International law advocacy bodies benefit analytically and reputationally without bearing implementation cost, which is why they are listed as beneficiary despite having no resource stake in the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling the historical nation-to-nation negotiating protocol with a domestic legal system that treated the same instruments as land cessions — remains genuinely live rather than resolved-but-persisting: courts, legislatures, and Indigenous nations are still actively contesting which reading governs, so this is not a case of an obsolete mandate propped up by inertia. The classification distinguishes this from pure extraction (a snare) precisely because the coordination function is real and contested in good faith by multiple institutional actors, not merely a cover story; it also distinguishes it from a clean rope because the extraction from Indigenous nations bearing the enforcement burden is asymmetric and requires active judicial and political enforcement to sustain even its partial recognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_equals_versus_domestic_constitutional_status,
    'Do the historical treaties genuinely instantiate international-law-grade sovereign-to-sovereign agreements, or is the nation-to-nation frame a modern doctrinal overlay imported from comparative international law onto instruments that domestic legal systems have long treated as constitutional or property matters?',
    'Comparative analysis of the original negotiating record (wampum protocols, treaty commissioner minutes, oral history) against the doctrinal moves made by courts and advocates who first applied international sovereign-equality frameworks to these instruments; tracking whether the framing predates or postdates the doctrine''s use in litigation strategy.',
    'If the sovereign-equals framing is substantially supported by the original negotiating record, this reading''s claim to authenticity strengthens and its extraction of ongoing litigation cost from Indigenous nations looks more like the cost of enforcing a genuine original bargain. If it is primarily a modern doctrinal construction, the reading''s coordination function weakens relative to the stewardship reading''s claim to closer fidelity with the original relational intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_equals_versus_domestic_constitutional_status, conceptual, 'Whether nation-to-nation sovereignty is the authentic original frame or a later doctrinal import.').

omega_variable(
    committer_structure_which_reading_prevails,
    'Which of the three declared readings of the historical treaty substrate (extinguishment, nation-to-nation, stewardship) will settler-state courts and legislatures ultimately entrench as binding domestic law, and does that entrenchment foreclose the others within that jurisdiction''s framework?',
    'Track domestic case law and constitutional amendment activity over multiple decades; a jurisdiction''s supreme court decisively adopting one reading as the binding constitutional characterization would resolve which reading is live there, though other jurisdictions or the international law community could retain a different reading.',
    'If a domestic court entrenches the extinguishment reading, the nation-to-nation reading''s coordination function is foreclosed in that jurisdiction even though it remains live in international advocacy discourse; if it entrenches the nation-to-nation reading, extractiveness may shift because the settler state institutionally can no longer treat unilateral resource extraction as lawful.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_which_reading_prevails, empirical, 'Which sibling reading of the kernel will be judicially entrenched, and where the disagreement is structurally located (constitutional characterization of the original treaty instrument).').

omega_variable(
    beneficiary_or_victim_dual_seat_stability,
    'Is the Indigenous treaty nations'' dual position as both beneficiary and payer a stable long-term structural feature of this reading, or does it represent a transitional phase that resolves toward one or the other as enforcement matures?',
    'Longitudinal tracking of litigation cost burden versus realized consent-authority outcomes across multiple treaty nations and jurisdictions over a 30+ year window.',
    'If the payer burden persists indefinitely even as recognition nominally increases, this looks structurally more like a snare wearing coordination language; if litigation cost declines as recognition becomes self-executing, the tangled rope classification is a genuine transitional description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_or_victim_dual_seat_stability, empirical, 'Whether the dual beneficiary/payer seat is a stable structural feature or a transitional artifact of incomplete enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(hist_tr_t10, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 50, 0.43).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hist_be_t10, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(hist_su_t10, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(hist_su_t20, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(hist_su_t60, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the historical_treaty_substrate kernel, each authored as a separate story with its own epsilon, beneficiary/victim structure, and classification per the ε-invariance principle. The extinguishment_reading treats the same historical instruments as completed property transactions (low ongoing extraction from the settler state's perspective, high extraction from the Indigenous perspective if the cession is contested as coerced); the stewardship_reading treats them as relational pacts with no sovereignty cession at all (a different coordination structure entirely, with no consent-veto mechanism over territorial change). The nation_to_nation_reading modeled here sits between them: it grants Indigenous nations formal sovereign standing and consent rights but imports an international-law framework that some stewardship-reading proponents consider itself a foreign imposition. All three are linked via affects_constraints because entrenchment of any one reading in a given jurisdiction structurally forecloses or pressures the others there.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
