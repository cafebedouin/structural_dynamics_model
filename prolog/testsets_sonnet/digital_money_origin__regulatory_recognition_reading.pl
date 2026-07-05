% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Digital Money's Origin as Regulatory Recognition (M-aggregate incorporation)
 *   domain: monetary_history/institutional_economics/regulation
 *
 * SUMMARY:
 *   This story instantiates the regulatory_recognition_reading of the
 *   digital_money_origin kernel: digital money is treated as having emerged
 *   only when central banks and prudential regulators formally swept digital
 *   balances into statistical aggregates (M1/M2/M3) and licensing perimeters.
 *   This is deliberately the LATEST-dated reading in the kernel — years or
 *   decades after functional digital value transfer was already practiced by
 *   fintech innovators, stablecoin issuers, and mobile-money operators. The
 *   constraint set here is dominated by legal and regulatory barriers
 *   (licensing, capital/reserve requirements, statistical classification
 *   criteria) rather than by technical conceivability or first practical use,
 *   which are the subjects of the sibling readings (became_thinkable_reading,
 *   first_held_reading). Those siblings are NOT part of this constraint; they
 *   are separate stories linked via network.affects_constraints. Extraction
 *   here concentrates on incumbent-favoring effects: institutions already
 *   inside the regulatory perimeter gain a state-conferred legitimacy
 *   premium, while the actors whose practical innovation preceded formal
 *   recognition bear compliance costs, retroactive registration burdens, or
 *   exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.61).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.58).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money's Origin as Regulatory Recognition (M-aggregate incorporation)").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/institutional_economics/regulation").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '81e1f834-9552-4fe4-b34f-f10516b1c499').
narrative_ontology:cs_kernel_codification('81e1f834-9552-4fe4-b34f-f10516b1c499', formalized).
narrative_ontology:cs_authority_grounding('81e1f834-9552-4fe4-b34f-f10516b1c499', extraction).
narrative_ontology:cs_interpretation_layer_present('81e1f834-9552-4fe4-b34f-f10516b1c499').
narrative_ontology:cs_reading_relation('81e1f834-9552-4fe4-b34f-f10516b1c499', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('81e1f834-9552-4fe4-b34f-f10516b1c499', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('81e1f834-9552-4fe4-b34f-f10516b1c499', foundational, state_classification_constitutes_monetary_existence).
narrative_ontology:cs_axiom_status(state_classification_constitutes_monetary_existence, holdable).
narrative_ontology:cs_axiom_grounding('81e1f834-9552-4fe4-b34f-f10516b1c499', state_classification_constitutes_monetary_existence, conventional).
narrative_ontology:cs_axiom('81e1f834-9552-4fe4-b34f-f10516b1c499', secondary, prudential_perimeter_precedes_monetary_legitimacy).
narrative_ontology:cs_axiom_status(prudential_perimeter_precedes_monetary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('81e1f834-9552-4fe4-b34f-f10516b1c499', prudential_perimeter_precedes_monetary_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('81e1f834-9552-4fe4-b34f-f10516b1c499', central_bank_statistical_sovereignty_framework).
narrative_ontology:cs_drift_state('81e1f834-9552-4fe4-b34f-f10516b1c499', post_stablecoin_and_cbdc_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('81e1f834-9552-4fe4-b34f-f10516b1c499', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_deposit_taking_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, central_bank_statistical_authorities).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, licensed_payment_processors).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_fintech_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, early_stablecoin_issuers).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, informal_mobile_money_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, licensed_payment_processors).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, regulatory_perimeter_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which instruments count as 'money' in M1/M2/M3 aggregates and which regulatory perimeter (banking license, e-money license, payment institution status) an instrument must sit inside before it is recognized. This decision is what the reading treats as the actual moment of emergence: the instrument existed technically before, but became 'digital money' only when swept into the statistical and legal apparatus. Controls the definitional gate itself.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, central_bank_statistical_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Already hold banking licenses and deposit insurance backstops, so formal recognition of digital balances as money simply confirms and extends their existing balance sheets into the digital ledger. They lobby for narrow licensing categories that keep new entrants underneath them in the settlement hierarchy, and benefit from the compliance costs the new perimeter imposes on competitors who lack in-house regulatory affairs departments.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_deposit_taking_banks, beneficiary,
    institutional, generational, arbitrage, national).

% Obtained e-money or payment institution licenses and now operate inside the recognized perimeter, gaining legitimacy and access to settlement rails denied to unlicensed rivals. They also pay ongoing compliance, capital-reserve, and audit costs to maintain that status, which the reading counts as the price of admission rather than as coordination overhead.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, licensed_payment_processors, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, licensed_payment_processors, payer).

% Built working digital-value transfer systems before any license existed for them, using the technology in practice for years. Under the regulatory-recognition reading, none of that prior use counts as the emergence of digital money — only formal incorporation does. They now face a binary choice: submit to licensing (accepting the cost and possible product redesign) or continue operating in a gray zone treated as illegitimate by the same statistics that will later claim the field began the day the state noticed it.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_fintech_innovators, payer,
    moderate, biographical, constrained, national).

% Issued dollar- or basket-pegged digital tokens that circulated as functional money among users well before any monetary authority classified them. Because the classification act itself is the origin event under this reading, their years of prior circulation are recast as a precursor phase rather than the emergence of digital money, and they now face retroactive registration demands, reserve requirements, or prohibition depending on jurisdiction.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, early_stablecoin_issuers, payer,
    moderate, biographical, trapped, global).

% Ran phone-credit-based value transfer networks in underbanked regions that functioned as de facto currency for millions before any central bank recognized or regulated them. Under this reading their systems are pre-monetary until a regulator says otherwise, which historically has meant either forced formalization under bank-partnership models that capture most of the margin, or shutdown for operating an unlicensed money-transmission business.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, informal_mobile_money_operators, payer,
    powerless, biographical, trapped, regional).

% Study when digital money 'really' began and note that the regulatory-recognition date is systematically the latest of any candidate origin story, often trailing practical adoption by a decade or more. They document the gap between functional use and official recognition as a research finding, without power to change which date monetary authorities publish.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formal incorporation into statistical aggregates and licensing regimes lets monetary authorities monitor money supply, enforce reserve and capital requirements, and give users and counterparties a legible signal of which digital balances carry sovereign or deposit-insurance-backed guarantees versus which do not.
% TRANSFER_FUNCTION: Moves legitimacy, access to settlement rails, and reduced counterparty risk toward institutions that clear the licensing bar, while moving compliance cost, legal exposure, and market exclusion onto instruments and operators that predate or fall outside the recognized perimeter.
% ABSENT_VOICES: The unregulated innovators and informal operators whose systems the regulatory act is defined against are rarely consulted in setting the recognition criteria; central bank statistics committees and prudential regulators set the perimeter largely among incumbents and their own supervisory concerns.
% DISAPPEARANCE_RATIONALE: If formal regulatory recognition vanished, incumbent banks would lose their statistically privileged distinction from other digital-value systems and unregulated instruments would circulate on equal statistical footing with regulated deposits — a real rearrangement for market structure and monetary policy transmission. But the underlying practice of transacting in digital balances would continue unchanged, which is exactly why the sibling readings (became_thinkable, first_held) locate the true origin earlier and treat this reading's 'origin' as a bureaucratic event rather than a monetary one.
% FOUNDING_PROBLEM: Monetary authorities needed a way to measure the money supply accurately and to draw a supervisory perimeter around instruments carrying deposit-insurance or lender-of-last-resort implications, once digital balances became large enough to matter for monetary policy and financial stability.
% FOUNDING_PROBLEM_CORROBORATION: Central bank research departments and prudential supervisors attest the measurement and stability problem remains live and cite it as the reason for ongoing perimeter expansion (e.g., stablecoin and CBDC consultations). Independent monetary historians and fintech-sector economists, outside the regulatory and incumbent-bank beneficiary set, corroborate that the problem is real but note the recognition date is chosen well after the practical problem of digital value transfer was already solved by the excluded actors — supporting the reading's own admission that its origin marker is administrative rather than technical or behavioral.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) is moderate-high and rising over the interval because the recognition perimeter has hardened progressively — early digital-value systems experienced relatively loose oversight, but M-aggregate incorporation criteria and licensing thresholds tightened over successive regulatory cycles, systematically favoring institutions with existing compliance infrastructure. Suppression (0.58) reflects the real legal force behind exclusion: operating outside the recognized perimeter risks enforcement action (unlicensed money transmission charges, cease-and-desist orders), not merely reputational disadvantage. Theater ratio (0.42) is meaningfully above zero because a portion of the 'incorporation' activity — press releases, task forces, statistical methodology committees — functions as legitimation performance for decisions substantially driven by incumbent lobbying rather than by the stated goal of accurate money-supply measurement. Accessibility collapse (0.48) is mid-range: once inside the recognized perimeter, alternatives for reaching regulatory legitimacy do narrow, but unlicensed operation persists as a real (if costly) alternative pathway in many jurisdictions, which is lower than a mountain's near-total collapse.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank authority's seat, formal incorporation is the coordination act that makes monetary policy and financial stability oversight possible — a genuinely necessary function. From the unlicensed innovator's seat, the same act is a retroactive disqualification of years of prior functional use, arriving with compliance costs timed to benefit whoever was already inside the perimeter. The engine should compute these as different types from the same structural data; this divergence is the point of the tangled_rope classification rather than an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistical authorities are the agenda-setters who control the classification gate itself — they decide what counts, and that decision is the origin event this reading names. Incumbent banks and licensed processors are structural beneficiaries: their existing balance sheets and compliance infrastructure position them to absorb formal recognition costlessly or even profitably, since new entrants must build the same infrastructure from scratch. Unregulated fintech innovators, early stablecoin issuers, and informal mobile money operators are targets: their prior practical innovation is definitionally excluded from 'emergence' under this reading, and they bear the downstream costs of retroactive formalization or exclusion. This is precisely the asymmetry the tangled_rope classification requires — genuine coordination function (accurate money-supply measurement, financial stability monitoring) coexists with asymmetric extraction (incumbents captured the definitional apparatus that determines whose activity 'counts').
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accurate measurement and prudential oversight of the money supply) remains live — this is not a pure zombie mandate. But the mismatch worth flagging is that the reading's own origin marker is administratively convenient for incumbents rather than monetarily necessary: the statistical/regulatory incorporation date could in principle track functional adoption much more closely than it has historically, and the systematic lag is itself a symptom of incumbent-favoring capture of the classification process, not an inherent feature of the coordination problem. Treating regulatory recognition AS the origin (rather than merely as a later administrative response to an earlier origin) is the move this constraint family exists to make legible — conflating 'when the state noticed' with 'when the thing began' is the same rhetorical move that lets incumbents claim credit for having 'created' functional patterns that outsiders built first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_date_selection_ambiguity,
    'Is the regulatory-recognition date a defensible marker of when digital money ''emerged,'' or is it simply the date incumbents'' preferred framing became institutionally dominant — with the true monetary phenomenon having emerged years earlier under the first_held_reading or became_thinkable_reading?',
    'Compare the lag between documented functional adoption (transaction volume data from pre-regulation fintech and mobile-money systems) and the date of formal statistical/regulatory incorporation across multiple jurisdictions; a consistently long and incumbent-favorable lag would support the reading that the regulatory date is an administrative artifact rather than the true origin.',
    'If the lag is shown to be systematically manipulated by incumbent lobbying rather than driven by genuine measurement difficulty, the tangled_rope classification''s extraction component strengthens further toward snare; if the lag reflects genuine technical/legal complexity in building measurement infrastructure, the coordination component is stronger than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_date_selection_ambiguity, conceptual, 'Whether the regulatory recognition date is a genuine origin marker or a captured administrative artifact.').

omega_variable(
    kernel_framing_selection,
    'Given that all three readings (became_thinkable, first_held, regulatory_recognition) are internally coherent framings of ''when digital money emerged,'' what determines which framing a given monetary history or policy document adopts, and does that choice track institutional interest?',
    'Survey central bank publications, academic monetary histories, and fintech-industry accounts to see whether framing choice correlates with the author''s institutional position (regulators favor regulatory_recognition_reading; technologists favor became_thinkable_reading; users/historians favor first_held_reading).',
    'If framing choice tracks institutional interest, this substantiates the claim that the choice of origin reading is itself a strategic move rather than a neutral historiographical one — reinforcing why the framework decomposes this into separate constraint stories rather than resolving the ambiguity within one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether framing selection among the three kernel readings correlates with institutional interest.').

omega_variable(
    coordination_extraction_separability,
    'Is the statistical/regulatory measurement function of monetary aggregation separable from the incumbent-favoring licensing perimeter, or are they structurally fused such that any accurate measurement regime necessarily privileges already-licensed entities?',
    'Examine jurisdictions that have implemented lighter-touch or activity-based (rather than entity-based) licensing for digital money and compare measurement accuracy and market concentration outcomes against entity-based licensing regimes.',
    'If separable, the extraction is contingent policy design rather than an inherent feature of monetary measurement, meaning the tangled_rope''s extractive component could in principle be engineered out; if inseparable, the extraction is a structural cost of any workable measurement regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the coordination and extraction functions of regulatory recognition are separable by policy design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__regulatory_recognition_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__regulatory_recognition_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__regulatory_recognition_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__regulatory_recognition_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__regulatory_recognition_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_origin__regulatory_recognition_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% Part of the digital_money_origin kernel family (3 stories, one per reading). became_thinkable_reading (earliest date, near-mountain: technical/institutional inevitability) -> first_held_reading (middle date, rope-leaning: bottom-up practical adoption) -> regulatory_recognition_reading (this story; latest date, tangled_rope: coordination function fused with incumbent-favoring extraction). Each story has a distinct epsilon, distinct beneficiary/victim structure, and distinct claimed_type; they are linked, not merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
