% ============================================================================
% CONSTRAINT STORY: burden_of_proof_engineering_safety
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burden_of_proof_engineering_safety, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burden_of_proof_engineering_safety
 *   human_readable: Reversal of the Engineering Burden of Proof in Safety Certification
 *   domain: technological/aviation_safety/regulatory
 *
 * SUMMARY:
 *   The reversal of the engineering burden of proof in aviation safety
 *   represents a critical institutional failure where regulatory frameworks
 *   shifted from requiring manufacturers to prove safety (defensible under
 *   precautionary principle) to requiring regulators to prove unsafety
 *   (defending the status quo). This constraint exhibits characteristics of a
 *   snare: beneficiaries (manufacturers) capture rents through reduced
 *   certification costs and expedited approval, while victims (flying public,
 *   independent safety engineers) bear suppressed voice and trapped exit. The
 *   constraint emerged gradually through regulatory capture, cost-pressures
 *   on certification authorities, and the normalization of manufacturer-led
 *   self-certification. Theater ratio increased from 0.35 to 0.68 over the
 *   interval as the formal certification process became increasingly
 *   performative: manufacturer designs reviewed by FAA Authorized
 *   Representatives (often employed by manufacturers), design approval grants
 *   authority to certify their own designs, regulatory oversight becomes a
 *   rubber-stamp process rather than independent verification. The extraction
 *   mechanism operates through asymmetric information (manufacturers know
 *   design details; regulators do not) and asymmetric liability
 *   (manufacturers distribute risk across insurance markets; public bears
 *   concentrated catastrophic risk). The constraint is not a natural law or
 *   temporary coordination challenge — it is a deliberate institutional
 *   choice that can be reversed by shifting the burden back to manufacturers.
 *
 * KEY AGENTS:
 *   - Aircraft Manufacturers: Primary beneficiary (institutional/arbitrage) — capture regulatory rents through reduced certification burden; can influence standards through lobbying
 *   - Flying Public: Primary victim (powerless/trapped) — passive participants with no control over certification criteria; cannot exit aviation without significant cost
 *   - Independent Safety Engineers: Secondary victim (moderate/constrained) — gatekept from certification processes; face career suppression for publishing independent assessments
 *   - Regulatory Authority (FAA/EASA): Captured institution (institutional/constrained) — formally mandated to protect safety but functionally dependent on manufacturer cooperation; resource-constrained
 *   - Certification Theater System: Institutional actor (institutional/arbitrage) — maintains performative approval processes; survives through inertia despite degraded function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes the reversal as a structural choice, not a natural law or technical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_engineering_safety, 0.58).
domain_priors:suppression_score(burden_of_proof_engineering_safety, 0.72).
domain_priors:theater_ratio(burden_of_proof_engineering_safety, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, extractiveness, 0.58).
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burden_of_proof_engineering_safety, snare).
narrative_ontology:human_readable(burden_of_proof_engineering_safety, "Reversal of the Engineering Burden of Proof in Safety Certification").
narrative_ontology:topic_domain(burden_of_proof_engineering_safety, "technological/aviation_safety/regulatory").

domain_priors:requires_active_enforcement(burden_of_proof_engineering_safety).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burden_of_proof_engineering_safety, aircraft_manufacturers).
narrative_ontology:constraint_beneficiary(burden_of_proof_engineering_safety, certification_authorities_capturing_rents).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, flying_public).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, independent_safety_engineers).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, regulatory_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FLYING PUBLIC (SNARE) — Passive participants in aviation system with zero control over certification criteria. Cannot exit (flying is economically necessary for modern life). Bears full cost if safety verification fails: catastrophic loss. No appeal mechanism; no visibility into certification process. Maximum structural extraction — beneficiaries capture rents, victims are trapped.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT SAFETY ENGINEERS (SNARE) — Can exit aviation sector but at high career cost. Face suppression: must work within manufacturer-approved frameworks or be excluded from certification processes. Cannot publish independent safety assessments without manufacturer approval (NDA barriers). Constrained exit + high suppression → snare classification. Career extraction mechanism operates through gatekeeping.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AIRCRAFT MANUFACTURERS (ROPE) — Primary beneficiary. Experience the burden-of-proof reversal as pure coordination: once certification criteria favor manufacturers, the certification process becomes a collaborative relationship. Arbitrage exit available (can relocate to favorable jurisdictions, influence regulatory standards through lobbying). Experiences effective extraction as negative (subsidy). Net benefit from the reversed burden.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Captured institution experiencing mixed signals. Formally has mandate to protect public safety (beneficiary function); functionally dependent on manufacturer cooperation and industry self-certification (victim to extraction pressure). Cannot exit without jurisdictional loss. Constrained by resource limitations and political pressure from manufacturers. Requires active enforcement (FAA Authorized Representatives system maintains performative oversight). Both extraction (manufacturer capture) and coordination (safety standards) functions present simultaneously.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CERTIFICATION THEATER SYSTEM (PITON) — The formal institutional structure (design review boards, hazard analyses, certification documents) persists as ritual despite degraded function. Theater ratio 0.68 reflects that much of the approval process is now performative: manufacturer-led self-certification, FAA stamp-of-approval after manufacturer design approval, regulatory bodies reviewing manufacturer submissions they lack resources to independently verify. The structure remains maintained through institutional inertia (international harmonization, legacy agreements, bureaucratic lock-in) despite its primary safety-verification function having atrophied.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes the burden-of-proof reversal as a hybrid institutional arrangement combining genuine coordination (the need to establish baseline safety criteria) with systematic extraction (the reversal that shifts proof burden from manufacturers to regulators, privatizing certification gains while socializing failure costs). Not a mountain (no natural law) — the reversal is a deliberate institutional choice. Not pure rope (genuine asymmetric extraction is present). The constraint exhibits both coordination function (safety standards) and extraction mechanism (rents captured through reversed burden).
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_engineering_safety_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(burden_of_proof_engineering_safety, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(burden_of_proof_engineering_safety, TR),
    TR >= 0.70.

:- end_tests(burden_of_proof_engineering_safety_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The burden reversal creates measurable rents for manufacturers through reduced certification costs, accelerated timelines, and reduced design constraints. However, the extraction is not total because: (1) manufacturers still face some regulatory oversight (theater-based rather than substantive), (2) liability and reputational incentives provide partial safety alignment, (3) international coordination creates some competitive pressure. The value reflects that extraction is real but constrained by imperfect liability regimes and remaining regulatory friction. Suppression (0.72): High. Multiple barriers suppress independent verification: NDAs on design information, regulatory authority gatekeeping, resource constraints on independent safety assessment, professional suppression (engineers cannot publish critical findings without manufacturer approval), structural dependence on manufacturer cooperation for data access. These are not soft social pressures — they are hard institutional barriers. Theater ratio (0.68): High-moderate. The certification process maintains formal appearance of rigorous safety review (design review boards, hazard analyses, certification documents, FAA stamp of approval) while the actual safety verification mechanism has degraded. Manufacturer-led self-certification with FAA review of manufacturer submissions means regulators are verifying manufacturers' verification rather than independently assessing safety. The theater persists because the alternative (full re-evaluation by regulators) is politically infeasible and resource-prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between beneficiaries and victims. Manufacturers experience the burden reversal as pure coordination (Rope): a efficient allocation where designers with full information handle safety assessment, regulated by clear criteria. Victims experience it as a snare: the flying public is trapped in a system where proof burden favors the beneficiary. Independent engineers experience it as extraction through gatekeeping (also Snare): they cannot participate in certification without manufacturer approval. The regulatory authority experiences it as tangled rope: formally mandated for safety (coordination function) but captured by manufacturer pressure (extraction mechanism). The certification theater experiences it as piton: the formal structure persists (design reviews, hazard analyses) but its function has atrophied (manufacturers verify themselves). The analytical observer recognizes this as tangled rope with high extractiveness: genuine coordination function exists (baseline safety standards are necessary) but is entangled with systematic extraction (burden reversal favors beneficiaries, socializes failure costs). The perspectival gap reflects that the same institutional arrangement delivers genuine goods (coordination of safety standards) bundled with real harms (extraction of rents, suppression of independent verification).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position and exit options. Manufacturers have arbitrage exit (can relocate to favorable jurisdictions, influence standards through lobbying) and beneficiary status → low d → negative f(d) → they experience effective extraction as subsidy. Flying public has trapped exit (cannot exit aviation) and victim status → high d → high f(d) → they experience maximum extraction. Independent engineers have constrained exit (can leave sector but at career cost) and victim status → high d but moderated by moderate power → they experience significant extraction through gatekeeping. Regulatory authority has constrained exit (losing jurisdiction is politically unacceptable) but mixed beneficiary/victim status (formal mandate for safety, functional dependence on manufacturers) → moderate d → mixed extraction experience. The reversal mechanism itself operates by shifting d: it changes what counts as evidence (manufacturer assertion instead of independent verification) such that victim d increases (burden falls on those challenging the claim) while beneficiary d decreases (burden removed from those making the claim).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The mandatrophy (the apparent paradox that a coordination mechanism appears extractive) is resolved by recognizing that the burden reversal decouples the coordination function from the extraction mechanism. The genuine coordination function (establishing baseline safety criteria through engineering consensus) is necessary and coordination-like. However, the institutional reversal (shifting proof burden from manufacturers to regulators) layers systematic extraction onto this function by: (1) privatizing the gains (manufacturers avoid certification costs), (2) socializing the losses (public bears concentrated catastrophic risk), (3) suppressing alternatives (independent verification gatekept), (4) asymmetricizing information (manufacturers know design details, regulators do not). The constraint is Snare (pure extraction dominant) rather than Tangled Rope because the extraction mechanism (burden reversal) is not necessary for the coordination function (safety standards can be established with manufacturer-proves-safety framework). The reversal is a deliberate institutional choice, not a constraint inherent to the coordination problem. This is falsifiable: if the burden-of-proof were reversed back to 'manufacturer must prove safety,' the coordination function would remain intact while the extraction mechanism would be eliminated, proving that the reversal is not technically necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    certification_standard_independence,
    'Are the certification standards established through genuine engineering consensus or through manufacturer-influenced regulatory capture?',
    'Historical analysis of rulemaking processes; comparison of manufacturer-advocated standards vs. independent engineer assessments; tracing of regulatory personnel movement to/from industry',
    'If captured: burden reversal is fully structural snare. If independent: reversal is a legitimate temporary design choice with oversight mechanisms in place.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(certification_standard_independence, empirical, 'Whether certification standards reflect genuine engineering consensus or manufacturer influence').

omega_variable(
    safe_to_fly_verification_gap,
    'What specific verification gap exists between what manufacturers claim is safe and what independent analysis would require as proof?',
    'Comparative analysis of manufacturer safety cases vs. independent accident investigation findings; longitudinal tracking of design assumptions validated vs. invalidated post-certification',
    'If gap < 5%: reversal may be efficient (asymmetric information is minimal). If gap > 15%: reversal creates systematic undetected failure modes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safe_to_fly_verification_gap, empirical, 'Magnitude of verification gap between manufacturer and independent safety standards').

omega_variable(
    regulatory_resource_adequacy,
    'Could regulatory authorities adequately verify safety claims if given sufficient resources, or is the reversal a structural necessity given real resource constraints?',
    'Modeling of verification resource requirements vs. actual/potential funding levels; international comparison of regulatory capacity across jurisdictions with different burden-of-proof approaches',
    'If adequacy is achievable: burden reversal is policy choice, not technical necessity (snare classification strengthened). If truly constrained: may justify temporary scaffold-like approach with sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_resource_adequacy, empirical, 'Whether regulatory authorities could adequately verify safety with sufficient resources').

omega_variable(
    manufacturer_safety_incentive_alignment,
    'Are manufacturer liability and reputational incentives sufficient to align private certification with public safety objectives, or are they structurally misaligned?',
    'Analysis of liability regimes across jurisdictions; study of manufacturer behavior under different liability structures; historical examination of safety-cutting corners vs. liability exposure',
    'If aligned: burden reversal may be efficient coordination. If misaligned: reversal creates systematic incentive for cost-cutting that harms safety.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manufacturer_safety_incentive_alignment, empirical, 'Whether manufacturer incentives align with public safety objectives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burden_of_proof_engineering_safety, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bpproof_tr_t0, burden_of_proof_engineering_safety, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bpproof_tr_t25, burden_of_proof_engineering_safety, theater_ratio, 25, 0.52).
narrative_ontology:measurement(bpproof_tr_t50, burden_of_proof_engineering_safety, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(bpproof_be_t0, burden_of_proof_engineering_safety, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bpproof_be_t25, burden_of_proof_engineering_safety, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(bpproof_be_t50, burden_of_proof_engineering_safety, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burden_of_proof_engineering_safety, enforcement_mechanism).
narrative_ontology:affects_constraint(burden_of_proof_engineering_safety, manufacturer_self_certification_authority).
narrative_ontology:affects_constraint(burden_of_proof_engineering_safety, regulatory_capture_in_aviation).
narrative_ontology:affects_constraint(burden_of_proof_engineering_safety, information_asymmetry_certification).

% DUAL FORMULATION NOTE:
% The burden-of-proof reversal is the institutional mechanism through which regulatory capture operates in aviation. It represents a shift in the epistemic burden from manufacturers (who must prove safety) to regulators (who must prove unsafety). This is distinct from but structurally upstream of specific manufacturer misconduct stories — it is the institutional framework that enables such misconduct by placing proof burden on those least resourced to meet it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(burden_of_proof_engineering_safety, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
