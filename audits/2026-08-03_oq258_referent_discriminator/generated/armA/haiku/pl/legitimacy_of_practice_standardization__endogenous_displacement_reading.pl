% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Practice Legitimacy: Voluntary Adoption via Utility & Cultural Evolution
 *   domain: political_history/institutional_change/modernization_studies
 *
 * SUMMARY:
 *   This constraint encodes one reading of how practice legitimacy operates
 *   during standardization: the endogenous reading claims practice change is
 *   legitimate when it arises from voluntary adoption by communities
 *   perceiving utility, enabled by cultural evolution and diffusion without
 *   central authority decree. The reading is instantiated in cases like the
 *   gradual adoption of the Gregorian calendar in Orthodox regions, the
 *   evolution of professional dress codes in emerging industries, or the
 *   standardization of administrative procedures through merchant networks.
 *   The claim/metric gap is deliberate: the constraint is CLAIMED as rope
 *   (genuine coordination solving friction) while the metrics show measurable
 *   extraction—rising extractiveness (0.12 → 0.28 over the interval) driven
 *   by the cost displacement onto traditional practitioners. The endogenous
 *   reading treats this as an acceptable cost of beneficial coordination; the
 *   exogenous and dual-practice readings contest this evaluation.
 *
 * KEY AGENTS:
 *   - early_adopters: organized/mobile — perceive utility and shift voluntarily; model the new practice for others
 *   - utility_maximizers: organized/mobile — actively promote adoption in their networks (merchants, scholars, laborers); drive the coordination curve
 *   - traditional_practitioners: moderate/identity_locked — bear the cost of displacement as expertise and identity rooted in old practice become marginal
 *   - religious_authorities: institutional/constrained — observe the adoption and may adjust doctrine to accommodate; constrained by scripture but not immobile
 *   - state_administration: institutional/analytical — records and codifies the voluntary change once a threshold is reached; does not drive the change in the endogenous reading
 *   - cross_cutting_communities: organized/mobile — adopt early and span multiple jurisdictions; reduce friction across boundaries
 *   - ritual_custodians: moderate/identity_locked — excluded from the adoption conversation; allowed to preserve old practice in protected ritual domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Practice Legitimacy: Voluntary Adoption via Utility & Cultural Evolution").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/institutional_change/modernization_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'fbd3538e-7475-440b-a0d3-2b2602b2f949').
narrative_ontology:cs_kernel_codification('fbd3538e-7475-440b-a0d3-2b2602b2f949', distributed).
narrative_ontology:cs_authority_grounding('fbd3538e-7475-440b-a0d3-2b2602b2f949', distributed).
narrative_ontology:cs_reading_relation('fbd3538e-7475-440b-a0d3-2b2602b2f949', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbd3538e-7475-440b-a0d3-2b2602b2f949', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('fbd3538e-7475-440b-a0d3-2b2602b2f949', foundational, voluntary_adoption_legitimates_practice_change).
narrative_ontology:cs_axiom_status(voluntary_adoption_legitimates_practice_change, holdable).
narrative_ontology:cs_axiom_grounding('fbd3538e-7475-440b-a0d3-2b2602b2f949', voluntary_adoption_legitimates_practice_change, conventional).
narrative_ontology:cs_axiom('fbd3538e-7475-440b-a0d3-2b2602b2f949', foundational, cultural_evolution_is_valid_authority_source).
narrative_ontology:cs_axiom_status(cultural_evolution_is_valid_authority_source, holdable).
narrative_ontology:cs_axiom_grounding('fbd3538e-7475-440b-a0d3-2b2602b2f949', cultural_evolution_is_valid_authority_source, conventional).
narrative_ontology:cs_axiom('fbd3538e-7475-440b-a0d3-2b2602b2f949', secondary, state_mandate_not_required_for_legitimacy).
narrative_ontology:cs_axiom_status(state_mandate_not_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('fbd3538e-7475-440b-a0d3-2b2602b2f949', state_mandate_not_required_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('fbd3538e-7475-440b-a0d3-2b2602b2f949', decentralized_voluntary_adoption_as_legitimacy_source).
narrative_ontology:cs_drift_state('fbd3538e-7475-440b-a0d3-2b2602b2f949', contemporary_state_capacity_normalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fbd3538e-7475-440b-a0d3-2b2602b2f949', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_maximizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, cross_cutting_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recognize the practical advantage of the new practice (calendar efficiency, dress utility in climate/labor, administrative convenience) and voluntarily shift. They bear the modest cost of non-conformity with the old system and gain the efficiency benefit of the new system. Their shift is genuinely driven by perceived utility and is reversible if the advantage evaporates.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters, beneficiary,
    organized, biographical, mobile, regional).

% Adopt the new practice because it solves a coordination problem or improves efficiency in their domain (merchants adopting a unified calendar for trade, laborers adopting dress conventions suited to manufacturing). They actively promote the shift within their networks through example and recommendation, not coercion.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_maximizers, beneficiary,
    organized, biographical, mobile, regional).

% Bear the cost of the shift as their identity and social standing are rooted in mastery of and adherence to the old practice. The transition imposes a reorientation cost: unlearning the old system, losing status as an expert in it, potentially losing economic position if the old practice becomes economically marginal. Their exit is constrained by identity fusion with the practice itself.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practitioners, payer,
    moderate, generational, identity_locked, local).

% May see the old practice as religiously mandated or cosmically grounded (e.g., a calendar that aligns with sacred computation, or dress codes bound to ritual purity). The endogenous reading frames this authority as having legitimate say over the slow pace of change, but not a veto—they observe the adoption curve and may shift their doctrine to accommodate or declare the practice domain-neutral (a key mechanism of the endogenous model).
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, religious_authorities, payer,
    institutional, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, religious_authorities, observer).

% Tracks the voluntary adoption curve and may codify it into law once it reaches a threshold, or may stay neutral and let adoption remain customary. In the endogenous reading, the state's role is follower and recorder, not driver—it does not decree the change but recognizes it after sufficient voluntary uptake.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_administration, observer,
    institutional, generational, analytical, national).

% Communities that span multiple jurisdictions or ethnic groups (traders, scholars, nomadic groups) adopt the new practice early because it reduces friction across boundaries. They model the change for others and demonstrate its workability without coercion.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cross_cutting_communities, beneficiary,
    organized, biographical, mobile, regional).

% Groups whose entire role is defined by stewardship of the old practice (keepers of ancient calendars, masters of traditional dress-making). They would be structurally displaced by the change and have no seat at the table where adoption curves are measured or utility is debated. The endogenous reading does not ask their permission but does allow them to maintain the practice in protected ritual domains.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, ritual_custodians, excluded,
    moderate, civilizational, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_maximizers).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared calendar, dress convention, or administrative standard across dispersed communities by allowing each community to adopt at the pace and in the form that maximizes its own utility, removing friction without imposing uniformity. The change solves a coordination problem (different systems prevent trade/administration/scholarship) without requiring central authority.
% TRANSFER_FUNCTION: Shifts the cost of coordination from the adopters (who no longer bear the cost of maintaining multiple parallel systems) to the traditional practitioners and ritual custodians (who lose status, expertise value, and identity coherence as the old system becomes marginal). The transfer is diffuse, emergent from the adoption curve rather than intentional extraction.
% ABSENT_VOICES: Ritual custodians, religious scholars bound to the old system, and communities geographically isolated from early-adopter networks are not consulted in the adoption process. The endogenous reading does not require their input to legitimate the change, only that sufficient numbers of voluntary adopters make the new practice 'normal.' They may preserve the old practice in protected ritual or household domains, but have no formal seat in determining legitimacy.
% DISAPPEARANCE_RATIONALE: If this legitimating principle vanished—if people stopped recognizing voluntary adoption and cultural evolution as valid grounds for practice change—then all change would require exogenous decree or dual-practice equilibrium governance. Merchants could not unilaterally adopt a more efficient calendar; dress would be legislated rather than evolved; administrative standards would require state mandate. The institutions of modern practice would not spontaneously form.
% FOUNDING_PROBLEM: Traditional societies operate multiple incompatible practices (calendars, dress codes, measurement systems, administrative procedures) across overlapping jurisdictions. Trade, scholarship, and governance create friction costs at every boundary. Early adopters recognize efficiency gains from unified practices but have no authority to impose them; ritual and religious authorities resist change on grounds of cosmic/sacred grounding; state authority is weak or distributed. The problem is: how does beneficial practice change occur without central authority or religious schism?
% FOUNDING_PROBLEM_CORROBORATION: Historians of early modern Europe and post-colonial societies document the gradual spread of calendar, dress, and administrative standardization through voluntary adoption and elite/merchant modeling, not state decree (see: calendar transitions in Orthodox vs. Western Christianity, dress codes in colonial and post-colonial dress debates). The endogenous reading is corroborated by the empirical record of adoption curves. The exogenous reading points to cases where state mandates accelerated adoption (e.g., French Revolutionary calendar, Soviet collectivization of labor dress). The dual-practice reading is corroborated by long-term coexistence of old and new practices in ritual vs. administrative domains. All three readings cite the same empirical base but interpret legitimacy differently.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.12 → 0.28) because the constraint's operation gradually displaces the cost of the old practice onto traditional practitioners and ritual custodians. Early in the interval (t=0–15), few have adopted and the cost is diffuse; as adoption reaches critical mass (t=30–75), the old practice becomes economically marginal and practitioners face real displacement. This is not suppression (forcing adoption) but the asymmetric outcome of voluntary choice: once adoption reaches ~60%, the old practice stops being economically viable or socially normal, and those bound to it by identity (religious scholars, traditional craftspeople) absorb a concentration of the transition cost. Suppression remains low (0.03 → 0.15) because voluntary adoption is the mechanism; no state apparatus is required to enforce the new practice. Theater is negligible (0.02 → 0.08) because the coordination function is real: the new practice genuinely solves friction. The rising trajectory reflects not degradation but the accumulation of a real cost (displacement) that the endogenous reading permits because it flows from legitimate voluntary choice, not coercive mandate. Accessibility_collapse (0.42) reflects that once the new practice reaches criticality, the old practice becomes structurally unavailable (even if not legally forbidden) because the coordination benefit is so strong; resistance (0.38) reflects the sustained objection from traditional practitioners and religious authorities, which the endogenous reading acknowledges but does not treat as a veto. Measurements share one time grid: every metric is authored at every time point (0, 15, 30, 50, 75, 100).
 *
 * PERSPECTIVAL GAP:
 *   The early-adopter and merchant seats see the constraint as genuine coordination: friction is reduced, trade flows, standards emerge. The traditional-practitioner seat sees it as displacement: expertise becomes worthless, identity becomes marginal, economic position erodes. The ritual-custodian seat sees it as partial preservation: the old practice survives in protected domains (household, ritual, sacred) but loses public legitimacy and economic support. The state sees it as natural evolution to be recorded after the fact. These divergences flow from the structural positions: beneficiaries perceive voluntary benefit; payers absorb diffuse costs that accumulate to concentration; excluded parties maintain islands of preservation. The endogenous reading's claim is that this structure is legitimate because the adoption was voluntary for those who chose it, even if it displaced those who did not choose. The engine computes these per-seat divergences from the structural data; the claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and utility maximizers benefit from reduced friction and are mobile enough to shift without identity loss (d ≈ 0.1–0.25, low extraction). Traditional practitioners and ritual custodians are identity-locked and displaced (d ≈ 0.75–0.95, high extraction). Religious authorities are constrained but not trapped (d ≈ 0.45–0.55, symmetric). The key structural asymmetry: voluntary adoption by the mobile enables displacement of the identity-locked. The endogenous reading treats this as acceptable because the change emerged from utility-driven choice, not central mandate; the exogenous reading contests this as illegitimate externality-imposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how beneficial practice change occurs in decentralized systems without central authority or schism—is not dead but contested. In societies with strong state capacity, the exogenous reading becomes dominant and practice change is legislated. In societies with weak state capacity or strong local autonomy, the endogenous reading remains live: practices evolve through merchant networks, elite adoption, and diffusion. The dual-practice reading is always available as a institutional solution: public/administrative domains adopt new practices; private/ritual domains preserve old practices. The three readings coexist as live institutional strategies, not as a sequence where one obsoletes another. The endogenous reading does not face mandatrophy (hollow command) but does face the omega question: does the voluntary adoption mechanism truly avoid illegitimate externality on the identity-locked, or does it produce a 'consensus through displacement' that is extractive in form even if not in intention?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_vs_displacement_externality,
    'Does ''voluntary adoption by some'' legitimize the displacement of the identity-locked (those for whom exit is not genuinely available)? Is the adoption truly voluntary if the alternative is economic and social marginalization?',
    'Ethnographic and historical investigation of adoption curves: do individuals truly face costless exit from the old practice, or is ''voluntary'' adoption an artifact of the high exit cost of refusing (taking up the new practice or being left behind)? Post-adoption interviews with traditional practitioners on whether they experienced choice or necessity.',
    'If adoption is structurally coercive (everyone must choose the new practice or be displaced), the constraint shifts toward snare classification despite the absence of formal enforcement. If adoption is genuinely voluntary with preservation options (old practice remains economically viable), the rope classification holds. If adoption is mixed (voluntary for the mobile, coercive for the identity-locked), the classification splits per seat and demands omega documentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_displacement_externality, empirical, 'Whether ''voluntary adoption'' mechanism truly avoids coercion or produces consensus through displacement of those who refuse.').

omega_variable(
    utility_perception_as_constructed_vs_objective,
    'Is the perceived utility that drives adoption an objective property of the new practice, or is it constructed/incentivized by early adopters and merchants who benefit from standardization?',
    'Controlled investigation: compare adoption curves in cases where utility is objectively measurable (e.g., calendar efficiency in astronomical precision) vs. cases where utility is constructed by advocacy (e.g., dress codes promoted by commercial interests as ''modern''). Do adoption curves match the objective utility or the constructed utility narrative?',
    'If utility is objective, the endogenous reading is strengthened: voluntary adoption flows from genuine benefit. If utility is constructed, the endogenous reading collapses toward snare or tangled_rope: the beneficiaries (merchants, administrators) use the ''voluntary adoption'' frame to mask their capture of the coordination problem and its redirection toward their interests. The reading''s legitimacy depends on the independence of utility perception from beneficiary interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_perception_as_constructed_vs_objective, empirical, 'Whether perceived utility driving adoption is independent from beneficiary interests or is constructed by them.').

omega_variable(
    dual_practice_coexistence_as_privilege_vs_preservation,
    'When the endogenous reading permits the old practice to coexist in protected domains (ritual, household, sacred), is this genuine preservation or a privilege extended only to the powerful/elite?',
    'Comparative analysis of dual-practice outcomes: does preservation of the old practice occur equally across all social levels (rich and poor alike maintain old dress at home), or is it skewed toward elite preservation while common people face pressure to adopt (wealthy merchants preserve heirloom traditions; poor laborers cannot afford dual wardrobes)? Historical documentation of who was permitted to maintain old practices.',
    'Equal coexistence supports the preservation claim and the rope reading. Skewed preservation toward elite suggests a snare mechanism: the old practice is permitted in elite ritual contexts (as culturally prestigious) while being prohibited or economically unsustainable for common people, producing a status/privilege gradient masked by formal symmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_practice_coexistence_as_privilege_vs_preservation, empirical, 'Whether permitted dual-practice coexistence is symmetric across social levels or is skewed toward elite privilege.').

omega_variable(
    kernel_reading_underspecification,
    'The legitimacy commitment itself—what makes a practice legitimate?—remains ambiguous even within the endogenous reading. Is legitimacy grounded in utility (empirical), custom (sociological), authority (normative), or consent (procedural)? The endogenous reading emphasizes ''voluntary adoption driven by perceived utility'' but does not specify whether utility is the legitimating source or merely the mechanism that produces the voluntary adoption that legitimates.',
    'Axiomatic specification: does the endogenous reading hold that ''voluntary adoption of anything perceived as useful is legitimate'' (weak reading, easily captured), or ''voluntary adoption of objectively beneficial practices is legitimate'' (strong reading, requires utility verification)? Clarification of whether utility perception or objective utility legitimates the change.',
    'Weak reading: vulnerable to gaming by beneficiaries who construct utility narratives to drive adoption. Strong reading: requires institutional capacity to verify objective utility, which may be unavailable in early modern or post-colonial contexts. The reading''s institutional robustness depends on this specification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'Whether the endogenous reading grounds legitimacy in utility perception (procedural) or objective utility (substantive).').

omega_variable(
    committer_frame_kernel_identity,
    'Which of the three readings—exogenous, endogenous, dual-practice—represents the true legitimacy ground, and which are illegitimate substitutes or cover stories? Or do all three remain genuinely live as institutional strategies for different contexts?',
    'This question cannot be resolved empirically; it is a preference/normative question. The resolution is political and institutional: societies choose which reading to institutionalize (strong state → exogenous; federated/merchant-network → endogenous; pluralist → dual-practice). No reading is universally true; all are contextually available.',
    'If the endogenous reading is chosen as the legitimacy ground, then state-imposed practice change (exogenous) is illegitimate, and dual-practice arrangements are viewed as compromise or failure. If the exogenous reading is chosen, then merchant-driven standardization is viewed as insufficient and requires state codification. The kernel contest is unresolvable without choosing a normative framework. The three readings coexist as sibling strategies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_kernel_identity, preference, 'Whether legitimacy of practice change is fundamentally grounded in voluntary adoption (endogenous), state authority (exogenous), or domain partitioning (dual-practice), or whether all remain live strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 15, 0.03).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.04).
narrative_ontology:measurement_basis(legi_tr_t30, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement_basis(legi_tr_t50, observed).
narrative_ontology:measurement(legi_tr_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 75, 0.07).
narrative_ontology:measurement_basis(legi_tr_t75, observed).
narrative_ontology:measurement(legi_tr_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(legi_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.23).
narrative_ontology:measurement_basis(legi_be_t30, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.27).
narrative_ontology:measurement_basis(legi_be_t50, observed).
narrative_ontology:measurement(legi_be_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 75, 0.28).
narrative_ontology:measurement_basis(legi_be_t75, observed).
narrative_ontology:measurement(legi_be_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(legi_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 15, 0.05).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement_basis(legi_su_t30, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(legi_su_t50, observed).
narrative_ontology:measurement(legi_su_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 75, 0.14).
narrative_ontology:measurement_basis(legi_su_t75, observed).
narrative_ontology:measurement(legi_su_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement_basis(legi_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, information_standard).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel legitimacy_of_practice_standardization. The kernel is the standing commitment grounding the authority of practice change. The endogenous_displacement_reading claims voluntary adoption and cultural evolution are sufficient legitimacy grounds; the exogenous_override_reading claims state authority is necessary; the dual_practice_equilibrium_reading claims both are legitimate in their respective domains. The three readings share the same empirical referent (adoption curves, resistance patterns, displacement outcomes) but differ in what makes the change legitimate. ε is identical across readings (0.28 at interval end)—extractiveness is a property of the constraint structure, not of the reading. The readings differ in their foundational axioms: the endogenous reading valorizes voluntary choice; the exogenous reading valorizes collective benefit via state coordination; the dual-practice reading valorizes institutional pluralism. Each reading is a structurally complete, ε-invariant constraint. The network links show causal/conceptual dependence: both sibling readings reference the endogenous reading as a contrast case and a legitimacy claim that they contest or complicate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
