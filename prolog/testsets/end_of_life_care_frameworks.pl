% ============================================================================
% CONSTRAINT STORY: end_of_life_care_frameworks
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_care_frameworks, []).

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
 *   constraint_id: end_of_life_care_frameworks
 *   human_readable: End-of-Life Care Frameworks: Medical Authority, Patient Autonomy, and Resource Allocation
 *   domain: healthcare/bioethics/institutional
 *
 * SUMMARY:
 *   End-of-life care frameworks (advance directives, DNR protocols, family
 *   conference structures, informed consent procedures, palliative care
 *   integration guidelines) exist to coordinate decisions about how to manage
 *   dying and death within medical institutions. They address a genuine
 *   coordination problem: patients, families, and clinicians must navigate
 *   complex medical, ethical, and emotional terrain when cure is no longer
 *   possible. The constraint exhibits the full range of DR types from
 *   different perspectives because the frameworks simultaneously accomplish
 *   genuine coordination (clarifying patient wishes, enabling shared
 *   decision-making, reducing miscommunication) and extract value
 *   (institutional risk reduction, family emotional labor, resource rationing
 *   masked as patient preference, clinician authority preservation). The
 *   extractiveness (0.58) reflects that institutional benefits measurably
 *   exceed patient/family benefits: frameworks reduce institutional liability
 *   and uncertainty while constraining patient choice through information
 *   asymmetry and emotional crisis. The suppression (0.68) reflects
 *   significant barriers to alternative end-of-life pathways: home death
 *   without medical oversight, family-led care decisions without professional
 *   consultation, unconventional comfort measures, and resource-intensive
 *   individualized approaches are systematically discouraged. The theater
 *   ratio (0.65) reflects that substantial framework activity (documentation,
 *   family conferences, advance directive completion) is performative: it
 *   establishes institutional legitimation and liability protection rather
 *   than improving communication or decision quality.
 *
 * KEY AGENTS:
 *   - Terminal Patients: Primary victim (powerless/trapped) — structurally dependent on medical system with no exit capacity; framework suppresses alternatives while reframing institutional constraints as patient choice
 *   - Families/Decision-Makers: Secondary victim (moderate/constrained) — high emotional and informational barriers constrain agency; framework extracts moral labor (emotional processing, institutional legitimation) while coordinating genuine decisions
 *   - Medical Institutions: Primary beneficiary (institutional/arbitrage) — reduce liability, establish protocols for resource allocation, gain risk management and legal protection with minimal coercive overhead
 *   - Healthcare Administrators: Secondary beneficiary (institutional/arbitrage) — benefit from frameworks as cost control mechanisms and liability shields; can adopt or modify with minimal operational cost
 *   - Insurance Systems: Secondary beneficiary (institutional/arbitrage) — frameworks enable and justify resource rationing by encoding it as patient preference rather than cost containment
 *   - Palliative Care Movement: Mixed (organized/constrained) — advocates coordinate genuine end-of-life improvements while experiencing structural extraction and resource subordination to curative protocols
 *   - Patient Advocacy Groups: Organized agents (organized/constrained) — reformers see frameworks as transitional structures with sunset toward genuine autonomy; building alternative pathways (advance directives, shared decision-making, patient self-determination)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements around mortality as immutable laws of dying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_care_frameworks, 0.58).
domain_priors:suppression_score(end_of_life_care_frameworks, 0.68).
domain_priors:theater_ratio(end_of_life_care_frameworks, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_care_frameworks, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_care_frameworks, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_care_frameworks, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_care_frameworks, tangled_rope).
narrative_ontology:human_readable(end_of_life_care_frameworks, "End-of-Life Care Frameworks: Medical Authority, Patient Autonomy, and Resource Allocation").
narrative_ontology:topic_domain(end_of_life_care_frameworks, "healthcare/bioethics/institutional").

domain_priors:requires_active_enforcement(end_of_life_care_frameworks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_care_frameworks, medical_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_care_frameworks, healthcare_administrators).
narrative_ontology:constraint_beneficiary(end_of_life_care_frameworks, insurance_systems).
narrative_ontology:constraint_victim(end_of_life_care_frameworks, terminal_patients).
narrative_ontology:constraint_victim(end_of_life_care_frameworks, family_decision_makers).
narrative_ontology:constraint_victim(end_of_life_care_frameworks, resource_constrained_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TERMINAL PATIENT (SNARE) — No exit capacity. Structurally dependent on medical system for pain management, consciousness control, and end-state determination. Framework suppresses alternatives (home death, family-led care, unconventional comfort measures) while extracting consent narratives that position institutional protocols as expressions of patient autonomy. Patient experiences maximum extraction: constrained choices reframed as autonomous choice, institutional risk management renamed patient preference.
constraint_indexing:constraint_classification(end_of_life_care_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY DECISION-MAKER (TANGLED ROPE) — High emotional cost and information asymmetry prevent true exit, but some constrained agency exists. Framework coordinates genuine end-of-life decisions (necessary clarification of patient wishes) while extracting moral labor: families perform emotional work and institutional legitimation in exchange for limited information and constrained choice sets. Moderate extraction with real coordination function.
constraint_indexing:constraint_classification(end_of_life_care_frameworks, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL INSTITUTION (ROPE) — Genuine coordination function: frameworks enable complex end-of-life decisions, reduce litigation risk, establish protocols for resource allocation. Institution experiences framework as enabling coordination with minimal coercive overhead. Arbitrage exit option: institutions can adopt frameworks or develop alternatives with minimal cost. Net beneficiary via risk reduction and liability shield.
constraint_indexing:constraint_classification(end_of_life_care_frameworks, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PALLIATIVE CARE MOVEMENT (TANGLED ROPE) — Organized agents (hospice networks, palliative care advocates, patient rights groups) coordinate genuine end-of-life improvements (pain management, dignity preservation) while experiencing extraction: frameworks often subordinate palliative to curative protocols, restrict resource allocation to palliative services, and create constrained career paths for palliative specialists. Coordinating real good while experiencing structural extraction.
constraint_indexing:constraint_classification(end_of_life_care_frameworks, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AUTONOMY-BASED FRAMEWORKS COALITION (SCAFFOLD) — Patient advocacy groups, bioethicists, and legal reformers see frameworks as temporary structures with a sunset: advance directives, patient self-determination laws, and informed consent protocols are designed to transition authority from institutions to individuals. Low theater because the mechanism (informed consent documents, advance directives, shared decision-making protocols) directly enables patient choice rather than performing it. Organized agents with clear exit vision and time-limited enforcement logic.
constraint_indexing:constraint_classification(end_of_life_care_frameworks, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CLINICAL NOTES AND DOCUMENTATION SYSTEM (PITON) — The performative machinery that records end-of-life decisions persists through institutional inertia rather than functional necessity. Detailed DNR orders, family conference notes, and prognostic documentation are generated to establish liability protection and medical-legal clarity, not to improve communication or patient outcomes. Theater ratio (0.65) reflects that substantial documentation effort goes to risk management rather than meaningful clarification. The system maintains itself through regulatory requirements and habit despite evidence that simpler communication methods yield equal or better outcomes.
constraint_indexing:constraint_classification(end_of_life_care_frameworks, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MORTALITY VIEW (MOUNTAIN) — From a civilizational view, human mortality and the necessity of end-of-life decisions are brute facts: all creatures die, medical systems must address death, frameworks are inevitable expressions of natural biological constraint. This perspective risks naturalizing contingent institutional arrangements (resource rationing, authority concentration, risk management hierarchies) as immutable consequences of mortality itself. Engine will flag this as a false summit — the natural law is mortality; the constraint is how institutions organize around mortality.
constraint_indexing:constraint_classification(end_of_life_care_frameworks, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_care_frameworks_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(end_of_life_care_frameworks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(end_of_life_care_frameworks, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_care_frameworks, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(end_of_life_care_frameworks, TR),
    TR >= 0.70.

:- end_tests(end_of_life_care_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The base measurement reflects measurable institutional benefit concentration: frameworks reduce litigation risk (quantified in liability reduction metrics), enable resource allocation justification (quantified in ICU bed availability and discharge timing patterns), and decrease institutional decision-making burden (quantified in clinician time allocation shifts toward documentation and family conferences rather than direct care). The trajectory from 0.35 to 0.58 over 20 years reflects increasing institutional reliance on frameworks for cost management and risk reduction. Suppression (0.68): High. Multiple categories of barriers constrain alternatives: medical system dependency (pain management, consciousness control, prognostic information all channeled through institutional providers); information asymmetry (terminal patients and families typically lack medical knowledge and experience; institutional actors monopolize prognostic information); emotional crisis (end-of-life decisions occur in conditions of maximal emotional distress, reducing cognitive capacity for agency); regulatory constraints (unlicensed family-led care decisions face legal and insurance barriers); economic constraints (home-based and family-intensive care receive minimal insurance coverage and require significant out-of-pocket expense). Theater ratio (0.65): Moderate-high. Substantial framework activity is performative: advance directive completion is often a bureaucratic box-checking exercise rather than genuine clarification; family conference documentation establishes institutional liability protection rather than improving actual communication quality; DNR orders encode institutional risk management as patient preference; prognostic discussions are often calibrated to institutional needs rather than to family understanding. The increase from 0.42 to 0.65 reflects growing institutional documentation requirements and increasing use of frameworks for liability management rather than genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the diagnostic power of perspectival decomposition. A single policy framework (advance directives, DNR protocols, family conference structures) is simultaneously: (1) a genuine coordination mechanism (enabling shared decision-making about complex end-of-life questions), (2) an extraction mechanism (concentrating institutional benefits and constraining patient choice), (3) a temporary transitional structure with a sunset (moving toward genuine patient autonomy), (4) a degraded ritual (performative documentation without decision impact), and (5) a naturalized institutional arrangement (presented as immutable response to mortality). Which description is 'true' depends on whose structural position you're describing. The perspectival gap reveals that the frameworks are not internally contradictory — they accomplish all of these simultaneously. The Tangled Rope classification captures this: genuine coordination function (family conference does clarify wishes, advance directives do document preferences) AND asymmetric extraction (frameworks also constrain choice, extract labor, concentrate institutional benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their power level, exit options, and beneficiary/victim status. Terminal patients with trapped exit options and victim status experience d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction even at moderate base ε. Family decision-makers with constrained options and mixed victim/participant status experience d ≈ 0.70 → moderate extraction. Medical institutions with institutional power and arbitrage options experience d ≈ 0.15 → low/negative extraction (they are net beneficiaries). Organized palliative care advocates with constrained exit despite organized power experience d ≈ 0.55 → moderate extraction. The directionality computation explains why a framework with moderate base extractiveness (0.58) can be experienced as Snare by powerless trapped agents and Rope by institutional beneficiaries — the same constraint produces dramatically different χ values across different power/exit combinations.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids misclassification as pure extraction (Snare, χ ≈ 0.66) by declaring genuine coordination functions (shared decision-making, clarifying patient wishes, reducing miscommunication). The Tangled Rope classification is justified because: (1) beneficiaries exist (medical institutions, healthcare administrators, insurance systems) and they benefit through coordination (reduced liability, clear protocols, risk management); (2) victims exist (terminal patients, families, resource-constrained populations) and they experience extraction (constrained choice, information asymmetry, labor extraction); (3) the same framework mechanisms accomplish both goals — the family conference both clarifies wishes AND serves as institutional legitimation; the advance directive both documents preference AND reduces institutional liability. The constraint is not a Snare with false coordination claims — it is a Tangled Rope with genuine coordination that happens to concentrate benefits asymmetrically. The Scaffold perspective (patient autonomy frameworks moving toward real sunset) and Piton perspective (clinical theater degrading over time) coexist: some frameworks are genuinely transitional (Scaffold) while others have become institutional rituals (Piton). Mandatrophy is resolved by recognizing that the same base framework plays multiple roles simultaneously — coordination, extraction, transition, and theater — depending on implementation and institutional context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informed_consent_authenticity,
    'Can terminal patients and families make genuinely informed autonomous choices, or does the structural dependency (pain, cognitive decline, information asymmetry, emotional crisis) preclude authentic autonomy?',
    'Comparative analysis of patient-reported decision satisfaction pre-decline vs. post-decline; assessment of whether patients retrospectively endorse decisions made in crisis vs. decisions made while cognitively intact; correlation between information quality and decision stability',
    'If authentic autonomy is possible: framework provides genuine coordination with patient as agent. If structural dependency precludes autonomy: framework is extractive — reframes institutional decisions as patient choices. Classification shifts from Tangled Rope to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_authenticity, empirical, 'Whether informed consent is authentic given structural dependency').

omega_variable(
    resource_allocation_mechanism,
    'Do end-of-life frameworks primarily coordinate individual patient decisions (therapeutic goal) or enable systemic resource rationing masked as patient preference (distributive goal)?',
    'Historical analysis of framework deployment patterns: correlation between ICU bed scarcity and DNR recommendation frequency; analysis of whether frameworks are invoked uniformly across economic strata or concentrate in high-cost populations; comparison of resource allocation outcomes pre-framework vs. post-framework',
    'If primarily therapeutic coordination: framework is Rope or Tangled Rope depending on patient agency. If primarily rationing mechanism: framework is Snare or Scaffold depending on transparency. Classification shifts based on revealed mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_mechanism, empirical, 'Whether frameworks coordinate individual decisions or mask systemic rationing').

omega_variable(
    family_labor_extraction,
    'Does the framework extract emotional and decision-making labor from families as a cost reduction strategy, or does it genuinely coordinate necessary decisions that families would need to make regardless?',
    'Ethnographic analysis of family conference content: proportion of time spent on clarifying patient wishes vs. explaining institutional constraints; analysis of whether family labor (emotional processing, institutional legitimation, care coordination) reduces institutional staffing requirements; comparison of family burden (hours of meetings, documentation, decision-making) across frameworks and across resource-constrained vs. well-staffed institutions',
    'If extraction of family labor is structural: increases measured suppression and identifies family decision-maker as victim rather than participant. If coordination is genuine: family participation is agent role rather than exploited role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_labor_extraction, empirical, 'Whether framework extracts family emotional and decision labor').

omega_variable(
    advance_directive_efficacy,
    'Do advance directives and patient self-determination laws actually shift decision authority from institutions to patients/families, or do they perform autonomy while institutional actors retain de facto decision control?',
    'Comparison of actual clinical decisions vs. documented patient wishes; analysis of whether directives are overridden by medical judgment and at what frequency; assessment of whether patient preference or institutional protocol predicts actual treatment path; longitudinal follow-up of patients with vs. without advance directives',
    'If directives effectively shift authority: frameworks validate the Scaffold perspective — genuine sunset toward patient autonomy. If directives are performative: frameworks validate the Piton perspective — theater without functional shift in authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(advance_directive_efficacy, empirical, 'Whether advance directives functionally shift decision authority').

omega_variable(
    cultural_and_class_variation,
    'Do frameworks impose a standardized (Western, individualist, middle-class) autonomy model that erases cultural variation in end-of-life decision-making and creates extractive conformity demands?',
    'Ethnographic comparison across cultural contexts: analysis of frameworks in settings with different decision traditions (collective vs. individual, family-centered vs. patient-centered, spiritually-guided vs. medically-guided); assessment of whether frameworks accommodate or suppress cultural variation; comparison of cultural communities'' satisfaction with end-of-life outcomes under standardized frameworks vs. culturally-adapted frameworks',
    'If frameworks erase cultural variation: adds suppression (0.68) by enforcing conformity, identifies resource-constrained and culturally-distinct populations as additional victims. If frameworks accommodate variation: reduces suppression and changes classification for those populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_and_class_variation, conceptual, 'Whether frameworks impose Western autonomy model and erase cultural variation').

omega_variable(
    identity_lock_in_role_specialization,
    'Do clinicians become identity-locked into institutional end-of-life protocols such that they cannot perceive or advocate for alternatives, even when evidence supports change?',
    'Analysis of clinician perspectives: interviews with experienced ICU/palliative care physicians on whether they feel constrained by existing protocols and whether they can articulate alternative approaches; assessment of whether clinicians'' professional identity is fused with specific end-of-life decision frameworks; historical analysis of adoption of new protocols (e.g., earlier palliative care integration) and barriers to change from within institutions',
    'If identity lock exists: adds to institutional extractiveness by making system self-reinforcing through professional identity capture. Indicates that even well-intentioned institutional actors cannot perceive or implement changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_role_specialization, empirical, 'Whether clinicians are identity-locked into specific end-of-life protocols').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_care_frameworks, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eolc_tr_t0, end_of_life_care_frameworks, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eolc_tr_t10, end_of_life_care_frameworks, theater_ratio, 10, 0.58).
narrative_ontology:measurement(eolc_tr_t20, end_of_life_care_frameworks, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(eolc_be_t0, end_of_life_care_frameworks, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eolc_be_t10, end_of_life_care_frameworks, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(eolc_be_t20, end_of_life_care_frameworks, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_care_frameworks, resource_allocation).
narrative_ontology:affects_constraint(end_of_life_care_frameworks, informed_consent_paradox).
narrative_ontology:affects_constraint(end_of_life_care_frameworks, medical_authority_concentration).
narrative_ontology:affects_constraint(end_of_life_care_frameworks, advance_directive_enforcement).

% DUAL FORMULATION NOTE:
% End-of-life care frameworks decompose into multiple structurally distinct constraints with different ε values: (1) informed_consent_paradox (ε≈0.45, Tangled Rope) addresses whether genuine informed consent is possible under conditions of terminal illness, cognitive decline, and emotional crisis; (2) medical_authority_concentration (ε≈0.62, Snare) addresses how frameworks concentrate prognostic information and decision authority in clinical hands; (3) advance_directive_enforcement (ε≈0.35, Scaffold) addresses whether frameworks successfully transition authority from institutions to patients. These three stories share institutional mechanisms but have distinct ε values and temporal trajectories. The present story captures the overarching constraint across all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_care_frameworks, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
