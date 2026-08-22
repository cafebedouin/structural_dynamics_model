% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause: Originalist Narrow Reading
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist narrow reading of the
 *   Commerce Clause: federal authority is confined to regulation of commerce
 *   that crosses state borders and the instrumentalities of interstate
 *   movement. Under this reading, a state retains police power to regulate
 *   economic activity occurring entirely within its territory, even if that
 *   activity has aggregate effects on interstate commerce. The reading
 *   benefits state governments (who retain regulatory autonomy) and
 *   disadvantages actors seeking uniform national standards (environmental
 *   coalitions, labor advocates, national market advocates). This is ONE
 *   reading of a contested kernel; the sibling readings
 *   (expansive_federal_reading, substantial_effects_limited_reading)
 *   instantiate different constraints from the same constitutional text, each
 *   with different ε values, beneficiary/victim structures, and persistence
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - state_governments: preserve intrastate regulatory autonomy
 *   - originalist_judiciary: interpret and enforce the narrow boundary
 *   - national_uniform_standards_advocates: bear costs of regulatory fragmentation
 *   - environmental_protection_coalitions: lose federal externality management authority
 *   - congress_expansionist_faction: excluded from broader regulatory scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.62).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.71).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause: Originalist Narrow Reading").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, 'bcb0c57b-ad4d-4f22-b3aa-f6183214aa80').
narrative_ontology:cs_kernel_codification('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', fixed_text).
narrative_ontology:cs_authority_grounding('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', lineage).
narrative_ontology:cs_interpretation_layer_present('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80').
narrative_ontology:cs_reading_relation('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', foundational, commerce_clause_textual_boundary_is_normative).
narrative_ontology:cs_axiom_status(commerce_clause_textual_boundary_is_normative, holdable).
narrative_ontology:cs_axiom_grounding('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', commerce_clause_textual_boundary_is_normative, deontological).
narrative_ontology:cs_axiom('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', foundational, enumerated_powers_are_exclusive).
narrative_ontology:cs_axiom_status(enumerated_powers_are_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', enumerated_powers_are_exclusive, deontological).
narrative_ontology:cs_axiom('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', secondary, federalism_limits_executive_consolidation).
narrative_ontology:cs_axiom_status(federalism_limits_executive_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', federalism_limits_executive_consolidation, instrumental).
narrative_ontology:cs_reference_frame('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', constitutional_text_as_binding_limit).
narrative_ontology:cs_drift_state('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', contemporary_economic_integration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bcb0c57b-ad4d-4f22-b3aa-f6183214aa80', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, localist_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_uniform_standards_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, environmental_protection_coalitions).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, labor_regulation_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, multinational_corporations).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, multinational_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain regulatory authority over economic activity that occurs entirely within state borders under this reading. Can set labor standards, environmental rules, consumer protections, and licensing regimes for intrastate commerce without federal override. Preserve tax and regulatory autonomy as a constitutional matter.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Argue that limiting federal commerce power to border-crossing activity preserves democratic accountability: local regulation remains closer to affected voters. Support this reading on federalism grounds as a constraint on centralized authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, localist_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of this reading's narrowed federal authority: cannot establish uniform national labor standards, environmental rules, or consumer protection regimes for activities that courts classify as intrastate. Face a patchwork of state rules that raise compliance costs and fragment markets.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_uniform_standards_advocates, payer,
    organized, biographical, constrained, national).

% Cannot rely on federal commerce authority to regulate interstate pollution, carbon emissions, or resource extraction with interstate spillovers. Under this reading, a state can regulate only activity occurring within its borders, even if that activity generates out-of-state environmental costs. Must negotiate interstate compacts or seek state-by-state regulatory adoption.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, environmental_protection_coalitions, payer,
    organized, biographical, constrained, global).

% Lose federal authority to set national minimum wages, workplace safety, and collective bargaining rules for firms engaged in interstate commerce under a narrow reading. States can regulate their own intrastate employment, but firms can locate operations across state lines to avoid higher-cost labor regimes.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, labor_regulation_proponents, payer,
    organized, biographical, constrained, national).

% Interprets the Commerce Clause text to mean commerce crossing state borders and the instrumentalities that move goods across borders. Sets the boundary of permissible federal regulation and enforces it through judicial review of federal statutes. Maintains the canon of enumerated powers as a structural limit.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Would prefer broader federal commerce authority to regulate national markets and set uniform standards. Is constrained by this reading's judicial enforcement; statutes they pass are struck down if courts find the regulated activity to be intrastate or lacking sufficient interstate nexus.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, congress_expansionist_faction, excluded,
    institutional, generational, trapped, national).

% Can navigate the constraint by locating supply chains across state lines to trigger interstate commerce classification (beneficiary position), but face regulatory uncertainty: whether a particular facility or transaction is classified as interstate or intrastate can shift with judicial interpretation, creating compliance cost (payer position). Benefit from the fragmentation insofar as they can arbitrage between state regimes.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, multinational_corporations, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a structural limit on federal authority by confining the commerce power to transactions that physically cross state borders or use instrumentalities of interstate movement. Coordinates the federalism arrangement by fixing the boundary between federal and state regulatory domains as a constitutional matter.
% TRANSFER_FUNCTION: Transfers regulatory authority from the federal government to state governments for all economic activity classified as intrastate. Transfers the burden of regulatory fragmentation and compliance complexity to national standards advocates and firms seeking uniform rules.
% ABSENT_VOICES: Congress in expansionist mode, multinational corporations preferring uniform national rules, and interstate externality victims (polluters in state A harming state B residents) would object to the narrowed federal authority. They are structurally excluded: this reading constrains what Congress can do and what courts will enforce, regardless of legislative intent.
% DISAPPEARANCE_RATIONALE: If this originalist reading of the Commerce Clause disappeared (replaced by an expansive reading), the federal government could regulate intrastate economic activity with substantial aggregate effects on interstate commerce; Congress could enact uniform national labor, environmental, and consumer protection standards; the regulatory landscape would consolidate. The state governments would lose substantial regulatory authority; the federal regulatory apparatus would expand. Markets would become more uniform; firms would face different compliance costs; environmental and labor regulations would harmonize nationally rather than fragment by state.
% FOUNDING_PROBLEM: The Articles of Confederation left commerce regulation fractured across states, enabling trade wars and tariff barriers that fragmented the national market. The Commerce Clause was written to authorize federal regulation of interstate trade and remove state barriers to a unified national market.
% FOUNDING_PROBLEM_CORROBORATION: Originalist interpreters argue the founding problem is fixed by a narrow reading: federal power over border-crossing commerce prevents state tariffs and barriers; that achieved the founding goal without authorizing federal regulation of intrastate activity. Expansive readers and economic historians argue the founding problem evolved: the modern national economy cannot function without federal authority to regulate activities with interstate effects, a problem the Framers could not have foreseen. Legislative testimony and economic analysis from non-originalist sources affirm the modern complexity; originalist legal scholarship affirms the textual boundary.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) is moderate-to-high because the constraint transfers regulatory authority from federal to state domains, benefiting state governments and localist advocates while imposing costs on national coordination actors. The measurement series shows extraction rising from 0.48 to 0.62 over the interval, reflecting gradual acceptance of originalist reasoning in constitutional law and increasing awareness of its costs (regulatory fragmentation, failure to address interstate externalities). Suppression (0.71) is substantial because maintaining the narrow boundary requires active judicial enforcement: every expansive federal statute is reviewed for whether the regulated activity is truly interstate or merely has interstate effects, and statutes fail that test. Theater ratio (0.38) rises modestly, reflecting an increasing rhetorical gap between the 'federalism principle' justification and the practical consequence (gutting federal environmental and labor authority). The constraint's extractiveness plateaus after t=25, suggesting the initial shock of adopting this reading has been absorbed; suppression stabilizes as precedent solidifies.
 *
 * PERSPECTIVAL GAP:
 *   State governments experience this constraint as coordination enabling their constitutional role; the originalist judiciary experiences it as faithful interpretation of enumerated powers. National standards advocates experience it as extraction: their preferred regulatory authority is denied, and they must negotiate patchwork state rules. Environmental coalitions face trapped exit (cannot relocate interstate pollution across state boundaries via federal authority). The engine computes these per-seat classifications from the structural data: state governments derive low d (beneficiary position, analytical exit), while national coalition actors derive high d (payer position, constrained exit).
 *
 * DIRECTIONALITY LOGIC:
 *   State governments benefit by retaining regulatory authority; their directionality is low (beneficiary end). National standards advocates pay by losing federal coordinating authority; their directionality is high (target end). Environmental coalitions have the highest d because they bear asymmetric costs: interstate pollution externalities cannot be managed by a state acting alone, so the constraint forces them into a trapped position. Localist advocates benefit ideologically, so their d is low-to-moderate. The originalist judiciary sets the boundary; its d is analytical (observer position). Congress's excluded faction would prefer lower d but is constrained by judicial enforcement; modeling them as excluded rather than paying reflects that their preferred role is structurally barred, not merely costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trade wars and state tariffs fragmenting the national market) was genuinely solved by granting federal commerce authority. This reading preserves that grant while narrowing it. The mandatrophy test: does the constraint still solve the founding problem? YES — it prevents state tariffs and barriers on interstate commerce. Does it persist for reasons beyond solving that problem? YES — it also prevents federal regulation of intrastate activity, which the Framers likely did not anticipate or foresee as the use of the power. The constraint shows NO mandatrophy according to the strict definition (founding problem solved, constraint still needed), but it shows MISSION CREEP in reverse: the constraint was written to prevent state barriers and ended up also preventing federal coordination. The theater ratio increase (from 0.22 to 0.38) reflects this: originalist interpreters increasingly justify the constraint in terms of 'federalism principle' and 'enumerated powers canon' rather than the original anti-tariff function, a rhetorical shift upward in performance relative to function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrastate_vs_interstate_boundary_contestation,
    'Where is the boundary between intrastate and interstate activity? Is manufacturing within a state that is then sold interstate intrastate or interstate commerce?',
    'Case-by-case judicial determination creates precedent; the line drifts through doctrine. Alternative: legislative clarification via the taxing power, spending power, or statute-specific interstate nexus requirements.',
    'Every placement of the boundary reallocates regulatory authority between federal and state. Uncertainty about which side of the line an activity falls on creates compliance costs and regulatory arbitrage opportunities. A narrower definition of interstate favors state authority; a broader definition reduces this constraint''s extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intrastate_vs_interstate_boundary_contestation, empirical, 'The boundary between intrastate and interstate is contested and doctrine-dependent, not text-determinative.').

omega_variable(
    framers_intent_vs_modern_economy,
    'Did the Framers intend the Commerce Clause to regulate only border-crossing transactions, or did they expect federal authority to evolve with an increasingly integrated economy?',
    'Historical scholarship on the Framers'' understanding; comparison with how other enumerated powers (tax, necessary-and-proper) evolved. Alternative: no definitive resolution — the reading depends on normative commitments to originalism vs. living constitutionalism.',
    'If the Framers intended narrow scope, the originalist reading is faithful to constitutional design. If they foresaw federal power would need to expand, the reading is unduly constraining. This is a conceptual/preference boundary: the resolution depends on one''s theory of constitutional interpretation, not on empirical fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framers_intent_vs_modern_economy, conceptual, 'The alignment between framers'' intent and originalist scope is contested between interpretive schools.').

omega_variable(
    federalism_benefit_distribution,
    'Which states benefit from retaining intrastate regulatory authority under this reading? Do low-regulation states gain competitive advantage, or do all states gain equally?',
    'Empirical analysis of regulatory adoption patterns: do states converge toward a common floor (suggesting federal pressure), or do they diverge toward different regulatory levels (suggesting state autonomy)? Measurement of firm location decisions relative to state regulatory regimes.',
    'If benefit is distributed asymmetrically (e.g., low-regulation states gain competitive advantage), the constraint functions as extractive from higher-regulation states and their constituents. If benefits are symmetric, the federalism rationale is stronger. Asymmetric distribution would suggest false-summit dynamics: the constraint benefits a subset of state governments while harming others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_benefit_distribution, empirical, 'Whether the beneficiaries of state regulatory autonomy are all states equally or a subset that gains competitive advantage.').

omega_variable(
    reading_foreclosure_relationship_to_substantial_effects,
    'Does the originalist narrow reading FORECLOSE the substantial_effects_limited reading, or do they coexist as alternative interpretations?',
    'Doctrinal comparison: can a court adopt substantial-effects reasoning while respecting originalist textual limits, or does accepting substantial effects as a legitimate category of federal authority logically dissolve the narrow reading? Historical precedent shows both readings have been held by different judicial coalitions, suggesting coexistence is possible.',
    'If they foreclose, the kernel contest is bipolar (originalist vs. expansive, with substantial-effects as a transitional hybrid). If they coexist, the contest is tripolar. This affects how the engine models the constraint family and whether it computes foreclosure-based reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_relationship_to_substantial_effects, conceptual, 'Logical relationship between the originalist and substantial-effects readings in constitutional theory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__originalist_narrow_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comm_tr_t5, commerce_clause_text__originalist_narrow_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_text__originalist_narrow_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_text__originalist_narrow_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_text__originalist_narrow_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(comm_tr_t25, commerce_clause_text__originalist_narrow_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_text__originalist_narrow_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(comm_tr_t35, commerce_clause_text__originalist_narrow_reading, theater_ratio, 35, 0.38).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comm_be_t5, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(comm_be_t10, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(comm_be_t15, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(comm_be_t20, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(comm_be_t25, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(comm_be_t30, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(comm_be_t35, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t5, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(comm_su_t10, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(comm_su_t15, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(comm_su_t20, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(comm_su_t25, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(comm_su_t30, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(comm_su_t35, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-constraint family decomposing the contested Commerce Clause kernel. The originalist narrow reading instantiates one of three structurally distinct constraints from the same constitutional text. The three readings have different ε values (extractiveness), different beneficiary/victim structures, and different persistence mechanisms. Each should be modeled as a separate constraint story with linked network relationships. The 'contest' is NOT a measurement parameter within one constraint; it is the fact that three distinct constraints compete for authorization by the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
