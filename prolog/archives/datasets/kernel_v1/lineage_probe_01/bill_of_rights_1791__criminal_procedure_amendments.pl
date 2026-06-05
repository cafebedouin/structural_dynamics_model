% ============================================================================
% CONSTRAINT STORY: bill_of_rights_1791__criminal_procedure_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bill_of_rights_1791__criminal_procedure_amendments, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bill_of_rights_1791__criminal_procedure_amendments
 *   human_readable: Fourth through Eighth Amendments: Criminal Procedure Protections (1791 Reading)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Fourth through Eighth Amendments constitute a reading of the
 *   bill_of_rights_1791 kernel that prioritizes criminal procedure
 *   protections: warrants limiting search and seizure, jury trial ensuring
 *   peer judgment, right to counsel balancing prosecutorial resources,
 *   proportionality bounding punishment severity. This reading advances the
 *   thesis that legitimate state power is procedurally constrained — the
 *   state cannot simply investigate, charge, and punish; it must obtain
 *   warrants before intrusion, submit accusations to jury judgment, provide
 *   the accused counsel, and tailor punishment to the crime. The constraint
 *   exhibits the perspectival structure of contested kernel readings:
 *   beneficiaries (the accused, secure householders) see genuine protection;
 *   victims (prosecutorial efficiency, investigative power) experience
 *   constraint; institutional actors (courts, prosecutors) navigate mixed
 *   coordination and extraction; the analytical observer risks naturalizing
 *   this reading as immutable constitutional law when it is actually a
 *   contingent interpretation of the kernel that competes with sibling
 *   readings (expression/conscience, reserved powers, security/arms). The
 *   theater ratio (0.55) reflects that criminal procedure is partly genuine
 *   constraint (warrants do limit investigation, counsel does mount defense)
 *   and partly performative ritual (warrant doctrine has expanded categorical
 *   exceptions, jury instructions can override jury judgment, proportionality
 *   standards defer to appellate judges). The suppression measurement rising
 *   from 0.35 to 0.48 over 100 years indicates that the state's investigative
 *   apparatus has developed increasingly sophisticated methods to comply
 *   formally with procedure while minimizing constraint force (parallel
 *   construction, third-party doctrine, digital surveillance categories) —
 *   the suppression requirement has intensified as the state works around the
 *   amendments' bounds.
 *
 * KEY AGENTS:
 *   - Accused persons (powerless/trapped): Primary beneficiary of procedure protections; experiences maximum extraction without enforcement.
 *   - Secure householders (moderate/constrained): Beneficiary of Fourth Amendment warrant requirement; bears cost of reduced police access to homes.
 *   - Prosecutors and law enforcement (institutional/arbitrage and institutional/constrained): Mixed relationship — benefit from legitimacy the amendments provide, bear costs of procedure compliance; state investigative apparatus (arbitrage) can evolve workarounds; individual prosecutors (constrained) must navigate procedure bounds.
 *   - Civil liberties advocates (organized/constrained): Coordinate enforcement of amendments; bear extraction cost of constant litigation to prevent state expansion.
 *   - Judiciary (institutional/arbitrage): Gate-keepers interpreting and enforcing amendments; can exit through deference; maintain institutional interest in warrant review role.
 *   - Federal law enforcement apparatus (institutional/arbitrage): Operational reality of warrant, counsel, jury constraints is theater overlaying sophisticated investigative techniques that comply formally while minimizing constraint force.
 *   - Analytical observer (analytical/analytical): Civilizational perspective risks naturalizing the criminal procedure reading as immutable constitutional law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bill_of_rights_1791__criminal_procedure_amendments, 0.38).
domain_priors:suppression_score(bill_of_rights_1791__criminal_procedure_amendments, 0.48).
domain_priors:theater_ratio(bill_of_rights_1791__criminal_procedure_amendments, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bill_of_rights_1791__criminal_procedure_amendments, extractiveness, 0.38).
narrative_ontology:constraint_metric(bill_of_rights_1791__criminal_procedure_amendments, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bill_of_rights_1791__criminal_procedure_amendments, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bill_of_rights_1791__criminal_procedure_amendments, tangled_rope).
narrative_ontology:human_readable(bill_of_rights_1791__criminal_procedure_amendments, "Fourth through Eighth Amendments: Criminal Procedure Protections (1791 Reading)").
narrative_ontology:topic_domain(bill_of_rights_1791__criminal_procedure_amendments, "political/legal/constitutional").

domain_priors:requires_active_enforcement(bill_of_rights_1791__criminal_procedure_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bill_of_rights_1791__criminal_procedure_amendments, '106bd650-8914-408b-a8ee-45ec6e6834ba').
narrative_ontology:cs_kernel_codification('106bd650-8914-408b-a8ee-45ec6e6834ba', formalized).
narrative_ontology:cs_authority_grounding('106bd650-8914-408b-a8ee-45ec6e6834ba', lineage).
narrative_ontology:cs_interpretation_layer_present('106bd650-8914-408b-a8ee-45ec6e6834ba').
narrative_ontology:cs_reading_relation('106bd650-8914-408b-a8ee-45ec6e6834ba', bill_of_rights_1791__expression_conscience_amendments, coexists_with).
narrative_ontology:cs_reading_relation('106bd650-8914-408b-a8ee-45ec6e6834ba', bill_of_rights_1791__reserved_powers_amendments, influences).
narrative_ontology:cs_reading_relation('106bd650-8914-408b-a8ee-45ec6e6834ba', bill_of_rights_1791__security_arms_amendments, coexists_with).
narrative_ontology:cs_axiom('106bd650-8914-408b-a8ee-45ec6e6834ba', foundational, procedure_bounds_state_power).
narrative_ontology:cs_axiom_status(procedure_bounds_state_power, holdable).
narrative_ontology:cs_axiom_grounding('106bd650-8914-408b-a8ee-45ec6e6834ba', procedure_bounds_state_power, deontological).
narrative_ontology:cs_axiom('106bd650-8914-408b-a8ee-45ec6e6834ba', foundational, individual_dignity_in_accusation).
narrative_ontology:cs_axiom_status(individual_dignity_in_accusation, holdable).
narrative_ontology:cs_axiom_grounding('106bd650-8914-408b-a8ee-45ec6e6834ba', individual_dignity_in_accusation, deontological).
narrative_ontology:cs_reference_frame('106bd650-8914-408b-a8ee-45ec6e6834ba', constitutional_procedure_framework).
narrative_ontology:cs_drift_state('106bd650-8914-408b-a8ee-45ec6e6834ba', contemporary_surveillance_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('106bd650-8914-408b-a8ee-45ec6e6834ba', '').
narrative_ontology:cs_kernel_id(bill_of_rights_1791__criminal_procedure_amendments, bill_of_rights_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__criminal_procedure_amendments, accused_persons).
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__criminal_procedure_amendments, secure_householders).
narrative_ontology:constraint_victim(bill_of_rights_1791__criminal_procedure_amendments, prosecutorial_efficiency).
narrative_ontology:constraint_victim(bill_of_rights_1791__criminal_procedure_amendments, state_investigative_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCUSED WITHOUT RESOURCES (SNARE) — A person accused of crime without wealth for private counsel faces the full coercive machinery of the state. Even with the Sixth Amendment right to counsel, the constraint operates as extraction: lengthy proceedings, limited investigative resources, exposure to self-incrimination risks before counsel is assigned. Maximum structural exposure to state power. The amendments provide procedural bounds but not material equality. Trapped exit option — cannot abandon the constitutional process without accepting conviction.
constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ACCUSED WITH COUNSEL (ROPE) — A person with resources to retain counsel experiences the amendments as coordination mechanism: warrants establish procedural boundaries, jury trial provides peer judgment, counsel ensures the accused can contest state claims. The constraint coordinates the state's investigative power against individual defense capacity. Significant cost (lost time, legal fees, conviction risk) but genuine negotiation capacity and some agency. Constrained exit — cannot abandon the process, but can exercise meaningful defense rights.
constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL LIBERTIES COALITION (TANGLED ROPE) — Organized advocates (ACLU, defense bar associations) see the amendments as hybrid: genuine coordination function (establishing procedural boundaries that prevent arbitrary state action) AND asymmetric extraction (the state retains investigative initiative, resources, and power to charge; the accused responds defensively). The coalition's coordination function is to enforce the amendments' protections; their extraction experience is that enforcement requires constant litigation and interpretation to prevent state expansion. Constrained exit — cannot exit the constitutional framework without abandoning individual rights protection.
constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY (SCAFFOLD) — Courts see the amendments as procedural scaffolding: their role is to interpret and enforce warrant, jury, counsel, and proportionality requirements. The institutional perspective experiences the amendments as a coordination mechanism with a built-in sunset clause: as judicial interpretation matures and state compliance improves, the need for aggressive judicial oversight decreases. However, the judiciary has institutional interest in maintaining its gatekeeping role (reviewing warrants, overseeing juries, assessing proportionality), which creates extraction pressure. Low effective extractiveness because the judiciary can exit through deference to state practices (arbitrage option). Theater present but moderate — judicial review of warrants is genuine process, not purely performative.
constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROSECUTION (TANGLED ROPE) — State prosecutors experience the amendments as coordinating constraints AND extractive barriers. Coordination function: the amendments establish predictable procedures that reduce arbitrary violence and enable legitimate prosecution. Extraction: the amendments require investiture of resources (obtaining warrants, providing discovery, affording counsel) and limit investigative shortcuts. Prosecutors benefit from the legitimacy the amendments provide (public confidence in convictions, reduced appeal reversals) while bearing costs (slower investigations, disclosure requirements, proportionality constraints). Constrained exit — cannot abandon constitutional procedures without delegitimizing prosecution itself.
constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL LAW ENFORCEMENT APPARATUS (PITON) — The FBI, federal prosecutors, and investigative agencies see the amendments as procedural theater overlaying operational reality. The amendments constrain techniques (warrants, interrogation limits) but law enforcement has evolved sophisticated methods to comply formally while minimizing constraint force (broad warrant language, qualified immunity, cooperating informants, parallel construction). The constraint persists through institutional inertia and constitutional deference, not because it meaningfully limits federal investigative capacity. Theater ratio high — the apparatus experiences warrant review as a formal gate, not a genuine constraint on investigation strategy.
constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some process between accusation and punishment is logically necessary — without procedural boundaries, state power becomes purely coercive and self-legitimating. The amendments instantiate the irreducible structure: warrant (limiting search scope), jury (introducing peer judgment), counsel (balancing investigative asymmetry), proportionality (bounding punishment to crime). These appear as immutable constitutional law — unchangeable within the American legal framework. However, the structural data contradicts the mountain classification — the amendments are a reading of a contested kernel, and their force depends on interpretation and enforcement. The engine will identify this as a false summit: naturalization of a contingent institutional arrangement (the 1791 reading of criminal procedure protections) as immutable law.
constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bill_of_rights_1791__criminal_procedure_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bill_of_rights_1791__criminal_procedure_amendments, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(bill_of_rights_1791__criminal_procedure_amendments, TR),
    TR >= 0.70.

:- end_tests(bill_of_rights_1791__criminal_procedure_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The amendments establish genuine procedural constraints (warrants, jury, counsel, proportionality) that limit state power, but the constraints are not total — state investigative and prosecutorial apparatus retains substantial power and initiative. The accused faces significant cost (loss of liberty, trial risk, conviction), but the amendments provide meaningful procedural bounds that distinguish constitutional prosecution from arbitrary violence. The extractiveness value reflects that the constraint mixes coordination (establishing predictable procedure) with asymmetric extraction (state retains power, accused responds defensively). Suppression (0.48): Moderate-high. The amendments suppress certain investigative shortcuts (warrantless search, coerced confession, jury exclusion, excessive punishment), but suppression is incomplete — doctrine has evolved categorical exceptions (business records, third-party doctrine, qualified immunity), and enforcement depends on accused's capacity to litigate suppression motions. Theater ratio (0.55): Moderate. Criminal procedure includes genuine constraint (warrant requirement does limit some investigation, jury trial does introduce peer judgment) and performative ritual (warrant doctrine has expanded exceptions, jury instructions can constrain jury judgment, proportionality is deferential). The theater ratio rises over time as the state develops compliance mechanisms that satisfy procedure formally while minimizing constraint force (parallel construction, digital surveillance categories). This reflects the classic pattern: a constraint designed to limit power is gradually encased in exceptions and doctrinal workarounds that preserve the appearance of constraint while expanding actual power.
 *
 * PERSPECTIVAL GAP:
 *   The criminal procedure amendments demonstrate the canonical perspectival gap between victim and beneficiary, powerless and institutional actors. The accused without resources sees pure extraction (Snare) — the state's coercive power operates against them despite procedure that is supposed to protect. The accused with counsel sees coordination (Rope) — procedure enables defense and negotiation. The civil liberties coalition sees tangled rope (genuine protection function plus enforcement burden). The judiciary sees scaffold (temporary procedures that can be relaxed as state compliance improves). The prosecution sees tangled rope (genuine benefits from legitimacy plus real costs of procedure). Federal law enforcement sees piton (procedure as theater overlaying operational investigative power). The analytical observer risks seeing mountain (immutable constitutional law) when the structural data reveals a false summit — the amendments' force depends on interpretation, enforcement, and state compliance, all of which are contingent and subject to doctrine drift. The perspectival gap exposes that the 'immutable' reading naturalizes what is actually a contingent institutional arrangement vulnerable to doctrinal erosion and state workarounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each actor's structural position relative to the constraint. Beneficiaries (accused, secure householders) with trapped/constrained exit options experience lower d values (benefits exceed costs). Victims (prosecution, state investigative power) with constrained exit experience higher d values (costs exceed benefits, but not absolute). Institutional actors navigate mixed d values: courts have low d (arbitrage exit, maintained institutional interest in procedure oversight); prosecutors have moderate d (constrained exit, mixed coordination and extraction). The analytical observer has high d (analytical exit, analytical objectivity obscures that the 'natural law' framing benefits state power legitimacy). The engines' false summit detector identifies the mountain perspective as naturalizing a contingent reading, revealing that the analytical observer's d value masks dependence on the criminal procedure interpretation of the kernel.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: The mandatrophy resolves by recognizing that the criminal procedure amendments are ONE reading of a contested kernel, not universal constitutional law. The constraint exhibits mixed classification precisely because the reading is contested. The beneficiary's rope, the victim's snare, the institutional tangled ropes, the observer's mountain — these are not contradictions but perspectival snapshots of a single contingent reading that competes with sibling readings (expression, reserved powers, security/arms). The analytical observer's mountain is a false summit produced by naturalizing the criminal procedure reading as immutable when the reading's force depends on interpretation and enforcement. The mandatrophy resolves when we recognize that the constraint's type depends on which reading of the kernel is authorized and enforced — and that authorization is itself the locus of political struggle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warrant_scope_under_determination,
    'Does the Fourth Amendment''s warrant requirement constitute a genuine procedural constraint on state investigation, or has categorical expansion (business records, third-party doctrine, digital surveillance) rendered it largely performative?',
    'Empirical analysis of warrant denial rates by jurisdiction; comparison of investigation outcomes under warrant vs. warrantless protocols (parallel investigations); longitudinal tracking of Fourth Amendment suppression motion success rates',
    'If warrant is genuine constraint: Fourth Amendment validates tangled_rope classification (mixing coordination and extraction). If warrant is performative: Fourth Amendment classification drifts toward piton (theater ratio rises, extractiveness unchanged).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(warrant_scope_under_determination, empirical, 'Whether the warrant requirement functions as a genuine procedural constraint or has become largely performative').

omega_variable(
    jury_nullification_and_voir_dire,
    'Does the jury trial requirement (Sixth Amendment) function as a genuine check on prosecutorial power, or has voir dire, jury instructions, and guilt-beyond-reasonable-doubt standards neutered the jury''s capacity to assess punishment proportionality?',
    'Comparative analysis of jury acquittal rates vs. judge acquittal rates; correlation between juror comprehension of nullification rights and verdict outcomes; historical analysis of jury nullification in drug and property cases',
    'If jury is genuine check: jury trial validates coordination function. If jury is formalized but constrained: jury classification shifts toward piton (theater without constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jury_nullification_and_voir_dire, empirical, 'Whether jury trial functions as genuine check on prosecutorial power').

omega_variable(
    counsel_resource_asymmetry,
    'Does the Sixth Amendment right to counsel meaningfully balance the state''s investigative and prosecutorial resources, or does public defender underfunding and indigent counsel assignment systematically disadvantage accused persons without means?',
    'Longitudinal data on public defender caseloads and conviction rates; comparison of outcomes for indigent vs. retained counsel; analysis of plea-vs.-trial rates by counsel funding source',
    'If counsel provides genuine balance: right to counsel validates coordination function. If counsel is underfunded formalism: right to counsel classification drifts toward snare (structurally benefiting the accused in theory but not in practice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counsel_resource_asymmetry, empirical, 'Whether the right to counsel meaningfully balances state investigative resources').

omega_variable(
    proportionality_standard_determinacy,
    'Does the Eighth Amendment''s proportionality requirement (cruel and unusual punishment) constitute a determinate constraint on sentencing severity, or does judicial discretion and appellate deference leave it substantively under-determined?',
    'Analysis of proportionality jurisprudence (Solem factors, effective proportionality challenges); comparison of sentencing ranges across similar crimes and jurisdictions; tracking of successful vs. failed proportionality challenges over time',
    'If proportionality is determinate: Eighth Amendment provides genuine constraint. If proportionality is indeterminate: classification shifts toward piton (judicial theater without substantive constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_standard_determinacy, empirical, 'Whether the Eighth Amendment''s proportionality requirement is determinate or subject to appellate deference').

omega_variable(
    reading_vs_natural_law_ambiguity,
    'Are the criminal procedure protections of the Fourth through Eighth Amendments universal features of legitimate state power (natural law), or are they one contested reading of a kernel that could be read differently (e.g., prioritizing security/efficiency over individual process rights)?',
    'Comparative constitutional analysis: examination of non-American systems (civil law countries, authoritarian systems) and their criminal procedure justifications; historical analysis of alternative readings of the 1791 kernel; evaluation of whether the criminal procedure reading is internally coherent or depends on suppression of alternative framings',
    'If natural law: mountain classification is correct. If reading: mountain is false summit; actual classification depends on perspective and interpretation layer; constraint is Tangled Rope or Snare from most perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_natural_law_ambiguity, conceptual, 'Whether criminal procedure amendments are universal natural law or contingent reading of a contested kernel').

omega_variable(
    kernel_reading_content,
    'This constraint is one reading of the kernel bill_of_rights_1791. Sibling readings include expression_conscience_amendments (First Amendment focus), reserved_powers_amendments (Ninth/Tenth focus), and security_arms_amendments (Second/Third focus). Does the criminal procedure reading foreclose, coexist with, or influence the sibling readings?',
    'Textual and structural analysis: examination of whether the criminal procedure emphasis (Fourth-Eighth) and the sibling readings'' emphases (First Amendment, Ninth/Tenth, Second/Third) can be held within a single coherent constitutional framework without logical contradiction; historical analysis of how courts have managed tensions between these readings',
    'If readings foreclose each other: the kernel is internally fractured and cannot be unified; constraint classification must decompose into separate constraints per reading. If readings coexist: all readings are simultaneously valid from different parties'' perspectives; the kernel represents a stable political compromise. If reading influences siblings: the criminal procedure emphasis creates structural pressure on how other amendments are interpreted (e.g., the Ninth/Tenth as residuary clauses supporting due process).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_content, conceptual, 'Whether the criminal procedure reading forecloses, coexists with, or influences sibling readings of the bill_of_rights_1791 kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bill_of_rights_1791__criminal_procedure_amendments, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bill_tr_t0, bill_of_rights_1791__criminal_procedure_amendments, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bill_tr_t50, bill_of_rights_1791__criminal_procedure_amendments, theater_ratio, 50, 0.48).
narrative_ontology:measurement(bill_tr_t100, bill_of_rights_1791__criminal_procedure_amendments, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(bill_be_t0, bill_of_rights_1791__criminal_procedure_amendments, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bill_be_t50, bill_of_rights_1791__criminal_procedure_amendments, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(bill_be_t100, bill_of_rights_1791__criminal_procedure_amendments, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bill_su_t0, bill_of_rights_1791__criminal_procedure_amendments, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bill_su_t50, bill_of_rights_1791__criminal_procedure_amendments, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(bill_su_t100, bill_of_rights_1791__criminal_procedure_amendments, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bill_of_rights_1791__criminal_procedure_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(bill_of_rights_1791__criminal_procedure_amendments, bill_of_rights_1791__expression_conscience_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__criminal_procedure_amendments, bill_of_rights_1791__reserved_powers_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__criminal_procedure_amendments, bill_of_rights_1791__security_arms_amendments).

% DUAL FORMULATION NOTE:
% The criminal_procedure_amendments constraint is one reading of the bill_of_rights_1791 kernel. The kernel is contested across four sibling readings, each with different extractiveness values, beneficiary/victim structures, and classification types. This constraint (criminal procedure focus) has extractiveness 0.38 and claims tangled_rope. The expression_conscience reading is a separate constraint with different ε; the reserved_powers and security_arms readings are separate constraints. All four readings coexist within American constitutional law but represent different institutional prioritizations and interpretive emphases. See the kernel_context in commentary for the full reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bill_of_rights_1791__criminal_procedure_amendments, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
