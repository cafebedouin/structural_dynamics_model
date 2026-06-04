% ============================================================================
% CONSTRAINT STORY: us_constitution__bill_of_rights_1791
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution__bill_of_rights_1791, []).

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
 *   constraint_id: us_constitution__bill_of_rights_1791
 *   human_readable: The 1791 Bill of Rights: Authority Extracted as Price of Union
 *   domain: political/legal/constitutional_theory
 *
 * SUMMARY:
 *   The 1791 Bill of Rights reading frames the Constitution's authority as
 *   perfected only by the first ten amendments, which the ratifying public
 *   extracted as the price of union. This reading models a specific
 *   contestation within constitutional law: whether the Constitution's
 *   legitimacy derives from its structural enumeration of federal powers (the
 *   1787 text) or from its enumeration of individual rights against federal
 *   power (the 1791 amendments). The constraint exhibits the classic tangled
 *   rope signature: the Bill of Rights provides genuine coordination (defines
 *   the boundary between federal and individual authority with precision)
 *   while simultaneously extracting (creates an enumeration asymmetry where
 *   listed rights are protected and unlisted ones are vulnerable; applies
 *   only to federal government, leaving states free to suppress the same
 *   rights; provides the Federal Government with a coordinate standard that
 *   enables power while constraining it). The extractiveness dropped sharply
 *   from 1789 (when the unratified Constitution threatened unlimited federal
 *   power) to 1791 (when enumerated rights were established). Over the long
 *   term (toward 1891 and beyond), the incorporation doctrine and Ninth
 *   Amendment jurisprudence have moved the constraint toward pure
 *   coordination (rope), though this movement may represent a
 *   reinterpretation of the 1791 reading rather than its internal
 *   development.
 *
 * KEY AGENTS:
 *   - The Ratifying Public: Primary beneficiary (moderate/constrained) and victim of the bargain — extracted enumerated rights in return for union; initially bears extraction via enumeration asymmetry (unenumerated rights vulnerable)
 *   - The Federal Government: Institutional beneficiary (institutional/arbitrage) — coordinates its own action within enumerated limits; gains legitimacy from respecting the boundary
 *   - The States: Secondary affected actor (moderate/constrained) — gain from preservation of state sovereignty against federal encroachment but lose in the sovereignty competition as federal rights enumerate
 *   - Later Rights Bearers: Extended beneficiary (powerless/trapped initially, becoming organized/constrained over time) — benefit from enumerated rights established in 1791, but remain vulnerable to state suppression and unenumerated rights gaps
 *   - The Judicial System: Institutional interpreter (institutional/arbitrage, later analytical) — enforces the boundary through litigation (Marbury v Madison, incorporation doctrine); gains authority through dispute resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution__bill_of_rights_1791, 0.38).
domain_priors:suppression_score(us_constitution__bill_of_rights_1791, 0.48).
domain_priors:theater_ratio(us_constitution__bill_of_rights_1791, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution__bill_of_rights_1791, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution__bill_of_rights_1791, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(us_constitution__bill_of_rights_1791, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution__bill_of_rights_1791, tangled_rope).
narrative_ontology:human_readable(us_constitution__bill_of_rights_1791, "The 1791 Bill of Rights: Authority Extracted as Price of Union").
narrative_ontology:topic_domain(us_constitution__bill_of_rights_1791, "political/legal/constitutional_theory").

domain_priors:requires_active_enforcement(us_constitution__bill_of_rights_1791).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution__bill_of_rights_1791, '29dea10a-fdb2-47be-a887-e7e6d7acfd6a').
narrative_ontology:cs_kernel_codification('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', formalized).
narrative_ontology:cs_authority_grounding('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', lineage).
narrative_ontology:cs_interpretation_layer_present('29dea10a-fdb2-47be-a887-e7e6d7acfd6a').
narrative_ontology:cs_reading_relation('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', us_constitution__original_constitution_1787, coexists_with).
narrative_ontology:cs_reading_relation('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', us_constitution__later_amendment_eras, influences).
narrative_ontology:cs_reading_relation('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', us_constitution__failed_amendments, coexists_with).
narrative_ontology:cs_reading_relation('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', us_constitution__pre_constitutional_frameworks, influences).
narrative_ontology:cs_axiom('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', foundational, enumerated_rights_constitute_federal_authority).
narrative_ontology:cs_axiom_status(enumerated_rights_constitute_federal_authority, holdable).
narrative_ontology:cs_axiom_grounding('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', enumerated_rights_constitute_federal_authority, deontological).
narrative_ontology:cs_axiom('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', secondary, enumeration_creates_asymmetric_protection).
narrative_ontology:cs_axiom_status(enumeration_creates_asymmetric_protection, holdable).
narrative_ontology:cs_axiom_grounding('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', enumeration_creates_asymmetric_protection, conventional).
narrative_ontology:cs_reference_frame('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', enumerated_individual_rights_as_federal_legitimacy_ground).
narrative_ontology:cs_drift_state('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', contemporary_judicial_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('29dea10a-fdb2-47be-a887-e7e6d7acfd6a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(us_constitution__bill_of_rights_1791, us_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution__bill_of_rights_1791, ratifying_individuals).
narrative_ontology:constraint_beneficiary(us_constitution__bill_of_rights_1791, later_rights_bearers).
narrative_ontology:constraint_victim(us_constitution__bill_of_rights_1791, federal_legislative_capacity).
narrative_ontology:constraint_victim(us_constitution__bill_of_rights_1791, state_sovereignty_circumscription).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIFYING PUBLIC WITHOUT BILL (SNARE) — The public in 1789 faced an impossible choice: ratify the Constitution without enumerated rights (accepting unlimited federal power over persons and property) or reject it entirely and dissolve the union. There is no third option. The suppression is total — exit means state dissolution. The extractiveness is maximized because the public surrenders all protections against federal power in exchange for union itself. This is the counterfactual against which the 1791 amendment is measured.
constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RATIFYING PUBLIC WITH BILL (TANGLED ROPE) — By 1791, the ratifying public obtains genuine coordination benefit (enumerated protections against federal overreach define the scope of federal power, enabling predictable governance) alongside extraction (the enumerated list creates an implicit negative: unenumerated rights lack protection, and states retain power to suppress rights not listed). The constraint has both coordination function (defines legitimate federal action) and asymmetric extraction (protects federal government from common-law remedies for rights violations).
constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT (ROPE) — The federal government experiences the Bill of Rights as pure coordination: it defines the limits of federal authority with precision, enabling executives and legislators to act within known bounds. The enumerated rights function as a Schelling point that coordinates federal action without ambiguity. The government benefits from the coordinate because it can claim legitimacy ('we act within our authority') and credibly constrain itself. This is coordination without net extraction flow toward the federal government.
constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE STATES (TANGLED ROPE) — The Bill of Rights applies only to the federal government, leaving states free to suppress the same rights within their jurisdictions. States gain coordination (enumerated limits on federal power preserve state authority over internal governance) but lose ground in the zero-sum struggle for sovereignty. The enumerated rights implicitly define rights that federal government cannot touch — but states can. This is mixed: states retain police power (benefit) but face implicit restriction on how far they can go before federal intervention (extraction relative to pre-1791 state autonomy).
constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RATIFICATION RITUAL (PITON) — The ratification process itself is largely performative at the civilizational scale. The Bill of Rights required the same ratification process as any other constitutional amendment, yet it is treated retrospectively as essential to the Constitution's legitimacy. The theater is high because the amendment's real force depends not on ratification ritual but on subsequent litigation (Marbury v Madison, later incorporation doctrine) that established judicial review. Ratification was necessary but not sufficient — the real constraint is enforced in courtrooms, not legislatures.
constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some enumeration of rights is inevitable in any constitution that claims to govern free persons. The requirement that a legitimate government define its limits in terms of individual rights is a structural inevitability, not a contingent bargain. This perspective sees the 1791 amendments as merely documenting what must be true of any legitimate written constitution. However, this classification is a false summit: the structural data reveals that the Bill of Rights is extractive at the point of ratification and remains so in operation.
constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution__bill_of_rights_1791_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution__bill_of_rights_1791, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution__bill_of_rights_1791, TR),
    TR >= 0.70.

:- end_tests(us_constitution__bill_of_rights_1791_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits extraction at the point of ratification (the public surrenders authority to the federal government in exchange for enumerated protections, a net loss if the protections are limited) and in ongoing operation (the enumeration of some rights creates vulnerability for unenumerated ones). However, the extractiveness is not maximal because the Bill of Rights provides genuine coordination function (defines federal authority with precision) and the extraction is unidirectional (federal government benefits from the coordinate; it is not reciprocal extraction). Suppression (0.48): Moderate. Barriers to exit include the state-based ratification requirement and the constitutional amendment process. The initial ratification presented a binary choice (ratify with or without bill), but exit at the point of amendment was available through failure-to-ratify scenarios that nearly occurred. Ongoing suppression is lower because individuals can exit by emigration or can seek remedies through litigation. Theater ratio (0.55): Moderate. The ratification process was performative (the real power lies in subsequent judicial interpretation, not legislative ratification), but the performance was consequential — public sentiment regarding rights protections was real. Over time, the theater ratio has increased as the constraint's enforcement mechanism has moved from legislative politics (ratification) to judicial review (litigation), which is more ceremonial than substantive.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The ratifying public without the Bill sees a snare (unlimited federal power, no exit). The ratifying public with the Bill sees a tangled rope (genuine protection but limited by enumeration). The federal government sees rope (pure coordination enabling legitimate action). The states see tangled rope (benefit from federal limits, cost from sovereignty erosion). The ratification ritual sees piton (performative amendment process whose real work is done in courts). The civilizational observer risks seeing mountain (natural law of rights) but structural data reveals false summit (the Bill of Rights is contingent institutional design, not inevitable). The gap reveals that the constraint's classification depends critically on the time horizon: immediate (ratification is extraction), biographical (enumeration is mixed), generational (incorporation doctrine is reinterpretation), civilizational (rights are natural law — false summit).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the ratifying public is derived from their dual role as both beneficiary (they obtain enumerated rights protections) and victim (they surrender federal authority to a distant government and accept enumeration asymmetry). The beneficiary side yields low d (beneficiaries experience low extracted from themselves); the victim side yields high d (victims experience extraction flowing toward the federal government). The net d balances constrained exit (ratification required state-level consensus; individual exit was costly) against moderate power (organized masses exert pressure). The federal government benefits (arbitrage exit available via constitutional amendment or reinterpretation) and experiences low d. States are secondary victims of the sovereignty competition and experience moderate d (constrained exit from the federal system; retained police power provides some benefit). The analytical observer at civilizational scale risks d = 0.0 (natural law beneficiary view) but this is a false summit — the structural derivation yields d > 0.4 reflecting that the constraint benefits organized parties (federal government, judiciary) at the cost of distributed parties (ratifying public, states, later individuals).
 *
 * MANDATROPHY ANALYSIS:
 *   READING IDENTITY: This story instantiates the bill_of_rights_1791 reading of the us_constitution kernel. The mandatrophy is resolved by recognizing that this reading IS a tangled rope (mixed coordination and extraction) and that the false summit (mountain classification from civilizational perspective) is a diagnostic artifact of the 'natural law of rights' framing, not a legitimate classification. The actual structure is: (1) genuine coordination function (enumeration defines federal authority), (2) asymmetric extraction (enumeration creates vulnerability for unenumerated rights and applies only to federal government), (3) active enforcement (constitutional litigation). The mandatrophy dissolves when we recognize that the constraint's legitimacy depends on both legs: without coordination, it would be pure snare (unlimited federal power); without extraction, it would be pure rope (simple coordination mechanism). The extraction is real (enumeration asymmetry, state exemption, unequal amendment procedures favor federal stability over rights expansion) and necessary to the constraint's existence (if the extraction were removed, the constraint would become a bare coordination mechanism and would lose its hold on constitutional politics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_point_ambiguity,
    'Is the extractiveness primarily at ratification (1789-1791) or in ongoing constitutional interpretation?',
    'Historical analysis of ratification debates and public sentiment regarding rights guarantees; comparison of extraction rates in early vs. late periods of Bill of Rights enforcement',
    'If extraction is primarily at ratification: the constraint is a snare that was partially converted to tangled_rope. If extraction is ongoing: the constraint remains tangled_rope throughout. If extraction declines over time as rights consciousness expands: the constraint moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_point_ambiguity, empirical, 'Whether extractiveness is concentrated at ratification or distributed across time').

omega_variable(
    unenumerated_rights_treatment,
    'Does the enumeration of some rights necessarily imply lack of protection for unenumerated rights, or can unenumerated rights claim equal status?',
    'Doctrinal analysis: the Ninth Amendment as remedy (unenumerated rights retained by the people) vs. judicial interpretation treating enumeration as exhaustive. Does Ninth Amendment jurisprudence expand to protect substantive rights not listed?',
    'If enumeration is exhaustive: the Bill of Rights creates genuine asymmetry (listed rights protected, unlisted rights vulnerable). If Ninth Amendment is functional: enumeration is merely illustrative, and the extraction mechanism is blocked. This shifts classification from tangled_rope toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unenumerated_rights_treatment, conceptual, 'Whether enumeration creates asymmetric vulnerability for unenumerated rights').

omega_variable(
    incorporation_doctrine_contingency,
    'Is the applicability of the Bill of Rights to states (via 14th Amendment incorporation) required by the 1791 reading, or is it a later reinterpretation that changes the constraint structure?',
    'Historical analysis of Reconstruction-era intent; comparison of enforcement mechanisms in 1791 (federal only) vs. post-incorporation (federal and state). Does incorporation doctrine flow from 1791 text or violates it?',
    'If incorporation is internal to the 1791 reading: the constraint''s beneficiary set expands over time, reducing extraction relative to later states. If incorporation is external/later: the 1791 reading must be distinguished from the incorporation-era reading (different constraints, different ε values).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incorporation_doctrine_contingency, conceptual, 'Whether 14th Amendment incorporation is implicit in or external to the 1791 reading').

omega_variable(
    kernel_contest_authority_grounding,
    'Is this reading (bill_of_rights_1791) grounded in textual lineage to the 1791 amendments themselves, or in a particular judicial/scholarly tradition of reading that text?',
    'Identify the contemporary authority structure claiming this reading: legal scholars (originalists vs. living constitutionalists), courts, public discourse. Does authority reside in the founding text, the court system, or the academy?',
    'If authority is textual lineage: the reading is stable when the 1791 text is stable. If authority is institutional (courts/scholarship): the reading drifts with institutional practices. This affects cs_structure.authority_grounding classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_authority_grounding, conceptual, 'Whether this reading''s authority is textual or institutional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution__bill_of_rights_1791, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usbor_theater_1789, us_constitution__bill_of_rights_1791, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usbor_theater_1791, us_constitution__bill_of_rights_1791, theater_ratio, 2, 0.55).
narrative_ontology:measurement(usbor_theater_1891, us_constitution__bill_of_rights_1791, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(usbor_extractiveness_1789, us_constitution__bill_of_rights_1791, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(usbor_extractiveness_1791, us_constitution__bill_of_rights_1791, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(usbor_extractiveness_1891, us_constitution__bill_of_rights_1791, base_extractiveness, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution__bill_of_rights_1791, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution__bill_of_rights_1791, us_constitution__original_1787).
narrative_ontology:affects_constraint(us_constitution__bill_of_rights_1791, us_constitution__14th_amendment_incorporation).
narrative_ontology:affects_constraint(us_constitution__bill_of_rights_1791, us_constitution__ninth_amendment_status).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the us_constitution kernel. Each reading has its own epsilon value and structural properties. The bill_of_rights_1791 reading emphasizes the extractiveness of enumeration asymmetry and single-level application. The original_constitution_1787 reading emphasizes the extractiveness of structural enumeration of federal powers. Later readings emphasize different sources of constitutional authority and different extraction mechanisms. All five readings are live in contemporary constitutional discourse, and multiple parties hold each reading simultaneously. The decomposition follows ε-invariance: different measurements of 'constitutional authority' yield different epsilon values, so they are modeled as distinct constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution__bill_of_rights_1791, institutional, 0.25).
constraint_indexing:directionality_override(us_constitution__bill_of_rights_1791, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
