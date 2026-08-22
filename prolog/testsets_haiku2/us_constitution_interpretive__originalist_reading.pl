% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation (Framers' Intent Authority)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The originalist reading of constitutional interpretation holds that the
 *   Constitution's meaning is fixed at the moment of ratification, and that
 *   interpretive authority derives from fidelity to the framers' intent or
 *   the original public meaning of the text. This constraint story models
 *   originalism as a reading of the contested US Constitution kernel—not as a
 *   natural law or a discovered fact, but as one of several live interpretive
 *   methodologies offered by different judicial coalitions and legal
 *   factions. The originalist reading constrains federal power by reading
 *   enumerated powers narrowly, preserves state police powers, and limits
 *   unenumerated rights to those historically recognized. It benefits
 *   federalism advocates, religious liberty claimants under the original Free
 *   Exercise reading, and property rights defenders who gain narrower federal
 *   regulatory scope. It extracts costs from unenumerated rights claimants
 *   (reproductive autonomy, privacy, dignity) who find their interests
 *   foreclosed by the original-meaning frame, and from federal regulatory
 *   expansion advocates who face narrower Commerce Clause interpretation. The
 *   claim and metrics are independent: this story claims the constraint is a
 *   tangled rope (genuine coordination function—the stability of fixed
 *   meaning—combined with asymmetric extraction) while the metrics describe
 *   substantial extractiveness (0.68), high suppression (0.72), and rising
 *   theater ratio (0.41) indicating performative maintenance. The engine will
 *   compute per-seat classifications from the structural data; the authored
 *   claim does not determine the outcome.
 *
 * KEY AGENTS:
 *   - Originalist judicial coalition (agenda_setter): Supreme Court and lower-court judges who control the interpretive frame by writing opinions invoking original meaning.
 *   - Federalism advocates (beneficiary): States, conservative intellectuals, institutional conservatives who benefit from narrower federal enumerated powers.
 *   - Religious liberty claimants under original understanding (beneficiary): Faith-based organizations and conscience-protection litigants who benefit from originalism's narrow Establishment Clause reading.
 *   - Property rights defenders (beneficiary): Business interests and libertarian actors who benefit from originalism's narrower Takings and Commerce Clause readings.
 *   - Unenumerated rights claimants (payer): Individuals and groups claiming reproductive autonomy, privacy, dignity—rights foreclosed by originalism's historical-scope limit.
 *   - Federal regulatory expansion advocates (payer): Federal agencies, progressive scholars, Congress members who bear the cost of narrower federal enumerated powers.
 *   - Living constitutionalist judicial coalition (excluded): Justices and academics who favor evolving constitutional meaning; excluded from setting the dominant frame by originalist institutional control.
 *   - State governments (beneficiary): Benefit from preserved state police powers; do not set the frame but receive the allocation benefit.
 *   - Congress and Executive (payer/beneficiary duality): Constrained by narrower federal power but sometimes benefit from originalism's protection of executive prerogatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Constitutional Interpretation (Framers' Intent Authority)").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3').
narrative_ontology:cs_kernel_codification('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', fixed_text).
narrative_ontology:cs_authority_grounding('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', lineage).
narrative_ontology:cs_interpretation_layer_present('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3').
narrative_ontology:cs_reading_relation('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', constitutional_meaning_fixed_at_ratification, empirically_contingent).
narrative_ontology:cs_axiom('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', foundational, framers_intent_accessible_via_historical_inquiry).
narrative_ontology:cs_axiom_status(framers_intent_accessible_via_historical_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', framers_intent_accessible_via_historical_inquiry, empirically_contingent).
narrative_ontology:cs_axiom('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', secondary, enumerated_powers_limit_federal_reach).
narrative_ontology:cs_axiom_status(enumerated_powers_limit_federal_reach, holdable).
narrative_ontology:cs_axiom_grounding('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', enumerated_powers_limit_federal_reach, deontological).
narrative_ontology:cs_axiom('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', secondary, unenumerated_rights_outside_scope).
narrative_ontology:cs_axiom_status(unenumerated_rights_outside_scope, holdable).
narrative_ontology:cs_axiom_grounding('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', unenumerated_rights_outside_scope, deontological).
narrative_ontology:cs_reference_frame('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', framers_intent_1787_1868).
narrative_ontology:cs_drift_state('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', contemporary_political_contestation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8cae65b1-8f8e-431e-9c18-fe6c32ce8bc3', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, congress_and_executive).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, state_governments).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, originalist_legal_academy).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, congress_and_executive).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, judicial_fidelity_to_text).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, enumerated_powers_limit_federal_reach).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, original_public_meaning_constrains_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Supreme Court justices and lower-court judges committed to originalist interpretive methodology. They set the interpretive frame by writing opinions that invoke framers' intent or original public meaning as the authoritative source of constitutional meaning. They enforce this frame by striking down regulations and doctrines they read as departing from the original understanding. Their power derives from Article III judicial supremacy over constitutional questions within their cases; they cannot legislate but shape what the law permits.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judicial_coalition, agenda_setter,
    institutional, generational, constrained, national).

% Actors (states, conservative intellectuals, institutional conservatives) who benefit from the originalist frame's constraint on federal enumerated powers. Under originalism, federal regulatory reach is narrower than under living constitutionalism; state police powers retain a presumptive sphere. They benefit from this allocation without having to run the judicial system themselves—they lobby for originalist judges and defend originalist doctrine, but the agenda-setting work is done by the judiciary. Their exit option is to advocate for alternative readings or constitutional amendment, but originalism's current institutional strength makes amendment costly.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    powerful, generational, mobile, national).

% Religious liberty advocates (congregations, faith-based organizations, conscience-protection litigants) who read the original Free Exercise Clause and Establishment Clause as protecting a narrower regulatory reach than the contemporary living-constitutionalist reading permits. Under originalism, they argue that laws targeting religious practice are subject to less strict scrutiny if they fail the original-meaning test; they benefit from the constrained federal power frame when it aligns with their sectarian interests. Their exit is costly—they cannot easily leave the constitutional frame—but they can switch between reading strategies if originalism ceases to advance their interests.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding, beneficiary,
    organized, biographical, constrained, national).

% Property owners, business interests, and libertarian-oriented actors who benefit from originalism's narrower reading of the Takings Clause, the scope of the Commerce Clause, and the reach of the Fourteenth Amendment's application to state economic regulation. Originalism's fidelity to 1787 and 1868 meanings constrains federal economic intervention. They have exit: they can invest across jurisdictions, lobby for legislative change, or advocate for alternative readings, but the current originalist trajectory serves their interests.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals and groups claiming rights not explicitly listed in the Constitution (reproductive autonomy, intimate association, privacy, dignity interests, procedural protections beyond those enumerated)—rights protected under living-constitutionalist doctrine but not recognized under originalism's historical-scope limit. They must litigate to establish rights; originalism's methodological frame narrows the success space available to them. Their exit is extremely limited: they cannot leave the constitutional jurisdiction; they can only advocate for competing readings, mobilize politically, or pursue constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    moderate, biographical, trapped, national).

% Federal agencies, progressive legal scholars, members of Congress who favor broad federal power to regulate interstate commerce, environmental protection, labor standards, and social welfare. Originalism constrains their policy reach by reading the Commerce Clause, Necessary and Proper Clause, and federal enforcement powers narrowly. They bear the cost of narrower federal scope and litigate against originalist judges. Their exit is constrained: they can advocate for living constitutionalism or amendment, but cannot leave the system; they can shift to state-level advocacy or pursue legislative workarounds (structural adjustments, limited constitutional amendment via convention).
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    organized, generational, constrained, national).

% Supreme Court justices and academic jurists who favor living-constitution interpretation, reading the Constitution's text as establishing principles that evolve with societal values. They are excluded from setting the dominant interpretive frame because originalist judges hold the majority of judicial power and the intellectual initiative on the current Court (2020s). They would argue for unenumerated rights protection, broader federal power, and Fourteenth Amendment incorporation of evolving liberty interests. Their exclusion is maintained by the originalist coalition's control of judicial appointments and doctrine-setting opinion.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalist_judicial_coalition, excluded,
    institutional, generational, constrained, national).

% The federal legislative and executive branches bear the cost of originalism's constraint on federal power (narrower authority to regulate, higher burden of constitutional justification for new programs) but also benefit from originalism in cases where it permits executive prerogatives, deference to military decision-making, or protection of presidential power. Congress specifically is both payer (narrower enumerated powers) and beneficiary (originalism can protect legislative prerogatives against executive encroachment, constrains unenumerated rights that compete with legislative choices). This duality reflects the original-meaning frame's ambiguous position on separation of powers.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, congress_and_executive, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, congress_and_executive, beneficiary).

% State legislatures and governors benefit from originalism's narrower reading of federal enumerated powers, which preserves state police powers and reserves certain regulatory domains to the states. They do not set the interpretive frame (federal judiciary does) but receive the allocation benefit. Their exit is constrained: they cannot leave the constitutional system but can invoke originalist arguments in state courts, lobby for originalist federal judges, and advocate for popular constitutionalism or amendment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, state_governments, beneficiary,
    organized, generational, constrained, national).

% Law professors, think-tank scholars, and intellectual architects who develop and defend originalist methodology (Scalia, Bork, Randy Barnett, Keith Whittington tradition). They benefit from institutional prestige, grant funding, ideological alignment with conservative legal movement, and the cultural authority of having shaped the current dominant judicial frame. They have exit: they can shift to other interpretive schools or leave academia; their mobility is relatively high due to professional mobility and ideological flexibility.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_legal_academy, beneficiary,
    powerful, generational, mobile, national).

% Scholars and activists advocating popular constitutionalism (Bruce Ackerman, Claudia Rosett tradition)—the reading that constitutional meaning is shaped by popular political movements and mobilization, not solely by judicial interpretation. They are excluded from the official interpretive frame (courts do not recognize popular constitutionalism as binding authority) but maintain an alternative voice in democratic contestation. Their exclusion is maintained by the judiciary's formal power to declare law unconstitutional.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, popular_constitutionalist_advocates, excluded,
    moderate, biographical, constrained, national).

% Academic analysts, legal historians, and comparative constitutionalists who study how originalism operates as an interpretive constraint, its social costs, its relationship to competing readings, and its institutional persistence. They assess the system; they do not participate in it.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, analytical_observer, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, originalist_judicial_coalition).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, predictable procedure for interpreting the Constitution's fixed meaning by reference to the framers' intent or original public meaning at ratification (1787 for the main text, 1868 for the Fourteenth Amendment). Solves the coordination problem of how courts justify departing from the text's plain language—by anchoring departures to discoverable historical meaning rather than to the judge's contemporary values. Produces ex-ante predictability for how constitutional claims will be adjudicated and constrains ad-hoc reasoning.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary legislative and popular majorities to the judiciary acting as agents of eighteenth-century (or nineteenth-century) framers' intent. It transfers policy-making space from federal regulators to state regulators and from broad rights-recognition to narrow historical-scope rights. It transfers the burden of constitutional constraint from the judiciary (which no longer needs to justify its choices by reference to contemporary values) to those seeking to change constitutional meaning (who must now pursue amendment, not litigation).
% ABSENT_VOICES: Living constitutionalists are excluded from setting the authoritative interpretive frame in the current judiciary; they maintain an alternative position but lack the institutional power to determine binding constitutional meaning. Popular constitutionalists are excluded from official recognition; their claim that popular political movements shape constitutional meaning is not incorporated into judicial doctrine. Citizens claiming unenumerated rights are excluded from representation in the frame: their interests are not accounted for when the originalist coalition sets the interpretive agenda. Future generations whose values diverge from 1787 framers are excluded: they cannot voice their contemporary understanding as part of the amendment-or-ratification process.
% DISAPPEARANCE_RATIONALE: If originalism as the dominant interpretive frame disappeared overnight, the judiciary would shift to alternative methodologies (living constitutionalism, popular constitutionalism, or hybrid approaches). This would immediately widen the recognized scope of unenumerated rights (reproductive autonomy, privacy, dignity), expand federal enumerated powers, and narrow state police power reserves. Constitutional litigation outcomes would differ systematically. Federalism boundaries would shift; federal regulatory authority would expand where living constitutionalism permits Fourteenth Amendment incorporation or broad Commerce Clause readings. The allocation of interpretive authority would shift from fidelity to eighteenth-century meaning toward adaptation to contemporary values.
% FOUNDING_PROBLEM: The Constitution is a fixed text that can be read to mean different things in different eras depending on the interpreter's methodology. Early constitutional practice (1790s–1900s) relied on ad-hoc reasoning, sectional compromise, and de facto amendment through reinterpretation. The founding problem was: how can courts justify their constitutional choices and constrain judges from imposing arbitrary contemporary values under the guise of constitutional interpretation? How can constitutional meaning remain stable enough to constrain power while courts maintain the flexibility needed to apply the text to unforeseen circumstances?
% FOUNDING_PROBLEM_CORROBORATION: Originalist justices (Scalia, Thomas, Barrett) attest the founding problem is live: without textual anchoring, judges become legislators; originalism constrains judicial lawmaking by tying interpretation to discoverable historical meaning. Living constitutionalists (Justices Kagan, Sotomayor, Breyer) and academic scholars (Cass Sunstein, Jack Balkin, Larry Tribe) attest the founding problem is partially solved but originalism trades constraint-of-judges for constraint-of-rights and constraint-of-federalism; the original problem was judicial discretion, which originalism doesn't fully eliminate (judges still disagree on original meaning). Legal historians and political scientists (Gordon Wood, Pauline Maier, Mark Graber) attest the founding problem is sociologically misframed: early constitutional meaning was never simply 'what the framers intended' but emerged from political contestation; originalism projects a false stability onto the founding era. Popular constitutionalists (Ackerman, Amar) attest the founding problem requires popular ratification and political mobilization, not judicial methodology alone.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) reflects the structural asymmetry: originalism constrains future-oriented rights claimants by fixing the meaning at a historical moment when their interests were not yet conceptualized (unenumerated rights, federal regulatory authority). The constraint is extractive because one party (the framers via judicial interpretation) controls the meaning-setting for all future generations. Suppression (0.72) is high because the judicial enforcement machinery actively excludes competing interpretations; originalist judges strike down regulations and doctrines they read as departing from original meaning; this exclusion is active and requires institutional resources to maintain. The theater ratio (0.41) reflects a moderate proportion of performative activity: originalist judges genuinely engage in historical scholarship and textual analysis (real coordination function), but an increasing share of their effort goes to defending originalism against living constitutionalist challenges and reinterpreting 'original meaning' in ways that produce predetermined political outcomes (rising from 0.25 at t=0 to 0.41 at t=50 suggests theater is accumulating as the reading matures). The measurement series shows extraction accumulating over 50 time units (proxy: decades of judicial appointments and doctrine refinement) as originalism crystallizes institutional power and begins to perform maintenance work (theater rising faster than extraction in the latter interval). The tangled_rope claim reflects both genuine coordination (fixed meaning provides stability and ex-ante predictability) and asymmetric extraction (the fixing occurs at a moment that benefits some generations and extracts from others).
 *
 * PERSPECTIVAL GAP:
 *   The originalist judicial coalition and federalism beneficiaries experience this constraint as stable coordination—a rule-of-law mechanism that constrains judges and provides predictability. Unenumerated rights claimants and federal regulatory advocates experience it as enforced extraction—a reading imposed by institutional power that forecloses their claims without their consent. From the originalist seat, the constraint is Rope (real coordination, minimal extraction relative to the coordination benefit). From the payer seats, the constraint is Snare (the coordination story is cover; persistence depends on judicial enforcement; alternatives are suppressed). The engine will compute these perspectives from the power atoms (institutional vs. moderate), exit options (constrained for payers vs. arbitrage for beneficiaries), and the structural relationship to the meaning-fixing moment. The authored claim (tangled_rope) sits between, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judicial coalition: power=institutional, exit=constrained (cannot leave Article III; sets the frame but is also constrained by it), directionality d ≈ 0.3 (near beneficiary, benefits from having set the frame). Federalism advocates: power=powerful, exit=mobile (can lobby, invest across jurisdictions, shift positions), d ≈ 0.2 (beneficiary; enjoy the narrower federal scope without having to run the system). Unenumerated rights claimants: power=moderate, exit=trapped (cannot leave constitutional jurisdiction; cannot pursue amendment easily), d ≈ 0.85 (near target; extraction from them is amplified by trapped exit). Federal regulatory advocates: power=organized (agencies, congressional coalitions), exit=constrained (can shift to state-level advocacy, congressional workarounds, but cannot leave the frame), d ≈ 0.75 (target; narrower federal scope directly constrains their reach). Living constitutionalist coalition: power=institutional, exit=constrained (same institutional position as originalists, different doctrine), d ≈ 0.8 (target; excluded from setting frame, extracts through direct competition for judicial power and intellectual authority). Congress/Executive duality: d ≈ 0.5 (symmetric; constrained by narrower federal power but sometimes benefit from originalism's presidential power protections; the mixed character is captured by the secondary_role duality).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for originalism is: how can constitutional meaning remain stable against judicial discretion? The originalist solution is to anchor meaning to eighteenth-century framers' intent. However, the founding problem's status is contested because: (1) originalism does not eliminate judicial discretion—judges disagree on original meaning (Justice Scalia vs. Justice Thomas on executive power, for instance); (2) the founding problem may have been misframed—the real problem was not judicial discretion per se but how to distribute power across branches and generations; (3) originalism may have solved the wrong problem: it addressed constraint-of-judges at the cost of constraint-of-rights and constraint-of-federalism, trading one allocative problem for another. Mandatrophy is partially present: the originalist frame has partially outlived its founding function (providing constraint-of-judges) and is now maintained partly through institutional inertia and partly through the political benefits it produces for beneficiary coalitions. The rising theater ratio (0.25 → 0.41) suggests performative maintenance is accumulating—originalists increasingly reinterpret 'original meaning' to defend predetermined political outcomes (the Dobbs decision on abortion, the Second Amendment expansions) rather than discovering new historical truths. This is not full mandatrophy (the coordination function is still real; originalists do engage in genuine historical scholarship), but partial: the reading's maintenance increasingly depends on defending a particular coalition's political interests, not on the original coordination function alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_committer_foreclosure,
    'Does originalism''s core axiom (constitutional meaning fixed at ratification, interpreted via framers'' intent/original public meaning) logically foreclose the living-constitution reading''s core axiom (constitutional meaning evolves with societal values), or do they coexist as different parties'' positions within a single constitutional framework?',
    'Examine whether a judge could coherently hold both axioms simultaneously without logical contradiction—is the disagreement a contradiction in the law itself, or a disagreement about what the law is? If a judge cannot write an opinion that satisfies both axioms, they foreclose; if competing camps of judges each hold one and both produce valid legal reasoning (even if mutually exclusive conclusions), they coexist.',
    'If foreclosure: the readings are fundamentally incompatible, and the engine should treat them as single-framework incompatibles (forecloses relation). If coexistence: both readings are live options held by different institutional actors, and the engine should treat them as parallel frameworks in tension (coexists_with relation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_foreclosure, conceptual, 'Whether originalist and living-constitutionalist readings logically foreclose or coexist within the same legal framework.').

omega_variable(
    reading_extraction_institutional_positioning,
    'Is the originalist reading''s measured extractiveness (0.68) a property of the reading''s substantive content (who benefits, who pays under originalist interpretation), or is it partly a property of originalism''s current institutional power position (originalists control the majority Court and set the interpretive agenda)?',
    'Counterfactual: if living constitutionalism controlled the majority Court, would its extraction metric be higher or lower than originalism''s? If a reading''s extractiveness rises and falls with institutional power, the extraction is partly positional; if substantive content dominates, the reading''s extraction remains stable across power positions.',
    'If positional: the originalist reading''s extraction is not invariant to institutional change; a future Court majority shifting to living constitutionalism would alter the measured χ. The reading''s identity and classification would depend partly on seat perspective and partly on institutional moment. If substantive: the reading''s extraction is stable regardless of institutional control; the measured χ reflects the permanent structural asymmetry of the reading (fixed meaning favors historical beneficiaries, limits future-oriented claimants).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_extraction_institutional_positioning, conceptual, 'Degree to which originalism''s extractiveness is institutional positioning vs. substantive reading content.').

omega_variable(
    amendment_alternative_viability,
    'Is constitutional amendment a genuine exit for victims of the originalist frame (unenumerated rights claimants, federal regulatory advocates), or is amendment-via-Article V so prohibitively costly that it functions as a theoretical exit only?',
    'Historical analysis: how many amendments have overturned judicial constitutional interpretation? What is the time-to-amendment and cost-to-amendment for changing originalist doctrine via Article V vs. changing it via appointing new judges or shifting judicial doctrine?',
    'If amendment is viable: victims have a real exit (trapped exit_options becomes constrained or mobile). If amendment is prohibitive: victims remain trapped, and the constraint''s suppression is genuinely structural, not escapable via amendment path. The exit-options classification for unenumerated rights claimants depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_alternative_viability, empirical, 'Viability of constitutional amendment as exit from originalist interpretive constraint.').

omega_variable(
    reading_dependent_beneficiary_stability,
    'Are the identified beneficiaries (federalism advocates, religious liberty claimants, property rights defenders) genuinely stable beneficiaries under originalism, or would they shift if the historical meaning became different (e.g., if new historical scholarship reinterpreted the framers'' intent differently)?',
    'Historical case study: when constitutional historians have revised their understanding of framers'' intent (e.g., regarding slavery, executive power, federal enumeration), have the identified beneficiary groups adjusted their positions, or have they continued to argue for originalism while reinterpreting ''original meaning'' to suit their interests?',
    'If stable: the beneficiary set is invariant to historical reinterpretation; the reading generates the same beneficiaries regardless. If unstable: the beneficiary set is dependent on the *particular historical interpretation* the originalist coalition currently endorses; if that interpretation shifts, beneficiaries shift, and the reading''s function becomes more extractive (the interpretation becomes a tool to justify pre-existing political goals, not a constraint on them).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dependent_beneficiary_stability, empirical, 'Stability of the beneficiary set under originalism across different historical interpretations.').

omega_variable(
    theater_ratio_source_identification,
    'What is the source of the rising theater ratio (0.25 → 0.41 over the interval)? Is originalism''s increasing performative activity a sign of institutional maturation and routinization, or a sign of mandatrophy where the reading is defending predetermined political outcomes rather than discovering historical meaning?',
    'Comparative analysis of originalist opinions over time: do later opinions engage with competing historical interpretations and genuinely grapple with alternative readings, or do they invoke ''original meaning'' in increasingly formulaic ways that reach predetermined conclusions? Measurement: ratio of opinions that substantively change the author''s historical interpretation vs. opinions that invoke ''original meaning'' to reach conclusions consistent with the author''s prior ideological position.',
    'If routinization: theater rise is normal institutional maturation; the reading maintains its coordination function. If mandatrophy: theater rise indicates loss of analytical content; the reading is becoming a political tool, extractiveness will rise further, and reclassification to snare becomes warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_source_identification, empirical, 'Whether rising theater ratio reflects institutional maturation or mandatrophy degradation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__originalist_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__originalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__originalist_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__originalist_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__originalist_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__originalist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__originalist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__originalist_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__originalist_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__originalist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(us_c_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__originalist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__originalist_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__originalist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__originalist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__originalist_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(us_c_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__originalist_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% The US Constitution interpretive kernel decomposes into three constraint stories: originalist_reading, living_constitution_reading, popular_constitutionalism_reading. Each instantiates a different judicial methodology and produces different beneficiary/victim sets. Originalism fixes meaning at ratification (narrow federal power, limited unenumerated rights). Living constitutionalism evolves meaning with societal values (expanded federal power, expanded rights). Popular constitutionalism emphasizes popular political movements (constitutional meaning shaped by mobilization, not solely courts). Each reading has its own ε value, beneficiary structure, and type classification; they are linked via network.affects_constraints because the judicial adoption of one reading directly influences the institutional viability of the others (winner-take-most in a generation of judges).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
