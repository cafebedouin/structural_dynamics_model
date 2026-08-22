% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Popular Sovereignty & Electoral Legitimation (Republican Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The republican reading of legitimate authority posits that power flows
 *   upward from the people through delegated consent: authority is legitimate
 *   only insofar as it can claim authorization from those it governs,
 *   validated through periodic elections and removal mechanisms. This reading
 *   was formalized in the 17th and 18th centuries (Locke, Rousseau, American
 *   Revolution, French Revolution) and instantiated in constitutional
 *   democracies. The constraint operates as both coordination (solves the
 *   problem of how to aggregate dispersed preferences into unified action)
 *   and extraction (beneficiaries — those with franchise — extract legitimacy
 *   from disenfranchised populations; elected representatives extract power
 *   that is revocable but concentrated; the apparatus itself requires
 *   enforcement against alternative legitimacy claims). The reading is one of
 *   three contestations of the underlying kernel: legitimate_authority can be
 *   interpreted as monarchical (inheritance), hybrid (constitutional
 *   balancing), or republican (delegation from below). Each reading produces
 *   a distinct constraint with different beneficiaries, extraction patterns,
 *   and persistence mechanisms. The claim/metric gap is intentional: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination + asymmetric
 *   extraction) while metrics are authored to be descriptive of actual
 *   operation. The engine computes the per-seat classification; the gap
 *   between claim and metrics is exactly the measurement the corpus exists to
 *   capture.
 *
 * KEY AGENTS:
 *   - franchise_holders: those with voting rights; structural beneficiaries of the legitimacy rule that authority must answer to them; also partly agenda-setters (administering the boundary that excludes others)
 *   - disenfranchised_populations: non-citizens, historical slaves, women pre-suffrage, minors, imprisoned persons; structural victims (subject to authority they cannot authorize; exit trapped by law and jurisdiction)
 *   - elected_representatives: delegated authority bearers; pay costs of accountability (removal at elections) but gain concentrated power and the ability to claim legitimacy for their actions
 *   - constitutional_interpreters: institutional mediators (courts, legal authorities) charged with maintaining adherence to the popular-sovereignty principle; substantial power to define its scope, constrained by requirement to cite it
 *   - competing_authority_claimants: monarchical, theocratic, or alternative-legitimacy frameworks; structurally excluded from the republican frame; would argue authority does not require consent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.38).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.42).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Popular Sovereignty & Electoral Legitimation (Republican Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '5fac0b22-4947-4356-9e82-6e3d1f25fe76').
narrative_ontology:cs_kernel_codification('5fac0b22-4947-4356-9e82-6e3d1f25fe76', formalized).
narrative_ontology:cs_authority_grounding('5fac0b22-4947-4356-9e82-6e3d1f25fe76', lineage).
narrative_ontology:cs_interpretation_layer_present('5fac0b22-4947-4356-9e82-6e3d1f25fe76').
narrative_ontology:cs_reading_relation('5fac0b22-4947-4356-9e82-6e3d1f25fe76', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('5fac0b22-4947-4356-9e82-6e3d1f25fe76', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('5fac0b22-4947-4356-9e82-6e3d1f25fe76', foundational, authority_requires_popular_delegation).
narrative_ontology:cs_axiom_status(authority_requires_popular_delegation, holdable).
narrative_ontology:cs_axiom_grounding('5fac0b22-4947-4356-9e82-6e3d1f25fe76', authority_requires_popular_delegation, deontological).
narrative_ontology:cs_axiom('5fac0b22-4947-4356-9e82-6e3d1f25fe76', foundational, legitimacy_revocable_through_election).
narrative_ontology:cs_axiom_status(legitimacy_revocable_through_election, holdable).
narrative_ontology:cs_axiom_grounding('5fac0b22-4947-4356-9e82-6e3d1f25fe76', legitimacy_revocable_through_election, conventional).
narrative_ontology:cs_reference_frame('5fac0b22-4947-4356-9e82-6e3d1f25fe76', popular_sovereignty_as_legitimacy_source).
narrative_ontology:cs_drift_state('5fac0b22-4947-4356-9e82-6e3d1f25fe76', contemporary_post_industrial_democracy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5fac0b22-4947-4356-9e82-6e3d1f25fe76', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, franchise_holders).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, participatory_citizens).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_populations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, non_citizen_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, elected_authority_representatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess formal voting rights and participate in periodic elections that determine authority composition and policy direction. They exercise delegated consent through ballots and hold removal power through electoral cycles. Their participation legitimates the authority structure and sustains its consent basis. Benefits from the rule that authority must answer to them through elections; also administers the franchise boundary that excludes others.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, franchise_holders, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, franchise_holders, agenda_setter).

% Engage in deliberation, assembly, petition, and voting mechanisms that constitute the popular sovereignty foundation. They experience the constraint as legitimate because they (in principle) author it through collective action. Exit is constrained by citizenship and territorial jurisdiction, but voice is structurally enabled.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, participatory_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Hold delegated power revocable at election cycles. Must justify actions to the electorate and face removal if legitimacy is withdrawn. They bear the cost of accountability: constant need to maintain electoral coalition, exposure to electoral defeat, and constraint that all action must claim popular foundation. Administers the election machinery and maintains the participatory apparatus.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_authority_representatives, payer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, elected_authority_representatives, agenda_setter).

% Subject to authority derived from consent they are structurally barred from giving: non-citizen residents, formerly enslaved peoples (historically), women before suffrage expansion, minors, institutionalized populations. They bear the enforcement costs of legitimacy rules that exclude them — rules that govern their conduct are authored by those they cannot vote out. Their exit options are severely constrained by law and geography.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_populations, payer,
    powerless, generational, trapped, national).

% Would-be monarchists, theocratic authorities, or alternative legitimacy frameworks (divine right, hereditary succession, technocratic rule) are structurally excluded from the discourse within the republican frame. They could participate only by accepting the legitimacy premise they contest. Their exclusion is constitutive of the republican reading.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, competing_authority_claimants, excluded,
    powerful, generational, trapped, national).

% Judges, constitutional scholars, and guardian institutions charged with maintaining adherence to the popular-sovereignty framework. They mediate between electoral mandates and constitutional constraints, deciding which popular expressions are legitimate and which violate the foundational consent premise. Their power is constrained by the requirement to cite popular sovereignty, yet substantial in determining its scope.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_interpreters, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, constitutional_interpreters, observer).

% External actors who recognize legitimate authority based on whether the republican apparatus functions. Diplomatic recognition, treaty participation, and international standing ride on the credibility of the popular-sovereignty claim. They observe the constraint's operation and validate it through international acceptance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, international_state_system, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, franchise_holders).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed preferences from millions of individual citizens into coherent policy direction through elections and representation. Solves the collective-action problem of 'how do the many authorize the few without consensus.' Creates a mechanism that binds all to decisions made by representatives while preserving the theoretical power of electoral removal.
% TRANSFER_FUNCTION: Transfers decision-making power from atomized individuals to organized representatives; transfers authorization authority from the people to elected authority; transfers accountability obligation from popular deliberation to electoral cycles. Money, status, and enforcement authority flow upward to government; legitimacy claims and (theoretically) removal power flow downward to citizens.
% ABSENT_VOICES: Disenfranchised populations (non-citizens, minors, imprisoned persons, institutionalized populations) would contest the premise that consent can be delegated from a narrower franchise to bind the whole. Alternative legitimacy claimants (monarchists, theocrats, autocrats, meritocratic technocrats) would argue that legitimate authority derives from sources other than electoral consent: tradition, divine right, superior expertise, efficiency. Their absence from the constitutional discourse is structural to the reading, not accidental.
% DISAPPEARANCE_RATIONALE: If the popular-sovereignty principle and its electoral apparatus vanished, authority would have to re-legitimize itself on other grounds: hereditary succession, religious sanction, expert claim, military power, or raw force. Existing democratic regimes would either collapse (if authority withdrawal is complete) or reorganize under alternative legitimacy frames (coup installations, monarchical restoration, theocratic revolution, oligarchic stabilization). The populations' relationship to authority would shift from 'we authorized this through voting' to 'this is imposed on us' or 'this rules us through tradition/competence/faith.' The change would be catastrophic to societies organized around the popular-sovereignty principle.
% FOUNDING_PROBLEM: How can government authority claim legitimacy rather than mere force? The republican answer: by claiming to be authorized by the people through their delegated consent, validated through periodic elections and subject to electoral removal.
% FOUNDING_PROBLEM_CORROBORATION: Democratic movements, constitutional democracies, international human rights frameworks (UN Charter, International Covenant on Civil and Political Rights), and empirical legitimacy research outside the benefiting parties all attest the founding problem remains live: citizens in democratic regimes perceive authority as more legitimate when they can vote than when they cannot, and authority claiming electoral mandate gains more compliance than authority claiming merely to be efficient or traditional. Autocratic regimes maintain power through substantial enforcement machinery (suppression higher than in democracies), suggesting the popular-sovereignty claim has real force even in regimes opposing it.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38): Moderate, not high, because the constraint does genuinely solve a real coordination problem (aggregating dispersed preferences into binding decisions) and beneficiaries do gain legitimate authority through it. But extraction is present because the franchise boundary is contested, enforcement machinery exists to exclude alternatives, and even within the franchise the periodic nature of elections means citizens surrender power between cycles. The measurement series shows a rise from 0.28 to 0.40 over time (t=0 to t=24), then stabilization around 0.38. This trajectory indicates mounting enforcement against alternative claims (suppression requirement rises from 0.25 to 0.44) but stabilized extraction at an intermediate level — neither purely coordination nor pure capture. The Republican reading is not a mountain: the people could theoretically authorize differently, the system requires active defense against alternative framings, alternatives do exist but are suppressed. It is not a pure snare because genuine coordination occurs and beneficiaries do pay costs of accountability. Tangled rope fits: coordination (elections aggregate preferences) + extraction (franchise holders claim legitimacy-authoring power from the whole population, many of whom cannot participate).
 *   
 *   SUPPRESSION (0.42): Active enforcement required against competing legitimacy claims (monarchical, theocratic, meritocratic alternatives). This enforcement is institutional and legal: exclusion of non-citizens from voting, prohibition of alternative authority mechanisms, legal penalties for advocating non-electoral legitimacy. The trajectory rises from 0.25 to 0.44 over the interval, indicating the Republican apparatus strengthened its enforcement machinery in response to pressure from both excluded populations (demanding inclusion) and alternative frameworks (claiming superior legitimacy). Stabilization at 0.42 reflects mature enforcement: the system has built sufficient institutional defenses that suppression is stable rather than accumulating, but remains substantial because the boundary is genuinely contested.
 *   
 *   THEATER_RATIO (0.28): Low-to-moderate. The core coordination function (elections, representation) remains operationally functional; citizens do participate, outcomes do change based on votes, representation does respond to constituency pressure in many cases. But the ratio rises from 0.15 to 0.28 over time, indicating growing performativity: electoral cycles persist but citizen participation declines (in many democracies), deliberation thins, media spectacle dominates campaigning, the mechanical operation of elections becomes theater while the legitimacy-grounding function attenuates. The stabilization at 0.28 after t=24 suggests a baseline theatricality that does not trend toward piton (0.50+) but is visible — the form persists more robustly than the substance of participation. Not yet mandatrophy but trending toward it.
 *   
 *   ACCESSIBILITY_COLLAPSE (0.65): Moderate-high. Once a population understands that authority's legitimacy depends on electoral authorization, alternatives collapse: one cannot claim legitimacy as a monarch within a Republican framework, one cannot claim the people *want* theocratic rule once the popular-sovereignty principle is established (one must argue they are wrong about what they want, or that authority overrides their preferences — both defeats the premise). However, collapse is not near-complete (not 0.85+) because people can imagine alternatives (authoritarian stability, expert rule, hereditary succession) and some do advocate them; the constraint's maintenance requires continuously defending the framework, not resting on natural-law status. The people *could* theoretically change their authorization at any point; that they do not is not structural inevitability but ongoing choice (possibly constrained).
 *   
 *   RESISTANCE (0.58): Moderate-high. The constraint meets substantial active resistance from multiple directions: excluded populations organizing for franchise expansion, alternative legitimacy frameworks (monarchists, theocrats, authoritarian modernizers) arguing for different authority sources, within-franchise minorities resisting majoritarian outcomes, and institutional actors (courts, legislatures) navigating boundary tensions. This resistance is not suppressed entirely (as it would be in a snare); it shapes the constraint's evolution (franchise expansion, constitutional amendment, constitutional courts emerging to mediate legitimacy claims). The constraint's persistence is not indifferent to resistance; it adapts. Resistance at 0.58 reflects genuine contestation without system collapse.
 *   
 *   MEASUREMENT_GRID_ALIGNMENT: All six measurements (two at each time point for base_extractiveness, suppression_requirement, theater_ratio) share one common time grid: t=0, 8, 16, 24, 32, 40. Every metric has a value at every time point. This alignment prevents the OQ-105 class of errors (endpoint values contaminating earlier analysis via sparse-grid imputation).
 *
 * PERSPECTIVAL GAP:
 *   SEAT DIVERGENCE: The engine should compute different types from different seats. FRANCHISE_HOLDERS would likely compute rope or tangled_rope from their seat: they benefit from legitimate authority they can control through voting; the coordination genuinely solves their preference-aggregation problem; extraction from them is modest (accountability costs). DISENFRANCHISED_POPULATIONS would likely compute snare or tangled_rope from their seat: they are subject to authority they cannot authorize; the coordination's benefit does not extend to them; suppression is structural (legal barriers); exit is trapped. ELECTED_REPRESENTATIVES would compute something near rope: they gain concentrated power and legitimacy, pay costs of removal, but the mechanism itself benefits them (it is how they came to power). CONSTITUTIONAL_INTERPRETERS would compute something asymmetric: they are empowered by the legitimacy principle but also constrained by it — they must cite popular sovereignty to justify their power-limiting decisions, yet that citation itself may be strategic. These divergences arise naturally from the authored stakeholder situations and power/exit combinations; the engine computes them.
 *
 * DIRECTIONALITY LOGIC:
 *   FRANCHISE_HOLDERS (d ≈ 0.2–0.3, beneficiary end): They benefit from the rule that authority must answer to voters; their benefit is concentrated and direct (voting power); their costs are diffuse and modest (civil duty, taxation to fund elections). Exit options are constrained (citizenship-locked) but directionality is toward beneficiary because the constraint subsidizes their political power. DISENFRANCHISED_POPULATIONS (d ≈ 0.75–0.85, target end): They bear the costs of a legitimacy structure they cannot authorize; their benefit (living under law, theoretical access once they gain franchise) is structural and contingent on future status change; their costs are immediate and certain (subject to rules authored without them). Exit is trapped (cannot leave citizenship easily, cannot vote for change). ELECTED_REPRESENTATIVES (d ≈ 0.4–0.5, symmetric to slightly target): They gain power and legitimacy (beneficiary direction) but face removal and accountability (target direction). The balance depends on their actual electoral security; those with safe majorities trend toward beneficiary, those facing competitive pressure trend toward target. CONSTITUTIONAL_INTERPRETERS (d ≈ 0.35–0.45, near-symmetric): They are empowered by the legitimacy principle (they are its guardians) but constrained by it (their power must be justified through it). No simple directionality; the constraint both enables and constrains them. The authored directionality overrides (if needed) would map specific institutional seats to positions that the structural derivation might not capture — e.g., a court that has effectively foreclosed certain electoral outcomes might sit nearer the target end (bearing the cost of legitimacy maintenance without real removal risk).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK PRESENT: The founding problem (authority requires popular consent) remains live, but there is visible attenuation in the substantive legitimacy-grounding mechanism. The theater_ratio trend (0.15 → 0.28) indicates the mechanical form of elections persists (elections are held, votes are counted, representatives take office) while citizen participation and deliberation have thinned in many democracies. This is exactly the mandatrophy pattern: the founding mandate (popular authorization of authority) is performed but not substantively fulfilled. A regime that holds regular elections while suppressing actual deliberation, narrowing franchise again through effective (if not legal) means, or decoupling electoral outcomes from policy would satisfy the form while betraying the substance. The constraint is not yet a PITON (that would require theater >0.5 and beneficiaries dropping to near-zero), but the trajectory toward increasing performance (theater rising) while suppression requirement also rises (0.25 → 0.42) is a mandatrophy signature: the system must invest more enforcement (suppression) to maintain the same formal legitimacy, suggesting the substance is slipping. RESOLUTION: The constraint's classification depends on whether the rising theater and suppression indicate a system in transition (tightening around a shrinking legitimacy coalition) or a system normalizing to a sustainable (if attenuated) republican apparatus. The omegas addressing electoral-cycle sufficiency and suppression internalization are directly relevant: if majorities can be manufactured and participation can be engineered toward theater, then mandatrophy is active and the constraint could degrade toward snare (legitimacy claim becomes cover for majoritarian extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is legitimate authority grounded in upward-flowing popular consent (republican reading), downward-flowing inherited right (monarchical reading), or a hybrid of both (constitutional hybrid reading)?',
    'Sibling constraints sovereign_legitimacy__monarchical_reading and sovereign_legitimacy__constitutional_hybrid_reading model competing readings of the same kernel. The engine compares per-seat classifications across readings: if a seat computes the same type across readings, that indicates shared ground; divergent types indicate the reading-specific structural differences are material. Cross-reading comparison table in network documentation.',
    'The reading determines who counts as a beneficiary (those authorized to delegate) versus a victim (those excluded from delegation). It determines whether the constraint''s persistence depends on continuous electoral validation (republican) or on hereditary and institutional continuity (monarchical). Fundamental classification consequences for the same underlying authority arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The contested kernel: is legitimacy upward-flowing consent or downward-flowing inheritance?').

omega_variable(
    franchise_boundary_construction,
    'Who counts as ''the people'' whose consent legitimates authority? Is the franchise boundary itself subject to popular sovereignty, or is it a fixed category that precedes and constrains what ''the people'' can decide?',
    'Historical trajectory of suffrage expansion: each boundary shift (property requirements, gender, race, age) was justified either as completing the true scope of ''the people'' or as corrupting it. The resolution lies in which framing the political movement endorsing expansion used — did they claim ''the people'' was always wider, or that ''the people'' should be redefined? Post-expansion acceptance as legitimate depends on whether the new boundary is rationalized within the popular-sovereignty framework or appears as external imposition.',
    'If the franchise boundary is subject to popular sovereignty (the people can expand/contract it), then legitimacy is self-regulating and the constraint is higher-purity rope. If the boundary is fixed (e.g., citizenship as a pre-political category), then legitimacy for disenfranchised populations cannot be obtained through the mechanism, and extraction is structural. Current ε assumes the boundary is contestable within the republican frame (moderate extraction); if boundary is fixed, ε rises toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_boundary_construction, conceptual, 'Is the franchise boundary itself democratically revisable or is it pre-political?').

omega_variable(
    electoral_cycle_sufficiency,
    'Does periodic electoral authorization constitute sufficient grounds for legitimacy, or are additional ongoing-participation mechanisms (direct democracy, citizen juries, deliberative assemblies) required for the popular-sovereignty claim to hold?',
    'Empirical: study societies with episodic elections only (low ongoing participation) and those with continuous deliberative mechanisms (town halls, citizen panels, referenda). Survey whether citizens in each system perceive authority as legitimate and whether they experience the constraint as self-imposed or externally imposed. Conceptual: determine whether the reading''s core claim requires only periodic consent or treats it as a continuous condition.',
    'If episodic elections suffice, then the constraint can operate with relatively low participation theater (current ~0.28); if ongoing participation is required for legitimacy, then theater at this level would indicate mandatrophy — the form of elections persists but the substantive legitimacy foundation atrophies. This affects whether the constraint trends toward piton with rising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_cycle_sufficiency, empirical, 'Whether continuous participation or periodic elections suffices for popular-sovereignty legitimacy.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.42) structural — legal barriers, geographic/economic constraints on participation — or internalized — citizens have accepted the legitimacy premise and voluntarily limit their own demands?',
    'Post-constraint removal trajectory: if suppression drops sharply when legal/institutional barriers are removed, it was primarily structural. If it persists or rebounds within new institutional forms, it has internalized components (belief that authority deserves deference, identity-fusion with the legitimacy framework, learned helplessness). Survey disenfranchised populations in regimes that expand franchise: do they immediately mobilize voting power, or does participation lag? The lag indicates internalized suppression.',
    'Structural suppression can be reduced by lowering barriers; internalized suppression persists after barrier removal and suggests the constraint''s persistence depends partly on citizens'' acceptance of their own exclusion from legitimacy-authoring. Higher internalization indicates the constraint is more dependent on what citizens believe than on institutional architecture, shifting the classification toward snare-with-false-consciousness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Suppression mechanism: structural barriers vs. internalized acceptance.').

omega_variable(
    majoritarian_tyranny_extraction,
    'Does the constraint enable democratic majorities to rule, or does it enable majorities to extract from minorities through the voting mechanism itself? Is majoritarian tyranny a feature of the constraint or a pathology to be constrained within it?',
    'Study historical cases where electoral majorities imposed extractive policies on minorities (slavery expansion via voting, dispossession via referendum, majority-supported persecution). Determine whether the constraint''s theory (as distinct from constitutional guardrails layered atop it) has resources to condemn such outcomes or must treat them as legitimate exercises of popular sovereignty. If the theory cannot condemn majoritarian extraction, then the constraint''s own logic generates victims who cannot appeal to ''they had their consent chance.'' This reclassifies the constraint relative to the beneficiary-set (majority) and victim-set (trapped minority).',
    'If majoritarian extraction is internal to the constraint (not a guardrail failure), then the beneficiary/victim split is not between franchise/disenfranchised but between electoral majorities and electoral minorities. ε might be higher and suppression mechanism might be internalized (minorities accept ''they lost the vote fairly''). The constraint would trend toward snare for stable minorities, even when they hold franchise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_extraction, conceptual, 'Whether the constraint enables democratic legitimacy or legitimizes majoritarian extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t8, sovereign_legitimacy__republican_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(sove_tr_t8, observed).
narrative_ontology:measurement(sove_tr_t16, sovereign_legitimacy__republican_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement_basis(sove_tr_t16, observed).
narrative_ontology:measurement(sove_tr_t24, sovereign_legitimacy__republican_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(sove_tr_t24, observed).
narrative_ontology:measurement(sove_tr_t32, sovereign_legitimacy__republican_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement_basis(sove_tr_t32, observed).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__republican_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(sove_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t8, sovereign_legitimacy__republican_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(sove_be_t8, observed).
narrative_ontology:measurement(sove_be_t16, sovereign_legitimacy__republican_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement_basis(sove_be_t16, observed).
narrative_ontology:measurement(sove_be_t24, sovereign_legitimacy__republican_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement_basis(sove_be_t24, observed).
narrative_ontology:measurement(sove_be_t32, sovereign_legitimacy__republican_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(sove_be_t32, observed).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__republican_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(sove_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t8, sovereign_legitimacy__republican_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement_basis(sove_su_t8, observed).
narrative_ontology:measurement(sove_su_t16, sovereign_legitimacy__republican_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(sove_su_t16, observed).
narrative_ontology:measurement(sove_su_t24, sovereign_legitimacy__republican_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(sove_su_t24, observed).
narrative_ontology:measurement(sove_su_t32, sovereign_legitimacy__republican_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(sove_su_t32, observed).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__republican_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(sove_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__republican_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, franchise_boundary_contestation).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, electoral_cycle_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel sovereign_legitimacy. Sibling readings (monarchical_reading, constitutional_hybrid_reading) model the same authority arrangement under different legitimacy interpretations. The three constraints share the same referent (how legitimate authority is structured) but author different ε values, beneficiary/victim sets, and persistence mechanisms according to their readings. Network links enable cross-reading comparison: the engine can compute per-seat types across all three readings and identify which seats' classifications diverge by reading (those indicate reading-dependent structural sensitivity) versus converge (those indicate shared ground across readings). Comparison table in audits/constraint_families/sovereign_legitimacy_triplet/.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
