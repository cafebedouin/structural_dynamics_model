% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__parmenidean_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_zero_mathematical_status__parmenidean_rejection, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: zero_mathematical_status__parmenidean_rejection
 *   human_readable: Parmenidean Exclusion of Zero from the Number Domain
 *   domain: history of mathematics / philosophy of mathematics / conceptual history
 *
 * SUMMARY:
 *   A dominant ontological doctrine — that non-being cannot be an object of
 *   thought, and therefore that zero, the number of nothing, is incoherent —
 *   governed the number concept across the Hellenic and Latin-scholastic
 *   worlds for roughly two millennia. The doctrine presents itself as a
 *   discovered limit on being: nothing cannot exist, so no number of nothing
 *   can exist, so arithmetic simply has no zero. Structurally, the exclusion
 *   is maintained by identifiable institutions that collect from it (the
 *   metaphysical schools that administer it, the doctrinal offices that
 *   enforce it, the reckon-masters whose fees depend on scarce computational
 *   skill) and imposes real costs on identifiable populations (merchants,
 *   astronomical computers, and algebraists who need positional efficiency
 *   and null-case expressibility). This story instantiates the
 *   parmenidean_rejection reading of the kernel zero_mathematical_status; the
 *   placeholder and number readings are separate constraints in separate
 *   files. The claim/metric gap is deliberate and disclosed: claimed_type is
 *   mountain because that is the doctrine's own self-presentation (a law of
 *   being, not a policy), while the authored metrics describe enforced,
 *   extractive operation — the false-summit signature this corpus exists to
 *   catch. Beneficiaries are declared intentionally to trigger FSM
 *   evaluation, and the schema-required omega documenting the
 *   natural-law-versus-constructed ambiguity is authored below. KEY AGENTS
 *   (by structural relationship): - peripatetic_metaphysicians: agenda-setter
 *   and principal beneficiary (institutional/identity_locked) — administer
 *   the exclusion through curriculum and licensure; collect ontological
 *   authority - ecclesiastical_scholastic_authorities: co-agenda-setter
 *   (institutional/identity_locked) — bind the doctrine into doctrinal
 *   enforcement - greek_geometric_tradition: beneficiary
 *   (institutional/constrained) — magnitude-supremacy preserved by the
 *   exclusion - professional_reckonmasters: dual beneficiary/payer
 *   (organized/constrained) — scarcity rents from hard arithmetic while
 *   paying the same inefficiency - mercantile_accountants: primary payer
 *   (moderate/constrained) — bear computational cost; defect informally in
 *   scratch work - astronomical_computers: payer (organized/constrained) —
 *   run placeholder workarounds under official denial - nascent_algebraists:
 *   payer (powerless/trapped) — null cases inexpressible within the doctrine
 *   - hindu_arabic_algorists: excluded (organized/mobile) — hold the solved
 *   alternative, outside the adjudicating conversation -
 *   historians_of_mathematics: analytical observer — sees the full
 *   transmission and enforcement record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, 0.42).
domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, 0.44).
domain_priors:theater_ratio(zero_mathematical_status__parmenidean_rejection, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, extractiveness, 0.42).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__parmenidean_rejection, mountain).
narrative_ontology:human_readable(zero_mathematical_status__parmenidean_rejection, "Parmenidean Exclusion of Zero from the Number Domain").
narrative_ontology:topic_domain(zero_mathematical_status__parmenidean_rejection, "history of mathematics / philosophy of mathematics / conceptual history").

domain_priors:requires_active_enforcement(zero_mathematical_status__parmenidean_rejection).
domain_priors:emerges_naturally(zero_mathematical_status__parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__parmenidean_rejection, '74ad7226-b4fc-45d7-bd6f-cedd7d3806e0').
narrative_ontology:cs_kernel_codification('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', fixed_text).
narrative_ontology:cs_authority_grounding('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', lineage).
narrative_ontology:cs_interpretation_layer_present('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0').
narrative_ontology:cs_reading_relation('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_reading_relation('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', zero_mathematical_status__placeholder_reading, coexists_with).
narrative_ontology:cs_axiom('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', foundational, nonbeing_cannot_be_instantiated).
narrative_ontology:cs_axiom_status(nonbeing_cannot_be_instantiated, holdable).
narrative_ontology:cs_axiom_grounding('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', nonbeing_cannot_be_instantiated, deontological).
narrative_ontology:cs_axiom('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', foundational, number_denotes_counted_beings).
narrative_ontology:cs_axiom_status(number_denotes_counted_beings, holdable).
narrative_ontology:cs_axiom_grounding('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', number_denotes_counted_beings, deontological).
narrative_ontology:cs_axiom('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', secondary, arithmetic_subordinate_to_geometry).
narrative_ontology:cs_axiom_status(arithmetic_subordinate_to_geometry, holdable).
narrative_ontology:cs_axiom_grounding('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', arithmetic_subordinate_to_geometry, conventional).
narrative_ontology:cs_reference_frame('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', parmenidean_plenum_ontology).
narrative_ontology:cs_drift_state('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', early_modern_algorism_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('74ad7226-b4fc-45d7-bd6f-cedd7d3806e0', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, peripatetic_metaphysicians).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, ecclesiastical_scholastic_authorities).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, greek_geometric_tradition).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, professional_reckonmasters).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, mercantile_accountants).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, astronomical_computers).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, nascent_algebraists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, professional_reckonmasters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and administer the doctrine that non-being cannot be an object of thought or arithmetic. Set the curriculum in which number is defined as multitude of counted units and measurable magnitude, examine candidates for licensure, and supply the refutations of the void that anchor the position. Their authority rests on the framework's stability; revising the number concept would require surrendering the ontological system their office guards. Exit would mean abandoning the tradition that constitutes them.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, peripatetic_metaphysicians, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Bind the doctrine into doctrinal and legal enforcement: condemnations of suspect teachings, scrutiny of foreign computational arts, and the framing of void and nothing as theological errors. Collect deference and institutional control from administering the boundary. Their commitment is total — the doctrine is woven into creation-doctrine and cannot be revised without doctrinal rupture.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, ecclesiastical_scholastic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Practice mathematics as the science of magnitude and proportion. The exclusion keeps number subordinate to geometry: no number-zero exists to mediate between discrete multitude and continuous magnitude, so the Euclidean program's hierarchy is preserved. They bear little of the computational cost and lose the option of arithmetizing geometry.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, greek_geometric_tradition, beneficiary,
    institutional, generational, constrained, continental).

% Earn their living computing for clients with counting boards and Roman numerals. Artificially hard arithmetic protects their fees and guild standing — every difficulty is billable expertise. They also pay the same inefficiency in their own daily work, and they resist the printed algorisms that would commodify their skill.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, professional_reckonmasters, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__parmenidean_rejection, professional_reckonmasters, payer).

% Keep ledgers, convert currencies, and compute interest across trading networks using zero-less numerals. Each calculation costs extra labor and error risk; double-entry bookkeeping waits on better notation. They adopt Hindu-Arabic figures informally in scratch work while keeping Roman numerals in official books to satisfy statute and custom.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, mercantile_accountants, payer,
    moderate, immediate, constrained, continental).

% Compute planetary tables and calendar corrections in sexagesimal fractions. They introduce a bare placeholder mark into their tables to hold empty places — a symbol stripped of number-status — because tabular computation without it is nearly unmanageable. Officially they affirm the doctrine; operationally they route around it.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, astronomical_computers, payer,
    organized, generational, constrained, global).

% Work on equations and unknowns at the margins of the schools. Within the doctrine, an equation with no positive solution has no answer at all: the null case is inexpressible, so whole classes of problems cannot be stated, let alone solved. Exiting the framework means leaving mathematics as it is institutionally constituted.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, nascent_algebraists, payer,
    powerless, biographical, trapped, regional).

% Carry a completed alternative: positional notation with a zero that has defined operations, tested over centuries from India through the Islamic world. They are not seated in the schools where the number concept is adjudicated; their results enter Latin Europe through translation channels and are received as foreign craft rather than as testimony in the ontological dispute.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, hindu_arabic_algorists, excluded,
    organized, generational, mobile, global).

% Reconstruct the transmission record, the ban decrees, and the adoption curves. They see the full structure: which communities solved the problem, when the solution was available to Europe, and what the schools did with it. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__parmenidean_rejection, peripatetic_metaphysicians).
narrative_ontology:fixing_cost_class(zero_mathematical_status__parmenidean_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within the Parmenidean-Aristotelian consensus, the exclusion coordinated a shared ontology: it fixed what may count as an object of number (existing units and measurable magnitudes only), kept arithmetic aligned with geometry, and gave the schools a common, teachable line on being and non-being. It also, less admirably, coordinated professional calculators around the scarcity of computational skill.
% TRANSFER_FUNCTION: Transfers computational ease and expressive power away from everyone who calculates — merchants, astronomers, navigators, accountants, algebraists — and transfers ontological authority, curricular control, and episodically fee-protecting scarcity to the metaphysical schools, the church's doctrinal offices, and the reckon-master guilds.
% ABSENT_VOICES: The computing classes had no seat where the number concept was adjudicated: merchants and navigators stood outside the universities; the Indian and Islamic mathematicians who had already solved the problem stood outside the civilization's conversation entirely, entering only as anonymous foreign craft; the household and convent account-keepers who did much of the era's arithmetic are wholly unrecorded in the dispute. Unanimity in the schools was purchased by the room's composition.
% DISAPPEARANCE_RATIONALE: The rearrangement already happened and is the recorded outcome: where the exclusion lapsed, positional notation displaced board reckoning within generations, ledger-keeping and navigation transformed, algebra acquired null solutions and then general equation theory, and the later analysis of the infinite presupposes zero as a bona fide number. Conversely, while the exclusion held, entire problem classes were unstatable. Arrangements demonstrably depended on it — and rearranged demonstrably when it went.
% FOUNDING_PROBLEM: Naive admission of zero into a framework that defines numbers as counts of existing units generates incoherence the available apparatus cannot repair: a number of nothing, self-cancellation, division by zero, and a void that contradicts plenum cosmology. The exclusion kept the number concept free of objects the framework could not define.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: Aristotle's own refutation-of-void literature (Physics IV) shows the hazard was live and seriously argued at founding; historians of Greek and medieval mathematics attest the exclusion was doctrinal rather than a limit of technical capacity; and the millennium-long, paradox-free operation of zero-inclusive arithmetic in the Indian-Islamic line attests the hazard was solvable. After circa 1200 CE no source outside the schools' own disputation practice supports the exclusion's continued necessity — that absence is itself signal.
narrative_ontology:disappearance_verdict(zero_mathematical_status__parmenidean_rejection, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__parmenidean_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__parmenidean_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__parmenidean_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__parmenidean_rejection, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__parmenidean_rejection_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, ExtMetricName, E),
    domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_mathematical_status__parmenidean_rejection),
    narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_mathematical_status__parmenidean_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is mountain because the doctrine presents itself as ontological necessity — 'nothing cannot exist' is offered as a law of being, not an enacted rule — and emerges_naturally is true because that is what the claim asserts; the engine owns certification. The metrics describe the arrangement's actual operation at interval end: extractiveness 0.42 (down from a 0.68 peak at the ban-era maximum, when the gap between available positional efficiency and permitted practice was widest), suppression 0.44 (down from a 0.68 enforcement peak as print culture made numeral bans unenforceable), theater_ratio 0.66 (above the 0.5 Goodhart line: by the early modern period the doctrine survives mainly as disputation performance — staged refutations of a void already demonstrated — while practice has defected). Accessibility_collapse is low (0.30) because the alternative never collapsed: Hindu-Arabic algorism persisted at the margins for centuries and was fully available once transmitted. Resistance is high (0.72) because the constraint ultimately met mass defection and lost. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. All three temporal series share one eight-point grid (0, 350, 700, 1050, 1400, 1750, 1950, 2100), so no metric borrows another's end-state at earlier times. Rising base_extractiveness across 0–1750 should trip the mountain-extraction-accumulation hypothesis for investigation. The axiom grounding_types are deontological deliberately: the doctrine's warrants ('what is not cannot be thought') are a priori and not empirically falsifiable, so they must not route to evidence-based foreclosure — its displacement was pragmatic and civilizational, not observational. Coordination type is identity_coordination because the doctrine's live function is boundary maintenance over the number concept and membership in the ontological consensus; the gaming risk this choice carries (identity framing as extraction cover) is exactly what the hazard_genuineness_vs_cover omega tests. Receipt: the arrangement's yield — curricular control, licensure authority, and the deference paid to ontological guardianship — accrues demonstrably to the administering school seat; the reckon-masters' fee-protection is episodic and derivative, so gain_flow names peripatetic_metaphysicians rather than diffuse.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the divergence is the finding. From the school seat the arrangement is a discovered limit: administrators experience no extraction because they collect the yield, and identity-lock makes the alternative literally unthinkable rather than merely unavailable — a mountain-shaped world. From the payer seats the same structure is enforced extraction with a known, circulating alternative: merchants experience billable inefficiency, algebraists experience unstatable problems, and both can name the fix. From the excluded algorist seat the dispute is surreal — a solved problem defended as unsolvable. The reckon-master seat is genuinely split: beneficiary of scarcity, victim of the same inefficiency, which is why it carries a secondary payer role rather than being flattened into either pole. The engine computes these per-seat classifications from the structural data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the schools, the geometric tradition, and the church offices: the exclusion subsidizes their authority and coherence, so effective extraction inverts toward subsidy at those seats. Victim declarations drive high directionality for merchants, astronomical computers, and algebraists; among them exit quality orders the severity — algebraists are trapped (no legitimate place for null cases inside the framework, and exit means leaving mathematics), merchants are constrained (informal scratch-work arbitrage softens but does not remove the cost), and astronomical computers are constrained with a partial workaround (the placeholder mark) that blunts their extraction below the pure-target end. The reckon-masters derive ambiguously from their dual declaration; their net position is beneficiary-side but materially dampened by the costs they themselves pay. No directionality overrides are used: the beneficiary/victim structure plus exit atoms already encode the relationships, and an override keyed to a power atom would collide across the several agents sharing each atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — naive zero is incoherent given a framework that defines numbers as counts of existing beings — was real at founding and is dead now: it was solved outside the tradition (Brahmagupta's rules, the Islamic algebraists, the modern field-theoretic definition), and the exclusion persisted roughly five centuries past solvability, increasingly as performance. Mandatrophy is therefore resolved, and the classification work cuts both ways. Reading the arrangement as pure extraction would erase the genuine coordination it performed for the ontological consensus (a real, if costly, service to its own community); reading it as the mountain it claims to be would erase two millennia of transferred computational cost and the enforced suppression of a known alternative. The end-state profile — high theater, eroding suppression, hollowed extraction — is piton-shaped drift on a hybrid body, which is what the temporal series is there to date. Fixing cost: for the seat that could fix it, fixing meant dissolving the framework that constituted its authority — an identity cost, not a materials cost; the technical fix (adopt transmitted algorism) was cheap and proven, which is precisely why the incumbents' cost was existential. fixing_cost is authored prohibitive on that evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_construction_of_number_domain,
    'Is the exclusion of zero from the number domain a discovered limit on being — nothing cannot exist, therefore no number of nothing — or a constructed boundary that identifiable traditions maintain because they benefit from it?',
    'Comparative-civilizational natural experiment: number systems that admit zero (Indian, Islamic, later commercial-European) operate coherently for a millennium without ontological collapse; if coherent zero-inclusive arithmetic exists and spreads, the exclusion tracks inherited metaphysics rather than structural necessity.',
    'Resolves the false-summit question: if constructed, the mountain claim fails and the constraint classifies among the enforced hybrids; if genuine, the declared beneficiaries are incidental to a real limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturality_vs_construction_of_number_domain, empirical, 'Whether the number-domain boundary is natural law or maintained construction.').

omega_variable(
    hazard_genuineness_vs_cover,
    'Did the exclusion solve a real conceptual hazard — naive zero generates incoherences (self-cancellation, division by zero, void against plenum cosmology) that the available apparatus could not repair — or was hazard-framing cover for disciplinary protection?',
    'Check the schools'' engagement with the transmitted definitions: Brahmagupta''s operation rules reached Latin translation by the twelfth century. If the definitions were available and were ignored or banned rather than examined and refuted on stated grounds, the hazard-framing is protective cover.',
    'Separates a hybrid with a genuine coordination component from pure enforced extraction riding an ontological story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hazard_genuineness_vs_cover, empirical, 'Whether the founding hazard was real and engaged, or invoked without examination.').

omega_variable(
    suppression_structural_or_internalized,
    'Was the doctrine''s hold on individual scholars structural (curriculum, licensure, patronage, statute) or internalized (trained conviction that zero-talk is sophistry, persisting after institutional pressure ends)?',
    'Biographical trajectories across the transmission era: scholars who left the schools and promptly adopted algorism versus those who carried the rejection into independent work; conversion latency after exit.',
    'If substantially internalized, effective suppression exceeds the structural measure and exit positions soften more slowly than enforcement records suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized mechanism of the doctrine''s hold.').

omega_variable(
    kernel_framing_under_determination,
    'Is the right framing of this kernel the ontological thesis itself (whether nothing can be numbered) or the disciplinary-boundary arrangement layered above it (who controls the number concept and licenses its revision)?',
    'Compare enforcement patterns against the two framings: if enforcement tracks institutional boundaries (schools, statutes, guilds) rather than ontological argument, the boundary-arrangement framing is the operative one.',
    'Under the boundary framing the constraint reads as enforced extraction with an ontological cover story; under the thesis framing it reads as a defended metaphysical commitment with incidental beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Two coherent framings of the kernel yield different classifications.').

omega_variable(
    placeholder_refuge_effect,
    'Did the availability of the placeholder reading — zero as an empty-place marker without number-status — absorb computational pressure and thereby extend this reading''s life?',
    'Compare adoption timelines across regions and registers: tabular astronomy ran on placeholder marks for centuries while number-status was denied; test whether full number-adoption accelerated where no placeholder refuge existed.',
    'If the refuge prolonged the exclusion, the sibling reading is structurally upstream of this one''s persistence, and network influence runs from placeholder tolerance to rejection longevity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placeholder_refuge_effect, empirical, 'Whether notational half-measures subsidized the doctrine''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__parmenidean_rejection, 0, 2100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zms_parmenidean_tr_t0, zero_mathematical_status__parmenidean_rejection, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zms_parmenidean_tr_t350, zero_mathematical_status__parmenidean_rejection, theater_ratio, 350, 0.12).
narrative_ontology:measurement(zms_parmenidean_tr_t700, zero_mathematical_status__parmenidean_rejection, theater_ratio, 700, 0.18).
narrative_ontology:measurement(zms_parmenidean_tr_t1050, zero_mathematical_status__parmenidean_rejection, theater_ratio, 1050, 0.25).
narrative_ontology:measurement(zms_parmenidean_tr_t1400, zero_mathematical_status__parmenidean_rejection, theater_ratio, 1400, 0.32).
narrative_ontology:measurement(zms_parmenidean_tr_t1750, zero_mathematical_status__parmenidean_rejection, theater_ratio, 1750, 0.45).
narrative_ontology:measurement(zms_parmenidean_tr_t1950, zero_mathematical_status__parmenidean_rejection, theater_ratio, 1950, 0.58).
narrative_ontology:measurement(zms_parmenidean_tr_t2100, zero_mathematical_status__parmenidean_rejection, theater_ratio, 2100, 0.66).

% Extraction over time
narrative_ontology:measurement(zms_parmenidean_be_t0, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(zms_parmenidean_be_t350, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 350, 0.38).
narrative_ontology:measurement(zms_parmenidean_be_t700, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 700, 0.48).
narrative_ontology:measurement(zms_parmenidean_be_t1050, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 1050, 0.52).
narrative_ontology:measurement(zms_parmenidean_be_t1400, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 1400, 0.6).
narrative_ontology:measurement(zms_parmenidean_be_t1750, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 1750, 0.68).
narrative_ontology:measurement(zms_parmenidean_be_t1950, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement(zms_parmenidean_be_t2100, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 2100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(zms_parmenidean_su_t0, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(zms_parmenidean_su_t350, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 350, 0.4).
narrative_ontology:measurement(zms_parmenidean_su_t700, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 700, 0.45).
narrative_ontology:measurement(zms_parmenidean_su_t1050, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 1050, 0.5).
narrative_ontology:measurement(zms_parmenidean_su_t1400, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 1400, 0.58).
narrative_ontology:measurement(zms_parmenidean_su_t1750, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 1750, 0.68).
narrative_ontology:measurement(zms_parmenidean_su_t1950, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(zms_parmenidean_su_t2100, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 2100, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__parmenidean_rejection, identity_coordination).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__placeholder_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__number_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the status of zero' decomposes per the epsilon-invariance principle into three structurally distinct constraints sharing the kernel zero_mathematical_status: number_reading (zero is a number with defined operations — settled, negligible extraction), placeholder_reading (zero is a notational device without arithmetic status — functional but deliberately inert), and this story, parmenidean_rejection (zero excluded outright — enforced, extractive). The readings differ in epsilon because they differ in what they permit: each admits a different object into practice. Edges: this reading forecloses number_reading within any single Parmenidean framework and coexists with placeholder_reading (tabular astronomy ran placeholder marks for centuries under official denial of zero-numberhood); the placeholder refuge likely subsidized this reading's persistence by absorbing computational pressure without conceding the ontological point. Number_reading, once established, retroactively exposes this reading's founding hazard as solved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
