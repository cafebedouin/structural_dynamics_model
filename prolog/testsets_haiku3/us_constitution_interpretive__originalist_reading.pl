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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution anchors constitutional
 *   meaning to the text's original public meaning at the moment of
 *   ratification (1787 for the main text; 1868 for the Fourteenth Amendment).
 *   Interpretive authority flows through federal judges, especially the
 *   Supreme Court, who apply historical sources (founding-era documents,
 *   contemporary usage, structural inferences from fixed text) to constrain
 *   both their own discretion and federal legislative/executive power. This
 *   constraint simultaneously coordinates judicial interpretation (solving
 *   the problem of how judges should interpret without imposing preferences)
 *   and extracts from coalitions seeking regulatory expansion or unenumerated
 *   rights. The originalist reading competes with living constitutionalism
 *   (meaning evolves with values) and popular constitutionalism (meaning
 *   shaped by democratic contestation). This story instantiates ONLY the
 *   originalist reading as a self-contained constraint; sibling readings are
 *   separate stories. The claim/metric gap is deliberate: originalism is
 *   claimed as coordination (fixing meaning to prevent judicial amendment),
 *   while the authored metrics reflect that the constraint also operates as
 *   enforced extraction from progressive coalitions and regulatory advocates.
 *   The engine measures this structural tension; the claim and metrics remain
 *   independent.
 *
 * KEY AGENTS:
 *   - originalist_judicial_faction: Institutional power; sets and enforces originalist interpretive canon through Supreme Court and appellate opinions; constrains own discretion through historical fidelity.
 *   - federalism_advocates: Organized power; benefit from constrained federal authority; win cases on state reserved powers and Commerce Clause limits.
 *   - religious_liberty_claimants_original_scope: Organized power; benefit from narrow historical reading of Free Exercise protections.
 *   - property_rights_defenders: Powerful institutional actors; benefit from constrained federal regulatory authority over land, commerce, and takings.
 *   - unenumerated_rights_claimants: Moderate power, identity-locked; bear costs when courts reject claims to rights not explicitly textured in 1787.
 *   - federal_regulatory_expansion_advocates: Institutional power; bear costs when originalist courts invalidate broad federal statutes.
 *   - living_constitution_advocates: Excluded from authoritative interpretation when originalists control courts.
 *   - popular_constitutionalism_advocates: Excluded from direct interpretive authority.
 *   - conservative_political_coalition: Organized power; agenda-setter through judicial appointments and litigation funding; also benefits from favorable rulings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Constitutional Interpretation Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, 'f2040b94-ce70-45de-a77a-5243d06d3451').
narrative_ontology:cs_kernel_codification('f2040b94-ce70-45de-a77a-5243d06d3451', fixed_text).
narrative_ontology:cs_authority_grounding('f2040b94-ce70-45de-a77a-5243d06d3451', lineage).
narrative_ontology:cs_interpretation_layer_present('f2040b94-ce70-45de-a77a-5243d06d3451').
narrative_ontology:cs_reading_relation('f2040b94-ce70-45de-a77a-5243d06d3451', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2040b94-ce70-45de-a77a-5243d06d3451', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('f2040b94-ce70-45de-a77a-5243d06d3451', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('f2040b94-ce70-45de-a77a-5243d06d3451', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('f2040b94-ce70-45de-a77a-5243d06d3451', foundational, historical_constraint_prevents_amendment_through_discretion).
narrative_ontology:cs_axiom_status(historical_constraint_prevents_amendment_through_discretion, holdable).
narrative_ontology:cs_axiom_grounding('f2040b94-ce70-45de-a77a-5243d06d3451', historical_constraint_prevents_amendment_through_discretion, empirically_contingent).
narrative_ontology:cs_reference_frame('f2040b94-ce70-45de-a77a-5243d06d3451', founding_era_fixed_constitution).
narrative_ontology:cs_drift_state('f2040b94-ce70-45de-a77a-5243d06d3451', contemporary_originalist_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2040b94-ce70-45de-a77a-5243d06d3451', '2026-06-12T14:37:00Z').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_scope).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, originalist_legal_academy).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, conservative_political_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of federal judges, including multiple Supreme Court justices, whose interpretive method anchors constitutional meaning to the text's original public meaning at ratification. They enforce this reading through opinions that block expansive federal power, strike down regulations as beyond enumerated powers, and narrow the scope of unenumerated rights claims. Their enforcement mechanism is the power to invalidate statutes and executive actions; their constraint on their own power is fidelity to historical sources rather than contemporary preference.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judicial_faction, agenda_setter,
    institutional, generational, constrained, national).

% Conservative legal scholars, state governors and legislatures, and property-rights organizations benefit from originalist doctrine because it contracts federal legislative scope and preserves state reserved powers. They win legal disputes over regulatory authority, achieve favorable rulings on Commerce Clause limits, and defend against centralized mandates. Their exit is to accept broader federal power if the reading fails; their benefit is durable because the reading institutionalizes federalism as constitutional law.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Religious organizations and civil-liberties advocates who interpret the Free Exercise Clause (First Amendment) as covering a narrower but more defensible historical set of religious practices benefit when originalist judges narrow the government's regulatory authority over religion. They use originalist argument to block employment discrimination laws and healthcare mandates that would restrict their institutional practice. Their constraint is that the historical scope excludes some contemporary religious minority practices.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_scope, beneficiary,
    organized, generational, constrained, national).

% Corporate entities, large landowners, and libertarian organizations benefit from originalist limits on federal power to regulate commerce, take property for public use, and enforce environmental rules. Originalist doctrine narrows the Takings Clause and Regulatory Taking doctrine by tying permitted regulation to 1787 understandings, which excludes modern environmental and labor protections. They can exit by supporting living constitutionalism, but the constraint gives them reliable doctrine.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals and groups claiming rights not explicitly textured in the Constitution (privacy, dignity, autonomy, unenumerated discrimination protections) find their claims narrowed or rejected when originalist judges decline to locate them in the historical record. They cannot exit by relocating their identity or claim; they are locked into the jurisdiction. They bear the cost of narrowed constitutional protection.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    moderate, biographical, identity_locked, national).

% Executive agencies, congressional majorities seeking to enact broad regulatory programs, and advocates for federal civil rights and environmental protections pay the cost of originalist interpretation through invalidation of statutes they craft. The constraint blocks their preferred federal authority expansion. They can exit by supporting living constitutionalism, but doing so requires shifting their foundational legal strategy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    institutional, generational, constrained, national).

% Progressive legal scholars, civil-rights organizations, and Democratic legislative and executive leadership would argue for evolving constitutional meaning but are effectively excluded from authoritatively interpreting the Constitution when originalists hold majority power on courts. They mount intellectual opposition and legislative response (constitutional amendments, court-packing proposals), but the constraint's enforcement (judicial review power) keeps them from directly shaping constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitution_advocates, excluded,
    institutional, generational, constrained, national).

% Social movements and democratic activists arguing that constitutional meaning should be shaped by popular political contestation rather than judicial fidelity to framers' intent are excluded from the authoritative interpretive process because the constraint locates interpretive authority in courts applying historical sources, not popular movements. They can mobilize constitutional amendment campaigns but cannot directly alter the court's interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, popular_constitutionalism_advocates, excluded,
    organized, generational, constrained, national).

% Constitutional law scholars committed to originalist methodology benefit from the constraint through professional advancement, institutional funding, and intellectual validation. They produce the doctrinal infrastructure (historical scholarship, interpretive principles, strategic litigation frameworks) that makes originalist interpretation operationable. Their exit is to abandon originalism and accept methodological pluralism.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_legal_academy, beneficiary,
    organized, generational, mobile, national).

% The Republican Party and conservative political movements maintain and invest in originalist interpretation through judicial appointment, funding legal organizations that litigate originalist cases, and political messaging. They both set the agenda (by choosing which judges to nominate, which cases to fund) and benefit (through favorable rulings on federalism, religious liberty, and property rights). Their constraint is judicial dependence—they cannot control outcomes, only influence judicial appointment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, conservative_political_coalition, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, conservative_political_coalition, beneficiary).

% The institutional court itself administers and maintains originalist interpretation through precedent, sitting judges, and the momentum of doctrine. It faces no direct exit—the court persists regardless of reading choice—but observes the constraint's effects on its own legitimacy and on political pressure for reform.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, supreme_court_institution, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, conservative_political_coalition).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originalist interpretation coordinates the application of fixed constitutional text by constraining judicial discretion to a bounded set of legitimate meanings derivable from historical sources. It solves the problem of preventing unilateral judicial amendment of the Constitution by tethering judicial authority to textual meaning rather than contemporary judicial preference.
% TRANSFER_FUNCTION: Transfers interpretive authority away from democratically accountable branches (Congress, executive) to historically-grounded judicial readings; transfers doctrinal wins (federalism, religious liberty, property rights) to conservative coalitions; transfers losses (unenumerated rights, federal regulatory expansion) to progressive and administrative coalitions. Moves the locus of power from legislative majorities and presidential administration to the judiciary and to historical sources.
% ABSENT_VOICES: Living constitutionalists and popular constitutionalists are excluded from setting the authoritative interpretive frame when originalists hold judicial power; they can mount intellectual opposition and seek to overturn precedent through amendment or court composition change, but they are not seated at the interpretive table. Unenumerated rights claimants have no direct voice in what the historical sources reveal as protected.
% DISAPPEARANCE_RATIONALE: If originalist interpretive authority vanished overnight, federal power would expand substantially; unenumerated rights doctrine would broaden; state reserved powers would contract; religious liberty protections indexed to modern understanding would widen; property-rights regulatory takings would expand. The entire federal judiciary's approach to constitutional interpretation would shift, affecting hundreds of pending cases and decades of precedent.
% FOUNDING_PROBLEM: The founding problem originalism was built to solve: How can courts interpret the Constitution without simply imposing contemporary values under the guise of textual reading? How can the Constitution constrain power if its meaning shifts with each generation's preferences? Historical constraint is the originalist answer—if meaning is fixed at ratification, judges cannot unilaterally amend the document.
% FOUNDING_PROBLEM_CORROBORATION: Conservative legal scholars and federalism advocates attest the problem remains live: activist judges still read contemporary preferences into the text. Progressive constitutional scholars and civil-rights advocates attest the problem is overstated or misframed—that living constitutionalism better solves the real problem of constitutional legitimacy in a changing world. Legal historians and political scientists divided on whether the historical record sufficiently constrains interpretation or whether historical constraint is itself a contestable interpretive choice. Independent academic analysis (Balkin, Sunstein, Solum) documents that originalism both constrains and enables discretion depending on which historical sources are selected and how they are weighted.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.58 reflects that originalism operates as genuine coordination (it does constrain judges and provide predictability through historical constraint) layered with substantial extraction from progressive coalitions. The constraint transfers doctrinal authority away from electoral majorities toward courts and historical sources, which is asymmetric. Suppression is moderate (0.42) because the constraint persists through intellectual and institutional endorsement rather than raw coercion—living constitutionalists and regulatory advocates mount real intellectual opposition and seek court composition change, so they are not completely suppressed, but the institutional structure (lifetime judicial appointment, judicial review power) does suppress their direct voice. Theater ratio is 0.31: originalism involves real historical scholarship and genuine constraint, but a measurable portion of originalist adjudication is post-hoc rationalization fitting preferred outcomes to historical sources (selective source choice, outcome-oriented historical claims). The measurement series track the interval from early originalism (1980s–1990s, lower extraction, lower theater) through peak originalist institutional power (2010s–2020s, higher extraction, rising theater as institutional investment increases) to projected plateau. The single shared time grid ensures every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats experience radically different types. From the originalist judicial faction and federalism advocate seats, originalism is rope—genuine coordination solving the problem of judicial constraint + mutual benefit from predictable doctrine. From the unenumerated rights claimant and federal regulatory expansion advocate seats, the same structure is snare—coercive constraint with no exit and no compensating benefit; identity-locked claimants cannot even shop for alternative jurisdictions. The engine computes these per-seat divergences from the structural data. Originalist judges themselves experience originalism as self-imposed rope (they benefit from constraint theory validating their role); critics experience it as imposed snare. This divergence is the core finding of per-seat classification: the same constraint is different things from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges are structurally near the beneficiary end (d low to moderate): they set doctrine and their constraint is self-imposed fidelity to historical sources rather than external pressure. Federalism advocates are beneficiaries (d near 0.2–0.3): the constraint subsidizes their preferred outcomes. Religious liberty claimants and property-rights defenders are also beneficiaries (d near 0.1–0.4 depending on alignment with historical record). Unenumerated rights claimants are targets (d near 0.8–0.9): the constraint extracts from them by narrowing their recognized protections, and they are identity-locked (cannot exit by abandoning their identity). Federal regulatory expansion advocates are targets (d near 0.7–0.85): the constraint extracts from them through invalidation of federal statutes. Living constitutionalists are excluded rather than coordinated (not assigned d directly, but would be targets if included). Conservative political coalition sits near the beneficiary end (d moderate: they benefit from favorable rulings but depend on continued judicial support, so they are not fully insulated). The engine derives these directionalities from the stakeholder beneficiary/victim/power/exit data; they are not authored as explicit d values but emerge from the structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem originalism was built to solve—how to prevent judges from imposing contemporary preferences under the guise of interpretation—remains contested. Some originalists argue the problem is live (judges still read the document freely; historical constraint is still needed). Progressive critics argue the problem is misframed or dead (the real problem is judicial supremacy, not interpretive methodology; living constitutionalism solves constraint better). This contest is captured in the cs_structure drift_state: originalism's reference frame is 'fixed_constitutional_meaning_prevents_amendment_through_judicial_discretion,' but the drift_state shows 'axiom_overriding' pressure from empirical legal studies demonstrating that originalism does NOT empirically constrain judges as completely as the theory promises (outcome-orientation persists). The mandatrophy flag does not fire because disappearance_verdict=world_rearranges (the constraint is still substantive), but the omega variables document the founding-problem contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_determinacy,
    'How much do historical sources actually constrain interpretation? Can different judges examining the same founding-era sources reach radically different conclusions about original meaning?',
    'Systematic empirical analysis of originalist judicial decisions paired with historical-source documentation: do decisions with identical historical-source records vary in outcome by judge? Do judges selectively emphasize sources to reach preferred outcomes?',
    'If historical sources are indeterminate or judges systematically select sources post-hoc to justify outcomes, originalism''s core constraint mechanism fails—it becomes snare (imposed meaning justified by selective scholarship) rather than tangled_rope (genuine coordination + extraction). If sources are robustly constraining across judges, originalism retains coordination legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_determinacy, empirical, 'Whether originalism''s historical constraint is robust or post-hoc rationalization.').

omega_variable(
    founding_problem_validity,
    'Is the founding problem originalism was built to solve—judicial amendment through discretionary interpretation—still live, or has it been misframed or solved?',
    'Comparative constitutional analysis: do non-originalist judiciaries (EU, Canada, other democracies) experience runaway judicial amendment, or do they maintain constitutional constraint through other mechanisms? If living constitutionalism elsewhere produces stable constitutional order without originalist constraint, the founding problem is not necessarily live.',
    'If the founding problem is dead or misframed, originalism persists as a doctrine benefiting certain coalitions (federalism, religious liberty, property rights) through institutional lock-in rather than through solving a live coordination problem—reclassification pressure toward snare/piton. If the problem remains live, originalism retains coordination justification despite extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_validity, conceptual, 'Whether the founding problem originalism solves is still live or has been superseded.').

omega_variable(
    originalism_as_alternative_reading,
    'Is originalism itself a reading of contested meaning (competing with living constitutionalism), or is it a methodology that objectively recovers fixed meaning?',
    'Philosophical analysis of reading theories: originalism claims to recover objective meaning, but can it be shown to depend on contestable interpretive choices (which historical sources count, how to weight evidence, what ''original public meaning'' means)? If interpretive theory forces originalism into the reading class alongside living constitutionalism, the constraint becomes a competition between readings rather than a constraint on reading.',
    'If originalism is a reading rather than objective methodology, it is structurally equivalent to living constitutionalism—both are contestable frames on the kernel. Reclassification pressure: originalism becomes less a constraining coordinate and more an extractive assertion of one reading''s authority over others. Affects legitimacy claims and mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_as_alternative_reading, conceptual, 'Whether originalism is a reading or a methodology that recovers objective constitutional meaning.').

omega_variable(
    identity_lock_mechanism_unenumerated_rights,
    'Is the identity-lock of unenumerated rights claimants structural (the constraint prevents constitutional recognition regardless of external changes) or internalized (claimants have internalized the constraint as natural law and would carry suppression even if the constraint were removed)?',
    'Post-constraint counterfactual: if the court shifted to living constitutionalism and recognized unenumerated rights based on contemporary values, would the previously suppressed claimants immediately assert their claims (structural lock), or would they maintain suppression patterns due to internalized defeat?',
    'If suppression is structural, removing originalism would immediately enable unenumerated rights claims (world_rearranges). If suppression is internalized, even removal of originalism would not restore the claiming function immediately (world_changes_partially). Affects theater_ratio interpretation and suppression mechanism analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_unenumerated_rights, empirical, 'Whether suppression of unenumerated rights claims is structural or internalized.').

omega_variable(
    originalism_kernel_reading_status,
    'Is originalism correctly modeled as ONE reading of a contested kernel (the US Constitution), or is it a methodological super-rule that applies across all interpretation contexts?',
    'Survey of originalist claims: do originalists argue for the specific kernel reading (constitutional meaning is fixed at ratification), or do they advance a general methodological thesis (all texts have fixed original meaning)? If the latter, originalism is not kernel-specific but domain-general.',
    'If kernel-specific, originalism''s alternatives are living constitutionalism and popular constitutionalism (other readings of same kernel). If domain-general, originalism''s alternatives are competing methodologies (textualism, intentionalism, etc.) across multiple kernels. Affects network structure and contraint decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_kernel_reading_status, conceptual, 'Whether originalism is a kernel reading or a domain-general methodology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_interpretive__originalist_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__originalist_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_interpretive__originalist_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__originalist_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_interpretive__originalist_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__originalist_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(us_c_tr_t35, us_constitution_interpretive__originalist_reading, theater_ratio, 35, 0.32).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__originalist_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(us_c_be_t5, us_constitution_interpretive__originalist_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__originalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(us_c_be_t15, us_constitution_interpretive__originalist_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__originalist_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(us_c_be_t25, us_constitution_interpretive__originalist_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__originalist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(us_c_be_t35, us_constitution_interpretive__originalist_reading, base_extractiveness, 35, 0.59).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__originalist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(us_c_su_t5, us_constitution_interpretive__originalist_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__originalist_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(us_c_su_t15, us_constitution_interpretive__originalist_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__originalist_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(us_c_su_t25, us_constitution_interpretive__originalist_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__originalist_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(us_c_su_t35, us_constitution_interpretive__originalist_reading, suppression_requirement, 35, 0.43).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__originalist_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% The us_constitution_interpretive kernel decomposes into three constraint stories corresponding to three competing readings: originalist_reading (this story), living_constitution_reading (meaning evolves with values), and popular_constitutionalism_reading (meaning shaped by democratic contestation). Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different classifications. The three readings coexist across different courts and political coalitions; none foreclosed the others within a single framework. Each story links to the others via network.affects_constraints to indicate kernel kinship and mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
