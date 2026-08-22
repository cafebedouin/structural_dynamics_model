% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary — Conceptualization Reading (1960s Telecom Advances / 1985 Chaum Formalization)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the earliest-boundary reading of a contested
 *   historiographic kernel: when did 'digital money' emerge? This reading
 *   locates emergence at the point of theoretical thinkability — 1960s
 *   telecommunications advances that made remote electronic transaction
 *   conceivable, formalized rigorously by David Chaum's 1985 blind-signature
 *   scheme. The coordination function is genuine: fields need a citable
 *   origin point for textbooks, legal filings, and retrospective surveys. But
 *   the boundary also does extractive work — it concentrates founder status
 *   and patent-priority leverage in the research community that produced the
 *   earliest formal paper, at the expense of the engineers who built
 *   circulating electronic-value systems (ATMs, ACH, SWIFT) and the
 *   consumer-holdings deployments that gave ordinary people digital
 *   instruments to actually hold and spend. The claim is authored as
 *   tangled_rope because both functions are real and co-present: it does
 *   solve a genuine historiographic coordination problem AND it does
 *   asymmetrically transfer credit/leverage toward the earliest-formalization
 *   community, requiring active maintenance (citation norms, patent
 *   litigation posture, textbook conventions) to hold against competing
 *   infrastructure- and deployment-based boundary claims.
 *
 * KEY AGENTS:
 *   - cryptography_research_community: Primary agenda-setter and beneficiary (organized/arbitrage) — sets the historiographic convention and collects founder status
 *   - priority_claiming_academics: Beneficiary (moderate/arbitrage) — individual careers ride on the conceptualization boundary being accepted
 *   - digital_currency_patent_holders: Beneficiary (powerful/arbitrage) — litigation leverage strengthens with an earlier priority date
 *   - infrastructure_era_engineers: Payer (moderate/constrained) — their operational contribution is subordinated to theoretical priority
 *   - central_bank_historians: Payer (institutional/constrained) — cannot operationalize 'potential money' in actual M1-M5 measurement
 *   - later_entrant_researchers: Payer (moderate/constrained) — cast as derivative rather than co-originating
 *   - monetary_taxonomy_analysts: Analytical observer — studies the boundary-drawing contest itself without a stake in the outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.38).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.28).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary — Conceptualization Reading (1960s Telecom Advances / 1985 Chaum Formalization)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'b596bba4-7d53-4505-afc5-4c033e8514f0').
narrative_ontology:cs_kernel_codification('b596bba4-7d53-4505-afc5-4c033e8514f0', distributed).
narrative_ontology:cs_authority_grounding('b596bba4-7d53-4505-afc5-4c033e8514f0', expertise).
narrative_ontology:cs_interpretation_layer_present('b596bba4-7d53-4505-afc5-4c033e8514f0').
narrative_ontology:cs_reading_relation('b596bba4-7d53-4505-afc5-4c033e8514f0', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('b596bba4-7d53-4505-afc5-4c033e8514f0', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('b596bba4-7d53-4505-afc5-4c033e8514f0', foundational, theoretical_specifiability_constitutes_emergence).
narrative_ontology:cs_axiom_status(theoretical_specifiability_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('b596bba4-7d53-4505-afc5-4c033e8514f0', theoretical_specifiability_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('b596bba4-7d53-4505-afc5-4c033e8514f0', secondary, priority_of_conception_over_deployment).
narrative_ontology:cs_axiom_status(priority_of_conception_over_deployment, holdable).
narrative_ontology:cs_axiom_grounding('b596bba4-7d53-4505-afc5-4c033e8514f0', priority_of_conception_over_deployment, conventional).
narrative_ontology:cs_reference_frame('b596bba4-7d53-4505-afc5-4c033e8514f0', theoretical_specifiability_as_sufficient_origin).
narrative_ontology:cs_drift_state('b596bba4-7d53-4505-afc5-4c033e8514f0', post_deployment_era_retrospective, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b596bba4-7d53-4505-afc5-4c033e8514f0', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, priority_claiming_academics).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, digital_currency_patent_holders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, infrastructure_era_engineers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, central_bank_historians).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, later_entrant_researchers).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, theoretical_possibility_constitutes_emergence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the historiographic framing that dates digital money's origin to the moment blind-signature and anonymous-credential schemes became mathematically thinkable, centering David Chaum's 1985 paper and 1960s telecommunications-enabled remote transaction theory. Sets citation lineages, textbook narratives, and conference retrospectives that anchor 'digital money' to conceptual breakthrough rather than deployed instrument. Benefits from being named originators of a trillion-dollar-adjacent field regardless of whether any of the described systems ever processed a real transaction.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community, agenda_setter,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community, beneficiary).

% Individual researchers and their institutions collect prestige, tenure cases, grant competitiveness, and named-lecture invitations from being positioned as intellectual originators of digital currency. Their claim to founder status depends entirely on the conceptualization boundary being accepted as the true emergence point rather than a preliminary theoretical stage superseded by deployed systems.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, priority_claiming_academics, beneficiary,
    moderate, biographical, arbitrage, global).

% Entities holding early cryptographic-payment patents (e.g. blind signature patents) derive litigation and licensing leverage from an emergence date fixed at theoretical formalization rather than deployment — a 1985 priority date reaches further back than a 1990s or 2000s deployment date, strengthening claims against later independently-developed systems.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, digital_currency_patent_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Engineers who built ATM networks (1967), ACH (1972), and SWIFT (1977) — the systems that actually moved electronic value at scale — find their contribution reframed as mere 'infrastructure' preceding the 'real' conceptual emergence, or as posterior implementation of ideas that had already been generally 'thinkable.' Their historical credit is diminished by a boundary that privileges formal theory over operational deployment. They have no forum to contest the framing; the academic literature that sets emergence dates is not their venue.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_era_engineers, payer,
    moderate, biographical, constrained, global).

% Monetary historians and central bank archivists who track money supply measures (M1-M5) find the conceptualization boundary analytically unworkable — it would require accounting for 'potential money' that existed only as an unimplemented cryptographic scheme, never circulated, never held a value, and therefore breaks the practical measurement apparatus they are responsible for maintaining. They must either reject the boundary in their own reporting (creating a two-track discourse) or awkwardly footnote it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_historians, payer,
    institutional, generational, constrained, national).

% Researchers who made independent contributions to digital cash schemes after 1985 (extending, correcting, or practically realizing early proposals) are structurally cast as followers rather than co-originators once the emergence date is fixed at the earliest formalization. Their work is read through a derivative lens even where it solved problems Chaum's scheme left open.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, later_entrant_researchers, payer,
    moderate, biographical, constrained, global).

% Economic historians and monetary theorists who study how 'emergence' boundaries get drawn across competing readings (conceptualization vs. infrastructure vs. consumer holdings), without a stake in which boundary wins, but responsible for making the boundary choice explicit rather than allowing it to default silently to whichever community writes the retrospective.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, monetary_taxonomy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixing an emergence date for 'digital money' coordinates citation practice, historiography, and priority attribution across a research field — without a dating convention, every retrospective survey would need to re-litigate which prior work counts as the origin.
% TRANSFER_FUNCTION: Moves credit, priority, and downstream patent/citation leverage from later deployers and infrastructure builders toward the community that produced the earliest theoretical formalization — the 'first to conceive' captures status that would otherwise be distributed across 'first to build' and 'first to deploy at scale.'
% ABSENT_VOICES: The ATM/ACH/SWIFT engineering communities and the consumer-e-purse deployment teams are not in the room when the emergence date is set in cryptography and monetary-theory literature; they would argue that a concept with no circulating instance and no transacting party is not yet 'money' in any economically meaningful sense.
% DISAPPEARANCE_RATIONALE: If the conceptualization boundary were abandoned in favor of an infrastructure or consumer-holdings dating convention, the cryptography community's founder narrative would lose its temporal priority, patent-litigation arguments anchored to 1985 would weaken, but the underlying research itself (blind signatures, anonymous credentials) would remain scientifically unchanged — only its label as 'the origin of digital money' would rearrange, not the technical content.
% FOUNDING_PROBLEM: The field needed a principled account of when digital money 'began' to organize its own historiography, assign credit for foundational insight, and give textbooks and legal filings a citable starting point.
% FOUNDING_PROBLEM_CORROBORATION: Cryptography researchers themselves attest the boundary is correct and necessary for accurate intellectual history. Central bank historians and monetary economists (outside the benefiting research community) attest the boundary is analytically unworkable for actual money-supply measurement and that 'potential money' that never circulated does not meet any operational definition of money they use.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, contested).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).
:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) — the transfer of credit and patent leverage is real but diffuse and reputational rather than a direct financial extraction from an identifiable payer class; it rises over the measured interval as the conceptualization boundary hardens into textbook orthodoxy and gets cited in patent litigation, entrenching the asymmetry. Suppression is moderate-low (0.28) — dissenting historiographic framings (infrastructure-first, deployment-first) are not forcibly excluded, merely under-resourced relative to the well-organized cryptography research community's citation machinery. Theater ratio is notable and rising (0.20 to 0.42) — a growing share of the boundary's defense consists of retrospective narrative-building (conference keynotes, 'father of digital cash' framings) rather than substantive historiographic argument, which is exactly the drift the theater_ratio metric is meant to catch. Accessibility collapse is moderate (0.4): once a field's citation convention sets around a boundary, switching costs for rewriting textbooks and legal arguments are real but not insurmountable. Resistance is moderate (0.35), reflecting active pushback from monetary historians and infrastructure-era engineers who dispute the framing in their own literatures.
 *
 * PERSPECTIVAL GAP:
 *   From the cryptography research community's seat, the conceptualization boundary is simply correct intellectual history — money is a system for value transfer, and once that system is mathematically specified, the essential emergence has occurred regardless of deployment. From the infrastructure engineers' and central bank historians' seats, the same boundary looks like retrospective credit inflation: a scheme that was never implemented, never held value, and never circulated cannot meaningfully be 'money' by any operational definition, and privileging it above working electronic-transfer systems inverts the actual causal history of how digital value moved.
 *
 * DIRECTIONALITY LOGIC:
 *   The cryptography research community and patent holders sit near the beneficiary end: they set the framing (agenda_setter) and collect status/leverage from it (arbitrage exit — they can always retreat to 'it's just intellectual history' if challenged). Infrastructure engineers, central bank historians, and later entrant researchers sit nearer the target end: they bear the diminished-credit cost through a discourse they do not control and cannot easily exit (constrained — their professional identity and institutional role require continuing to operate inside monetary/engineering historiography even as it is reframed around them).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing SOME citable origin point for a fast-moving research field — was live and legitimate in the 1980s-1990s when digital money was purely theoretical. Whether it remains live today (with three decades of deployed digital payment systems since) is contested: the coordination need for a dating convention persists, but the specific conceptualization-first convention increasingly serves credit-allocation and patent-litigation functions that have drifted from the original coordination purpose. This is precisely the tangled_rope pattern — a real coordination function (citable origin) persists alongside an asymmetric extraction function (credit concentration) that has grown as the field matured, and disentangling them requires exactly this kind of decomposed, ε-invariant story rather than a single flattened judgment about whether 'the emergence boundary' is good or bad.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading (conceptualization_reading) of the contested kernel digital_money_emergence_boundary. The infrastructure_reading dates emergence to 1967-1977 electronic transfer systems (ATMs, ACH, SWIFT); the consumer_holdings_reading dates it to 1990s-2000 consumer-held digital instruments (e-purses, EMD). Where exactly do these readings disagree structurally?',
    'The disagreement is located in what counts as the necessary condition for ''money'' to exist: this reading holds that theoretical/mathematical specifiability of a value-transfer scheme is sufficient (money exists once it is thinkable and formally correct); the infrastructure_reading holds that operational movement of value between real parties is necessary (money exists once value actually moves electronically, regardless of whether anyone has theorized why it works); the consumer_holdings_reading holds that individual possession and discretionary transaction by ordinary holders is necessary (money exists once a person, not an institution, can hold and spend the instrument). A sibling reading would change the beneficiary set entirely: infrastructure_reading''s beneficiaries are ATM/ACH/SWIFT-building institutions and telecom-banking engineers; consumer_holdings_reading''s beneficiaries are e-purse issuers and payment-card standards bodies (e.g., those behind the 2000 EU Electronic Money Directive).',
    'If the field converged on the infrastructure_reading or consumer_holdings_reading as canonical, this reading''s beneficiary community (cryptography researchers, patent holders anchored to 1985) would lose temporal priority and the associated credit/litigation leverage — the underlying research would be unchanged but its historiographic status as ''the origin'' would not survive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locates the structural disagreement between the three sibling readings of the digital money emergence kernel: sufficiency of theoretical specifiability vs. operational value-transfer vs. individual discretionary holding.').

omega_variable(
    potential_money_measurement_problem,
    'Can ''potential money'' — a formally specified but never-implemented value-transfer scheme — be meaningfully incorporated into monetary aggregates (M1-M5), or does the conceptualization boundary produce a category that monetary measurement cannot operationalize?',
    'Attempt to construct a monetary aggregate that includes theoretically-specified-but-undeployed digital cash schemes from the 1980s and observe whether it produces any non-arbitrary, non-zero measurement, or whether it collapses to measuring citation counts rather than economic quantity.',
    'If no coherent measurement is possible, the conceptualization_reading''s claim to be about ''money'' rather than ''the history of an idea about money'' weakens substantially, supporting a reclassification of this reading''s function as almost entirely credit-allocation rather than economically descriptive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_money_measurement_problem, empirical, 'Tests whether the conceptualization boundary can survive contact with actual monetary measurement practice.').

omega_variable(
    founder_status_naturalness,
    'Is the concentration of ''digital money founder'' status in the cryptography research community a natural consequence of genuine intellectual priority, or a constructed outcome of which community controls the retrospective historiography?',
    'Compare citation and credit patterns across fields with different historiographic control structures — e.g., whether telecommunications engineering histories that are NOT written primarily by cryptographers show different founder attributions for the same underlying events (1960s remote transaction telecom advances).',
    'If founder attribution tracks which community writes the history rather than independent measures of causal contribution, this substantially strengthens the tangled_rope reading over a pure-rope reading of this constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_status_naturalness, conceptual, 'Whether founder-status concentration is a natural priority signal or a constructed historiographic artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(digi_tr_t8, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(digi_tr_t16, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(digi_tr_t24, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(digi_tr_t32, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(digi_tr_t40, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(digi_be_t8, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(digi_be_t16, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(digi_be_t24, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(digi_be_t32, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(digi_be_t40, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 40, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(digital_money_emergence_boundary__conceptualization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.1).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the digital_money_emergence_boundary kernel, decomposed per the ε-invariance principle because the three readings assign structurally different ε values, different beneficiary sets, and different classifications (this reading: tangled_rope, ε=0.38; siblings authored separately). The conceptualization_reading dates emergence earliest (1960s-1985) and is authored here as a hybrid coordination/extraction structure. The infrastructure_reading (1967-1977) and consumer_holdings_reading (1990s-2000) are separate constraint files linked here; each should in turn link back to this constraint_id in its own network.affects_constraints array to complete the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
