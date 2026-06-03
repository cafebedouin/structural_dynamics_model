% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence: Conceptualization Reading (Theory-First Boundary)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint captures one reading of a deeply contested kernel: WHEN
 *   did digital money emerge? The conceptualization reading places emergence
 *   at the theoretical formalization moment (1960s telecommunications
 *   advances enabling digital transfer concepts; 1985 David Chaum's
 *   cryptographic protocols formalizing digital cash). This reading is in
 *   direct structural tension with two sibling readings: the infrastructure
 *   reading (emergence at the technology deployment moment: 1967 ATMs, 1972
 *   ACH, 1977 SWIFT) and the consumer holdings reading (emergence when
 *   end-users could directly hold digital instruments: 1990s e-purses, 2000
 *   Electronic Money Directive). Each reading instantiates a different
 *   constraint with distinct beneficiaries, victims, and extraction
 *   mechanisms. The conceptualization reading benefits the academic research
 *   community and cryptography pioneers by establishing their theoretical
 *   contributions as the 'true' origin of digital money. It imposes costs on
 *   monetary authorities (whose regulatory framework must be retroactively
 *   extended) and on regulatory clarity (the boundary is definitionally
 *   unstable as new historical theory emerges). The constraint exhibits
 *   genuine coordination (Chaum's formalization DID enable subsequent
 *   infrastructure development) bundled with extraction (the research
 *   community's priority claim constrains regulatory authority's historical
 *   coherence).
 *
 * KEY AGENTS:
 *   - Academic Research Community: Primary beneficiary (institutional/arbitrage) — establishes conceptual priority for 1960s-1985 theoretical contributions; captures reputation and funding
 *   - Cryptography Pioneers: Primary beneficiary (organized/mobile) — David Chaum, Whitfield Diffie, Martin Hellman establish foundational protocols; their work is validated as the 'true' origin
 *   - Monetary Authorities: Primary victim (institutional/constrained) — must retroactively revise regulatory historical boundaries; face definitional instability as new theory is discovered
 *   - Regulatory Clarity: Secondary victim (powerless/trapped) — abstract collective good; cannot organize or exit the definitional boundary disputes; each new theory extends emergence further backward
 *   - Technology Standards Bodies: Secondary actor (organized/mobile) — benefit from clear theoretical foundation but constrained by multiple competing boundary claims
 *   - Historical Economic Canonicity: Institutional performer (institutional/arbitrage) — textbooks assign origin dates; the assignment is increasingly performative as multiple valid readings proliferate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.38).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.48).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence: Conceptualization Reading (Theory-First Boundary)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '9abcef25-88fb-400a-afe8-a69fe2c1d5cf').
narrative_ontology:cs_kernel_codification('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', formalized).
narrative_ontology:cs_authority_grounding('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', distributed).
narrative_ontology:cs_reading_relation('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', foundational, theoretical_formalization_constitutes_emergence).
narrative_ontology:cs_axiom_status(theoretical_formalization_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', theoretical_formalization_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', secondary, academic_priority_legitimate_origin_claim).
narrative_ontology:cs_axiom_status(academic_priority_legitimate_origin_claim, holdable).
narrative_ontology:cs_axiom_grounding('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', academic_priority_legitimate_origin_claim, conventional).
narrative_ontology:cs_reference_frame('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', conceptual_formalization_framework).
narrative_ontology:cs_drift_state('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', contemporary_regulatory_multiplicity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9abcef25-88fb-400a-afe8-a69fe2c1d5cf', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_research_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, cryptography_pioneers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, monetary_authority_conceptual_coherence).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, regulatory_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONETARY AUTHORITY CONCEPTUAL FRAMEWORK (SNARE) — The definitional boundary moves backwards in time as theory solidifies, retroactively expanding what 'counts' as digital money. Monetary authorities cannot exit this revision trap: accepting the conceptualization reading means accepting that money emerged decades before regulatory infrastructure existed. The constraint extracts legitimacy cost from the monetary system's authority.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC RESEARCH COMMUNITY (ROPE) — Benefits from establishing conceptual priority: the 1960s-1985 window becomes the 'true' origin, elevating the theoretical contributions of information theorists, cryptographers, and telecommunications researchers. The research community coordinates knowledge through publication and formalization. Experiences the constraint as legitimate priority-staking rather than extraction.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: FINANCIAL REGULATORS (TANGLED ROPE) — Regulators benefit from the clarity that theoretical formalization provides (Chaum's protocols enable precise regulatory definitions) but bear the cost of needing to revise the historical boundary of regulation. They are constrained by international coordination requirements and cannot easily revise statutory definitions retroactively. Mixed coordination (the theory enables regulation) and extraction (the boundary revision exposes gaps in regulatory authority).
constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECHNOLOGY STANDARDS BODIES (SCAFFOLD) — ISO, W3C, and other standards organizations see the conceptualization boundary as a temporary coordination problem with a clear sunset: once the academic-to-infrastructure transition is complete (expected by 2030s), the conceptual debate will settle and standards will stabilize. Low effective extraction because these organizations have agency and see the endpoint.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL ECONOMIC CANONICITY (PITON) — Textbooks and economic histories perform their canonical role (assigning a definitive origin date) but the performance is increasingly hollow as historians discover earlier theoretical work and as contemporary emergence definitions proliferate. The ritual of assigning 'the' origin date persists through institutional inertia despite the structural multiplicity of valid readings. Theater is high because the canonical date is partly performative assertion rather than empirically settled.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a universal/civilizational perspective, the theoretical formalization of digital money IS a genuine coordination achievement: Chaum's protocols provide a rigorous framework that enabled subsequent infrastructure development. But this achievement is bundled with an extraction mechanism: the conceptualization reading naturalizes a particular historical origin that benefits theorists and researchers while constraining regulators and historical accuracy. The constraint is legitimately both coordination and extraction.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_money_emergence_boundary__conceptualization_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, TR),
    TR >= 0.70.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The research community's gain from establishing conceptual priority is real and significant (reputation, funding, historical recognition) but bounded. Chaum's 1985 formalization IS genuinely foundational — subsequent digital cash research built directly on it. The extraction is not maximal because the coordination benefit is also genuine: the theoretical framework enables infrastructure development. Suppression (0.48): Moderate. The constraint suppresses alternative boundary claims (infrastructure and consumer-holdings readings become 'secondary' or 'derivative') and suppresses the straightforward use of contemporary regulatory definitions. But suppression is not total — the sibling readings remain intellectually live and influence policy. Theater ratio (0.65): Moderate-high. Economic histories perform the ritual of assigning 'the' origin date, but the performance is increasingly unstable. As historians excavate earlier theoretical work (1960s Bell Labs studies, 1970s Merkle-Hellman proposals) and as contemporary definitions proliferate (Central Bank Digital Currencies, stablecoins, programmable money), the canonical date becomes less a factual claim and more a narrative assertion. The theater reflects the growing gap between the textbook origin story and the actual multiplicity of valid emergence moments.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps in this constraint are radical. The research community sees the constraint as pure coordination (Rope) — theoretical formalization enabling infrastructure. Regulators see it as extraction (Snare) — a boundary redefinition that destabilizes their authority. The analytical observer sees both (Tangled Rope) — genuine coordination bundled with real extraction. Financial regulators see a temporary problem (Scaffold) with a sunset as standards settle. Historical canonicity sees a performative ritual (Piton) that persists through inertia. The gap reveals that 'emergence' is not a factual moment but a contested definition whose placement benefits different actors differently. No single perspective captures 'when digital money actually emerged' — the constraint IS the contest over that definition.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value for each perspective is derived from the agent's structural relationship to the extraction flow. The research community (beneficiary + arbitrage) experiences low/negative effective extraction — they are capturing value from the constraint. Monetary authorities (victim + constrained) experience higher effective extraction — they bear definitional and regulatory costs. The analytical observer at institutional power with constrained exit (embedded in the dispute) experiences high extraction. The standards bodies with mobile options experience moderate extraction. The powerless monetary authority conceptual framework experiences maximum extraction (trapped, victim status, institutional scale). The piton perspective (historical canonicity) experiences low theater-driven extraction because its primary mechanism is performative maintenance, not coercive extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the theoretical emergence reading is BOTH coordination (Chaum's formalization genuinely enabled digital cash research) AND extraction (the research community's priority claim constrains regulatory authority). The mandatrophy is not 'which is it?' but 'how much of each?' The measurements show increasing theater ratio (0.42→0.65) and extractiveness (0.22→0.38), indicating that as the theoretical reading gains academic acceptance, its extractive dimension (constraining alternative readings, suppressing regulatory authority's definitional autonomy) intensifies while its coordination function (enabling new research) stabilizes. The constraint is legitimately Tangled Rope — it solves the coordination problem of defining digital money formally while extracting legitimacy cost from monetary authorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theory_implementation_gap,
    'Does a theoretical formalization that lacks implementation constitute the emergence of digital money, or only the emergence of the concept?',
    'Empirical tracking of which reading correlates with contemporary monetary policy''s operative definition of digital money emergence; identification of which boundary regulators and central banks explicitly adopt',
    'If theory-only is accepted: emergence boundary moves to 1960s-1985, M4/M5 aggregates become historically problematic. If implementation required: boundary moves to 2000s, conceptualization reading becomes aspirational rather than descriptive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theory_implementation_gap, empirical, 'Gap between theoretical formalization and operational implementation').

omega_variable(
    research_priority_vs_monetary_reality,
    'Is the research community''s interest in establishing conceptual priority aligned with or opposed to monetary authority''s interest in coherent historical boundaries?',
    'Analysis of academic citation patterns and research funding flows during 1960s-1985 period; comparison with regulatory intent in subsequent monetary policy documents',
    'If aligned: the constraint is pure coordination (Rope). If opposed: the constraint is extraction (Snare or Tangled Rope). Current evidence suggests opposition — incentive structures differ fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_priority_vs_monetary_reality, empirical, 'Alignment or opposition between research priority-setting and regulatory boundary-setting').

omega_variable(
    retroactive_definition_legitimacy,
    'Is it analytically valid to apply contemporary definitions of ''digital money'' to theoretical work that predates the technology and regulatory frameworks those definitions assume?',
    'Philosophical analysis of anachronism; determination of whether definitions are descriptive (capturing what agents intended) or prescriptive (assigning modern meaning retroactively)',
    'If retroactive application is invalid: the conceptualization reading is category error, not genuine emergence boundary. If valid: the boundary extends backwards in time as theory solidifies, creating permanent instability in historical accounting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retroactive_definition_legitimacy, conceptual, 'Validity of retroactive application of modern definitions to historical theory').

omega_variable(
    beneficiary_asymmetry_in_origin_claims,
    'Does the academic research community''s structural benefit from claiming earlier origin dates (1960s theory vs 2000s infrastructure) represent a coordination function or an extraction mechanism?',
    'Analysis of citation advantage, funding flows, and professional reputation accrual for researchers claiming 1960s origins vs those claiming 2000s infrastructure emergence',
    'If coordination: the constraint is Rope or Tangled Rope with genuine mutual benefit. If extraction: the constraint is Snare, with researchers capturing reputation and funding at the cost of regulatory clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_in_origin_claims, empirical, 'Whether academic origin-dating provides coordination or enables extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmec_tr_t0, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dmec_tr_t15, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(dmec_tr_t30, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(dmec_be_t0, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dmec_be_t15, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(dmec_be_t30, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dmec_su_t0, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dmec_su_t15, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(dmec_su_t30, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The digital money emergence boundary decomposes into three structurally distinct constraints based on which observable (theoretical formalization, infrastructure deployment, or consumer access) is used to determine 'emergence.' Each reading produces a different ε value and a different set of victims and beneficiaries. The three constraints form a constraint family linked by network edges. Sibling constraints share the same kernel_id but different reading_id values in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
