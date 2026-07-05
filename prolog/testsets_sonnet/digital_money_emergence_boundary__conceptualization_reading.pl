% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This story instantiates the earliest-boundary reading of the contested
 *   'digital money emergence' kernel: the claim that digital money emerged
 *   when it became theoretically thinkable, anchored to 1960s
 *   telecommunications advances that made electronic value transfer
 *   conceivable and formalized by David Chaum's 1985 blind-signature
 *   cryptography paper. This is a genealogical/priority claim internal to the
 *   cryptography and theoretical computer science research tradition,
 *   structurally distinct from the infrastructure reading (which dates
 *   emergence to deployed transfer systems like ATMs, ACH, SWIFT) and the
 *   consumer-holdings reading (which dates emergence to direct consumer
 *   digital instrument holding, e.g. e-purses and the 2000 E-Money
 *   Directive). The three readings have different beneficiaries, different
 *   evidentiary bases, and would require different monetary-statistics
 *   treatments — this reading alone would require M4/M5 aggregates to somehow
 *   account for 'potential money,' theoretical constructs never actually in
 *   circulation, which is analytically incoherent for central bank
 *   statisticians. Per the ε-invariance principle, these are three separate
 *   constraints, not one constraint viewed three ways; this file covers only
 *   the conceptualization reading.
 *
 * KEY AGENTS:
 *   - cryptography_research_community: agenda_setter/beneficiary (institutional/arbitrage) — administers the historiographical framing and collects citation/prestige capital
 *   - priority_claiming_academics: beneficiary (moderate/constrained) — career and tenure narratives ride on 1985 being the origin point
 *   - chaum_lineage_researchers: beneficiary (moderate/constrained) — institutional identity built on the conceptualization boundary holding
 *   - infrastructure_engineering_historians: payer (moderate/constrained) — displaced from 'first mover' narrative slot
 *   - central_bank_monetary_statisticians: payer (institutional/constrained) — bear the analytic cost of an unusable emergence boundary intruding into policy discourse
 *   - policy_historians: observer (analytical) — compares the three readings without adjudicating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.28).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.15).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary — Conceptualization Reading (1960s Telecom Advances / 1985 Chaum Formalization)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '6171b5a3-b60b-409f-9233-a8d597896709').
narrative_ontology:cs_kernel_codification('6171b5a3-b60b-409f-9233-a8d597896709', distributed).
narrative_ontology:cs_authority_grounding('6171b5a3-b60b-409f-9233-a8d597896709', practice).
narrative_ontology:cs_interpretation_layer_present('6171b5a3-b60b-409f-9233-a8d597896709').
narrative_ontology:cs_reading_relation('6171b5a3-b60b-409f-9233-a8d597896709', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('6171b5a3-b60b-409f-9233-a8d597896709', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('6171b5a3-b60b-409f-9233-a8d597896709', foundational, theoretical_specifiability_constitutes_emergence).
narrative_ontology:cs_axiom_status(theoretical_specifiability_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('6171b5a3-b60b-409f-9233-a8d597896709', theoretical_specifiability_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('6171b5a3-b60b-409f-9233-a8d597896709', secondary, formal_priority_claims_ground_disciplinary_credit).
narrative_ontology:cs_axiom_status(formal_priority_claims_ground_disciplinary_credit, holdable).
narrative_ontology:cs_axiom_grounding('6171b5a3-b60b-409f-9233-a8d597896709', formal_priority_claims_ground_disciplinary_credit, conventional).
narrative_ontology:cs_reference_frame('6171b5a3-b60b-409f-9233-a8d597896709', cryptographic_formalization_priority_tradition).
narrative_ontology:cs_drift_state('6171b5a3-b60b-409f-9233-a8d597896709', post_cryptocurrency_boom_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('6171b5a3-b60b-409f-9233-a8d597896709', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, priority_claiming_academics).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, chaum_lineage_researchers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, infrastructure_engineering_historians).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, central_bank_monetary_statisticians).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, theoretical_possibility_constitutes_monetary_emergence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the historiographical framing that dates digital money's emergence to theoretical formalization (Chaum's 1985 blind-signature paper, earlier telecom-enabled thought experiments) rather than to deployed infrastructure or consumer uptake. Citation counts, textbook narratives, and disciplinary prestige flow to this framing; the community administers conference retrospectives, encyclopedia entries, and foundational-paper canons that fix the boundary at the moment of conceptual thinkability.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community, beneficiary).

% Individual researchers whose careers and tenure cases rest on being cited as having 'first thought of' digital cash. They benefit directly when the emergence boundary is drawn at the theoretical-formalization moment rather than at later deployment, since deployment credit typically accrues to engineers and firms outside academia. Exit from this framing would cost them priority and citation standing already banked.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, priority_claiming_academics, beneficiary,
    moderate, biographical, constrained, global).

% Students, collaborators, and intellectual descendants of David Chaum whose scholarly identity and grant narratives are built around 1985 as the canonical origin point of digital money. Their institutional positioning (cryptographic cash research centers, blockchain-history courses) depends on the conceptualization boundary holding as the authoritative one.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, chaum_lineage_researchers, beneficiary,
    moderate, generational, constrained, global).

% Historians and economists who trace digital money's emergence through the material build-out of ATMs, ACH, and SWIFT bear a narrative cost when the conceptualization framing dominates: their causal story (money emerges when it can actually move value between real accounts) gets treated as a secondary or derivative account. Their work is not suppressed by force, but it is displaced from the 'first mover' slot in textbooks and popular retellings that instead credit theoretical papers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_engineering_historians, payer,
    moderate, generational, constrained, global).

% Central banks constructing M1-M5 monetary aggregates need a workable, measurable emergence boundary. A conceptualization-dated boundary is analytically unusable for their purposes — it would require counting 'potential money' that never entered circulation, contaminating monetary statistics with unmeasurable theoretical constructs. They must either ignore the conceptualization reading entirely or spend analytic resources rebutting it when it surfaces in policy discourse, a cost imposed by the reading's persistence in academic and popular discourse.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_monetary_statisticians, payer,
    institutional, civilizational, constrained, national).

% Compare the three competing emergence-boundary readings (conceptualization, infrastructure, consumer holdings) to understand how disciplinary incentives shape historical periodization. They document which reading serves which community without adjudicating a single 'true' boundary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, policy_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared reference point within the cryptography and theoretical-computer-science research tradition for when digital money became a coherent intellectual object — allowing researchers, funders, and historians of that tradition to coordinate citations, retrospectives, and disciplinary genealogies around a single canonical origin moment.
% TRANSFER_FUNCTION: Moves historiographical credit, citation capital, and disciplinary prestige toward the theoretical/cryptographic research community and away from infrastructure engineers and central-bank statisticians, by fixing the 'emergence' label at the moment of conceptual thinkability rather than at deployment or circulation.
% ABSENT_VOICES: Infrastructure engineers who built ATMs, ACH, and SWIFT, and central bank statisticians who must operationalize monetary aggregates, are rarely consulted when the conceptualization boundary is asserted in popular and academic narratives; they encounter the framing only after it is already institutionalized in textbooks and encyclopedia entries.
% DISAPPEARANCE_RATIONALE: If the conceptualization boundary vanished as the accepted framing, the cryptography research community's priority narratives would lose their anchor point and would need to be rebuilt around a different origin story (world_rearranges for that community); but for the broader monetary system, payment infrastructure, and actual money supply, nothing would change — no transaction, account, or aggregate depends on when theorists say digital money became 'thinkable' (world_unchanged for the monetary system). The verdict is genuinely split by which world is asked.
% FOUNDING_PROBLEM: The problem the conceptualization boundary claims to solve is establishing intellectual priority and a coherent history of ideas: when did digital money become a rigorously specified concept rather than science fiction or hand-waving? Chaum's 1985 paper on blind signatures gave cryptographic privacy-preserving digital cash a formal mathematical treatment for the first time.
% FOUNDING_PROBLEM_CORROBORATION: Cryptography historians and Chaum's own citation record attest the 1985 paper is a genuine formal milestone. However, monetary economists and central bank researchers outside the cryptography community (e.g., BIS and Federal Reserve historical papers on payment system evolution) attest that 'emergence' in a monetary-economics sense requires circulating instruments accepted in exchange, and that the theoretical-formalization framing answers a different question (when was digital cash mathematically specified) than the one monetary history needs answered (when did digital money exist). No corroboration exists from outside the cryptography/computer-science community for treating conceptual formalization as monetary emergence per se.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, contested).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-low (0.28) because the primary transfer is reputational/disciplinary credit rather than material resource extraction — a real but narrow rent. Suppression is low (0.15) because no one is coercively barred from advancing the infrastructure or consumer-holdings readings; they simply compete for narrative dominance in textbooks and popular history, which is soft displacement, not coercion. Theater ratio is notably higher (0.42) and rising over the interval: as the conceptualization framing became canonical in cryptography curricula and retrospective histories, an increasing share of its maintenance became performative (anniversary papers, 'father of digital cash' tributes, conference retrospectives) rather than functional coordination of ongoing research priority-setting. Accessibility collapse is moderate (0.35): the alternative framings remain fully articulable and are actively maintained by rival disciplinary communities, so the collapse is partial, not complete.
 *
 * PERSPECTIVAL GAP:
 *   From the cryptography research community's seat, this is legitimate intellectual history — correctly crediting the moment a rigorous formal treatment made digital cash a specifiable object, which is a real and defensible act of disciplinary coordination (a rope-like function: establishing shared reference points for citation and pedagogy). From the central bank statistician's seat, the same boundary is close to unusable noise that must be actively rebutted whenever it leaks into policy or monetary-aggregate discourse — an extractive intrusion of academic priority politics into empirical measurement practice. The engine should compute these as different seat-level classifications from the same structural data; the divergence is exactly what a tangled-rope hybrid encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   The cryptography research community and its lineage academics are declared beneficiaries because the conceptualization framing directly produces the citation capital, disciplinary prestige, and pedagogical anchor points they collect — d sits near the beneficiary end for these seats. Infrastructure historians and central bank statisticians are declared victims not because anything is extracted from them coercively, but because they bear the narrative-displacement and analytic-friction costs of a competing origin story dominating discourse — d sits toward the target end, though moderated by their genuine institutional power (they are not powerless, so the effective extraction stays modest).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving digital cash a rigorous formal treatment — was genuinely live in 1985 and is now largely solved; cryptographic digital cash schemes are a mature, well-specified subfield. What persists past that resolution is the boundary-claiming function: continuing to assert 1985-as-origin serves disciplinary credit allocation rather than any live coordination need. This is not pure extraction (the coordination function of citation/pedagogy is real and ongoing) nor is it a dead husk (the cryptography community continues to actively use and defend the framing) — it sits as a tangled rope precisely because both the coordination function and the extractive displacement of rival framings persist simultaneously, requiring the community's active curatorial enforcement (canon-setting in textbooks, retrospectives, obituary framings) to hold the boundary in place against the competing infrastructure and consumer-holdings readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptualization_vs_realization_boundary,
    'Does ''emergence'' properly denote the moment a phenomenon becomes theoretically specifiable, or only the moment it becomes materially real and operative? This is the crux the three kernel readings dispute.',
    'No empirical resolution is available — this is a conceptual/definitional dispute about what ''money'' and ''emergence'' mean, not a fact about the world. It could be partially informed by examining how other technologies'' histories are periodized (is the transistor''s ''emergence'' dated to 1947 physics or to 1954 commercial transistor radios?) for a consistency check, but the underlying question remains a framing choice.',
    'If emergence requires material realization, this conceptualization_reading constraint is a category error — it should not be treated as a competing account of monetary history at all, only as intellectual history of cryptography. If theoretical thinkability suffices, the conceptualization_reading has equal or superior claim to the infrastructure and consumer-holdings readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptualization_vs_realization_boundary, conceptual, 'Whether emergence denotes theoretical specifiability or material realization — the kernel''s central contested premise.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the classification of this reading change if merged analytically with the infrastructure_reading or consumer_holdings_reading rather than kept as a separate story?',
    'Compare the three linked constraint stories'' computed types once all three are generated; examine whether their beneficiary/victim sets overlap enough that a merged treatment would suppress a real structural distinction (per the ε-invariance decomposition test).',
    'If merged, the extraction/beneficiary signal from the cryptography-priority function would be diluted by averaging with the infrastructure and consumer-holdings readings'' very different beneficiary structures (payment-network operators; e-money regulators), likely masking the tangled-rope signature this story identifies. Keeping them separate, as done here, preserves the distinct ε for each.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Whether decomposing the kernel into three separate stories (as done) versus one merged story changes the detectable classification.').

omega_variable(
    chaum_priority_contestability,
    'Is the 1985 Chaum paper genuinely the first rigorous formalization of digital cash, or does this claim itself reflect selective citation practices within the cryptography community that marginalize earlier or parallel theoretical work?',
    'Systematic literature review of pre-1985 cryptographic and telecommunications-theory publications addressing electronic value transfer, cross-checked against non-Chaum-lineage historians of computer science.',
    'If earlier formalizations exist and are excluded from the canon, this strengthens the tangled-rope reading — the boundary-setting function actively suppresses alternative priority claims even within the theoretical tradition, not just against rival infrastructure/consumer-holdings framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chaum_priority_contestability, empirical, 'Whether the 1985 date itself, within the theoretical tradition, reflects genuine priority or curated canon-formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(digi_tr_t1968, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(digi_tr_t1976, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(digi_tr_t1992, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1992, 0.4).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(digi_be_t1968, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1968, 0.14).
narrative_ontology:measurement(digi_be_t1976, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1976, 0.18).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.24).
narrative_ontology:measurement(digi_be_t1992, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1992, 0.27).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2000, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(digi_su_t1968, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1968, 0.11).
narrative_ontology:measurement(digi_su_t1976, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1976, 0.12).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.14).
narrative_ontology:measurement(digi_su_t1992, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1992, 0.15).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.08).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the digital_money_emergence_boundary kernel. infrastructure_reading dates emergence to 1967-1977 deployed transfer systems (ATMs/ACH/SWIFT); consumer_holdings_reading dates it to 1990s-2000 consumer-held digital instruments (e-purses/EMD). Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct monetary-statistics implications, and is authored as a separate constraint file per the ε-invariance principle. The conceptualization_reading (this file) is the earliest-dated and most contested of the three, since it requires accounting for 'potential money' that never entered circulation — a category most monetary economists outside the cryptography tradition reject as incoherent for M4/M5 purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
