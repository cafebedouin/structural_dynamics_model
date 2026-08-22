% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Digital Money Emergence Boundary — Conceptualization Reading (1960s Telecom Theory, Chaum 1985)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the conceptualization_reading of the
 *   digital_money_emergence_boundary kernel: digital money is held to have
 *   emerged when it became theoretically thinkable — 1960s
 *   telecommunications-theoretic groundwork culminating in David Chaum's 1985
 *   formalization of cryptographic blind signatures. This is the earliest of
 *   three competing boundaries. It is NOT a story about infrastructure
 *   deployment (that is the sibling infrastructure_reading, anchored to 1967
 *   ATMs / 1972 ACH / 1977 SWIFT) and NOT a story about consumer-facing
 *   instruments (that is the sibling consumer_holdings_reading, anchored to
 *   1990s e-purses / 2000 EMD). Each reading has a distinct beneficiary
 *   structure and a distinct ε: this reading's ε is markedly lower than the
 *   infrastructure or consumer readings' extractiveness would be, since the
 *   standing arrangement under contest here is academic-disciplinary credit
 *   allocation, not a live financial-market extraction mechanism. The
 *   coordination function (a shared citation and periodization reference
 *   point) is genuine; the extraction is the systematic transfer of
 *   founding-moment credit toward the theoretical/cryptographic tradition and
 *   away from telecommunications engineering, plus the real operational
 *   burden placed on monetary statisticians who must contend with 'potential
 *   money' as a category.
 *
 * KEY AGENTS:
 *   - cryptography_research_community: primary beneficiary (organized/arbitrage) — collects disciplinary credit and citation primacy
 *   - priority_claiming_academics: agenda-setting beneficiary (moderate/mobile) — sets textbook periodization
 *   - chaum_lineage_researchers: secondary beneficiary (moderate/constrained) — inherits lineage prestige
 *   - infrastructure_engineering_historians: primary payer (moderate/constrained) — demoted to 'implementers'
 *   - central_bank_monetary_statisticians: primary payer (institutional/trapped) — bears unresolved measurement burden
 *   - science_historians: analytical observer — sees the three-reading contest structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.32).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.28).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary — Conceptualization Reading (1960s Telecom Theory, Chaum 1985)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '184c02c8-433b-4c78-a88c-b913c644b49a').
narrative_ontology:cs_kernel_codification('184c02c8-433b-4c78-a88c-b913c644b49a', distributed).
narrative_ontology:cs_authority_grounding('184c02c8-433b-4c78-a88c-b913c644b49a', expertise).
narrative_ontology:cs_interpretation_layer_present('184c02c8-433b-4c78-a88c-b913c644b49a').
narrative_ontology:cs_reading_relation('184c02c8-433b-4c78-a88c-b913c644b49a', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('184c02c8-433b-4c78-a88c-b913c644b49a', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('184c02c8-433b-4c78-a88c-b913c644b49a', foundational, formal_theorization_constitutes_emergence).
narrative_ontology:cs_axiom_status(formal_theorization_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('184c02c8-433b-4c78-a88c-b913c644b49a', formal_theorization_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('184c02c8-433b-4c78-a88c-b913c644b49a', secondary, priority_of_conceptual_possibility_over_deployment).
narrative_ontology:cs_axiom_status(priority_of_conceptual_possibility_over_deployment, holdable).
narrative_ontology:cs_axiom_grounding('184c02c8-433b-4c78-a88c-b913c644b49a', priority_of_conceptual_possibility_over_deployment, instrumental).
narrative_ontology:cs_reference_frame('184c02c8-433b-4c78-a88c-b913c644b49a', cryptographic_theory_founding_moment).
narrative_ontology:cs_drift_state('184c02c8-433b-4c78-a88c-b913c644b49a', post_cryptocurrency_boom_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('184c02c8-433b-4c78-a88c-b913c644b49a', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, priority_claiming_academics).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, chaum_lineage_researchers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, infrastructure_engineering_historians).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, central_bank_monetary_statisticians).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, theoretical_thinkability_as_emergence_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cites David Chaum's 1985 blind-signature formalization and 1960s-70s telecommunications theory as the moment digital money became conceptually possible. Citation counts, conference founding narratives, and disciplinary origin stories accrue to this community when the boundary is drawn at theoretical formalization rather than deployed infrastructure or consumer adoption. Can freely publish, revise, and reframe the historical claim since it costs them nothing to assert.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, cryptography_research_community, beneficiary,
    organized, generational, arbitrage, global).

% Individual researchers and historians of computer science who build career narratives and grant justifications around being first to theorize a mechanism, rather than first to deploy or scale one. They set the periodization used in textbooks and survey articles, actively promoting the conceptualization boundary in review literature and retrospectives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, priority_claiming_academics, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, priority_claiming_academics, agenda_setter).

% Researchers whose intellectual lineage traces through Chaum's ecash and blind-signature work benefit directly from a periodization that treats 1985 as the founding moment of digital money, since it elevates their tradition over parallel engineering lineages (ATM networks, ACH, SWIFT) that predate or coexist with it without theoretical fanfare.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, chaum_lineage_researchers, beneficiary,
    moderate, generational, constrained, global).

% Historians and engineers who document the 1967 ATM rollout, 1972 ACH network, and 1977 SWIFT messaging system as the actual moment money became electronically mobile. Under the conceptualization reading, their infrastructural achievements are demoted to 'mere implementation' of an idea theorized elsewhere, erasing the independent engineering-driven emergence narrative they would otherwise claim credit for.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_engineering_historians, payer,
    moderate, generational, constrained, global).

% Statisticians responsible for M4/M5 monetary aggregates must, under this reading, account for theoretically-conceived-but-not-yet-circulating 'potential money' — an accounting category with no settled measurement convention. This creates real operational cost: no consistent methodology exists for counting money that exists only as a formal cryptographic proposal, and their reporting frameworks are destabilized by a boundary they did not choose and cannot easily reject without appearing to dismiss the academic priority claim.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_monetary_statisticians, payer,
    institutional, immediate, trapped, national).

% Would object that a monetary-emergence boundary keyed to unpublished or lab-stage cryptographic theory has no bearing on actual systemic risk, consumer protection, or currency circulation and should not inform policy periodization, but are not participants in the academic historiography debate that sets this boundary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, financial_regulators, excluded,
    institutional, immediate, analytical, national).

% Study how technical fields construct origin narratives and periodization boundaries, observing that the conceptualization reading, the infrastructure reading, and the consumer-holdings reading are three live, mutually competing claims serving three different communities' priority interests.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, science_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared reference point for citation, textbook periodization, and disciplinary self-understanding within cryptography and theoretical computer science — a genuine coordination need since without SOME agreed-upon origin marker, comparative history and citation practice become incoherent.
% TRANSFER_FUNCTION: Moves historiographic credit, citation primacy, and disciplinary founding-narrative status toward the cryptography/theoretical research tradition and away from telecommunications engineers and payment-infrastructure builders; also transfers unresolved measurement burden onto central bank statisticians who must awkwardly account for 'potential money.'
% ABSENT_VOICES: Financial regulators and payment-systems engineers who built and deployed the ACH, SWIFT, and ATM networks are not party to the academic periodization debate; they would object that theoretical formalization without circulating instruments is not monetary emergence in any operational sense.
% DISAPPEARANCE_RATIONALE: If the conceptualization boundary were abandoned, the cryptography research tradition would lose a foundational origin narrative used in grant applications, textbook chapters, and retrospective framing — a real but narrow rearrangement confined to academic credit allocation. Central bank statistical practice would arguably simplify (no more need to theorize 'potential money'). Whether the world meaningfully rearranges depends entirely on which community you ask, which is itself the signature of a contested kernel reading rather than a settled fact.
% FOUNDING_PROBLEM: Cryptography and computer science needed a defensible historical marker establishing when digital money became a coherent theoretical object, distinguishing it from ad hoc electronic funds transfer mechanisms that predated any formal cryptographic money model.
% FOUNDING_PROBLEM_CORROBORATION: Chaum-lineage researchers and priority-claiming academics attest the founding problem (establishing intellectual priority for cryptographic money) remains live and important. Infrastructure historians and financial-systems economists, from outside the beneficiary set, attest the more operationally relevant founding problem — when money actually became electronically transferable at scale — was already solved by 1977 (SWIFT) and that the conceptualization boundary answers a narrower, discipline-internal question dressed as a general monetary-history claim.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, contested).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-low (0.32 by 2000) because the transfer here is reputational/disciplinary rather than monetary — no cash actually moves, but citation credit, textbook space, and grant-narrative primacy do, and these have real career and institutional consequences for the paying seats. Suppression is comparatively low (0.28) because dissenting historiographic framings (crediting engineers over theorists) remain publishable and circulate; the boundary is contested rather than coercively enforced, but it is actively defended in review articles and disciplinary retrospectives, which is why requires_active_enforcement is true. Theater ratio is elevated (0.4) because a substantial share of the boundary's maintenance is performative — anniversary retrospectives, 'father of digital cash' framings, and priority disputes that function more as disciplinary ritual than as settled historical method. accessibility_collapse (0.45) and resistance (0.35) are moderate: alternative periodizations remain visible and actively argued by rival communities, so alternatives have not collapsed the way they would for a genuine natural-law boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   The cryptography research community and Chaum-lineage researchers sit near the beneficiary end of directionality: the boundary was constructed largely by and for their citation practices, and their exit options (arbitrage/constrained but professionally secure) reflect low vulnerability to the boundary's costs. Infrastructure historians and central bank statisticians sit nearer the target end: the former lose disciplinary credit they would otherwise claim for engineering achievement, and the latter — institutionally trapped, unable to simply opt out of monetary reporting frameworks — bear a genuine operational cost (accounting for theoretical 'potential money') that was never designed with their reporting needs in mind.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing a defensible origin point for cryptographic money theory) remains partially live within the cryptography discipline itself — it still needs SOME periodization for citation coherence. But its extension into general monetary-history and monetary-statistics territory (the M4/M5 'potential money' problem) is where the founding problem's status becomes contested-to-dead: statisticians did not ask for and do not need a theoretical-thinkability boundary to do their job, and forcing the boundary onto their accounting practice is a case of an academic-internal need being generalized past its actual jurisdiction. Classifying this as tangled_rope rather than a pure snare captures that the coordination function (disciplinary periodization) is real and valuable within its home domain, while the extraction (imposed accounting burden, demoted engineering credit) is the cost of that function overflowing its boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theoretical_vs_operational_emergence_criterion,
    'Is ''theoretical thinkability'' a legitimate emergence criterion for a monetary phenomenon, or does monetary emergence require actual circulation/use, making the conceptualization boundary a category error dressed as historical fact?',
    'Comparative analysis of how other technologies'' ''emergence'' is dated in economic history (e.g., does the airplane ''emerge'' at Leonardo''s sketches or the Wright Flyer?) combined with explicit adoption of a stated criterion by a cross-disciplinary standards body (e.g., economic historians'' associations, central bank statistics committees).',
    'If theoretical thinkability is rejected as a criterion, this reading''s claimed_type and beneficiary structure collapse — the boundary becomes purely an internal disciplinary artifact with no claim on general monetary history, weakening the tangled_rope coordination-function premise toward a narrower, more clearly extractive framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theoretical_vs_operational_emergence_criterion, conceptual, 'Whether theoretical formalization alone can constitute monetary emergence.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one of three readings (conceptualization, infrastructure, consumer_holdings) of the digital_money_emergence_boundary kernel. Where exactly does the disagreement between readings live — is it about WHEN money became technically possible, WHEN it became transactable, or WHEN it became consumer-accessible? Each reading treats a different one of these as decisive.',
    'A cross-reading structural comparison (already partially performed via network.affects_constraints links) documenting which reading each community of practice (cryptographers, payment engineers, consumer-finance regulators) actually uses in its own internal historiography, independent of academic dispute.',
    'If one reading is shown to dominate practitioner usage across all three communities, the other two readings'' claimed beneficiary structures would be revealed as minority/contested positions rather than co-equal alternatives, though per DP-001 each remains its own constraint with its own ε regardless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement among the three sibling kernel readings.').

omega_variable(
    potential_money_measurement_convention,
    'Is there a coherent, agreed methodology for central banks to account for theoretically-conceived-but-not-circulating ''potential money'' in M4/M5 aggregates, or is this an unresolved measurement burden imposed without practical resolution?',
    'Survey of central bank statistical methodology documents (BIS, Federal Reserve, ECB) for any explicit treatment of pre-circulation theoretical monetary instruments in aggregate reporting.',
    'If no such convention exists and none is being developed, the extraction imposed on central_bank_monetary_statisticians is more severe and more purely theatrical than currently scored (theater_ratio and extractiveness would both be underestimated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_money_measurement_convention, empirical, 'Whether monetary statisticians have any workable convention for ''potential money.''').


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
narrative_ontology:measurement(digi_tr_t1976, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1976, 0.28).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(digi_tr_t1992, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1992, 0.38).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(digi_be_t1968, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1968, 0.18).
narrative_ontology:measurement(digi_be_t1976, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1976, 0.2).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(digi_be_t1992, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1992, 0.31).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2000, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(digi_su_t1968, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1968, 0.15).
narrative_ontology:measurement(digi_su_t1976, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1976, 0.18).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.24).
narrative_ontology:measurement(digi_su_t1992, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1992, 0.27).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2000, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.08).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial phrase 'when did digital money emerge' per the ε-invariance principle. conceptualization_reading (this story, ε=0.32) treats 1985 Chaum formalization and 1960s telecom theory as the emergence boundary, with the academic/research community as primary beneficiary. infrastructure_reading treats 1967 ATMs / 1972 ACH / 1977 SWIFT as the boundary, with payment-network engineers and operators as primary beneficiary and a distinct, likely higher extractiveness profile tied to network-operator rent capture. consumer_holdings_reading treats 1990s e-purses / 2000 EMD as the boundary, with payment-industry firms and consumer-protection regulatory bodies as the relevant parties. Each carries its own claimed_type, its own metrics, and its own stakeholder set; they are linked here rather than merged because changing the observable (theory vs. infrastructure vs. consumer access) changes ε substantially — exactly the disambiguation the framework requires rather than a single story with a hidden measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
