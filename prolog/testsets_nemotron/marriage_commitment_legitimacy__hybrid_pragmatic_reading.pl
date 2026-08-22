% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Marriage Commitment Legitimacy — Hybrid Pragmatic Reading (Official Declaration-1 / Manifesto)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration-1) issued by LDS Church
 *   President Wilford Woodruff publicly advised against contracting plural
 *   marriages 'forbidden by the law of the land.' The text is deliberately
 *   ambiguous: it does not renounce the doctrine of plural marriage as false,
 *   does not dissolve existing plural marriages, and frames the advice as
 *   inspired counsel rather than doctrinal reversal. This reading
 *   (hybrid_pragmatic) holds that the leadership strategically deployed
 *   prophetic authority to navigate an existential federal threat while
 *   preserving the underlying theological commitment through scope ambiguity
 *   — the 'eternal principle' remains, only its 'temporal practice' is
 *   suspended. This is one of three contested readings of the
 *   marriage_commitment_legitimacy kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.48).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.42).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Marriage Commitment Legitimacy — Hybrid Pragmatic Reading (Official Declaration-1 / Manifesto)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '0b94d130-6edf-4733-b7a4-84403243a413').
narrative_ontology:cs_kernel_codification('0b94d130-6edf-4733-b7a4-84403243a413', formalized).
narrative_ontology:cs_authority_grounding('0b94d130-6edf-4733-b7a4-84403243a413', lineage).
narrative_ontology:cs_interpretation_layer_present('0b94d130-6edf-4733-b7a4-84403243a413').
narrative_ontology:cs_reading_relation('0b94d130-6edf-4733-b7a4-84403243a413', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b94d130-6edf-4733-b7a4-84403243a413', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('0b94d130-6edf-4733-b7a4-84403243a413', foundational, prophetic_authority_instrumentally_deployed).
narrative_ontology:cs_axiom_status(prophetic_authority_instrumentally_deployed, holdable).
narrative_ontology:cs_axiom_grounding('0b94d130-6edf-4733-b7a4-84403243a413', prophetic_authority_instrumentally_deployed, instrumental).
narrative_ontology:cs_axiom('0b94d130-6edf-4733-b7a4-84403243a413', foundational, scope_ambiguity_preserves_doctrinal_continuity).
narrative_ontology:cs_axiom_status(scope_ambiguity_preserves_doctrinal_continuity, holdable).
narrative_ontology:cs_axiom_grounding('0b94d130-6edf-4733-b7a4-84403243a413', scope_ambiguity_preserves_doctrinal_continuity, conventional).
narrative_ontology:cs_reference_frame('0b94d130-6edf-4733-b7a4-84403243a413', eternal_plural_marriage_covenant).
narrative_ontology:cs_drift_state('0b94d130-6edf-4733-b7a4-84403243a413', post_manifesto_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b94d130-6edf-4733-b7a4-84403243a413', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authorities).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, polygamous_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the Manifesto as official doctrine to terminate plural marriage practice while preserving the underlying theological commitment through deliberate ambiguity about whether the revelation was divine command or strategic concession. Retains authority to define orthodoxy, controls temple rites, and negotiates statehood. Gains federal legitimacy and preserves institutional assets.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, beneficiary).

% Achieves the policy objective (ending plural marriage) without destroying the institution or provoking armed resistance. Accepts the Manifesto as sufficient compliance. Gains political resolution of the 'Mormon question' and integrates the territory into the federal system.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Bear the interpretive uncertainty: taught that plural marriage was an eternal covenant, now told it is suspended but not renounced. Face social stigma, legal jeopardy for pre-Manifesto marriages, and spiritual whiplash. Cannot exit without abandoning religious identity, community, and salvation framework. Pay through cognitive dissonance, family disruption, and loss of theological coherence.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer).

% Existing plural wives and children lose legal recognition, inheritance rights, and community standing overnight. Husbands face prosecution if they continue cohabitation; wives face destitution if abandoned. No exit option that preserves family integrity. The Manifesto provides no transition mechanism for existing families — only cessation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).

% Reject the Manifesto as apostasy; maintain that the revelation was genuine and the suspension temporary. Form breakaway communities (eventually FLDS and others). Their exclusion is structural: the institutional leadership defines them as apostate to secure federal compliance. They bear the cost of schism but preserve the 'pure' doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dissident_fundamentalists, excluded,
    moderate, generational, constrained, regional).

% Examine the Manifesto as a case study in religious institutional adaptation under coercion. Trace the rhetorical strategies, the ambiguity's function, and the long-term consequences for authority, legitimacy, and schism. No stake in the outcome; their analysis maps the constraint's operation across seats.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the existential crisis of federal anti-polygamy legislation (Edmunds Act, Edmunds-Tucker Act) threatening institutional dissolution, asset seizure, and disincorporation. Provides a single authoritative text that satisfies federal compliance demands while preserving the theological framework that makes the institution coherent to its members.
% TRANSFER_FUNCTION: Moves institutional survival and federal legitimacy from the threat of destruction to the leadership's control, at the cost of rank-and-file members' theological coherence and polygamous families' legal standing. Transfers interpretive authority definitively to the institutional center; transfers the burden of ambiguity to the laity.
% ABSENT_VOICES: Polygamous wives and children — the most directly harmed — had no formal role in the revelation process or the Manifesto's ratification. Their situation was managed by male priesthood holders (husbands, leaders) who themselves faced prosecution. The federal government's demand for 'genuine abandonment' excluded transitional protections for existing families.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished, the institution would face immediate federal re-escalation (asset seizure, disincorporation, leadership imprisonment). The theological framework would revert to open plural marriage teaching, triggering schism between compliance and resistance factions. The federal-territory relationship would renegotiate from a different baseline. The world rearranges.
% FOUNDING_PROBLEM: Federal anti-polygamy legislation (1862 Morrill Act through 1887 Edmunds-Tucker Act) escalated from criminal penalties to institutional death penalties: disincorporation of the Church, seizure of all assets over $50,000, abolition of women's suffrage in Utah, replacement of local judges with federal appointees. The institution faced existential destruction unless it could demonstrate genuine abandonment of the practice.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records (Edmunds-Tucker Act text, congressional debates), contemporary newspaper accounts (Salt Lake Tribune, Deseret News), and the Church's own legal filings (Supreme Court briefs in Late Corp. v. United States, 1890) corroborate the existential threat. The Manifesto's own preamble cites 'laws enacted by Congress' as the occasion. No serious historian disputes the coercive pressure; the dispute is over the leadership's internal motivation.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48) because the leadership extracts institutional survival and federal legitimacy while members bear interpretive uncertainty and families bear legal destruction — but the coordination function (institutional survival under coercion) is genuine, not pretext. Suppression is moderate (0.42): enforcement targets new plural marriages (excommunication, denial of temple recommends) but existing families are largely left alone after 1890; the Smoot hearings (1904-1907) spike suppression briefly. Theater ratio rises from 0.2 to 0.42 as the ambiguity becomes increasingly performative — the 'revelation' frame is maintained while practice, doctrine, and public signaling diverge. The 1904 Second Manifesto and 1910 end of the interval mark the transition to active enforcement against post-Manifesto marriages.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat: a genuine revelation adapting eternal principle to temporal necessity (rope-like coordination). From the rank-and-file seat: a betrayal of covenant obligations justified by opaque authority (snare-like extraction). From the polygamous family seat: destruction of their legal and social standing without consent or transition (pure snare). From the federal seat: a negotiated compliance mechanism (coordination). The engine computes per-seat types from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership: d ≈ 0.15 (beneficiary — controls the narrative, gains survival, arbitrage exit via doctrinal authority). Federal authorities: d ≈ 0.1 (beneficiary — achieves policy goal, arbitrage exit). Rank-and-file members: d ≈ 0.75 (payer/victim — identity_locked, bear ambiguity, constrained exit). Polygamous families: d ≈ 0.9 (victim — trapped, no exit preserving family). Dissidents: d ≈ 0.6 (excluded — constrained exit via schism). Analysts: d = 0.5 (analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is coded as 'live' because the coercive structure (federal oversight of marital definition, state regulation of religious practice) persists. The Manifesto did not resolve the tension — it managed it through ambiguity. The arrangement persists because the ambiguity serves the leadership (doctrinal flexibility) and the federal state (plausible compliance). Mandatrophy is unresolved: the constraint's mandate (manage the crisis) has outlived the acute crisis but the ambiguity it installed became structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_strategy_ambiguity,
    'Was the Manifesto experienced by Woodruff and his counselors as genuine revelation, strategic calculation, or an inseparable fusion of both?',
    'Private diaries, council minutes, and contemporaneous correspondence (Woodruff, Cannon, Smith, Young journals; First Presidency/Quorum of Twelve minutes). The 1904 Reed Smoot hearings testimony provides later retrospective framing.',
    'If genuine revelation, the endogenous_reinterpretation_reading gains structural weight; if strategic calculation, the exogenous_override or hybrid_pragmatic readings dominate. The fusion possibility — that prophetic consciousness genuinely experiences strategic necessity as divine will — is the hybrid_pragmatic reading''s core claim and resists binary resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_strategy_ambiguity, conceptual, 'The epistemic status of the Manifesto''s origin: revelation, strategy, or fused.').

omega_variable(
    ambiguity_as_coordination_or_extraction,
    'Does the Manifesto''s deliberate scope ambiguity function as coordination (allowing diverse interpretations to coexist peacefully) or extraction (concentrating interpretive authority in the leadership while distributing costs to the laity)?',
    'Track interpretive disputes in Church disciplinary councils (1890-1910), member correspondence, and the 1904 Second Manifesto''s clarification. If ambiguity suppresses dissent, it extracts; if it enables peaceful transition, it coordinates.',
    'If coordination-dominant, the constraint leans rope/tangled_rope with lower effective extraction for members. If extraction-dominant, the ambiguity is a mechanism of control and the constraint leans snare/tangled_rope with higher effective extraction for identity_locked members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_coordination_or_extraction, empirical, 'Whether ambiguity serves coordination or extraction in practice.').

omega_variable(
    federal_compliance_sincerity,
    'Did the federal government (Congress, executive, judiciary) accept the Manifesto as genuine compliance, or as a tactical concession requiring continued pressure?',
    'Congressional records (Smoot hearings, statehood debates), Department of Justice enforcement patterns, Supreme Court rulings (Late Corp. v. United States 1890; subsequent cases).',
    'If accepted as genuine, the exogenous_override_reading weakens (coercion succeeded). If treated as tactical, the hybrid_pragmatic reading gains weight (strategic adaptation met with strategic skepticism). The 1904-1907 Smoot hearings suggest the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_compliance_sincerity, empirical, 'Federal sincerity in accepting the Manifesto as compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1895, 0.28).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1904, 0.42).
narrative_ontology:measurement(marr_tr_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1907, 0.4).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1910, 0.38).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1895, 0.42).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1900, 0.48).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1904, 0.52).
narrative_ontology:measurement(marr_be_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1907, 0.5).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1910, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1895, 0.48).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1904, 0.38).
narrative_ontology:measurement(marr_su_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1907, 0.4).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1910, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'Manifesto meaning' into three structurally distinct readings with different ε values, beneficiary/victim structures, and CS axioms. The hybrid_pragmatic_reading shows moderate extractiveness (ε≈0.48) because the leadership gains survival/legitimacy while members bear ambiguity costs. The exogenous_override_reading would show lower ε (coercion narrative, leadership as victim). The endogenous_reinterpretation_reading would show near-zero ε (genuine revelation, no extraction). All three share the same referent (the 1890 Manifesto and its enforcement) but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, organized, 0.75).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
