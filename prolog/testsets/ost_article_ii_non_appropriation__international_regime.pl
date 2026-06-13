% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation (International Regime Reading)
 *   domain: international/legal/commons governance
 *
 * SUMMARY:
 *   The Outer Space Treaty (OST, 1967) Article II declares that celestial
 *   bodies are not subject to national appropriation. Article XI defers the
 *   question of benefit-sharing from space resource extraction to a future
 *   international regime—a mechanism never negotiated. This reading
 *   interprets Article II as establishing a deferral: neither the
 *   extraction-permissive reading (private extraction with no international
 *   benefit-sharing requirement) nor the conservation reading (extraction
 *   prohibited unless explicitly authorized) is treaty-authoritative without
 *   a multilateral regime framework. The constraint is the legal suspension
 *   itself—regulatory uncertainty as a temporary structural feature pending
 *   regime emergence. First-mover firms operate in a grey zone; developing
 *   nations and conservation advocates bear the cost of delay; spacefaring
 *   states remain gridlocked in zero-sum negotiation. The claim is Scaffold
 *   because the deferral is explicitly temporary (sunset condition: future
 *   regime emergence or explicit decision to close the question). The metrics
 *   describe substantial extraction activity operating under regulatory
 *   uncertainty, with high theater—the ongoing COPUOS negotiation performs
 *   legitimacy while substantive distributional agreement does not emerge.
 *
 * KEY AGENTS:
 *   - space_extraction_firms: First-mover commercial operators (SpaceX, Axiom Space, private asteroid-mining consortia) exploit the regulatory grey zone to invest and operate; their exit is mobile but their investment risk is substantial.
 *   - developing_nations: G77 + China coalition seeks mandatory benefit-sharing and technology transfer; their exit is constrained (capacity limit on unilateral extraction).
 *   - spacefaring_nations: OST state parties (U.S., Russia, China, EU) control negotiation agenda; gridlock reflects incompatible distributional objectives.
 *   - environmental_advocates: Identity-locked payers bearing environmental cost of unregulated extraction during deferral.
 *   - regime_negotiators: Institutional actors (COPUOS, diplomatic corps) whose authority depends on the reading's deferral logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.58).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.42).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation (International Regime Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international/legal/commons governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'a257dd91-52dd-44cf-b12f-2851dc1cf911').
narrative_ontology:cs_kernel_codification('a257dd91-52dd-44cf-b12f-2851dc1cf911', fixed_text).
narrative_ontology:cs_authority_grounding('a257dd91-52dd-44cf-b12f-2851dc1cf911', lineage).
narrative_ontology:cs_interpretation_layer_present('a257dd91-52dd-44cf-b12f-2851dc1cf911').
narrative_ontology:cs_reading_relation('a257dd91-52dd-44cf-b12f-2851dc1cf911', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('a257dd91-52dd-44cf-b12f-2851dc1cf911', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_axiom('a257dd91-52dd-44cf-b12f-2851dc1cf911', foundational, appropriation_question_deferrable).
narrative_ontology:cs_axiom_status(appropriation_question_deferrable, holdable).
narrative_ontology:cs_axiom_grounding('a257dd91-52dd-44cf-b12f-2851dc1cf911', appropriation_question_deferrable, deontological).
narrative_ontology:cs_axiom('a257dd91-52dd-44cf-b12f-2851dc1cf911', foundational, procedural_legitimacy_vindicates_deferral).
narrative_ontology:cs_axiom_status(procedural_legitimacy_vindicates_deferral, holdable).
narrative_ontology:cs_axiom_grounding('a257dd91-52dd-44cf-b12f-2851dc1cf911', procedural_legitimacy_vindicates_deferral, conventional).
narrative_ontology:cs_reference_frame('a257dd91-52dd-44cf-b12f-2851dc1cf911', treaty_validity_through_regime_negotiation).
narrative_ontology:cs_drift_state('a257dd91-52dd-44cf-b12f-2851dc1cf911', contemporary_extraction_feasibility, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a257dd91-52dd-44cf-b12f-2851dc1cf911', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, regime_negotiators).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, future_multilateral_authority).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, developing_nations_delayed_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, space_extraction_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, space_extraction_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, developing_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, environmental_conservation_advocates).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, common_heritage_principle).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, procedural_legitimacy_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First-mover commercial firms (asteroid mining, lunar resource extraction) operate in legal ambiguity created by the reading's deference to future regime. They invest capital and technology under regulatory uncertainty, bearing the cost of potential future regime prohibition or mandatory revenue-sharing. They also benefit from the grace period: no current enforcement mechanism constrains extraction activity, allowing first-mover advantage and knowledge accumulation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_extraction_firms, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, space_extraction_firms, beneficiary).

% Developing states lack the capital and technology to conduct space extraction during the deferral period, but want participation and benefit-sharing in any future regime. They pay in delayed access and technological disadvantage while the regime negotiation stalls. Their exit is constrained: they cannot simply extract independently (capacity limit) nor unilaterally reshape the treaty framework.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, developing_nations, payer,
    moderate, generational, constrained, global).

% States that ratified the OST (U.S., Russia, China, EU members, etc.) are collectively responsible for negotiating the Article XI regime. They set the agenda by choosing negotiation pace, framework architecture, and whether to enforce the deferral itself. They are gridlocked by zero-sum distributional conflict: extraction-favoring states resist mandatory revenue-sharing; common-heritage advocates resist unregulated extraction. The deferral reading leaves them in permanent negotiation stalemate.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_nations, agenda_setter,
    institutional, generational, constrained, global).

% Advocacy coalitions committed to environmental protection and preservation of space environments bear the cost of the regime's non-emergence: no binding conservation standards, no enforcement of environmental impact assessment, no planetary-protection framework in force during the deferral. Their identity is fused with the preservation mission; exit would require abandoning the advocacy platform itself.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, environmental_conservation_advocates, payer,
    organized, civilizational, identity_locked, global).

% The diplomatic corps and technical experts tasked with negotiating the Article XI regime (within COPUOS and Ad Hoc Committee forums) set the pace and scope of negotiation. They benefit from the deferral reading (it keeps the negotiation alive as a legitimate ongoing process) while bearing the cost of gridlock (distributional conflict remains unresolved, negotiations stall for years between sessions).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, regime_negotiators, agenda_setter,
    institutional, generational, constrained, global).

% The OST's limited enforcement mechanisms (state parties, COPUOS, potential ICJ adjudication) have no clear mandate under this reading to prohibit or regulate extraction activity—that authority is deferred. They observe the legal gap and the commercial activity filling it, unable to enforce without a regime framework that does not yet exist.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, treaty_enforcement_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, regime_negotiators).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the collective fiction of treaty legitimacy while distributional conflict over resource benefits persists unresolved. Enables states to claim they are negotiating toward a fair regime without actually implementing one, and allows extraction firms to operate under the assumption that current activity will be grandfathered into any future regime.
% TRANSFER_FUNCTION: Defers the transfer of authority (and wealth) from the treaty framework itself to a hypothetical future multilateral body. During the deferral, first-mover extraction firms accumulate knowledge, capital stakes, and factical presence that will give them leverage in regime negotiation. Developing nations and conservation advocates pay in delayed access and environmental uncertainty.
% ABSENT_VOICES: Non-state environmental organizations, indigenous knowledge systems, and future generations (those who will inherit the environmental consequences of extraction decisions made during the deferral) have no formal seat in treaty interpretation or regime negotiation. Their absence is structural: the OST framework prioritizes state parties and commercial actors.
% DISAPPEARANCE_RATIONALE: If this reading vanished—if Article II were authoritatively read as either extraction-permissive (sibling extraction_permissive) or conservation-binding (sibling commons_conservation)—the regulatory landscape would crystallize: extraction would either face binding conservation limits or receive explicit authorization with clear terms. The current legal grey zone would close. First-movers would face either retroactive prohibition or stable property rights. Developing nations would either gain immediate regime negotiation authority or lose it. The deferral's only function is to keep all these outcomes suspended; its disappearance collapses the suspension.
% FOUNDING_PROBLEM: The OST's Article II prohibition on national appropriation of celestial bodies was negotiated in the 1960s when space extraction was not technically feasible and the distribution question was abstract. By the 2020s, extraction became materially possible. The treaty left no mechanism for adjudicating whether resource extraction (as opposed to territorial sovereignty claims) constitutes 'appropriation.' This reading emerged to finesse the problem: defer the question to a future regime, preserving both states' authority (to eventually set rules) and treaty continuity (the OST remains valid while the appropriation question is resolved externally).
% FOUNDING_PROBLEM_CORROBORATION: Technical feasibility of asteroid mining and lunar resource extraction is corroborated by independent engineering assessment and commercial investment (SpaceX, private mining consortia). The absence of a binding regime is corroborated by the UN COPUOS record: no Article XI regime has been negotiated despite 30+ years of on-and-off discussions. Gridlock over distributional terms is corroborated by negotiating blocs' incompatible opening positions (extraction-favoring states vs. common-heritage advocates). This corroboration is from outside the beneficiary class (the regime negotiators themselves do not benefit from the founding problem's persistence—they benefit from the reading's deferral, which suspends resolution).
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint permits extraction activity (no hard prohibition) but conditions it on regulatory uncertainty—firms cannot claim stable property rights or plan long-term investment without regime clarity. Suppression is moderate (0.42) because there is no active enforcement machinery prohibiting extraction; the constraint's force is epistemic (uncertainty about future rules) rather than coercive. Theater is high (0.67) and declining slightly over the interval: in 1967, the treaty was almost pure theater (extraction was not technically feasible, so the debate was entirely symbolic); by 2025, extraction is real and proceeding, but the theatrical COPUOS negotiation continues without producing a binding regime—the ratio declined as factical extraction diverged from negotiation performance. Accessibility collapse is low (0.38) because alternative readings remain live and well-articulated; firms can cite the extraction-permissive reading as their legal basis, conservationists can cite the conservation reading, and the regime reading offers no exclusive interpretation. Resistance is high (0.71) because negotiating blocs and advocacy coalitions actively dispute the reading—distributional conflict is visible and contested.
 *
 * PERSPECTIVAL GAP:
 *   Extraction-firm seat: the regulatory grey zone is economically efficient—it allows them to operate, accumulate knowledge and stakes, and position themselves as essential participants in future regime negotiation. The constraint benefits them. Developing-nation and conservation-advocate seats: the deferral is cost-imposition—they are frozen out during the grace period, unable to shape the rules or claim extraction benefits, and cannot resolve the question through their own action. Spacefaring-state seats: they experience the constraint as their own negotiation mandate (Article XI's promise of a future regime), but that mandate is unfulfillable due to distributional gridlock. The reading vindicates procedural legitimacy (eventually there will be a regime) while structurally producing substantive delay.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover firms are direct beneficiaries (d ≈ 0.2): they collect the option value of regulatory ambiguity, operate without enforcement, and gain first-mover advantage. Developing nations are victims (d ≈ 0.85): they bear the cost of delayed regime emergence, technological disadvantage, and gridlock they cannot unilaterally break. Conservation advocates are victims (d ≈ 0.8): they pay in environmental uncertainty and inability to enforce conservation standards pending regime. Spacefaring nations sit near symmetric (d ≈ 0.5): they have the authority to negotiate the regime but are locked in zero-sum distributional conflict that prevents its emergence—authority without execution. Regime negotiators (institutional actors) have high power but constrained exit (d ≈ 0.6): they must continue negotiating per treaty obligation, but distributional conflict is not resolvable through negotiation mechanics alone.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mild mandatrophy: Article XI's mandate was to establish a benefit-sharing regime within a reasonable timeframe (the tacit post-war expectation was 10-20 years). The mandate is now 50+ years unfulfilled, not because the treaty language changed or became physically impossible, but because the distribution problem proved zero-sum and states prioritized maintaining negotiation theater over resolving the question. The reading exacerbates this by treating the deferral as legitimate indefinitely—so long as negotiations 'continue,' the reading preserves treaty validity while actual regime emergence recedes. The scaffold's sunset condition is not achievable without breaking the distributional gridlock, which the current negotiation structure does not allow. The theater metric (0.67) captures this: COPUOS meetings occur regularly, documents are produced, negotiating positions are restated, but substantive movement toward a regime has stalled. This is the mandatrophy signature: a legitimate institutional process (regime negotiation) performing its function while its actual mandate (producing a binding regime) recedes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_emergence_mechanism,
    'What negotiating structure or event would break the current distributional gridlock and allow the Article XI regime to emerge? Is the stalemate resolvable through the existing COPUOS framework, or does it require external pressure (e.g., actual resource scarcity, technological breakthrough, geopolitical shift)?',
    'Longitudinal analysis of COPUOS negotiating records, state-position evolution, and exogenous shocks (scarcity events, technology breakthroughs, geopolitical realignment). A simulated negotiation model incorporating binding coalitional constraints would test whether the current framework structure permits regime emergence.',
    'If the gridlock is fundamentally unresolvable within the current structure, the deferral reading is a de facto permanent constraint masquerading as temporary—it would reclassify from Scaffold toward Piton (expired mandate maintained by institutional inertia). If resolvable, the reading is correctly classified as transitional and regime emergence is the resolvable outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_emergence_mechanism, empirical, 'Whether the deferral''s sunset condition is achievable or the constraint''s temporality is illusory.').

omega_variable(
    common_heritage_exhaustion,
    'The ''common heritage of mankind'' principle (invoked in the preamble and by conservation/developing-nation advocates) is not explicitly binding in Article II''s non-appropriation language. Is this omission an intentional limits to the principle''s scope, or a drafting gap that later interpretation should fill?',
    'Historical-origins inquiry into OST negotiating records (UNGA Committee on Peaceful Uses of Outer Space, 1960s archives); textual comparison with later common-heritage instruments (Law of the Sea Convention, 1982); state ratification history and reservation statements.',
    'If the omission was intentional (to preserve state sovereignty and extraction rights), the extraction_permissive reading gains treaty support and the international_regime reading loses normative grounding. If a gap, the commons_conservation reading gains stronger textual foundation. The deferral''s legitimacy depends on reading the common-heritage principle as real but procedurally deferred—not as absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_heritage_exhaustion, conceptual, 'Whether ''common heritage'' is binding in Article II or a future regime''s invention.').

omega_variable(
    first_mover_grandfathering_risk,
    'If and when the Article XI regime emerges, will extraction activity undertaken during the deferral period be grandfathered (exempt from new rules) or retroactively subjected to the regime''s constraints (extraction prohibited, or required to pay retroactive benefit-sharing)? The reading leaves this unspecified, which creates investment risk for extraction firms.',
    'Comparative legal analysis of precedent (e.g., how the Law of the Sea regime treated mining activity undertaken in international waters before ISA authority was operative; how climate-change accords treated pre-regime carbon liabilities). Simulation of regime-negotiation outcomes under different grandfathering assumptions.',
    'If grandfathering is likely, first-movers face lower risk and the constraint is more attractive to firms (higher beneficiary extraction for this seat). If retroactive constraints are possible, first-movers face sunk-cost losses and the constraint becomes more like a snare (deferred prohibition becomes a trap). The deferral reading''s credibility depends on implying (without stating) that grandfathering is likely—that first-mover activity will not be prohibited retroactively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_mover_grandfathering_risk, empirical, 'Whether first-mover extraction rights will survive regime emergence or face retroactive constraints.').

omega_variable(
    dual_reading_institutional_friction,
    'This reading coexists with two sibling readings held by different state blocs and NGO coalitions. Is the coexistence stable (three live interpretations that can be held in parallel), or does the institutional machinery of treaty interpretation eventually require selecting one canonical reading, foreclosing the others?',
    'Study of similar multi-reading treaty systems (e.g., constitutional interpretation across federalism disputes, Law of the Sea regime''s handling of continental-shelf ambiguities). Analysis of International Court of Justice precedent on treaty interpretation methodologies and whether they drive toward canonical or pluralistic readings.',
    'If institutional machinery eventually selects one reading, the deferral is temporary by structure—the Scaffold sunset is driven by compulsory interpretation rather than voluntary regime negotiation. If coexistence is stable indefinitely, the Scaffold may be misclassified and the constraint is actually a Piton (indefinite institutional theater without substantive resolution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_reading_institutional_friction, conceptual, 'Whether international legal interpretation permits stable coexistence of three readings or drives toward canonical selection.').

omega_variable(
    suppression_internalization,
    'The measured suppression (0.42) is relatively low—there is no active enforcement machinery preventing extraction during the deferral. But conservation advocates and developing nations do not extract; is this restraint due to lack of capacity (structural suppression) or internalized acceptance of the deferral reading''s legitimacy (internalized suppression, where they have accepted the regime-negotiation logic)?',
    'Post-exit analysis: if developing nations acquire extraction capacity and immediately begin operations, the suppression is structural (they were constrained only by inability, not belief). If they continue to wait for regime emergence despite acquiring capacity, suppression is partly internalized. Interview-based research on conservation-advocate and developing-nation negotiating positions regarding whether they accept the deferral reading''s legitimacy.',
    'If suppression is internalized, the constraint is more durable (internalized norms persist after external barriers drop). If structural, the constraint is fragile (once capacity increases, extraction pressure rises rapidly). The classification does not change, but the strategic volatility differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether low-suppression is structural (capacity constraint) or internalized (belief in regime-negotiation process).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.95).
narrative_ontology:measurement_basis(ost__tr_t1967, projected).
narrative_ontology:measurement(ost__tr_t1990, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1990, 0.88).
narrative_ontology:measurement_basis(ost__tr_t1990, projected).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2010, 0.75).
narrative_ontology:measurement_basis(ost__tr_t2010, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.7).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2025, 0.67).
narrative_ontology:measurement_basis(ost__tr_t2025, observed).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2030, 0.67).
narrative_ontology:measurement_basis(ost__tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.0).
narrative_ontology:measurement_basis(ost__be_t1967, projected).
narrative_ontology:measurement(ost__be_t1990, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement_basis(ost__be_t1990, projected).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement_basis(ost__be_t2010, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(ost__be_t2025, observed).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2030, 0.58).
narrative_ontology:measurement_basis(ost__be_t2030, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ost_article_ii_non_appropriation__international_regime, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, lunar_resource_rights_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, deep_sea_mining_isba_governance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of Article II's non-appropriation principle. The kernel is the same OST language across all three readings; the structural delta is in how each reading instantiates the appropriation question. The international_regime reading defers both extraction and conservation to future multilateral authority; the extraction_permissive reading treats extraction as permitted and conservation as a future optional choice; the commons_conservation reading treats extraction as prohibited absent explicit authorization. Each reading has its own ε, beneficiary/victim structure, and type. The three readings coexist as positions in an ongoing institutional dispute; none is treaty-authorized as canonical. The constraint family is linked via network.affects_constraints so that corpus analysis can track how uncertainty in one reading propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
