% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Demarcation: Portuguese Exploration Legitimation Reading
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint story captures the Portuguese reading of the Tordesillas
 *   demarcation kernel: the papal treaty and bulls as a confirmation of prior
 *   Portuguese exploration rights east of the line and a legal instrument for
 *   excluding rival European powers from Asian and African trade. In this
 *   reading, the primary extraction target is not indigenous populations (who
 *   are structurally excluded from the legal conversation) but other European
 *   crowns and merchants. The Estado da Ãndia benefits from a trade monopoly
 *   legitimated by papal authority, while rival Europeans bear the cost of
 *   exclusion. The claim/metric independence is maintained: the constraint is
 *   claimed as a tangled rope (genuine coordination of Iberian claims plus
 *   asymmetric extraction of commercial surplus from rivals) while metrics
 *   describe substantial active enforcement and moderate theater.
 *
 * KEY AGENTS:
 *   - Portuguese Crown: Primary agenda-setter (institutional/constrained) â administers and enforces the demarcation and trade monopoly through naval power and fortification.
 *   - Portuguese Estado da Ãndia: Primary beneficiary (organized/constrained) â collects monopoly rents from eastern trade routes and feitoria network.
 *   - Rival European Powers: Primary payer (powerful/constrained) â excluded from eastern trade by legal and naval barriers, bear cost of diverted commerce and military confrontation.
 *   - Indigenous Peoples: Excluded (powerless/trapped) â overwritten by the legal framework without consultation or standing.
 *   - Papal Curia: Agenda-setting authority (institutional/constrained) â provides the spiritual-legal kernel and interpretive apparatus that grounds the demarcation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.66).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.78).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.66).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Demarcation: Portuguese Exploration Legitimation Reading").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '6b68e2fb-c152-46b8-99a8-99d15d33174d').
narrative_ontology:cs_kernel_codification('6b68e2fb-c152-46b8-99a8-99d15d33174d', fixed_text).
narrative_ontology:cs_authority_grounding('6b68e2fb-c152-46b8-99a8-99d15d33174d', lineage).
narrative_ontology:cs_interpretation_layer_present('6b68e2fb-c152-46b8-99a8-99d15d33174d').
narrative_ontology:cs_reading_relation('6b68e2fb-c152-46b8-99a8-99d15d33174d', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('6b68e2fb-c152-46b8-99a8-99d15d33174d', foundational, prior_discovery_confers_exclusive_commerce).
narrative_ontology:cs_axiom_status(prior_discovery_confers_exclusive_commerce, holdable).
narrative_ontology:cs_axiom_grounding('6b68e2fb-c152-46b8-99a8-99d15d33174d', prior_discovery_confers_exclusive_commerce, conventional).
narrative_ontology:cs_axiom('6b68e2fb-c152-46b8-99a8-99d15d33174d', foundational, papacy_may_adjudicate_sovereignty_among_catholic_crowns).
narrative_ontology:cs_axiom_status(papacy_may_adjudicate_sovereignty_among_catholic_crowns, holdable).
narrative_ontology:cs_axiom_grounding('6b68e2fb-c152-46b8-99a8-99d15d33174d', papacy_may_adjudicate_sovereignty_among_catholic_crowns, theological).
narrative_ontology:cs_reference_frame('6b68e2fb-c152-46b8-99a8-99d15d33174d', christendom_sovereignty_allocation).
narrative_ontology:cs_drift_state('6b68e2fb-c152-46b8-99a8-99d15d33174d', post_reformation_north_european_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b68e2fb-c152-46b8-99a8-99d15d33174d', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the demarcation line and exclusive trading rights east of the line through naval patrols, fortification of strategic ports, and diplomatic pressure on other Catholic crowns. Justifies the monopoly as protecting prior exploration investments, spreading Christianity, and preventing intra-Iberian conflict.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, constrained, global).

% Operates the network of trading posts (feitorias) and fortified factories across the Indian Ocean, Southeast Asia, and the African coast. Collects monopoly rents on spices, precious metals, and slaves. Its commercial viability depends on excluding Dutch, English, and French merchants from established routes and port access.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    organized, biographical, constrained, global).

% Dutch, English, and French crowns and merchant companies excluded from lucrative eastern trade routes by Portuguese naval interdiction and the legal-diplomatic apparatus of the demarcation. They bear the cost of diverted trade, military confrontation, or delayed market entry, and fund expeditions to circumvent or breach the monopoly.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, biographical, constrained, continental).

% African, Asian, and American populations whose territories and waters were traversed, claimed, and allocated by European powers. They were not parties to the treaty, had no standing in its legal framework, and their existing sovereignty and trading systems were overwritten without consultation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_peoples, excluded,
    powerless, generational, trapped, local).

% Issued the papal bulls and negotiated the treaty that drew the demarcation line and allocated spheres of influence. Derives authority from apostolic spiritual jurisdiction over Christian princes; does not collect trade rents but gains political deference and the expansion of Christendom under its moral leadership.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates competing Iberian territorial claims in the wake of transatlantic discovery by drawing a demarcation line, establishing a single legal-religious framework for Christian expansion and preventing direct war between the two principal Catholic naval powers over newly encountered lands.
% TRANSFER_FUNCTION: Moves exclusive commercial jurisdiction and monopoly trading rights in the eastern hemisphere from rival European merchants and crowns to the Portuguese Crown and its Estado da Ãndia, legitimated by papal spiritual authority.
% ABSENT_VOICES: Indigenous peoples of Africa, Asia, and the Americas; non-Catholic European powers and their merchants; established Asian and African trading states and intermediaries. They were not consulted in the allocation of spheres and had no standing in the treaty framework.
% DISAPPEARANCE_RATIONALE: If the demarcation and its papal confirmation vanished overnight, Portuguese legal justification for interdicting rival European shipping would collapse; Dutch, English, and French merchants would rapidly encroach on Asian trade routes and ports (as historically occurred after 1580), and the early modern trading system would reorganize from a single Crown monopoly toward competitive multinational commerce.
% FOUNDING_PROBLEM: Competing territorial claims between Spain and Portugal in the wake of Columbus's 1492 voyage, risking intra-Iberian naval conflict and undermining the unity of Christendom's expansion; combined with the need to legally exclude other European powers from trade routes the Portuguese had begun exploring along the African coast and into the Indian Ocean.
% FOUNDING_PROBLEM_CORROBORATION: Portuguese royal chroniclers and Crown jurists attest the problem was preventing Spanish encroachment on prior Portuguese exploration. Independent Venetian, Genoese, and later Dutch and English observers attest the arrangement was a preemptive cartel dividing anticipated spoils; corroboration from outside the benefiting Iberian parties is limited because excluded rivals were not present at the 1494 negotiations.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) is moderate-high because the Portuguese Crown captured substantial monopoly rents from spice and slave trades, decoupled from any marginal service provided to rivals. Suppression (0.78) is high because the constraint's persistence required active naval interdiction, port denial, and diplomatic threats against European competitors. Theater ratio (0.55) reflects that by the mid-16th century, much of the monopoly's maintenance was legal-ritual performance (asserting papal titles, ceremonial claims) while actual enforcement increasingly depended on pure naval force; the legal form became partly theatrical as Dutch and English encroachment grew. Accessibility collapse (0.40) is moderate because alternatives existed for rivals (circumnavigation, smuggling, direct confrontation) but at high cost and risk. Resistance (0.70) is high because Dutch, English, and French merchants and crowns actively contested the monopoly through privateering, alternative route-finding, and eventual open warfare.
 *
 * PERSPECTIVAL GAP:
 *   The Portuguese Crown and Estado da Ãndia experience the constraint as a necessary coordination mechanism protecting prior investment and Christian expansion; they compute a low directionality (beneficiary). Rival European powers experience the same legal structure as an enforced cartel blocking their entry into Asian markets; they compute a high directionality (target). Indigenous peoples sit outside the directionality derivation entirely because they are not structurally positioned as beneficiaries or victims in this specific reading, though they are territorially affected.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Portuguese Estado da Ãndia) and victims (rival European powers) are explicitly declared. The Crown sits near the beneficiary end as the enforcing seat that captures the monopoly rent. Rivals sit near the target end as the excluded parties against whom enforcement is directed. The Papal Curia sits near symmetric: it provides the legitimating framework but does not collect material rents, gaining only diffuse spiritual deference.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing Iberian war and legitimizing prior exploration â was substantially contested and arguably solved by the treaty itself. However, the arrangement persisted well beyond its coordination function, evolving into a sustained extraction mechanism. The Portuguese reading prevents mislabeling the constraint as pure snare by acknowledging the genuine Iberian coordination problem it addressed, while the victim declaration prevents mislabeling it as pure rope by naming the asymmetric extraction from excluded Europeans.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_indigenous_target,
    'Does this constraint''s primary extraction target rival European powers to the exclusion of indigenous populations, or does indigenous subjugation inhere in the same kernel under the sibling Spanish conquest legitimation reading?',
    'Comparative structural analysis of the sibling spanish_conquest_legitimation reading; evaluate whether the two readings describe separable constraints or coupled aspects of a single extraction structure.',
    'If indigenous subjugation is inseparable from the kernel, this reading''s epsilon misprices the constraint and the victim set is incomplete; if separable, the Portuguese trade-monopoly reading is distinct from the territorial-conquest reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_indigenous_target, conceptual, 'Commit uncertainty about kernel reading boundary and indigenous targeting').

omega_variable(
    papal_authority_universality,
    'Was the papal demarcation understood as exercising universal natural-law jurisdiction over all peoples, or as an interstate compact valid only among Catholic crowns?',
    'Historiographic analysis of bulls Inter Caetera and Dudum Siquidem, plus contemporary legal commentary (Vitoria, Gentili) on the binding force over non-Christian rulers.',
    'A universal natural-law claim would assert Mountain-like immunity; an interstate compact reading confirms Tangled Rope status with no binding force on non-parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_universality, conceptual, 'Natural-law vs conventional grounding of papal authority').

omega_variable(
    enforcement_ritual_vs_material,
    'Did Portuguese naval enforcement actually exclude rivals materially, or did the treaty function as legal theater while real exclusion depended on naval superiority independent of the papal grant?',
    'Archival analysis of Portuguese naval deployments, interdiction records, and rival European shipping penetration rates in the Indian Ocean 1494-1580.',
    'If enforcement was primarily material rather than legal, the treaty''s extraction was a Tangled Rope of real coordination plus asymmetric extraction; if the legal form was purely theatrical, it trends toward Piton or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_ritual_vs_material, empirical, 'Material vs ritual enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0, 86).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tordesillas_port_tr_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tordesillas_port_tr_t15, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 15, 0.25).
narrative_ontology:measurement(tordesillas_port_tr_t30, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 30, 0.3).
narrative_ontology:measurement(tordesillas_port_tr_t45, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 45, 0.38).
narrative_ontology:measurement(tordesillas_port_tr_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 60, 0.48).
narrative_ontology:measurement(tordesillas_port_tr_t75, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 75, 0.58).
narrative_ontology:measurement(tordesillas_port_tr_t86, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 86, 0.65).

% Extraction over time
narrative_ontology:measurement(tordesillas_port_be_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tordesillas_port_be_t15, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(tordesillas_port_be_t30, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(tordesillas_port_be_t45, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(tordesillas_port_be_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(tordesillas_port_be_t75, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 75, 0.7).
narrative_ontology:measurement(tordesillas_port_be_t86, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 86, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tordesillas_port_su_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tordesillas_port_su_t15, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(tordesillas_port_su_t30, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(tordesillas_port_su_t45, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(tordesillas_port_su_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(tordesillas_port_su_t75, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 75, 0.85).
narrative_ontology:measurement(tordesillas_port_su_t86, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 86, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This story is one reading of the tordesillas_demarcation_kernel, decomposed from the colloquial label 'Treaty of Tordesillas' into structurally distinct claims: Portuguese exploration legitimation (trade monopoly east) and Spanish conquest legitimation (territorial conquest west). The epsilon values and victim sets differ between the two readings, requiring separate constraint stories per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
