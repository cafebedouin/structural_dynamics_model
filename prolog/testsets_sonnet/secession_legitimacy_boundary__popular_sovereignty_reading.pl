% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Provincial Secession Legitimacy
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   secession_legitimacy_boundary kernel: the popular sovereignty reading,
 *   under which a bare provincial referendum majority is treated as
 *   self-legitimating and sufficient to establish a right to unilateral exit,
 *   subordinating federal constitutional authority and treaty-holder consent
 *   to provincial electoral outcome. This is not a story about the general
 *   question of secession legitimacy — it is the specific structural claim
 *   that majoritarian vote within provincial boundaries is, by itself, the
 *   complete legitimating mechanism. The sibling readings
 *   (constitutional_impossibility, grievance_threshold, treaty_primacy) are
 *   separate constraints with separate ε values, evaluated in their own
 *   files. As this reading matures politically and legally, it increasingly
 *   requires active defense against constitutional challenge and treaty
 *   claims, and the referendum mechanism itself becomes contested terrain
 *   (threshold-setting, question-wording disputes) — hence the rising
 *   theater_ratio and suppression_requirement trajectories.
 *
 * KEY AGENTS:
 *   - provincial_secessionist_majority: Primary beneficiary (organized/mobile) — captures sovereignty claim and resource control
 *   - provincial_political_leadership: Agenda-setter (institutional/arbitrage) — designs and administers the referendum
 *   - provincial_minority_dissenters: Primary target (moderate/constrained) — bound by a result they rejected
 *   - indigenous_treaty_nations: Excluded party (powerless/trapped) — treaty sovereignty erased from the framing
 *   - federal_taxpayers_outside_province: Diffuse payer (powerless/trapped) — absorbs fiscal shock without a vote
 *   - cross_border_resource_dependent_communities: Secondary payer (moderate/constrained) — bears externalized infrastructure disruption
 *   - federal_government: Excluded institutional party (institutional/constrained) — sovereignty claim subordinated
 *   - constitutional_courts: Analytical observer — adjudicates the reading's practical force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.52).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.47).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Reading of Provincial Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, 'c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6').
narrative_ontology:cs_kernel_codification('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', distributed).
narrative_ontology:cs_authority_grounding('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', distributed).
narrative_ontology:cs_reading_relation('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', foundational, provincial_electoral_majority_is_sufficient_sovereignty).
narrative_ontology:cs_axiom_status(provincial_electoral_majority_is_sufficient_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', provincial_electoral_majority_is_sufficient_sovereignty, conventional).
narrative_ontology:cs_axiom('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', secondary, referendum_outcome_requires_no_external_ratification).
narrative_ontology:cs_axiom_status(referendum_outcome_requires_no_external_ratification, holdable).
narrative_ontology:cs_axiom_grounding('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', referendum_outcome_requires_no_external_ratification, instrumental).
narrative_ontology:cs_reference_frame('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', post_westphalian_popular_sovereignty_norm).
narrative_ontology:cs_drift_state('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', contemporary_multinational_federalism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c0d91f9c-8b92-48c2-aeb2-e4a90d4b55d6', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_political_leadership).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_dissenters).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_taxpayers_outside_province).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, cross_border_resource_dependent_communities).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, referendum_self_legitimation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A voting majority within provincial boundaries backs a referendum declaring the province's exit from the federation legitimate by virtue of the vote alone. They control the provincial legislature, set the referendum question and threshold, and stand to gain full control over resource revenues, taxation, and governance currently shared with or ceded to the federal structure. Their exit is framed as self-evident democratic right requiring no external ratification.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority, agenda_setter).

% Provincial government officials draft the referendum question, control its timing and threshold, and administer the vote. They gain expanded jurisdiction, control over resource royalties, and international recognition claims if the reading holds. They benefit from ambiguity about what threshold (50%+1 versus supermajority) counts as legitimate, since they set it.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_political_leadership, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Residents within the province who oppose secession are bound by the referendum result regardless of the margin, losing federal citizenship, currency, and institutional protections they did not vote to leave. Their only recourse is relocation out of the province or accepting a new sovereign arrangement they rejected at the ballot box; internal minority status within the newly sovereign unit offers no exit.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_dissenters, payer,
    moderate, biographical, constrained, regional).

% Nations holding treaties predating both federal and provincial sovereignty are not party to the referendum and their consent is not sought under this reading, even though the seceding province's territory substantially overlaps treaty lands and resources. The popular sovereignty framing treats provincial electoral majority as sufficient, erasing treaty-holder status as a separate sovereignty claim.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_nations, excluded,
    powerless, civilizational, trapped, regional).

% Citizens in the remainder of the federation absorb the fiscal, currency, and debt-apportionment shocks of secession without having voted on it, and without standing to object under a reading that locates ultimate sovereignty entirely within the seceding province's boundaries.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_taxpayers_outside_province, payer,
    powerless, generational, trapped, national).

% Communities adjacent to the province whose water, energy, or transit infrastructure crosses the proposed new border face disrupted access and unresolved cross-jurisdictional governance the moment the province declares the referendum self-legitimating, since this reading treats external effects as someone else's problem to solve after the fact.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, cross_border_resource_dependent_communities, payer,
    moderate, biographical, constrained, regional).

% The federal government, under this reading, has no veto and no constitutionally required role beyond post-hoc negotiation of terms; its own claim to determine the legitimacy of the boundary and the process is treated as subordinate to the provincial vote, a position it does not accept but cannot unilaterally override once the reading gains political traction.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, excluded,
    institutional, generational, constrained, national).

% Courts are asked to adjudicate whether a bare referendum majority is self-legitimating or whether it triggers a negotiation duty rather than automatic exit. Their rulings shape which reading of the kernel gains practical force, but under the popular sovereignty reading their role is advisory at most — political fact is expected to outrun legal process.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the seceding province's population with a clear, low-ambiguity mechanism (a majority vote) for resolving disputed sovereignty claims internally, avoiding protracted multi-party negotiation before any exit can proceed.
% TRANSFER_FUNCTION: Moves ultimate authority over territory, resources, taxation, and treaty administration from the federal structure and from non-consenting parties within and adjacent to the province to the provincial majority, on the strength of a single vote whose threshold and terms the majority itself sets.
% ABSENT_VOICES: Indigenous treaty nations whose land and resource rights predate the province and the federation are not consulted as a distinct sovereign party. Federal taxpayers elsewhere in the country, and residents of neighboring jurisdictions dependent on cross-border infrastructure, have no vote and no seat in the referendum design.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty reading were abandoned overnight, the secessionist majority and provincial leadership would lose their claimed self-legitimating exit path and would need to negotiate through constitutional or treaty-based channels instead — a substantial rearrangement for them. Federal authorities, treaty nations, and provincial minorities would regard this as restoring rather than disrupting the prior order, since their position is that the popular-sovereignty claim is itself the destabilizing intervention.
% FOUNDING_PROBLEM: The reading was constructed to resolve the practical problem that provinces with strong secessionist movements faced no clear internal path to test or express sovereign will short of unilateral declaration, and to give referendum outcomes binding force without requiring prior federal or treaty-holder agreement.
% FOUNDING_PROBLEM_CORROBORATION: Provincial secessionist leadership and allied political theorists attest the founding problem (lack of a democratic exit mechanism) remains live and unresolved. Federal constitutional scholars, treaty nation representatives, and comparative federalism researchers outside the secessionist movement dispute the premise itself, arguing no such gap exists once negotiated-exit and treaty-consent channels are counted, and that the reading manufactures a problem to justify majoritarian override of non-consenting parties.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.52 because the reading concentrates gains (resource control, jurisdiction) in the provincial majority and leadership while imposing uncompensated costs on treaty nations, minority dissenters, and external parties who had no vote — but the extraction is bounded by the fact that a genuine coordination function (resolving an otherwise unresolvable sovereignty dispute) is present. Suppression is moderate (0.47): the reading does not primarily rely on coercion of the majority itself, but it does suppress treaty-holder and minority objections by definitional fiat (declaring the referendum self-legitimating forecloses their standing to object procedurally). Theater ratio is moderate and rising (0.38 at T=40) because as the reading is invoked politically, increasing energy goes into referendum-design theater (threshold selection, question framing) rather than substantive resolution of the excluded parties' claims. Resistance is high (0.68) reflecting the fact that this reading is fiercely contested by federal authorities, treaty nations, and comparative constitutional scholarship — it is far from a settled or uncontested claim.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial majority's seat, this is Rope or at most Tangled Rope: a legitimate democratic mechanism resolving a real coordination failure (no clear internal exit path existed). From the treaty nations' and federal government's seats, the same structure computes as extractive and coercive — the referendum's self-legitimating claim is precisely what forecloses their standing to object, which is a suppression mechanism operating through definitional exclusion rather than physical force. The engine's per-seat computation should reflect this divergence structurally rather than resolve it: the claim (tangled_rope) already builds in this seat-asymmetry by requiring both a coordination function AND victims who pay through the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial secessionist majority and its political leadership sit near the full-beneficiary end: they set the referendum terms, control timing and threshold, and capture the resulting sovereignty and resource claims. Provincial minority dissenters sit near the target end: constrained exit, bound by a result they opposed, no meaningful alternative within the new sovereign unit. Indigenous treaty nations and federal taxpayers elsewhere sit at the most extractive end relative to their exit options (trapped) precisely because this reading treats their consent as unnecessary — the reading's core move is to locate ALL relevant sovereignty inside the provincial boundary, which is exactly what makes it extractive toward parties whose claims exist partly or wholly outside or athwart that boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the absence of a clear internal democratic exit mechanism — may or may not still be live depending on whether one credits the counter-claim that negotiated-exit and treaty-consent channels already exist and were simply not the ones the secessionist movement wanted to use. Classifying this as tangled_rope rather than snare or pure rope prevents two mislabeling failures: treating it as pure extraction would erase the genuine coordination problem majoritarian referendum solves for the provincial population; treating it as pure rope would erase the uncompensated costs imposed on treaty nations, dissenting minorities, and external parties who never consented to the boundary redefinition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    referendum_threshold_legitimacy,
    'Does a bare 50%+1 majority carry the same self-legitimating force this reading claims, or does the reading implicitly require a supermajority that the provincial leadership has strategic incentive to avoid specifying?',
    'Comparative analysis of prior secession referenda thresholds accepted as legitimate by international bodies and negotiating counterparts (e.g., Scotland 2014, Quebec 1995/1980, Catalonia 2017) and whether ambiguity in this reading''s threshold has historically produced downstream contestation.',
    'If a bare majority is insufficient in practice, the reading''s central claim (referendum result is self-legitimating) is weaker than authored, and the constraint''s suppression score may be understated since threshold ambiguity is itself a tool for whichever side is setting the rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_threshold_legitimacy, conceptual, 'Whether a bare majority genuinely suffices under this reading or whether unstated supermajority norms operate.').

omega_variable(
    treaty_claim_severability,
    'Can provincial popular sovereignty and indigenous treaty sovereignty be treated as genuinely separable claims over the same territory, or does asserting the former necessarily extinguish or subordinate the latter?',
    'Comparative constitutional and international law analysis of cases where sub-state secession claims overlapped with pre-existing indigenous treaty territories, examining whether any resolution mechanism has successfully held both claims as compossible.',
    'If the claims are not severable, this reading is not merely silent on treaty nations but structurally incompatible with treaty_primacy_reading in a way that would warrant reclassifying the relation from coexists_with to forecloses; if severable, the current coexists_with framing holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_claim_severability, conceptual, 'Whether provincial popular sovereignty and treaty sovereignty claims over the same land are logically compossible.').

omega_variable(
    which_reading_is_operative_kernel_ambiguity,
    'In an actual secession crisis, which of the four kernel readings would courts, other states, and international bodies actually treat as operative — and does the popular sovereignty reading''s practical force come from its normative persuasiveness or from the political fact of the majority''s mobilization capacity?',
    'Track record of how comparable disputes were actually resolved (negotiation, international recognition patterns, domestic court rulings) versus how they were rhetorically framed by the parties who benefited from each framing.',
    'If the reading''s practical force derives mainly from mobilization capacity rather than normative persuasiveness, the reading functions more as a legitimation narrative for a power fact than as an independent legal or moral claim, which would push its classification toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_operative_kernel_ambiguity, empirical, 'Whether the popular sovereignty reading''s practical force is normative or a post-hoc rationalization of mobilized power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 40, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the secession_legitimacy_boundary kernel, each authored as a separate constraint story per the ε-invariance principle. The popular_sovereignty_reading claims referendum self-legitimation independent of federal or treaty-holder consent; constitutional_impossibility_reading claims unilateral exit is categorically impermissible absent constitutional amendment; grievance_threshold_reading conditions legitimacy on a structural-injustice threshold rather than either pure majoritarianism or constitutional process; treaty_primacy_reading places indigenous treaty consent above both federal and provincial sovereignty claims. The four readings do not share an ε value — each has a distinct beneficiary/victim structure and distinct extraction profile, linked here via network edges rather than merged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
