% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Deferred-Regime Reading (Appropriation Question Suspended Pending Multilateral Framework)
 *   domain: international_law/commons_governance/space_policy
 *
 * SUMMARY:
 *   This story instantiates the 'deferred regime' reading of Article II of
 *   the Outer Space Treaty: the article neither permits nor prohibits
 *   resource extraction, but hands the question to a future multilateral
 *   framework analogous to the Moon Agreement's Article XI 'common heritage
 *   of mankind' regime — a framework that has never been concluded among
 *   spacefaring states. Structurally this is a scaffold: the deferral was
 *   meant to be transitional, buying time for a considered multilateral
 *   answer, with an implicit sunset when that regime materializes. Instead,
 *   the absence of the regime has become the steady state. COPUOS
 *   negotiations have stalled for decades on zero-sum distributional
 *   questions (who gets priority claims, whether benefit-sharing is owed to
 *   non-spacefaring states), and in that vacuum first-mover states and firms
 *   are constructing extraction rights unilaterally through domestic
 *   legislation. The theater ratio rises over the interval because COPUOS
 *   working-group activity continues — meetings, working papers, draft
 *   principles — while producing no binding resolution; the
 *   coordination-adjacent activity increasingly performs deliberation rather
 *   than converging on one.
 *
 * KEY AGENTS:
 *   - first_mover_extraction_firms: primary beneficiary of the ambiguity itself
 *   - spacefaring_state_governments: agenda_setter controlling both the domestic legal cover and the diplomatic venue
 *   - non_spacefaring_states: structurally powerless payer bearing precedent-setting costs
 *   - smaller_commercial_entrants: payer frozen out of capital by unresolved legal risk
 *   - future_generations_claimants: excluded, non-agent, rhetorically invoked but unrepresented
 *   - international_legal_scholars: analytical observer documenting the gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.42).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.28).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Deferred-Regime Reading (Appropriation Question Suspended Pending Multilateral Framework)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_law/commons_governance/space_policy").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '1c70d565-29a8-4438-8441-d979f2fbdb49').
narrative_ontology:cs_kernel_codification('1c70d565-29a8-4438-8441-d979f2fbdb49', fixed_text).
narrative_ontology:cs_authority_grounding('1c70d565-29a8-4438-8441-d979f2fbdb49', distributed).
narrative_ontology:cs_reading_relation('1c70d565-29a8-4438-8441-d979f2fbdb49', ost_article_ii_non_appropriation__extraction_permissive, influences).
narrative_ontology:cs_reading_relation('1c70d565-29a8-4438-8441-d979f2fbdb49', ost_article_ii_non_appropriation__commons_conservation, influences).
narrative_ontology:cs_axiom('1c70d565-29a8-4438-8441-d979f2fbdb49', foundational, appropriation_question_requires_multilateral_settlement).
narrative_ontology:cs_axiom_status(appropriation_question_requires_multilateral_settlement, holdable).
narrative_ontology:cs_axiom_grounding('1c70d565-29a8-4438-8441-d979f2fbdb49', appropriation_question_requires_multilateral_settlement, conventional).
narrative_ontology:cs_axiom('1c70d565-29a8-4438-8441-d979f2fbdb49', foundational, unilateral_state_practice_cannot_substitute_for_treaty_authority).
narrative_ontology:cs_axiom_status(unilateral_state_practice_cannot_substitute_for_treaty_authority, holdable).
narrative_ontology:cs_axiom_grounding('1c70d565-29a8-4438-8441-d979f2fbdb49', unilateral_state_practice_cannot_substitute_for_treaty_authority, deontological).
narrative_ontology:cs_reference_frame('1c70d565-29a8-4438-8441-d979f2fbdb49', moon_agreement_common_heritage_framework).
narrative_ontology:cs_drift_state('1c70d565-29a8-4438-8441-d979f2fbdb49', post_artemis_accords_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c70d565-29a8-4438-8441-d979f2fbdb49', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, spacefaring_state_governments).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, future_generations_claimants).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, smaller_commercial_entrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate asteroid and lunar resource prospecting and extraction programs under domestic licensing regimes (US, Luxembourg, UAE) that assert extraction rights unilaterally. The deferred-regime reading lets them proceed without a multilateral framework telling them they cannot, and without one telling them exactly what they owe or to whom. They benefit from the ambiguity itself: it is cheaper to act first and litigate later than to wait for a regime that might impose royalties, benefit-sharing, or caps.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms, beneficiary,
    organized, biographical, arbitrage, global).

% Control the COPUOS negotiation calendar and can accelerate or stall regime formation. Also issue domestic space-resource statutes that function as de facto policy in the absence of the treaty regime the article defers to. They set the pace of the deferred question because they administer both the domestic legal cover and the diplomatic venue where the future regime would be built.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_state_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Signed the OST on the understanding that celestial resources were a common concern to be governed collectively (the 'common heritage' logic underlying Article XI's later, largely unratified elaboration). They have no extraction capability and no leverage to compel regime formation; every year of deferral is a year in which first movers establish facts on the ground that a future regime will find politically costly to unwind. Their only lever is the UN General Assembly and COPUOS consensus process, which spacefaring states can slow-walk indefinitely.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states, payer,
    powerless, generational, trapped, global).

% Lack the capital or state backing to self-insure against future regime risk the way first movers can. They face investor reluctance and insurance costs precisely because the appropriation question remains open; a firm regime (either permissive or restrictive) would let them price risk, but the deferred state leaves them frozen out of financing that requires legal certainty.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, smaller_commercial_entrants, payer,
    moderate, biographical, constrained, global).

% Have no seat in COPUOS and no standing to object to extraction decisions made now that will shape the resource base and legal precedent they inherit. Represented, if at all, only rhetorically in 'common heritage of mankind' language that has no enforceable content absent the deferred regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_generations_claimants, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__international_regime, future_generations_claimants).

% Analyze state practice, domestic statutes, and COPUOS working-group documents to assess whether customary international law is crystallizing around the extraction-permissive reading by default, simply because the deferred regime never arrives. They document the gap but cannot close it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defers a genuinely hard distributive question — who may extract celestial resources and on what terms — to a future multilateral framework, avoiding a premature, possibly unworkable rule imposed by treaty drafters in 1967 who could not anticipate commercial extraction capability.
% TRANSFER_FUNCTION: Moves de facto priority and precedent-setting advantage from patient multilateral process to whichever actors can act first under domestic cover; moves risk and uncertainty costs onto capital-constrained entrants and diplomatically weak states who cannot force regime formation.
% ABSENT_VOICES: Non-spacefaring states and any representation of future generations have no mechanism to compel COPUOS consensus; they are formally parties to the OST but structurally unable to convert that status into an accelerated or binding regime.
% DISAPPEARANCE_RATIONALE: If the deferral itself vanished — i.e., if a multilateral regime were actually concluded, resolving the appropriation question either way — first-mover firms would face either confirmed property rights (removing their arbitrage advantage over slower entrants) or confirmed prohibition/benefit-sharing obligations (removing the value of unilateral domestic licensing). Either resolution collapses the grey-zone advantage the deferral currently preserves; investment calculus, insurance pricing, and diplomatic leverage would all shift substantially.
% FOUNDING_PROBLEM: In 1967, the OST drafters could not resolve whether resource extraction from celestial bodies constituted prohibited 'appropriation' under Article II, so they left the question open, later gesturing toward a dedicated regime (the 1979 Moon Agreement's Article 11, itself unratified by spacefaring states) rather than resolving it in the text.
% FOUNDING_PROBLEM_CORROBORATION: COPUOS Legal Subcommittee working papers (2016-present) and independent international law scholarship (e.g., analyses from the University of Nebraska Space, Cyber, and Telecommunications Law program and multiple UN rapporteur submissions) attest that the appropriation question remains genuinely unresolved as a matter of positive international law, not merely as a matter that beneficiary states prefer to leave open — though these same scholars note that continued deferral increasingly serves spacefaring states' interests regardless of original intent.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) rather than high because the deferral itself does not directly transfer wealth — it is the mechanism ENABLING a transfer that occurs through the parallel, unilateral domestic-licensing track riding on top of it. Suppression is low-moderate (0.28): no party is coerced into silence; non-spacefaring states can and do object publicly, they simply lack a mechanism to convert objection into a binding regime. Theater ratio is authored HIGH and rising (0.58 by T=40) because this is the central diagnostic feature of the story: COPUOS process continues to produce papers, working groups, and draft principles without resolving the underlying question, which is exactly the performative-coordination-without-function signature scaffold decay produces when the sunset never arrives. Accessibility collapse is moderate (0.35) — alternatives to the deferred posture (concluding either the permissive or conservationist regime) remain technically available and are actively drafted, just never adopted. Resistance is moderate-high (0.55): non-spacefaring states and legal scholars actively contest the drift toward de facto extraction-permissive practice.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of spacefaring states and first-mover firms, deferral looks like prudent, ongoing multilateral process — a scaffold doing exactly what scaffolds do, waiting for the right moment to conclude. From the seat of non-spacefaring states, the same deferral looks increasingly like a de facto extraction-permissive regime being constructed by unilateral state practice while the promised multilateral answer is permanently delayed — the engine should register this as computed seat divergence rather than an authored contradiction.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover firms and spacefaring states sit near the beneficiary end of directionality: the deferred state is not neutral to them, it is actively useful, since it lets them establish extraction practice without having to negotiate away rights in a concluded regime. Non-spacefaring states and smaller commercial entrants sit near the target end: they bear the costs of unresolved risk (financing costs, precedent loss) without the capability to exploit the ambiguity themselves. Future generations are declared as a non-agent excluded party — they cannot bear directionality in the formal sense but are named for narrative completeness, per the schema's non-agent convention, and are explicitly excluded from beneficiary/victim derivation weight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the 1967 drafters' genuine inability to resolve appropriation ex ante) was live and legitimate. Its status here is authored 'live' rather than 'dead' because the underlying legal and technical uncertainty has not actually been resolved by anyone — no regime exists that answers the question either way. But the corroboration entry flags the mandatrophy risk directly: independent scholarship increasingly observes that continued deferral serves spacefaring-state interests regardless of original intent, meaning the scaffold's declared sunset condition (regime conclusion) is receding rather than approaching. This is the classic scaffold-to-piton drift signature — a temporary coordination structure whose justification (the transition) is being displaced by an emergent steady-state benefit to some parties, without those parties needing to argue the deferral should continue; it just needs to keep not-concluding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_conclusion_timeline,
    'Will a binding multilateral space-resource regime ever be concluded, or has the deferred state become permanent through the accumulation of unilateral state practice?',
    'Track COPUOS Legal Subcommittee agenda items and draft-principle convergence over the next decade; a Moon-Agreement-style outcome (near-universal non-ratification by spacefaring states) versus genuine treaty conclusion would resolve this empirically.',
    'If no regime is ever concluded, this scaffold reading converges toward the extraction_permissive reading by default (customary law crystallizing around state practice) despite never having treaty authority to do so — effectively validating one sibling reading through inaction rather than argument, which would retroactively suggest the deferral was a snare-in-scaffold-clothing for non-spacefaring states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_conclusion_timeline, empirical, 'Whether deferral resolves into a concluded regime or becomes permanent through accreted unilateral practice.').

omega_variable(
    reading_authority_indeterminacy,
    'Is the international_regime reading itself the legally correct reading of Article II, or is it merely the diplomatically convenient reading that lets spacefaring states avoid committing to either the permissive or conservationist position while acting as if the permissive reading holds?',
    'International Court of Justice advisory opinion, or a critical mass of state practice/opinio juris analysis by the International Law Commission, could establish whether ''deferred'' is a description of genuine legal uncertainty or a diplomatic fiction covering de facto extraction-permissive practice.',
    'If the deferred reading is itself a fiction — if state practice already demonstrates opinio juris toward extraction-permissive customary law — then this constraint''s claimed_type of scaffold is a false transitional framing for what is structurally already a tangled_rope or snare favoring first-mover states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_authority_indeterminacy, conceptual, 'Whether the deferral reading describes genuine indeterminacy or masks an already-settled de facto permissive practice.').

omega_variable(
    coalition_capacity_non_spacefaring_states,
    'Can non-spacefaring states form an effective negotiating coalition (as they did, unsuccessfully in binding-force terms, for the 1979 Moon Agreement) to force regime conclusion before extraction practice forecloses their bargaining position?',
    'Monitor G77/developing-state coordination within COPUOS and UN General Assembly resolutions on space resources for signs of unified bargaining position formation versus continued fragmentation.',
    'Successful coalition formation would shift the powerless payer stakeholders toward organized power, potentially forcing regime conclusion; continued fragmentation confirms the trapped/powerless characterization is durable, not merely a snapshot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_capacity_non_spacefaring_states, empirical, 'Whether non-spacefaring states can convert formal treaty-party status into effective coalition leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ost__tr_t8, ost_article_ii_non_appropriation__international_regime, theater_ratio, 8, 0.35).
narrative_ontology:measurement(ost__tr_t16, ost_article_ii_non_appropriation__international_regime, theater_ratio, 16, 0.42).
narrative_ontology:measurement(ost__tr_t24, ost_article_ii_non_appropriation__international_regime, theater_ratio, 24, 0.48).
narrative_ontology:measurement(ost__tr_t32, ost_article_ii_non_appropriation__international_regime, theater_ratio, 32, 0.53).
narrative_ontology:measurement(ost__tr_t40, ost_article_ii_non_appropriation__international_regime, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ost__be_t8, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(ost__be_t16, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(ost__be_t24, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(ost__be_t32, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(ost__be_t40, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ost_article_ii_non_appropriation__international_regime, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the ost_article_ii_non_appropriation kernel. The extraction_permissive reading (Article II bars sovereign claims but not private ownership of extracted resources) and the commons_conservation reading (Article II's 'use or occupation' language prohibits de facto appropriation) each claim the treaty text itself resolves the appropriation question, in opposite directions, with correspondingly different ε profiles (each substantially higher-confidence and more extractive/protective in their own framing than this deferred reading, which authors a moderate ε reflecting genuine unresolved status rather than a resolved-but-contested claim). This international_regime reading instead claims the text does NOT resolve the question, producing the distinctive scaffold classification absent from either sibling. All three share the same underlying treaty text and beneficiary/victim actors but diverge entirely on which actors the constraint's operation currently favors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
