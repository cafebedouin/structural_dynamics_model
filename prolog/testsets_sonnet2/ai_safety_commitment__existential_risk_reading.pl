% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: AI Safety as X-Risk Prevention (Existential Risk Reading)
 *   domain: technology/governance/risk
 *
 * SUMMARY:
 *   This story instantiates the existential-risk reading of the contested 'AI
 *   safety' kernel: the claim that AI safety fundamentally means preventing
 *   extinction-level outcomes from misaligned superintelligent systems. Under
 *   this reading, the coordination function (concentrating technical
 *   alignment, interpretability, and governance-pause research) is real and
 *   non-trivial, but it operates through an enforcement apparatus — funding
 *   gatekeeping, conference/publication framing, policy-advisor pipelines —
 *   that systematically routes resources away from documented present-day
 *   algorithmic harms and toward speculative long-horizon technical work,
 *   while the frontier labs administering that apparatus are simultaneously
 *   the entities racing to build the very systems the risk model warns about.
 *   The sibling readings (near_term_harms_reading, dual_priority_reading) are
 *   separate constraint stories with their own ε and stakeholder sets; this
 *   story does not average over them or hedge its ε against theirs.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs_safety_teams: agenda_setter/beneficiary (institutional/arbitrage) — administers the framing and captures funding, legitimacy, hiring pipeline
 *   - existential_risk_research_institutes: beneficiary (organized/mobile) — captures dedicated x-risk research funding and agenda-setting power
 *   - future_humans: payer (powerless/trapped) — the invoked beneficiary class with civilizational stakes and zero present agency
 *   - present_day_algorithmic_harm_victims: payer (powerless/constrained) — bears the opportunity cost of displaced attention and funding
 *   - global_south_ai_deployment_populations: payer (powerless/trapped) — bears labor and deployment costs of the systems being built under the safety banner
 *   - near_term_harm_advocates: excluded (organized/constrained) — present in the field but marginalized within the specific 'AI safety' apparatus
 *   - ai_governance_regulators: observer (institutional/analytical) — must adjudicate between competing readings under lobbying pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.42).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "AI Safety as X-Risk Prevention (Existential Risk Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology/governance/risk").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, 'a69a4a1d-66e7-491a-a146-866789a3e40b').
narrative_ontology:cs_kernel_codification('a69a4a1d-66e7-491a-a146-866789a3e40b', distributed).
narrative_ontology:cs_authority_grounding('a69a4a1d-66e7-491a-a146-866789a3e40b', expertise).
narrative_ontology:cs_interpretation_layer_present('a69a4a1d-66e7-491a-a146-866789a3e40b').
narrative_ontology:cs_reading_relation('a69a4a1d-66e7-491a-a146-866789a3e40b', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('a69a4a1d-66e7-491a-a146-866789a3e40b', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('a69a4a1d-66e7-491a-a146-866789a3e40b', foundational, extinction_severity_dominates_prioritization).
narrative_ontology:cs_axiom_status(extinction_severity_dominates_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('a69a4a1d-66e7-491a-a146-866789a3e40b', extinction_severity_dominates_prioritization, instrumental).
narrative_ontology:cs_axiom('a69a4a1d-66e7-491a-a146-866789a3e40b', secondary, misalignment_risk_is_present_tractable_problem).
narrative_ontology:cs_axiom_status(misalignment_risk_is_present_tractable_problem, holdable).
narrative_ontology:cs_axiom_grounding('a69a4a1d-66e7-491a-a146-866789a3e40b', misalignment_risk_is_present_tractable_problem, empirically_contingent).
narrative_ontology:cs_reference_frame('a69a4a1d-66e7-491a-a146-866789a3e40b', bostrom_yudkowsky_foundational_risk_model).
narrative_ontology:cs_drift_state('a69a4a1d-66e7-491a-a146-866789a3e40b', post_frontier_lab_commercialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a69a4a1d-66e7-491a-a146-866789a3e40b', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment_success).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, frontier_ai_labs_safety_teams).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, existential_risk_research_institutes).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, future_humans).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_day_algorithmic_harm_victims).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, global_south_ai_deployment_populations).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, instrumental_convergence_thesis).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, orthogonality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set research priorities, funding allocation, and public framing around 'AI safety' as extinction prevention. Run interpretability, RLHF, and alignment research programs inside labs that are simultaneously racing to build the systems the research is meant to constrain. Their institutional legitimacy, funding, and hiring pipeline are built on the existential framing; they can pivot resources toward or away from present-harm work at will.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, frontier_ai_labs_safety_teams, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, frontier_ai_labs_safety_teams, beneficiary).

% Philanthropically and lab-funded institutes whose research agendas, career pipelines, and public credibility depend on extinction risk being treated as the central AI safety question. They receive the bulk of dedicated 'AI safety' funding and set conference agendas, journal framing, and policy-advisor pipelines around long-horizon technical alignment problems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, mobile, global).

% Cannot participate in any decision made now about alignment research direction, governance pause proposals, or deployment speed, yet bear the entire tail-risk payoff if the reading's core premise is correct and alignment fails. They have no seat, no proxy with binding authority, and no exit — their stake is invoked constantly but never consulted.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, future_humans, payer,
    powerless, civilizational, trapped, universal).

% People harmed today by biased hiring algorithms, discriminatory risk-scoring, exploitative content-moderation labor, and misinformation-amplifying systems. Under the existential risk reading, resources, researcher attention, and regulatory bandwidth that could address their documented, measurable harms are diverted toward speculative superintelligence scenarios. Their harms are treated as lower priority because they are not extinction-level.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_day_algorithmic_harm_victims, payer,
    powerless, immediate, constrained, national).

% Populations in regions where AI systems are deployed with weaker regulatory oversight and less labor-condition scrutiny (data labeling, content moderation, algorithmic gig-work management). The existential risk framing directs global governance conversation toward frontier-lab pause proposals that do little for their present working conditions, while frontier labs continue extracting low-cost labor from their communities to build the very systems the x-risk framing warns about.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, global_south_ai_deployment_populations, payer,
    powerless, biographical, trapped, regional).

% The abstract collective that benefits IF the alignment research program succeeds and IF the risk model is correct. This beneficiary class has no current agency, no representative body, and its benefit is entirely conditional and unverifiable in advance — it functions rhetorically as the constraint's justification even though it cannot currently confirm or contest that justification.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment_success, beneficiary,
    analytical, civilizational, analytical, universal).

% Researchers, journalists, and advocacy organizations documenting present algorithmic harms who argue the existential framing crowds out their concerns in funding, media attention, and regulatory drafting. They are not absent from the field but are structurally marginalized within the specific institutional apparatus that calls itself 'AI safety.'
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_harm_advocates, excluded,
    organized, immediate, constrained, national).

% Legislators and regulatory bodies drafting AI governance frameworks who must weigh competing framings — extinction-risk pause proposals versus present-harm accountability rules — often under direct lobbying influence from frontier labs whose safety teams also set the extinction-risk research agenda.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_governance_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, frontier_ai_labs_safety_teams).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates researcher attention, philanthropic funding, and emerging governance proposals (compute thresholds, pause triggers, interpretability mandates) around the hypothesis that sufficiently capable misaligned systems pose extinction-level risk, enabling concentrated technical work on alignment, interpretability, and control mechanisms that would be underfunded if diffused across all AI harm categories.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, and public safety-discourse bandwidth away from present, documented, distributional AI harms and toward speculative long-horizon technical alignment work; also moves reputational and funding capital toward the frontier labs and institutes that author and administer the existential framing.
% ABSENT_VOICES: Present-day victims of algorithmic bias, exploited data-labeling and content-moderation workers in the Global South, and the abstract 'future humans' class are all invoked as justification but none sit at the table where research agendas or governance proposals are actually drafted; near-term harm advocates are present in the broader AI ethics field but structurally excluded from the specific 'AI safety' institutional apparatus this reading names.
% DISAPPEARANCE_RATIONALE: If the existential risk framing vanished overnight, the frontier labs and existential risk institutes dispute what would happen: the labs claim capability development would proceed with less safety brake at all, potentially increasing real extinction risk if the premise is correct; near-term harm advocates claim funding and regulatory attention would immediately reallocate to documented present harms, materially improving conditions for algorithmic harm victims and Global South deployment populations. Whether the world 'rearranges' or 'stays the same' depends entirely on whether the underlying extinction risk premise is true — which is exactly the omega this story cannot resolve.
% FOUNDING_PROBLEM: Early AI safety researchers (Bostrom, Yudkowsky, and later some frontier lab founders) identified that a sufficiently capable, misaligned optimization process pursuing goals not perfectly specified by humans could produce catastrophic, irreversible, civilization-ending outcomes, and that this risk was structurally under-addressed by market incentives and existing regulation because the harm is diffuse, low-probability-per-unit-time, and not yet empirically observed.
% FOUNDING_PROBLEM_CORROBORATION: Some independent AI researchers outside frontier labs (academic ML theorists, some governance scholars) corroborate that misalignment risk from advanced systems is a live, unsolved technical problem. Other independent voices — AI ethics researchers, labor scholars studying data-work supply chains, and some philosophers of science — argue from outside the beneficiary set that the extinction framing is empirically unfalsifiable at present capability levels and functions primarily to legitimize continued frontier scaling ('safety-washing') while displacing accountability for measurable present harms; this corroboration is explicitly adversarial to the beneficiary institutes, not aligned with them.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 by interval end: the reading's own technical interventions (RLHF, interpretability research, pause/slowdown governance proposals) are speculative relative to a risk that has not been empirically observed, yet they command a large and growing share of dedicated 'AI safety' funding and regulatory bandwidth — this is the high-ε-on-speculative-interventions structural delta named in the kernel contest. Suppression is moderate (0.42): there is no direct coercion preventing near-term-harm researchers from working, but funding gatekeeping, conference framing, and policy-advisor access function as soft suppression of the competing framing. Theater ratio rises to 0.44 because a growing share of 'AI safety' activity (safety-washing announcements, voluntary commitments without binding mechanisms) is performative relative to verifiable alignment progress. Accessibility collapse is moderate (0.5) — the near-term-harms framing remains a visible alternative in the broader field, it has simply been institutionally out-competed for the specific 'AI safety' label and its resources.
 *
 * PERSPECTIVAL GAP:
 *   From the frontier lab safety team seat, the arrangement reads as genuine, urgent coordination against a civilization-ending risk that the market and existing regulation structurally under-price. From the present-day algorithmic harm victim seat, the same arrangement reads as an extraction mechanism: attention, funding, and governance bandwidth that could fix a measurable present harm is being redirected toward a speculative scenario administered by the same institutions that profit from continued scaling. The engine should compute divergent per-seat types from this structural asymmetry — the coordination function is real, but its beneficiary and administrator sets substantially overlap while its victim sets are diffuse, distant, and largely unrepresented, which is the tangled_rope signature rather than a clean rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier lab safety teams and existential risk institutes sit near the beneficiary end: they administer the framing, capture the funding and legitimacy it generates, and face no binding external check on whether their risk model is validated before resources are allocated on its basis. Future humans sit at the extreme target end: infinite in principle, zero present agency, fully trapped — the classic textbook case for high derived directionality toward the target end. Present-day algorithmic harm victims and global south deployment populations are less extreme but still clearly targets: powerless, constrained or trapped exit, bearing real opportunity costs today for a speculative future benefit. Humanity-conditional-on-alignment-success is analytically positioned as a beneficiary class but its exit options and power are marked analytical/analytical because it is not yet an operative agent — it functions as the reading's rhetorical anchor, not a party that can act.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (misalignment risk from advanced optimization processes) may still be live in a technical sense — that is contested, not resolved, in this story. What is separable is whether the SPECIFIC institutional apparatus that administers the existential-risk reading of 'AI safety' still serves that founding problem efficiently, or whether it has drifted toward serving the career, funding, and legitimacy interests of the institutes and lab safety teams that administer it while under-serving both the future humans it claims to protect and the present-day victims it deprioritizes. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (concentrated technical alignment work is not obviously worthless) while still registering the asymmetric extraction — this is exactly the mislabeling this classification is built to prevent: neither dismissing the safety work as pure extraction nor crediting it as costless pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinction_risk_premise_validity,
    'Is the core premise of this reading — that sufficiently capable misaligned AI systems pose genuine extinction-level risk requiring present resource concentration — empirically well-founded, or is it a currently unfalsifiable claim that functions to legitimize continued capability scaling while deflecting accountability for present harms?',
    'No direct empirical test exists prior to the hypothesized threshold event by construction (the risk is claimed to be low-probability-per-unit-time but catastrophic and potentially irreversible). Partial resolution mechanisms: track record of falsified or confirmed sub-catastrophic alignment failure predictions, independent technical audits of interpretability progress relative to capability progress, and whether governance pause proposals produce measurable slowdown or are captured as safety-washing.',
    'If the premise is well-founded, the coordination function dominates and a rope-leaning reading becomes more defensible despite present opportunity costs. If the premise is primarily unfalsifiable and institutionally self-serving, the tangled_rope reading understates the extraction and a snare classification becomes more defensible — the beneficiary/administrator overlap would then look like capture rather than coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extinction_risk_premise_validity, empirical, 'Whether the extinction risk model justifying this reading''s resource allocation is empirically grounded or currently unfalsifiable.').

omega_variable(
    future_humans_beneficiary_or_victim_ambiguity,
    'Are ''future humans'' primarily a beneficiary class (if alignment succeeds, they inherit a safe civilization) or primarily a victim class (they bear the entire risk of the current generation''s allocation choices with zero say in those choices), and does the story''s dual listing of them under both categories (implicitly, via humanity_conditional_on_alignment_success as beneficiary and future_humans as payer) correctly capture a genuinely bifurcated structural position?',
    'This is not resolvable empirically before the fact — it depends on whether alignment research succeeds, which is itself downstream of the omega above. Philosophically, the two framings can be held simultaneously: future humans are conditional beneficiaries AND unconditional bearers of present-generation risk allocation, which is why this story authors them as distinct stakeholder entries rather than merging them.',
    'If treated purely as beneficiaries, the reading looks more like a rope (real coordination benefiting a real, if future, party). If treated purely as victims of present resource allocation with no say, the payer weighting dominates and the tangled_rope/snare boundary shifts toward snare. The story deliberately keeps both entries to avoid collapsing this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_humans_beneficiary_or_victim_ambiguity, conceptual, 'Whether future humans are best modeled as a conditional beneficiary class, an unconditional victim class, or genuinely both.').

omega_variable(
    cs_framing_kernel_vs_apparatus,
    'Should the kernel here be read as the abstract normative claim (''AI safety means X'') or as the concrete institutional apparatus (specific labs, institutes, funding bodies) that currently administers that claim? The obvious framing treats ''AI safety'' as a contested definitional claim (the kernel as authored). A less obvious framing treats the REAL kernel as the legitimacy narrative of frontier-lab self-regulation — the claim that labs racing to build advanced systems are also the appropriate parties to define and fund the research that constrains those systems.',
    'Track whether alignment/interpretability funding and agenda-setting authority migrate toward independent, non-lab-affiliated bodies over time (would support the definitional-claim framing as primary) or remain concentrated in the same frontier labs building frontier capability (would support the self-regulation-legitimacy framing as primary, in which case this constraint is better read as a Tangled-Rope reading of institutional self-regulation rather than a straightforward reading of a safety-definition kernel).',
    'Under the definitional-claim framing, the cs_pattern here is a relatively clean kernel/reading structure (as authored). Under the self-regulation-legitimacy framing, the extraction would be understated — the true kernel would be ''who has standing to define and fund AI safety,'' and this reading would appear as one move in a legitimacy contest rather than a pure claim about what safety means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_apparatus, conceptual, 'Whether the kernel is best framed as a definitional claim about AI safety or as a legitimacy claim about frontier-lab self-regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__existential_risk_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ai_s_tr_t0, observed).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__existential_risk_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement_basis(ai_s_tr_t4, observed).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__existential_risk_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(ai_s_tr_t8, observed).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__existential_risk_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(ai_s_tr_t12, observed).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__existential_risk_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(ai_s_tr_t16, projected).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__existential_risk_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(ai_s_tr_t20, projected).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__existential_risk_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(ai_s_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(ai_s_be_t0, observed).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__existential_risk_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(ai_s_be_t4, observed).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__existential_risk_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement_basis(ai_s_be_t8, observed).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__existential_risk_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(ai_s_be_t12, observed).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__existential_risk_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(ai_s_be_t16, projected).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__existential_risk_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_s_be_t20, projected).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__existential_risk_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(ai_s_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(ai_s_su_t0, observed).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__existential_risk_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement_basis(ai_s_su_t4, observed).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__existential_risk_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement_basis(ai_s_su_t8, observed).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__existential_risk_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement_basis(ai_s_su_t12, observed).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__existential_risk_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(ai_s_su_t16, projected).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__existential_risk_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(ai_s_su_t20, projected).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__existential_risk_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(ai_s_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, dual_priority_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the ai_safety_commitment kernel. near_term_harms_reading authors a lower ε for present algorithmic accountability interventions and names present, documented victims (bias/discrimination targets, exploited data-labor) as its primary victim set rather than the diffuse future-humans class this reading centers. dual_priority_reading denies the two priorities are structurally competing at all, a premise this reading's own resource-allocation trajectory (rising base_extractiveness and theater_ratio over the interval) puts pressure against. The three stories share no averaged ε; each is self-contained per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, analytical, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
