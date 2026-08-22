% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Monarchy: Dual-Sourced Legitimacy Split Between Inherited Ceremony and Delegated Politics
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story reads the sovereign_legitimacy kernel through the
 *   constitutional-hybrid lens: authority is understood as genuinely
 *   dual-sourced, ceremonial legitimacy inherited by the crown and political
 *   legitimacy delegated by voters to elected officials, with courts and
 *   constitutional convention mediating disputes at the boundary. This is not
 *   a description of the contest between readings — the monarchical reading
 *   (authority flows downward from the sovereign by inherited/divine right)
 *   and the republican reading (authority flows upward from popular consent
 *   alone) are separate constraints entirely, each with its own ε,
 *   beneficiaries, and victims. This story's ε is authored for the hybrid
 *   arrangement itself, as the hybrid reading's own lights see it:
 *   low-to-moderate, because splitting sovereignty removes the concentrated
 *   extraction risk of either pure form while introducing new, smaller costs
 *   from boundary ambiguity and the ongoing need for interpretive
 *   adjudication.
 *
 * KEY AGENTS:
 *   - hereditary_monarch: primary ceremonial beneficiary (institutional/identity_locked) — retains status and income, surrenders policy control
 *   - elected_officials: primary political beneficiary (institutional/constrained) — hold and administer real governing power
 *   - constitutional_courts: agenda_setter/beneficiary (institutional/analytical) — adjudicate and thereby perpetuate the boundary's ambiguity
 *   - absolutist_restorationists: victim (powerless/trapped) — denied the pure monarchical form they regard as legitimate
 *   - republican_abolitionists: victim (moderate/constrained) — forced to tolerate and fund an inherited office they regard as illegitimate
 *   - general_public: observer/incidental beneficiary (organized/constrained) — receives stability, treats the split as background
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.32).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.38).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Monarchy: Dual-Sourced Legitimacy Split Between Inherited Ceremony and Delegated Politics").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, 'af83924d-7693-4e09-8a5c-4ce35fe9e775').
narrative_ontology:cs_kernel_codification('af83924d-7693-4e09-8a5c-4ce35fe9e775', formalized).
narrative_ontology:cs_authority_grounding('af83924d-7693-4e09-8a5c-4ce35fe9e775', practice).
narrative_ontology:cs_interpretation_layer_present('af83924d-7693-4e09-8a5c-4ce35fe9e775').
narrative_ontology:cs_reading_relation('af83924d-7693-4e09-8a5c-4ce35fe9e775', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('af83924d-7693-4e09-8a5c-4ce35fe9e775', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_axiom('af83924d-7693-4e09-8a5c-4ce35fe9e775', foundational, sovereignty_is_divisible_by_function).
narrative_ontology:cs_axiom_status(sovereignty_is_divisible_by_function, holdable).
narrative_ontology:cs_axiom_grounding('af83924d-7693-4e09-8a5c-4ce35fe9e775', sovereignty_is_divisible_by_function, conventional).
narrative_ontology:cs_axiom('af83924d-7693-4e09-8a5c-4ce35fe9e775', foundational, constitutional_precedent_adjudicates_legitimacy_boundary).
narrative_ontology:cs_axiom_status(constitutional_precedent_adjudicates_legitimacy_boundary, holdable).
narrative_ontology:cs_axiom_grounding('af83924d-7693-4e09-8a5c-4ce35fe9e775', constitutional_precedent_adjudicates_legitimacy_boundary, conventional).
narrative_ontology:cs_axiom('af83924d-7693-4e09-8a5c-4ce35fe9e775', secondary, ceremonial_and_political_authority_require_distinct_sources).
narrative_ontology:cs_axiom_status(ceremonial_and_political_authority_require_distinct_sources, holdable).
narrative_ontology:cs_axiom_grounding('af83924d-7693-4e09-8a5c-4ce35fe9e775', ceremonial_and_political_authority_require_distinct_sources, instrumental).
narrative_ontology:cs_reference_frame('af83924d-7693-4e09-8a5c-4ce35fe9e775', post_settlement_dual_source_equilibrium).
narrative_ontology:cs_drift_state('af83924d-7693-4e09-8a5c-4ce35fe9e775', contemporary_constitutional_monarchy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af83924d-7693-4e09-8a5c-4ce35fe9e775', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_restorationists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_abolitionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial headship of state, personal status, palace income, and symbolic continuity with a founding lineage, in exchange for surrendering direct policy control. Cannot exit the arrangement without dissolving the very identity that grounds the role — the monarch's whole self-conception is constituted by the inherited office, even though the office's political teeth have been removed by the same bargain that preserves it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, civilizational, identity_locked, national).

% Hold actual governing power — legislation, budget, foreign policy — legitimated by periodic delegation from voters rather than by lineage. They administer the boundary day to day, deciding what counts as 'merely ceremonial' versus 'substantively political,' and benefit from a stable, uncontested claim to policy authority that a purely republican system would have to re-litigate from first principles every generation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter).

% Interpret and police the boundary between crown and elected government whenever it is disputed — reserve powers, royal assent conventions, succession law. Their institutional relevance and authority both derive from the boundary's persistent ambiguity: a cleanly resolved boundary (pure monarchy or pure republic) would make much of their interpretive function unnecessary.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, beneficiary).

% Hold that legitimate authority should flow undivided from the sovereign by inherited and (for some) divinely sanctioned right. The hybrid arrangement structurally denies them the substantive monarchical power they regard as rightful, reducing the crown to ceremony. They have no institutional lever to restore the pure form short of revolution or constitutional amendment they cannot command votes for.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_restorationists, payer,
    powerless, generational, trapped, national).

% Hold that legitimate authority should flow entirely upward from popular consent, with no inherited office at all. The hybrid arrangement forces them to fund, tolerate, and formally defer to a hereditary head of state whose office they regard as an unearned residue. They can organize, publish, and campaign for abolition, but face high mobilization costs against a popular or inertial status quo and a constitutional amendment threshold designed to be difficult to clear.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_abolitionists, payer,
    moderate, generational, constrained, national).

% Receive a stable, continuity-signaling head of state alongside accountable, removable government — a package that avoids both the volatility of contested absolute rule and the perceived rootlessness some associate with a purely elected symbolic apparatus. Most treat the split as background furniture rather than a live question, which itself sustains the arrangement.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, general_public, observer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, general_public, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates the symbolic/unifying function of the state (continuity, ceremony, national identity) from the contestable, revisable function of governing (policy, budgets, war and peace), so that political conflict does not have to relitigate the very existence and identity of the state each electoral cycle.
% TRANSFER_FUNCTION: Moves undivided sovereign authority — which a pure monarchical or pure republican settlement would concentrate in one source — into two channels: status, ceremonial primacy, and personal provision to the hereditary line; governing power and policy legitimacy to periodically elected officials. Constitutional courts receive ongoing interpretive authority over the boundary itself.
% ABSENT_VOICES: Absolutist restorationists and republican abolitionists are both structurally present as payers but functionally marginal to the boundary-drawing process — the actual line between ceremonial and political authority is negotiated between the crown's household, sitting governments, and courts, not between the two camps who would each prefer a pure settlement.
% DISAPPEARANCE_RATIONALE: If the constitutional split vanished overnight, the state would have to resolve, by force or immediate constitutional convention, whether the monarch resumes full sovereign power or is abolished outright — either path reorganizes head-of-state law, oaths of office, armed forces command structures, and the symbolic apparatus of the state (currency, honors, national ceremony) that currently rides on the inherited half of the split.
% FOUNDING_PROBLEM: Historical settlements that ended or averted civil war between absolutist monarchy and republican revolution by splitting sovereignty rather than letting one side win outright — the crown kept its head (literally, in some lineages) and its ceremonial role; parliaments and cabinets kept the power to govern.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative-government scholars outside both the royal household and sitting governments attest the founding problem (avoiding renewed civil conflict over the location of sovereignty) is largely resolved in mature constitutional monarchies — the risk of restorationist or revolutionary rupture is low. The monarchy itself and allied traditionalist commentators attest the arrangement remains necessary for national unity and continuity; republican-movement analysts attest the arrangement now functions mainly to protect an inherited institution's status rents rather than to prevent conflict.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.32 by interval end) because the hybrid split is genuinely load-bearing: it removes both the volatility risk of contested absolute monarchy and the higher transition costs a full republican abolition would impose, and neither surviving beneficiary group (monarch, elected officials) extracts at the scale a pure winner-take-all settlement would allow. Suppression starts moderate (0.5) reflecting the settlement-era need to actively foreclose restorationist and revolutionary alternatives, and gently declines over the interval (0.38) as the arrangement normalizes and fewer people treat either pure alternative as live. Theater ratio rises over the same interval (0.2 to 0.42) — as the monarchy's actual governing content shrinks toward zero, an increasing share of its persistence is ceremonial performance (state openings, honors, symbolic assent) rather than functional necessity, which is a hallmark to watch for possible drift toward a piton reading of the ceremonial half specifically, distinct from this tangled-rope reading of the whole split.
 *
 * PERSPECTIVAL GAP:
 *   The monarch's seat and the elected officials' seat both compute as beneficiaries, but structurally differently: the monarch benefits from a shrinking, increasingly symbolic entitlement it cannot renegotiate upward without breaking the settlement that legitimates it at all (identity_locked), while elected officials benefit from an expanding, actively administered entitlement they can and do renegotiate at each boundary dispute (constrained but agentic). Absolutist and republican payers experience the same structure as a wall in opposite directions — one wants more crown power, one wants none — which is the diagnostic signature of a genuine compromise settlement rather than a disguised extraction by either pure form.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: hereditary_monarch (ceremonial/status rents), elected_officials (policy authority), constitutional_courts (ongoing interpretive relevance) all collect something the pure alternatives would not give them in this configuration. Victim declarations: absolutist_restorationists and republican_abolitionists both bear the cost of NOT getting their preferred pure settlement — their 'victimhood' is the frustration of a rival legitimacy claim, not conventional material extraction, which is exactly the low-ε, high-ambiguity-cost signature the hybrid reading predicts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — averting civil war over the location of sovereignty — is largely dead in mature constitutional monarchies (corroborated by outside constitutional historians), yet the split persists. This is not automatically mandatrophy: elected_officials' half of the split still performs a live coordination function (routine, peaceful transfer of governing power), so classifying the whole arrangement as pure inertial residue would mislabel a working coordination mechanism as pure extraction. The ceremonial half is the plausible piton candidate within this tangled rope — a genealogically distinct sub-claim that would warrant its own decomposed story if the theater_ratio trend continues past this interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_dispute_resolution_authority,
    'When crown and elected government disagree about where ceremonial authority ends and political authority begins (e.g. reserve powers, dissolution of parliament, royal assent), is the constitutional court''s resolution a neutral application of settled law, or a discretionary political act dressed in interpretive language?',
    'Track court rulings on boundary disputes across multiple crises; assess whether reasoning is doctrinally consistent and predictable in advance, or whether outcomes track the sitting government''s preferences post hoc.',
    'If discretionary, constitutional_courts function closer to a third political beneficiary exercising undeclared political power rather than a neutral mediator, which would push the classification toward a more extractive, less clean tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_resolution_authority, empirical, 'Whether the boundary-adjudicating authority is neutral or a hidden political actor.').

omega_variable(
    ceremonial_extraction_magnitude,
    'Is the monarch''s retained status/income proportionate to a genuine ongoing coordination function (national symbolic unity, diplomatic soft power), or is it a legacy rent that has outlived any coordination justification?',
    'Compare public cost of maintaining the ceremonial apparatus against measurable soft-power/tourism/unity benefits claimed on its behalf, and against comparable republics that perform similar unifying functions without an inherited office.',
    'If the rent substantially exceeds any measurable coordination benefit, the ceremonial half of this tangled rope drifts toward a piton or snare classification independent of the still-functional elected-government half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_extraction_magnitude, empirical, 'Whether crown retention is proportionate coordination cost or legacy extraction.').

omega_variable(
    kernel_reading_selection_stability,
    'Is the hybrid reading a stable equilibrium in its own right, or an unstable compromise that inherently drifts toward one of the two pure readings (monarchical restoration or republican abolition) over long civilizational timescales?',
    'Comparative historical analysis of constitutional monarchies across centuries: do hybrid settlements persist indefinitely, or do they systematically resolve toward one pole under sufficient stress (revolution, war, republic referenda)?',
    'If inherently unstable, the low ambiguity-cost this story attributes to the hybrid form understates its true long-run cost, since periodic renegotiation or collapse toward a pure form is itself a hidden cost of maintaining the split.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_stability, conceptual, 'Whether the constitutional hybrid is a stable third form or a transitional state between the two pure readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 60, 0.29).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 100, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the sovereign_legitimacy kernel. sovereign_legitimacy__monarchical_reading authors undivided downward-flowing inherited/divine authority; sovereign_legitimacy__republican_reading authors undivided upward-flowing popular-consent authority; this story (constitutional_hybrid_reading) authors the split settlement between them, mediated by constitutional courts. Each reading has its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — this story's low-to-moderate ε and tangled_rope classification are not comparable to a hedge or average across the siblings; they describe only the hybrid arrangement as the hybrid reading's own lights see it. Because this reading's foundational axiom (sovereignty is functionally divisible) directly contradicts the undivided-sovereignty premise shared by both pure readings, the reading_relations to both siblings are declared as forecloses rather than coexists_with — a party fully committed to this hybrid framework cannot simultaneously hold either pure reading's foundational claim within the same commitment structure, even though all three readings persist as live positions across different parties in public discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
