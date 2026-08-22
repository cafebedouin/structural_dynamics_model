% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Monarchy Dual-Authority Settlement
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This story instantiates the constitutional-hybrid reading of the
 *   sovereign legitimacy kernel: authority is dual-sourced, with
 *   ceremonial/symbolic status flowing through hereditary inheritance and
 *   political power flowing through periodic democratic delegation, the
 *   boundary between the two continuously drawn and redrawn by constitutional
 *   courts and accumulated precedent. This is a distinct constraint from the
 *   monarchical reading (authority flows downward from the sovereign alone,
 *   grounded in divine sanction and bloodline) and the republican reading
 *   (authority flows upward from the people alone, grounded in popular
 *   sovereignty) — each of those readings has its own ε, its own
 *   beneficiary/victim structure, and its own story file. This reading's ε
 *   sits low-to-moderate specifically because the hybrid compromise dampens
 *   the extractive potential of either pure form: the monarch cannot extract
 *   political power, elected officials cannot extract permanent unaccountable
 *   status, but the compromise introduces its own residual cost in the form
 *   of boundary-dispute risk and interpretive ambiguity, which is why
 *   theater_ratio climbs over the interval as ceremonial function
 *   increasingly substitutes for a settled political rationale.
 *
 * KEY AGENTS:
 *   - hereditary_monarch: Primary ceremonial beneficiary (institutional/identity_locked) — retains status and income without discretionary political power
 *   - elected_officials: Primary political beneficiary (institutional/mobile) — hold actual policy authority, benefit from an apolitical head of state absorbing legitimacy-continuity function
 *   - absolutist_restorationists: Structural victim (powerless/trapped) — denied any institutional path to undivided sovereign authority
 *   - republican_abolitionists: Structural victim (moderate/constrained) — bear the funding cost of an office they cannot abolish without supermajority constitutional action
 *   - constitutional_courts: Boundary-adjudicating agenda-setter (institutional/analytical) — the mechanism that keeps the dual-source settlement from collapsing into either pure form
 *   - general_public: Diffuse beneficiary/payer (organized/constrained) — gains continuity stability, funds the ceremonial apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.34).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.41).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Monarchy Dual-Authority Settlement").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '7ea639e6-b161-4cff-b5e3-7e47449b76c5').
narrative_ontology:cs_kernel_codification('7ea639e6-b161-4cff-b5e3-7e47449b76c5', formalized).
narrative_ontology:cs_authority_grounding('7ea639e6-b161-4cff-b5e3-7e47449b76c5', lineage).
narrative_ontology:cs_interpretation_layer_present('7ea639e6-b161-4cff-b5e3-7e47449b76c5').
narrative_ontology:cs_reading_relation('7ea639e6-b161-4cff-b5e3-7e47449b76c5', sovereign_legitimacy__monarchical_reading, influences).
narrative_ontology:cs_reading_relation('7ea639e6-b161-4cff-b5e3-7e47449b76c5', sovereign_legitimacy__republican_reading, influences).
narrative_ontology:cs_axiom('7ea639e6-b161-4cff-b5e3-7e47449b76c5', foundational, legitimacy_is_functionally_divisible).
narrative_ontology:cs_axiom_status(legitimacy_is_functionally_divisible, holdable).
narrative_ontology:cs_axiom_grounding('7ea639e6-b161-4cff-b5e3-7e47449b76c5', legitimacy_is_functionally_divisible, conventional).
narrative_ontology:cs_axiom('7ea639e6-b161-4cff-b5e3-7e47449b76c5', foundational, constitutional_interpretation_settles_boundary_disputes).
narrative_ontology:cs_axiom_status(constitutional_interpretation_settles_boundary_disputes, holdable).
narrative_ontology:cs_axiom_grounding('7ea639e6-b161-4cff-b5e3-7e47449b76c5', constitutional_interpretation_settles_boundary_disputes, conventional).
narrative_ontology:cs_axiom('7ea639e6-b161-4cff-b5e3-7e47449b76c5', secondary, symbolic_continuity_requires_hereditary_insulation).
narrative_ontology:cs_axiom_status(symbolic_continuity_requires_hereditary_insulation, holdable).
narrative_ontology:cs_axiom_grounding('7ea639e6-b161-4cff-b5e3-7e47449b76c5', symbolic_continuity_requires_hereditary_insulation, instrumental).
narrative_ontology:cs_reference_frame('7ea639e6-b161-4cff-b5e3-7e47449b76c5', post_settlement_dual_authority_equilibrium).
narrative_ontology:cs_drift_state('7ea639e6-b161-4cff-b5e3-7e47449b76c5', contemporary_republican_resurgence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ea639e6-b161-4cff-b5e3-7e47449b76c5', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_restorationists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_abolitionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, general_public).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, general_public).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, separation_of_dignified_and_efficient_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial headship, state income, and symbolic authority (opening parliament, head-of-state functions, honors system) by hereditary succession. Has surrendered discretionary political power in exchange for constitutional guarantee of status continuity. Cannot exit the role without abdication, which dissolves the identity the institution is built around; the monarchy's entire claim to legitimacy rests on continuity of bloodline, so acting politically would void the bargain that protects it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, civilizational, identity_locked, national).

% Hold delegated political authority through periodic election, exercising actual policy-making power (legislation, executive administration, budget) while the monarch retains only symbolic function. Benefit from a stable, apolitical head of state that absorbs ceremonial obligations and can arbitrate constitutional crises as neutral referee, while officials retain policy control without needing to also perform sovereignty theater. Can lose office at the next election but the office itself persists.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter).

% Hold that legitimate authority should flow undivided from the sovereign by divine or traditional right, with elected bodies at most advisory. The hybrid settlement structurally denies this position any institutional foothold — the monarch is legally barred from exercising the political authority this faction believes properly belongs to the crown. Their preferred arrangement has no constitutional path back short of a constitutional crisis or coup; they bear the cost of a foreclosed political future with no legal remedy.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_restorationists, payer,
    powerless, generational, trapped, national).

% Hold that all legitimate authority should derive from popular consent alone, with no inherited office retaining any public function or funding. They pay the ongoing cost of a hereditary institution they did not consent to and cannot vote out, funded through public revenue, while their preferred abolition requires a referendum or supermajority constitutional amendment that the settlement's own entrenchment provisions make difficult to achieve. Can organize and campaign but cannot exit the arrangement individually.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_abolitionists, payer,
    moderate, generational, constrained, national).

% Adjudicate disputes at the boundary between ceremonial and political authority — for example whether a monarch's reserve powers (dissolving parliament, withholding royal assent) may be exercised, and under what circumstances. Their rulings and accumulated precedent constitute the actual mechanism that keeps the dual-source settlement stable; a boundary dispute they cannot resolve cleanly is the settlement's principal failure mode.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, observer).

% Receive a stable constitutional order with continuity symbolism separated from partisan politics, and can change government without a legitimacy crisis since the head of state is untouched by electoral turnover. Also fund the ceremonial apparatus through taxation and have no direct vote over its existence, since it sits outside the ordinary electoral cycle. Broadly consents to the arrangement through inaction rather than affirmative endorsement.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates symbolic continuity (which benefits from being stable, unelected, and above partisan contest) from operational political power (which benefits from being accountable, contestable, and periodically renewed), so that neither function contaminates the other — governments can change without a legitimacy vacuum, and the ceremonial office can persist without accumulating unaccountable political power.
% TRANSFER_FUNCTION: Moves practical policy-making authority from the hereditary line to elected officials while moving public funds and formal state dignity to the hereditary office; moves the cost of maintaining both apparatuses onto general taxpayers, and moves institutional standing away from both absolutist and republican pure-form advocates, who are structurally denied a route to their preferred settlement.
% ABSENT_VOICES: Absolutist restorationists and republican abolitionists are both structurally present as payers but functionally excluded from the settlement's ongoing operation — neither has an institutional seat at the boundary-drawing table (that seat belongs to the constitutional courts and precedent-setting political practice), so their objections surface only as periodic political movements rather than as parties to the constitutional interpretation process itself.
% DISAPPEARANCE_RATIONALE: If the dual-source settlement dissolved overnight, the state would have to resolve immediately toward either a republic (abolishing the hereditary office and its funding, transferring ceremonial function to an elected or appointed head of state) or an absolutist restoration (returning discretionary political power to the monarch) — the entire apparatus of reserve powers, royal assent conventions, and constitutional-court boundary jurisprudence would become either moot or newly contested, and the courts would lose their central adjudicative function.
% FOUNDING_PROBLEM: Prevent the volatility of contested succession disputes and the illegitimacy risk of purely delegated authority (which can be revoked or denied) by giving continuity-sensitive symbolic functions to an inherited office insulated from electoral turnover, while giving contestable, policy-making authority to periodically renewed democratic mandate — solving both the 'who embodies the state when governments change' problem and the 'who is accountable for policy' problem simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative-government scholars outside both the monarchy's household and the elected government attest that the succession-volatility problem the settlement was built to solve has been substantially reduced in practice (peaceful transfers of political power now occur routinely even in pure republics), while defenders inside the arrangement attest the symbolic-continuity function remains independently valuable regardless of whether the original succession-crisis rationale still applies. No fully external referee institution certifies which reading is correct; the constitutional courts that would adjudicate this question are themselves a beneficiary of the arrangement's continued existence.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.34, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-to-moderate (0.34 at interval end) because the settlement is genuinely a compromise structure: neither the monarch nor elected officials can extract the full rent available to a pure-form arrangement (absolute monarchy's unconstrained extraction, or a republic's unchecked majoritarian power), and the coordination function — separating continuity from accountability — is real and load-bearing. Suppression is moderate and essentially flat (0.38 to 0.41) because the settlement does not require escalating coercive enforcement against the excluded factions; it requires only the ordinary constitutional machinery (courts, precedent, convention) to keep functioning, and that machinery's enforcement need has not meaningfully intensified. Theater ratio rises substantially (0.30 to 0.52) because as the founding succession-crisis rationale weakens (per the founding_problem status being contested), an increasing share of the ceremonial function's justification shifts from solving a live coordination problem to performing continuity for its own sake — this is the Goodhart-drift signal the temporal series is designed to surface, and it is the primary internal tension this reading carries.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch and elected officials are both structural beneficiaries but through different mechanisms: the monarch benefits from guaranteed status/income insulated from political risk (d near the beneficiary end, though identity-locked exit tempers how 'free' this benefit really is — the monarch cannot simply walk away and keep the benefits, since the benefit IS the identity). Elected officials benefit from offloading continuity/legitimacy-symbol duties onto an institution they do not have to run themselves, at only the ceremonial funding cost, while retaining full policy control (d near the beneficiary end, with full mobility since electoral loss does not trap them personally). Absolutist restorationists and republican abolitionists are both structural targets of the compromise: the settlement's entire logic is built on denying each faction its preferred pure form, so both carry high d despite occupying opposite ends of the ideological spectrum — this is the signature of a genuine hybrid, not a disguised victory for either pure position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than a clean rope) is load-bearing: this settlement genuinely coordinates a real problem (separating continuity-function from accountability-function so governments can change without a legitimacy vacuum) AND asymmetrically extracts from two identifiable victim classes (both flavors of purist who are constitutionally locked out of their preferred arrangement). Treating this as a pure rope would erase the real cost borne by absolutists and republicans who never consented to the compromise and have no clean exit; treating it as a pure snare would erase the genuine coordination function that peaceful-transition stability provides to the general public. The rising theater_ratio is the mandatrophy signal to watch: if the founding succession-crisis problem is now substantially dead (as external constitutional historians attest) while the arrangement persists on inertia and accumulated ceremonial investment, the settlement is drifting from coordination toward inertial performance — a piton-in-waiting rather than a stable tangled rope, though it has not yet crossed that threshold since elected officials still derive an active operational benefit (offloaded continuity duties) rather than merely tolerating vestigial tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_dispute_resolution_stability,
    'Can constitutional courts reliably resolve monarch/executive boundary disputes (reserve powers, royal assent, dissolution authority) without triggering a legitimacy crisis that collapses the hybrid into one pure form or the other?',
    'Track historical frequency and resolution outcomes of boundary disputes across comparable constitutional monarchies; a pattern of courts consistently defusing disputes via precedent supports stability, while a pattern of disputes resolving only through political crisis or constitutional amendment indicates chronic vulnerability.',
    'If boundary disputes are reliably resolved through ordinary adjudication, the tangled_rope classification holds with low volatility. If disputes systematically escalate past the courts'' capacity, the settlement is closer to a scaffold awaiting a forcing crisis, since its actual stability mechanism would be shown to be inadequate to its stated function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_resolution_stability, empirical, 'Whether constitutional adjudication can durably hold the dual-authority boundary.').

omega_variable(
    founding_problem_obsolescence,
    'Has the original succession-volatility rationale for separating symbolic and political authority actually become obsolete now that peaceful democratic transfers of power are routine even without a hereditary continuity office (as most republics demonstrate)?',
    'Comparative analysis of constitutional crisis frequency in matched republics versus constitutional monarchies over the same historical period, controlling for other institutional variables.',
    'If republics achieve comparable transition stability without a hereditary office, the founding problem this reading was built to solve is substantially dead, and the settlement''s persistence is better explained by inertia and accumulated symbolic investment than by ongoing functional necessity — strengthening the theater_ratio trend as the primary diagnostic rather than a secondary signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the succession-crisis problem the hybrid was built to solve still requires this solution.').

omega_variable(
    kernel_reading_selection,
    'This story treats ''sovereign legitimacy'' as decomposing into three structurally distinct readings (constitutional hybrid, monarchical, republican) rather than one constraint measured three ways — is this decomposition itself contestable, or could a fourth reading (e.g. a purely ceremonial-figurehead reading with zero residual monarchical discretion) exist as a distinct constraint rather than a boundary case of this one?',
    'Check whether a ''ceremonial-only, zero-discretion'' variant produces a materially different ε and victim set than this hybrid reading — if the reserve-powers question is dispositive (i.e., whether the monarch retains ANY discretionary reserve power changes the beneficiary structure materially), it is a fourth sibling reading, not a variant of this one.',
    'If a fourth reading is warranted, this story''s ε and stakeholder set should be understood as specifically the ''live reserve powers'' variant of the hybrid, not the hybrid reading in general — narrowing its claim and requiring a new network edge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether the constitutional-hybrid reading itself further decomposes by degree of retained monarchical discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sove_tr_t8, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(sove_tr_t16, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(sove_tr_t24, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(sove_tr_t32, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sove_be_t8, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(sove_be_t16, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(sove_be_t24, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(sove_be_t32, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 32, 0.33).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(sove_su_t8, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(sove_su_t16, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(sove_su_t24, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(sove_su_t32, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the sovereign_legitimacy kernel, decomposed per the ε-invariance principle: monarchical_reading (authority flows downward from inherited/divine right, ε high — extraction concentrated on the entire non-sovereign population), republican_reading (authority flows upward from popular consent, ε low but with its own suppression profile against minority/traditionalist claims), and this constitutional_hybrid_reading (dual-sourced, ε low-to-moderate, extraction distributed onto both pure-form factions rather than concentrated on a single class). All three describe the SAME contested kernel — what makes authority legitimate — through structurally incompatible premises about where legitimacy originates. They are linked bidirectionally: adoption of this hybrid reading structurally forecloses full monarchical restoration and full republican abolition within the same constitutional order, which is why cs_structure.reading_relations declares an influences relationship to both siblings rather than coexists_with — the hybrid's entrenchment provisions actively raise the cost of either pure-form transition, not merely disagree with them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
