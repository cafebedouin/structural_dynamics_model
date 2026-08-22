% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Coercion Capitulation Settlement (Exogenous Override Reading of the 1890 Manifesto)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   A territorially concentrated religious polity (the Latter-day Saint
 *   church in Utah) practiced plural marriage as a divinely mandated covenant
 *   for two generations before a federal enforcement campaign (criminal
 *   statutes, marshal-led prosecutions, corporate dissolution, property
 *   confiscation, disfranchisement, and a conditional statehood offer) made
 *   continuation untenable. Official Declaration 1 (1890) suspended new
 *   plural marriages; the institution taught thereafter that the doctrine
 *   stood while the practice waited. This story instantiates ONE reading of
 *   that settlement: the exogenous_override_reading, under which the
 *   Manifesto's operative cause is federal coercion, the theological doctrine
 *   is unchanged, and only practice is suspended under duress. Per the
 *   committer-frame rules, the sibling readings (endogenous reinterpretation;
 *   hybrid pragmatic adaptation) are separate constraints in separate files;
 *   nothing about them is averaged into this one. The epsilon referent is the
 *   standing arrangement under contest, the coercion-and-compliance
 *   settlement of roughly 1880-1910, assessed by this reading's own lights:
 *   the federal government collects capitulation, the membership bears
 *   doctrinal-abandonment costs, and the gap between material cause and
 *   spiritual framing is the legitimacy crisis this reading predicts. KEY
 *   AGENTS (by structural relationship): federal_government: Agenda-setter
 *   (institutional/arbitrage) - legislates, prosecutes, confiscates, and
 *   conditions statehood; collects the capitulation. lds_first_presidency:
 *   Captive administrator (institutional/constrained) - issues and enforces
 *   the suspension internally while bearing its legitimacy costs;
 *   dual-positioned. lds_membership: Primary target
 *   (moderate/identity_locked) - bears the doctrinal-abandonment cost across
 *   generations. post_manifesto_plural_marriage_believers: Sharpest target
 *   (powerless/trapped) - disciplined for continuing the suspended covenant
 *   after 1904. american_monogamous_norm_public: Diffuse beneficiary
 *   (organized/mobile) - collects national uniformity without enforcement
 *   burden. utah_statehood_bloc: Incidental beneficiary
 *   (moderate/constrained) - collects statehood upon compliance.
 *   historical_analysts: Analytical observer (analytical/analytical) -
 *   adjudicates the causal attribution from archives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.76).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.55).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Coercion Capitulation Settlement (Exogenous Override Reading of the 1890 Manifesto)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '0e308b01-12a1-4470-af4e-5fc23d15ce13').
narrative_ontology:cs_kernel_codification('0e308b01-12a1-4470-af4e-5fc23d15ce13', fixed_text).
narrative_ontology:cs_authority_grounding('0e308b01-12a1-4470-af4e-5fc23d15ce13', lineage).
narrative_ontology:cs_interpretation_layer_present('0e308b01-12a1-4470-af4e-5fc23d15ce13').
narrative_ontology:cs_reading_relation('0e308b01-12a1-4470-af4e-5fc23d15ce13', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('0e308b01-12a1-4470-af4e-5fc23d15ce13', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('0e308b01-12a1-4470-af4e-5fc23d15ce13', foundational, manifesto_operative_cause_is_federal_coercion).
narrative_ontology:cs_axiom_status(manifesto_operative_cause_is_federal_coercion, holdable).
narrative_ontology:cs_axiom_grounding('0e308b01-12a1-4470-af4e-5fc23d15ce13', manifesto_operative_cause_is_federal_coercion, empirically_contingent).
narrative_ontology:cs_axiom('0e308b01-12a1-4470-af4e-5fc23d15ce13', foundational, plural_marriage_doctrine_binding_not_revoked).
narrative_ontology:cs_axiom_status(plural_marriage_doctrine_binding_not_revoked, holdable).
narrative_ontology:cs_axiom_grounding('0e308b01-12a1-4470-af4e-5fc23d15ce13', plural_marriage_doctrine_binding_not_revoked, theological).
narrative_ontology:cs_axiom('0e308b01-12a1-4470-af4e-5fc23d15ce13', secondary, suspension_temporary_not_abrogation).
narrative_ontology:cs_axiom_status(suspension_temporary_not_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('0e308b01-12a1-4470-af4e-5fc23d15ce13', suspension_temporary_not_abrogation, conventional).
narrative_ontology:cs_reference_frame('0e308b01-12a1-4470-af4e-5fc23d15ce13', unchanged_doctrine_suspended_practice).
narrative_ontology:cs_drift_state('0e308b01-12a1-4470-af4e-5fc23d15ce13', contemporary_post_second_manifesto, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e308b01-12a1-4470-af4e-5fc23d15ce13', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, american_monogamous_norm_public).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, utah_statehood_bloc).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_plural_marriage_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_first_presidency).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, belief_action_distinction_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, national_domestic_relations_uniformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and the federal courts criminalized plural marriage (Edmunds Act 1882; Edmunds-Tucker Act 1887), dissolved the Church's corporate charter, confiscated its temporal property including temple funds, disfranchised polygamists, and made Utah statehood conditional on cessation. Collects capitulation: the practice ends, national marriage law applies uniformly, and the sovereignty claim is settled without conceding anything doctrinal.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Issued Official Declaration 1 suspending new plural marriages, then administered compliance inside the institution: instructing officers, disciplining members who continued, and privately permitting some sealings in Mexico and Canada until the 1904 Second Manifesto closed the gap. Bears the cost of presenting a political necessity in spiritual language while continuing to teach that the doctrine itself stands.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_first_presidency, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_first_presidency, payer).

% Raised for two generations on the teaching that plural marriage is required for the highest degree of exaltation, then instructed that the practice is suspended while the doctrine remains. Reconciles material capitulation with spiritual framing inside congregations, families, and sealing lineages; leaving the institution means forfeiting salvation claims, community standing, and family sealings.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    moderate, generational, identity_locked, continental).

% The minority who treated the covenant as eternally binding and continued or sought new sealings after 1890, some in the Mexican and Canadian colonies under private authorization. After 1904 they face church discipline (two apostles resigned or were removed over new-marriage questions), and their descendants seed schismatic communities; they carry both the legal risk of the earlier decades and the institutional abandonment afterward.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_plural_marriage_believers, payer,
    powerless, biographical, trapped, continental).

% Nineteenth-century reform movements and the national press treated monogamous marriage as a pillar of republican citizenship; the settlement confirms the norm's reach across the territories. Most of this public bears no enforcement burden and pays nothing directly; the benefit arrives as settled law and national uniformity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, american_monogamous_norm_public, beneficiary,
    organized, civilizational, mobile, national).

% Territorial leadership and non-Mormon Utahns gain statehood in 1896 once the practice ceases, trading the covenant practice for full constitutional integration: voting representation, local control of offices, and an end to federal territorial administration. They collect the constitutional benefits and bear none of the doctrinal cost.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, utah_statehood_bloc, beneficiary,
    moderate, biographical, constrained, regional).

% Historians of American religion reconstruct the causal chain from diaries, correspondence, court records, and hearing transcripts, and adjudicate between rival accounts of what authored the 1890 reversal. They collect understanding rather than rents and hold no enforcement or compliance position.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled the collision between a territorially concentrated religious polity running its own marriage regime and a national legal order asserting uniform sovereignty over domestic relations: one rule of family formation, administered by ordinary courts, replaced dual jurisdiction over marriage in the territory.
% TRANSFER_FUNCTION: Moved institutional assets and autonomy upward from the Church to federal authority (corporate charter, temporal property, voter eligibility, and the practice of plural marriage itself) while moving statehood, amnesty, and legal toleration downward to the territory and its members.
% ABSENT_VOICES: Plural wives whose legal status the settlement rendered permanently precarious were not consulted; rank-and-file members learned of the suspension after the decision was made; colonists who had emigrated to Mexico and Canada expressly to keep the covenant had no seat in the 1889-1890 deliberations. Each would have objected from a seat the negotiation never opened.
% DISAPPEARANCE_RATIONALE: Without the coercion arrangement (no Edmunds-Tucker machinery, no statehood lever, no confiscation) the institution had both the doctrine and the demonstrated willingness to continue the practice; the Utah settlement, the timing of statehood, and the shape of national marriage law all reorganize around a polity that kept its covenant regime.
% FOUNDING_PROBLEM: Federal sovereignty confronted a religious polity claiming divine mandate for a practice federal law criminalized; Congress sought to bring the territory under uniform national law without triggering open religious war.
% FOUNDING_PROBLEM_CORROBORATION: Supreme Court records (Reynolds v. United States, 1879) and congressional floor debates attest the federal formulation from outside the Church; academic historiography of American religious history, written by scholars holding no seat in either the federal apparatus or the institution, corroborates both the original collision and its transformation after 1890. The institution's own account is a party account and corroborates nothing beyond itself.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.76 at interval end) because what the arrangement takes is not marginal: corporate charter, temporal property, franchise eligibility, and finally the covenant practice itself, taken from a polity that had staked its theology on it. Suppression (0.55 scalar) traces the enforcement arc on the shared grid: external machinery builds hard to 1890 (marshals, prisons, confiscation), then internalizes after capitulation as church discipline against continuers, relaxing slowly once compliance becomes self-enforcing. Theater_ratio (0.45) tracks the hypocrisy window this reading predicts: low while defiance was open, spiking across 1890-1904 when officials denied new marriages while privately authorizing sealings abroad, declining after the 1904 Second Manifesto made the denial approximately true. Accessibility_collapse (0.65): alternatives partially collapsed - colonial exit to Mexico and Canada was real and used, then foreclosed by the Second Manifesto and regional upheaval; doctrinal exit remained available only at the price of salvation claims. Resistance (0.60): secret post-Manifesto marriages, apostolic resignations over new-marriage questions, and the later fundamentalist schisms are the measurable residue of refusal. Claim and metrics are independent: this reading claims snare because the uniformity story functions as cover, persistence depended on coercion, exits were suppressed, and victims are identifiable; the metrics report the arrangement's observed operation without being tuned to that claim. All three tracked series share one time grid (T0=1880, T7=Edmunds-Tucker, T10=Manifesto, T24=Second Manifesto, T30=post-Smoot settlement). Receipt surface: the gains demonstrably accrue to the federal seat (hence named gain_flow); for the agenda-setting seat, dismantling the arrangement after 1890 meant conceding the sovereignty claim after a quarter-century of enforcement investment, so fixing is prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same document. From the federal seat the arrangement is lawful enforcement of democratically enacted domestic-relations law: low cost, collected compliance. From the Presidency's seat it is captivity with residual administrative agency: compelled to suspend a covenant it teaches is eternal, administering a suspension it did not originate. From the membership's seat it is covenant rupture presented in spiritual language; from the believing minority's seat, abandonment compounded by discipline. The analytical seat sees all four at once. The engine derives this divergence from power, exit, and role data; the divergence is the finding, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the federal government (arbitrage exit, agenda-setting power) sits nearest the beneficiary pole; the monogamous-norm public and the statehood bloc collect diffuse incidental gains with mobile or constrained exit. Victim declarations drive high d: the membership's identity_locked exit amplifies its target position (exit would forfeit salvation claims, so the constraint binds at identity depth, not mere convenience); the post-1904 believers sit nearest the full-target pole with trapped exit. One override is authored: the derivation would read the First Presidency's agenda_setter role as beneficiary-side (roughly d 0.15, an administrator collecting positional rents). Under this reading the Presidency is a captive administrator - it runs the suspension it did not originate and pays the legitimacy cost - so the override moves the institutional seat to d 0.40. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against several mislabels. Against rope: the national-uniformity story is, on this reading, the cover rather than the function - participants on the paying side did not consent and could not exit, so the arrangement fails the net-beneficiary test. Against scaffold: no sunset was ever declared; the 'temporary' suspension hardened into permanence without a transition clause, which is exactly the drift the reference_frame/drift_state block records. Against piton: the function did not atrophy into performance - it succeeded, and the arrangement dissolved into ordinary law; the theater concentration in 1890-1904 is a hypocrisy window, not vestigial maintenance. The R5 interview locates the mandatrophy risk precisely: the founding problem's specific instance (plural marriage in Utah) is dead while the arrangement's framing persists, so the status x verdict pair is authored contested/world_rearranges rather than dead/world_rearranges, flagging zombie-framing risk without collapsing the classification. Finally, epsilon is reading-indexed over a fixed referent: were the endogenous sibling true, the same events would author as transitional support for the institution's survival and the victim set would dissolve; the classification is a property of this reading, not of the topic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_causal_attribution_kernel_reading,
    'This constraint is one reading of the marriage_commitment_legitimacy kernel: it holds the Manifesto''s operative cause to be federal coercion with no independent revelatory content. Would adopting the endogenous_reinterpretation_reading (genuine revelation commanded the reversal) or the hybrid_pragmatic_reading (strategic deployment of prophetic authority under exogenous pressure) change the constraint''s beneficiary/victim structure?',
    'Archival adjudication - Woodruff''s private diaries and contemporaneous First Presidency correspondence weighed against the public framing - together with cross-reading comparison of the compiled stories for this kernel.',
    'Under the endogenous reading the federal government loses beneficiary position, the victim set dissolves into voluntary covenant transition, and epsilon falls toward coordination-cost levels; under the hybrid reading a partial victim set survives with reduced epsilon. The disagreement is located in the causal attribution of a single document''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_causal_attribution_kernel_reading, conceptual, 'Reading-indexicality of the Manifesto''s causal attribution within the marriage_commitment_legitimacy kernel.').

omega_variable(
    manifesto_scope_ambiguity,
    'What did compliance actually require - cessation of new plural marriages within United States jurisdiction only, or cessation everywhere including the Mexican and Canadian colonies?',
    'The documented record of post-1890 sealings performed with presidential knowledge or authorization, and the 1904 Second Manifesto''s explicit worldwide prohibition.',
    'If the narrow reading governed 1890-1904, members disciplined after 1904 were punished for conduct the original text did not clearly forbid, raising the suppression borne by the believer seat; if the broad reading governed, the theater spike shrinks and the hypocrisy-window reading weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manifesto_scope_ambiguity, empirical, 'Scope of the suspension demanded by the 1890 text versus the 1904 clarification.').

omega_variable(
    victim_set_concentration,
    'Is the doctrinal-abandonment cost borne diffusely by the whole membership or concentrated in the minority who held the covenant eternally binding?',
    'Membership-level evidence: fundamentalist schism sizes, disciplinary case distributions, Smoot hearing testimony, and subsequent survey data on belief in the covenant''s continuing validity.',
    'Concentrated victims sharpen the directionality asymmetry and raise effective extraction at the believer seat; diffuse costs flatten the profile toward the membership average and soften the snare signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_concentration, empirical, 'Distribution of the doctrinal-abandonment cost across the membership.').

omega_variable(
    federal_objective_ambiguity,
    'Was the federal objective confined to suppressing plural marriage as such, or did it extend to dismantling Mormon political-economic power (corporate dissolution, church-property seizure, statehood leverage)?',
    'Congressional debate records and the Edmunds-Tucker provisions compared against enforcement practice after 1890: if coercion instruments persisted against a compliant church, the broader objective is indicated.',
    'The broader reading widens the beneficiary seat''s capture beyond marriage policy and raises epsilon; the narrow reading confines the arrangement to the marriage question and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_objective_ambiguity, empirical, 'Whether the enforcement campaign targeted the practice or the polity behind it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mclegexo_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(mclegexo_tr_t0, observed).
narrative_ontology:measurement(mclegexo_tr_t4, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(mclegexo_tr_t4, observed).
narrative_ontology:measurement(mclegexo_tr_t7, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement_basis(mclegexo_tr_t7, observed).
narrative_ontology:measurement(mclegexo_tr_t10, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(mclegexo_tr_t10, observed).
narrative_ontology:measurement(mclegexo_tr_t14, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 14, 0.63).
narrative_ontology:measurement_basis(mclegexo_tr_t14, observed).
narrative_ontology:measurement(mclegexo_tr_t18, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 18, 0.67).
narrative_ontology:measurement_basis(mclegexo_tr_t18, observed).
narrative_ontology:measurement(mclegexo_tr_t24, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 24, 0.51).
narrative_ontology:measurement_basis(mclegexo_tr_t24, observed).
narrative_ontology:measurement(mclegexo_tr_t30, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(mclegexo_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(mclegexo_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(mclegexo_be_t0, observed).
narrative_ontology:measurement(mclegexo_be_t4, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 4, 0.66).
narrative_ontology:measurement_basis(mclegexo_be_t4, observed).
narrative_ontology:measurement(mclegexo_be_t7, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 7, 0.73).
narrative_ontology:measurement_basis(mclegexo_be_t7, observed).
narrative_ontology:measurement(mclegexo_be_t10, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement_basis(mclegexo_be_t10, observed).
narrative_ontology:measurement(mclegexo_be_t14, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 14, 0.79).
narrative_ontology:measurement_basis(mclegexo_be_t14, observed).
narrative_ontology:measurement(mclegexo_be_t18, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement_basis(mclegexo_be_t18, observed).
narrative_ontology:measurement(mclegexo_be_t24, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(mclegexo_be_t24, observed).
narrative_ontology:measurement(mclegexo_be_t30, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement_basis(mclegexo_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(mclegexo_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(mclegexo_su_t0, observed).
narrative_ontology:measurement(mclegexo_su_t4, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 4, 0.67).
narrative_ontology:measurement_basis(mclegexo_su_t4, observed).
narrative_ontology:measurement(mclegexo_su_t7, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 7, 0.83).
narrative_ontology:measurement_basis(mclegexo_su_t7, observed).
narrative_ontology:measurement(mclegexo_su_t10, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 10, 0.86).
narrative_ontology:measurement_basis(mclegexo_su_t10, observed).
narrative_ontology:measurement(mclegexo_su_t14, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 14, 0.71).
narrative_ontology:measurement_basis(mclegexo_su_t14, observed).
narrative_ontology:measurement(mclegexo_su_t18, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 18, 0.61).
narrative_ontology:measurement_basis(mclegexo_su_t18, observed).
narrative_ontology:measurement(mclegexo_su_t24, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(mclegexo_su_t24, observed).
narrative_ontology:measurement(mclegexo_su_t30, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(mclegexo_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Manifesto' covers three structurally distinct constraints, one per reading of the marriage_commitment_legitimacy kernel. Each reading authors its own epsilon over the same referent - the standing arrangement of roughly 1880-1910 - and the readings diverge on beneficiary/victim structure: this (exogenous) reading declares the federal government beneficiary and LDS believers victims; the endogenous reading dissolves the victim set; the hybrid reading splits it. Family links run through affects_constraints. Upstream/downstream: the endogenous reading functions as the institutional legitimation that the exogenous reading reads as imposed cover, so the endogenous story is routinely cited as evidence within this one's contest even though the two are separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
