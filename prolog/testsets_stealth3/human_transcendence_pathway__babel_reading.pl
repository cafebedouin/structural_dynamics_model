% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__babel_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__babel_reading, []).

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
 *   constraint_id: human_transcendence_pathway__babel_reading
 *   human_readable: Babel Unification Regime: Enforced Linguistic-Technological Uniformity Under Concentrated Command
 *   domain: political-theological/technological
 *
 * SUMMARY:
 *   A concentrated command center summons a whole population into a single
 *   project — a city and a tower with its top in the heavens — under one
 *   imposed language and one standardized technique. The arrangement's
 *   declared warrant is collective self-sufficiency: unity strong enough to
 *   secure stability and permanence without appeal to any authority beyond
 *   the project. Its actual operation couples a genuine coordination
 *   instrument (the common tongue genuinely mobilizes the workforce) to a
 *   steeply asymmetric transfer: the architects accumulate a name, standing,
 *   and command capacity; the builders pay in conscripted labor; the
 *   linguistic minorities pay in the public erasure of their mother tongues.
 *   The arrangement holds only while the power holds — when the center fails,
 *   communication fails with it, and the population scatters into the very
 *   diversity the project was built to suppress. The claim/metric gap is
 *   deliberate: the arrangement is CLAIMED by its architects as pure
 *   coordination (the common-tongue rope story) while the authored metrics
 *   describe coercive homogenization — the engine measures that divergence;
 *   the claim is not reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - tower_architects: primary beneficiary and agenda-setter (institutional/arbitrage) — concentrates the arrangement's gains and administers its enforcement
 *   - conscripted_builders: primary target (powerless/trapped) — bears labor conscription; holds a genuine secondary beneficiary position in the common tongue
 *   - linguistic_minorities: primary target (powerless/identity_locked) — bears linguistic and cultural erasure; exit would be identity dissolution
 *   - project_overseers: secondary beneficiary (organized/constrained) — collects standing and provision without setting direction
 *   - peripheral_clans: excluded seat (powerless/constrained) — not yet absorbed, unrepresented in the plan that presupposes them
 *   - post_collapse_chroniclers: analytical observer (analytical/analytical) — transmits the full arc from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, 0.78).
domain_priors:suppression_score(human_transcendence_pathway__babel_reading, 0.76).
domain_priors:theater_ratio(human_transcendence_pathway__babel_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__babel_reading, snare).
narrative_ontology:human_readable(human_transcendence_pathway__babel_reading, "Babel Unification Regime: Enforced Linguistic-Technological Uniformity Under Concentrated Command").
narrative_ontology:topic_domain(human_transcendence_pathway__babel_reading, "political-theological/technological").

domain_priors:requires_active_enforcement(human_transcendence_pathway__babel_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__babel_reading, 'd6925b27-01c7-4abd-ba50-57d32f19d98c').
narrative_ontology:cs_kernel_codification('d6925b27-01c7-4abd-ba50-57d32f19d98c', implicit).
narrative_ontology:cs_authority_grounding('d6925b27-01c7-4abd-ba50-57d32f19d98c', extraction).
narrative_ontology:cs_reading_relation('d6925b27-01c7-4abd-ba50-57d32f19d98c', human_transcendence_pathway__jerusalem_reading, forecloses).
narrative_ontology:cs_reading_relation('d6925b27-01c7-4abd-ba50-57d32f19d98c', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_axiom('d6925b27-01c7-4abd-ba50-57d32f19d98c', foundational, unity_through_enforced_uniformity).
narrative_ontology:cs_axiom_status(unity_through_enforced_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('d6925b27-01c7-4abd-ba50-57d32f19d98c', unity_through_enforced_uniformity, empirically_contingent).
narrative_ontology:cs_axiom('d6925b27-01c7-4abd-ba50-57d32f19d98c', foundational, self_sufficiency_without_transcendent_reference).
narrative_ontology:cs_axiom_status(self_sufficiency_without_transcendent_reference, holdable).
narrative_ontology:cs_axiom_grounding('d6925b27-01c7-4abd-ba50-57d32f19d98c', self_sufficiency_without_transcendent_reference, instrumental).
narrative_ontology:cs_reference_frame('d6925b27-01c7-4abd-ba50-57d32f19d98c', unified_command_concentration).
narrative_ontology:cs_drift_state('d6925b27-01c7-4abd-ba50-57d32f19d98c', post_scattering_account, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d6925b27-01c7-4abd-ba50-57d32f19d98c', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__babel_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, project_overseers).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, conscripted_builders).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, linguistic_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, conscripted_builders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Summon the whole population into a single project — a city and a tower with its top in the heavens — and administer the unified language and labor system that builds it. They set the direction, define what counts as acceptable speech and acceptable deviation, and decide what the project is for. Its stated aim, a name that prevents scattering, accrues to them as standing and command capacity; no seat above them can review the decision. If the project stopped, they would lose the name they are building, so their stake in its continuation is total, and their command of the system's resources lets them redirect, exempt, and reframe at will.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, tower_architects, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__babel_reading, tower_architects, beneficiary).

% Run the day-to-day mobilization: scheduling labor, standardizing technique, reporting progress upward. Their position, provision, and standing exist only inside the project; if it halted they would be ordinary builders again. They collect status and material share from the arrangement without setting its direction, and their constrained position makes them its most reliable defenders.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, project_overseers, beneficiary,
    organized, biographical, constrained, regional).

% Supply the labor that raises the city and the tower under a work discipline and a public language not of their choosing. They receive the common tongue's practical benefits — coordination, trade, a share in the security the project promises against scattering — and pay with conscripted work, the submersion of their local speech into the imposed standard, and a monument whose name belongs to the architects. Leaving means the scattering the project exists to prevent, and no provision is made for those who would go.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, conscripted_builders, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__babel_reading, conscripted_builders, beneficiary).

% Communities whose mother tongues and customary practice are being standardized away inside the project's perimeter. Each generation raised in the imposed language loses the inherited one; what is taken is not a fee but the medium of their memory, their songs, and their worship. They have no forum in which their speech counts as a contribution rather than a deviation, and abandoning the inherited tongue would be the dissolution of who they are, not a relocation.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, linguistic_minorities, payer,
    powerless, generational, identity_locked, regional).

% Clans at the margins whose tongues and customs differ from the imposed standard. The project's plan presupposes their eventual absorption, but no seat in its planning represents them; they would contest the premise that their differentiation is a defect to be engineered away. Their present option is distance, and the project's logic holds distance to be temporary.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, peripheral_clans, excluded,
    powerless, generational, constrained, regional).

% The transmitting tradition that preserves the account of the project: its founding summons, its method, its end in mutual incomprehension. They command no labor and collect no share of the name; their function is memory — recording that the arrangement held only while the power held, and handing the pattern to every later generation that attempts it.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, post_collapse_chroniclers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__babel_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single administrative language and standardized technique let a dispersed population be mobilized as one workforce: the city, its works, and the tower are built once under central direction instead of through negotiation among autonomous communities. The common tongue is a real information standard — it genuinely lowers the cost of large-scale cooperation.
% TRANSFER_FUNCTION: Moves labor, loyalty, and linguistic particularity from the general population to the project center. The many surrender conscripted work and the public use of their own tongues; the center accumulates standing, command capacity, and the name the tower exists to secure.
% ABSENT_VOICES: The peripheral clans whose absorption the plan presupposes are not in the conversation, and neither are the generations who would inherit the erased languages — both would contest the premise that their differentiation is a defect to be solved. Within the project itself, the builders' attachment to their mother tongues has no seat; the planning treats speech as an administrative variable.
% DISAPPEARANCE_RATIONALE: Without the enforced standard, linguistic drift resumes immediately, local loyalties and customary practice reassert themselves, and the conscripted workforce disperses — the tower stops where the workers walk. The arrangement was not holding a coordination equilibrium the participants preferred; it was suppressing the differentiation they were already tending toward, so its removal rearranges the linguistic and political landscape rather than leaving a coordination vacuum.
% FOUNDING_PROBLEM: A gathered population fearing dissolution: the founders state it themselves — let us make a name for ourselves, lest we be scattered. The problem as the architects framed it was obscurity and dispersal, to be solved by a single monument and a single speech under one command.
% FOUNDING_PROBLEM_CORROBORATION: No attestation from outside the benefiting parties exists inside the project: the founding problem is voiced only by the architects, and the arrangement's own record shows it solved neither half of it — the scattering came anyway. The corroborating witness lies entirely outside the project: the transmitting account, kept by those who commanded no labor and collected no share, records the motive as self-aggrandizement and the outcome as collapse. That external attestation supports reading the founding problem as cover rather than need.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__babel_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__babel_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__babel_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__babel_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__babel_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__babel_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_transcendence_pathway__babel_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the arrangement's gains — the name, the standing, the command capacity — accrue to a narrow center while its costs are borne broadly, without consent and without pricing: labor is conscripted, and linguistic erasure is not a fee anyone agreed to pay. Suppression (0.76) is constitutive rather than incidental: languages drift toward divergence naturally, so uniformity must be continuously enforced, and the arrangement exists precisely to prevent exit (the scattering). Theater (0.30) is moderate at peak: the city and its works are real outputs, but a rising share of the project is monumental self-reference — the tower's function is the architects' name, which is performance from every seat but theirs. Accessibility collapse (0.52) is mid-range: the suppressed alternatives (local tongues, customary practice) lose public standing but survive in memory and private use, so understanding the arrangement does not fully collapse them. Resistance (0.40): no open revolt is recorded, but the arrangement meets continuous passive friction — linguistic drift is itself a form of resistance, and the desire to disperse is the arrangement's named enemy. The base_properties scalars describe the constraint at its consolidated operating peak (the T32 point); the measurement series runs one shared grid across the whole lifecycle and carries the terminal collapse at the final point — extraction and suppression crater when the power fails, and theater spikes as the unfinished tower becomes pure monument. The arc is rise-and-collapse, not a cycle: the enforcement machinery the series tracks is built up steadily, then disintegrates with the power that maintained it.
 *
 * PERSPECTIVAL GAP:
 *   From the architects' seat the arrangement is the project they summoned and direct — coordination they built, justified by the security it promises against scattering. From the conscripted builders' seat the same structure is conscription under a language not their own. From the linguistic minorities' seat it is the public dissolution of the medium of their memory. The engine computes these as different per-seat classifications from power and exit data: the architects (institutional power, arbitrage exit) compute near the beneficiary end; the trapped builders and the identity-locked minorities compute near the full-target end. The overseers sit between — genuine collectors, but their standing is hostage to the project's continuation, which makes them enforcement's most willing hands.
 *
 * DIRECTIONALITY LOGIC:
 *   The architects and overseers are declared beneficiaries and sit at the beneficiary end of d — the arrangement subsidizes them. The conscripted builders are declared victims with trapped exit: their d sits high, and the trap amplifies effective extraction. The linguistic minorities are victims with identity_locked exit — the imposed standard demands abandonment of the medium of their identity, which places them nearest the full-target end. The peripheral clans are excluded rather than extracted-from; as an authored absence they are commentary-grade and drive no classification override. The chroniclers are the analytical seat with no extraction flow. Suppression is authored as a raw structural property of the constraint and is not scaled by power or scope; extractiveness is what directionality and scope scale in the engine's computation — and the arrangement's regional scope concentrates verification in the center's hands, which is exactly where the enforcement lives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fear of scattering and namelessness — is contested: the arrangement's own record shows it solved neither half, since the scattering came anyway, and the corroborating witness outside the benefiting parties records the motive as self-aggrandizement. Reading the arrangement as a snare rather than a rope is what prevents the mislabeling: the common tongue's genuine coordination value is precisely what makes the extraction's cover effective, and a rope-reading would credit the arrangement with the coordination while ignoring who pays for the tower. The residual question — whether the coordination substrate is genuine net benefit with extraction layered on, or cover through and through — is held open in the coordination_cover_ambiguity omega rather than resolved by fiat. If the founding problem is judged dead (dispersal was never a defect but the misdiagnosed vocation of the population), the arrangement is a monument to a problem that did not exist as stated, maintained by coercion against the participants' own drift — the mandatrophy signature this reading exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the babel_reading of the human_transcendence_pathway kernel. The jerusalem_reading would structure the same human-unity problem as participatory labor under divine blessing — no concentrated architect seat, no enforced uniformity, plurality integrated rather than erased — and the technocratic_vs_incarnational_reading would relocate the contest from linguistic uniformity to the elimination of limits versus their reception as gift. Which structural elements of this arrangement are constitutive of the reading rather than of the kernel?',
    'Cross-reading comparison: author the sibling readings as separate constraints and compare beneficiary/victim structure and epsilon across the family. The disagreement is located in whether unity requires imposed uniformity under concentrated command (this reading) or integrated plurality under blessing (jerusalem_reading), and in whether the pathway requires transcendent reference at all.',
    'Adopting a sibling reading dissolves or relocates this constraint''s victim set: under the jerusalem arrangement there is no linguistic erasure and no architect seat to collect; under the technocratic half the casualties shift from erased languages to eliminated limits. This story''s epsilon is indexed to the enforced-uniformity arrangement and does not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of the human_transcendence_pathway kernel; sibling readings would restructure beneficiaries, victims, and the location of extraction.').

omega_variable(
    coordination_cover_ambiguity,
    'Is the unified language''s coordination function a genuine net-benefit coordination good with extraction layered on top (a tangled-rope structure), or instrumental consolidation whose coordination story is the cover (a snare)?',
    'Compare the arrangement''s fate under enforcement failure with arrangements that solve genuine collective-action problems: a rope survives its enforcer''s departure because participants prefer it, whereas this arrangement collapses into mutual incomprehension when the power fails — which discriminates toward cover. Confirm against counterfactual unification projects that persisted without coercion.',
    'If the coordination substrate dominates, reclassify as tangled_rope with high extraction, and the builders'' secondary beneficiary role becomes primary; if the cover reading holds, the snare classification stands and the common tongue is best read as the extraction''s delivery mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cover_ambiguity, conceptual, 'Whether the arrangement''s coordination function is genuine net benefit or cover for consolidation.').

omega_variable(
    collapse_endogeneity_question,
    'Was the arrangement''s terminal collapse endogenous to its structure (coercion-dependent cohesion that fails when power fails) or exogenous (an external intervention no internal arrangement could have survived)?',
    'Comparative lifecycle analysis of unification-by-fiat projects that faced no external shock: if they fragment on their own timescale, the fragility is structural; if comparable arrangements endured indefinitely, the collapse is attributable to the specific intervention recorded in the account.',
    'An endogenous collapse supports the snare reading — persistence depends on coercion, and the coercion is self-undermining. An exogenous collapse would leave the arrangement''s stability open and shift the classification question toward tangled_rope with the intervention as an independent variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_endogeneity_question, empirical, 'Whether the communication breakdown at the end of the interval was structural fragility or external intervention.').

omega_variable(
    archetype_vs_instantiation,
    'Does the constraint''s epsilon describe the singular narrative event or the recurring imperial pattern — every unification-by-fiat and imposed-lingua-franca project — that the reading treats as the arrangement''s afterlife?',
    'Test the metric profile against documented homogenization regimes (imposed administrative languages, standardization campaigns, forced linguistic consolidation): if the profile recurs across instantiations, the epsilon is pattern-level and stable; if it varies widely, the archetype must be decomposed into per-regime constraint stories with their own epsilon.',
    'A pattern-level reading supports this story''s high epsilon as structural to the arrangement type; wide variance would require decomposition, with this story re-scoped to the narrative instantiation only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archetype_vs_instantiation, conceptual, 'Whether the enforced-uniformity arrangement is assessed as one event or a recurring structural pattern.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__babel_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(babel_reading_tr_t0, human_transcendence_pathway__babel_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(babel_reading_tr_t8, human_transcendence_pathway__babel_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(babel_reading_tr_t16, human_transcendence_pathway__babel_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(babel_reading_tr_t24, human_transcendence_pathway__babel_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(babel_reading_tr_t32, human_transcendence_pathway__babel_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(babel_reading_tr_t40, human_transcendence_pathway__babel_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(babel_reading_be_t0, human_transcendence_pathway__babel_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(babel_reading_be_t8, human_transcendence_pathway__babel_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(babel_reading_be_t16, human_transcendence_pathway__babel_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(babel_reading_be_t24, human_transcendence_pathway__babel_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(babel_reading_be_t32, human_transcendence_pathway__babel_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(babel_reading_be_t40, human_transcendence_pathway__babel_reading, base_extractiveness, 40, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(babel_reading_su_t0, human_transcendence_pathway__babel_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(babel_reading_su_t8, human_transcendence_pathway__babel_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(babel_reading_su_t16, human_transcendence_pathway__babel_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(babel_reading_su_t24, human_transcendence_pathway__babel_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(babel_reading_su_t32, human_transcendence_pathway__babel_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(babel_reading_su_t40, human_transcendence_pathway__babel_reading, suppression_requirement, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__babel_reading, information_standard).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__jerusalem_reading).

% DUAL FORMULATION NOTE:
% The human_transcendence_pathway kernel decomposes into three structurally distinct readings, each a separate constraint with its own epsilon, beneficiaries, and victims: this file (babel_reading — enforced uniformity under concentrated command, high epsilon, coercive homogenization), the jerusalem_reading (participatory communion integrating plurality — coordination without a concentrated extractor), and the technocratic_vs_incarnational_reading (limit-elimination versus received transcendence). The babel reading is upstream in the tradition's logic: the tower is the archetype against which every technological-unification project is measured, so this constraint's rise-and-collapse pattern structurally pressures how the sibling readings are assessed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
