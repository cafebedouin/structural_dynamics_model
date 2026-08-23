% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause Scope — Expansive Universalist Reading
 *   domain: constitutional law/political philosophy/civil rights history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the equality-clause-scope kernel:
 *   the expansive universalist reading, under which the constitutional
 *   equality guarantee speaks for every human being, founding-era exclusions
 *   register as betrayals to be corrected rather than precedents to honor,
 *   and courts may widen the guarantee's application by interpretation
 *   without awaiting amendment. The story describes that reading as it
 *   operates — the standing arrangement it constitutes — and not the contest
 *   over it; the restrictive-originalist and progressive-textualist readings
 *   are separate constraint files linked through network.affects_constraints.
 *   Claim and metrics are authored independently: I claim tangled_rope
 *   because the reading genuinely coordinates membership (a single public
 *   criterion for who counts, replacing case-by-case boundary renegotiation)
 *   while asymmetrically concentrating interpretive authority in the bench
 *   and imposing concentrated losses on displaced prerogative-holders, all
 *   held by active enforcement; the metrics report the arrangement's
 *   operation as I descriptively assess it through this reading's own lights.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setting interpreter (institutional/identity_locked) — accrues permanent doctrinal territory with each expansion
 *   - historically_excluded_groups: primary beneficiary (organized/constrained) — hold their inclusion by court decision
 *   - civil_rights_advocacy_bar: secondary beneficiary (organized/constrained) — staffs the expansion pipeline
 *   - displaced_privilege_holders: principal payer (powerful/constrained) — lost exclusion-based standing by mandate
 *   - state_legislatures: payer with incidental protective cover (institutional/trapped)
 *   - democratic_electorates: dual-positioned payer/beneficiary (organized/trapped)
 *   - nonvoting_residents: covered by the universal claim but absent from the conversation (powerless/trapped)
 *   - future_generations: silent beneficiaries of already-asserted scope (powerless/trapped)
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.32).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.62).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.32).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause Scope — Expansive Universalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional law/political philosophy/civil rights history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '2e2f0c64-4980-47bd-aafb-5694ed2b4cf6').
narrative_ontology:cs_kernel_codification('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', fixed_text).
narrative_ontology:cs_authority_grounding('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', lineage).
narrative_ontology:cs_interpretation_layer_present('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6').
narrative_ontology:cs_reading_relation('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', foundational, universal_scope_regardless_of_founding_consent).
narrative_ontology:cs_axiom_status(universal_scope_regardless_of_founding_consent, holdable).
narrative_ontology:cs_axiom_grounding('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', universal_scope_regardless_of_founding_consent, deontological).
narrative_ontology:cs_axiom('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', foundational, judicial_expansion_low_threshold).
narrative_ontology:cs_axiom_status(judicial_expansion_low_threshold, holdable).
narrative_ontology:cs_axiom_grounding('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', judicial_expansion_low_threshold, instrumental).
narrative_ontology:cs_axiom('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', secondary, historical_exclusions_correctable_not_binding).
narrative_ontology:cs_axiom_status(historical_exclusions_correctable_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', historical_exclusions_correctable_not_binding, deontological).
narrative_ontology:cs_reference_frame('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', universal_self_evident_scope).
narrative_ontology:cs_drift_state('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', contemporary_originalist_revival_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2e2f0c64-4980-47bd-aafb-5694ed2b4cf6', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_advocacy_bar).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, federal_judiciary).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, displaced_privilege_holders).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, state_legislatures).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, democratic_electorates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, state_legislatures).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, democratic_electorates).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, future_generations).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, self_evident_equality_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, anti_hypocrisy_precedent_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, judicial_expansion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured judges and justices who decide what the equality guarantee requires in concrete disputes. Each ruling that widens coverage becomes precedent only the bench can revisit, so interpretive territory accrues to the court permanently. A sitting judge's professional identity is fused with the body of rulings they join; abandoning the expansive line would mean repudiating their own work and colleagues', which in practice no one does.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Groups the founding generation barred from the franchise, office, and full civil standing — later brought inside the guarantee by judicial construction. They hold their inclusion by court decision rather than unanimous consent, depend on the bench continuing to maintain it, cannot relocate out of the jurisdiction governing them, and organize as their principal lever.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Litigators, clinics, and advocacy organizations that bring the suits through which coverage expands. Careers, funding streams, and reputations concentrate where new exclusion questions keep arising; the skills port to other fields, but the invested institutions and coalitions do not.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_advocacy_bar, beneficiary,
    organized, biographical, constrained, national).

% Holders of positions that rested on the old exclusions — operators of segregated institutions, gatekeepers of restricted franchises and professions. Mandates stripped or diluted these prerogatives faster than gradual adjustment would have. Residual wealth and political influence remain, but the lost standing cannot be recovered within the current arrangement; some divert assets into private domains the mandates reach less directly.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, displaced_privilege_holders, payer,
    powerful, biographical, constrained, national).

% State lawmakers who lost direct control over who counts and what equal treatment demands within their borders, as federal mandates supersede local statute. The same mandates sometimes shield them from locally powerful factions they would prefer to defy, so the loss of discretion and the protective cover arrive together, and no lawful exit from federal supervision exists.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, state_legislatures, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, state_legislatures, beneficiary).

% Voters who once set the guarantee's reach through amendment and legislation and now watch unelected benches redraw it case by case. They retain the formal amendment route and gained a rights floor no local majority can strip; most live inside both facts at once and cannot exit the polity either way.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, democratic_electorates, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, democratic_electorates, beneficiary).

% Residents — including long-term non-citizen residents and disenfranchised citizens — whom the universal claim covers in principle but who cannot vote for the officials who appoint and confirm interpreters and rarely hold the standing or resources to litigate. They receive the arrangement's protections mostly at second hand and have no seat in any forum that moves its scope.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, nonvoting_residents, excluded,
    powerless, immediate, trapped, national).

% People not yet born whose entitlement the reading already asserts. They are covered by today's doctrine but take no part in today's appointments, elections, or lawsuits; whatever scope is settled now arrives to them as settled fact.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Academic observers tracking how equality clauses operate across dozens of constitutional orders — adoption, backlash, export. They hold no enforcement power, owe allegiance to no camp, and study the trajectories from outside the disputes they document.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, federal_judiciary).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one public criterion for full membership in the constitutional community — who is owed equal concern — so that each newcomer's status need not be renegotiated case by case and no faction must separately defend every exclusion it inherits.
% TRANSFER_FUNCTION: Moves interpretive authority and public standing from holders of inherited prerogative to courts and to groups newly brought inside the guarantee; moves binding equal-treatment obligations onto dissenting institutions and jurisdictions; moves the warrant for scope from founding-generation consent to present-day moral assertion.
% ABSENT_VOICES: Restrictive-originalist voices are heard but lose doctrinally; the structurally absent are non-voting residents covered without standing to shape the arrangement, future generations bound without consent, the founding generation whose assent is overridden, and communities abroad upon whom the exported doctrine lands without local ratification.
% DISAPPEARANCE_RATIONALE: If the universal reading vanished overnight, seven decades of settled inclusion — desegregation, sex equality, equal franchise — would lose their doctrinal floor at a stroke; courts, legislatures, schools, employers, and marriage law would all have to re-derive status rules from scratch, and every previously excluded group would face immediate renegotiation of standing with no agreed criterion for conducting it.
% FOUNDING_PROBLEM: How to bind a polity to an equality principle written by a generation that enslaved people and barred women and the propertyless from the franchise — preserving the text's authority and the principle's force together instead of sacrificing one to the other.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and scholars outside the beneficiary set attest the founding problem is unresolved — they dispute the resolution, not the problem's persistence. Reconstruction-era congressional debates and modern historiography document the reconciliation struggle contemporaneously. Comparative constitutional scholars record the same struggle recurring in every successor republic that adopted the formula. Corroboration of the problem is broad and external; corroboration of THIS resolution comes almost entirely from the arrangement's beneficiaries, which is itself signal.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.32 at interval end) is authored through this reading's own lights over the arrangement it constitutes: the reading prizes universal inclusion above procedural purity and inherited expectation, so it registers the arrangement as only mildly extractive — real costs (displaced expectations, compelled compliance for dissenters, authority drained from electoral channels) set against a benefit flow the reading regards as vastly larger. Suppression (0.62) reports the raw structural fact that rival readings are barred from operative law by judicial supremacy while remaining free in political discourse; suppression is a structural property and is deliberately not scaled by power or scope. Theater (0.20) reflects predominantly functional adjudication with a growing ceremonial layer. The three temporal series share one eight-point grid (interval units approximate years from Brown v. Board, 1954 to 2024). Extractiveness creeps upward as each expansion seeds the next contested frontier; the suppression_requirement series climbs because this interval's story is genuinely enforcement-capacity build-up — federal enforcement statutes, administrative machinery, injunctive practice erected to hold expansions against backlash — not merely shifting extraction; theater drifts up as commemorative and curricular invocation grows alongside the operative work.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the bench, the arrangement is the republic finally honoring its own text — each expansion is discovery, not decree. From displaced prerogative-holders, the same rulings are uncompensated expropriation of standing they held in good faith. From statehouses aligned with the amendment-first camp, the mechanism itself is the injury: scope moved without a single ratifying vote. From the newly included, the arrangement is a promissory note honored late and partially. The engine derives these per-seat classifications from power, exit, and declared position; the divergence is the finding, not noise to be reconciled away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The judiciary sits nearest the beneficiary pole (d roughly 0.05-0.10): it collects the arrangement's principal yield — durable interpretive authority — and its identity lock removes the exit pressure that would otherwise temper collection. Newly included groups derive low d as beneficiaries, but their trapped exit keeps realized protection hostage to continued enforcement. The advocacy bar derives low d with mild capture risk worth watching. Displaced privilege-holders derive near-target directionality: they pay in lost standing with constrained exit; private-domain arbitrage blunts but does not remove the charge. State legislations would derive near-target directionality from the victim declaration alone; the derivation understates the shielding benefit federal mandates occasionally provide against local factions, so their true position sits slightly off the target pole — noted here rather than overridden, because a single power-atom override would also displace the judiciary's correctly derived value. Electorates straddle the middle: scope-control surrendered against a rights floor gained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a universal principle with its violating founders — remains live: each new claimant group re-poses it. With founding_problem_status live paired with disappearance_verdict world_rearranges, the mismatch consumer finds no zombie flag, and none is due: the arrangement performs its function daily. The classification disciplines both failure directions. Reading the arrangement as pure coordination would erase the identifiable payers — displaced holders, sidelined legislatures — and launder authority concentration as neutrality. Reading it as pure extraction would erase the genuine membership coordination that makes most participants embrace the structure, and would misread a widely ratified-in-effect guarantee as mere cover. The hybrid records both truths and lets per-seat computation expose who is coordinated and who pays. No directionality overrides are used: the beneficiary/victim-plus-exit derivation separates the seats adequately, and the coarse power-atom keying of overrides risks colliding the judiciary and state legislatures onto one value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_referent,
    'Does this story''s epsilon describe the expansive-universalist arrangement itself, or the underlying equality clause independent of any reading?',
    'Side-by-side compilation of all three sibling readings: identical referents across the files confirm the decomposition succeeded; convergence of epsilon values across readings would indicate the stories leaked into one another.',
    'If epsilon were attributed to the bare clause rather than to this reading, classification would migrate toward a clause-level structure and the per-reading structural deltas would vanish.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_referent, conceptual, 'Separation of kernel-level and reading-level referents for epsilon.').

omega_variable(
    self_evidence_vs_enactment,
    'Is the universalist claim''s warrant genuine deontological self-evidence, or enacted convention layered through Reconstruction amendments and accumulated judicial doctrine?',
    'Doctrinal genealogy: trace whether landmark expansions cite moral self-evidence or ratified text; count expansions resting solely on unratified moral assertion versus amendment-backed ones.',
    'If convention dominates, the arrangement''s naturality presentation fails and its profile shifts toward a constructed-and-defended structure; if self-evidence genuinely carries the docket, the reading''s self-presentation as discovered truth gains substance and mountain-flavored evaluation becomes defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_evidence_vs_enactment, conceptual, 'Whether the reading''s authority rests on moral self-evidence or on enactment.').

omega_variable(
    restitution_or_extraction,
    'Are displaced privilege-holders'' losses extraction performed by this arrangement, or the cessation of an extraction they had themselves been running?',
    'Cross-seat value-framework comparison: the verdict turns on whether pre-existing exclusion-based holdings count as legitimate baselines, which no structural measurement can settle alone.',
    'Under the restitution framing, effective extraction for the displaced seats collapses and the arrangement trends toward pure coordination; under the extraction framing, their measured burden stands and the asymmetric half of the hybrid classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restitution_or_extraction, preference, 'Whether losses borne by former insiders count as extraction or restitution.').

omega_variable(
    judicial_authority_byproduct_or_purpose,
    'Is the accumulation of judicial interpretive authority a side-effect of rights expansion, or an operating objective that the expansion serves?',
    'Counterfactual docket analysis: identify cases where the bench declined expansion opportunities carrying no authority gain, versus expansions pressed at doctrinal cost to consolidate interpretive territory.',
    'A byproduct finding supports coordination primacy within the hybrid classification; a purpose finding upgrades extraction and raises the weight of pure-extraction risk in downstream analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_byproduct_or_purpose, empirical, 'Whether judicial authority growth is incidental or purposive.').

omega_variable(
    civic_cre_internalization,
    'Is the measured suppression of rival readings chiefly structural (court enforcement barring them from operative law) or internalized (citizens schooled into the universalist creed finding restrictive arguments literally unthinkable)?',
    'Post-suppression trajectory test: if restrictive arguments recover vigor wherever enforcement lapses, suppression was structural; if they fail to revive even where unenforced, internalization dominates.',
    'A large internalized share raises true suppression above the structural scalar and predicts the arrangement persisting well past any enforcement decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_cre_internalization, empirical, 'Structural versus internalized mechanism behind suppression of rival readings.').

omega_variable(
    expansion_backlash_cycle_function,
    'Does the expansion-backlash-consolidation cycle function as intermittent reinforcement that leaves the bench with broader doctrine after each round, or is it ordinary democratic oscillation?',
    'Phase-mapping of docket composition across at least two full cycles: measure whether post-backlash settlements consistently restore more interpretive territory to the courts than existed before the triggering expansion.',
    'A reinforcement reading upgrades the oscillation itself to an extraction mechanism and raises long-run extraction projections; oscillation-as-ordinary-politics leaves the trajectory effectively linear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansion_backlash_cycle_function, empirical, 'Function of the recurring expansion-backlash cycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__expansive_universalist, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__expansive_universalist, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__expansive_universalist, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(equa_tr_t50, observed).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__expansive_universalist, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(equa_tr_t60, observed).
narrative_ontology:measurement(equa_tr_t70, equality_clause_scope__expansive_universalist, theater_ratio, 70, 0.2).
narrative_ontology:measurement_basis(equa_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__expansive_universalist, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__expansive_universalist, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__expansive_universalist, base_extractiveness, 40, 0.29).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.3).
narrative_ontology:measurement_basis(equa_be_t50, observed).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__expansive_universalist, base_extractiveness, 60, 0.31).
narrative_ontology:measurement_basis(equa_be_t60, observed).
narrative_ontology:measurement(equa_be_t70, equality_clause_scope__expansive_universalist, base_extractiveness, 70, 0.32).
narrative_ontology:measurement_basis(equa_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__expansive_universalist, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__expansive_universalist, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__expansive_universalist, suppression_requirement, 40, 0.57).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.59).
narrative_ontology:measurement_basis(equa_su_t50, observed).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__expansive_universalist, suppression_requirement, 60, 0.61).
narrative_ontology:measurement_basis(equa_su_t60, observed).
narrative_ontology:measurement(equa_su_t70, equality_clause_scope__expansive_universalist, suppression_requirement, 70, 0.62).
narrative_ontology:measurement_basis(equa_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'constitutional equality.' The single label conflates three structurally distinct arrangements that differ on the scope-determination rule: this file authors the expansive-universalist arrangement (universal beneficiary set, exclusions non-binding, judicial expansion legitimate); equality_clause_scope__restrictive_originalist authors the founding-consent-limited arrangement; equality_clause_scope__progressive_textualist authors the amendment-channel arrangement. Each carries its own epsilon, beneficiary/victim structure, and classification. The upstream/downstream edge runs from this file to both siblings because the operative dominance of the universalist reading changes the legitimacy conditions and practical stakes under which the rivals are argued: it does not logically eliminate the textualist channel (amendment remains available) but it does raise the status-quo floor the textualist must amend from, and it supplies the originalist camp its grievance and recruiting narrative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
