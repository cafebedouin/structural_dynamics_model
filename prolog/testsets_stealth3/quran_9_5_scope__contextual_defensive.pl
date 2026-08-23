% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Verse 9:5 Contextual-Defensive Scope Settlement
 *   domain: religious/hermeneutic/political
 *
 * SUMMARY:
 *   Within the quran_9_5_scope kernel, the contextual_defensive reading fixes
 *   Verse 9:5's legal operation to its seventh-century Medinan occasion: the
 *   verse licenses force against specific treaty-breaking polytheist tribes,
 *   does not abrogate the Quran's peaceful verses, and subordinates any
 *   martial application to prior aggression or treaty breach. As a standing
 *   arrangement, this settlement gates the verse's war-making force behind a
 *   narrow, adjudicated exception while structurally protecting coexistence
 *   norms, treaty obligations, and minority communities. It is maintained
 *   actively: state muftiates, juristic councils, curricula, and
 *   counter-extremism law all work to hold the contextual reading against
 *   abrogationist revival. CONSTRAINT FAMILY NOTE: the colloquial label 'the
 *   Sword Verse' decomposes into three structurally distinct constraints per
 *   the epsilon-invariance principle — abrogating_universal (mass victim set,
 *   high extraction), this contextual_defensive settlement (narrow
 *   misconduct-conditioned victim set, moderate-low extraction), and
 *   progressive_synthesis (no operative victim set; the verse retired as
 *   law). Each member has its own epsilon, beneficiaries, and victims; they
 *   are linked, not merged. KEY AGENTS (by structural relationship): -
 *   integrationist_muslim_majority_states: Agenda setter & primary
 *   beneficiary (institutional/constrained) — administers the settlement,
 *   adjudicates breach, collects legitimacy - juristic_establishment:
 *   Adjudicative beneficiary (institutional/identity_locked) — gatekeeping
 *   authority constituted by the reading itself -
 *   protected_treaty_communities: Protected beneficiary
 *   (moderate/constrained) — receives doctrinal coexistence cover -
 *   lay_muslim_communities: Incidental beneficiary (organized/constrained) —
 *   enjoys civic peace, bears security costs - treaty_violating_belligerents:
 *   Primary target (organized/trapped) — bears the settlement's authorized
 *   defensive force - abrogationist_movements: Secondary target
 *   (organized/identity_locked) — bears doctrinal suppression -
 *   international_law_bodies: Analytical observer (institutional/analytical)
 *   — treats the settlement as the legitimate baseline
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.3).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.36).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.3).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Verse 9:5 Contextual-Defensive Scope Settlement").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/hermeneutic/political").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '9a852faf-e3a0-4c87-acf9-d735f4eae283').
narrative_ontology:cs_kernel_codification('9a852faf-e3a0-4c87-acf9-d735f4eae283', fixed_text).
narrative_ontology:cs_authority_grounding('9a852faf-e3a0-4c87-acf9-d735f4eae283', lineage).
narrative_ontology:cs_interpretation_layer_present('9a852faf-e3a0-4c87-acf9-d735f4eae283').
narrative_ontology:cs_reading_relation('9a852faf-e3a0-4c87-acf9-d735f4eae283', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('9a852faf-e3a0-4c87-acf9-d735f4eae283', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('9a852faf-e3a0-4c87-acf9-d735f4eae283', foundational, verse_scope_fixed_by_occasion_of_revelation).
narrative_ontology:cs_axiom_status(verse_scope_fixed_by_occasion_of_revelation, holdable).
narrative_ontology:cs_axiom_grounding('9a852faf-e3a0-4c87-acf9-d735f4eae283', verse_scope_fixed_by_occasion_of_revelation, conventional).
narrative_ontology:cs_axiom('9a852faf-e3a0-4c87-acf9-d735f4eae283', foundational, offensive_force_requires_prior_aggression_or_treaty_breach).
narrative_ontology:cs_axiom_status(offensive_force_requires_prior_aggression_or_treaty_breach, holdable).
narrative_ontology:cs_axiom_grounding('9a852faf-e3a0-4c87-acf9-d735f4eae283', offensive_force_requires_prior_aggression_or_treaty_breach, deontological).
narrative_ontology:cs_reference_frame('9a852faf-e3a0-4c87-acf9-d735f4eae283', treaty_scoped_medinan_command).
narrative_ontology:cs_drift_state('9a852faf-e3a0-4c87-acf9-d735f4eae283', contemporary_global_security_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9a852faf-e3a0-4c87-acf9-d735f4eae283', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, juristic_establishment).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, protected_treaty_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_violating_belligerents).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, abrogationist_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, lay_muslim_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact the settlement through official fatwa councils, school curricula, and counter-extremism legislation; decide when a group counts as treaty-breaking and therefore liable to force; collect international legitimacy, minority-protection credentials, and domestic civic peace from the arrangement. Reversing course would mean repudiating decades of official doctrine, inviting insurgent recruitment and diplomatic isolation, so exit is formally available but heavily penalized.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary).

% Councils of ulama, state muftiates, and endowed seminaries adjudicate the scope questions the settlement turns on — which treaties bind, which breaches count, when defense begins. Their gatekeeping office is constituted by the contextual reading itself; adopting a rival reading would dissolve the adjudicative role they occupy. Institutional identity is fused with custodianship of the interpretive settlement.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, juristic_establishment, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__contextual_defensive, juristic_establishment, agenda_setter).

% Religious minorities living as citizens or protected communities under the settlement's treaty-priority norms. They receive doctrinal cover for coexistence, worship, and property, but enforcement varies by state and they hold no formal seat in the juristic bodies that adjudicate their communities' status. Emigration is possible but costly in kinship, livelihood, and legal terms.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, protected_treaty_communities, beneficiary,
    moderate, generational, constrained, national).

% Majority populations whose civic life depends on the internal peace the settlement supports. They bear the security costs of insurgent violence carried out under the rival reading and the social costs of counter-extremism policing. Personal exit (apostasy or emigration) carries severe familial and legal consequence, binding them to the settlement's fortunes.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, lay_muslim_communities, beneficiary,
    organized, biographical, constrained, national).

% Armed groups or polities adjudicated to have broken treaties or initiated aggression. Once classified, they bear the settlement's authorized defensive force. Their path out runs through ceasing hostilities and renewing agreement, which the adjudicating side must accept; while hostilities continue they sit inside the target set with no third-party appeal channel.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_violating_belligerents, payer,
    organized, immediate, trapped, regional).

% Transnational currents holding that Verse 9:5 abrogates the peaceful verses and licenses universal offensive jihad. Under the settlement they are proscribed, their preaching criminalized, their financing and networks disrupted. The doctrine is constitutive of membership — abandoning it dissolves the movement — so they absorb suppression rather than exit, and their persistence feeds back into harder enforcement.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, abrogationist_movements, payer,
    organized, immediate, identity_locked, global).

% UN organs, counter-terrorism directorates, and treaty-monitoring bodies engage Muslim-majority states on counter-terrorism and minority rights. They treat the contextual-defensive settlement as the legitimate doctrinal baseline, fund compliance programming, and document gaps between doctrine and practice without adjudicating the underlying interpretive contest.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes the legal scope of a martial verse so that Muslim polities, minority communities, and treaty partners share a stable rule for when force is licit: treaties bind, defense follows adjudicated breach, and the peaceful verses retain force. Without a fixed scope, every faction reads 9:5 to suit itself and inter-communal peace becomes unenforceable.
% TRANSFER_FUNCTION: Moves the war-licensing force of 9:5 off the general population of non-Muslims and onto a narrow adjudicated class (treaty violators after prior aggression); moves adjudication authority over 'violation' to state and juristic gatekeepers; moves security, legal, and social costs onto doctrinal dissenters who advocate the rival reading.
% ABSENT_VOICES: The seventh-century treaty parties whose breach occasioned the verse cannot testify — the contextual case rests on asbab-al-nuzul reports transmitted largely by the victorious side. Contemporary non-Muslim citizens sit outside the juristic councils that adjudicate their communities' status in most adopting states. Progressive-reform scholars skeptical of any operative martial reading are consulted unevenly. Abrogationist currents are not absent but suppressed — present in the discourse precisely as the proscribed other.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the interpretive vacuum would be contested immediately: abrogationist currents claim the verse directly and would gain recruits among the aggrieved; states would lose the doctrinal basis for minority-protection guarantees and counter-terrorism cooperation; treaty frameworks across dozens of polities would lose their scriptural anchor. Minority protections, counter-extremism law, and inter-state security arrangements all depend on this reading holding.
% FOUNDING_PROBLEM: Reconcile 9:5's unconditional-sounding martial language with the Quran's own treaty and restraint norms (2:190, 4:90, 8:61, 9:4, 9:6-7) and with the demands of governing pluralistic societies — first in the classical period (explaining why the Medinan polity fought some tribes while honoring treaties with others), and in the modern period (reconciling scripture with citizenship, international law, and the state's monopoly on force).
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: academic Quranic scholarship in Muslim and non-Muslim universities documents the unresolved tension; the persistent citation of 9:5 by proscribed abrogationist movements demonstrates the problem is not closed; international counter-terrorism bodies' continued engagement with Muslim-majority states on exactly this doctrinal terrain confirms liveness. No party to the dispute treats the problem as settled.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-low (0.30 at interval end): the victim class is narrow and misconduct-conditioned — entry into the target set requires adjudicated treaty breach or prior aggression — but the costs borne there are severe (authorized armed response), and dissenters against the settlement's doctrine bear real suppression costs. Suppression (0.36, raw and unscaled — only extractiveness is scaled by directionality and scope in the engine's computation) reflects enforcement machinery that matured materially over the interval, which is why suppression_requirement is the temporal series this story traces: the settlement's enforcement capacity hardened, especially as counter-extremism frameworks expanded. Theater is low but rising (0.22): ceremonial tolerance performances and interfaith spectacle have grown faster than minority-protection delivery in several adopting states. Accessibility_collapse is low (0.30): the rival readings remain fully live — understanding this settlement does not close off the abrogationist or progressive alternatives, which persist as competing constraints. Resistance is substantial (0.55): abrogationist currents contest the settlement directly by citing the verse's plain text, and progressive scholars push from the opposite flank. The measurement series run on ONE shared eight-point grid (t=0..70) so every tracked metric is authored at every examined time point; the drift is monotone, not cyclical, driven by enforcement ratcheting rather than oscillating reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute a coordination-dominant type: from the state and juristic seats, the settlement is fidelity to revelation's context, treaty prudence, and civic peace. The payer seats should compute extraction-dominant classifications: from inside an adjudicated belligerent group or a proscribed abrogationist network, the same structure operates as a violence license held by adversaries and a doctrinal prosecution. The engine derives this divergence from the structural data — power, exit, and role asymmetries — not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist states sit near the beneficiary end (d low): they administer the gate and collect legitimacy and security-policy flexibility from it. The juristic establishment likewise collects authority rents, with identity-lock amplifying its attachment to the settlement. Protected treaty communities and lay Muslim communities are subsidized beneficiaries (d very low) — coexistence cover and civic peace flow to them, with diffuse indirect costs. Treaty-violating belligerents sit near the full-target end (d high): they bear concentrated, severe costs, and their exit (renewed agreement accepted by the adjudicator) is controlled by the opposing side. Abrogationist movements also sit near the full-target end: they bear proscription and prosecution, and their identity lock removes exit-by-conversion, trapping them in the suppression relationship. Scope effects amplify effective extraction for the globally distributed abrogationist target class relative to regionally bounded belligerents.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Reading the settlement as pure rope would erase the genuine structural asymmetry: the party that benefits from the martial exception also adjudicates when the exception applies, and the enforcement machinery increasingly serves gatekeeper authority as much as civilian protection. Reading it as a snare would erase the real coordination function — treaty priority, minority protection, and a shared rule that keeps inter-communal peace enforceable — and would wrongly equate a misconduct-conditioned victim class with the mass victim set of the abrogating_universal sibling. The mandate is not outlived: the founding problem (reconciling 9:5 with the Quran's treaty norms and with pluralistic governance) remains live and contested, so no mandatrophy resolution is declared; the founding_problem_status x disappearance_verdict pair (live x world_rearranges) carries no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_scope_contest,
    'This constraint is one reading of kernel quran_9_5_scope: is Verse 9:5''s legal scope fixed by its Medinan treaty context (this reading, contextual_defensive), universal and abrogating of the peaceful verses (abrogating_universal), or time-bound and retired as law (progressive_synthesis)?',
    'Intra-traditional jurisprudential adjudication (ijma formation, authoritative tafsir and fatwa councils across the major madhhabs) converging with academic asbab-al-nuzul scholarship; monitor shifts in official state muftiate positions.',
    'Sibling readings change the victim set and epsilon wholesale: abrogating_universal moves every non-treatied polytheist community into the victim set (snare-classified arrangement, mass extraction); progressive_synthesis empties the operative victim set entirely (retires the verse as law, leaving at most retained rhetoric). This file authors epsilon only for the contextual-defensive arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_contest, conceptual, 'The kernel contest over 9:5''s legal scope; this story is one seat of three.').

omega_variable(
    adjudication_capture_risk,
    'Who determines that a treaty was violated, and do violation determinations track independent evidence or the adjudicating party''s interest?',
    'Comparative case study of breach determinations across the settlement''s history, coding each for independent corroboration (third-party witnesses, adversary concessions, neutral arbitration) versus unilateral declaration by the beneficiary-side polity.',
    'Systematic unilateral determination converts the exception gate into a beneficiary-controlled license to fight and pushes the arrangement toward snare; consistently corroborated determinations support the tangled_rope reading with genuine coordination dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_capture_risk, empirical, 'Whether the treaty-violation gate is captured by the party that benefits from it.').

omega_variable(
    suppression_function_ambiguity,
    'Is the enforcement directed at abrogationist advocacy protective counter-extremism, or extractive orthodoxy enforcement that also silences nonviolent reform and rival scholarship?',
    'Compare enforcement targets and outcomes: prosecutions of violent networks versus of nonviolent dissenting scholars; grievance and radicalization trajectories in heavily policed communities; scope of proscription lists.',
    'If enforcement routinely sweeps nonviolent dissent, measured suppression serves establishment rent-preservation and effective extraction rises above the authored base; if narrowly targeted at violent networks, suppression sits closer to coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_function_ambiguity, conceptual, 'Whether the settlement''s suppressive machinery protects coexistence or entrenches gatekeeper authority.').

omega_variable(
    doctrine_practice_gap,
    'Do protected_treaty_communities actually receive the coexistence protections the settlement promises, or does the gap between doctrinal guarantee and administrative practice widen over the interval?',
    'Longitudinal minority-rights indicators (freedom of worship, property security, legal equality, council representation) in states officially adopting the settlement, benchmarked against the doctrinal commitments the reading declares.',
    'A widening gap drives theater_ratio upward toward piton dynamics (protective doctrine maintained performatively while function atrophies); convergence supports the coordination-function reading and keeps theater low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap, empirical, 'Whether promised minority protections are delivered or performed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95_ctx_def_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.08).
narrative_ontology:measurement(q95_ctx_def_tr_t10, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.09).
narrative_ontology:measurement(q95_ctx_def_tr_t20, quran_9_5_scope__contextual_defensive, theater_ratio, 20, 0.11).
narrative_ontology:measurement(q95_ctx_def_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.13).
narrative_ontology:measurement(q95_ctx_def_tr_t40, quran_9_5_scope__contextual_defensive, theater_ratio, 40, 0.16).
narrative_ontology:measurement(q95_ctx_def_tr_t50, quran_9_5_scope__contextual_defensive, theater_ratio, 50, 0.18).
narrative_ontology:measurement(q95_ctx_def_tr_t60, quran_9_5_scope__contextual_defensive, theater_ratio, 60, 0.2).
narrative_ontology:measurement(q95_ctx_def_tr_t70, quran_9_5_scope__contextual_defensive, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(q95_ctx_def_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(q95_ctx_def_be_t10, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(q95_ctx_def_be_t20, quran_9_5_scope__contextual_defensive, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(q95_ctx_def_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(q95_ctx_def_be_t40, quran_9_5_scope__contextual_defensive, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(q95_ctx_def_be_t50, quran_9_5_scope__contextual_defensive, base_extractiveness, 50, 0.27).
narrative_ontology:measurement(q95_ctx_def_be_t60, quran_9_5_scope__contextual_defensive, base_extractiveness, 60, 0.29).
narrative_ontology:measurement(q95_ctx_def_be_t70, quran_9_5_scope__contextual_defensive, base_extractiveness, 70, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(q95_ctx_def_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(q95_ctx_def_su_t10, quran_9_5_scope__contextual_defensive, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(q95_ctx_def_su_t20, quran_9_5_scope__contextual_defensive, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(q95_ctx_def_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.21).
narrative_ontology:measurement(q95_ctx_def_su_t40, quran_9_5_scope__contextual_defensive, suppression_requirement, 40, 0.24).
narrative_ontology:measurement(q95_ctx_def_su_t50, quran_9_5_scope__contextual_defensive, suppression_requirement, 50, 0.28).
narrative_ontology:measurement(q95_ctx_def_su_t60, quran_9_5_scope__contextual_defensive, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(q95_ctx_def_su_t70, quran_9_5_scope__contextual_defensive, suppression_requirement, 70, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the Sword Verse' (kernel quran_9_5_scope). Three stories, three epsilons: abrogating_universal instantiates a standing universal offensive-war obligation (mass victim set, high extraction, snare-flavored); contextual_defensive (this file) instantiates a treaty-gated defensive settlement (narrow misconduct-conditioned victim set, moderate-low extraction, tangled_rope); progressive_synthesis retires the verse as law (no operative victim set). The abrogating reading cites the verse's plain text as upstream evidence against this reading, while this reading's philological and asbab-al-nuzul apparatus supplies the scholarly substrate the progressive reading builds on — hence bidirectional family links rather than a single chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
