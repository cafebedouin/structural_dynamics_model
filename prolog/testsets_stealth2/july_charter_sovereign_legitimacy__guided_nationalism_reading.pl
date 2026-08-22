% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter Religious-Identity Legitimacy Ground (Guided Nationalist Reading)
 *   domain: constitutional/political_transitions
 *
 * SUMMARY:
 *   A post-revolutionary constituent process produced a charter whose
 *   legitimacy clause anchors sovereign authority in the nation's religious
 *   identity: the state speaks constitutionally as the political expression
 *   of a faith-defined people, religious norms receive binding constitutional
 *   status, and the machinery of the new republic — courts, education, family
 *   law, party registration — operates inside that ground. This story
 *   instantiates one reading of the charter as a single epsilon-invariant
 *   constraint: the standing arrangement under contest is the
 *   charter-as-religious-legitimacy-settlement, and extractiveness is
 *   authored for that arrangement as this reading assesses it, never for any
 *   alternative settlement the reading's opponents would prefer. The
 *   settlement solves a real founding problem — the revolution destroyed the
 *   old order's legitimacy without leaving an agreed replacement — while
 *   imposing identifiable, asymmetric costs on those outside the
 *   faith-defined nation. The claimed type and the metrics are authored
 *   independently: the claim records the structure I believe true (a genuine
 *   coordination function joined to asymmetric extraction, held up by active
 *   enforcement); the metrics record the operation I believe descriptively
 *   accurate.
 *
 * KEY AGENTS:
 *   - islamist_nationalist_coalition: agenda-setter and principal collector (institutional/arbitrage) — wrote the legitimacy ground, governs through it, and can revise it
 *   - religious_establishment: constitutional-status beneficiary (organized/constrained) — collects juridical authority bound to the settlement
 *   - majority_faith_citizens: identity-coordination beneficiary carrying diffuse costs (moderate/constrained)
 *   - secular_civil_society: primary target (organized/constrained) — bears political exclusion and registration disadvantage
 *   - religious_minorities: primary target (powerless/constrained) — bears legal disability and qualified protection
 *   - private_unbelievers_in_majority_community: identity-locked bearer (powerless/identity_locked) — carries the constraint internally as permanent performed observance
 *   - expatriate_dissident_networks: excluded voice (organized/mobile) — contests the clause from outside the conversation
 *   - international_rights_monitors: analytical observer (institutional/analytical) — documents and conditions aid without a vote inside the order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.62).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter Religious-Identity Legitimacy Ground (Guided Nationalist Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '72032301-16c0-4a8a-8165-618573ac9b17').
narrative_ontology:cs_kernel_codification('72032301-16c0-4a8a-8165-618573ac9b17', fixed_text).
narrative_ontology:cs_authority_grounding('72032301-16c0-4a8a-8165-618573ac9b17', lineage).
narrative_ontology:cs_interpretation_layer_present('72032301-16c0-4a8a-8165-618573ac9b17').
narrative_ontology:cs_reading_relation('72032301-16c0-4a8a-8165-618573ac9b17', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('72032301-16c0-4a8a-8165-618573ac9b17', july_charter_sovereign_legitimacy__military_custodian_reading, influences).
narrative_ontology:cs_axiom('72032301-16c0-4a8a-8165-618573ac9b17', foundational, religious_identity_constitutes_sovereign_authority).
narrative_ontology:cs_axiom_status(religious_identity_constitutes_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('72032301-16c0-4a8a-8165-618573ac9b17', religious_identity_constitutes_sovereign_authority, theological).
narrative_ontology:cs_axiom('72032301-16c0-4a8a-8165-618573ac9b17', secondary, religious_norms_bind_state_lawmaking).
narrative_ontology:cs_axiom_status(religious_norms_bind_state_lawmaking, holdable).
narrative_ontology:cs_axiom_grounding('72032301-16c0-4a8a-8165-618573ac9b17', religious_norms_bind_state_lawmaking, conventional).
narrative_ontology:cs_reference_frame('72032301-16c0-4a8a-8165-618573ac9b17', faith_grounded_national_sovereignty).
narrative_ontology:cs_drift_state('72032301-16c0-4a8a-8165-618573ac9b17', contemporary_post_ratification, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('72032301-16c0-4a8a-8165-618573ac9b17', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_nationalist_coalition).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, majority_faith_citizens).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, private_unbelievers_in_majority_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, majority_faith_citizens).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, divine_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, national_unity_through_shared_faith).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Led the constituent process after the revolution and wrote the legitimacy clause anchoring constitutional authority in the nation's religious identity. Controls the implementing legislation, the amendment machinery, and the appointment pipeline for courts that administer the religious-norm clauses. Governing authority flows to it through the clause: challenges to its rule become framed as challenges to the constitutional ground itself. It can revise the settlement through supermajorities it largely commands, so its exit from any particular provision is open even as it locks others in.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_nationalist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_nationalist_coalition, beneficiary).

% Ulama councils, fatwa boards, and faith-run educational bodies receive constitutional status, formal advisory and juridical roles, state funding channels, and veto points in curriculum and family law under the charter. Their rulings gain state recognition they did not previously hold. Their institutional authority is bound to the settlement that recognizes them; losing that recognition would return them to purely social standing.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment, beneficiary,
    organized, generational, constrained, national).

% Receive affirmation of collective identity: a state that speaks in their idiom, public observance of their calendar, schooling that transmits their tradition, and a constitutional text that names them as the nation. They also carry diffuse costs: enforcement spending, the loss of pluralist safeguards that once protected dissenters of every kind, and exposure to a state whose power is harder to check because criticism can be reframed as impiety and prosecuted.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, majority_faith_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, majority_faith_citizens, payer).

% Student federations, labor unions, women's movement organizations, and rights associations that drove the revolution find their platforms constitutionally marginal. Parties must accommodate the religious-identity clauses to register; speech touching the legitimacy ground carries legal risk; funding and registration rules favor faith-aligned organizations. Exiting means ceding the domestic field to the settlement's supporters; staying means operating inside constraints their opponents wrote.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    organized, biographical, constrained, national).

% Communities outside the majority faith hold constitutional status as permanent outsiders: equal-protection guarantees are qualified by the religious-identity clauses, family-law matters fall under majority-faith jurisdiction, property and worship disputes are adjudicated by courts instructed in the legitimacy ground, and episodes of communal violence meet lenient treatment. Emigration is physically possible but means abandoning homes, livelihoods, and graves; most stay and absorb the costs.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, constrained, regional).

% Citizens of majority-faith background who privately disbelieve cannot say so. Apostasy and blasphemy exposure, marriage and inheritance consequences, and family rupture make open disbelief ruinous, so the constraint is carried internally: silence and performed observance maintained continuously, including where no state actor is watching. Their exit from the identity is socially unavailable even where physical relocation would be possible.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, private_unbelievers_in_majority_community, payer,
    powerless, biographical, identity_locked, local).

% Emigrated activists, exiled journalists, and diaspora legal campaigns contest the legitimacy clause from abroad. They are outside the ratification conversation entirely, and their arguments enter domestic discourse only at the risk of foreign-agent prosecution for those who repeat them. They publish, litigate in foreign forums, and lobby foreign governments, but hold no seat in the constitutional order they contest.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, expatriate_dissident_networks, excluded,
    organized, generational, mobile, global).

% Treaty bodies, special rapporteurs, and foreign ministries review the charter's conformity with minority-rights and free-expression commitments. They document patterns, publish findings, and condition aid, but hold no vote inside the constitutional order they assess and cannot themselves amend the legitimacy clause.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_rights_monitors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_nationalist_coalition).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-revolutionary legitimacy problem: the revolution destroyed the old constitutional order without leaving an agreed replacement, and the new state needed a source of authority that depended neither on the ousted regime nor on a fragile procedural consensus. Anchoring sovereignty in the nation's shared religious identity supplied a pre-political ground, mass legitimacy for the transition, and legal continuity for courts and administration that had to keep operating while the founding dispute remained unresolved.
% TRANSFER_FUNCTION: Moves constitutional authority and juridical status toward the religious establishment and the governing coalition; moves political exclusion, legal disability, and social pressure onto secular civil society, religious minorities, and private dissenters within the majority community.
% ABSENT_VOICES: Religious minorities and secular organizers were marginalized during ratification: consulted late, outvoted on the legitimacy clause, and absent from the drafting committee where its language was fixed. Expatriate dissidents were outside the process entirely. Their objection — that a legitimacy ground which excludes them is not their constitution — never entered the room where the settlement's unanimity was recorded.
% DISAPPEARANCE_RATIONALE: If the legitimacy clause vanished overnight, the state's authority claims would lose their ground: courts administering religious-norm jurisdictions would lose their jurisdictional warrant, the governing coalition's equation of criticism with impiety would fail, and the constitutional order would reopen into precisely the founding dispute the charter was built to close. Institutions from family courts to education ministries would require re-founding on some other basis.
% FOUNDING_PROBLEM: The revolution removed the old order's legitimacy without producing agreement on what would replace it; the charter was built to close a sovereign-legitimacy vacuum and prevent counter-revolutionary fragmentation of the state.
% FOUNDING_PROBLEM_CORROBORATION: Independent jurists and international constitutional monitors, sitting outside the benefiting parties, corroborate that the legitimacy vacuum was real at adoption and that some ground had to be chosen. Minority-rights organizations and secular parties attest from their seats that the acute phase of the vacuum has narrowed into ordinary political contestation while the clause persists — supporting a contested status rather than either the coalition's claim that the emergency persists intact or a complacent finding that the problem is simply dead.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the costs are real and asymmetric — legal disability, registration disadvantage, qualified equal protection — but ride on a structure that also delivers a functioning legitimacy settlement, so the arrangement is not pure rent collection. Suppression is higher (0.68) than extraction because persistence depends on active machinery: registration regimes, speech liability around the legitimacy ground, majority-faith family-law jurisdiction, and prosecution of foreign-linked advocacy; the constraint does not hold by consent alone. Theater is moderate-low (0.30): the juridical and coordinative functions genuinely operate, but a growing share of activity is ritualized piety — ceremonial observance, loyalty oaths, commemorative politics — that maintains the settlement's appearance as mobilization energy fades. Accessibility collapse is 0.50: alternatives do not vanish as they would under natural law — emigration, underground organizing, and external advocacy remain reachable — but each carries severe cost, so alternatives are degraded rather than eliminated. Resistance is 0.55: sustained secular and minority mobilization keeps contesting the clause rather than acquiescing. The three temporal series run on one shared seven-point grid so every metric is authored at every examined time point; extractiveness and suppression rise together as enforcement machinery matured, and theater creeps upward as performance substitutes for mobilization. Suppression is authored as a raw structural property throughout — it is not scaled by power or scope; only extractiveness is scaled downstream.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently, and the structural data is built to let the engine produce that divergence. From the coalition's position the settlement is a founding achievement it authored and defends: coordination it built, authority it legitimately holds, revision levers it controls. From the secular and minority seats the identical clause operates as enforced exclusion: the same text that grounds the coalition's authority strips theirs. The majority-public seat sits between — genuine identity benefit, diffuse and growing cost — and should compute nearer symmetric than either pole. The religious establishment computes as a concentrated beneficiary with constrained exit: its authority exists only inside the settlement. No authored claim adjudicates these differences; they are computed per seat from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   The coalition is declared both agenda-setter and beneficiary: it defines the ground, collects authority through it, and holds arbitrage-grade exit (it can amend what it wrote), placing it near the full-beneficiary end. The religious establishment is a declared beneficiary with constrained exit — its d sits low but its lock-in is real. Majority-faith citizens are declared beneficiaries, but the derivation from 'beneficiary + constrained exit' alone would read them near d approximately 0.2, which misdescribes their position: the same settlement that subsidizes their identity also dismantled the pluralist safeguards that protected them, concentrated power that can turn on any dissenter, and taxes them through enforcement. The directionality override for the moderate power atom corrects this to 0.42 — nearer symmetric — because the declaration structure cannot see costs that fall on a beneficiary diffusely. Secular civil society, religious minorities, and private unbelievers are declared victims with constrained or identity-locked exit, placing them near the full-target end; identity lock-in pushes the private unbeliever seat furthest, since their constraint travels with them regardless of location. Scope is national for most seats, which modestly amplifies effective extraction through verification difficulty; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a sovereign-legitimacy vacuum after revolutionary destruction of the old order — was real and is corroborated from outside the benefiting parties, but its status is contested: the acute phase has narrowed into ordinary politics while the clause persists at full strength. This classification prevents two opposite mislabelings. Reading the settlement as pure extraction ignores that the coordination function was genuine: mass legitimacy, legal continuity, and a workable founding ground were delivered, and the majority public still collects identity value from the arrangement. Reading it as pure coordination ignores the identifiable victim set and the enforcement dependence the suppression series documents. The temporal data is the drift instrument: if the founding problem dies outright while base_extractiveness and suppression continue climbing, the arrangement is drifting snare-ward — coordination cover over entrenched extraction; if enforcement decays while ceremony expands, theater_ratio growth marks piton-ward decay into performed legitimacy. Theater at 0.30 and rising slowly is consistent with early substitution, not yet dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'The charter''s legitimacy clause underdetermines its own ground: does the text actually fix religious identity as the source of sovereign authority, or does it admit the secular-democratic and military-custodian readings as equally textual?',
    'Observe which reading''s implementing statutes pass, survive judicial review, and get enforced: the reading whose provisions actually operate is the reading the constraint instantiates. Cross-check against drafting-history records and the amendment votes that followed adoption.',
    'If a sibling reading prevails, this constraint''s victim set relocates (under the secular reading, the paying seats become the religious establishment and faith-aligned parties), epsilon is re-authored, and the classification is recomputed for that reading''s file — this story''s values do not transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the legitimacy clause the operative constitutional order actually instantiates.').

omega_variable(
    legitimacy_vacuum_persistence,
    'Is the founding legitimacy vacuum the charter was built to close still live, or has ordinary political contestation replaced it while the clause persists as an entrenchment instrument?',
    'Behavioral and survey evidence on whether challenges to the government are still processed as constitutional-or-impiety questions versus ordinary policy disputes, and whether repeal proposals treat the clause as load-bearing or as removable.',
    'If the vacuum is dead and the clause persists, the arrangement is drifting from coordination toward entrenched extraction — the mismatch flag fires and the snare-ward drift in the temporal series becomes the primary reading; if the vacuum is live, the elevated suppression is partially the price of the founding settlement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vacuum_persistence, empirical, 'Whether the founding problem survives or the clause now serves entrenchment.').

omega_variable(
    majority_net_benefit_question,
    'Does the majority-faith public net-benefit from the settlement, or do the dismantled pluralist safeguards and uncheckable concentrated authority impose costs on it that rival the identity subsidy?',
    'Longitudinal tracking of majority-background dissenters: prosecution rates, treatment of intra-majority criticism, and whether majority members who oppose the coalition receive the protections the settlement promises believers.',
    'If costs dominate, the majority seat''s directionality rises above 0.5, the coordination-function gate weakens, and the arrangement moves from tangled-rope toward snare with a broadened victim set; if benefits hold, the current hybrid classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_net_benefit_question, conceptual, 'Whether the identity subsidy or the lost safeguards dominate for the majority public.').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the suppression borne by private unbelievers in the majority community is structural (statutory liability, family-law consequence) and how much is internalized (self-censorship and performed observance that would persist if the statutes were repealed)?',
    'Post-repeal or post-decriminalization speech trajectories in comparable jurisdictions: if disbelief declarations remain rare after legal exposure ends, the internalized component dominates; if declarations surge, the structural component dominated.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — targets carry the constraint with them after any formal liberalization, and exit-option assessments based on statute books alone will understate lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression split for the identity-locked seat.').

omega_variable(
    minority_exit_elasticity,
    'How elastic is minority emigration in response to intensifying enforcement — does rising exit act as a safety valve that stabilizes the settlement at lower suppression, or does it strip the polity of pluralist capacity and harden the remaining population''s trap?',
    'Census and emigration-flow data correlated with enforcement-intensity milestones from the temporal series; remittance and diaspora-growth patterns as leading indicators.',
    'High elasticity predicts stabilization of the current hybrid profile; low elasticity converts the minority seat toward trapped-target treatment, amplifying effective extraction and pushing the computed classification snare-ward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_exit_elasticity, empirical, 'Whether minority exit relieves or intensifies the extraction dynamic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement_basis(july_tr_t4, observed).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(july_tr_t8, observed).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(july_tr_t12, observed).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(july_tr_t16, observed).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(july_tr_t20, observed).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(july_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(july_be_t4, observed).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(july_be_t8, observed).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(july_be_t12, observed).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(july_be_t16, observed).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(july_be_t20, observed).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(july_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement_basis(july_su_t4, observed).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(july_su_t8, observed).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement_basis(july_su_t12, observed).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(july_su_t16, observed).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(july_su_t20, observed).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(july_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the July Charter establishes' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints corresponding to the three readings of the legitimacy clause. Each member has its own epsilon, beneficiary/victim structure, and classification; measuring the clause one way yields a low-extraction settlement and another way a high-extraction one precisely because they are different constraints sharing one text. This file is the guided-nationalist instantiation; the secular-democratic and military-custodian files are linked here as siblings. The readings compete for the same clause rather than stacking: whichever reading's implementing statutes prevail determines which constraint actually operates, so each file's network edge records the contest rather than a causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, moderate, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
