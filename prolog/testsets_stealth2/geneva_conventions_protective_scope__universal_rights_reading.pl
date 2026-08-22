% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Universal Protective Floor of the Geneva Conventions (Common Article 3 + Human Rights Law)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This story instantiates the universal_rights_reading of the kernel
 *   geneva_conventions_protective_scope: the claim that Geneva protections
 *   extend to every person affected by armed conflict regardless of combatant
 *   status, with Common Article 3 and international human rights law jointly
 *   forming an unconditional floor. The constraint under classification is
 *   that universal-floor arrangement itself — the standing arrangement under
 *   contest — and its ε is assessed by this reading's own lights: the floor
 *   extracts real operational costs from state military and intelligence
 *   apparatus while delivering protections to persons no sibling reading
 *   would cover. Structurally the arrangement coordinates (it removes the
 *   payoff to conflict-classification gaming and guarantees reciprocity
 *   coverage for all parties' captured personnel) while extracting
 *   asymmetrically (state operational flexibility pays; non-state fighters,
 *   detainees, and civilians collect), which is the tangled_rope structure
 *   claimed here. The claimed type and the authored metrics are independent
 *   facts: the metrics describe moderately extractive, actively enforced,
 *   partly theatrical operation; the engine computes each seat's type from
 *   the structural data, and divergence between the claim and any computed
 *   seat type is the measurement this corpus exists to take. Sibling readings
 *   are separate constraints with their own ε and are not averaged into this
 *   file.
 *
 * KEY AGENTS:
 *   - state_military_operational_commands: primary payer seat (institutional/constrained) — bears targeting, detention, and interrogation constraints; collects reciprocity coverage for its own captured personnel
 *   - state_intelligence_detention_services: secondary payer seat (institutional/constrained) — loses the practices the floor prohibits; gains almost no offset
 *   - non_state_armed_groups: primary beneficiary (organized/trapped) — members gain protections no classification can remove; bears unenforced reciprocal obligations
 *   - conflict_zone_civilian_populations: beneficiary (powerless/trapped) — protected regardless of how any party classifies the conflict
 *   - conflict_detainees: beneficiary (powerless/trapped) — the reading's clearest case; holds enforceable rights against the detaining state
 *   - humanitarian_organizations: beneficiary (organized/mobile) — expanded mandate and access; withdrawal leverage
 *   - international_courts_treaty_bodies: agenda_setter (institutional/analytical) — adjudicates the floor's content and reach; jurisdiction expands with the reading
 *   - nonstate_custody_detainees: excluded voice (powerless/trapped) — holds floor rights with no forum; their absence keeps enforcement asymmetric
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.55).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Universal Protective Floor of the Geneva Conventions (Common Article 3 + Human Rights Law)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'b0c0e764-05fc-44ed-af5a-f1c8df4bef95').
narrative_ontology:cs_kernel_codification('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', distributed).
narrative_ontology:cs_authority_grounding('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', distributed).
narrative_ontology:cs_reading_relation('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', foundational, protection_independent_of_combatant_status).
narrative_ontology:cs_axiom_status(protection_independent_of_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', protection_independent_of_combatant_status, deontological).
narrative_ontology:cs_axiom('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', foundational, ihrl_floor_persists_during_conflict).
narrative_ontology:cs_axiom_status(ihrl_floor_persists_during_conflict, holdable).
narrative_ontology:cs_axiom_grounding('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', ihrl_floor_persists_during_conflict, deontological).
narrative_ontology:cs_reference_frame('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', universal_protective_floor).
narrative_ontology:cs_drift_state('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', contemporary_conflict_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b0c0e764-05fc-44ed-af5a-f1c8df4bef95', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, conflict_zone_civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, conflict_detainees).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_organizations).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_commands).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_intelligence_detention_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_commands).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, martens_clause_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, common_article_3_minimum_humanity).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, extraterritorial_human_rights_jurisdiction).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, non_derogable_core_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commands state armed forces in armed conflict. Pays the floor's operational costs: targeting decisions must distinguish protected persons regardless of status, detention must meet humane-treatment and judicial-guarantee minimums, and interrogation is bounded by the non-derogable core. Also collects the floor's reciprocity side: its own captured personnel are covered by the same unconditional floor, whichever classification the adversary accepts. Exit would mean denouncing the treaty framework and absorbing the reputational, reciprocal, and domestic-litigation costs of operating outside it.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_commands, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_commands, beneficiary).

% Runs conflict-related detention and interrogation. The floor removes the practices its tradecraft was built around — status-based denial of protections, prolonged incommunicado detention, coercive interrogation — and exposes past practice to litigation and universal-jurisdiction prosecution. It gains little reciprocity: the persons it holds are its targets, not its personnel. Its leadership turns over faster than military commands, so it experiences the constraint as a compliance burden imposed mid-career.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_intelligence_detention_services, payer,
    institutional, biographical, constrained, global).

% Fight state forces without meeting formal combatant criteria. Under this reading their captured fighters hold floor protections no state can classify away, and their claims gain a legal vocabulary. They also bear the floor's obligations — Common Article 3 binds each party to the conflict — but no court enforces those obligations against them with the regularity that courts bind states. Their exit from the conflict itself is typically closed; they fight because the political situation gives them no alternative.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, biographical, trapped, regional).

% Live where the fighting happens. The floor extends to them regardless of how any party classifies the conflict, and gives them a claim against every party's conduct. They cannot leave the conflict zone at will and cannot enforce their claims directly; their protection runs through courts, treaty bodies, and humanitarian access they do not control.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, conflict_zone_civilian_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Held by state forces during armed conflict, including persons no party grants combatant status. This reading is what stands between them and status-based denial of protections: humane treatment, judicial guarantees, and a non-derogable core that no classification game removes. Their situation is the constraint's clearest case — they hold enforceable rights against the state that holds them and little else.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, conflict_detainees, beneficiary,
    powerless, immediate, trapped, regional).

% The ICRC and peer organizations receive expanded access and mandate from the floor's universality: a person's protection no longer depends on conflict classification, so their detention visits and protection work cover categories the state-centric reading would leave out. They can withdraw from a country when access is denied, which gives them leverage most protected persons lack.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_organizations, beneficiary,
    organized, generational, mobile, global).

% The ICJ, regional human rights courts, treaty bodies, and international criminal tribunals adjudicate the floor's content and reach: whether human rights law applies extraterritorially, what Common Article 3's minimum requires, whether derogations are lawful. Their judgments define the floor for the parties before them; they collect no material rent from its operation, but their jurisdiction and dockets expand with the reading's acceptance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_courts_treaty_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Persons held by non-state armed groups. The floor names them as rights-holders — Common Article 3 binds each party to the conflict — but the adjudicative conversation runs through institutions that bind states, and they have no forum in which to claim the floor against their captors. If present they would object that the reading's universality is enforced on one side only; their absence keeps the enforcement record asymmetric.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, nonstate_custody_detainees, excluded,
    powerless, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the classification-gaming and race-to-the-bottom problem in armed conflict: by attaching a minimum humane-treatment floor to every person in every conflict regardless of status or conflict classification, it removes the payoff to denying that a conflict exists, recharacterizing detainees, or exploiting gaps between international and non-international armed conflict regimes, and stabilizes reciprocity expectations — each party's captured personnel are covered whichever classification the adversary accepts.
% TRANSFER_FUNCTION: Moves operational latitude — targeting discretion, detention practice, interrogation method — from state military and intelligence apparatus toward protected persons (non-state fighters, detainees, civilians), and moves adjudication authority over conflict conduct from national chains of command toward international and regional courts and treaty bodies.
% ABSENT_VOICES: Persons detained by non-state armed groups hold floor rights on paper — Common Article 3 binds each party to the conflict — but the adjudicative conversation runs entirely through institutions that bind states, and they have no forum. If present they would object that the reading's universality is enforced on one side only. Military operational practitioners are also under-represented: the constraint is adjudicated by lawyers and judges, with the operational judgment being constrained present mostly as a respondent.
% DISAPPEARANCE_RATIONALE: If the universal floor vanished overnight, conflict parties would re-optimize around classification within months: states would recharacterize detainees and deny conflict existence where advantageous, detention and interrogation practice would drift toward the pre-floor baseline, non-state fighters would lose the only protections no classification can remove, and every state's guarantee for its own captured personnel would become contingent on winning the classification dispute. Detention law, targeting law, and the adjudicative dockets built on the floor would all rearrange.
% FOUNDING_PROBLEM: Status-based protection gaps: persons affected by armed conflict who fell outside every protected category — unprivileged belligerents denied prisoner-of-war status, civilians in non-international conflicts outside treaty scope — were tortured, disappeared, or summarily executed with no legal recourse. Common Article 3 (1949) and the human rights covenants (1966) were built to close exactly that gap by making a minimum floor attach to persons rather than to statuses.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the paying seats themselves: state military legal advisers' own wartime guidance acknowledges the status gaps the floor closes (the post-2001 detention debates turned on precisely which persons fell outside protected categories), and national courts in paying states have affirmed the gap's reality while contesting the floor's reach — the US Supreme Court in Hamdan v. Rumsfeld recognized Common Article 3's minimum protections as applicable where classification had excluded detainees. No corroborating source attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the floor's demands on state operations are real (status-independent targeting discrimination, humane-treatment and judicial-guarantee minimums in detention, a non-derogable interrogation core, litigation and universal-jurisdiction exposure) but bounded — the floor prohibits a narrow core rather than micromanaging operations, and this reading assesses that burden as the price of the humanitarian function rather than as parasitic on it. Suppression (0.58) reflects the enforcement machinery that keeps states from defecting — courts, treaty bodies, international tribunals, ICRC access, reputational and reciprocal costs of denunciation — substantial but incomplete in reach; suppression is authored as a raw structural property and is not scaled by scope or power. Theater ratio (0.38) splits the machinery: periodic reporting, derogation filings, and reservation practice are partly performative, while judicial enforcement and detention visits are functional. Accessibility collapse (0.45): classification games remain partially available alternatives — contesting that a conflict exists, recharacterizing detainees, derogating under emergency clauses — the floor collapses their payoff without eliminating the attempts. Resistance (0.60): sustained state pushback — reservations, derogation practice, non-recognition positions, non-cooperation with tribunals. All three temporal series are authored on one shared eight-point grid (1949–2026 at roughly eleven-year steps) so no metric is sampled against another's scalar; each series endpoint matches the corresponding base_properties value. The series show the reading's consolidation: extraction and enforcement capacity rising steeply through the jurisprudential consolidation era (Tadić, Hamdan, the extraterritoriality arc) and plateauing as the floor's content stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the protected seats should compute differently. From the military command seat the floor is a binding operational tax partially offset by reciprocity; from the intelligence/detention seat it is a direct prohibition of core practice with almost no offset; from the detainee and civilian seats it is the only enforceable protection that exists; from the adjudicative seat it is the jurisdictional foundation of the docket. Same text, same floor — structurally different constraints per seat, which the engine computes from power, exit, and role rather than from this story's claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (civilian populations, detainees, non-state armed groups, humanitarian organizations) drive those seats toward the beneficiary end of d: the floor subsidizes them, and their exit positions (trapped, or mobile-with-leverage for the humanitarian organizations) do not convert subsidy into exposure. Victim declarations (military commands, intelligence/detention services) drive those seats toward the target end: they bear the transfer, and their exit is constrained by customary status and domestic incorporation. Two refinements the structural data carries: the military seat holds secondary_role beneficiary (reciprocity coverage for its own captured personnel), pulling its d below the pure-target value; the intelligence seat gains no reciprocity, so it sits nearer full target. The agenda-setting adjudicative seat holds analytical exit and collects jurisdiction rather than material rent, placing it near symmetric-low. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the right structure without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Reading the floor as a snare (the state payer's complaint) erases the genuine coordination function: an unconditional floor is precisely what removes the payoff to classification gaming, and reciprocity coverage is a benefit the paying militaries themselves collect. Reading it as a pure rope (the humanitarian triumphalist reading) erases the real asymmetric extraction from state operational flexibility and the enforcement asymmetry that leaves non-state-custody detainees with paper rights. The founding problem — status-based protection gaps — remains live: every recent conflict regenerates classification disputes, so the arrangement's function has not atrophied and no mandatrophy is declared. Theater accretion (0.38, rising slowly across the interval) is a watch item, not a verdict: the reporting layer is drifting performative while the judicial core stays functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the universal_rights_reading of the kernel geneva_conventions_protective_scope; how would the constraint''s structure differ under the sibling readings?',
    'Compare against the sibling stories state_centric_reading and hybrid_proportionality_reading: the state-centric reading shrinks the protected set (unprivileged belligerents exit), lowering ε on state operations; the hybrid reading makes application contingent on conflict classification, re-enabling the classification games this reading''s floor closes.',
    'Under the state-centric reading the victim set shrinks toward the unprivileged-belligerent exclusion and state-seat extraction drops sharply; under the hybrid reading coverage becomes contestable per conflict, raising the accessibility of alternatives. This story''s ε and type are valid only for the universal reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel-reading position: one of three readings of the Geneva protective-scope kernel.').

omega_variable(
    asymmetric_enforcement_universality,
    'Does the universal floor bind non-state armed groups in operation, or does it bind only states while non-state obligations remain unenforced on paper?',
    'Systematic data on non-state group compliance with Common Article 3 minimums, and prosecution records for violations committed by non-state parties, compared against state-seat enforcement rates.',
    'If enforcement is effectively state-only, the floor''s extraction concentrates on the state seats while protections for non-state-held persons remain partly theoretical — the universality claim weakens and the state seats'' effective extraction rises relative to the protected seats'' realized benefit, pushing the structure toward the snare end from the state seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_enforcement_universality, empirical, 'Whether the floor''s universality is real in operation or enforced asymmetrically against states only.').

omega_variable(
    customary_status_of_floor,
    'Has the universal floor attained customary international law status (possibly jus cogens at its core) such that it would survive treaty denunciation by any state?',
    'Practice and opinio juris surveys of the ICRC customary-IHL type; state reactions to attempted denunciation or wholesale derogation; whether any state has successfully exited the floor''s core in practice.',
    'If the floor is customary, suppression is higher than treaty-text analysis suggests and exit via denunciation is closed, pushing the state seats'' directionality toward full target; if not, persistence depends on continued consent and the arrangement is more rope-like than the current metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_of_floor, empirical, 'Customary-law status of the universal floor and its consequence for exit options.').

omega_variable(
    reciprocity_offset_for_state_seats,
    'Do state militaries'' reciprocity gains — their own captured personnel covered unconditionally, whichever classification the adversary accepts — offset their operational costs enough to lower their directionality below the victim-derived value?',
    'Comparative analysis of state practice: states invoking the floor''s protections for their own captured personnel while denying it to adversaries; military legal-adviser testimony on the reciprocity guarantee''s operational value.',
    'If reciprocity materially offsets costs, the military seat''s d drops toward symmetric and the state seats'' computed type softens toward rope; if not, the seat sits near full target and the tangled_rope claim rests mainly on the coordination function rather than on payer-side benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_offset_for_state_seats, conceptual, 'Whether reciprocity gains offset state military extraction, changing the payer seat''s directionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_universal_rights_tr_t0, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t0, observed).
narrative_ontology:measurement(geneva_universal_rights_tr_t11, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 11, 0.18).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t11, observed).
narrative_ontology:measurement(geneva_universal_rights_tr_t22, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 22, 0.22).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t22, observed).
narrative_ontology:measurement(geneva_universal_rights_tr_t33, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 33, 0.25).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t33, observed).
narrative_ontology:measurement(geneva_universal_rights_tr_t44, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 44, 0.28).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t44, observed).
narrative_ontology:measurement(geneva_universal_rights_tr_t55, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 55, 0.33).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t55, observed).
narrative_ontology:measurement(geneva_universal_rights_tr_t66, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 66, 0.36).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t66, observed).
narrative_ontology:measurement(geneva_universal_rights_tr_t77, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 77, 0.38).
narrative_ontology:measurement_basis(geneva_universal_rights_tr_t77, observed).

% Extraction over time
narrative_ontology:measurement(geneva_universal_rights_be_t0, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t0, observed).
narrative_ontology:measurement(geneva_universal_rights_be_t11, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 11, 0.28).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t11, observed).
narrative_ontology:measurement(geneva_universal_rights_be_t22, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 22, 0.33).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t22, observed).
narrative_ontology:measurement(geneva_universal_rights_be_t33, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 33, 0.36).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t33, observed).
narrative_ontology:measurement(geneva_universal_rights_be_t44, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 44, 0.42).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t44, observed).
narrative_ontology:measurement(geneva_universal_rights_be_t55, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 55, 0.52).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t55, observed).
narrative_ontology:measurement(geneva_universal_rights_be_t66, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 66, 0.53).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t66, observed).
narrative_ontology:measurement(geneva_universal_rights_be_t77, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 77, 0.55).
narrative_ontology:measurement_basis(geneva_universal_rights_be_t77, observed).

% Suppression requirement over time
narrative_ontology:measurement(geneva_universal_rights_su_t0, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t0, observed).
narrative_ontology:measurement(geneva_universal_rights_su_t11, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 11, 0.22).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t11, observed).
narrative_ontology:measurement(geneva_universal_rights_su_t22, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 22, 0.28).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t22, observed).
narrative_ontology:measurement(geneva_universal_rights_su_t33, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 33, 0.31).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t33, observed).
narrative_ontology:measurement(geneva_universal_rights_su_t44, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 44, 0.38).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t44, observed).
narrative_ontology:measurement(geneva_universal_rights_su_t55, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 55, 0.48).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t55, observed).
narrative_ontology:measurement(geneva_universal_rights_su_t66, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 66, 0.55).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t66, observed).
narrative_ontology:measurement(geneva_universal_rights_su_t77, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 77, 0.58).
narrative_ontology:measurement_basis(geneva_universal_rights_su_t77, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Geneva protections' covers three structurally distinct claims about protective scope, decomposed per the ε-invariance principle into a three-story constraint family sharing the kernel geneva_conventions_protective_scope. The state-centric reading is the historical baseline text reading (coverage by Article 4 status; unprivileged belligerents outside scope; low ε on state operations, no protection for the excluded). This universal reading consolidates through jurisprudence (Common Article 3 plus human rights law as unconditional floor; higher ε on state operations, protections for all). The hybrid proportionality reading scales standards by conflict type and makes application contingent on classification analysis. The upstream baseline reading is cited as evidence in downstream consolidation; the universal reading's jurisprudential consolidation in turn changes the legitimacy conditions under which hybrid classification-based denials operate. Each story carries its own ε, beneficiaries, victims, and claimed type; they are linked here rather than averaged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
