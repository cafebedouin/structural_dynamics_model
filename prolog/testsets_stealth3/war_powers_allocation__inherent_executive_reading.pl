% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War-Initiation Authority (Commander-in-Chief Reading)
 *   domain: constitutional law / separation of powers / war powers
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the war_powers_allocation kernel:
 *   the inherent executive reading, under which the commander-in-chief clause
 *   is taken to confer deployment authority that does not depend on prior
 *   legislative sanction. The standing arrangement under contest is the
 *   operating practice built on that reading: presidents initiate force on
 *   self-certified authority, report to Congress after commitment when
 *   reporting is useful, and treat subsequent appropriations as confirmation.
 *   The reading's own framing presents the arrangement as constitutional
 *   design working as intended; the authored metrics describe the
 *   arrangement's actual operation — genuine rapid-response coordination
 *   carrying a substantial transfer of initiation discretion away from the
 *   deliberative branch, with checking instruments foreclosed by standing
 *   doctrine and by the political pricing of defunding committed forces.
 *   Claim and metrics are authored independently; the engine computes
 *   per-seat classifications from the structural data. Sibling readings
 *   (congressional primacy, functional accommodation) are separate
 *   constraints in the same family and are neither described nor averaged
 *   here. KEY AGENTS (by structural relationship): - the_presidency: Primary
 *   beneficiary and agenda-setter (institutional/arbitrage) — initiates
 *   deployments on self-certified authority; collects initiation discretion
 *   and deferred accountability - congress_as_institution: Primary target
 *   (institutional/trapped) — declaration and purse powers reduced to
 *   post-commitment ratification; checking tools foreclosed -
 *   deployed_service_members: Target (powerless/trapped) — bear operational
 *   risk from campaigns never deliberated; their exposure anchors the funding
 *   lock-in - taxpayers: Target (moderate/constrained) — bear post-hoc fiscal
 *   costs through supplementals and debt - national_security_bureaucracy:
 *   Secondary beneficiary (institutional/constrained) — gains operational
 *   flexibility and ratified budget flows - federal_courts: Enforcement seat
 *   (institutional/analytical) — maintains the arrangement by turning away
 *   challenges on standing and political-question grounds -
 *   affected_host_populations: Excluded voice (powerless/trapped) — lives
 *   where operations land; no channel into the initiation decision
 *
 * KEY AGENTS:
 *   - the_presidency: Primary beneficiary and agenda-setter (institutional/arbitrage) — initiates deployments on self-certified authority; collects initiation discretion and deferred accountability
 *   - congress_as_institution: Primary target (institutional/trapped) — declaration and purse powers reduced to post-commitment ratification; checking tools foreclosed
 *   - deployed_service_members: Target (powerless/trapped) — bear operational risk from campaigns never deliberated; their exposure anchors the funding lock-in
 *   - taxpayers: Target (moderate/constrained) — bear post-hoc fiscal costs through supplementals and debt
 *   - national_security_bureaucracy: Secondary beneficiary (institutional/constrained) — gains operational flexibility and ratified budget flows
 *   - federal_courts: Enforcement seat (institutional/analytical) — maintains the arrangement by turning away challenges on standing and political-question grounds
 *   - affected_host_populations: Excluded voice (powerless/trapped) — lives where operations land; no channel into the initiation decision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.72).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.58).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War-Initiation Authority (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional law / separation of powers / war powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '887743ab-85a5-47a1-be16-92550bd6a550').
narrative_ontology:cs_kernel_codification('887743ab-85a5-47a1-be16-92550bd6a550', fixed_text).
narrative_ontology:cs_authority_grounding('887743ab-85a5-47a1-be16-92550bd6a550', lineage).
narrative_ontology:cs_interpretation_layer_present('887743ab-85a5-47a1-be16-92550bd6a550').
narrative_ontology:cs_reading_relation('887743ab-85a5-47a1-be16-92550bd6a550', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('887743ab-85a5-47a1-be16-92550bd6a550', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('887743ab-85a5-47a1-be16-92550bd6a550', foundational, force_initiation_authority_inheres_in_executive_office).
narrative_ontology:cs_axiom_status(force_initiation_authority_inheres_in_executive_office, holdable).
narrative_ontology:cs_axiom_grounding('887743ab-85a5-47a1-be16-92550bd6a550', force_initiation_authority_inheres_in_executive_office, conventional).
narrative_ontology:cs_axiom('887743ab-85a5-47a1-be16-92550bd6a550', secondary, appropriations_after_commitment_constitute_ratification).
narrative_ontology:cs_axiom_status(appropriations_after_commitment_constitute_ratification, holdable).
narrative_ontology:cs_axiom_grounding('887743ab-85a5-47a1-be16-92550bd6a550', appropriations_after_commitment_constitute_ratification, instrumental).
narrative_ontology:cs_reference_frame('887743ab-85a5-47a1-be16-92550bd6a550', inherent_commander_in_chief_authority).
narrative_ontology:cs_drift_state('887743ab-85a5-47a1-be16-92550bd6a550', contemporary_post_2001_aumf_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('887743ab-85a5-47a1-be16-92550bd6a550', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, the_presidency).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_bureaucracy).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress_as_institution).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, deployed_service_members).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, taxpayers).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, curtiss_wright_foreign_affairs_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates military operations on its own certification of constitutional authority, relying on Justice Department opinions that prior legislative sanction is unnecessary for operations short of declared war. Reports to Congress after commitment when reporting serves the operation, and treats subsequent funding votes as confirmation of the action taken. Stepping outside the arrangement would mean voluntarily submitting initiation decisions to pre-clearance — an option no administration has taken.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_presidency, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, the_presidency, beneficiary).

% Holds the formal powers of the purse and of war declaration. In practice, once forces are committed, funding votes are framed as tests of support for troops in harm's way, and chamber majorities for limiting resolutions dissolve against vetoes and procedural hurdles. Litigation to assert its prerogatives has been turned away on standing and political-question grounds. Its checking instruments remain on paper; using them carries costs that rise the more forces are exposed.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress_as_institution, payer,
    institutional, generational, trapped, national).

% Receive deployment orders and carry out operations whose initiation they played no part in deliberating. Bear injury, death, and long-term health consequences. Their exposure then becomes the argument against cutting funds — withdrawing support is framed as abandoning them — which binds them more tightly to a commitment they did not choose.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, deployed_service_members, payer,
    powerless, biographical, trapped, global).

% Fund operations through supplemental appropriations and borrowing after commitments are already made. Have no direct lever over initiation decisions; their influence arrives as general-election choices between administrations, filtered through years. Long-run costs land as debt service carried across generations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, taxpayers, payer,
    moderate, generational, constrained, national).

% Plans and executes operations sized to whatever authorities the executive certifies for itself. Gains operational flexibility and predictable budget flows when commitments are ratified after the fact. Adapts planning to the absence of pre-clearance rather than pressing for it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_bureaucracy, beneficiary,
    institutional, generational, constrained, global).

% Turn away suits by legislators and veterans challenging operations begun without prior sanction, citing standing, ripeness, and political-question doctrines. Publish no contrary holdings that would force the issue. Their abstention is the quiet half of the arrangement's upkeep: each dismissed challenge lowers the price of the next unilateral action.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Live where operations occur. Experience strikes, raids, and advising missions decided entirely inside the executive branch. Have no seat in any deliberative forum of the deploying state and no channel through which their objection to being made operational terrain reaches the decision.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, affected_host_populations, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, the_presidency).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Places a single decision-maker over the armed forces so that response to sudden attack and fast-moving crises does not wait on assembling a deliberative quorum; unified command also resolves the operational problem of split authority in the field.
% TRANSFER_FUNCTION: Moves initiation discretion from the legislature to the executive; moves the timing of democratic consent from before commitment to after it, via appropriations; moves the risks of error — casualties, open-ended entanglement, debt — onto seats excluded from the initiation decision.
% ABSENT_VOICES: Deployed service members beyond chain-of-command channels, populations living where operations land, and legislators whose checking instruments have been procedurally foreclosed would object to initiation without prior sanction. They are absent because the interpretive loop that certifies authority runs entirely inside the executive branch, and court access has been closed on standing grounds.
% DISAPPEARANCE_RATIONALE: Ongoing and future operations would immediately require prior congressional sanction; forces in the field would face funding uncertainty at the next supplemental; the executive's first-mover advantage in crises would disappear; and decades of compounding practice precedent would stop accumulating. Allies and adversaries alike would reprice the deploying state's ability to act quickly.
% FOUNDING_PROBLEM: The early republic could not answer sudden attacks — frontier raids, maritime seizure — if defense waited on a dispersed deliberative assembly. The 1787 frame gave the executive command of the armed forces while reserving war declaration to Congress; this reading extends that command into initiation itself, on the strength of the founding debates' arguments for energy, dispatch, secrecy, and decision in foreign defense.
% FOUNDING_PROBLEM_CORROBORATION: For the narrow sudden-attack core, the founding record itself attests: Convention debates rejecting an executive initiate-force grant, and Federalist 69's contrast between the president's command and Congress's declaration power, are corroborated by constitutional historians outside the benefiting parties. For the broad discretionary-interests extension this reading now operates, no corroborating source outside the benefiting parties exists — the operative witnesses are executive-branch legal opinions and post-commitment appropriations votes cast by legislators whose alternatives had already been foreclosed.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.72 because the arrangement moves initiation discretion wholesale to the executive and converts the legislature's sanction into a post-commitment formality, while the costs of error — casualties, entanglement, debt — land on seats excluded from the decision. Suppression (0.58) is moderate and structural: standing and political-question doctrine close the litigation route, and once forces are committed, defunding is priced as abandonment. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater (0.44) reflects real operations wrapped in growing ritual: after-the-fact notifications styled as consultation, sixty-day clocks managed by redefining hostilities, and legacy authorizations stretched over mission sets they never named. Accessibility collapse (0.60): once the arrangement is understood, the ordinary checking alternatives — litigation, binding resolutions, timely funding cuts — are seen to be closed or prohibitively priced, though constitutional amendment and electoral turnover remain formally open. Resistance (0.55): the War Powers Resolution, repeated chamber-passed limitation attempts, and legislator lawsuits constitute persistent, organized, and consistently absorbed resistance. All three tracked series run on one shared time grid (1950, 1965, 1973, 1990, 2001, 2011, 2025) with every metric authored at every point. The series show a shock-recovery episode rather than a sustained cycle: the 1973 legislative counterattack briefly raised the price of unilateral action (extractiveness dips, suppression requirement peaks locally), and the arrangement recovered within two decades through appropriations-ratification and tightening justiciability doctrine — the recovery, not the shock, is the load-bearing phase. Extraction accumulation and an enforcement ratchet dominate the long run; theater growth concentrates after 2001 as mission sets outrun their authorizations.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as rightful constitutional design: initiation authority is the office's own, and post-hoc funding reads as affirmation rather than submission. The payer seats compute the same structure oppositely: for the legislature the sanction is a courtesy it cannot withhold; for service members the decision arrived without their deliberation and their exposure then hardens the funding lock; for taxpayers consent is requested only after the bill exists. Courts occupy a third position — from the abstention seat the arrangement is simply not an adjudicable question, which is precisely why it holds. The engine computes these divergences from power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the presidency and the security bureaucracy near the beneficiary end of d: the arrangement subsidizes both with discretion and ratified resources, and the presidency's arbitrage-grade position (act first, ratify later) sits it nearest zero. Victim declarations place congress_as_institution, deployed_service_members, and taxpayers near the target end; trapped position (no viable checking route, no opting out of deployment, no leaving taxation) pushes the legislature and service members toward the full-target pole, with service members — powerless and trapped — furthest. Taxpayers sit somewhat lower: constrained rather than trapped, with electoral turnover as a slow partial way out. Federal courts are enforcement-side: administering the arrangement through abstention places them near the beneficiary end despite collecting no rents. Affected host populations are excluded rather than coordinated, but their exposure to the arrangement's outputs is total; they enter the computation, if at all, as unseated targets. Scope amplification applies modestly: the arrangement operates globally, which raises verification difficulty and therefore effective extraction on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — answering sudden attack without waiting on a dispersed assembly — remains live in its narrow core, so this is not a dead-mandate zombie: the R5 mismatch check (status=contested x verdict=world_rearranges) raises no obsolescence flag. What has happened is accretion: a rapid-defense instrument has grown a discretionary interest-projection practice far larger than its founding warrant, with the warrant's narrowness kept visible mainly by sources outside the benefiting parties. The tangled-rope classification is what keeps both faces legible: reading the arrangement as pure extraction would erase the genuine coordination value of unified rapid command that every sibling reading also concedes; reading it as pure coordination would erase the documented conversion of legislative sanction into courtesy. Mandatrophy is not resolved — the mandate has not outlived its function so much as been extended past it — and the naturality omega tracks whether the 'inherent' framing is doing load-bearing rhetorical work in keeping that extension from being priced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_constructed_naturality,
    'Is commander-in-chief deployment authority a pre-existing attribute of sovereignty, as the reading''s ''inherent'' framing asserts, or a constructed allocation produced by practice precedent, executive-branch self-certification, and appropriations acquiescence?',
    'Founding-record analysis cross-checked against comparative constitutional study: parliamentary systems allocate force initiation to the legislature, and the 1787 Convention explicitly declined to grant the executive an initiate-force power. If the allocation replicates only under specific institutional conditions, it is constructed.',
    'If constructed, the arrangement is revisable by statute or amendment and the inherent framing is rhetorical cover that suppresses revision pressure; if genuinely inherent, statutory limits such as the War Powers Resolution are constitutionally inert and the measured resistance is misdirected by design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_constructed_naturality, conceptual, 'Whether the arrangement is a natural feature of executive sovereignty or a constructed, revisable allocation presented as natural.').

omega_variable(
    appropriations_ratification_validity,
    'Does post-commitment funding genuinely constitute ratification of an initiated operation, or is it coerced acquiescence — troops already exposed, with defunding priced as abandonment?',
    'Compare legislative voting on supplemental appropriations before versus after public commitment, and count operations funded that chambers had voted against initiating; independent legislative-history analysis outside the benefiting parties.',
    'If coerced, the ratification mechanism launders unilateral initiation as consent and the arrangement''s effective extraction exceeds the authored measure; if genuine consent, part of the transfer function is consensual and the payer seats'' directionality sits lower than modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_ratification_validity, empirical, 'Whether appropriations-after-commitment function as consent or as captured acquiescence.').

omega_variable(
    kernel_disagreement_location,
    'This constraint is one reading of the war_powers_allocation kernel. Across the three readings, is the operative disagreement located in the necessity of prior authorization, in the boundary of ''national interests,'' or in the enforceability of legislative limits — and would a sibling reading relocate the victim set and enforcement profile?',
    'Adoption tracing: when a governing coalition holds one reading, track which downstream allocations move — authorization rates, interest-scope definitions, and legislative-limit enforcement — to identify which structural element the reading actually controls.',
    'Mislocating the disagreement would attribute extraction to the wrong structural element: congressional_primacy_reading would shrink the victim set to the executive''s discretion alone; functional_accommodation_reading would bound ''national interests'' by imminence and split this story''s epsilon across contexts. This story authors the unbounded, ornamental-sanction variant as one clean epsilon-invariant constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Committer-frame routing: which structural element separates this reading from its siblings, and what each sibling would change.').

omega_variable(
    suppression_mechanism_split,
    'Is the arrangement''s suppression structural (justiciability doctrine, procedural veto points) or internalized (legislators'' self-restraint under the political norm that challenging committed forces equals abandoning them)?',
    'Observe checking-tool usage when the political price shifts: if legislators invoke funding leverage and litigation more readily as casualty aversion rises or operations grow unpopular, the binding is substantially internalized; if usage stays flat regardless of popularity, the binding is structural.',
    'If mostly internalized, jurisdictional or statutory reform alone will not restore legislative checking — the norm must shift first; if structural, procedural reform (standing rules, ripeness standards) is sufficient and the measured suppression overstates the arrangement''s durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Whether suppression is carried by external doctrine or by internalized political self-restraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_inherent_tr_t1950, war_powers_allocation__inherent_executive_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement(war_powers_inherent_tr_t1965, war_powers_allocation__inherent_executive_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(war_powers_inherent_tr_t1973, war_powers_allocation__inherent_executive_reading, theater_ratio, 1973, 0.36).
narrative_ontology:measurement(war_powers_inherent_tr_t1990, war_powers_allocation__inherent_executive_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(war_powers_inherent_tr_t2001, war_powers_allocation__inherent_executive_reading, theater_ratio, 2001, 0.26).
narrative_ontology:measurement(war_powers_inherent_tr_t2011, war_powers_allocation__inherent_executive_reading, theater_ratio, 2011, 0.4).
narrative_ontology:measurement(war_powers_inherent_tr_t2025, war_powers_allocation__inherent_executive_reading, theater_ratio, 2025, 0.44).

% Extraction over time
narrative_ontology:measurement(war_powers_inherent_be_t1950, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1950, 0.46).
narrative_ontology:measurement(war_powers_inherent_be_t1965, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1965, 0.56).
narrative_ontology:measurement(war_powers_inherent_be_t1973, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1973, 0.49).
narrative_ontology:measurement(war_powers_inherent_be_t1990, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1990, 0.57).
narrative_ontology:measurement(war_powers_inherent_be_t2001, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(war_powers_inherent_be_t2011, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2011, 0.7).
narrative_ontology:measurement(war_powers_inherent_be_t2025, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_inherent_su_t1950, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1950, 0.34).
narrative_ontology:measurement(war_powers_inherent_su_t1965, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1965, 0.41).
narrative_ontology:measurement(war_powers_inherent_su_t1973, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1973, 0.47).
narrative_ontology:measurement(war_powers_inherent_su_t1990, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1990, 0.51).
narrative_ontology:measurement(war_powers_inherent_su_t2001, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(war_powers_inherent_su_t2011, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2011, 0.57).
narrative_ontology:measurement(war_powers_inherent_su_t2025, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'war powers' covers one kernel (war_powers_allocation) with three structurally distinct readings, emitted as three stories. This file instantiates inherent_executive_reading only. The congressional_primacy_reading is the formal-text baseline (higher empirical confidence in the founding record; authorization as necessity); this reading is downstream of it in the sense that each appropriations-ratification episode consumes a foothold of the primacy claim's practical force without eliminating it as a position. The functional_accommodation_reading sits between: this reading's expansion of the 'imminent threat' exception exerts structural pressure on its context thresholds, pushing accommodation's rule-boundary outward. Each story carries its own epsilon, victim set, and enforcement profile; averaging across readings would fabricate a measurement parameter where three constraints exist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
