% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Freedom-of-Movement Reading of Border Legitimacy
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This story instantiates the freedom_of_movement_reading of the contested
 *   border_legitimacy kernel: freedom of movement is a human right, and
 *   border restrictions are presumptively illegitimate - restrictions carry a
 *   rebuttable burden of proof rather than riding a default entitlement to
 *   exclude. Per the epsilon-referent rule for kernel readings,
 *   extractiveness is authored for the STANDING ARRANGEMENT UNDER CONTEST -
 *   the existing admission-restriction and border-enforcement regime operated
 *   by destination states - assessed by this reading's own lights, never for
 *   the open-mobility arrangement the reading endorses. On that referent this
 *   reading scores the regime heavily extractive: mobility opportunity
 *   transfers from poor-world passport holders to rich-world holders and
 *   destination insiders; enforcement appropriations concentrate in the
 *   enforcement sector; a removable labor tier subsidizes employers; and -
 *   the reading's structural delta - the victim set extends to current
 *   citizens: displaced workers undercut by a legally subordinate labor pool,
 *   and welfare recipients channeled through multiplying status-verification
 *   checkpoints. Claimed type and metrics are authored independently: the
 *   claim is tangled_rope; the metrics describe heavy extraction under active
 *   enforcement, and the engine measures the divergence rather than the
 *   author reconciling it.
 *
 * KEY AGENTS:
 *   - destination_state_governments: agenda_setter + beneficiary (institutional/arbitrage) - sets admission rules and enforcement intensity, collects electoral and fiscal returns
 *   - border_enforcement_industry: fiscal beneficiary (institutional/identity_locked) - receives enforcement appropriations; mission-fused with continued growth
 *   - privileged_passport_holders: mobility beneficiary (powerful/arbitrage) - consumes the mobility the system reserves, bears none of its friction
 *   - employers_of_deportable_labor: labor-side beneficiary (organized/mobile) - buys below-documentation-rate labor whose availability enforcement secures
 *   - origin_state_governments: secondary beneficiary (institutional/constrained) - remittances and demographic pressure-valve, net of brain-drain costs
 *   - undocumented_migrants: primary payer (powerless/trapped) - lives inside the enforcement perimeter permanently
 *   - visa_denied_global_south_applicants: payer (powerless/trapped) - the never-apply majority bearing the largest aggregate foregone mobility
 *   - rejected_asylum_seekers: payer (powerless/trapped) - claims processed into return-to-danger or transit limbo
 *   - displaced_domestic_workers: citizen payer added under this reading (powerless/constrained) - competes across the legal divide it cannot organize across
 *   - welfare_dependent_citizens: citizen payer added under this reading (powerless/trapped) - bears status-verification burdens and service diversion
 *   - deterred_potential_migrants: excluded seat (powerless/trapped) - absent before any conversation begins
 *   - human_rights_treaty_bodies: analytical observer (institutional/analytical) - names violations, compels nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.8).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.85).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Freedom-of-Movement Reading of Border Legitimacy").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '8ac13c84-9293-4d6c-8b6b-e7fde06a8a32').
narrative_ontology:cs_kernel_codification('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', fixed_text).
narrative_ontology:cs_authority_grounding('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', lineage).
narrative_ontology:cs_interpretation_layer_present('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32').
narrative_ontology:cs_reading_relation('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', foundational, freedom_of_movement_is_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_human_right, holdable).
narrative_ontology:cs_axiom_grounding('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', freedom_of_movement_is_human_right, deontological).
narrative_ontology:cs_axiom('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', foundational, admission_restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(admission_restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', admission_restrictions_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', presumptive_freedom_of_movement).
narrative_ontology:cs_drift_state('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', contemporary_externalization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8ac13c84-9293-4d6c-8b6b-e7fde06a8a32', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, destination_state_governments).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, privileged_passport_holders).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, employers_of_deportable_labor).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, origin_state_governments).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, undocumented_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, visa_denied_global_south_applicants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, rejected_asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_dependent_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set admission categories, visa prices, quotas, and enforcement intensity, and administer asylum adjudication. Collect electoral returns from visible enforcement posture and fiscal headroom from a labor pool whose legal status suppresses wage demands. Their exit from the arrangement is repositioning: shifting enforcement outward to transit states, repricing visas, or adjusting rhetoric - the machinery of admission control stays theirs either way.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, destination_state_governments, beneficiary).

% Border agencies, detention operators, surveillance vendors, and patrol contractors receive appropriations scaled to enforcement volume: detention beds, drone hours, wall segments, case-processing backlogs. Revenue tracks enforcement intensity independently of migration outcomes, and the sector converts appropriations into expansion advocacy. Exit would mean mission dissolution - the agencies' statutory purposes and the vendors' booklists are built on continued enforcement growth.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry, beneficiary,
    institutional, biographical, identity_locked, national).

% Hold documents that open most borders visa-free. They purchase the mobility the system reserves - relocation on demand, study and retirement abroad, investor residencies - while bearing almost none of the queueing, refusal, or detention risk imposed on others. Exit is easy and upgrades their position further: a second passport or a residency-by-investment.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, privileged_passport_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Staff agriculture, construction, care work, and food processing with workers whose lawful presence hangs on continued employment. Pay runs below the documented-market rate because the alternative to accepting offered terms is removal. Exit is mobile - shift recruitment corridors, switch sectors, automate - and the workforce cannot withhold labor collectively without triggering the enforcement response.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, employers_of_deportable_labor, beneficiary,
    organized, immediate, mobile, regional).

% Export unemployment through emigration and receive remittance flows that reach double-digit GDP shares in several cases; consulates negotiate whatever protections they can for nationals abroad. Costs arrive as brain drain, diaspora politics, and dependence on flows they do not control. Their leverage over destination-state admission rules is thin, and they have no seat at the tables where those rules are written.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, origin_state_governments, beneficiary,
    institutional, generational, constrained, national).

% Live and work without authorization after crossing or overstaying. Wages sit below documented rates, injuries and abuses go unreported, and savings drain into smuggling debts; family separation compounds across years. Leaving means returning to the conditions that drove departure; staying means permanent exposure to arrest and removal.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, undocumented_migrants, payer,
    powerless, biographical, trapped, global).

% Mostly never migrate at all: applications refused, fees forfeited, interview slots unavailable. Their passports open a handful of doors against a hundred-plus for rich-world counterparts - a lifetime mobility gap measured in destinations, wages, and schooling options. No forum exists where their particular refusal is reviewed.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, visa_denied_global_south_applicants, payer,
    powerless, generational, trapped, global).

% Flee persecution, war, or state collapse and submit claims into progressively stricter adjudication - safe-third-country routing, externally processed hearings, accelerated rejection. Outcomes are return to danger or years parked in transit camps and legal limbo, with the next border always further away than the last.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, rejected_asylum_seekers, payer,
    powerless, biographical, trapped, continental).

% Compete in construction, agriculture, and service trades against a parallel workforce that can be paid less and cannot complain - undercutting wages precisely where displaced citizens cluster. Workplace organizing stalls because any joint action crosses the legal divide and invites site raids. They also fund the enforcement apparatus through general taxation.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers, payer,
    powerless, immediate, constrained, national).

% Access benefits through status-verification checkpoints that multiply documentation requirements at every renewal. Public money shifts from services toward enforcement and eligibility auditing, and their bargaining position erodes as the subordinate labor tier expands in the adjacent markets they depend on.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_dependent_citizens, payer,
    powerless, immediate, trapped, national).

% Never apply. Price, risk, and rumor stop them before any official interaction - the largest population the arrangement touches and the least visible. They appear in no hearing, no applicant dataset, no consular queue; their foregone movement registers only as absence.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, deterred_potential_migrants, excluded,
    powerless, generational, trapped, global).

% Monitor state practice against movement and exile provisions, publish general comments and country findings, and receive individual complaints. They can name violations and mobilize shame; they cannot compel admission, reopen a closed route, or redirect an enforcement budget.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: States and travelers need shared instruments for crossing jurisdictions legibly and safely: standardized identity documents, visa adjudication, health screening, security vetting, and orderly claim processing solve real collective-action problems that unilateral improvisation between hundreds of states would worsen.
% TRANSFER_FUNCTION: Moves access to territory, labor markets, and life opportunity from nationals of poor-world states toward destination insiders and privileged passport holders; moves enforcement expenditure from general taxpayers into the enforcement sector; delivers a legally subordinate, removable workforce to employers at below-documented rates.
% ABSENT_VOICES: Would-be migrants deterred before applying, the dead and disappeared en route, and sending-community residents have no seat where admission policy is negotiated; their interests are voiced secondhand by NGOs and treaty bodies, which destination governments discount as interested advocacy.
% DISAPPEARANCE_RATIONALE: Overnight removal of admission restriction and its enforcement would reprice labor markets on both ends of the corridors, reroute remittance economies, alter destination demographics and welfare-state eligibility within years, collapse the enforcement sector's revenue base, and dissolve the passport-privilege gradient - the arrangement organizes too many flows for the world to stay put.
% FOUNDING_PROBLEM: Wartime control of subversive circulation and the management of post-WWI mass displacement produced the modern passport-and-visa regime (standardization circa 1914-1920); Cold War bloc closure and decolonization-era flows entrenched and expanded it.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the passport system, working outside the destination-state beneficiary seats, document the regime's emergency origins and its persistence past them; UNHCR and treaty-body archives attest that displacement management, not unconstrained sovereignty defense, motivated successive expansions. Destination-state ministries attest the contrary - that security and absorptive-capacity problems remain live. Outside corroboration therefore supports the 'contested' verdict, with the emergency-origin reading independently documented.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.80: on this reading's referent the standing regime transfers the single largest asset the global poor hold - the option to move - to insiders, at rising price (visa fees, smuggling debts, route mortality), with the mobility gap widening across the interval. Suppression 0.85 is authored as a RAW STRUCTURAL property, unscaled by power or scope: carrier sanctions, externalized processing, detention capacity, and deterrence messaging are the load-bearing wall; the regime must actively close exits (irregular channels, overstays, asylum claims) that remain materially open, which is also why accessibility_collapse sits mid-range (0.5) rather than mountain-high - alternatives degrade but persist, and persistent alternatives are exactly what sustained suppression exists to police. Theater_ratio 0.40: documentation, screening, and adjudication are real functions, but a growing share of enforcement activity is staged for domestic audiences (wall segments, televised removal flights, backlog announcements) - the series rises with enforcement budgets outpacing crossing volumes. Resistance 0.65: sanctuary networks, abolitionist campaigns, search-and-rescue NGOs, litigation against pushbacks, and the smuggling counter-institution all contest the regime continuously. The three series share one time grid (points 0-30 in years, roughly the post-Cold War externalization era); trajectories are ratchet-shaped, not cyclical - each enforcement intensification persists into the next phase rather than oscillating. Receipt surface: gain_flow names border_enforcement_industry because appropriations demonstrably land there and convert into expansion advocacy; the mobility premiums enjoyed by privileged holders are real but diffuse across millions of holders with no capturing seat. fixing_cost is prohibitive: removal confronts electoral veto players, destination labor-market shock, and treaty renegotiation, vastly exceeding any single seat's benefit from fixing. Coalitions: the regime suppresses the payer coalition it creates - worksite raids and legal precarity chill the cross-status organizing that would give powerless seats weight - a self-sealing feature the classifier should see as part of suppression, not noise.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat computation should diverge sharply. Payer seats (migrant classes, citizen workers) compute high-chi extractive types: trapped exit multiplies effective extraction. The enforcement seat computes subsidy-side (low or negative chi): the regime pays it. The agenda_setter seat computes a governance-coordination framing - from the ministry desk the same structure is population management it operates. Three institutional seats share the 'institutional' power atom yet diverge on exit options (destination governments arbitrage by externalizing; origin governments are constrained by remittance dependence; the enforcement sector is identity_locked to its mission) - same nominal level, different structural relationships, which is the inter-institutional signal. The observer seat sees both faces and neither pocket. The engine owns these computations; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for destination_state_governments, border_enforcement_industry, privileged_passport_holders, and employers_of_deportable_labor; victim declarations drive high d for the four migrant-class payers and the two citizen payers. Two overrides are declared because the structural derivation cannot see intra-agent cost-bearing: origin_state_governments derive near-full-beneficiary from the beneficiary list but carry remittance dependence, brain drain, and zero leverage over the rules they feed - overridden to 0.35; destination_state_governments derive near-zero as agenda-setting beneficiaries but appropriate the enforcement bill and diplomatic friction themselves - overridden to 0.20. Receipt is not benefit: the enforcement sector is named in gain_flow as the demonstrable receiver of the fiscal stream even though privileged_passport_holders collect the larger diffuse mobility premium.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - wartime subversion control and interwar displacement management - is substantially superseded, but the arrangement not only persists, it grew through every subsequent crisis, which is the mandate-drift signature. The founding_problem_status is authored 'contested' rather than 'dead': destination states credibly attest residual security and capacity problems, so the dead-x-world_rearranges capture flag does not cleanly fire; instead the enforcement-sector accumulation in the measurement series (budgets and theater rising together, independent of crossing volumes) is the observable mandate drift. The tangled_rope claim does the mislabeling prevention: reading the regime as pure snare erases the documentation, screening, and epidemiological coordination even this reading concedes in presumptive form (its own axiom permits justified exceptions); reading it as pure rope erases the mobility transfer and the citizen-victim delta that define the reading. Mandatrophy is deliberately NOT flagged resolved - the contested genealogy is the finding, not a defect to paper over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    border_kernel_reading_membership,
    'This constraint is the freedom_of_movement_reading of the border_legitimacy kernel - what structurally changes under the sibling readings (sovereignty_reading, humanitarian_obligation_reading)?',
    'Read the sibling stories'' victim sets, epsilon values, and claimed types; the disagreement is located in the normative status of entry restriction itself (personal right vs conditional admission duty vs sovereign prerogative).',
    'Under sovereignty_reading the migrant victim set empties and the regime recomputes as defended coordination; under humanitarian_obligation_reading economic movers fall outside the protected set and epsilon concentrates on enforcement abuses against recognized claimants rather than restriction per se. This file''s epsilon is valid only for this reading over the fixed referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(border_kernel_reading_membership, conceptual, 'Reading-indexed membership in the border_legitimacy kernel; committer structure routed here per Rule 2').

omega_variable(
    entry_right_textual_grounding,
    'Does the reading''s extension of a movement right to ENTRY rest on its grounding texts (ICCPR Article 12 covers internal movement and the right to leave, not a right to enter another state), or is it a deliberate normative extension beyond them?',
    'Jurisprudential tracing of treaty-body practice, General Comment 27, and drafting history of UDHR Article 13; observe whether any authoritative interpreter has ever treated entry denial as a movement-right violation simpliciter.',
    'If textually grounded, this reading inherits treaty-lineage authority and its critique binds states; if it is an extension, the reading''s epsilon rests on its deontological axiom alone, its foreclosure pressure against the sovereignty_reading strengthens (axiom-level contradiction rather than interpretation-level dispute), and the interpretation_layer absorbs less drift on this reading''s behalf.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entry_right_textual_grounding, conceptual, 'Whether the entry-extension is inside or beyond the reading''s own fixed-text grounding').

omega_variable(
    citizen_victim_status_empirics,
    'Do admission restrictions actually harm displaced domestic workers and welfare-dependent citizens (this reading''s structural delta), or do they protect those citizens'' wages and benefit pools as the standard restrictionist account holds?',
    'Natural experiments: bracero-program termination, the Mariel boatlift, EU enlargement waves, visa-lottery randomization; estimate native wage and employment effects in the exposed skill cells.',
    'If restrictions protect low-wage citizens, the two citizen seats drop out of the victim set, epsilon falls for those seats, and the regime''s asymmetry narrows toward a composite (coordination-defending for insiders, extractive for migrants); if they harm, the delta holds and the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_victim_status_empirics, empirical, 'Empirical status of the citizen-victim inclusion that distinguishes this reading').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (patrols, carrier sanctions, detention, externalized processing) or internalized (would-be migrants self-deterring before any enforcement contact)?',
    'Post-liberalization flow trajectories: after visa waivers or regularization programs, if movement jumps far beyond the mechanical enforcement change, the prior gap carried an internalized component; compare application-rate responses to identical objective barriers across information environments.',
    'An internalized share means effective suppression exceeds the structural measure and travels with the migrant after any barrier is removed; the deterrent messaging apparatus (the theater component) is doing enforcement work invisible to facility counts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism in the mobility regime').

omega_variable(
    externalization_liability_relocation,
    'Does enforcement externalization (transit-state processing, offshore interdiction, carrier sanctions) reduce the regime''s total extraction or merely relocate it beyond the measuring frame?',
    'Trace mortality, detention incidence, and enforcement financing along route segments before and after externalization compacts; count the off-frame costs (transit-state coercion, offshore deaths) against headline destination-state metrics.',
    'If relocated, the authored epsilon understates the standing arrangement''s total extraction and the transit-state governments enter as additional payer-beneficiary hybrids; headline improvement is then a measurement artifact of the frame, not a property of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_liability_relocation, empirical, 'Whether externalization moves extraction out of frame rather than reducing it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bord_tr_t6, border_legitimacy__freedom_of_movement_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(bord_tr_t12, border_legitimacy__freedom_of_movement_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(bord_tr_t18, border_legitimacy__freedom_of_movement_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__freedom_of_movement_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__freedom_of_movement_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(bord_be_t6, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(bord_be_t12, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(bord_be_t18, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(bord_su_t6, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(bord_su_t12, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(bord_su_t18, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 18, 0.81).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, identity_coordination).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'border legitimacy' decomposes into three reading-stories of one kernel. This member authors epsilon approximately 0.80 over the standing restriction regime, with migrants and status-policed citizens as payers; the sovereignty_reading relocates the victim set (migrants leave it; the regime becomes defended coordination) and the humanitarian_obligation_reading narrows the protected mover class to persecution/disaster flight. Each story links the other two via affects_constraints; epsilon values are per-file and never averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, institutional, 0.35).
constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
