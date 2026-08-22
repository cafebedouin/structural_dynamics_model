% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: Combatant Status Definition — State-Centric Reading (Article 4 Exhaustive Test)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   The standing arrangement under contest is the state-centric test for
 *   combatant status: membership in a state's armed forces, meeting the
 *   Article 4 criteria of responsible command, distinctive sign, open
 *   arms-carrying, and conduct per the laws of war, yields prisoner-of-war
 *   protections and combatant privilege upon capture; fighters outside state
 *   organization are categorically ineligible and remain exposed to domestic
 *   prosecution for the act of fighting itself. The arrangement does real
 *   coordination work — it is the machinery that makes inter-state POW
 *   reciprocity credible and keeps the combatant/civilian line administrable
 *   — and the same structure imposes severe asymmetric costs on the class it
 *   excludes. This file instantiates ONE reading of the
 *   combatant_status_definition kernel; the sibling readings are separate
 *   constraints (separate files) linked through the network block, and the
 *   contest between readings is carried in the omega variables, not inside
 *   this constraint's classification. The claimed type and the metrics are
 *   authored independently: tangled_rope is what I believe is structurally
 *   true of this arrangement; the metric values are what I believe is
 *   descriptively true of its operation.
 *
 * KEY AGENTS:
 *   - - state_armed_forces: Primary beneficiary (institutional/constrained) — receives POW protections and combatant privilege by state commission
 *   - - detaining_state_authorities: Agenda-setter and collecting seat (institutional/arbitrage) — administers status determination, selects applicable instruments, collects prosecutorial leverage
 *   - - organized_non_state_fighters: Primary target (organized/trapped) — bears categorical exclusion despite de facto discipline; structurally absent from the drafting table
 *   - - detained_non_state_fighters: Immediate target (powerless/trapped) — bears detention and prosecution exposure in real time
 *   - - state_soldiers_in_insurgent_custody: Conditional beneficiary (moderate/trapped) — holds status whose realization depends on captor compliance
 *   - - icrc_and_humanitarian_monitors: Analytical observer (organized/analytical) — sees the full structure, advocates within it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.68).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.68).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "Combatant Status Definition — State-Centric Reading (Article 4 Exhaustive Test)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '80864982-da7f-4734-b6da-f5e09ef5bb17').
narrative_ontology:cs_kernel_codification('80864982-da7f-4734-b6da-f5e09ef5bb17', formalized).
narrative_ontology:cs_authority_grounding('80864982-da7f-4734-b6da-f5e09ef5bb17', lineage).
narrative_ontology:cs_interpretation_layer_present('80864982-da7f-4734-b6da-f5e09ef5bb17').
narrative_ontology:cs_reading_relation('80864982-da7f-4734-b6da-f5e09ef5bb17', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('80864982-da7f-4734-b6da-f5e09ef5bb17', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('80864982-da7f-4734-b6da-f5e09ef5bb17', foundational, combatant_privilege_requires_state_commission).
narrative_ontology:cs_axiom_status(combatant_privilege_requires_state_commission, holdable).
narrative_ontology:cs_axiom_grounding('80864982-da7f-4734-b6da-f5e09ef5bb17', combatant_privilege_requires_state_commission, conventional).
narrative_ontology:cs_axiom('80864982-da7f-4734-b6da-f5e09ef5bb17', secondary, reciprocity_requires_verifiable_membership).
narrative_ontology:cs_axiom_status(reciprocity_requires_verifiable_membership, holdable).
narrative_ontology:cs_axiom_grounding('80864982-da7f-4734-b6da-f5e09ef5bb17', reciprocity_requires_verifiable_membership, instrumental).
narrative_ontology:cs_reference_frame('80864982-da7f-4734-b6da-f5e09ef5bb17', westphalian_state_monopoly_on_force).
narrative_ontology:cs_drift_state('80864982-da7f-4734-b6da-f5e09ef5bb17', post_additional_protocol_i_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('80864982-da7f-4734-b6da-f5e09ef5bb17', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_armed_forces).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, detaining_state_authorities).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, detained_non_state_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_soldiers_in_insurgent_custody).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, organized_non_state_fighters).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, strict_distinction_principle).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, treaty_reciprocity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of state militaries commissioned under national command structures. Upon capture by an opposing state they are entitled to prisoner-of-war status: detention without prosecution for lawful acts of war, and repatriation at the cessation of hostilities. The act of fighting itself carries no criminal exposure for them, because their state commission immunizes it. Some states' forces extend equivalent treatment voluntarily to adversaries who lack formal status; most do not.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_armed_forces, beneficiary,
    institutional, generational, constrained, global).

% National governments and military commands that determine who qualifies for prisoner-of-war status upon capture, operate detention facilities, and decide whether to prosecute captured fighters under domestic criminal law. They select which treaty instruments to apply in a given conflict — the 1949 Conventions alone, protocols they have ratified, reservations they maintain — and this selection discretion shapes what each captured fighter faces. Prosecution for mere participation remains available to them against fighters lacking formal status, up to capital charges in some jurisdictions.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detaining_state_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Armed groups with command structures, internal discipline, and in many cases visible insignia and open weapons-carrying, fighting state forces without being organs of any state. When captured, their members face domestic prosecution for the act of fighting itself, regardless of how cleanly they fought. They were not parties to the diplomatic conferences that drafted the status criteria and cannot accede to the treaties in their own name; their claims to equivalent treatment reach the system only through intermediaries such as the ICRC or sympathetic states.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, organized_non_state_fighters, payer,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, organized_non_state_fighters, excluded).

% Individual fighters currently held after capture, awaiting or undergoing status determination, internment, or criminal proceedings. Their day-to-day treatment is set by whichever instrument their captor state acknowledges; where the captor applies only the 1949 Conventions' baseline, they sit outside the prisoner-of-war regime entirely and can be tried for participation. Their way out runs through release, escape, exchange, or the end of the conflict — none of which they control.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detained_non_state_fighters, payer,
    powerless, immediate, trapped, national).

% State military personnel captured by non-state armed groups. Their status as lawful combatants attaches by virtue of their state commission and travels with them, but its realization depends entirely on whether their captor chooses to honor it; non-state captors are bound only to baseline humane-treatment obligations. Their protection in practice is hostage to reciprocity structures that their own side's reading does not extend to their captors.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_soldiers_in_insurgent_custody, beneficiary,
    moderate, immediate, trapped, regional).

% The International Committee of the Red Cross and associated monitoring bodies visit detention facilities, register detainees, press for baseline treatment in every conflict, and act as custodian of the Geneva tradition's interpretive continuity. They advocate inside the framework rather than against it, documenting the differences in protection between categories of detainee.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc_and_humanitarian_monitors, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, detaining_state_authorities).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the inter-state reciprocity problem: by tying prisoner-of-war protection to objectively verifiable membership criteria (responsible command, distinctive signs, open arms-bearing, compliance with the laws of war), each state party can honor protections for enemy soldiers with confidence the treatment will be reciprocated. The same criteria keep the combatant/civilian line sharp, which is what lets civilians avoid targeting and lets fighters be distinguished from the population around them.
% TRANSFER_FUNCTION: Moves legal immunity — combatant privilege, meaning exemption from prosecution for lawful acts of war — and full detention protections to members of state militaries; moves criminal liability for mere participation, and reduced detention protections, onto fighters outside state organization. The transfer runs from non-state fighters to state-organized forces along the same legal structure.
% ABSENT_VOICES: Non-state armed groups themselves. They had no seat at the diplomatic conferences where the status criteria were drafted, cannot accede to the treaties in their own name, and enter the record only through ICRC intermediation, UN inquiries, and the testimony of sympathetic states. Their objection — that organized, disciplined groups fighting under the laws of war merit equivalent status — is documented but structurally unrepresented at the table where the criteria are set.
% DISAPPEARANCE_RATIONALE: If the state-centric test vanished overnight and status became independent of state organization, detention regimes would reorganize immediately: prosecution-for-participation would lose its legal basis, exchange negotiations would no longer hinge on status determinations, and states would lose the criminal-law lever they currently hold over insurgencies. Reciprocity expectations between state militaries would need rebuilding on a new eligibility basis, and the distinction architecture governing targeting would have to be re-derived.
% FOUNDING_PROBLEM: After the Second World War, the drafters needed clear, verifiable criteria deciding who must be treated as a prisoner of war — so that states would reciprocate protections rather than defect, so that fighters could not hide their status, and so the combatant/civilian line stayed enforceable enough to protect civilians.
% FOUNDING_PROBLEM_CORROBORATION: ICRC custodial commentary and the academic IHL literature — sources outside the benefiting parties — attest both halves: the inter-state reciprocity problem is live and the state-centric criteria solve it for interstate war, AND the question of extending the solution to non-international armed conflict is unresolved, with AP I ratification by a majority of states and persistent doctrinal objection by several major military powers recorded on opposite sides. State militaries attest liveness from inside the benefiting set; the corroborating dispute documentation comes from outside it.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.68: for the excluded class the arrangement is severely costly — prosecution for mere belligerency, capital exposure in some jurisdictions, detention outside the POW regime — while for state militaries it is near-costless and purely protective; weighted across the population the arrangement governs, this lands mid-high. Suppression 0.68: the exclusion is not self-sustaining; it is maintained by active domestic prosecution, status-determination discretion, and treaty-selection arbitrage, and a fighter cannot exit the position except through release, escape, exchange, or war's end. Theater ratio 0.25: the enforcement is overwhelmingly real, but the 2001-2006 window shows a pronounced performative component — 'unlawful combatant' labeling and ad hoc tribunal architecture performing legality while holding detainees between frameworks — visible as the spike in the shared-grid series before partial normalization. Accessibility collapse 0.45: alternatives persist (voluntary extension of protections, AP I accession, reliance on the Common Article 3 baseline) but none of them dissolves the categorical line itself. Resistance 0.55: a majority of states adopted AP I's extension, fighters assert disciplined conduct to claim equivalence, and scholarship and litigation press the point continuously — real, sustained, not yet victorious. All three tracked series run on one shared ten-point grid spanning 1949-2025; the 2001-2006 elevation across all three metrics shares a single driver (explicit invocation of the exclusion to hold detainees outside both POW and ordinary criminal frameworks), not independent noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same text. From the state_armed_forces seat the arrangement is the thing that makes their acts of war immune and their capture survivable — its operation is almost entirely protective. From the organized_non_state_fighters and detained_non_state_fighters seats the same clauses are the wall between them and any immunity at all. The detaining_state_authorities seat experiences it as administrative capability: discretion over which instrument applies, and a criminal-law lever held in reserve. The conditional seat (state_soldiers_in_insurgent_custody) experiences a further divergence — formal entitlement intact, realization contingent on a captor the reading does not bind to grant it. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: state_armed_forces and detaining_state_authorities sit near the beneficiary pole; organized and detained non-state fighters sit near the target pole, amplified by trapped exit. One override is authored: power_atom 'institutional' to d=0.12. Rationale: generic derivation would place an agenda_setter near symmetric, but both institutional seats here do not merely benefit passively — they administer the arrangement and collect from it (prosecution leverage, preservation of the violence monopoly, discretionary instrument selection), which places them deeper on the benefit side than a plain beneficiary declaration implies. The override applies to exactly the two institutional seats; no other stakeholder carries that power atom. The conditional beneficiary seat derives low d from its beneficiary declaration; its situation text records the realization contingency that a single scalar cannot express.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making inter-state POW reciprocity credible and keeping the combatant/civilian line enforceable — is live for interstate war, which is exactly why this arrangement must not be mislabeled a dead mandate or a pure extraction scheme: the reciprocity engine is what keeps state parties complying at all. Equally, it must not be mislabeled pure coordination: the categorical exclusion transfers immunity from one entire class of fighters to another through the same clauses. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges — no dead-mandate flag fires, correctly, because the function operates daily; the contest is over scope, not existence. Mandatrophy_resolved is deliberately not set: the mandate has not outlived its function, and declaring otherwise would launder a live scope-dispute into an obsolescence finding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positionality,
    'This constraint is one reading of the combatant_status_definition kernel — the state_centric_reading. Would instantiating the national_liberation_reading or the functional_protection_reading instead change the victim set and epsilon?',
    'Comparative instantiation: generate the sibling stories and diff their victim arrays and epsilon values against this file. The disagreement is located at the exhaustiveness of the Article 4 criteria — the single clause this reading asserts and the siblings modify or dissolve.',
    'The national_liberation_reading removes anti-colonial and anti-occupation fighters from the victim set, cutting epsilon for that subclass; the functional_protection_reading eliminates status-differentiated protections entirely, collapsing extraction toward the Common Article 3 floor and dissolving the tangled_rope structure into something rope-like with a universal minimum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positionality, conceptual, 'Committer structure: this story is the state-centric member of a three-reading kernel; sibling instantiation would restructure the victim set.').

omega_variable(
    ap1_plurality_of_arrangements,
    'Does the state-centric arrangement now govern only the bloc of states that never ratified AP I or maintain reservations against its extension — making the standing arrangement regionally plural rather than globally uniform?',
    'Map ratification, reservation, and declaration positions against observed detention and prosecution practice in recent non-international armed conflicts, jurisdiction by jurisdiction.',
    'If the reading effectively governs a minority bloc, the global classification masks regionally concentrated pockets where the exclusion operates at full severity; effective extraction becomes geography-dependent and the aggregate epsilon understates the worst jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ap1_plurality_of_arrangements, empirical, 'Whether the arrangement is uniform worldwide or a plurality of regional regimes under one label.').

omega_variable(
    organizational_parity_of_armed_groups,
    'Do most organized non-state parties to contemporary armed conflicts meet Article 4-equivalent discipline de facto — responsible command, internal discipline, visible signs, open arms-carrying — such that the categorical exclusion extracts without functional justification?',
    'Systematic coding of armed-group organization against the four criteria (ICRC customary-IHL study methodology, armed-group structure datasets), aggregated across recent conflicts.',
    'High parity would indicate the exclusion no longer tracks any functional boundary and reads as rent-preservation by state monopolies, pushing the structure snare-ward; low parity would confirm a genuine boundary-maintenance function and hold the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_parity_of_armed_groups, empirical, 'Whether the excluded class is functionally indistinguishable from the included one under the arrangement''s own criteria.').

omega_variable(
    realized_vs_nominal_prosecution,
    'Do detaining states actually exercise domestic prosecution for mere belligerency against captured fighters, or do they predominantly intern and release — is the nominal severity of the exclusion realized in practice?',
    'Cross-conflict compilation of prosecution-for-participation cases, charge outcomes, and sentence severity versus internment-without-trial dispositions.',
    'Low realized prosecution would lower effective extraction below the nominal measure and soften the target-seat classification; high realized prosecution, especially capital outcomes, would raise effective extraction above the authored base and sharpen the divergence between seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(realized_vs_nominal_prosecution, empirical, 'Gap between the arrangement''s nominal severity for excluded fighters and its realized application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement_basis(comb_tr_t1949, observed).
narrative_ontology:measurement(comb_tr_t1958, combatant_status_definition__state_centric_reading, theater_ratio, 1958, 0.14).
narrative_ontology:measurement_basis(comb_tr_t1958, observed).
narrative_ontology:measurement(comb_tr_t1967, combatant_status_definition__state_centric_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement_basis(comb_tr_t1967, observed).
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__state_centric_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement_basis(comb_tr_t1977, observed).
narrative_ontology:measurement(comb_tr_t1986, combatant_status_definition__state_centric_reading, theater_ratio, 1986, 0.22).
narrative_ontology:measurement_basis(comb_tr_t1986, observed).
narrative_ontology:measurement(comb_tr_t1995, combatant_status_definition__state_centric_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement_basis(comb_tr_t1995, observed).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement_basis(comb_tr_t2001, observed).
narrative_ontology:measurement(comb_tr_t2006, combatant_status_definition__state_centric_reading, theater_ratio, 2006, 0.42).
narrative_ontology:measurement_basis(comb_tr_t2006, observed).
narrative_ontology:measurement(comb_tr_t2014, combatant_status_definition__state_centric_reading, theater_ratio, 2014, 0.32).
narrative_ontology:measurement_basis(comb_tr_t2014, observed).
narrative_ontology:measurement(comb_tr_t2025, combatant_status_definition__state_centric_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement_basis(comb_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.58).
narrative_ontology:measurement_basis(comb_be_t1949, observed).
narrative_ontology:measurement(comb_be_t1958, combatant_status_definition__state_centric_reading, base_extractiveness, 1958, 0.6).
narrative_ontology:measurement_basis(comb_be_t1958, observed).
narrative_ontology:measurement(comb_be_t1967, combatant_status_definition__state_centric_reading, base_extractiveness, 1967, 0.63).
narrative_ontology:measurement_basis(comb_be_t1967, observed).
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.64).
narrative_ontology:measurement_basis(comb_be_t1977, observed).
narrative_ontology:measurement(comb_be_t1986, combatant_status_definition__state_centric_reading, base_extractiveness, 1986, 0.65).
narrative_ontology:measurement_basis(comb_be_t1986, observed).
narrative_ontology:measurement(comb_be_t1995, combatant_status_definition__state_centric_reading, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement_basis(comb_be_t1995, observed).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.72).
narrative_ontology:measurement_basis(comb_be_t2001, observed).
narrative_ontology:measurement(comb_be_t2006, combatant_status_definition__state_centric_reading, base_extractiveness, 2006, 0.74).
narrative_ontology:measurement_basis(comb_be_t2006, observed).
narrative_ontology:measurement(comb_be_t2014, combatant_status_definition__state_centric_reading, base_extractiveness, 2014, 0.71).
narrative_ontology:measurement_basis(comb_be_t2014, observed).
narrative_ontology:measurement(comb_be_t2025, combatant_status_definition__state_centric_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(comb_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement_basis(comb_su_t1949, observed).
narrative_ontology:measurement(comb_su_t1958, combatant_status_definition__state_centric_reading, suppression_requirement, 1958, 0.52).
narrative_ontology:measurement_basis(comb_su_t1958, observed).
narrative_ontology:measurement(comb_su_t1967, combatant_status_definition__state_centric_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement_basis(comb_su_t1967, observed).
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement_basis(comb_su_t1977, observed).
narrative_ontology:measurement(comb_su_t1986, combatant_status_definition__state_centric_reading, suppression_requirement, 1986, 0.62).
narrative_ontology:measurement_basis(comb_su_t1986, observed).
narrative_ontology:measurement(comb_su_t1995, combatant_status_definition__state_centric_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement_basis(comb_su_t1995, observed).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.74).
narrative_ontology:measurement_basis(comb_su_t2001, observed).
narrative_ontology:measurement(comb_su_t2006, combatant_status_definition__state_centric_reading, suppression_requirement, 2006, 0.78).
narrative_ontology:measurement_basis(comb_su_t2006, observed).
narrative_ontology:measurement(comb_su_t2014, combatant_status_definition__state_centric_reading, suppression_requirement, 2014, 0.72).
narrative_ontology:measurement_basis(comb_su_t2014, observed).
narrative_ontology:measurement(comb_su_t2025, combatant_status_definition__state_centric_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement_basis(comb_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'combatant status in IHL' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints: this state-centric reading (epsilon high for non-state fighters, near-zero for state militaries), the national-liberation reading (removes anti-colonial and anti-occupation fighters from the victim set via AP I Article 1(4)), and the functional-protection reading (dissolves status-differentiated protections; epsilon collapses toward the Common Article 3 floor). Measuring 'combatant status' through different observables yields materially different epsilon values, which is the signature that these are different constraints sharing a label, not one constraint viewed from angles. The state-centric reading is the historically prior codification (1949); the siblings respond to it — the national-liberation reading as a partial treaty-level override, the functional-protection reading as a floor beneath it — so the influence edges run from this file to both dependents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
