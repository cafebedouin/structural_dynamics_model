% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
% Status: [CONTESTED — judicially superseded at federal level (2008); live in scholarship and state-analogue debates]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective-Rights Settlement (Militia-Only Protection)
 *   domain: constitutional law/political philosophy/legal interpretation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Second Amendment kernel: the
 *   collective-right reading, under which the amendment protects state
 *   militia authority and places no limit on legislative regulation of
 *   private arms ownership outside organized militia service. The standing
 *   arrangement under contest — the ε referent — is the militia-only
 *   constitutional settlement as it actually operated from ratification to
 *   Heller, assessed by this reading's own lights: prohibition and regulation
 *   measures are ordinary police-power law, so they carry low ε, and the
 *   individuals subject to them bear lawful regulation rather than
 *   extraction. KEY AGENTS (by structural relationship): state_governments —
 *   primary beneficiary (institutional/constrained), collect the militia
 *   guarantee; organized_militia_institutions — the protected object
 *   (organized/constrained), transforming beneath the guarantee;
 *   firearms_regulators — secondary beneficiary (institutional/mobile),
 *   receive cleared regulatory space; individual_gun_owners — excluded
 *   claimants (organized/constrained), defined out of the text's scope and
 *   bearing upheld restrictions; federal_government — restricted sovereign
 *   (institutional/trapped); federal_judiciary — agenda setter
 *   (institutional/constrained), administered then dissolved the doctrine;
 *   constitutional_scholars — analytical observers. The sibling readings
 *   (individual_right_reading, civic_republican_reading) are separate
 *   constraint stories with their own ε, beneficiary/victim structures, and
 *   types; this file authors only the collective reading and does not average
 *   across readings. Claim/metric independence is preserved: the claimed type
 *   (rope) reflects the arrangement's life-dominant structure, while the
 *   authored metrics describe the full arc including terminal atrophy — no
 *   reconciliation is performed.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/constrained) — collect the militia guarantee against federal dissolution
 *   - organized_militia_institutions: Protected object (organized/constrained) — the arrangement's referent, federally absorbed after 1903
 *   - firearms_regulators: Secondary beneficiary (institutional/mobile) — receive constitutionally unobstructed regulatory space
 *   - individual_gun_owners: Excluded claimants (organized/constrained) — defined out of the amendment's scope; bear upheld restrictions
 *   - federal_government: Restricted sovereign (institutional/trapped) — bears the militia guarantee as a permanent limit
 *   - federal_judiciary: Agenda setter (institutional/constrained) — administered the reading for two centuries, then reversed it in 2008
 *   - constitutional_scholars: Analytical observers (analytical/analytical) — produced the revisionist historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.22).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.32).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective-Rights Settlement (Militia-Only Protection)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional law/political philosophy/legal interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'aabb643c-3874-4c50-899c-be2e319f0dab').
narrative_ontology:cs_kernel_codification('aabb643c-3874-4c50-899c-be2e319f0dab', fixed_text).
narrative_ontology:cs_authority_grounding('aabb643c-3874-4c50-899c-be2e319f0dab', lineage).
narrative_ontology:cs_interpretation_layer_present('aabb643c-3874-4c50-899c-be2e319f0dab').
narrative_ontology:cs_reading_relation('aabb643c-3874-4c50-899c-be2e319f0dab', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('aabb643c-3874-4c50-899c-be2e319f0dab', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('aabb643c-3874-4c50-899c-be2e319f0dab', foundational, militia_prefatory_clause_confines_operative_right).
narrative_ontology:cs_axiom_status(militia_prefatory_clause_confines_operative_right, holdable).
narrative_ontology:cs_axiom_grounding('aabb643c-3874-4c50-899c-be2e319f0dab', militia_prefatory_clause_confines_operative_right, empirically_contingent).
narrative_ontology:cs_axiom('aabb643c-3874-4c50-899c-be2e319f0dab', foundational, states_are_sole_rights_holders).
narrative_ontology:cs_axiom_status(states_are_sole_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('aabb643c-3874-4c50-899c-be2e319f0dab', states_are_sole_rights_holders, conventional).
narrative_ontology:cs_axiom('aabb643c-3874-4c50-899c-be2e319f0dab', secondary, private_arms_within_plenary_police_power).
narrative_ontology:cs_axiom_status(private_arms_within_plenary_police_power, holdable).
narrative_ontology:cs_axiom_grounding('aabb643c-3874-4c50-899c-be2e319f0dab', private_arms_within_plenary_police_power, conventional).
narrative_ontology:cs_reference_frame('aabb643c-3874-4c50-899c-be2e319f0dab', founding_era_state_militia_compact).
narrative_ontology:cs_drift_state('aabb643c-3874-4c50-899c-be2e319f0dab', pre_heller_twentieth_century, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('aabb643c-3874-4c50-899c-be2e319f0dab', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militia_institutions).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, firearms_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the amendment as a constitutional shield for their military institutions against federal dissolution or disarmament, while separately exercising police power over private arms within their borders. The reading assigns them the protected position: they collect the guarantee without bearing a corresponding obligation. Exit from the constitutional order is unavailable; their stake tracks whichever interpretive settlement maximizes retained state authority.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Exist as the arrangement's protected object: state-run armed forces the amendment guarantees against federal abolition. Across the interval their character transforms — after 1903 they are federally organized, funded, and trained, and ultimately deployable under federal command — so the protection attaches to institutions progressively integrated into the very federal structure the guarantee was designed to guard against.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militia_institutions, beneficiary,
    organized, generational, constrained, national).

% State legislatures and administrative agencies enacting restrictions on private weapons. Because the reading confers no individual entitlement, their statutes face only ordinary political checks and deferential review; the arrangement clears constitutional friction from their path. They can enact, amend, or repeal at will; their stake is the regulatory space itself.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, firearms_regulators, beneficiary,
    institutional, biographical, mobile, national).

% Persons who keep weapons for self-defense, hunting, or collection and who sought constitutional shelter for that possession. Under this reading their claims are defined out of the amendment's scope rather than weighed and rejected: courts dismiss them at the threshold. They bear the practical costs of upheld restrictions — surrendered property, licensing burdens, prohibited categories — while their avenue of recourse runs only through ordinary politics. Owner associations gave them collective voice but no constitutional traction during the doctrine's reign.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, individual_gun_owners, payer).

% Bound by the militia guarantee: it may not dissolve or disarm state military institutions, and its authority over organized state forces is constitutionally qualified. Bears the restriction as a permanent feature of the union it constitutes and cannot exit the structure that binds it. Receives in exchange the stability of the federal bargain the guarantee helped secure.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, payer,
    institutional, generational, trapped, national).

% Administers the reading: decides which claims the amendment reaches, supplied its canonical statements (Cruikshank, Miller), and sustained it through uniform threshold dismissal of individual claims across the twentieth century. Bound by precedent and appointment politics; ultimately reversed course in District of Columbia v. Heller (2008), dissolving the doctrine it had maintained.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Debate the reading's fidelity to founding-era text and history. From the 1980s forward a revisionist wave (Levinson 1989, Amar, Barnett, and respondents) attacked the collective reading's historical foundations, producing the evidentiary record on which the later litigation campaign drew. Neither collects nor pays; supplies the terrain on which the interpretive contest is fought.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the federal division of military power: guarantees that states retain organized armed institutions of their own, so that the union's armed force is never solely federal. Solves the founding-era collective-action problem of securing the states against federal military consolidation without requiring each state to defend its militia ad hoc.
% TRANSFER_FUNCTION: Moves constitutional shelter toward state military institutions and regulatory discretion toward legislatures. Moves nothing material from individuals, who under this reading are assigned neither entitlement nor obligation by the amendment; the operative transfer is the allocation of a litigation position — claimants who would invoke the amendment personally have their claims extinguished at the threshold.
% ABSENT_VOICES: Individual rights-bearers asserting self-defense entitlements appeared in courtrooms but were defined out of the text's scope — the reading treats their claim as a category error rather than a losing argument. Historically, the enslaved and freedmen disarmed by state regimes had no voice in the settlement at all; and the unorganized militia — the able-bodied population nominally enrolled on paper — was never consulted about the arrangement maintained in its name.
% DISAPPEARANCE_RATIONALE: If the militia-only settlement vanished overnight and an individual-right settlement took its place, large numbers of prospective constitutional claims would spring into existence immediately; firearms regulation nationwide would face searching review; legislative agendas at both levels would reorder around a new constitutional constraint; and the states' militia guarantee would lose its textual home. The arrangement's disappearance rearranges the legal world — which is why its 2008 replacement produced decades of litigation churn.
% FOUNDING_PROBLEM: Anti-federalist opposition to ratification centered on the fear that a standing federal army would render state governments powerless; the amendment was built to guarantee that states could maintain their own armed militias, preserving a military counterweight inside the federal design.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the statutory record of the militia's transformation — the Militia Act of 1903 and the National Defense Act of 1916, which placed state forces under federal organization, funding, and command — and by mainstream military-history scholarship documenting that no state retains independent military capacity. State governments, the arrangement's beneficiaries, attest only to the general value of federalism safeguards, not to the liveness of the specific militia-protection problem; no party outside the arrangement's defenders attests that the founding problem survives.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22 representative of the mature phase) because, by this reading's lights, the settlement transfers no material burden from anyone: it shields state institutions and delegates arms policy to ordinary democratic processes. Suppression (0.32) is moderate and structural-operational rather than personal: the arrangement coerced through threshold dismissal of claims, not through enforcement aimed at persons; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Accessibility_collapse (0.50) blends two opposite pictures: for individual claimants the alternative (constitutional relief) collapsed completely, while for arms policy the space of legislative alternatives stayed wide open. Resistance (0.72) is high — the settlement met two centuries of contest, a scholarship revolt from the 1980s, and eventual judicial reversal, which is what constructs that must be defended rather than discovered typically experience. Theater_ratio (0.50 representative; 0.08 rising to 0.65 across the series) tracks the arrangement's referent decaying beneath it: after the Dick Act (1903) and National Defense Act (1916) federalized the state forces, courts recited militia language while applying the settlement to private weapons far from any militia, and the 'unorganized militia' survived as a paper enrollment. The measurement series run on one shared eight-point grid so every tracked metric is authored at every examined time point; the 2008 column records the terminal event — enforcement collapsing and operational extraction falling as Heller repudiated the reading.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical text. From the state seat the settlement is genuine protection it lobbied for and still values — a working coordination of military federalism. From the excluded claimant seat the same twenty-seven words operate as a locked door: a categorical bar encountered before any merits hearing. From the federal seat it is a binding limit accepted as constitutive design rather than suffered as loss. From the judicial seat it was, for two centuries, a settled administration practice, then an embarrassment to be discarded. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries sit at the low-directionality end: state_governments and organized_militia_institutions are subsidized by the guarantee directly, and firearms_regulators benefit from the regulatory space the reading clears. No victims array is declared, deliberately: under this reading's own lights no group is extracted from — prohibition targets bear lawful, democratically enacted regulation, and the claimant's exclusion is an absence of subsidy, not an imposition of burden. That absence is why ε is low rather than why it is hidden. The federal government sits mid-range: restricted in its military authority but stabilized by the same bargain. Because the derivation from beneficiary declarations plus exit options already locates every seat correctly, no directionality overrides are used.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving state military capacity against a standing federal army — died between 1903 and 1916, when the militia system was converted into a federally organized, funded, and commanded National Guard. The settlement nonetheless persisted for roughly ninety further years, increasingly theatrical in its maintenance (rising theater_ratio) and increasingly useful mainly as a litigation shield. Mandatrophy was resolved not by any internal sunset but by external judicial repudiation in 2008, which is why the R5 interview records status=dead against disappearance_verdict=world_rearranges: the mismatch flags the zombie phase honestly rather than flattering the arrangement with a functional origin story. The rope claim prevents two symmetrical misreadings: calling the whole life a snare would erase the genuine early coordination function (and no victim set exists to support it), while treating the settlement as a natural or settled feature would ignore that it was a constructed interpretive choice, defended against sustained resistance, and ultimately reversible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_subject_disagreement,
    'Which subject does ''the right of the people'' in the amendment name — the states as corporate rights-holders (this reading), persons as holders of a pre-existing individual liberty (individual_right_reading), or the armed citizen-body in its civic capacity (civic_republican_reading)?',
    'Founding-era corpus linguistic analysis of ''bear arms'' and ''the people'' usage, systematic review of state ratification conventions, and comparison of how each sibling reading''s story structures its beneficiary/victim sets; the disagreement is located precisely in the grammatical and political referent of the operative clause''s subject.',
    'Switching the subject inverts this story''s entire structure: under the individual reading the excluded claimants become the protected class and regulatory measures become the arrangement''s targets (high ε on prohibitions); under the civic republican reading an obligation-bearing armed citizenship replaces both. This file''s low-ε authorization of prohibitions holds only within the collective reading''s frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_subject_disagreement, conceptual, 'Committer-frame routing: this constraint is the collective_right_reading of the second_amendment_arms_right kernel; the subject-of-the-right question is where the readings structurally diverge.').

omega_variable(
    founding_semantic_evidence,
    'Does founding-era usage of ''keep and bear arms'' support the militia-confined idiom this reading relies on, or the broader individual usage its rivals assert?',
    'Large-scale corpus analysis of eighteenth-century legal, political, and literary texts, adjudicated against contemporaneous drafting history of the amendment and state analogue provisions.',
    'If the militia-confined idiom predominates, this reading''s textual foundation strengthens and its low-ε settlement looks like faithful construction; if broader usage predominates, the settlement is overlay on the text and its interpretive authority weakens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_semantic_evidence, empirical, 'Whether the reading''s prefatory-clause confinement reflects founding-era semantics.').

omega_variable(
    militia_obsolescence_permanence,
    'Is the death of independent state military capacity permanent, or could decentralized state military institutions revive?',
    'Structural analysis of defense economics, federal preemption of military organization, and any state-level movements toward independent military capacity; treat revival as live only if organizational and fiscal prerequisites reappear.',
    'If revival is possible, the founding problem is dormant rather than dead and this reading''s protective function could reactivate, changing the mandatrophy verdict; if permanent, the reading''s remaining life is purely interpretive and its zombie phase is terminal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_obsolescence_permanence, empirical, 'Whether the founding problem''s death is irreversible.').

omega_variable(
    heller_supersession_scope,
    'Does federal judicial repudiation in 2008 terminate this reading, or merely demote it to a live minority position sustained by state constitutional analogues and ongoing scholarship?',
    'Track post-2008 state supreme court treatment of state militia-and-arms clauses, continued scholarly output, and any federal doctrinal retrenchment; classify as terminated only if no institutional venue sustains the reading.',
    'If terminated, this story''s status is historical and its measurements close at 2008; if demoted-but-live, the reading remains an operative alternative whose reinstatement would restore the entire beneficiary structure authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heller_supersession_scope, conceptual, 'Scope of the 2008 repudiation: termination versus demotion of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__collective_right_reading, theater_ratio, 1791, 0.08).
narrative_ontology:measurement(seco_tr_t1830, second_amendment_arms_right__collective_right_reading, theater_ratio, 1830, 0.1).
narrative_ontology:measurement(seco_tr_t1870, second_amendment_arms_right__collective_right_reading, theater_ratio, 1870, 0.18).
narrative_ontology:measurement(seco_tr_t1903, second_amendment_arms_right__collective_right_reading, theater_ratio, 1903, 0.3).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__collective_right_reading, theater_ratio, 1939, 0.42).
narrative_ontology:measurement(seco_tr_t1970, second_amendment_arms_right__collective_right_reading, theater_ratio, 1970, 0.52).
narrative_ontology:measurement(seco_tr_t1995, second_amendment_arms_right__collective_right_reading, theater_ratio, 1995, 0.58).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.65).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.08).
narrative_ontology:measurement(seco_be_t1830, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1830, 0.09).
narrative_ontology:measurement(seco_be_t1870, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1870, 0.14).
narrative_ontology:measurement(seco_be_t1903, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1903, 0.16).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1939, 0.2).
narrative_ontology:measurement(seco_be_t1970, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(seco_be_t1995, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1995, 0.26).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1791, 0.05).
narrative_ontology:measurement(seco_su_t1830, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1830, 0.06).
narrative_ontology:measurement(seco_su_t1870, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1870, 0.15).
narrative_ontology:measurement(seco_su_t1903, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1903, 0.18).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1939, 0.3).
narrative_ontology:measurement(seco_su_t1970, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(seco_su_t1995, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2008, 0.04).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Second Amendment right' decomposes into three structurally distinct constraints — one per reading of the shared kernel (second_amendment_arms_right). This file is the collective_right_reading member: states as rights-holders, individual ownership regulable, low ε on prohibition measures. The individual_right_reading member inverts the structure (persons as rights-holders; regulatory measures bear high ε because the arrangement blocks them). The civic_republican_reading member occupies the intermediate position (armed citizenship as the protected subject; obligation-bearing structure). The members share a fixed-text kernel but differ in ε, beneficiary/victim sets, and type; they are linked here so contamination and legitimacy-pressure propagate across the family rather than being silently averaged inside any single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
