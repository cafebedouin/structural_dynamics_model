% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority — Abolition Reading (Categorical Impermissibility)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   state_execution_authority: the abolition reading, which holds that state
 *   execution is categorically impermissible regardless of crime severity or
 *   procedural safeguards. Per the committer-frame rules, the sibling
 *   readings (retributive_reading, deterrence_reading) are OTHER constraints
 *   in OTHER files; they are not averaged, hedged, or described inside this
 *   one. The epsilon referent is the standing arrangement under contest — the
 *   operating capital-punishment machinery of retentionist jurisdictions —
 *   assessed by this reading's own lights: the state takes life itself,
 *   irreversibly, from condemned persons who include the wrongfully
 *   convicted, under procedural safeguards that this reading holds legitimate
 *   the taking rather than cure it. Because the categorical axiom admits no
 *   substitution (life imprisonment is held qualitatively different, not a
 *   lesser dose of the same thing), epsilon sits at its practical ceiling and
 *   rises slightly across the interval as accumulated exonerations convert
 *   abstract fallibility into demonstrated systemic error. KEY AGENTS (by
 *   structural relationship): see key_agents; the defining structural fact is
 *   that the only seats with arbitrage-grade exit from this constraint are
 *   the seats that wield it.
 *
 * KEY AGENTS:
 *   - all_executed_persons: primary target (powerless/trapped) — bears the terminal extraction; per this reading the victim set includes the guilty, since the axiom rejects the act itself, not its misapplication
 *   - death_row_inmates: primary target (powerless/trapped) — bears years-to-decades of confinement under sentence of death while every exit (appeal, clemency) is controlled by the same apparatus
 *   - capital_charge_defendants: secondary target (powerless/constrained) — extracted from before trial via plea leverage, conceding guilt and waiver rights to escape the death charge
 *   - prosecuting_authorities: agenda-setter and collector (institutional/arbitrage) — decides which cases carry the death charge, converts the threat into pleas and convictions, and can step down to lesser charges at will
 *   - elected_officials: beneficiary (institutional/arbitrage) — converts the sanction into electoral capital and blocks repeal through manufactured political risk
 *   - closure_seeking_victim_families: beneficiary seat, internally divided (organized/identity_locked) — receive the symbolic settlement the procedure offers; a substantial minority of survivors reject it
 *   - execution_administrators: administering arm (institutional/constrained) — corrections systems that must procure drugs, staff executions, and absorb the documented attrition and refusal the task produces
 *   - appellate_judiciary: gatekeeping arm (institutional/constrained) — operates the review layer that this reading holds performs scrutiny while ratifying the overwhelming majority of outcomes
 *   - abolition_advocacy_movement: excluded from operative decision points (organized/constrained) — litigates, documents exonerations, and lobbies, but sits outside charging decisions and clemency boards where outcomes are actually determined
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — monitor, report, and treat the practice as a rights violation; no enforcement reach inside retentionist states
 *   - taxpayers: diffuse payer (moderate/mobile) — fund the capital track, whose trial, appellate, and death-row costs substantially exceed life-imprisonment costs in every audited jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.94).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.85).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.94).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority — Abolition Reading (Categorical Impermissibility)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '2659e97f-2402-47c8-aa7a-e86fc8e1089d').
narrative_ontology:cs_kernel_codification('2659e97f-2402-47c8-aa7a-e86fc8e1089d', fixed_text).
narrative_ontology:cs_authority_grounding('2659e97f-2402-47c8-aa7a-e86fc8e1089d', lineage).
narrative_ontology:cs_interpretation_layer_present('2659e97f-2402-47c8-aa7a-e86fc8e1089d').
narrative_ontology:cs_reading_relation('2659e97f-2402-47c8-aa7a-e86fc8e1089d', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('2659e97f-2402-47c8-aa7a-e86fc8e1089d', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('2659e97f-2402-47c8-aa7a-e86fc8e1089d', foundational, execution_categorical_impermissibility).
narrative_ontology:cs_axiom_status(execution_categorical_impermissibility, holdable).
narrative_ontology:cs_axiom_grounding('2659e97f-2402-47c8-aa7a-e86fc8e1089d', execution_categorical_impermissibility, deontological).
narrative_ontology:cs_axiom('2659e97f-2402-47c8-aa7a-e86fc8e1089d', secondary, wrongful_execution_systemic_illegitimacy).
narrative_ontology:cs_axiom_status(wrongful_execution_systemic_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2659e97f-2402-47c8-aa7a-e86fc8e1089d', wrongful_execution_systemic_illegitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('2659e97f-2402-47c8-aa7a-e86fc8e1089d', nonexecution_dignity_baseline).
narrative_ontology:cs_drift_state('2659e97f-2402-47c8-aa7a-e86fc8e1089d', contemporary_abolition_majority_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2659e97f-2402-47c8-aa7a-e86fc8e1089d', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, prosecuting_authorities).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, elected_officials).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, closure_seeking_victim_families).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, all_executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, capital_charge_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons the state has put to death after capital trial and appeal. Under this reading the victim set includes those guilty as charged, because the axiom condemns the act rather than its misapplication; it necessarily includes the wrongfully convicted, whose execution is held to be proof of the system's illegitimacy rather than an anomaly within it. Every exit ran through the apparatus itself: appeals decided by its courts, clemency granted or withheld by its officers. The harm is total and irreversible, and the seat is permanently silenced once the procedure completes.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, all_executed_persons, payer,
    powerless, biographical, trapped, national).

% Living prisoners under sentence of death, confined in restrictive death-row conditions for years to decades while their cases move through review. Their conduct, cooperation, and rehabilitation have no bearing on the outcome except at the margins of clemency discretion. Some will be exonerated after the fact; some will die by execution; some will die of age or illness inside the sentence. Exit from the category exists only through the machinery that imposed it.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_row_inmates, payer,
    powerless, biographical, trapped, national).

% Defendants facing a potential death charge at the charging and plea stage. The threat of the capital charge is used to extract guilty pleas, waiver of trial and appeal rights, and cooperation against others in exchange for a life or term sentence. Their realistic options are accepting the extracted plea or gambling their life on trial; declining both is not available.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, capital_charge_defendants, payer,
    powerless, immediate, constrained, national).

% Surviving family members of homicide victims who accept and articulate the settlement the capital process offers them: a verdict, an execution date, and a public ritual of finality. The state cites them as the constituency the sanction serves. The seat is internally divided — organized groups of survivors oppose execution and report the decades of appeals as renewed trauma rather than closure — and their standing in the process is fused with the case's identity, making dissent costly within it. Whether the gain is genuinely received or imputed by the procedure is an open question this story carries as an omega.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, closure_seeking_victim_families, beneficiary,
    organized, biographical, identity_locked, national).

% District attorneys and attorneys general who decide which cases carry the death charge, negotiate pleas under its shadow, and try capital cases. The charge converts directly into plea concessions, convictions, and career capital; the same office can step down to a lesser charge whenever the lever loses value, which is why its exposure to the constraint is voluntary and reversible. It sets the agenda for whom the machinery runs on at all.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, prosecuting_authorities, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__abolition_reading, prosecuting_authorities, beneficiary).

% Governors, legislators, and executives who campaign on the sanction, sign death warrants or commute them, and block or advance repeal. The issue yields reliable electoral capital at low personal risk because the risk of supporting repeal is managed by primary challenges and advertising funded around the issue. Their horizon is the next election cycle, which shapes every clemency and repeal decision they touch.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, elected_officials, beneficiary,
    institutional, immediate, arbitrage, national).

% Corrections departments and their staff who operate death rows and carry out executions. They are bound by statute once a warrant issues, and they absorb the operational frictions the task generates: pharmaceutical suppliers refusing to sell, staff turnover and refusal among execution teams, litigation over protocol secrecy, and botched procedures. They administer the constraint rather than profit from it, and their exit is statutory rather than chosen.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, execution_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% State and federal judges who operate the review layer: direct appeal, post-conviction, habeas. They vacate a small fraction of death judgments and ratify the overwhelming majority, working within doctrines (harmlessness, procedural default, deference standards) that channel what scrutiny can reach. Their legitimacy rests on the appearance of exhaustive review; they are bound by precedent and statute and cannot exit the role the constraint assigns them.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Defense lawyers, innocence organizations, advocacy coalitions, and religious bodies working to end the practice. They litigate post-conviction, document exonerations, support repeal campaigns, and accompany the condemned. They are absent from the rooms where outcomes are actually determined — charging decisions, clemency boards, warrant signings — and their influence must route through the same courts and legislatures the constraint's beneficiaries control. Their knowledge of the machinery's failures is the most detailed of any seat and carries no formal vote.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolition_advocacy_movement, excluded,
    organized, generational, constrained, global).

% Treaty bodies, regional courts, and rights organizations that classify the practice as a violation of evolving international norms and press retentionist states through reporting, review, and diplomatic pressure. They take testimony from every seat, compile comparative data, and hold no enforcement power inside retentionist jurisdictions. Their analytical distance is their contribution: they document the direction of travel of the norm itself.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% Residents who fund the capital track: capital-case prosecution and defense, the extended appellate pipeline, death-row housing, and execution operations, which audited comparisons place substantially above the lifetime cost of life imprisonment. The cost is diffuse and invisible in ordinary budget perception, and individual exit (moving jurisdictions) is weakly related to the fiscal flow. They bear a real, documented cost without choosing it.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, taxpayers, payer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, prosecuting_authorities).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Caps the criminal-penalty hierarchy with a terminal sanction above which no further escalation exists, and channels public demand for maximal response to heinous crimes into a regulated judicial procedure; historically it also coordinated the sovereign's display of ultimate punitive authority in eras before long-term incarceration existed.
% TRANSFER_FUNCTION: Moves life itself, plus the years of death-row confinement preceding it, from condemned persons to the state's punitive account; moves plea concessions and waived rights from capital defendants to prosecutors; moves fiscal resources from taxpayers to the capital track, which costs more than the life-imprisonment alternative; moves a symbolic settlement to closure-seeking constituencies; and moves electoral capital to officials who defend it.
% ABSENT_VOICES: The executed are the structurally absent voice: the seat with the most complete knowledge of the constraint's operation is the one the constraint deletes, and it cannot testify. With them: the wrongfully convicted exonerated only after decades or posthumously, the condemned's families and communities, foreign nationals processed without consular access, and the minority of surviving family members whose opposition to execution is absorbed out of the official narrative. Unanimity in favor of the arrangement is procured partly by the absence of the seat best positioned to describe it.
% DISAPPEARANCE_RATIONALE: If the sanction vanished overnight, charging patterns, plea-bargaining leverage, penalty hierarchies, clemency machinery, death-row infrastructure, and tough-on-crime political rhetoric would all reorganize around the new terminal point; victim-services framing and international human-rights posture would shift with them. The rearrangement is not hypothetical: dozens of abolitionist jurisdictions have already performed it and continue to prosecute the same categories of crime, which maps the route the retentionist world would follow.
% FOUNDING_PROBLEM: In eras before the penitentiary existed, the sovereign faced two problems at once: how to permanently remove offenders deemed beyond redemption when no long-term incarceration infrastructure was available, and how to display the ultimacy of its authority to punish. Capital punishment solved both with a single instrument.
% FOUNDING_PROBLEM_CORROBORATION: Penal-history scholarship on the emergence of the penitentiary documents both halves of the founding problem and their solution by incarceration rather than execution. Comparative practice corroborates from outside the beneficiary set: every abolitionist democracy prosecutes and contains the same categories of crime using life imprisonment, demonstrating the removal function survives the instrument's removal. Exoneration registries maintained independent of the state attest the error burden the arrangement carries. Retentionist offices dispute the 'dead' status by asserting continuing retributive demand, but no source outside the benefiting parties attests that the founding problem remains unsolved by other means.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.94, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claimed type is snare because, from this reading's seat, the arrangement's coordination stories (deterrence, retribution, incapacitation) are cover: the categorical axiom denies that any quantity of deterrence or proportionality could justify the act, so the stories cannot launder the extraction into coordination. The metrics describe the practice's actual operation as this reading assesses it. Extractiveness 0.94: the extracted good is life itself, taken irreversibly, from a set that provably includes the innocent; no substitute exists under the reading's axiom. Suppression 0.85 is a raw structural property, unscaled: the condemned cannot exit, alternatives are statutorily foreclosed for capital crimes, clemency is discretionary, and repeal-facing officeholders face beneficiary-managed political risk. Theater_ratio 0.52: the procedural layer is partly real (DNA-era exonerations prove the review machinery occasionally functions) but predominantly legitimation-performance — it ratifies the overwhelming majority of death judgments while displaying scruple. Accessibility_collapse 0.78: for a defendant inside the capital process, alternatives collapse nearly completely; at the societal level comparative abolitionist jurisdictions keep alternatives visibly alive, which keeps the value below mountain-range collapse. Resistance 0.62: sustained moratoria, declining death sentences, exoneration-driven litigation, juror reluctance, and lethal-injection drug shortages impose real friction without yet displacing the practice in retentionist jurisdictions. The temporal series share one grid (T0=1972 Furman vacuum through T50=2022). Note the deliberate dissociation in the series: suppression_requirement peaks around T30 (post-AEDPA enforcement hardening) and then decays as drug shortages, sentencing decline, and moratoria erode active enforcement capacity, while base suppression stays high — the trap's structure now does the coercive work the machinery used to do. Base_extractiveness creeps upward as accumulated exonerations deepen the demonstrated-illegitimacy record; theater_ratio climbs as the procedural display grows relative to the shrinking number of carried-out executions.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical legal material. The prosecuting_authorities seat experiences a lawful discretionary tool with arbitrage-grade exit — the charge is a lever it may pull or release, so effective extraction toward it is minimal and possibly negative (the lever subsidizes its conviction statistics). The condemned seats experience total, irreversible extraction with zero exit. The closure_seeking_victim_families seat receives a symbolic settlement whose authenticity is itself contested (see omega closure_gain_authenticity). The appellate_judiciary experiences a functioning review institution; the abolition movement experiences a review theater. The engine computes these divergences from power, exit, and role data; this story authors the structure and refuses to reconcile the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides are needed. all_executed_persons and death_row_inmates: payer role plus trapped exit places them at the full-target end — the highest effective extraction the engine can compute, appropriate for a constraint whose extracted good is life. capital_charge_defendants: payer with constrained exit (the plea is the only door, and it is the extraction mechanism) — high d. prosecuting_authorities: agenda_setter with secondary beneficiary role and arbitrage exit — derived d sits near the beneficiary end; the arbitrage atom matters because their exposure to the constraint is voluntary and reversible. elected_officials: beneficiary with arbitrage exit — low d. closure_seeking_victim_families: beneficiary, identity_locked — low d; the reading's objection to this gain is registered in the omegas and axioms, not in d, because directionality encodes structural receipt, not legitimacy of receipt. taxpayers: payer, mobile — moderate-high d, diluted by diffuseness. The excluded and observer seats feed the consensus-provenance picture rather than the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — permanent removal of offenders deemed beyond redemption in an era before long-term incarceration existed, plus sovereign display of ultimate punitive authority — is dead on its own terms: the penitentiary and life-without-parole solve the removal problem everywhere, and every abolitionist democracy manages identical crimes without the terminal sanction. The arrangement persists past its function, which is the classic mandatrophy signature; the R5 mismatch (founding_problem_status=dead x disappearance_verdict=world_rearranges) should fire the zombie/capture flag, and per this reading that flag is correct rather than spurious. The classification discipline matters here in both directions: calling this a rope would launder the cover stories (deterrence, closure) into genuine coordination the participants endorse; calling it a piton would miss that concentrated collectors exist (prosecutorial leverage, electoral capital) and that the extraction is acute, not vestigial. The snare classification holds both facts: real coordination-shaped activity surrounds the practice, and the practice itself is extraction whose persistence depends on coercion and on suppressing the exits (repeal, clemency, refusal) that would dissolve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the kernel state_execution_authority (reading: abolition_reading). Where exactly is the disagreement with the sibling readings located, and what would adopting a sibling change structurally?',
    'Comparative structural audit of the sibling story files: the retributive reading shrinks the victim set to wrongfully executed persons only and installs closure-seeking constituencies as legitimate beneficiaries; the deterrence reading makes epsilon contingent on an empirical preventive effect and weighs executed persons against hypothetically saved lives. The disagreement is located in the justifiability premises, not in any factual predicate both sides share.',
    'If a sibling reading were adopted instead, the victim set contracts sharply, beneficiaries appear where this reading finds none, and epsilon drops from ~0.94 to a fraction of that value; the categorical axiom is what fixes the victim set at all executed persons and epsilon at its maximum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: this file is the abolition reading of the state_execution_authority kernel; sibling readings instantiate different constraints with different victim sets and epsilon.').

omega_variable(
    deterrence_effect_null_question,
    'Is the deterrent effect of execution actually null, as this reading asserts, or merely unmeasured?',
    'Natural experiments: moratoria and matched-jurisdiction comparisons of homicide rates; the National Research Council''s 2012 review found the existing literature insufficient to conclude any deterrent effect.',
    'This reading''s classification is consequence-insensitive (the categorical axiom holds regardless), so even a large measured effect would not flip the type; it would however change the transfer-function analysis and strengthen the retentionist coalition''s cover story, raising measured resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_effect_null_question, empirical, 'Whether the coordination cover story (deterrence) has any empirical content.').

omega_variable(
    closure_gain_authenticity,
    'Do surviving family members genuinely receive a benefit (closure, settlement) from execution, or is that gain imputed onto them by the state''s procedure?',
    'Longitudinal studies of surviving families in execution versus life-sentence cases; the documented internal division of victims'' families (organizations of family members opposing execution) is already partial evidence.',
    'If the closure gain is imputed rather than received, the beneficiary declaration for closure_seeking_victim_families weakens toward pure rhetorical deployment and the extraction profile approaches unanimity of victimhood; if genuine, a real (though, per this reading''s axiom, insufficient) benefit stream exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_gain_authenticity, empirical, 'Whether the closure-seat beneficiary declaration reflects received gain or state-imputed gain.').

omega_variable(
    lwop_substitutability,
    'Is life imprisonment without parole a qualitative substitute for execution (making the marginal extraction smaller) or categorically different, as this reading holds?',
    'Conceptual analysis of irreversibility, and revealed preference: jurisdictions that substituted LWOP report no loss of incapacitation function, while this reading holds that remediability is itself the morally decisive property.',
    'If substitution were accepted, effective extraction of the marginal execution falls and the arrangement looks more like a harsh-but-revisable sanction; this reading''s categorical stance denies substitutability, which is what keeps epsilon at its ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lwop_substitutability, conceptual, 'Whether the no-substitution premise of the categorical prohibition holds.').

omega_variable(
    death_sentence_error_rate,
    'What is the true rate of wrongful conviction among death sentences?',
    'Exoneration registries and survival-analysis of death-row exonerations (the leading study estimates roughly 4% of death-sentenced defendants would be exonerated if left under sentence indefinitely); DNA-era reversal data.',
    'Under this reading any nonzero rate is dispositive given irreversibility — wrongful execution is treated as proof of systemic illegitimacy, not as a tunable error budget — so higher discovered rates raise epsilon and resistance but do not change the type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(death_sentence_error_rate, empirical, 'Empirical error rate feeding the systemic-illegitimacy axiom.').

omega_variable(
    persistence_preference_vs_suppression,
    'Why does a mechanically cheap-to-fix constraint persist — genuine constituent preference for the sanction, or political risk manufactured and enforced by the constraint''s own beneficiaries?',
    'Compare jurisdictions where repeal passed with versus without prior polling majorities; track officeholder statements and primary-challenge patterns around repeal votes; observe post-repeal electoral outcomes in abolishing jurisdictions.',
    'If persistence is manufactured risk, the suppression component is beneficiary-produced and the snare classification tightens; if genuine preference, disappearance would be slower and more contested than the mechanical cheapness suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_preference_vs_suppression, preference, 'Whether persistence rests on authentic demand or beneficiary-enforced political risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_exec_abolition_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.33).
narrative_ontology:measurement(state_exec_abolition_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(state_exec_abolition_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(state_exec_abolition_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(state_exec_abolition_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement(state_exec_abolition_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(state_exec_abolition_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.86).
narrative_ontology:measurement(state_exec_abolition_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.88).
narrative_ontology:measurement(state_exec_abolition_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.9).
narrative_ontology:measurement(state_exec_abolition_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.91).
narrative_ontology:measurement(state_exec_abolition_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.93).
narrative_ontology:measurement(state_exec_abolition_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.94).

% Suppression requirement over time
narrative_ontology:measurement(state_exec_abolition_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(state_exec_abolition_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(state_exec_abolition_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(state_exec_abolition_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(state_exec_abolition_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(state_exec_abolition_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'capital punishment' conflates three structurally distinct constraints corresponding to the three declared readings of the state_execution_authority kernel. This file authors the abolition reading (epsilon ~0.94, victim set = all executed persons including the guilty, no legitimate beneficiaries, categorical no-substitution premise). The sibling files author the retributive reading (victim set contracted to the wrongfully executed; closure-seeking constituencies enter as legitimate beneficiaries; epsilon materially lower) and the deterrence reading (epsilon contingent on an empirical preventive effect; executed persons weighed against hypothetically saved lives). Upstream/downstream structure: the deterrence reading's empirical claims have historically been cited as the load-bearing justification for retention, so the deterrence file influences the other two; the abolition reading contests both siblings' justifiability premises outright. Each file links the others via network.affects_constraints; contamination analysis should expect abolition-side purity findings to propagate hardest onto the deterrence file, whose empirical floor is the first thing exoneration data erodes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
