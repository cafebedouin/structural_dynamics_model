% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Capital Punishment as Proportional Desert (Lex Talionis Reading)
 *   domain: criminal justice / political philosophy / legal theory
 *
 * SUMMARY:
 *   In retentionist jurisdictions, a person convicted of murder may be
 *   sentenced to death on the ground that murder forfeits the perpetrator's
 *   right to life: the execution is what proportional desert says justice
 *   owes, not an instrument for producing future safety. This story models
 *   that arrangement as it operates — the sentencing adjudication that
 *   affirms desert, the appellate layers that re-affirm it, the confinement
 *   that precedes the act, and the act itself. The arrangement presents
 *   itself as the moral settlement of the community's gravest wrong; its
 *   operation takes the condemned person's remaining life, and — in the error
 *   tail its own adjudications cannot catch in time — occasionally the life
 *   of someone innocent of the crime. Survivors are promised vindication;
 *   officeholders bank the arrangement's visible proceeds; the machinery's
 *   costs are absorbed by administrators and jurors who did not design it.
 *   Sibling readings of the same underlying commitment (a
 *   deterrence-justified variant and a dignity-abolitionist variant) are
 *   modeled as separate constraint stories and linked through
 *   network.affects_constraints; this file instantiates only the desert
 *   reading and hedges nothing across readings. KEY AGENTS (by structural
 *   relationship): - condemned_murder_convicts: Full-charge bearer
 *   (powerless/trapped) — forfeits the remaining life under an affirmed
 *   desert finding - wrongly_condemned_defendants: Identical procedural seat
 *   without the crime (powerless/trapped) - homicide_victims_survivors:
 *   Declared recipients of the promised settlement (organized/constrained),
 *   internally split on delivery - law_and_order_officeholders: Visible
 *   collector of the arrangement's proceeds (powerful/arbitrage) -
 *   state_penal_authorities: End-to-end administrator
 *   (institutional/arbitrage), absorbing the machinery's litigation load -
 *   capital_jury_citizens: Compelled near-symmetric participant
 *   (moderate/constrained, local seat) - abolition_advocacy_movements:
 *   Contestant with no admissible seat inside the adjudication
 *   (organized/mobile) - international_human_rights_bodies: External monitor
 *   and pressure source (global)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.87).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.82).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Capital Punishment as Proportional Desert (Lex Talionis Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal justice / political philosophy / legal theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '0c547762-1091-4d9b-91f3-6a8f1b9e6930').
narrative_ontology:cs_kernel_codification('0c547762-1091-4d9b-91f3-6a8f1b9e6930', formalized).
narrative_ontology:cs_authority_grounding('0c547762-1091-4d9b-91f3-6a8f1b9e6930', lineage).
narrative_ontology:cs_interpretation_layer_present('0c547762-1091-4d9b-91f3-6a8f1b9e6930').
narrative_ontology:cs_reading_relation('0c547762-1091-4d9b-91f3-6a8f1b9e6930', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c547762-1091-4d9b-91f3-6a8f1b9e6930', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('0c547762-1091-4d9b-91f3-6a8f1b9e6930', foundational, desert_forfeits_life_right).
narrative_ontology:cs_axiom_status(desert_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('0c547762-1091-4d9b-91f3-6a8f1b9e6930', desert_forfeits_life_right, deontological).
narrative_ontology:cs_axiom('0c547762-1091-4d9b-91f3-6a8f1b9e6930', foundational, talionis_proportional_equivalence).
narrative_ontology:cs_axiom_status(talionis_proportional_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('0c547762-1091-4d9b-91f3-6a8f1b9e6930', talionis_proportional_equivalence, deontological).
narrative_ontology:cs_reference_frame('0c547762-1091-4d9b-91f3-6a8f1b9e6930', classical_lex_talionis_frame).
narrative_ontology:cs_drift_state('0c547762-1091-4d9b-91f3-6a8f1b9e6930', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c547762-1091-4d9b-91f3-6a8f1b9e6930', '2026-08-04T00:00:00Z').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, homicide_victims_survivors).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, law_and_order_officeholders).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_murder_convicts).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, wrongly_condemned_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, state_penal_authorities).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, capital_jury_citizens).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, capital_jury_citizens).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_desert_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death for murder after a desert-affirming adjudication. From sentencing forward, every official layer — appeals, clemency review, warrant scheduling — proceeds on the finding that the sentence is what justice owes them. They live under a countdown they did not choose and cannot negotiate; their remaining years pass in maximum-security confinement until the state takes the remainder of their lives. Exit means a governor's clemency signature, a court overturning the finding, or nothing.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_murder_convicts, payer,
    powerless, immediate, trapped, national).

% Occupy the identical procedural position — a death sentence and the same countdown — without having committed the crime. Their status is a determination error the process is built not to catch in time; proof of innocence, when it arrives at all, arrives through luck, journalism, or decades of volunteer litigation. Until then they stand exactly where the guilty stand, and after execution no correction reaches them.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, wrongly_condemned_defendants, payer,
    powerless, immediate, trapped, national).

% Lost a family member to murder and are told the killer's execution will settle the account. Some organize to support capital sentences, attend executions, and report relief; others find the promised settlement never arrives and organize against further executions. Either way their stake was created by someone else's crime and cannot be discharged except through the state's answer.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, homicide_victims_survivors, beneficiary,
    organized, biographical, constrained, national).

% Prosecutors who seek death sentences and elected officials who campaign on delivering them. Each capital case adds a career line — convictions won, toughness signaled — and each attempt to retire the practice becomes campaign material against whoever proposed it. They are not themselves subject to the sentence and can leave office freely; the arrangement's continuation is legible in their records and platforms.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, law_and_order_officeholders, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, law_and_order_officeholders, agenda_setter).

% Judges who pronounce the sentence, governors who sign warrants or commute them, and correction departments that run death rows and carry out executions. They administer the machinery end to end, absorbing its litigation load, staffing burdens, and protocol controversies, while drawing institutional prestige from operating the state's gravest penal power.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_penal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, state_penal_authorities, beneficiary).

% Ordinary residents summoned to help decide whether a stranger lives or dies, then released back into their lives carrying the memory of the vote. Jury obligation binds them once seated; afterward they report lasting disturbance at rates correctional psychology treats as routine for the rooms they sat in, while also receiving the enactment of a shared moral order they are told their service secured.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, capital_jury_citizens, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, capital_jury_citizens, beneficiary).

% Campaign externally against every death sentence — litigating, legislating, petitioning governors, documenting botched executions — but hold no admissible seat inside a desert adjudication, where the proceeding's premise has already answered their category of objection. Their wins arrive from outside the room: statute votes, moratoria, clemency grants, treaty pressure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolition_advocacy_movements, excluded,
    organized, generational, mobile, global).

% Treaty bodies and courts that monitor retentionist states, publish findings, and press for commutation. They can attach reputational and material consequences at the state level but cannot reach into a domestic sentencing docket; their reports arrive as external weather, not as procedure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, law_and_order_officeholders).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single lawful channel for the community's response to its gravest crime: it bounds retaliatory response to measured proportion, retires private vendetta by monopolizing the answer, and publicly enacts the community's commitment that deliberate killing receives the gravest sanctioned reply.
% TRANSFER_FUNCTION: Moves the condemned person's remaining life, and with it their standing ever to contest the finding, from the offender to the community's ledger of settled justice. Secondarily it moves money: capital cases cost jurisdictions multiples of equivalent life sentences, transferred from taxpayers to the enlarged machinery of specialized trials, appellate layers, and execution protocols.
% ABSENT_VOICES: The executed are the structurally absent voice: once the sentence is carried out, the one party with first-person knowledge of the finding's correctness can never speak again, and the wrongly condemned among them exit without ever having been heard. Inside the adjudication itself, categorical-dignity objection has no admissible seat — a proceeding premised on desert has already answered that category — so abolitionist jurists address the arrangement only from outside it: statutes, moratoria, clemency petitions, treaty bodies.
% DISAPPEARANCE_RATIONALE: The parties dispute what vanishing would do. On one account the moral order rearranges: the community's gravest wrong would go unanswered at scale, retaliation would migrate to private channels, and survivors' claims to settlement would be officially refused. On the other account almost nothing rearranges: every jurisdiction that has retired the practice continues to bound violence through policing and life imprisonment without any resurgence of feud, and homicide-response continues unchanged. Both accounts are held by parties with standing, so the verdict is recorded as contested rather than resolved from this seat.
% FOUNDING_PROBLEM: Before lawful proportional penalty, a killing created a debt that kin collected personally, with escalation beyond measure and cycles that outlived the original parties; the talionic rule bounded the answer to measured desert and vested it in a single authorized hand.
% FOUNDING_PROBLEM_CORROBORATION: Historians of early penal law corroborate the feud-bounding origin (wergild-to-talion-to-state-monopoly genealogies); comparative criminology corroborates the administrative-obsolescence side — abolitionist democracies show no resurgence of private retaliation. No corroborating source outside the arrangement's beneficiary set attests the founding problem as still live in consolidated jurisdictions; retentionist officials assert liveness from inside the beneficiary set, which the provenance rule discounts.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, contested).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.87, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored high (0.87) because the arrangement's charge on its target is the entire remaining life, collected irreversibly once the desert finding is affirmed; the desert framing governs whether the taking is called just, not how much of the person it takes. The wrongful-conviction tail compounds this: a fraction of charges fall on non-deserving bearers with no compensating mechanism. Suppression (0.82) is authored raw and is NOT scaled by power or scope — it reflects the near-total closure of alternatives at the point of application: after sentencing, every official layer presumes desert, clemency is discretionary and rare, and the categorical-dignity alternative is out of order inside the adjudication. Theater (0.26): the core function is really carried out, but closure-rhetoric, solemnity scripting, and humane-execution claims have grown as usage declined, raising the performative share. Accessibility_collapse (0.62): once desert is affirmed inside the frame, mercy and commutation lose standing and alternatives collapse for the condemned — but the frame itself stays externally contestable, so collapse stops well short of natural-law completeness. Resistance (0.58): sustained abolition campaigning, moratoria drives, and international pressure meet a machinery that persists. The temporal series share one grid (t=0,15,30,45,60,75) with every tracked metric authored at every point: base_extractiveness climbs monotonically (procedural elaboration lengthens confined person-years per case; deterrence-collapse concentrates the load-bearing justification onto desert alone), suppression_requirement hardens with the machinery (appellate layers that re-affirm, clemency contraction, secrecy statutes), theater drifts upward slowly as each execution carries more symbolic freight. The rising extractiveness series is T17-advisory — it feeds the accumulation hypothesis for investigation, not a reclassification.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply apart. The condemned and the wrongly condemned occupy the deepest-charge position: full charge, no exit, no post-sentencing voice — and they differ only in standing, not in what is taken, since the machinery cannot price determination errors into the charge. Survivors sit near the beneficiary end but split internally: the promised settlement demonstrably fails to arrive for a measurable share, which is routed to the closure-delivery omega rather than averaged away. Law-and-order officeholders collect visibly (conviction records, campaign positioning) while bearing none of the charge and holding arbitrage-grade exit from the whole matter. Penal authorities both run the machinery and absorb its litigation load — a cost-absorbing administrator whose net position sits nearer symmetry than a beneficiary label suggests; if the engine's derived directionality for that seat lands above roughly 0.4, an institutional override at approximately 0.32 is the indicated correction. Capital jury citizens are the near-symmetric seat: compelled participation and documented lasting psychological cost on one side, the enactment of a shared moral order on the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: homicide_victims_survivors and law_and_order_officeholders are declared beneficiaries (low d); condemned_murder_convicts and wrongly_condemned_defendants are declared victims with trapped exit and no power (d pinned near the full-target end). capital_jury_citizens carries payer with a secondary beneficiary role and should derive near-symmetric from the paired declarations. state_penal_authorities carries agenda_setter with a secondary beneficiary role — the prestige collection is declared so the derivation reads the administrator's net position rather than guessing from the power atom. vindicated_propositions (lex_talionis_principle, proportional_desert_doctrine) are deliberately kept out of the beneficiary arrays: doctrines collect no rents; the actors who bank the arrangement's proceeds are named as beneficiaries instead. No directionality_overrides are authored: the structural data (paired roles, trapped exit, absorbed administrative cost) is declared precisely so the derivation chain can read it, and the commentary flags the one seat (state_penal_authorities) where a correction would be indicated if the derivation misses the cost-absorption.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bounding unbounded blood revenge behind a single lawful proportional answer — is the arrangement's original mandate, and in consolidated states its administrative substance is disputed: policing and life-imprisonment capacity now perform the feud-bounding function everywhere the practice has been retired, without feud resurgence. The classification guards against two opposite mislabels. Reading the arrangement as pure coordination would erase the life-charged target and the officeholder receipt surface; reading it as pure extraction would erase the genuine channeling function that historically retired vendetta and still structures the community's answer to its gravest wrong. The hybrid claim keeps both halves on the table while the receipt surface (visible officeholder capture) and the R5 interview (contested-status founding problem, with no outside corroboration of liveness) expose where the arrangement is drifting from mandate toward maintained performance. If closure-delivery continues to fail and the channeling function stays vestigial in consolidated jurisdictions, the drift path runs toward extractive operation on a thinning coordination story — the temporal series is built to catch exactly that transition, and the wrongful-conviction omega prices the error tail that would complete it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Which reading of the state_killing_legitimacy kernel does this constraint instantiate, and what would each sibling reading change structurally?',
    'Fixed by authorship: this file instantiates the retributive_reading. The deterrence sibling relocates legitimacy from moral desert to empirical signal efficacy, making guilt necessary but not sufficient and the arrangement hostage to deterrence evidence; the abolition sibling deletes desert legitimacy entirely, recasting the executed from deserving settler-of-account to rights-bearing person and driving the charge toward the dignity-violation ceiling with no coordination half at all. The disagreement is located at the axiom layer — whether desert can ground life-taking at all — not at any factual question.',
    'If the kernel''s operative reading shifted to deterrence, the charge on the condemned would persist but the justification becomes falsifiable by evidence; under abolition the arrangement computes as a categorical dignity violation with no residual coordination function, and this file''s beneficiary structure dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel, siblings as separate files.').

omega_variable(
    wrongful_conviction_tail,
    'What fraction of death sentences are carried out on persons innocent of the crime, and does the arrangement''s own adjudication catch them in time?',
    'Post-execution exoneration audits and matched cohort comparisons of death-row exoneration rates against execution volume, using innocence-project archives as case denominators.',
    'A material wrongful-execution rate means the charge repeatedly falls on non-deserving bearers with no compensating adjustment, pushing the arrangement''s effective profile toward pure extraction on that segment and sharpening the snare-side reading of the same structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_tail, empirical, 'Size of the error tail in the desert adjudication.').

omega_variable(
    closure_delivery_failure,
    'Does execution actually deliver the vindication and closure the arrangement promises to survivors?',
    'Longitudinal study of homicide survivors comparing closure outcomes where the perpetrator was executed versus sentenced to life; existing survey literature points toward frequent non-delivery.',
    'If delivery systematically fails, the survivor beneficiary declaration thins toward nominal, weakening the coordination half and drifting the computed type toward the snare side; if it holds, the hybrid reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_delivery_failure, empirical, 'Whether the promised settlement to survivors is actually delivered.').

omega_variable(
    vengeance_channel_necessity,
    'Is a lawful capital answer necessary to bound private vengeance, or do policing and life imprisonment perform the channeling adequately on their own?',
    'Natural experiment across abolitionist democracies: absence of feud resurgence after retirement of the practice; historical comparison with pre-state-monopoly periods where the channeling function was demonstrably load-bearing.',
    'If channeling is performed adequately without executions, the coordination function is vestigial in consolidated jurisdictions and the arrangement persists on desert legitimacy plus officeholder receipt — a materially thinner coordination story than the founding genealogy suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vengeance_channel_necessity, empirical, 'Whether the feud-channeling coordination function is still live or vestigial.').

omega_variable(
    desert_constructedness,
    'Is proportional desert an objective moral fact that licenses the life-for-life settlement, or a constructed norm whose operation concentrates identifiable gains?',
    'Cross-cultural moral-psychological study of desert intuitions and their variance; institutional tracing of whose positions improve under the norm''s operation.',
    'If constructed with identifiable gainers, the arrangement''s presentation as moral necessity is a false-summit pattern: the naturality claim would need re-authoring with the gainer set attached and a false-summit evaluation triggered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_constructedness, conceptual, 'Naturality versus construction of the desert foundation.').

omega_variable(
    suppression_internalization_split,
    'How much of the condemned person''s observed acquiescence is structural (no admissible exit once desert is affirmed) versus internalized (resignation, chaplaincy-mediated acceptance, identity collapse under prolonged death-row conditions)?',
    'Post-commutation trajectories: among condemned persons whose sentences were later commuted, if opposition and self-advocacy resume once the trap opens, the suppression was predominantly structural; persistent passivity after release from the sentence indicates internalized residue.',
    'Internalized components mean the arrangement''s coercive footprint outlasts its own machinery and effective suppression exceeds the structural measure; the split also determines what abolition would actually release.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized share of the condemned''s suppressed objection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t15, state_killing_legitimacy__retributive_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__retributive_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t45, state_killing_legitimacy__retributive_reading, theater_ratio, 45, 0.23).
narrative_ontology:measurement_basis(stat_tr_t45, observed).
narrative_ontology:measurement(stat_tr_t60, state_killing_legitimacy__retributive_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(stat_tr_t60, observed).
narrative_ontology:measurement(stat_tr_t75, state_killing_legitimacy__retributive_reading, theater_ratio, 75, 0.26).
narrative_ontology:measurement_basis(stat_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t15, state_killing_legitimacy__retributive_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__retributive_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t45, state_killing_legitimacy__retributive_reading, base_extractiveness, 45, 0.81).
narrative_ontology:measurement_basis(stat_be_t45, observed).
narrative_ontology:measurement(stat_be_t60, state_killing_legitimacy__retributive_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement_basis(stat_be_t60, observed).
narrative_ontology:measurement(stat_be_t75, state_killing_legitimacy__retributive_reading, base_extractiveness, 75, 0.87).
narrative_ontology:measurement_basis(stat_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t15, state_killing_legitimacy__retributive_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__retributive_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t45, state_killing_legitimacy__retributive_reading, suppression_requirement, 45, 0.78).
narrative_ontology:measurement_basis(stat_su_t45, observed).
narrative_ontology:measurement(stat_su_t60, state_killing_legitimacy__retributive_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement_basis(stat_su_t60, observed).
narrative_ontology:measurement(stat_su_t75, state_killing_legitimacy__retributive_reading, suppression_requirement, 75, 0.82).
narrative_ontology:measurement_basis(stat_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: 'when may the state kill a convicted person' is a single colloquial label covering at least three structurally distinct arrangements — desert-justified execution (this file: channeling function plus a life-charge on the condemned), deterrence-justified execution (empirically contingent legitimacy, hostage to signal-efficacy evidence), and abolition (no execution legitimacy; dignity-barred). Each carries its own epsilon, its own beneficiary/victim structure, and its own type; they are linked here via affects_constraints as a constraint family. Dependency direction: the deterrence sibling historically fronted the public justification; as its evidentiary base eroded, this reading's desert premise became the load-bearing rationale in retentionist jurisdictions — the family edges encode that dependency, and this file's persistence creates the structural conditions (a standing practice to oppose) under which the abolition sibling operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
