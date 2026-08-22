% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor-Settlement Legitimacy: the Dueling Norm (Composite Reading)
 *   domain: historical sociology / legal history / cultural anthropology
 *
 * SUMMARY:
 *   The standing arrangement under contest is the honor-settlement legitimacy
 *   regime: the normative structure that made challenge-and-combat the
 *   legitimate — and for honor-bearing men, effectively compulsory — mode of
 *   dispute settlement from the height of the code duello (c. 1770) to its
 *   terminal remnants (1940). This file instantiates the composite_reading of
 *   the kernel honor_settlement_legitimacy: the decline was overdetermined,
 *   with cultural contraction (dueling becoming cognitively unthinkable) as
 *   the dominant edge, reinforced by material and institutional changes —
 *   state prohibition, jury nullification's erosion of class protection,
 *   officer professionalization, urban anonymity, and mass-press reputation
 *   markets — each of which would independently have suppressed practice.
 *   Epsilon's referent is the standing dueling arrangement itself as this
 *   reading assesses it, never the court-mediated settlement order the
 *   reformers endorsed. Authoring convention: the base_properties scalars
 *   record the interval-end (terminal) snapshot, matching the final
 *   measurement points; the operative arc lives in the measurement series.
 *   The claimed type describes the arrangement's historical structure —
 *   genuine rule-governed settlement coordination bound to coerced lethal
 *   risk under active enforcement — while the terminal scalars describe what
 *   it decayed into; that divergence is the lifecycle signal, not an
 *   inconsistency.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_class: primary beneficiary and collective agenda_setter (institutional/identity_locked) — accrued standing and internal order; administered the codes and the sanction machinery
 *   - military_officer_corps: secondary beneficiary (institutional/identity_locked) — collected discipline and the courage screen while its juniors bore the mortality
 *   - code_duello_seconds_and_arbiters: agenda_setter (organized/identity_locked) — ran negotiation, terms-drafting, and adjudication day to day
 *   - coerced_duel_participants: primary target (moderate/trapped) — bore coerced lethal risk under sanction
 *   - ostracized_refusers: target who exited at cost (moderate/constrained) — paid social death; their unpunished survival eroded the sanction
 *   - duel_casualties_and_dependents: terminal bearers (powerless/trapped) — the killed and maimed and their families; no seat in the code
 *   - state_criminal_jurisdiction: excluded institutional actor (institutional/constrained) — statutes nullified by honor-class juries; a parallel jurisdiction it could not reach
 *   - anti_dueling_reformers: analytical observer (organized/analytical) — published casualty rolls, lobbied statutes, and prosecuted seconds from outside the settlement conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.22).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.35).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor-Settlement Legitimacy: the Dueling Norm (Composite Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical sociology / legal history / cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '88748e94-2fff-4ec1-87aa-2f580c1ea0ee').
narrative_ontology:cs_kernel_codification('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', formalized).
narrative_ontology:cs_authority_grounding('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', practice).
narrative_ontology:cs_interpretation_layer_present('88748e94-2fff-4ec1-87aa-2f580c1ea0ee').
narrative_ontology:cs_reading_relation('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', foundational, dueling_decline_overdetermined).
narrative_ontology:cs_axiom_status(dueling_decline_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', dueling_decline_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', foundational, cultural_contraction_dominant_edge).
narrative_ontology:cs_axiom_status(cultural_contraction_dominant_edge, holdable).
narrative_ontology:cs_axiom_grounding('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', cultural_contraction_dominant_edge, empirically_contingent).
narrative_ontology:cs_reference_frame('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', functioning_code_duello_settlement_regime).
narrative_ontology:cs_drift_state('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', post_great_war_terminal_period, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('88748e94-2fff-4ec1-87aa-2f580c1ea0ee', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, aristocratic_honor_class).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, coerced_duel_participants).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, duel_casualties_and_dependents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, ostracized_refusers).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, satisfaction_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, code_duello_proportionality_rules).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, courage_as_status_currency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The landed and titled class whose standing rested on courage publicly demonstrated and insult ritually answered. It set and revised the codes, staffed the courts of honor, and administered the sanction that made refusal costly: exclusion from clubs, regiments, and marriageable society. Its members accrued the standing the system distributed while bearing a steady toll of sons, brothers, and heirs killed in settlement. Exit was not available as an individual act: leaving the code meant leaving the class, and the class was who they were.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, aristocratic_honor_class, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, aristocratic_honor_class, agenda_setter).

% Officer corps across European armies treated challenge-acceptance as proof of fitness for command; a refused challenge could end a commission. The corps collected discipline and a courage screen from the practice — the timid were filtered out or reformed — while losing a steady share of its junior officers to dueling deaths and courts-martial. An officer's exit ran through resignation, which most could not afford and few could imagine.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, military_officer_corps, beneficiary,
    institutional, biographical, identity_locked, continental).

% Seconds, courts of honor, and club tribunals negotiated challenges, drafted terms, adjudicated whether an apology sufficed, and could defuse a duel entirely. They ran the system day to day, and their craft was keeping the ritual inside its rules. Their standing inside the class depended on discharging the role; declining a seconds' appointment marked a man.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, code_duello_seconds_and_arbiters, agenda_setter,
    organized, biographical, identity_locked, national).

% Men who did not believe in the code — religious dissenters, cautious professionals, younger sons without leverage — but accepted challenges because refusal carried social and professional death. They staked their lives on ritual combat they regarded as absurd, in garrison towns and club districts where the class's sanction reached. Their alternative was not another settlement forum; it was exile from the world their careers ran through.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, coerced_duel_participants, payer,
    moderate, biographical, trapped, national).

% The men who refused — early conscientious objectors to the code, and a growing stream as the norm weakened — and paid the published price: dropped from invitation lists, passed over for promotion, cut by former friends. Some bore it as principle. Their visible, unpunished survival was itself evidence the sanction was decaying, and each unpunished refusal lowered the cost of the next.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, ostracized_refusers, payer,
    moderate, biographical, constrained, national).

% The men killed or maimed in settlement and the widows, orphans, and dependents left carrying the loss. The code had no seat for them: a death in settlement was classified as honorable, closing the question before their claims could be voiced. In most jurisdictions dueling's illegality left them without civil recourse against survivors or estates.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, duel_casualties_and_dependents, payer,
    powerless, biographical, trapped, local).

% The courts and prosecutors who held statutes against dueling and watched juries of honor-class men acquit every case that mattered. The class's settlement channel operated as a parallel jurisdiction the state could not reach; its statutes were performed, not enforced. Its options were harsher statutes (nullified all the same), selective prosecution of seconds, or waiting for the class's own sanction machinery to decay.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_criminal_jurisdiction, excluded,
    institutional, generational, constrained, national).

% Religious societies, humanitarian campaigns, and later statistical reformers who published the casualty rolls, lobbied for statutes, prosecuted seconds, and argued the practice was murder dressed as honor. They stood outside the settlement conversation for a century — their objections were heard and disregarded — while accumulating the caseload evidence that the class's own young men would eventually read.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, anti_dueling_reformers, observer,
    organized, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, aristocratic_honor_class).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized, rule-governed settlement channel for disputes among honor-bearing men whom the ordinary courts could not or would not reach: codified rules of challenge, refusal, and combat; seconds who negotiated and could defuse; courts of honor and club tribunals that adjudicated standing. It channeled private violence into limited, mutually consented, ritual forms and made reputation a credible commitment device inside the class.
% TRANSFER_FUNCTION: Moved lethal risk and social standing. It compelled honor-bearing men, willing and unwilling alike, to stake life and limb on ritual combat; transferred standing between parties (satisfaction to the challenged, standing to the demonstrably courageous, dishonor to refusers); and extracted deference and career compliance from men who did not believe in the code, transferring order and status benefits to the class and its hierarchies.
% ABSENT_VOICES: The killed and maimed had no seat — the code classified their deaths as honorable settlement, closing the question their families would have opened. Widows, orphans, and dependents had no standing in any court of honor. The state's criminal jurisdiction was structurally excluded: anti-dueling statutes were routinely nullified by honor-class juries. Religious authorities condemned the practice from outside and were disregarded for a century. Men outside the honor class were never eligible for the code's protections or its obligations.
% DISAPPEARANCE_RATIONALE: At any point before roughly 1900, overnight disappearance forces immediate rearrangement: the honor class loses its dispute-settlement channel and its standing currency, officer corps lose their discipline mechanism and courage screen, and thousands of pending disputes revert to feud, litigation, or press warfare. By the terminal phase (post-1918) the arrangement is already remnant and overnight disappearance would change little — the rearrangement happened gradually across the interval, which is what the decline is.
% FOUNDING_PROBLEM: Private violence among armed elites positioned at or above the reach of ordinary law: feuds, ambushes, and escalating retaliation threatened both the class's internal order and the public peace. The founding problem was how to make elite reputation enforceable and elite violence limited where royal and state courts could not or would not arbitrate between gentlemen.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the state-formation literature (the monopoly-of-violence thesis and its historical applications) and historians of the duel attest that state courts, police, and mass-circulation press came to perform the settlement and reputation functions the code once provided, dissolving the founding problem. Honor-class memoirists themselves conceded the practice anachronistic from the 1880s onward. No living party attests that the founding problem persists.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The terminal snapshot records a remnant: extractiveness 0.22 because by 1940 the norm compelled almost no one — participation in the surviving enclaves (academic corps fencing with protective equipment, symbolic challenges) was substantially voluntary; suppression 0.35 because what enforcement remained was symbolic (corps codes, club etiquette) rather than sanctioning machinery; theater_ratio 0.80 because the surviving practice was overwhelmingly performative — honor displayed without lethal settlement. Accessibility_collapse is low (0.12): once the arrangement was understood as anachronistic, every alternative — courts, public apology, press rebuttal, simple refusal — stood open. Resistance is low (0.10) for the converse reason: nothing compelled anyone, so nothing was resisted. The measurements carry the operative arc on one shared grid. Extractiveness peaked around 1800 (0.70) when the class's sanction was credible and refusal ruinous, then fell monotonically as contraction proceeded. The suppression_requirement series is the composite reading's signature: an inverted U. Enforcement had to intensify through 1860 (0.55 to 0.70) precisely because contraction and legal prohibition were dissolving the norm's self-enforcement — a ratchet against decay — and then collapsed (0.70 to 0.35) as the class's sanctioning power itself dissolved. Theater rises fastest after the enforcement peak: the cultural contraction edge did the terminal work that enforcement could no longer do, which is the overdetermination claim rendered as data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the aristocratic_honor_class seat (beneficiary, identity_locked, generational horizon), the code was the class's constitution — settlement, standing, and courage-screen in one structure — and its decline reads as civilizational self-dissolution. From coerced_duel_participants (trapped, biographical), the same structure was compulsory Russian roulette administered by social pressure. From ostracized_refusers (constrained), it was a tax paid in social death for declining a wager they never entered. From state_criminal_jurisdiction (excluded, generational), it was a parallel court that tried its statutes and acquitted. The officer corps seat is genuinely dual: the corps collected discipline while its junior members paid the mortality — the beneficiary position sits at the corps level, the borne costs at the member level, and the corps' identity lock kept the two books from being audited against each other until contraction forced them open.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared at the collective level: the honor class accrued standing and internal order; the officer corps accrued discipline and a courage screen. Their identity-locked exit (leaving the code meant leaving the class or the commission) pushes their derived directionality toward the subsidized end — the arrangement constituted them. Payers are declared at the borne-cost level: coerced participants staked life under compulsion (trapped — no alternative forum, no affordable exit), casualties' dependents carried the losses with no seat and no recourse (powerless, trapped), and refusers paid the published sanction (constrained — they could exit, at cost, and their exiting is what eroded the norm). Trapped exit pushes payers toward the full-target end. The continental scope of the class and corps amplifies effective pressure on trapped payers: a refusal followed a man across jurisdictions and garrisons, so verification of compliance was hard while sanction was portable. The state's criminal jurisdiction is authored as an excluded stakeholder without a beneficiary or victim declaration — its position (statutes nullified by honor-class juries) is recorded in its situation and in absent_voices rather than in the structural arrays, so its directionality rides the canonical fallback. No directionality overrides are authored: the override mechanism is keyed to power atoms, and the institutional atom is shared by the class, the corps, and the state, so any single override would misdescribe the others.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 mismatch is authored and meant to fire: founding_problem_status is dead (the feud-among-armed-elites problem dissolved as states monopolized violence and courts reached gentlemen) while disappearance_verdict is world_rearranges (at any pre-1900 moment the arrangement was load-bearing). That mismatch, cross-checked against the theater path, is the mandatrophy signature: the mandate outlived its function by roughly two generations. The composite reading matters to the resolution because the function did not simply expire — it migrated. Courts, police, and mass-circulation press absorbed the settlement and reputation functions gradually while the class's enforcement still held, which is why theater_ratio rises (0.15 to 0.55) before extractiveness falls steeply: the arrangement was performing a settlement function that had already moved elsewhere, first under an enforcement ratchet (1830-1860), then under identity performance alone (1890-1940). A single-cause account would date the mandatrophy differently: the contraction-only sibling would find the mandate cognitively dead early and the enforcement era inexplicable; a materialist account would find the mandate institutionally dead early and the cultural persistence inexplicable. The overdetermined account is the one that explains both the ratchet and the performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_overdetermination_contest,
    'This story is the composite_reading of the kernel honor_settlement_legitimacy: does the overdetermination claim (contraction dominant, material/institutional changes independently sufficient) survive against the sibling contraction_reading, which holds cultural unthinkability as the sole sufficient cause?',
    'Jurisdiction-level natural experiments: compare decline trajectories where legal prohibition and professional restructuring preceded cultural shift against those where the honor language collapsed before statute (Britain''s lagging law versus France''s revolutionary rupture). If decline timing tracks cultural markers independent of institutional ones, the contraction reading gains; if either pathway alone shows sufficiency in some major jurisdiction, the composite holds.',
    'If contraction alone suffices, the material-reinforcement pathways drop from this reading''s structure and the story converges toward the contraction_reading sibling; if material factors alone suffice in any major jurisdiction, the contraction edge is demoted and the reading becomes a multi-causal account without a dominant term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_overdetermination_contest, empirical, 'Whether dueling''s decline was overdetermined with a dominant contraction edge, or reducible to a single sufficient cause.').

omega_variable(
    contraction_causal_direction,
    'Within the composite reading, is cultural contraction an independent mechanism or the downstream effect of the material changes — did the honor framework become unthinkable because enforcement, career incentives, and reputation markets changed first?',
    'Temporal sequencing within jurisdictions: date the collapse of honor vocabulary in correspondence, memoirs, and press against the dates of statutory change, army professionalization, and urban anonymity; lag analysis of which moved first.',
    'If contraction is downstream, the reading''s edge attribution is spurious — the true structure is material-first with cultural accommodation, and the terminal phase''s speed reflects accumulated material pressure rather than an independent cognitive shift; classification consequences ride on whether the terminal enforcement collapse was chosen or compelled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contraction_causal_direction, empirical, 'Causal direction between cultural unthinkability and material/institutional change in the decline.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if a sibling reading were adopted in place of this one — specifically, does drop_reading''s fringe-persistence account imply live coercion with later bearers, and does contraction_reading''s single-cause account change who counts as having been coerced?',
    'Re-author the sibling stories (honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading) and compare victim sets and terminal-state classification: drop_reading implies residual adherents still bound by corps codes — live bearers of the arrangement''s costs into the twentieth century and, in academic-fencing enclaves, beyond; contraction_reading implies the unwilling were released earlier and the enforcement ratchet was never load-bearing.',
    'Under drop_reading the arrangement''s terminal phase is a going concern with later victims rather than a remnant, which would move this kernel''s terminal classification from inertial to operative; under contraction_reading the material-suppression omegas lose their referent. The victim-set boundary between readings is the kernel''s live dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta across sibling readings of the honor-settlement kernel.').

omega_variable(
    honor_class_net_benefit_ambiguity,
    'Did the honor class as a whole net-gain from the settlement code it administered, or did a dominant subset (senior officers, club elders, politically secure magnates) capture its benefits while the costs fell on juniors and the unconnected?',
    'Casualty and challenge rolls by rank, age, and family standing: if challenge initiation and death rates concentrate among juniors and peripheral families while adjudication power sat with seniors, the capture reading holds; if initiation and death distributed evenly across the class, a mutual-insurance reading holds.',
    'If captured, the beneficiary declaration splits — the class''s senior stratum moves toward the receiving seat and its junior stratum toward the paying seats, raising measured asymmetry within the class and sharpening the extraction reading of the arrangement; if mutual, the arrangement''s coordination function is stronger than the current structure records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_class_net_benefit_ambiguity, empirical, 'Whether the code''s gains accrued to the honor class generally or to a dominant subset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1770, 1940).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1770, honor_settlement_legitimacy__composite_reading, theater_ratio, 1770, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1770, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__composite_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1830, honor_settlement_legitimacy__composite_reading, theater_ratio, 1830, 0.25).
narrative_ontology:measurement_basis(hono_tr_t1830, observed).
narrative_ontology:measurement(hono_tr_t1860, honor_settlement_legitimacy__composite_reading, theater_ratio, 1860, 0.38).
narrative_ontology:measurement_basis(hono_tr_t1860, observed).
narrative_ontology:measurement(hono_tr_t1890, honor_settlement_legitimacy__composite_reading, theater_ratio, 1890, 0.55).
narrative_ontology:measurement_basis(hono_tr_t1890, observed).
narrative_ontology:measurement(hono_tr_t1920, honor_settlement_legitimacy__composite_reading, theater_ratio, 1920, 0.72).
narrative_ontology:measurement_basis(hono_tr_t1920, observed).
narrative_ontology:measurement(hono_tr_t1940, honor_settlement_legitimacy__composite_reading, theater_ratio, 1940, 0.8).
narrative_ontology:measurement_basis(hono_tr_t1940, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1770, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1770, 0.66).
narrative_ontology:measurement_basis(hono_be_t1770, observed).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1800, 0.7).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1830, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1830, 0.67).
narrative_ontology:measurement_basis(hono_be_t1830, observed).
narrative_ontology:measurement(hono_be_t1860, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1860, 0.58).
narrative_ontology:measurement_basis(hono_be_t1860, observed).
narrative_ontology:measurement(hono_be_t1890, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1890, 0.44).
narrative_ontology:measurement_basis(hono_be_t1890, observed).
narrative_ontology:measurement(hono_be_t1920, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement_basis(hono_be_t1920, observed).
narrative_ontology:measurement(hono_be_t1940, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1940, 0.22).
narrative_ontology:measurement_basis(hono_be_t1940, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1770, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1770, 0.55).
narrative_ontology:measurement_basis(hono_su_t1770, observed).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1830, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1830, 0.66).
narrative_ontology:measurement_basis(hono_su_t1830, observed).
narrative_ontology:measurement(hono_su_t1860, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1860, 0.7).
narrative_ontology:measurement_basis(hono_su_t1860, observed).
narrative_ontology:measurement(hono_su_t1890, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1890, 0.62).
narrative_ontology:measurement_basis(hono_su_t1890, observed).
narrative_ontology:measurement(hono_su_t1920, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement_basis(hono_su_t1920, observed).
narrative_ontology:measurement(hono_su_t1940, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1940, 0.35).
narrative_ontology:measurement_basis(hono_su_t1940, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the decline of dueling' covers structurally distinct claims and is decomposed into the honor_settlement_legitimacy kernel family: this composite_reading (overdetermined decline with a dominant contraction edge), contraction_reading (cultural unthinkability as sole sufficient cause — no independent material pathways, hence a different omega structure and a different account of when the unwilling were released), and drop_reading (fringe persistence among residual adherents — a terminal-distribution claim implying live coercion and later bearers, hence a different victim set and terminal classification). The readings differ in epsilon-relevant structure — victim sets, enforcement trajectory, and terminal state — not merely in emphasis; each is authored as its own file with a single stable epsilon, linked through reading_relations and this network edge set. The composite reading is upstream of the drop reading in the sense that its causal account constrains how the residual-practice evidence may be interpreted (inertial holdout versus viable niche).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
