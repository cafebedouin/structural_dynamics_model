% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__catastrophe_necessity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe-Necessity Doctrine: Only Real Catastrophes Maintain Genuine Competence
 *   domain: safety engineering / organizational learning / high-reliability organizations
 *
 * SUMMARY:
 *   In high-consequence industries — aviation, nuclear operations, medicine,
 *   emergency response — the governing belief is that genuine operational
 *   competence is conferred and maintained only by actual catastrophic
 *   events: the stress, uncertainty, and irreversibility of real disaster
 *   have no substitute, and simulation, however sophisticated, is
 *   insufficient. The standing arrangement built under that belief relies on
 *   simulation to carry competence through catastrophe-free periods that the
 *   belief itself declares cannot work, so the margin between recorded
 *   preparedness and actual readiness is spent as operating tempo and renewed
 *   only when catastrophe arrives. This story instantiates the
 *   catastrophe-necessity reading of the kernel
 *   catastrophe_proxy_sufficiency: it asserts the categorical limit, names
 *   the eroded safety margin — carried by protected publics and junior
 *   operators — as the harm it produces, and names the seats that collect
 *   from the categorical form (veteran status, post-disaster liability
 *   shields, tempo and budget relief) as its beneficiaries. The claim/metric
 *   gap is deliberate and is the measurement: the reading CLAIMS mountain (a
 *   categorical limit of stress physiology and organizational cognition),
 *   while the metrics describe the standing arrangement as this reading
 *   itself assesses it — a regime spending margin it cannot replenish, with
 *   rents accruing to the categorical form's holders. Whether that profile
 *   certifies as natural law or trips the false-summit path is exactly what
 *   the engine is asked to decide. The sibling readings are separate
 *   constraints linked through the network; they are not folded into this
 *   classification. KEY AGENTS (by structural relationship): -
 *   veteran_operator_cohorts: primary status beneficiary
 *   (organized/identity_locked) — authority premium from irreplaceable
 *   real-event experience - operational_tempo_owners: primary economic
 *   beneficiary (powerful/arbitrage) — collect the margin conversion as
 *   throughput and cost relief - post_catastrophe_institutional_defendants:
 *   beneficiary (institutional/constrained) — deploy the categorical claim as
 *   post-disaster shield - protected_publics: primary bearer of the cost
 *   (powerless/trapped) — hold the eroded margin as accident risk -
 *   junior_operator_cohorts: secondary bearer (moderate/constrained) —
 *   simulation-trained, discounted until blooded -
 *   catastrophe_victims_and_families: realized-cost bearers, structurally
 *   absent from the conversation (powerless/trapped) -
 *   simulation_training_industry: dual-positioned collector
 *   (organized/mobile) — paid by the arrangement its product's adequacy
 *   contests - safety_certification_authorities: agenda-setter
 *   (institutional/constrained) — codify the simulated/real boundary -
 *   training_science_community: analytical observer (organized/analytical) —
 *   holds the measurement apparatus
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.64).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.34).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe-Necessity Doctrine: Only Real Catastrophes Maintain Genuine Competence").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety engineering / organizational learning / high-reliability organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '1bb38e0e-04a2-40d8-941e-28565355e8fb').
narrative_ontology:cs_kernel_codification('1bb38e0e-04a2-40d8-941e-28565355e8fb', distributed).
narrative_ontology:cs_authority_grounding('1bb38e0e-04a2-40d8-941e-28565355e8fb', lineage).
narrative_ontology:cs_interpretation_layer_present('1bb38e0e-04a2-40d8-941e-28565355e8fb').
narrative_ontology:cs_reading_relation('1bb38e0e-04a2-40d8-941e-28565355e8fb', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('1bb38e0e-04a2-40d8-941e-28565355e8fb', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bb38e0e-04a2-40d8-941e-28565355e8fb', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, forecloses).
narrative_ontology:cs_axiom('1bb38e0e-04a2-40d8-941e-28565355e8fb', foundational, genuine_competence_requires_actual_catastrophe).
narrative_ontology:cs_axiom_status(genuine_competence_requires_actual_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('1bb38e0e-04a2-40d8-941e-28565355e8fb', genuine_competence_requires_actual_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('1bb38e0e-04a2-40d8-941e-28565355e8fb', secondary, simulation_insufficient_for_stress_competence).
narrative_ontology:cs_axiom_status(simulation_insufficient_for_stress_competence, holdable).
narrative_ontology:cs_axiom_grounding('1bb38e0e-04a2-40d8-941e-28565355e8fb', simulation_insufficient_for_stress_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('1bb38e0e-04a2-40d8-941e-28565355e8fb', catastrophe_exclusive_competence_regime).
narrative_ontology:cs_drift_state('1bb38e0e-04a2-40d8-941e-28565355e8fb', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1bb38e0e-04a2-40d8-941e-28565355e8fb', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_operator_cohorts).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, post_catastrophe_institutional_defendants).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_tempo_owners).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_training_industry).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, protected_publics).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, junior_operator_cohorts).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_victims_and_families).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, skill_decay_without_stress_exposure).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_transfer_insufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior pilots, plant operators, incident commanders, and surgeons whose authority, pay grade, and certification roles rest on having been tested by real catastrophic events. They sit on review boards, sign off on junior competence, and set the informal standard that simulated experience is not the same as having been there. Their standing depends on real-event experience remaining scarce and irreplaceable; leaving the cohort would mean surrendering the professional identity built on having seen the real thing.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_operator_cohorts, beneficiary,
    organized, biographical, identity_locked, global).

% Executives, boards, insurers, and institutional counsel who, after a catastrophe, argue that no training or simulation could have surfaced what the event revealed. Each time that argument is accepted, it converts what would otherwise be read as negligence into misfortune and shields the institution's decisions from retrospective second-guessing. They do not run training programs; they collect the interpretive shield the categorical claim provides whenever it is treated as settled.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, post_catastrophe_institutional_defendants, beneficiary,
    institutional, biographical, constrained, global).

% The executives and operators who set schedules, staffing levels, and training budgets. Counting simulated hours as preparedness lets them run higher tempo and carry leaner margins than an honest depreciation schedule for competence would allow; the difference shows up as throughput and cost performance. The eroded margin itself lands elsewhere, on the systems' users, and capital can redeploy if the doctrine ever falls.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_tempo_owners, beneficiary,
    powerful, immediate, arbitrage, global).

% Manufacturers of full-mission simulators, immersive training systems, and the training centers that operate them. They collect revenue for the very mechanism the categorical claim declares insufficient, which places them in an awkward position: their commercial thesis aligns with the sibling claims that simulation can suffice, and they fund the fidelity research that tests the categorical claim's limits. Their revenue grows with the arrangement even as their product's adequacy is what the dispute is about.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_training_industry, beneficiary,
    organized, biographical, mobile, global).

% Passengers, patients, plant neighbors, and communities downstream of industrial systems. They hold the difference between the competence the organization's records claim and the competence actually present as personal accident risk. They cannot observe the decay, cannot price it into their choices, and cannot individually exit air travel, medical care, or industrial proximity; their protection depends on margins they do not control.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, protected_publics, payer,
    powerless, generational, trapped, global).

% Pilots, nurses, reactor operators, and emergency responders trained overwhelmingly in simulators. Under the doctrine that only real events produce genuine competence, their judgment is structurally discounted — not blooded — until a real event tests them, and the real event is precisely the test no simulator could rehearse them for. They bear both the discount on their standing and the frontline exposure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, junior_operator_cohorts, payer,
    moderate, biographical, constrained, global).

% Those who were aboard, downstream, or on shift when the real event arrived — the people whose losses the doctrine classifies as the necessary tuition of genuine competence. They are absent from the doctrine's formulation: the necessity is always asserted by those who survived to theorize it. Survivors' groups contest the framing that their losses were required, but they hold no seat in certification or doctrine-setting rooms.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_victims_and_families, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_victims_and_families, payer).

% National and international bodies that codify how much simulated experience counts toward qualification and what real-event exposure is required for advanced certification. Over the interval they have steadily expanded simulated-hours credit and retired real-event requirements — zero-flight-time type ratings, simulator-based requalification — while leaving the categorical claim formally unadjudicated. They administer the boundary between simulated and real experience without collecting from either side of it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_certification_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Researchers in simulation fidelity, stress inoculation, skill decay, and transfer of training. They hold the measurement apparatus the dispute will be settled with — dose-response studies, fidelity sweeps, retention curves — and their findings have progressively narrowed the territory on which the categorical claim stands, without any of them holding authority over certification practice.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, training_science_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_tempo_owners).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives large cohorts of operators exposure to catastrophic conditions without waiting for catastrophes: the simulation regime centralizes rare-event experience, makes it schedulable, repeatable, and survivable, and lets certification systems standardize preparedness across organizations that could never each generate real events — while the categorical claim calibrates how much trust that standardization is permitted to carry.
% TRANSFER_FUNCTION: Converts simulated training hours into recorded preparedness at a fraction of the cost of operational conservatism; moves the un-replenished difference between recorded and actual readiness onto protected publics as accident risk; and moves status, certification authority, and post-event interpretive authority to operators and institutions with real-event experience.
% ABSENT_VOICES: Catastrophe victims and their families are structurally absent: the necessity of the tuition is always asserted by those who survived to theorize it, and survivors' groups that contest the framing have no seat in certification or doctrine-setting rooms. Simulation-sufficiency researchers publish but do not adjudicate — their findings enter training policy only as filtered through the authorities and veteran boards the claim empowers. Junior operators' post-event testimony that reality diverged from the simulator is collected as anecdote rather than as evidence.
% DISAPPEARANCE_RATIONALE: The parties genuinely dispute it. On this reading's own account the underlying limit persists regardless of belief — competence still erodes in catastrophe-free periods, so at the level of physiology nothing rearranges when the doctrine falls. But the institutional superstructure is built on the categorical claim: certification credit for simulated hours, veteran gatekeeping authority, post-disaster 'no simulation could have shown this' defenses, and training budgets justified against a mechanism declared incapable would all be renegotiated within years. The sibling readings predict further that part of the decay itself is an artifact of the underinvestment the claim licenses, in which case even the physiology-level picture would change. Whether disappearance alters doctrine only, or doctrine and decay together, is exactly the kernel dispute.
% FOUNDING_PROBLEM: High-consequence industries had no way to give operators experience of catastrophic conditions without causing catastrophes: real-event learning meant learning from disasters, and rare-event readiness had no schedulable source. The necessity claim is the residue of that original bargain — when simulation was crude, 'only the real thing teaches' was close to literally true, and the doctrine predates the simulation era it now governs.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the training-science community's skill-decay and transfer literature documents that rare-event readiness remains unsolved, and independent accident-investigation boards repeatedly find readiness gaps for conditions outside the simulated envelope. Survivors' testimony attests the gap from the receiving end. The veteran cohorts and institutional defendants also attest the problem is live, but they sit inside the beneficiary set and their attestation is discounted accordingly — that the problem is live is not in dispute; what is disputed is whether simulation can now solve it.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64) is assessed from this reading's own lights against the standing arrangement: a training regime that counts simulated hours as preparedness while holding, by its own doctrine, that simulation cannot maintain genuine competence is spending a margin it cannot replenish, and the deficit lands on those who fly, heal, and live downstream. The value is high but not extreme because part of the cost is the price of the underlying limit itself rather than of anyone's collection. Suppression (0.34) tracks the formal enforcement machinery, which decayed across the interval as certification bodies expanded simulated-hours credit and retired real-event requirements; what persists is social gatekeeping rather than coercive enforcement — and suppression is authored as a raw structural property, unscaled by power or scope, with only extractiveness scaled downstream. Theater ratio (0.55) crosses half only at interval end: as simulation centralized, a growing share of drill and certification activity produces assurance artifacts — hours logged, boxes checked, confidence displayed — rather than the competence the doctrine says only reality can supply. Accessibility collapse (0.72) is high because the categorical claim, if true, closes the exit: no amount of simulation substitutes for the real thing; it stays below mountain-typical values because residual partial value (procedural fluency, stress inoculation) keeps alternatives partly open. Resistance (0.55) reflects organized, well-funded contest: the training-science community, the simulation industry, and the sibling readings all actively test the categorical form. All three tracked series run on one shared six-point grid (t=0,9,18,27,36,48). The central dynamic is the divergence between rising extraction (0.48 to 0.64) and falling formal enforcement (0.52 to 0.34): the margin deficit deepens as simulation reliance grows precisely while the machinery that once enforced real-event standards is dismantled. Receipt: the material margin conversion lands demonstrably on operational_tempo_owners (tempo and cost relief), with secondary flows — status premium, liability shield — accruing to the veteran and defendant seats; fixing is prohibitive because no single organization can unilaterally carry the tempo cost of honest margin budgeting, real-exposure programs cannot be ethically scheduled, and on the law side there is nothing to fix.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the veteran seat the arrangement is the natural order of things — a fact about stress physiology that no one built and no one can repeal — and the classification should come out mountain-like from that position. From the protected-public seat the same arrangement is an unchosen, unpriced risk carried without consent. From the tempo-owner seat it is cost relief: the doctrine's truth is convenient and is not examined closely. From the training-science seat it is a falsifiable claim currently failing parts of its own tests. The engine computes these per-seat classifications from the structural data — power, exit options, declared position — and this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: veteran_operator_cohorts (identity-locked exit) sit near the beneficiary end — the categorical form confers a status premium unobtainable elsewhere, and their identity is fused with having been tested; operational_tempo_owners (arbitrage exit) sit nearest it — they collect the margin conversion and can redeploy capital if the doctrine falls; post_catastrophe_institutional_defendants collect the shield case by case. Victim declarations drive high directionality: protected_publics (trapped) sit near the full-target end — they carry the deficit with no exit and no observation; junior_operator_cohorts (constrained) carry both the discount on their judgment and the exposure; catastrophe_victims_and_families bear the realized form of the cost while being structurally absent from the conversation that names it necessary. simulation_training_industry is genuinely dual: it collects revenue from the arrangement while its commercial thesis aligns with the sibling readings against the categorical claim — mid-low directionality. safety_certification_authorities administer the boundary without collecting from it; training_science_community observes from the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving operators readiness for conditions that cannot be ethically scheduled — is live, so this is not a mandate outliving its function; what may be outliving its evidence is the categorical ANSWER. The classification machinery prevents mislabeling in both directions: reading the arrangement as pure extraction would erase the genuine epistemic service the claim performs (calibrating trust in training, preventing overreliance on simulated assurance — a real coordination function with real value); accepting it as pure natural law would erase the rent-bearing categorical form (status premiums, liability shields, budget relief) that persists past the evidence. The false-summit path is the pivot: the story declares beneficiaries on a mountain claim precisely so the engine tests whether the categorical form is law or cover. With founding_problem_status live and disappearance contested, the mismatch consumer raises no zombie flag — but the drift state (axiom_overriding, substantial, unacknowledged) records that the doctrine's authority increasingly rests on gatekeeping rather than on the dose-response evidence that would settle it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the categorical insufficiency of simulation a genuine natural limit of human stress physiology and organizational cognition, or a constructed doctrine whose categorical form serves identifiable interests (veteran status, post-disaster liability defense, training-budget minimization)?',
    'Convergent evidence: dose-response studies of stress exposure against competence retention across simulation fidelity levels; natural experiments comparing organizations with long catastrophe-free periods under differing simulation regimes; adversarial collaboration among the four readings.',
    'If genuine law, the mountain claim stands and the correct response is honest margin budgeting for irreducible decay; if constructed, this is a false summit — the categorical form is rent-bearing and simulation-fidelity investment becomes the primary lever.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether the catastrophe-necessity claim is natural law or interest-serving doctrine.').

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates one reading (catastrophe_necessity_reading) of the kernel catastrophe_proxy_sufficiency; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'The disagreement is located in the quantifier over simulation''s sufficiency: categorical insufficiency (this reading), categorical sufficiency (simulation_as_proxy_catastrophe_reading), partial sufficiency with generational-timescale decay (hybrid_degradation_reading), and threshold-dependent sufficiency (simulation_fidelity_threshold). Adjudication requires the dose-response and fidelity-sweep evidence named in natural_law_vs_constructed_doctrine.',
    'Under the proxy reading the standing arrangement''s margin deficit vanishes and its extraction collapses toward coordination cost; under the hybrid reading the deficit is generational-scale rather than perpetual; under the fidelity-threshold reading the deficit is an engineering residual. The same arrangement is a tragic natural limit under this reading and a sold illusion under the proxy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of four readings of the catastrophe-proxy kernel; the quantifier over sufficiency is where the readings diverge.').

omega_variable(
    irreducibility_vs_fidelity_artifact,
    'Is the measured gap between simulated and real catastrophic stress an irreducible property of genuine lethal stakes, or an artifact of current simulator fidelity with no in-principle barrier?',
    'Psychophysiological dose-response comparison of real-event arousal cascades against maximum-fidelity simulation across modalities; an asymptoting arousal gap supports irreducibility, a narrowing gap refutes it.',
    'Irreducible sustains the mountain claim; artifact transfers the constraint to the fidelity-threshold sibling reading, where it degrades toward an engineering scaffold with a sunset once the threshold is crossed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreducibility_vs_fidelity_artifact, empirical, 'Whether simulation''s insufficiency is irreducible or a fidelity artifact.').

omega_variable(
    individual_decay_vs_organizational_amnesia,
    'Does the catastrophe-free decay attributed to simulation insufficiency reflect individual competence decay, organizational memory loss, or both — and does this reading attribute to simulation what is actually institutional amnesia?',
    'Decompose retention curves: track individual operator performance against organizational decision quality across catastrophe-free generations, holding simulation regimes constant.',
    'If the dominant term is organizational memory, the remedy is knowledge-preservation structures (a coordination problem with cheap fixes) rather than catastrophe exposure, and the necessity reading over-attributes to simulation insufficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_decay_vs_organizational_amnesia, empirical, 'Whether the observed decay is individual skill loss or institutional forgetting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t9, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement(cata_tr_t18, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(cata_tr_t27, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 27, 0.45).
narrative_ontology:measurement(cata_tr_t36, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 36, 0.5).
narrative_ontology:measurement(cata_tr_t48, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 48, 0.55).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t9, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(cata_be_t18, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(cata_be_t27, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 27, 0.6).
narrative_ontology:measurement(cata_be_t36, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(cata_be_t48, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 48, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cata_su_t9, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 9, 0.48).
narrative_ontology:measurement(cata_su_t18, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(cata_su_t27, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 27, 0.4).
narrative_ontology:measurement(cata_su_t36, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 36, 0.37).
narrative_ontology:measurement(cata_su_t48, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 48, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, information_standard).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The colloquial question 'is simulation enough to maintain catastrophe-readiness?' decomposes into four structurally distinct claims (this categorical-necessity reading; categorical sufficiency; hybrid partial sufficiency with generational decay; fidelity-threshold sufficiency). They differ in epsilon because they differ in the victim set they imply for the same standing arrangement: under categorical sufficiency the margin deficit vanishes; under hybrid it is generational-scale; under fidelity-threshold it is an engineering residual; under this reading it is perpetual and un-replenishable. Each is a separate file with its own beneficiaries, victims, and classification. The citation pattern runs from this reading (the traditional doctrine, upstream) to the challengers, whose research programs exist partly to test it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
