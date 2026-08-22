% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Fixed Mourning Forms as Cross-Generational Symbolic Continuity Mechanism
 *   domain: religious/collective-memory/ritual-practice
 *
 * SUMMARY:
 *   A dispersed community that survived repeated catastrophes maintains a
 *   fixed complex of mourning and commemorative practices — fixed texts,
 *   fixed calendar dates, fixed gestures — that hands a shared symbolic
 *   vocabulary from generation to generation. This story instantiates ONE
 *   reading of the catastrophe-memory kernel: the claim that the practice
 *   complex's persistence-function is symbolic continuity and collective
 *   identity. Under this reading the arrangement is close to pure
 *   coordination: nearly every participant is a net beneficiary of inheriting
 *   a ready-made identity vocabulary, the enforcement needed is social and
 *   light, and the principal cost falls on would-be adapters whose
 *   modifications are refused. The epsilon referent is the standing practice
 *   complex as this reading assesses it — low extraction, since symbolic
 *   transmission yields no operational surplus for anyone to collect. Sibling
 *   readings of the same kernel are different constraints with different
 *   epsilon: the survival-competence reading assesses the same practices as
 *   carriers of persecuted-community know-how (moderate extraction where
 *   rigidity blocks updating of survival knowledge); the trauma-encoding
 *   reading assesses them as intergenerational warning systems (higher
 *   extraction where compulsory rehearsal burdens descendants); the
 *   boundary-maintenance reading assesses them as policing instruments
 *   (highest extraction, with clear insiders and excluded outsiders). Those
 *   epsilon differences are why the label decomposes into four files linked
 *   by network edges. KEY AGENTS (by structural relationship): -
 *   observant_community_members: primary beneficiary (organized/constrained)
 *   — inherit and carry the symbolic vocabulary - rabbinic_authorities:
 *   agenda-setter and secondary beneficiary (institutional/identity_locked) —
 *   administer the forms, receive deference as custodians -
 *   bereaved_mourners: beneficiary with payer secondary
 *   (moderate/constrained) — receive shaped grief, pay the mourning
 *   disciplines - liturgical_reformers: primary payer (moderate/constrained)
 *   — proposals refused; the rigidity cost lands here -
 *   secularized_descendants: payer (moderate/mobile) — inherit forms that fit
 *   badly; drift is the common exit - womens_prayer_pioneers: excluded with
 *   payer secondary (organized/constrained) — barred from setting practice,
 *   built parallel services - ritual_studies_scholars: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.31).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.34).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Fixed Mourning Forms as Cross-Generational Symbolic Continuity Mechanism").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious/collective-memory/ritual-practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '203c4981-e672-4c5e-a084-2fb990959d44').
narrative_ontology:cs_kernel_codification('203c4981-e672-4c5e-a084-2fb990959d44', fixed_text).
narrative_ontology:cs_authority_grounding('203c4981-e672-4c5e-a084-2fb990959d44', lineage).
narrative_ontology:cs_interpretation_layer_present('203c4981-e672-4c5e-a084-2fb990959d44').
narrative_ontology:cs_reading_relation('203c4981-e672-4c5e-a084-2fb990959d44', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('203c4981-e672-4c5e-a084-2fb990959d44', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('203c4981-e672-4c5e-a084-2fb990959d44', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('203c4981-e672-4c5e-a084-2fb990959d44', foundational, symbolic_identity_constituted_by_unbroken_forms).
narrative_ontology:cs_axiom_status(symbolic_identity_constituted_by_unbroken_forms, holdable).
narrative_ontology:cs_axiom_grounding('203c4981-e672-4c5e-a084-2fb990959d44', symbolic_identity_constituted_by_unbroken_forms, deontological).
narrative_ontology:cs_axiom('203c4981-e672-4c5e-a084-2fb990959d44', secondary, form_fidelity_outweighs_adaptive_revision).
narrative_ontology:cs_axiom_status(form_fidelity_outweighs_adaptive_revision, holdable).
narrative_ontology:cs_axiom_grounding('203c4981-e672-4c5e-a084-2fb990959d44', form_fidelity_outweighs_adaptive_revision, conventional).
narrative_ontology:cs_reference_frame('203c4981-e672-4c5e-a084-2fb990959d44', unbroken_symbolic_transmission_chain).
narrative_ontology:cs_drift_state('203c4981-e672-4c5e-a084-2fb990959d44', contemporary_assimilation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('203c4981-e672-4c5e-a084-2fb990959d44', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, observant_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, bereaved_mourners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, rabbinic_authorities).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, liturgical_reformers).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, secularized_descendants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, womens_prayer_pioneers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, bereaved_mourners).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, symbolic_transmission_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, chain_of_generations_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the fixed mourning and commemorative forms: reciting the mourner's prayer, lighting anniversary candles, gathering for the fast days and memorial services. Each member receives a ready-made vocabulary of grief and belonging that connects them to relatives and strangers across centuries. Leaving the forms is possible but means losing the community's shared language and often family ties with it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, observant_community_members, beneficiary,
    organized, generational, constrained, global).

% Adjudicate how the forms are performed: which texts are recited, who may lead, which new commemorations enter the calendar. Their rulings keep practice uniform across dispersed congregations. Their training, standing, and livelihood are constituted by the practice they administer; stepping outside it would dissolve the role itself. They receive deference as custodians of continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, rabbinic_authorities, beneficiary).

% Newly bereaved members take on the mourning disciplines: daily prayer attendance for eleven months, the housebound week, the annual candle. The forms give their grief a shape and an audience, and mark them publicly as people the community must support. The disciplines also demand time and composure at the moment they have least of either to spare.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, bereaved_mourners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, bereaved_mourners, payer).

% Members who propose altering the forms — vernacular recitation, shortened rites, gender-shared leadership, added commemorations — meet resistance from custodians and congregants who treat the received forms as non-negotiable. Some found parallel congregations at the cost of splitting from family and friends; others drop the attempt. Their proposals rarely reach the official calendar.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, liturgical_reformers, payer,
    moderate, biographical, constrained, continental).

% Inherit the forms without the beliefs that once carried them. They attend memorial gatherings occasionally, light candles sometimes, and feel the pull of obligations they cannot articulate. The fixed forms fit their lives poorly; drifting away is easy and common, and each departure thins the chain the forms exist to maintain.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, secularized_descendants, payer,
    moderate, biographical, mobile, global).

% Women barred from leading public mourning prayer organized parallel services beginning in the 1970s, saying the mourner's prayer in quorums of their own. Long absent from the councils where practice is set, they argued for inclusion from outside it; their innovations are being absorbed gradually and unevenly, congregation by congregation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, womens_prayer_pioneers, excluded,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, womens_prayer_pioneers, payer).

% Document and compare how mourning practices transmit identity across ruptures: interviewing practitioners, archiving liturgical change, comparing post-catastrophe communities. They describe the arrangement's workings without participating in its upkeep.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of handing a shared symbolic vocabulary — texts, dates, gestures, names — across generational breaks without any central compulsion: dispersed households coordinate on the same words at the same times, so a mourner in one decade performs what a mourner performed centuries earlier, and strangers recognize each other as continuations of one chain.
% TRANSFER_FUNCTION: Moves symbolic content from deceased generations to living ones (language, calendar, gesture); moves recognition and standing within the community toward those who perform and administer the forms correctly; and moves grief-labor — time, discipline, presence — from individual mourners into a public, witnessed schedule.
% ABSENT_VOICES: Would-be adapters sit outside the councils where practice is set: women historically barred from leading public mourning, secularized descendants who find the forms unintelligible, and marginal members whose proposed modifications never reach the calendar. The dead, on whose behalf the forms claim to speak, are represented only by the forms themselves.
% DISAPPEARANCE_RATIONALE: Without the fixed forms, each household would improvise its own mourning and commemoration; within two or three generations the shared vocabulary would fragment into private memories, mutual recognition across communities would fail, and the identity markers that let dispersed members recognize a common chain would need wholesale replacement.
% FOUNDING_PROBLEM: Repeated catastrophes — destruction, expulsion, massacre, and finally industrial genocide — left a dispersed community with no territory or sovereignty, facing the problem of how to keep collective memory and identity continuous when every institution that might carry them kept being destroyed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the practicing community: demographic and sociological studies document continuing assimilation and discontinuity pressure; historians of liturgy trace deliberate post-catastrophe additions to the calendar (post-Crusade laments, postwar memorial days and memorial-book compilations) as responses to exactly this problem; survivor-testimony archives independently attest the perceived need. Only the adequacy of the solution, not the existence of the problem, depends on the beneficiary congregations' own attestation.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.31 at interval end) because the arrangement's yield is symbolic — a transmitted vocabulary — and no seat converts the rigidity others bear into a collected surplus; the transmitter seat receives deference, but deference funds the transmission rather than enriching a collector. Suppression (0.34) is social rather than mechanical: communal expectation, Hebrew-only defaults, and gendered role assignments hold the forms in place, while exit through assimilation or denomination-switching remains genuinely available, which caps suppression well below enforced-arrangement levels. Theater (0.26) is low: from inside the practice, repetition is the function — a form repeated identically is doing its work — though rote performance among later generations raises the share of motion without meaning. Accessibility collapse (0.42) and resistance (0.44) sit mid-range: adapted and secular alternatives remain reachable, and reform, feminist, and secularizing pressure is organized and sustained. The measurement series run on one shared nine-point grid spanning 1945–2025; both tracked metrics rise gently as catastrophe-memory urgency fades and rigidity costs weigh more heavily on later generations. Suppression is authored as a raw structural property and is deliberately NOT scaled by scope or directionality — only extractiveness is scaled, inside the engine.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same arrangement. From the rabbinic seat the forms are a trust handed down intact — alteration is breach, and refusing modification is fidelity, not harm. From the reformer and secularized-descendant seats the same refusal is a cost imposed on them for a benefit they did not negotiate: they bear the friction between inherited form and lived life. Bereaved mourners straddle the line — the disciplines that shape their grief are also levied on them at their weakest moment. The engine derives these divergent per-seat classifications from the declared roles, powers, and exits; nothing in the authored claim adjudicates between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (observant members, mourners, the transmitter seat) derive low directionality — the arrangement subsidizes them with vocabulary, shaped grief, and standing. Declared victims (reformers, secularized descendants, the excluded women's seat) derive high directionality — they pay in refused modification, ill-fitting inheritance, and barred leadership. Exit modulation separates the payer seats: secularized descendants hold mobile exit (drift is cheap and common), placing them nearer the beneficiary end than their victim listing alone would suggest; reformers are constrained (parallel congregations cost community), holding them nearer the target end. No directionality overrides are used: the derivation from declared structure already places each seat correctly, and the one live ambiguity — whether the transmitter seat's deference constitutes capture — is routed to an omega rather than forced through an override keyed to a power atom shared with unrelated seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two mislabels. Against the snare reading (tempting from the boundary-maintenance sibling, where enforcement polices outsiders): here enforcement is social, exit is real, and no seat collects the rigidity costs others pay — the structure lacks the extractor's receipt a snare requires. Against the piton reading (tempting from outside, where repetition looks empty): the theater ratio is low and the founding problem is live — the transmission problem the forms solve recurs with every generation, so the practice is maintained because it works, not because it once did. Mandatrophy verdict: the mandate (hand the vocabulary across the break) is still being executed; nothing has atrophied into performance. The receipt surface records the healthy-coordination shape — diffuse gains, prohibitive fixing — and should be read against the low theater ratio, not as a decay signature: rebuilding a working identity-coordination good from scratch is expensive, which is why fixing is prohibitive even though nothing is broken.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the symbol-continuity framing capture the standing practice complex as a single stable-epsilon constraint, or does the complex''s actual operation mix the functions the sibling readings isolate, such that no single reading''s epsilon is invariant?',
    'Cross-reading comparison of per-seat classifications across the four family files: if the same seats compute as coordinated under this reading and targeted under a sibling with no structural difference in the declared data, the referent is one arrangement and the readings are perspectives; if the declared victim sets genuinely differ, the readings are distinct constraints and the family decomposition stands.',
    'If the mixed-function account wins, each reading''s epsilon is unstable and the family needs a composite story with reconciled beneficiary/victim sets; if the readings are distinct, the current decomposition holds and cross-reading epsilon comparison becomes the measurement of the kernel contest itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Whether this reading''s epsilon is invariant or the arrangement mixes sibling functions.').

omega_variable(
    transmitter_capture_ambiguity,
    'Is the deference flowing to the transmitter seat (rabbinic authorities) part of the coordination''s operating cost, or a concentrated return that would make that seat a capturer despite this reading''s diffuse-gains finding?',
    'Compare the standing and compensation of transmitters in communities holding fixed forms versus communities running adapted equivalents: if transmitter standing is equivalent under adapted regimes, deference tracks the transmission service; if it tracks form-fidelity specifically, the seat collects from rigidity.',
    'If deference tracks form-fidelity, the receipt surface should name the transmitter seat and the arrangement shifts toward the hybrid coordination/extraction class; if it tracks service, the diffuse finding holds and the low-extraction classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmitter_capture_ambiguity, empirical, 'Whether the transmitter seat captures the arrangement''s returns or merely administers them.').

omega_variable(
    rigidity_cost_necessity,
    'Is the cost imposed on adaptive modification the necessary price of cross-generational coordination, or suppressible overhead that adapted forms would eliminate without loss?',
    'Compare multi-generation identity-retention outcomes between communities holding fixed forms and communities running adapted equivalents, controlling for catastrophe proximity and community density.',
    'If adapted forms retain identity comparably, the measured extraction is excess and the arrangement sits closer to the hybrid class; if fixed forms outperform, the rigidity cost is the coordination price itself and the low-extraction reading is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rigidity_cost_necessity, empirical, 'Whether ritual rigidity is coordination price or removable overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 70, 0.22).
narrative_ontology:measurement_basis(cata_tr_t70, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(cata_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.27).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 70, 0.29).
narrative_ontology:measurement_basis(cata_be_t70, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement_basis(cata_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_kernel__symbol_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'catastrophe memory ritual' decomposes into four structurally distinct claims: this file (symbol continuity) plus the survival-competence, trauma-encoding, and boundary-maintenance readings. Each sibling gets its own epsilon, its own beneficiary/victim sets, and its own classification; the epsilon differences arise because each reading assigns the fixed forms a different function, which changes who pays and who benefits. This reading carries the lowest epsilon of the family: symbolic transmission yields no operational surplus anyone collects, so extraction is limited to rigidity costs on would-be adapters. No upstream/downstream ordering is asserted among siblings — they compete as interpretations of one kernel rather than feeding one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
