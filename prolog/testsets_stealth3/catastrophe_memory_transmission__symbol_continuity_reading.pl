% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe-Memory Ritual Fidelity Regime (Symbol-Continuity Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   A post-catastrophe community — founded by survivors of a disaster that
 *   destroyed its institutions and dispersed its population — maintains a
 *   high-fidelity ritual regime: fixed mourning rites, a fixed commemorative
 *   calendar, prescribed forms transmitted verbatim across four generations,
 *   administered by an elder council with censure for deviation. This story
 *   instantiates ONE reading of the kernel catastrophe_memory_transmission:
 *   the symbol_continuity_reading, which holds that ritual preserves identity
 *   and mourning-practice as an intrinsic communal good and that transmission
 *   of symbolic form is itself the survival mechanism. Two sibling readings —
 *   operational_competence_reading and hybrid_embedded_reading — are separate
 *   constraint files linked through the network edges; per the
 *   epsilon-invariance principle this file authors epsilon for the standing
 *   fidelity regime as seen from THIS reading's lights, never averaged across
 *   readings. Claim and metrics are independent authored facts: claimed_type
 *   records this reading's structural account (tangled_rope — real
 *   identity-coordination carrying real asymmetric costs), and the metric
 *   values record what the regime's operation descriptively looks like across
 *   the record. The epsilon referent is the standing arrangement under
 *   contest — the fidelity regime as it actually operates — not the adapted
 *   alternative this reading declines to endorse.
 *
 * KEY AGENTS:
 *   - ritual_council_elders — primary administrator (agenda_setter; organized/generational; identity_locked): fixes fidelity standards, adjudicates deviation, collects deference and final authority
 *   - founding_survivor_generation — primary beneficiary (moderate/biographical; identity_locked): their losses anchor the canon; honor and narrative centrality flow to them
 *   - ritual_specialists — secondary beneficiary (moderate/biographical; identity_locked): officiants whose livelihood and rank ride the ceremonial calendar
 *   - descendant_generations — primary target (moderate/biographical; constrained): bear obligations, time, and funds; receive belonging and grief-support in return
 *   - reform_advocates — secondary target (powerless/biographical; constrained): bear targeted censure for proposing adaptation
 *   - departed_assimilated_members — absent voice (excluded; moderate/biographical; mobile): left the community; would testify that rigidity drove them out
 *   - catastrophe_memory_scholars — analytical observer (institutional/civilizational; analytical): comparative-evidence seat outside the community
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.6).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe-Memory Ritual Fidelity Regime (Symbol-Continuity Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'a23388cb-83a4-4a44-beb3-4f590765513f').
narrative_ontology:cs_kernel_codification('a23388cb-83a4-4a44-beb3-4f590765513f', formalized).
narrative_ontology:cs_authority_grounding('a23388cb-83a4-4a44-beb3-4f590765513f', lineage).
narrative_ontology:cs_interpretation_layer_present('a23388cb-83a4-4a44-beb3-4f590765513f').
narrative_ontology:cs_reading_relation('a23388cb-83a4-4a44-beb3-4f590765513f', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a23388cb-83a4-4a44-beb3-4f590765513f', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('a23388cb-83a4-4a44-beb3-4f590765513f', foundational, symbolic_form_intrinsically_constitutive).
narrative_ontology:cs_axiom_status(symbolic_form_intrinsically_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('a23388cb-83a4-4a44-beb3-4f590765513f', symbolic_form_intrinsically_constitutive, deontological).
narrative_ontology:cs_axiom('a23388cb-83a4-4a44-beb3-4f590765513f', secondary, fidelity_priority_over_adaptation).
narrative_ontology:cs_axiom_status(fidelity_priority_over_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('a23388cb-83a4-4a44-beb3-4f590765513f', fidelity_priority_over_adaptation, instrumental).
narrative_ontology:cs_reference_frame('a23388cb-83a4-4a44-beb3-4f590765513f', founder_form_canon).
narrative_ontology:cs_drift_state('a23388cb-83a4-4a44-beb3-4f590765513f', post_living_memory_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a23388cb-83a4-4a44-beb3-4f590765513f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, founding_survivor_generation).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, ritual_specialists).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, ritual_council_elders).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, descendant_generations).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, reform_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, descendant_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, symbolic_form_transmission_survival_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the annual commemorative calendar, adjudicates questions of correct performance, and presides over the mourning rites. Deference and final say over communal symbols concentrate here, and their standing rests on being trustees of uninterrupted transmission. Stepping back would mean surrendering the office their entire adult lives are organized around.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_council_elders, agenda_setter,
    organized, generational, identity_locked, national).

% Direct survivors of the catastrophe whose testimony anchors the rites; the calendar names and honors their losses year by year. As they age, the rites increasingly speak in their voice. Standing apart from the practice would read as disowning their own dead, and their self-understanding is bound up with being remembered in exactly this way.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, founding_survivor_generation, beneficiary,
    moderate, biographical, identity_locked, regional).

% Officiants, cantors, and memorial-keepers trained from youth in the prescribed forms. Stipends, marriage prospects, and communal rank follow the ceremonial calendar. Alternative livelihoods exist in principle, but leaving would forfeit standing that accumulates only inside the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_specialists, beneficiary,
    moderate, biographical, identity_locked, national).

% Attend, fund, and reproduce the rites they inherited; obligations arrive with adulthood and are audited socially. They draw belonging, grief-support, and a usable past from the same forms. Proposing changes invites censure; leaving cuts kin networks and marks the whole family.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, descendant_generations, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, descendant_generations, beneficiary).

% Press for shorter rites, vernacular recitation, and merging overlapping commemorations to free time and funds for housing and schooling needs. They meet procedural delay, public correction of their manners, and quiet loss of matchmaking and committee standing; several have emigrated or fallen silent.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, reform_advocates, payer,
    powerless, biographical, constrained, regional).

% Left for host-country cities and secular life; still listed on the communal rolls, rarely consulted. From outside they argue that the rigidity itself pushed them out and that lighter forms would have kept them. They hold no seat in calendar decisions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, departed_assimilated_members, excluded,
    moderate, biographical, mobile, continental).

% Comparative researchers studying how post-catastrophe and diaspora populations transmit memory across generations. They publish on the fidelity-versus-adaptation trade-off and take no part in communal governance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, ritual_council_elders).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates grief-processing and communal boundary-maintenance for a catastrophically wounded, dispersed population: synchronized mourning, a shared calendar, and a common symbolic vocabulary that let members recognize one another and coordinate mutual aid, marriage, and remembrance across distance.
% TRANSFER_FUNCTION: Moves time, labor, and funds from all members — heaviest from the descendant generations — into ceremonial production and upkeep; moves deference, authority, and specialist livelihood toward the elder council and the officiant class; moves the canonical narrative of the catastrophe from founder testimony into each new cohort.
% ABSENT_VOICES: Departed assimilated members would object that the regime's rigidity drove them out and that adaptive lightening would have retained them; silenced reformers inside the community object but have been marginalized out of the calendar-setting conversation. Neither seat is present when fidelity standards are fixed.
% DISAPPEARANCE_RATIONALE: If the fidelity regime vanished overnight, the commemorative calendar and shared mourning practice would dissolve immediately; within a generation or two the community would lose its boundary-markers against host societies, intermarriage and assimilation would accelerate, the elder council's authority would evaporate, and grief currently processed collectively would privatize. On this reading's account, the community as a distinct, self-recognizing entity fails to reproduce itself — the world rearranges around its absence.
% FOUNDING_PROBLEM: After the catastrophe destroyed the community's institutions and killed much of its population, the survivors faced total dissolution: scattered members, no functioning bodies, host societies pressing assimilation, and a grief too large for private processing. The fidelity regime was built to guarantee that a recognizable community — same forms, same calendar, same mourning — would still exist after the founders died.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the beneficiary set, on both sides: demographic and sociological studies of comparable post-catastrophe and diaspora populations show that communities which relaxed ritual transmission exhibit measurably faster institutional dissolution, supporting the live-threat side; the same literature documents counter-cases where identity persisted through vernacular adaptation, supporting the critics. No external source attests that the founding problem is simply dead, and none attests it is simply live — the dispute is genuine and externally witnessed.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.60: the regime takes real, recurrent payments — obligatory attendance, ceremonial levies, years of specialist training, foregone adaptation — while delivering goods this reading counts as the point (identity continuity, grief-processing, boundary-maintenance). Extraction is substantial but bounded by genuine delivery; it is not cover for nothing. Suppression 0.60 is authored as a raw structural property, unscaled by power or scope: fidelity is held by active machinery — fidelity committees, censure protocols, marriage-network pressure — not by unanimous preference. The suppression_requirement series is authored deliberately because the story's traced dynamic IS enforcement-capacity change: fidelity boards and censure procedures were built out decade by decade as drift pressure grew, and a static scalar would hide that ratchet. Theater 0.33 and rising: as founder-generation memory thins, a growing share of performance is correctness-maintenance rather than lived mourning — below the majority-performative zone but trending toward it. Accessibility_collapse 0.50: vernacular, shortened, and secular alternatives are imaginable and periodically attempted, but the tradition frames them as betrayal, so they half-exist. Resistance 0.48: recurring reform waves, quiet nonattendance, and emigration meet each generation. All three series run on one shared nine-point grid (t=0..80 by decades) so every metric is authored at every examined time point; endpoint values equal the base_properties scalars. Receipt note: the concentrated gains (deference, final authority, specialist livelihoods) demonstrably accrue to the elder council, while the identity good itself is diffuse — hence gain_flow names the council rather than asserting diffuseness. Fixing is prohibitive: relaxing fidelity threatens the continuity mechanism the community's arrangements depend on, and the council that could fix it bears almost none of the cost of leaving it unfixed.
 *
 * PERSPECTIVAL GAP:
 *   The elder and specialist seats should compute this as coordination they personally uphold: their directionality sits near the beneficiary end, enforcement is their craft, and the identity good is vivid to them. The descendant seat computes heavy effective extraction: obligations arrive regardless of conviction, exit is costly, and the received good is partly deferred and diffuse. The reform-advocate seat computes the sharpest extraction of all — targeted censure plus foregone adaptive projects with no offsetting benefit. Same regime, divergent computed types by seat; the divergence is the finding the corpus exists to take, not noise to be reconciled into the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation. Founding survivors, specialists, and elders sit at the beneficiary pole (low d): the regime subsidizes their standing, honor, and livelihood. Descendant generations declare payer with a secondary beneficiary position — mostly target, partly subsidized — so their d lands high but short of the trapped-full-target end. Reform advocates declare payer with no offsetting benefit and constrained exit, landing nearest full-target; their extraction is amplified by being the enforcement object itself. Departed assimilated members are excluded rather than coordinated: the regime's rigidity is part of why they left. Scholars are analytical and collect nothing. No directionality_overrides were needed — the beneficiary/victim declarations plus exit atoms already separate the seats correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guaranteeing that a recognizable community would outlive its founders — was substantially solved by the regime's own success: the community persisted. That success is exactly why the mandate is contested rather than dead: defenders read the dissolution threat as permanent (every generation re-enters assimilation pressure), critics read the emergency as passed. The mismatch consumer reads status=contested crossed with verdict=world_rearranges — no zombie flag fires, because the arrangements genuinely depend on the regime. The classification supplies guard-rails against two opposite mislabels: calling the regime a snare ignores that its payers are dual-positioned (descendants receive the identity good they pay for) and that the coordination function is real; calling it a rope ignores the enforced fidelity, the censure of adapters, and the measurable sacrifice of adaptive capacity. The theater_ratio rise from 0.12 to 0.33 across the interval is the early-warning signal: if form outlives feeling entirely while enforcement keeps growing, the trajectory bends toward piton — theatrical maintenance administered by people with no incentive to fix it. That bend has not yet been taken.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_commitment_symbol_continuity,
    'This constraint instantiates the symbol_continuity_reading of kernel catastrophe_memory_transmission. How would classifying the identical ritual regime under the sibling readings — operational_competence_reading or hybrid_embedded_reading — change the computed structure?',
    'Classify the sibling story files (same regime, different reading) and diff per-seat classifications, effective extraction, and epsilon across the family.',
    'Under hybrid_embedded_reading, part of the measured burden re-reads as unavoidable cost of transmitting embedded competence (lower excess extraction, stronger coordination gate). Under operational_competence_reading, fidelity maintained without adaptation registers as failed coordination — rehearsal that no longer rehearses anything — pushing the classification toward piton or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commitment_symbol_continuity, conceptual, 'Committer-frame routing: the kernel contest between three readings is carried here rather than folded into this file''s classification.').

omega_variable(
    form_competence_separability,
    'Is the symbolic form separable from whatever survival-relevant content it carries — could the community keep its identity while adapting form, or does form-loss cause dissolution?',
    'Comparative natural experiments: track communities that relaxed ritual form under pressure (migration, prohibition, generational revolt) against matched communities that held fidelity, measuring institutional survival and self-identification over subsequent generations.',
    'If form is dispensable without dissolution, this reading''s foundational axiom fails and the fidelity regime reclassifies toward inertial or theatrical maintenance; if form-loss reliably predicts dissolution, the reading is vindicated and a share of the measured extraction re-reads as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_competence_separability, empirical, 'The central contest axis of the kernel: whether form-as-such does the survival work.').

omega_variable(
    adaptive_capacity_opportunity_cost,
    'How large is the adaptive capacity actually sacrificed — the measurable diversion of time, funds, and blocked innovations attributable to fidelity requirements?',
    'Household time-use and communal budget audits; count abandoned adaptation projects (vernacular liturgies, shortened calendars, merged festivals) and estimate their forgone returns.',
    'A large verified sacrifice confirms the victim structure and supports tangled_rope; a negligible sacrifice collapses the victim declaration and pushes the classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity_opportunity_cost, empirical, 'Whether the declared victim — sacrificed adaptive capacity — is materially real.').

omega_variable(
    compliance_mechanism_internalization,
    'Is descendant compliance driven by internalized conviction (the inherited form IS their identity) or by structural sanction (censure, marriage-network consequences, exclusion)?',
    'Post-exit trajectories of leavers plus cohort attitude surveys: does perceived obligation persist after physical exit from the community?',
    'If compliance is largely internalized, effective suppression exceeds the structural measure and exit-option data understates lock-in; if sanction-driven, relaxing enforcement would rapidly loosen fidelity and the suppression series overstates the regime''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism in the descendant seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 70, 0.31).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 80, 0.33).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 70, 0.59).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 80, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 70, 0.58).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 80, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel catastrophe_memory_transmission decomposes into three reading-stories of one standing regime — high-fidelity post-catastrophe ritual transmission. This file is the symbol_continuity_reading; the operational_competence_reading and hybrid_embedded_reading are separate constraints. Each authors its own epsilon over the same regime from its own lights: this reading prices form-fidelity itself as the survival mechanism and counts sacrificed adaptive capacity as the cost; the operational reading prices unrehearsed adaptation as lost competence; the hybrid reading prices any separation attempt as damage to fused content. The epsilon differences across the family are the measured disagreement between readings, not an error to reconcile; the files cross-link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
