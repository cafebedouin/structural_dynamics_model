% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Symbol-Continuity Reading of the Catastrophe Mourning-Practice Regime
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Post-catastrophe diaspora communities maintain a fixed mourning-practice
 *   regime: annual fast and memorial days, prescribed lament texts, household
 *   observances, and a dispensation process administered by liturgical
 *   authorities. The regime marks membership, synchronizes grief across
 *   dispersion, and carries the catastrophe's memory forward as the witness
 *   generation dies. This file instantiates ONE reading of the
 *   catastrophe_memory_kernel - the symbol-continuity reading, under which
 *   the fixed forms exist to preserve symbolic content and collective
 *   identity across time - and authors epsilon only for that reading's
 *   constraint. The sibling readings (survival-competence, trauma-encoding,
 *   boundary-maintenance) instantiate different constraints over the same
 *   practices, with different beneficiary/victim structures and different
 *   epsilon values; they are linked through network edges, never averaged
 *   into this story. The claim/metric independence rule is honored: the
 *   claimed type is what this reading's seat takes to be structurally true,
 *   and the metrics describe the regime's observed operation without being
 *   tuned to agree with the claim.
 *
 * KEY AGENTS:
 *   - liturgical_authorities: agenda-setting administrator [institutional/identity_locked] - maintains the calendar, adjudicates dispensations, trains transmitters; office and standing depend on the forms staying fixed
 *   - observant_households: primary beneficiary with real cost-bearing [moderate/constrained] - keep the calendar at home, receive belonging and identity assurance, bear observance labor
 *   - adaptation_pressured_members: primary target [powerless/trapped] - illness, caregiving, shift work, intermarriage, and distance make fixed observance costly; dispensation carries stigma, exit severs kin and burial standing
 *   - ritual_reform_advocates: sanctioned proposer [moderate/constrained] - press for shortened rites, vernacular laments, inclusive practice; meet pulpit admonition and reputational cost
 *   - younger_diaspora_generation: drifting beneficiary [moderate/mobile] - inherit the calendar third-hand, attend episodically, can and quietly do exit to private or civic mourning
 *   - catastrophe_survivor_elders: legitimating beneficiary [organized/identity_locked] - the witness generation whose testimony the forms honor; supply moral authority to enforcement at little rigidity cost to themselves
 *   - comparative_ritual_scholars: analytical observer [analytical/analytical] - document drift, sanction patterns, and transmission outcomes across communities; no stake in form-fixity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.34).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Symbol-Continuity Reading of the Catastrophe Mourning-Practice Regime").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'de918fe7-b0e9-40d2-b75d-401571ff9dc5').
narrative_ontology:cs_kernel_codification('de918fe7-b0e9-40d2-b75d-401571ff9dc5', fixed_text).
narrative_ontology:cs_authority_grounding('de918fe7-b0e9-40d2-b75d-401571ff9dc5', lineage).
narrative_ontology:cs_interpretation_layer_present('de918fe7-b0e9-40d2-b75d-401571ff9dc5').
narrative_ontology:cs_reading_relation('de918fe7-b0e9-40d2-b75d-401571ff9dc5', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('de918fe7-b0e9-40d2-b75d-401571ff9dc5', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('de918fe7-b0e9-40d2-b75d-401571ff9dc5', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('de918fe7-b0e9-40d2-b75d-401571ff9dc5', foundational, symbolic_form_carries_identity_across_generations).
narrative_ontology:cs_axiom_status(symbolic_form_carries_identity_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('de918fe7-b0e9-40d2-b75d-401571ff9dc5', symbolic_form_carries_identity_across_generations, deontological).
narrative_ontology:cs_axiom('de918fe7-b0e9-40d2-b75d-401571ff9dc5', secondary, ritual_fixity_outweighs_adaptive_modification).
narrative_ontology:cs_axiom_status(ritual_fixity_outweighs_adaptive_modification, holdable).
narrative_ontology:cs_axiom_grounding('de918fe7-b0e9-40d2-b75d-401571ff9dc5', ritual_fixity_outweighs_adaptive_modification, instrumental).
narrative_ontology:cs_reference_frame('de918fe7-b0e9-40d2-b75d-401571ff9dc5', fixed_mourning_form_as_identity_vessel).
narrative_ontology:cs_drift_state('de918fe7-b0e9-40d2-b75d-401571ff9dc5', post_witness_generational_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('de918fe7-b0e9-40d2-b75d-401571ff9dc5', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, liturgical_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, observant_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_survivor_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, younger_diaspora_generation).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptation_pressured_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, ritual_reform_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, observant_households).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, younger_diaspora_generation).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, symbolic_continuity_across_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_persistence_through_fixed_mourning_form).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the mourning calendar, publish the observance rules, adjudicate dispensation requests, and train the next generation of transmitters. Their office, standing, and vocation exist because the forms stay fixed; permitting systematic modification would dissolve the role that constitutes their place in the community. Leaving the role entirely would mean abandoning the communal identity they have embodied for their working lives.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, liturgical_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Keep the calendar at home: fast days, memorial candles, lament recitations, attendance at communal commemorations. They receive belonging, legible identity, and a sanctioned way to grieve with their dead. They also pay in time, disrupted work and school schedules, and the labor of teaching children practices whose original context they did not live. Reducing observance invites family disappointment and communal comment; leaving altogether means losing the kin network, marriage pool, and burial arrangements that structure their lives.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, observant_households, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, observant_households, payer).

% Members whose circumstances resist the fixed forms: chronic illness that makes fasting dangerous, shift work that collides with commemoration times, caregiving obligations, intermarriage, or residence far from any congregation. Requesting a dispensation marks them publicly as exceptions; observing silently against medical or economic interest harms them; reducing observance draws sanction; full exit severs family ties, community standing, and access to communal lifecycle services. Every available path costs them something the forms do not refund.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptation_pressured_members, payer,
    powerless, biographical, trapped, national).

% Press for shortened rites, vernacular laments, gender-inclusive practice, and accommodation of modern work life. They win occasional marginal accommodations - a shortened service here, a private dispensation there - while systemic modification meets pulpit admonition, reputational cost, and accusations of disloyalty to the dead. They stay inside the community because their objection is to the forms' rigidity, not to the community itself, which leaves them paying the social price of persistent dissent.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_reform_advocates, payer,
    moderate, biographical, constrained, national).

% Inherited the calendar third-hand: they know the memorial days as school assemblies and family dinners more than as lived catastrophe. They attend commemorations episodically, observe selectively, and increasingly supplement or replace communal forms with private and civic mourning. Nothing physically prevents them from walking away, and many quietly do - which means the regime holds them weakly, and their drift registers as attrition rather than rebellion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, younger_diaspora_generation, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, younger_diaspora_generation, payer).

% The witness generation whose testimony the fixed forms exist to honor. They attend commemorations as central figures, lend moral authority to enforcement, and experience proposed modifications as threats to the dignity of their dead. The forms impose almost no rigidity cost on them - their lives are already ordered around the calendar - and their remaining years give them a short horizon in which preservation, not adaptation, is the only meaningful goal.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_survivor_elders, beneficiary,
    organized, biographical, identity_locked, global).

% Study mourning-practice regimes across communities and centuries: documenting how forms drift, how sanction operates, what transmits and what fades between generations. They take testimony from every seat, publish analyses the authorities may dispute, and hold no stake in whether the forms remain fixed. Their seat sees the whole structure - coordination achievement, cost distribution, and enforcement machinery together - without standing inside any of it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, liturgical_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes a dispersed community's mourning calendar and symbolic repertoire so that successive generations inherit the same catastrophic memory in the same forms: shared fast days, fixed memorial dates, common lament texts, and household observances solve the collective-action problem of transmitting memory without living witnesses, a central archive, or territorial continuity.
% TRANSFER_FUNCTION: Moves observance labor and conformity cost from member households toward the maintenance of fixed symbolic form; moves identity assurance, belonging, and a sanctioned grief practice to observing members; moves interpretive standing, office, and vocation to the authorities who administer the calendar and gate dispensations.
% ABSENT_VOICES: Secularized descendants who left the community would object that fixed forms alienate the very heirs the regime claims to bind; they are absent because exit stripped them of standing to speak. Non-observant descendants of the catastrophe's victims who mourn in private or civic registers are likewise outside the room. Inside the room, adaptation-pressured members speak only through dispensation requests framed as exceptions to be granted, not as design input to be weighed.
% DISAPPEARANCE_RATIONALE: If the fixed mourning calendar and its enforcement vanished overnight, commemoration would fragment into private and civic registers within a generation or two: synchronized communal grief days would lapse, lament texts would lose their performance occasions, the authorities' office would lose its function, and the community's shared identity marker would dissolve into heterogeneous family memory. The rearrangement would be gradual but structural - which is precisely why the regime is defended so consistently.
% FOUNDING_PROBLEM: After catastrophe destroyed the community's territory, institutions, and much of its population, the survivors faced a transmission problem: how to keep the dead present and the collective identity intact across dispersion and succeeding generations once the living witnesses begin to die - memory coordination with no state, no archive, and no territory to anchor it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: memory-studies historiography and survivor-cohort demography independently document accelerating witness attrition and treat intergenerational transmission as an open and intensifying problem; civic memorial institutions and diaspora archives record the same transmission anxiety in their own programming. No source outside the benefiting parties attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-low (0.34 at interval end) because the regime's costs are real but bounded: observance labor, restricted schedules, and dispensation stigma, weighed - by this reading's own lights - against a continuity benefit most members actively value. Suppression (0.42) reflects social sanction and gatekept dispensation rather than coercion; it is authored as a raw structural property and is not scaled by power or scope - only extractiveness is scaled downstream. Theater (0.24) is low but rising: the forms still function, yet a growing share of observance is heritage performance by members who did not live the catastrophe. Accessibility collapse (0.38) is well below mountain range because alternatives persist - private mourning, civic memorial, modified home practice, secular exit. Resistance (0.45) captures reform advocacy and quiet generational attrition. The temporal series run on one shared grid (t = 0,10,20,30,40,50,60) with every tracked metric authored at every point; the suppression_requirement series is included deliberately because the story traces enforcement-capacity change: as drift pressure grew, sanction machinery hardened (pulpit admonition, dispensation gatekeeping, standing consequences) rather than staying static. The annual ritual cycle produces seasonal intensity, but the decade-scale series tracks structural drift, so monotone points are the honest shape. Coalition potential for the powerless seat exists in principle - dispensation-requester networks and reform caucuses - but is weak in fact: members are geographically dispersed, stigma suppresses visible organizing, and the identity stakes that trap them also discourage coalition formation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the liturgical-authority seat the regime is sacred stewardship it personally maintains - coordination-dominant, with extraction registering mainly as the burden of administration. From the adaptation-pressured seat the same regime is a costly imposition administered by people who do not bear its costs - extraction-dominant. Observant households sit near-symmetric: genuine belonging received, real labor paid. Identity-lock binds two seats: for the authorities the lock is institutional (the office has become their communal self; exit means ceasing to be who they are in the community), and for the survivor elders it is testimonial-relational (the fixed forms are the vessel of their dead; loosening them feels like a second abandonment). If the authority identity frame broke - a reform movement capturing the institutions - the authorities' computed classification would shift sharply toward payer, and the regime's enforcement profile would soften within a generation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations plus exit options drive the derivation, and no overrides are needed. Liturgical authorities: beneficiary and agenda-setter with identity_locked exit - low d, near the beneficiary end, though their stewardship labor keeps them off zero. Catastrophe survivor elders: pure beneficiary at negligible cost - very low d. Observant households: listed as beneficiaries but bearing real observance costs with constrained exit - the derivation lands them near-symmetric, which matches the situation. Adaptation-pressured members: victims with trapped exit - high d, near the full-target end; trapped or identity-locked targets sit nearer full-target than mobile ones, and this seat is the regime's extraction center of gravity. Reform advocates: victims with constrained exit - high d, slightly below the trapped seat because partial exit (quiet private observance) is available. Younger diaspora generation: listed as beneficiaries with mobile exit - the derivation pulls them toward the beneficiary end, and that is accepted rather than overridden: their mobility genuinely dampens what the regime can take from them, which is exactly why they drift rather than fight. Scope is continental-to-global (dispersed diaspora), which modestly amplifies effective extraction by making enforcement verification harder at the margins.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem - transmitting catastrophe memory past the dying witness generation - is live and intensifying, corroborated from outside the beneficiary set. The tangled_rope claim prevents two opposite misreadings. Reading the regime as pure rope would erase the documented rigidity costs borne by a trapped minority and the sanction machinery that holds the forms fixed; reading it as snare would erase the genuine coordination achievement - synchronized intergenerational memory sustained without state power, territory, or archive. The hybrid classification holds both facts. The watch condition for mandatrophy is visible in the theater_ratio series: if the transmission problem were ever solved elsewhere (state memorials, archival saturation, civic incorporation), the regime's function would atrophy while the forms persisted as performance - the signature drift toward piton. The current series shows the function still dominant, with theatrical share rising slowly from a low base.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the symbol_continuity_reading of the catastrophe_memory_kernel; would instantiating a sibling reading (survival_competence, trauma_encoding, or boundary_maintenance) over the same mourning-practice regime produce a different classification?',
    'Author the three sibling stories against the same standing arrangement and compare computed per-seat types; the disagreement is located in what the fixed forms are FOR, which changes the beneficiary/victim sets and the weighting of rigidity costs.',
    'The survival_competence reading would likely credit the forms with operational yield and lower measured rigidity cost; the boundary_maintenance reading would raise suppression because exclusion becomes the point of the practice; the trauma_encoding reading would reassign the victim set to those carrying hypervigilance burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame contest: four readings of one kernel, each a separate constraint with its own epsilon.').

omega_variable(
    continuity_good_vs_captured_gain,
    'Does the regime''s gain accrue to the abstract continuity good alone (no seat captures it), or does it land concretely on the liturgical-authority seat as office, standing, and vocation?',
    'Trace the material and status dependence of authority careers on form-fixity, and compare against a counterfactual community operating modified forms: who loses position, income, or standing?',
    'If gains are purely abstract, the regime trends rope-ward and the extraction component is coordination overhead; if the authority seat captures them, the tangled_rope reading firms and effective extraction for that seat rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_good_vs_captured_gain, empirical, 'Whether tradition-continuity is a vindicated proposition with no rent-collector or a cover for concentrated authority gains.').

omega_variable(
    rigidity_cost_concentration,
    'How concentrated are the costs of ritual rigidity among adaptation-pressured members, and are they growing as member circumstances diversify?',
    'Dispensation-request rates, survey evidence of quiet non-observance, and cohort studies of members who reduce or abandon observance after life disruptions (illness, relocation, intermarriage).',
    'Rising concentration on a trapped minority pushes the regime snare-ward from that seat; diffuse shallow costs support a rope-ward reading in which most participants are net beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigidity_cost_concentration, empirical, 'Distribution and trajectory of the rigidity costs this reading declares as the victim side.').

omega_variable(
    conformity_enforcement_mechanism,
    'Is member conformity held by structural sanction (loss of standing, dispensation stigma, communal pressure) or by internalized duty (guilt, filial obligation to the dead) that would persist if the sanction machinery relaxed?',
    'Compare observance levels in small diaspora cells with weak sanction capacity against institutional centers with strong enforcement, controlling for piety demographics.',
    'A large internalized share means true suppression exceeds the structural measure and reform would not follow automatically from relaxing enforcement; a structural share means the sanction machinery is the operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conformity_enforcement_mechanism, empirical, 'Structural versus internalized suppression mechanism in communal mourning-conformity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_symbol_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_tr_t0, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_tr_t10, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_tr_t10, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_tr_t20, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_tr_t30, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_tr_t30, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_tr_t40, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_tr_t50, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_symbol_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_be_t0, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_be_t10, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_be_t10, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_be_t20, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_be_t30, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_be_t30, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_be_t40, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_be_t50, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_symbol_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_su_t0, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_su_t10, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_su_t10, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_su_t20, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_su_t30, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_su_t30, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_su_t40, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_su_t50, observed).
narrative_ontology:measurement(catastrophe_memory_symbol_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(catastrophe_memory_symbol_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'catastrophe mourning ritual' covers four structurally distinct claims about what the fixed forms do. This story authors the symbol-continuity claim only, with its own epsilon (moderate-low: symbolic transmission whose rigidity costs fall on an adaptation-pressured minority), its own beneficiary set (including tradition-continuity routed to vindicated_propositions rather than beneficiaries, since an abstract good collects no rents), and its own victims. The upstream/downstream structure runs through shared practice: whichever reading a community's authorities adopt shapes how they defend the forms, so each sibling story links back to this one via affects_constraints. Sibling epsilons are expected to differ - survival-competence credits operational yield (lower net extraction), boundary-maintenance elevates the exclusion function (higher suppression), trauma-encoding relocates the victim set (hypervigilance burden) - and those differences are the measurement the family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
