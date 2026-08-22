% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed as Binding Metaphysical Ontology — Strict Orthodox Reading
 *   domain: systematic_theology/ecclesiology/history_of_doctrine
 *
 * SUMMARY:
 *   This story instantiates the strict orthodox reading of the Nicene Creed:
 *   the creed is understood to bind all believers to a single specified
 *   metaphysical ontology (the homoousion, the two-natures Christology, the
 *   Trinitarian relations as formally defined), such that deviation
 *   constitutes heresy warranting ecclesial and, historically, civil
 *   sanction. This reading is one of three structurally distinct constraints
 *   sharing the Nicene Creed as their kernel — the
 *   liturgical_habituation_reading treats the creed as an identity-boundary
 *   practice independent of cognitive metaphysical assent, and the
 *   symbolic_confessional_reading treats it as historically contingent
 *   witness authorized by community discernment rather than fixed ontology.
 *   Each reading has its own ε: this reading's extraction is high (0.68)
 *   because it authors a concrete enforcement apparatus — councils,
 *   anathemas, deposition, historically allied civil penalty — targeting a
 *   named victim class (heterodox communities, lay interpreters, excluded
 *   minority traditions) for the benefit of a named beneficiary class (the
 *   hierarchy and its supporting theological establishment). The other two
 *   readings would not authorize comparable extraction because they do not
 *   tie communion or standing to assent on a fixed metaphysical proposition.
 *
 * KEY AGENTS:
 *   - episcopal_hierarchy: agenda-setter and beneficiary — administers doctrine and sanctions
 *   - credal_orthodoxy_theologians: beneficiary — supplies intellectual legitimation, receives patronage
 *   - heterodox_communities: primary target — bears confiscation, exclusion, historical civil penalty
 *   - lay_interpreters: secondary target — bears catechetical discipline for private deviation
 *   - state_civil_authorities: co-agenda-setter in establishment periods — supplies coercive force
 *   - non_creedal_minority_traditions: excluded — never had standing in the interpretive process
 *   - historical_theologians_observer: analytical seat — traces the mechanism without power to alter it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.79).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed as Binding Metaphysical Ontology — Strict Orthodox Reading").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '50b8c009-8926-4267-a929-4c90666656b6').
narrative_ontology:cs_kernel_codification('50b8c009-8926-4267-a929-4c90666656b6', fixed_text).
narrative_ontology:cs_authority_grounding('50b8c009-8926-4267-a929-4c90666656b6', lineage).
narrative_ontology:cs_interpretation_layer_present('50b8c009-8926-4267-a929-4c90666656b6').
narrative_ontology:cs_reading_relation('50b8c009-8926-4267-a929-4c90666656b6', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('50b8c009-8926-4267-a929-4c90666656b6', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_axiom('50b8c009-8926-4267-a929-4c90666656b6', foundational, single_correct_ontology_is_binding_on_conscience).
narrative_ontology:cs_axiom_status(single_correct_ontology_is_binding_on_conscience, holdable).
narrative_ontology:cs_axiom_grounding('50b8c009-8926-4267-a929-4c90666656b6', single_correct_ontology_is_binding_on_conscience, deontological).
narrative_ontology:cs_axiom('50b8c009-8926-4267-a929-4c90666656b6', foundational, deviation_from_specified_ontology_constitutes_culpable_heresy).
narrative_ontology:cs_axiom_status(deviation_from_specified_ontology_constitutes_culpable_heresy, holdable).
narrative_ontology:cs_axiom_grounding('50b8c009-8926-4267-a929-4c90666656b6', deviation_from_specified_ontology_constitutes_culpable_heresy, conventional).
narrative_ontology:cs_reference_frame('50b8c009-8926-4267-a929-4c90666656b6', conciliar_ontological_settlement).
narrative_ontology:cs_drift_state('50b8c009-8926-4267-a929-4c90666656b6', post_reformation_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('50b8c009-8926-4267-a929-4c90666656b6', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, credal_orthodoxy_theologians).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, non_creedal_minority_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, state_civil_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, ratifies the creed's wording as binding doctrine, and administers sanction mechanisms — excommunication, deposition, anathema — against clergy and laity who deviate from the specified ontology (homoousios, Trinitarian relations, the two natures). Derives institutional authority, doctrinal gatekeeping power, and resource control (church property, ordination pipelines) from being the sole legitimate interpreter of what the creed's metaphysical claims mean and who has violated them.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy, beneficiary).

% Careers, teaching posts, and publication platforms depend on defending the creed's ontological claims as settled and non-negotiable. They supply the intellectual apparatus that translates hierarchical sanction into theological argument, and in turn receive institutional legitimacy, patronage, and protection from the same hierarchy whose authority they underwrite.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, credal_orthodoxy_theologians, beneficiary,
    organized, generational, constrained, continental).

% Groups holding alternative accounts of Christ's nature or the Trinity (historically Arian, Nestorian, Monophysite, and later various dissenting communities) are declared heretical under the strict ontological reading. They face confiscation of church buildings, loss of clerical status, social exclusion, and in historical periods, civil penalties enforced by state power acting at the church's request. Their exit is blocked because leaving the creed's jurisdiction means leaving the only recognized channel of ecclesial legitimacy in their region.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, generational, trapped, regional).

% Ordinary believers who arrive at the creed's language through devotional or philosophical reflection different from the official ontological reading are required to submit their private understanding to hierarchical correction. They bear the cost of catechetical discipline, exclusion from sacraments, or social stigma if their expressed beliefs are judged deviant, even absent any intent to found a rival community.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, biographical, constrained, local).

% Communities whose Christology or Trinitarian language was never admitted to the councils that produced the creed (or was condemned there) have no seat in the ongoing interpretive process. They would object that the ontological reading forecloses their historically prior or independently developed Christological grammar, but the sanction mechanism was built specifically to exclude their claims from consideration as live options.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, non_creedal_minority_traditions, excluded,
    powerless, generational, trapped, regional).

% In periods of church-state establishment, civil rulers enforce credal conformity through law — banishment, property seizure, criminal penalty for heresy — receiving in exchange the church's blessing on political legitimacy and a unified population easier to govern. They administer coercive force the ecclesial hierarchy alone could not apply.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, state_civil_authorities, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, state_civil_authorities, beneficiary).

% Historians of doctrine and comparative theologians trace how the councils' decisions were reached, who was present, who was excluded, and how the ontological reading became the operative one among several live options at Nicaea and Chalcedon. They document the mechanism without holding institutional power to alter it.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, historical_theologians_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, shared Christological and Trinitarian grammar that lets geographically dispersed communities recognize each other as holding the same faith, enabling communion, mutual recognition of clergy, and a stable teaching tradition across centuries.
% TRANSFER_FUNCTION: Moves interpretive authority, property, clerical standing, and social legitimacy from communities and individuals holding alternative ontological accounts to the hierarchy and theologians who administer the sanctioned reading; moves social and material cost (exclusion, confiscation, civil penalty) onto those judged heterodox.
% ABSENT_VOICES: The condemned parties at Nicaea, Constantinople, Ephesus, and Chalcedon — and their theological descendants among non-Chalcedonian and non-Trinitarian communities — are structurally excluded from the ongoing interpretive body that judges their claims heretical; they would argue the ontological formula settled a live philosophical dispute by vote and force rather than by demonstrated necessity.
% DISAPPEARANCE_RATIONALE: If binding, sanction-backed ontological uniformity vanished, communion would no longer require single-metaphysics agreement; excommunicated and historically condemned communities would be free to hold clerical standing and church property on equal footing; theological careers built on defending the fixed ontology would lose their institutional anchor; the visible unity the hierarchy currently administers would fragment into a plurality of Christological grammars.
% FOUNDING_PROBLEM: Early Christian communities held incompatible accounts of Christ's relation to God the Father (Arian subordinationism vs. homoousios) and needed some resolution to prevent schism and preserve a recognizable common faith across a rapidly Christianizing empire.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy attests the ontological dispute remains live wherever heterodox Christologies persist (e.g., ongoing dialogue with Oriental Orthodox and other non-Chalcedonian churches) and treats the strict reading as still doctrinally necessary. Independent historians of the councils and representatives of the condemned traditions themselves attest that the philosophical dispute was substantive at the time but that its resolution by imperial-backed conciliar vote, rather than continued theological development, is what fixed the ontology as binding rather than the ontological question itself remaining genuinely unsettled — corroboration exists outside the beneficiary set, but it disputes the founding problem's continued liveness rather than confirming it.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high and rising over the interval (0.42 to 0.68) reflecting the historical trajectory from a genuinely contested philosophical dispute at Nicaea toward an increasingly rigid, institutionally self-reinforcing orthodoxy apparatus by the post-Chalcedonian and medieval periods. Suppression rises even faster and peaks mid-interval (0.85) during periods of active civil-ecclesial cooperation in heresy prosecution, then eases slightly as state enforcement machinery weakens in later centuries while ecclesial sanction persists on its own. Theater ratio stays comparatively low throughout (0.10 to 0.28) because the enforcement mechanism retained a substantive function — actual exclusion from communion, office, and property — rather than becoming merely performative; this distinguishes the strict orthodox reading from a piton trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat, the creed's binding ontological claim is the coordination achievement that prevented Christological fragmentation and preserved recognizable unity across a vast and diverse church — a genuine rope. From the seat of a condemned community, the same structure is an imposed metaphysical settlement backed by confiscation and exile, indistinguishable in its lived effect from pure extraction. The engine computes both seats from the same structural data; the divergence is exactly what a tangled_rope classification is built to register — both a real coordination function (shared Christological grammar enabling mutual recognition) and asymmetric extraction (costs concentrated on those excluded from having shaped the settlement) coexist in the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy and allied theologians sit near the full-beneficiary end: they set the ontology's content, administer its enforcement, and derive institutional standing, resource control, and career structure from being its sole legitimate interpreters — arbitrage-grade exit (they can revise doctrine through conciliar process while retaining authority). Heterodox communities and lay interpreters sit near the full-target end: trapped or constrained exit, bearing concentrated costs (property loss, exclusion, civil penalty) for holding a metaphysical position outside the sanctioned range. State civil authorities are treated as a secondary beneficiary/agenda-setter pair because establishment-era enforcement was materially dependent on state coercive capacity, not ecclesial authority alone — this is a genuinely inter-institutional dynamic, not merely intra-ecclesial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving incompatible Christological accounts to prevent schism in a Christianizing empire) was live and substantive at Nicaea. Whether it remains live for the strict ontological reading specifically, or whether the reading has calcified into administrative self-perpetuation independent of the original theological dispute, is genuinely contested — corroboration from outside the hierarchy (comparative historians, non-Chalcedonian traditions) suggests the ontological question itself has had defensible alternative resolutions for over a millennium, while the hierarchy continues to treat the strict reading as doctrinally non-negotiable. This is not adjudicated here; it is the structural ambiguity the omega variables document.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontology_vs_practice_kernel_ambiguity,
    'Does the Nicene Creed''s authority ultimately rest on binding metaphysical assent (this reading), on liturgical-practical identity formation (liturgical_habituation_reading), or on contingent communal discernment (symbolic_confessional_reading)? The historical record contains textual evidence compatible with all three.',
    'Comparative analysis of how heresy trials historically weighted stated belief versus liturgical participation versus community standing; examination of whether condemned parties were sanctioned for holding a different metaphysical view privately or for refusing public conciliar submission.',
    'If historical sanction practice targeted liturgical non-conformity rather than private metaphysical belief, this strict_orthodox_reading overstates the ontological-assent mechanism relative to the liturgical_habituation_reading, and its authored extraction level would need revision downward toward that sibling''s lower ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontology_vs_practice_kernel_ambiguity, conceptual, 'Which of the three kernel readings best captures the creed''s actual historical operating mechanism.').

omega_variable(
    genuine_natural_law_vs_constructed_orthodoxy,
    'Is the specific metaphysical settlement (homoousios, two-natures Christology) a discoverable theological truth the councils correctly identified, making deviation genuinely erroneous, or a constructed political-theological settlement among live philosophical options that could have gone otherwise?',
    'This is not empirically resolvable by historical method alone; it depends on theological commitments about whether conciliar determination tracks truth. Partial evidence: examine whether condemned positions (Arianism, Nestorianism) were philosophically coherent internally and had substantial contemporary support, which would weigh against the settlement being a discovered necessity.',
    'If the ontology is a genuine discovered truth, the sanction apparatus is defensible as truth-protection rather than extraction; if constructed, the apparatus is better read as institutional self-interest wearing a metaphysical-necessity framing — directly bearing on whether the coordination function is real or cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_natural_law_vs_constructed_orthodoxy, preference, 'Whether the bound ontology is discovered theological truth or a constructed, contestable settlement — the central irreducible uncertainty of this reading.').

omega_variable(
    coercion_separability,
    'Was the imperial civil-penalty mechanism separable from the strict ontological reading''s persistence, or is sustained doctrinal uniformity of this kind structurally dependent on state coercive backing?',
    'Examine periods and regions where ecclesial sanction operated without state enforcement power (e.g., post-establishment eras, minority church contexts) to see whether doctrinal uniformity was maintained at comparable strength through purely ecclesial means (excommunication, social exclusion) alone.',
    'If separable, the constraint''s suppression component is largely attributable to the state_civil_authorities seat rather than intrinsic to the creed''s ontological claim, suggesting the ecclesial-only mechanism is less severely a tangled_rope and closer to a rope with occasional snare-like state capture episodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_separability, empirical, 'Whether doctrinal sanction requires state coercive backing or is self-sustaining through ecclesial means alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nice_tr_t20, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(nice_tr_t40, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(nice_tr_t60, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(nice_tr_t80, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(nice_tr_t100, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nice_be_t20, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(nice_be_t40, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(nice_be_t60, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(nice_be_t80, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(nice_be_t100, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(nice_su_t20, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(nice_su_t40, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(nice_su_t60, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(nice_su_t80, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(nice_su_t100, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'the authority of the Nicene Creed' per the ε-invariance principle. strict_orthodox_reading (this file) authors high extraction (0.68) via a binding-ontology-plus-sanction mechanism with clear beneficiary (hierarchy) and victim (heterodox/lay dissenters) sets. symbolic_confessional_reading authors near-zero extraction because authority is grounded in ongoing community discernment rather than fixed propositional assent — no sanctioned victim class. liturgical_habituation_reading authors low-moderate extraction because identity-boundary maintenance through practice has real but much milder exclusionary cost than metaphysical heresy sanction. The three share the same textual kernel (the Nicene Creed itself) but instantiate structurally distinct constraints with different ε, different stakeholders, and different classifications — they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
