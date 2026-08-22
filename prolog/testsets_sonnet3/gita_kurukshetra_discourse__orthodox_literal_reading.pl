% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading of the Bhagavad Gita's Caste-Duty and Righteous-War Doctrine
 *   domain: religious/social/ethical
 *
 * SUMMARY:
 *   This story is one reading of the Kurukshetra discourse kernel — the
 *   dialogue between Krishna and Arjuna at the opening of battle, forming the
 *   Bhagavad Gita. The orthodox literal reading holds that the text mandates
 *   caste-assigned duty (svadharma tied to varna) as cosmically ordained and
 *   legitimates the kshatriya's violence in a righteous war as morally
 *   neutral when performed in fulfillment of that duty, with Brahmin
 *   commentators retaining sole authority to fix the text's meaning. This is
 *   distinct from the gandhian_allegorical_reading (battlefield as metaphor
 *   for internal spiritual struggle, violence non-literal) and the
 *   universalist_devotional_reading (bhakti as caste-independent path, dharma
 *   as surrender rather than social role). The three readings are not the
 *   same constraint measured differently — each has a distinct
 *   beneficiary/victim structure and a distinct epsilon. This story authors
 *   epsilon for the orthodox literal reading's standing arrangement (caste
 *   hierarchy plus sanctioned violence) as that reading's own tradition
 *   presents it, not for what a reformist alternative would look like.
 *
 * KEY AGENTS:
 *   - brahmin_interpretive_class: agenda_setter/beneficiary (institutional/arbitrage) — controls textual meaning
 *   - kshatriya_warrior_lineages: beneficiary (powerful/constrained) — receives moral exemption for violence
 *   - caste_hierarchy_incumbents: beneficiary (organized/constrained) — hierarchy stabilized as divine order
 *   - lower_caste_and_outcaste_groups: payer (powerless/trapped) — bears servitude framed as duty
 *   - war_dead_and_conscripted_combatants: payer (powerless/trapped) — bears physical cost of sanctioned violence
 *   - women_barred_from_dharmic_agency: payer (powerless/trapped) — dharma defined relationally, not independently
 *   - reformist_and_bhakti_movements: excluded (moderate/constrained) — alternate reading marginalized historically
 *   - comparative_textual_scholars: observer (analytical/analytical) — sees the layered textual and social history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.68).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading of the Bhagavad Gita's Caste-Duty and Righteous-War Doctrine").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/social/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '529c34a0-2535-4b1d-9d41-a07952dd45b5').
narrative_ontology:cs_kernel_codification('529c34a0-2535-4b1d-9d41-a07952dd45b5', fixed_text).
narrative_ontology:cs_authority_grounding('529c34a0-2535-4b1d-9d41-a07952dd45b5', lineage).
narrative_ontology:cs_interpretation_layer_present('529c34a0-2535-4b1d-9d41-a07952dd45b5').
narrative_ontology:cs_reading_relation('529c34a0-2535-4b1d-9d41-a07952dd45b5', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('529c34a0-2535-4b1d-9d41-a07952dd45b5', gita_kurukshetra_discourse__universalist_devotional_reading, influences).
narrative_ontology:cs_axiom('529c34a0-2535-4b1d-9d41-a07952dd45b5', foundational, caste_born_duty_is_cosmic_order).
narrative_ontology:cs_axiom_status(caste_born_duty_is_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('529c34a0-2535-4b1d-9d41-a07952dd45b5', caste_born_duty_is_cosmic_order, theological).
narrative_ontology:cs_axiom('529c34a0-2535-4b1d-9d41-a07952dd45b5', foundational, warrior_violence_in_dharmic_war_is_morally_neutral).
narrative_ontology:cs_axiom_status(warrior_violence_in_dharmic_war_is_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('529c34a0-2535-4b1d-9d41-a07952dd45b5', warrior_violence_in_dharmic_war_is_morally_neutral, deontological).
narrative_ontology:cs_axiom('529c34a0-2535-4b1d-9d41-a07952dd45b5', secondary, brahmin_lineage_holds_exclusive_interpretive_authority).
narrative_ontology:cs_axiom_status(brahmin_lineage_holds_exclusive_interpretive_authority, overridden).
narrative_ontology:cs_axiom_grounding('529c34a0-2535-4b1d-9d41-a07952dd45b5', brahmin_lineage_holds_exclusive_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('529c34a0-2535-4b1d-9d41-a07952dd45b5', varna_ordained_dharma_order).
narrative_ontology:cs_drift_state('529c34a0-2535-4b1d-9d41-a07952dd45b5', post_independence_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('529c34a0-2535-4b1d-9d41-a07952dd45b5', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_lineages).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_incumbents).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_and_outcaste_groups).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_and_conscripted_combatants).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, women_barred_from_dharmic_agency).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, svadharma_over_universal_ethics).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, caste_born_duty_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, sanctioned_violence_in_righteous_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls Sanskrit textual transmission, commentary tradition (bhashya), and ritual authority to declare what the verses mean and how svadharma applies to each caste. Determines which readings of Krishna's counsel to Arjuna are orthodox. Collects social deference, ritual fees, and institutional position from being the sanctioned interpreter; can revise interpretation to suit circumstance while denying that flexibility to others.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, beneficiary).

% Receives explicit textual sanction for killing in battle as fulfillment of caste-born duty rather than moral transgression — Krishna's counsel to Arjuna is read as removing personal moral liability for violence performed in the warrior role. This underwrites martial rule and land-holding power across generations; exit from the warrior role is discouraged as dharmic failure, but the caste's material position benefits from the doctrine.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_lineages, beneficiary,
    powerful, generational, constrained, regional).

% Landowning and administrative castes positioned favorably within the fourfold varna order benefit from the text's framing of caste assignment as divinely instituted (verse 4.13) rather than socially constructed. Their relative position is stabilized by a doctrine that makes rebellion against caste role a form of adharma.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_incumbents, beneficiary,
    organized, civilizational, constrained, continental).

% Assigned birth-determined duties at the bottom of the ordained hierarchy; the same verses that grant kshatriyas violence-without-guilt assign them servitude-without-appeal. Under the orthodox literal reading, seeking to leave their caste-designated role or contesting the hierarchy is framed as spiritual failure rather than legitimate grievance. Historically barred from Sanskrit literacy needed to contest the interpretive monopoly directly.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_and_outcaste_groups, payer,
    powerless, generational, trapped, continental).

% Common soldiers and conscripts on both sides of a dharmic war fought under this doctrine's sanction bear the direct physical cost of violence legitimated as righteous duty. The text's counsel is addressed to a princely warrior (Arjuna) with a personal moral crisis; the rank-and-file who die carrying out the sanctioned violence have no comparable voice in the text and no dharmic exemption from the battlefield.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_and_conscripted_combatants, payer,
    powerless, immediate, trapped, regional).

% Under the orthodox literal reading's caste-and-role framework, women's dharma is typically subordinated to marital and household duty rather than treated as an independent path of action or renunciation available to men of the higher varnas; their exit from this assignment is treated as dharmic transgression rather than a live option.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, women_barred_from_dharmic_agency, payer,
    powerless, civilizational, trapped, continental).

% Devotional and reform traditions arguing the text's core teaching is caste-independent surrender (bhakti) rather than caste-bound duty are historically marginalized from the Brahminical commentary tradition that fixes 'orthodox' meaning; their reading exists but is not the sanctioned one under this reading's interpretive monopoly.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, reformist_and_bhakti_movements, excluded,
    moderate, generational, constrained, continental).

% Study the historical layering of the text, the plurality of commentarial traditions (Shankara, Ramanuja, Madhva, and later reformist readings), and the social function of caste-duty doctrine in maintaining varna hierarchy, without themselves being bound by any single tradition's interpretive authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, comparative_textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared moral framework resolving the acute problem of a warrior facing combat against kin and teachers: it coordinates action under conditions of paralyzing moral conflict by subordinating individual conscience to role-based duty (svadharma) and detachment from outcome (nishkama karma).
% TRANSFER_FUNCTION: Moves moral liability for violence away from the individual kshatriya actor and onto the cosmic order of duty; simultaneously moves social deference, ritual authority, and hierarchical position toward Brahmin interpreters and upper-caste incumbents, and moves the costs of both caste subordination and sanctioned violence onto lower castes, common combatants, and women.
% ABSENT_VOICES: The voices of lower-caste subjects assigned servitude under the same doctrine that exempts warriors from guilt are structurally absent from the text's central dialogue, which occurs between a prince and his divine charioteer. Common soldiers who die in the sanctioned war, and women whose dharma is defined relationally rather than independently, are likewise not interlocutors in the text's central ethical resolution.
% DISAPPEARANCE_RATIONALE: Brahmin interpreters and caste incumbents would argue the moral and cosmological order itself unravels without the doctrine. Reformist and lower-caste-descended communities, and many contemporary readers, would argue the social hierarchy the doctrine underwrites would lose its principal scriptural legitimation, accelerating a shift already underway toward the universalist and allegorical readings — the world of formal caste subordination has substantially rearranged in modern India through legal and social reform even while this reading persists ceremonially, so verdicts differ by whose arrangement is asked about.
% FOUNDING_PROBLEM: A warrior's paralysis at the prospect of killing kinsmen and revered teachers in a legitimate succession war needed a resolvable framework; more broadly, a stratified agrarian society needed a stabilizing account of why birth-assigned social role and occasional sanctioned violence were not moral failures but cosmic order.
% FOUNDING_PROBLEM_CORROBORATION: Brahmin commentarial tradition and traditionalist institutions attest the founding problem (dharmic confusion, social stability) remains live and the text's resolution still applies. Independent historians of South Asian religion, Dalit scholars (e.g., B.R. Ambedkar's critique of the Gita's caste apologetics), and comparative textual critics outside the beneficiary tradition attest that the caste-duty component addresses a social-control problem rather than an enduring metaphysical one, and that the doctrine has been actively used to resist caste reform well after any claimed original context lapsed.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, contested).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.72 (rising from 0.55 at the interval's start) reflecting that the caste-duty component has been increasingly recognized and contested as a mechanism of social control layered onto a spiritual text, even as its core interpretive claim persisted institutionally. Suppression is high (0.68, drifting down modestly from 0.75) because enforcement of caste roles relied heavily on denial of Sanskrit literacy and ritual access to lower castes, a suppression mechanism that has partially eroded with modern literacy and legal reform without disappearing. Theater ratio rises over the interval (0.15 to 0.40) because as legal caste discrimination was formally abolished in modern India, much of the doctrine's operative force shifted from binding social enforcement to ceremonial/ritual affirmation — a genuine drift pattern worth tracking, not a static picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin interpreters sit at the clearest beneficiary pole: they set the interpretive terms and collect deference and institutional standing without bearing the hierarchy's costs (arbitrage exit — they can revise doctrine as needed). Kshatriya lineages and caste incumbents are beneficiaries with less mobility (constrained exit) because their material position is bound to a caste system they cannot easily exit without losing standing. Lower castes, war dead, and women are targets: trapped exit options, powerless standing, and the doctrine's own terms frame their subordination or death as sanctioned rather than a claim they may contest through orthodox channels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a warrior's moral paralysis before a legitimate but kin-killing war — is philosophically live as a general ethical dilemma, but the caste-duty apparatus riding alongside it has substantially lost its original social conditions (legal caste discrimination is now formally abolished in India) while the orthodox reading's authority persists in religious and cultural practice. Classifying this as tangled_rope rather than a pure snare preserves the genuine coordination function (resolving action-guiding moral paralysis, a real problem for the addressed agent) while still registering the asymmetric extraction (caste hierarchy stabilization, moral exemption for warrior violence) that rides on the same textual authority and requires active interpretive enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthodox_reading_committer_ambiguity,
    'Is the orthodox literal reading the text''s original/intended meaning, a later Brahminical accretion layered onto an earlier composite text, or one legitimate reading among several coexisting from antiquity?',
    'Comparative philology across textual strata (the Gita''s composite layers within the Mahabharata), comparison with contemporaneous dharma-shastra literature, and analysis of which caste-duty verses are structurally central versus peripheral to the core dialogue.',
    'If the caste-duty framing is a later accretion, the orthodox reading''s claim to textual fidelity weakens relative to the universalist and allegorical readings, shifting where interpretive authority should legitimately sit. If original, the orthodox reading''s historical primacy claim strengthens even as its normative force remains separately contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orthodox_reading_committer_ambiguity, conceptual, 'Whether the orthodox caste-duty reading reflects the text''s original layer or a later interpretive accretion.').

omega_variable(
    sibling_reading_foreclosure_relations,
    'Does the orthodox literal reading''s caste-duty claim logically foreclose the universalist_devotional_reading''s caste-independence claim, or can both be held as differently-scoped readings by different communities simultaneously?',
    'Textual analysis of whether verses affirming caste-birth duty (e.g. 4.13, 18.41-48) and verses affirming caste-independent devotional access (e.g. 9.32) are read by any single tradition as compatible, or whether traditions that hold one systematically reject the other.',
    'If foreclosing, only one reading can be doctrinally coherent within a given religious community at a time, sharpening the contest into a zero-sum interpretive struggle. If coexisting, both readings persist as live alternatives within the broader Vaishnava and Hindu traditions without one displacing the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_relations, conceptual, 'Whether the orthodox and universalist readings can coexist within a single tradition or structurally exclude each other.').

omega_variable(
    violence_sanction_scope_ambiguity,
    'Does the text''s sanction of Arjuna''s specific violence in a specific just war generalize into a broader doctrine legitimating caste-based martial violence across history, or is the orthodox reading''s generalization itself a later extension beyond the text''s narrow narrative scope?',
    'Historical analysis of how the text was invoked to justify martial and political violence across South Asian history (e.g., colonial-era and 20th-century political uses) versus its narrower narrative function within the epic.',
    'A narrow original scope with broad later application would indicate the extraction (caste-order stabilization via sanctioned violence) is substantially a construction added onto the text rather than intrinsic to it, supporting a lower epsilon for the text''s original function and a higher epsilon specifically for its later political deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_sanction_scope_ambiguity, empirical, 'Whether the righteous-violence doctrine''s broad application is textually intrinsic or a historical extension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 80, 0.71).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gita_kurukshetra_discourse kernel, each authored as a separate constraint per the epsilon-invariance principle. orthodox_literal_reading (this story) authors caste hierarchy and sanctioned righteous violence as intrinsic to the text's meaning, with Brahmin interpreters as agenda-setters/beneficiaries and lower castes plus war casualties as victims — epsilon 0.72, tangled_rope. gandhian_allegorical_reading treats the battlefield as metaphor for inner struggle, denying literal violence-sanction, with a correspondingly different (much lower) epsilon and different beneficiary structure. universalist_devotional_reading treats dharma as caste-independent devotional surrender, denying that caste hierarchy is central at all, again with a distinct epsilon and victim set. All three are linked here; each documents the relationship in its own narrative_context rather than averaging or hedging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
