% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Scriptural Varna Framework: Birth-Fixed Jati Boundaries with Pollution Sanction
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   Under the orthodox textual reading, jati boundaries are not custom or
 *   convenience but derivations from a fixed scriptural varna framework —
 *   ranked orders established in revelation, elaborated by the Dharmashastra
 *   literature, and binding at birth. The operative rule: each person's
 *   station, occupation, marriage circle, and ritual entitlement follow from
 *   birth, and crossing a boundary — eating together, marrying out, taking
 *   forbidden work, entering forbidden space — generates ritual pollution
 *   requiring expiation and inviting sanction. The arrangement assigns the
 *   defiling work of the village economy (scavenging, leatherwork, corpse
 *   disposal) to communities at the bottom, blocks their mobility, and
 *   concentrates interpretive authority, honor, and material dues at the top.
 *   Enforcement is distributed: priestly adjudication, caste-council
 *   sanction, social boycott, and episodic violence, sustained by a theodicy
 *   that reads birth-station as earned. KEY AGENTS (by structural
 *   relationship): see key_agents. Sibling readings of the same kernel
 *   (locally negotiated practice norms; census-administered categorization)
 *   are separate constraint stories with their own epsilon and victim sets;
 *   this file instantiates only the textual-fixity reading.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Primary beneficiary and agenda-setter (institutional / identity_locked) — authors, transmits, and adjudicates the boundary framework; collects ritual dues and honor
 *   - dominant_landowning_castes: Principal material beneficiary and local enforcer (powerful / identity_locked) — receives hereditary labor and deference; runs caste-council sanction
 *   - village_caste_panchayats: Enforcement arm (organized / identity_locked) — punishes boundary breaches with fines, ostracism, expulsion
 *   - dalit_polluting_occupation_castes: Primary target (powerless / trapped) — hereditarily assigned defiling work; barred from temples, wells, schools
 *   - shudra_laboring_jatis: Secondary target (powerless / trapped) — cultivating and artisan jatis under a ritual ceiling with inherited station
 *   - bhakti_and_ambedkarite_movements: Excluded resisters (organized / mobile) — reject birth-ranked status; organize outside the orthodox establishment
 *   - comparative_religion_scholars: Analytical observer (analytical / analytical) — compares textual prescription against practice records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.8).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.82).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Scriptural Varna Framework: Birth-Fixed Jati Boundaries with Pollution Sanction").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '707872af-6ab5-4727-8125-a82d6af2bc51').
narrative_ontology:cs_kernel_codification('707872af-6ab5-4727-8125-a82d6af2bc51', fixed_text).
narrative_ontology:cs_authority_grounding('707872af-6ab5-4727-8125-a82d6af2bc51', lineage).
narrative_ontology:cs_interpretation_layer_present('707872af-6ab5-4727-8125-a82d6af2bc51').
narrative_ontology:cs_reading_relation('707872af-6ab5-4727-8125-a82d6af2bc51', jati_practice_norm__localized_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('707872af-6ab5-4727-8125-a82d6af2bc51', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('707872af-6ab5-4727-8125-a82d6af2bc51', foundational, varna_status_birth_fixed_by_scripture).
narrative_ontology:cs_axiom_status(varna_status_birth_fixed_by_scripture, holdable).
narrative_ontology:cs_axiom_grounding('707872af-6ab5-4727-8125-a82d6af2bc51', varna_status_birth_fixed_by_scripture, theological).
narrative_ontology:cs_axiom('707872af-6ab5-4727-8125-a82d6af2bc51', foundational, boundary_deviation_is_ritual_pollution).
narrative_ontology:cs_axiom_status(boundary_deviation_is_ritual_pollution, holdable).
narrative_ontology:cs_axiom_grounding('707872af-6ab5-4727-8125-a82d6af2bc51', boundary_deviation_is_ritual_pollution, theological).
narrative_ontology:cs_reference_frame('707872af-6ab5-4727-8125-a82d6af2bc51', varnashrama_dharma_fixed_order).
narrative_ontology:cs_drift_state('707872af-6ab5-4727-8125-a82d6af2bc51', post_constitutional_abolition_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('707872af-6ab5-4727-8125-a82d6af2bc51', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, dominant_landowning_castes).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, dalit_polluting_occupation_castes).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, shudra_laboring_jatis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritative interpreters and transmitters of the scriptural corpus from which varna boundaries are said to derive. Preside over life-cycle rites, rule on purity questions, receive ritual dues and honors, and train successors in an unbroken line of transmission. Their standing depends on the framework's fixity: if boundaries became negotiable, the adjudicating office loses its object. Leaving the framework would mean surrendering the source of their rank, livelihood, and learned identity; they cannot exit without ceasing to be what they are.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_priestly_class, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Upper-tier cultivating and trading jatis that hold the land, wells, and credit in a typical locality. Receive hereditary labor and service from lower jatis under customary obligation, set the terms of village exchange, and organize local enforcement of purity rules through caste councils and social boycott. Their status, marriage alliances, and property arrangements are constituted by the hierarchy; abandoning it would cost them the deference and labor surplus they currently command.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dominant_landowning_castes, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, dominant_landowning_castes, agenda_setter).

% Councils of senior caste members that hear boundary disputes, punish breaches of purity and marriage rules, and impose sanctions ranging from fines to ostracism and expulsion. They operate without written statute, drawing legitimacy from custom and elder authority. Members are drawn from the same status order they police, so relaxing the rules they enforce would undercut their own office.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, village_caste_panchayats, agenda_setter,
    organized, generational, identity_locked, local).

% Communities hereditarily assigned work the purity code marks as defiling — scavenging, leatherwork, corpse handling, funeral drumming. Barred from temples, wells, schools, and upper-caste homes; segregated in hamlets at the village edge; paid in grain allotments rather than wages. Marriage outside the jati invites violence. Leaving the village means losing the subsistence ties that bind them to it, and their occupation follows them wherever their origin is known.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dalit_polluting_occupation_castes, payer,
    powerless, biographical, trapped, local).

% Cultivating and artisan jatis ranked beneath the twice-born: tenant farmers, field laborers, weavers, potters. Barred from Vedic ritual and from rising into the upper tiers regardless of wealth or learning; sons inherit fathers' station. Some accumulate property or organize, but the ritual ceiling and endogamy hold them in place, and defection attempts have historically drawn collective punishment.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, shudra_laboring_jatis, payer,
    powerless, biographical, trapped, regional).

% Devotional and anti-caste currents — saint-poets, sects, and twentieth-century movements led by figures such as Phule, Periyar, and Ambedkar — that reject birth-ranked status and ritual pollution outright. They preach and organize outside the orthodox interpretive establishment, convert, build separate institutions, and carry their case to legislatures and courts. Their voice is structurally absent from the councils where purity rules are authored and applied.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, bhakti_and_ambedkarite_movements, excluded,
    organized, generational, mobile, continental).

% Historians, anthropologists, and indologists who study how jati boundaries are made, maintained, and contested. They compare textual prescriptions against practice records, track how the framework spread and hardened, and publish analyses available to all parties. They hold no stake in the arrangement and can adopt or drop any framing without cost.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, dominant_landowning_castes).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes each community's occupation, marriage pool, ritual duties, and mutual obligations in advance, so that a village economy runs on inherited custom rather than negotiation: food gets cooked, corpses handled, leather worked, fields tilled, and rites performed because each jati's duty is settled. Boundary rules also give every member an unambiguous identity and a known set of counterparts for exchange and marriage.
% TRANSFER_FUNCTION: Moves labor, service, deference, and ritual dues upward — grain allotments, hereditary village-service, unpaid festival labor, and honor flow from lower jatis to landed and priestly households — while moving ritual legitimacy downward, as upper-caste officiants dispense purification, life-cycle rites, and penances to those permitted to receive them.
% ABSENT_VOICES: The people the purity code binds most tightly — the scavenging, leatherworking, and laboring jatis, and women of every jati — had no seat where the rules were composed, commented on, or adjudicated. They speak instead in vernacular devotional songs, sect affiliation, conversion, and courtroom testimony; the textual tradition records them mainly as objects of regulation.
% DISAPPEARANCE_RATIONALE: If the purity-and-birth-rank framework vanished overnight, marriage pools would widen immediately, hereditary service obligations would lapse into wage bargains, temple and well access would open, and the grain-allotment economy would convert to contracted labor; landowners and priests would lose the deference and dues they currently collect, and millions of occupational identities would lose their ascribed content.
% FOUNDING_PROBLEM: Consolidating a large, heterogeneous agrarian population — immigrant lineages, indigenous tribes, occupational specialists — into a single enduring order with a fixed place, duty, and marriage circle for every group, under an interpretive priesthood able to certify purity and legitimacy across generations.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox seats attest the problem is perennial (disorder is dharma's opposite and never finally solved). Outside the beneficiary set, anti-caste intellectuals (Phule, Ambedkar, Periyar) attest the framework was built to secure elite rank and labor control rather than social peace; historians of the period attest that textual fixity was a later rationalization layered over far more fluid practice; constituent-assembly debates record the republic's judgment that the founding problem, as posed, is obsolete. Corroboration for the superseded reading is therefore ample and external; corroboration for liveness comes only from within the tradition.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.80) because the arrangement transfers labor, service, and deference from bounded communities to ranked superiors as a condition of birth, decoupled from any service rendered. Suppression (0.82) reflects structural lock-in on every axis — inherited occupation, policed endogamy, segregated residence, collective punishment for defection — and is deliberately distinguished from enforcement intensity, which the suppression_requirement series shows maturing through medieval codification, peaking in the eighteenth and nineteenth centuries, and declining after constitutional prohibition of enforcement practices; the trap persists even as the machinery that patrols it loses capacity. Theater_ratio (0.42) is moderate: purity observance still does real boundary work, but a growing share of orthodox activity — public ritual display, honor claims unbacked by consensus — maintains appearance where substance has eroded. Accessibility_collapse (0.72) is high but short of natural-law grade: conversion to heterodox traditions (Buddhist, Bhakti, Sikh, Muslim, Christian) has always existed as a costly partial exit, so alternatives narrow severely without vanishing. Resistance (0.60) is substantial and old: devotional egalitarianism, anti-caste movements, temple-entry campaigns, and constitutional politics have contested the framework for centuries. The claimed type is snare on structural grounds independent of these numbers: the coordination story (each varna fulfills its divinely allotted duty) functions as cover for a transfer that runs one way, persistence depends on active enforcement and closed exits, and identifiable victim communities bear the costs. Identity_coordination is declared because the framework genuinely coordinates membership and boundary maintenance — that is why it endured where simple tyranny fails — but the FNL gaming risk is acute here: the identity story is simultaneously the cover for extraction, and the conservative floor ensures the excess extraction surfaces rather than being excused as belonging's price.
 *
 * PERSPECTIVAL GAP:
 *   Seats should diverge sharply. From the brahmin_priestly_class seat the arrangement is a sacred order it is sworn to transmit — the constraint is experienced as vocation, and extraction is nearly invisible behind duty. From dominant_landowning_castes the same structure is advantageous custom: cheap labor, guaranteed deference, stable marriage alliances. From the payer seats — dalit and shudra jatis — the identical rules are a sealed room: inherited defilement, barred wells and temples, no purchasable exit. The village_caste_panchayats experience enforcement as obligation management among kin. The engine computes these per-seat classifications from power, horizon, and exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly onto directionalities. brahmin_priestly_class and dominant_landowning_castes sit at the beneficiary pole (low d): the framework subsidizes them with labor, dues, and honor, and their identity_lock raises the cost of reform to themselves. dalit_polluting_occupation_castes and shudra_laboring_jatis sit at the target pole (high d): they pay labor, service, and exclusion with trapped exit, so effective extraction approaches the full unscaled epsilon. village_caste_panchayats are enforcers drawn from the beneficiary stratum — structurally beneficiaries who administer rather than merely collect. bhakti_and_ambedkarite_movements are excluded rather than coordinated: the boundary system's enforcement exists partly to keep their alternative out. No directionality overrides were needed: beneficiary/victim declarations plus exit options already place every seat correctly. The one nuance the derivation smooths over — that upper castes are also bound by purity rules they enforce — trims but does not reverse their net-beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthodox presentation naturalizes the arrangement — dharma as eternal, station as earned, boundaries as cosmic fact — which is precisely the move the classification refuses: a construct maintained by councils, boycotts, and expiations is not a mountain, and authoring it as snare keeps the naturalization from laundering extraction into nature. The inverse protection matters equally: the framework does solve real boundary problems for insiders (unambiguous identity, settled exchange counterparts, marriage-circle clarity), which is why it endured; reading it as pure coercion would miss the mechanism. On obsolescence: the founding integrative problem has been superseded by states, markets, and a constitution that forbids the arrangement's central practice, while the arrangement persists — the mandate has outlived its function, hence mandatrophy_resolved is declared, with the residual question routed to the abolition_vs_substantive_persistence omega rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the orthodox_textual_reading of the jati_practice_norm kernel: jati boundaries derive from a fixed scriptural varna framework and deviation is ritual pollution. Would instantiating a sibling reading — boundaries as continuously renegotiated local practice norms, or as categories stabilized by an external administrative census — yield a different beneficiary structure, enforcement profile, and epsilon over the same social terrain?',
    'Generate the sibling stories and compare computed classifications; locate the disagreement in the source-of-legitimacy element (divine textual ordinance vs local negotiation vs administrative stabilization), which is the specific structural element on which the readings differ.',
    'If the localized reading dominates the terrain, this reading''s high epsilon attributes to textual authority an extraction that local bargaining actually produces; if the census reading dominates, the victim set and enforcement dates shift to the administrative period. This story''s classification stands or falls with the textual-fixity premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings would restructure beneficiaries, victims, and epsilon.').

omega_variable(
    textual_fixity_vs_practice_gap,
    'Do jati boundaries in fact derive from the scriptural varna framework as the orthodox texts claim, or did a far more fluid practice reality acquire a retrospective textual rationalization?',
    'Compare epigraphic, inscriptional, and court records on boundary disputes and recorded mobility against prescriptive passages; test whether prescriptive texts track practice or lag behind and idealize it.',
    'If practice precedes and exceeds the texts, authorship of the constraint shifts from divine ordinance to elite rationalization, raising the extraction attributable to the interpretive class and weakening the fixity claim; if the texts genuinely governed, the arrangement is closer to a consciously designed order than an emergent one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fixity_vs_practice_gap, empirical, 'Whether the scriptural framework is the generator of boundaries or their later rationalization.').

omega_variable(
    internalized_purity_suppression,
    'Is the suppression holding the boundary system in place structural (boycott, violence, economic dependency, residential segregation) or internalized (purity anxiety and status aspiration carried by lower-caste members themselves)?',
    'Post-exit trajectory: track converts and migrants — if purity and prejudice norms persist after structural barriers are removed (as documented among caste-converted Christian and Muslim communities and in diaspora matchmaking), the suppression is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint with them after exit, and removing external enforcement would not dissolve the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalized_purity_suppression, empirical, 'Structural vs internalized suppression mechanism in the purity regime.').

omega_variable(
    karma_theodicy_consent_status,
    'Do lower-jati participants accept birth-ranked station as deserved (karma-rebirth theodicy), stabilizing the arrangement as quasi-consent, or is expressed acceptance compliance under coercion?',
    'Compare stated theodicy beliefs against behavior when enforcement weakens (urban migration, legal protection): rapid defection from prescribed occupations and marriage rules indicates coerced assent; persistence of theodicy belief under freedom indicates genuine stabilization.',
    'If theodicy consent is genuine, measured suppression understates the arrangement''s stability and extraction persists without visible enforcement; if coerced, the arrangement is purely enforced extraction and its trajectory should track enforcement-capacity decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(karma_theodicy_consent_status, empirical, 'Whether theodicy belief stabilizes the arrangement as consent or masks coercion.').

omega_variable(
    abolition_vs_substantive_persistence,
    'Does the constitutional abolition of untouchability and the reservation architecture represent genuine erosion of the constraint, or displacement of its operation into forms law does not reach (manual scavenging, matrimonial endogamy, residential segregation, caste atrocity)?',
    'Longitudinal data on intercaste marriage rates, manual-scavenging employment, segregation indices, and prosecution of caste atrocities, plotted against the formal-legal timeline.',
    'If substantive persistence dominates, the end-of-interval extractiveness decline is cosmetic and the arrangement should be modeled as re-entrenching; if erosion is real, the terminal trajectory bends toward transitional dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolition_vs_substantive_persistence, empirical, 'Whether modern legal abolition erodes the constraint substantively or cosmetically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 200, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t200, jati_practice_norm__orthodox_textual_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(jati_tr_t600, jati_practice_norm__orthodox_textual_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement(jati_tr_t1000, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(jati_tr_t1400, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1400, 0.26).
narrative_ontology:measurement(jati_tr_t1700, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1700, 0.3).
narrative_ontology:measurement(jati_tr_t1850, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1850, 0.33).
narrative_ontology:measurement(jati_tr_t1950, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(jati_tr_t2000, jati_practice_norm__orthodox_textual_reading, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(jati_be_t200, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 200, 0.7).
narrative_ontology:measurement(jati_be_t600, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 600, 0.74).
narrative_ontology:measurement(jati_be_t1000, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1000, 0.79).
narrative_ontology:measurement(jati_be_t1400, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1400, 0.83).
narrative_ontology:measurement(jati_be_t1700, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1700, 0.86).
narrative_ontology:measurement(jati_be_t1850, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1850, 0.87).
narrative_ontology:measurement(jati_be_t1950, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1950, 0.83).
narrative_ontology:measurement(jati_be_t2000, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 2000, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t200, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 200, 0.5).
narrative_ontology:measurement(jati_su_t600, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 600, 0.58).
narrative_ontology:measurement(jati_su_t1000, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1000, 0.68).
narrative_ontology:measurement(jati_su_t1400, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1400, 0.74).
narrative_ontology:measurement(jati_su_t1700, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1700, 0.78).
narrative_ontology:measurement(jati_su_t1850, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(jati_su_t1950, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(jati_su_t2000, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 2000, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, colonial_census_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'the caste system' into three structurally distinct constraints per the epsilon-invariance principle: the orthodox textual-normative claim (this file — high epsilon, theological enforcement, ancient victim set), the localized practice regime (negotiated boundaries, lower epsilon, distributed enforcement), and the colonial census reification (administrative stabilization, state-era victim set). Edges run upstream to downstream: the textual framework supplied the classificatory template the census hardened; local practice both feeds and evades the textual norm. Each story carries its own beneficiaries, victims, and claimed type; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
