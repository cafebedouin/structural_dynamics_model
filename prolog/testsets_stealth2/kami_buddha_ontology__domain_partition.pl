% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Functional Domain Partition (Life/Death Division)
 *   domain: religious_studies/japanese_cultural_history
 *
 * SUMMARY:
 *   This story instantiates the domain_partition reading of the
 *   kami_buddha_ontology kernel: kami and buddhas as ontologically distinct
 *   entities governing non-overlapping functional domains — Shinto holding
 *   life, purity, and the living; Buddhism holding death, impurity, and the
 *   deceased — coordinated practically without theoretical unity and without
 *   hierarchy between them. The standing arrangement under contest is the
 *   enforced partition as it has operated from the 1868 separation edicts to
 *   the present: a division that was legislated against a prior norm of fused
 *   practice, that redistributed property and personnel by state force, and
 *   that now persists as a dual-affiliation burden on households and a pair
 *   of protected clerical revenue niches. KEY AGENTS (by structural
 *   relationship): jinja_honcho_shrine_establishment — co-agenda-setter and
 *   beneficiary (institutional/arbitrage), administers the life-domain side
 *   and collects offerings and definitional authority; funerary_temple_clergy
 *   — beneficiary with residual payer exposure (organized/constrained), holds
 *   the death-domain practice and bears legacy losses from the separations;
 *   meiji_state_ritual_bureaucracy — founding agenda-setter
 *   (institutional/mobile), built the partition and exited by dissolution in
 *   1945; parish_households — primary payer (powerless/constrained), bears
 *   both fee streams and obligatory dual participation; syncretic_devotees —
 *   payer with identity-locked exit (powerless/identity_locked), practice
 *   rendered illegible by the division; suppressed_shugendo_lineages —
 *   historical payer (organized/trapped), tradition outlawed wholesale
 *   1872-1946; funeral_industry_operators — excluded competitor
 *   (powerful/mobile), gated by custom and parish ties; religion_scholars —
 *   analytical observer (analytical/analytical). Family note: the colloquial
 *   label 'shinbutsu-shugo' decomposes into three structurally distinct
 *   claims with different epsilon values and victim sets — this file authors
 *   epsilon only for the enforced partition arrangement; the monism sibling
 *   authors epsilon for the medieval hierarchical subordination arrangement,
 *   and the bundle sibling authors epsilon for the oscillating institutional
 *   bundle. The claim/metric gap is deliberate: the partition is CLAIMED here
 *   as tangled_rope (genuine ritual-labor coordination plus asymmetric
 *   extraction under enforcement) while the metrics are authored
 *   independently from its observed operation; the engine measures any
 *   divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.52).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.35).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.52).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Functional Domain Partition (Life/Death Division)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e').
narrative_ontology:cs_kernel_codification('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', formalized).
narrative_ontology:cs_authority_grounding('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', extraction).
narrative_ontology:cs_interpretation_layer_present('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e').
narrative_ontology:cs_reading_relation('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', kami_buddha_ontology__incoherent_bundle, forecloses).
narrative_ontology:cs_axiom('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', foundational, kami_buddha_natures_distinct).
narrative_ontology:cs_axiom_status(kami_buddha_natures_distinct, holdable).
narrative_ontology:cs_axiom_grounding('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', kami_buddha_natures_distinct, theological).
narrative_ontology:cs_axiom('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', secondary, ritual_jurisdiction_follows_ontology).
narrative_ontology:cs_axiom_status(ritual_jurisdiction_follows_ontology, holdable).
narrative_ontology:cs_axiom_grounding('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', ritual_jurisdiction_follows_ontology, conventional).
narrative_ontology:cs_reference_frame('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', parallel_ontological_complementarity).
narrative_ontology:cs_drift_state('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', contemporary_postsecular_japan, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17e19e87-6ca8-4a0e-91ff-ae0ed35cc04e', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, jinja_honcho_shrine_establishment).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, funerary_temple_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, meiji_state_ritual_bureaucracy).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, parish_households).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, syncretic_devotees).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, suppressed_shugendo_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, funerary_temple_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the nationwide network of shrines, trains and licenses priests, and defines Shinto publicly as the life-affirming indigenous tradition of Japan. Sets the purity norms that mark the life-domain side of the division, polices shrine practice against Buddhist elements, and collects offerings, licensing fees, and definitional authority. Because it writes the terms of the arrangement, it faces no binding cost from it.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, jinja_honcho_shrine_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Provides nearly all funerals and ancestral memorial services through hereditary parish (danka) ties, and collects the associated fees — the largest reliable revenue stream in Japanese institutional religion. Lost shrine precincts, shrine-temple composite institutions, and state patronage in the separations of 1868 and 1945, and carries those losses in its institutional memory. Temples are place-bound and their congregations are constituted by ancestral registration, so leaving the arrangement would mean abandoning the ties that make a temple a temple.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, funerary_temple_clergy, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, funerary_temple_clergy, payer).

% Issued the 1868 separation edicts, dissolved the composite shrine-temples, banned the mountain ascetic orders in 1872, and reassigned personnel and property, all to build an emperor-centered state ritual order purified of Buddhist presence. Collected administrative control and ritual legitimacy from the arrangement it built. Ceased to exist as an apparatus in 1945 — exiting the arrangement entirely by dissolution, the freest exit available to any party in this story.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, meiji_state_ritual_bureaucracy, agenda_setter,
    institutional, civilizational, mobile, national).

% Maintain dual affiliation: registered danka ties to a temple for funerals and ancestral rites, and participatory ties to a shrine for festivals, New Year observance, and life-cycle blessings. Pay both fee streams and must route each life event to the correct register — weddings and births to the shrine side, deaths to the temple side. Leaving the danka roll risks losing funeral provision and friction with kin; individual exit is costly, and no organized vehicle for collective exit currently exists.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, parish_households, payer,
    powerless, biographical, constrained, national).

% Practitioners whose inherited devotion spans both registers — household altars pairing the kami shelf and the Buddha shelf, veneration of kami such as Hachiman and Tenjin in Buddhist idiom. The enforced division renders their practice illegible, requiring every act to be assigned to one column or the other. Their devotional identity was formed in the fused idiom, so complying means dividing a practice that constitutes them, and exiting means abandoning it.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, syncretic_devotees, payer,
    powerless, generational, identity_locked, regional).

% Mountain ascetic orders fusing kami worship with Buddhist esotericism, centered on specific sacred ranges. Outlawed outright from 1872 to 1946: leaders laicized, temples destroyed, transmission lines severed. The practice is tied to particular mountains and to lineages of transmission that cannot be relocated; postwar revivals operate under reconstructed legitimacy and diminished continuity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, suppressed_shugendo_lineages, payer,
    organized, generational, trapped, regional).

% Commercial funeral firms that increasingly handle death care outside temple auspices. Customary routing of funerals through temples and the danka tie structure limit their addressable market; they market around the division and their growing share of funerals is the clearest contemporary signal that the arrangement's death-side gate is eroding.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, funeral_industry_operators, excluded,
    powerful, immediate, mobile, national).

% Historians and folklorists of Japanese religion who document that the life/death division was legislated in 1868 against a prior norm of fused practice, and trace its subsequent career through state ritual policy, occupation-era disestablishment, and postwar secularization. Hold no stake in either establishment and publish the record both establishments prefer not to foreground.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, funerary_temple_clergy).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ritual jurisdiction along the life/death axis so that two clerical professions serve non-overlapping needs without jurisdictional war, and so that households have a legible map of which provider handles which rite.
% TRANSFER_FUNCTION: Moves money (danka fees, funeral and memorial payments, shrine offerings) and obligatory participation from households to both establishments; moved property, buildings, and personnel during the 1868-1872 dissolutions; moves definitional legitimacy — exclusive authority over purity and the living to the shrine side, over death and the deceased to the temple side.
% ABSENT_VOICES: Syncretic devotees and the mountain ascetic lineages — the people whose integrated practice the division criminalized — had no seat when the edicts were drafted; ordinary households were addressed as objects of administration rather than participants. Their objections survive only as enforcement records, destroyed-institution registries, and postwar scholarship.
% DISAPPEARANCE_RATIONALE: If the division vanished overnight, funeral provision would reorganize around commercial operators and openly competing temples, household practice would re-fuse (the paired kami shelf and Buddha shelf already cohabit in most homes), shrine and temple jurisdictions would blur back toward the pre-1868 composite pattern, and two clerical professions would lose protected revenue niches.
% FOUNDING_PROBLEM: Constructing an emperor-centered state ritual order purified of Buddhist presence: separating kami worship from temple Buddhism so shrines could anchor state ritual, and settling centuries of temple-shrine jurisdictional entanglement in the same stroke.
% FOUNDING_PROBLEM_CORROBORATION: Academic historiography of the separation edicts and the anti-Buddhist destruction that followed them, together with the Occupation-era disestablishment record, attests from outside both benefiting establishments that the founding problem was the Meiji state's ritual purification program — a project abolished in 1945. The shrine establishment's own public account (a timeless native tradition requiring no founder) declines to corroborate any founding problem at all, which is itself signal.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: real but bounded — dual fee streams, forced-choice costs imposed on fused practitioners, and the historical expropriation phase now amortized; the recent slight uptick reflects funeral-price inflation offsetting secularizing defection. Suppression 0.35: postwar enforcement is professional gatekeeping and social expectation, far below the state-violence regime of 1868-1945; the 1945 series point shows the wartime re-intensification before the collapse. Theater_ratio 0.45: a large and growing share of partition-maintenance is customary performance (New Year observance as civic routine, nominal religious identity) while the core death-care function remains materially real — hence moderate, not high. Accessibility_collapse 0.40: alternatives stay visible and usable (commercial funerals, chapel weddings outside both domains, new religions integrating freely) but carry social price. Resistance 0.50: historical institutional resistance was crushed; contemporary resistance takes the form of consumer defection and scholarship rather than confrontation. All three series run on one shared seven-point grid (1868-2025) so every metric is authored at every examined time point; suppression_requirement is tracked because enforcement capacity is a central dynamic of this story (state violence to professional gatekeeping), not merely background.
 *
 * PERSPECTIVAL GAP:
 *   From the shrine-establishment seat the division presents as restored native order — coordination it administers and defines. From the parish-household seat the same structure presents as a double bill for services never chosen. From the temple-clergy seat it computes as both livelihood and loss: the death-side niche is the compensation for what the separations took. From the scholar seat the whole appears as a nineteenth-century construction retrojected onto antiquity. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three declared beneficiaries derive low directionality (subsidy side): the shrine establishment controls the terms it lives under, the temple clergy collects the largest fee stream, and the Meiji bureaucracy built the arrangement for its own ritual program. The three declared payers derive high directionality, amplified by their exits: households are constrained (funeral necessity binds), syncretic devotees are identity_locked (the fused idiom constitutes them), and the ascetic lineages were trapped (mountain- and lineage-bound practice with nowhere to relocate). One caution the structural data supports: households are individually powerless but collectively numerous — a consumer coalition or secular funeral association could convert diffuse numbers into bargaining power, which caps how far their effective extraction can ratchet. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate every seat, including the dual-positioned temple clergy, whose mixed position is left to the engine rather than overridden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — constructing an emperor-centered state ritual order purified of Buddhist presence — died with the 1945 disestablishment, while the arrangement persists serving clerical revenue niches and customary habit; mandatrophy_resolved is therefore declared true. The classification discipline matters in both directions here: reading the partition as pure extraction ignores that its ritual-labor coordination is real and used daily by millions of households; reading it as pure coordination ignores that it was born in expropriation, still taxes households doubly, and survives primarily because dismantling it would bankrupt its administrators. The lifecycle question (functioning hybrid versus decaying residue) is routed to an omega variable rather than asserted, because the temporal series alone cannot settle whether the arrangement is stabilizing as heritage-performance or still enforcing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (domain_partition) of the kami_buddha_ontology kernel; would instantiating a sibling reading change the structural classification?',
    'Generate the sibling files (honji_suijaku_monism, incoherent_bundle) and compare computed types across the family. The disagreement is located in the ontological-relation premise — distinctness versus identity versus incoherence — and in whether hierarchy exists between the two cults.',
    'Under the monism reading the victim set shifts (kami-worship autonomy becomes the casualty of subordination) and epsilon''s referent changes to the medieval hierarchical arrangement; under the bundle reading no stable victim set exists because commitments oscillate, and classification instability is itself the finding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of three rival readings of a single kernel; sibling instantiations are separate constraints.').

omega_variable(
    naturalness_vs_state_manufacture,
    'Is the life/death functional division a spontaneous folk taxonomy (kami as gods of the living) or a Meiji state manufacture retrojected as timeless?',
    'Pre-1868 documentary record: composite shrine-temples, kami venerated in Buddhist idiom, Buddhist death rites performed at shrines. If fused practice was the norm before the edicts, the division is manufactured; the establishment''s naturality claim then rests on retrojection.',
    'A manufactured origin supports constructed-constraint classification in the tangled_rope-to-snare range and undermines the naturality framing; a genuinely spontaneous taxonomy would push the arrangement toward emergent-norm (rope) classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_state_manufacture, empirical, 'Whether the partition''s apparent naturality is real or a nineteenth-century retrojection.').

omega_variable(
    partition_decay_trajectory,
    'Is the standing arrangement still a functioning enforced partition, or decaying into customary residue maintained theatrically?',
    'Track temple share of the funeral market against commercial operators, danka-roll attrition rates, and shrine wedding demand; sustained decline accompanied by rising theater_ratio indicates inertial maintenance of an atrophying function.',
    'Continued erosion would move the arrangement toward inertial classification (function atrophied, persistence by habit and administration); stabilization or political re-enforcement of Shinto nationalism would restore active hybrid dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_decay_trajectory, empirical, 'Lifecycle drift question: functioning hybrid now, but the temporal series leaves the terminal state open.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the remaining suppression of cross-domain practice structural (fee schedules, parish obligation, professional gatekeeping) or internalized (the conviction that mixing registers is improper)?',
    'Post-exit trajectory: households that formally leave the parish roll or hold secular funerals — if impropriety-anxiety persists after the external obligation ends, part of the suppression is carried internally.',
    'An internalized component raises effective suppression above the structural measure and slows decay regardless of enforcement changes; purely structural suppression would fall quickly if the fee and registration machinery were deregulated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism ambiguity in the residual suppression measure: external barriers versus carried conviction.').

omega_variable(
    authority_grounding_framing,
    'Should the partition''s authority structure be framed as lineage (the establishments'' professed warrant: unbroken native tradition) or extraction (the operative warrant: both professions profit from preventing reunification)?',
    'Compare classifications under both framings. Signals favoring the extraction framing: revenue dependence on domain exclusivity, coordinated resistance to funeral-market liberalization, and persistence after 1945 removed the state that validated the lineage claim.',
    'The lineage framing yields a traditional-authority pattern with drift absorbed by interpretive bodies; the extraction framing yields a capture pattern in which drift denial is the source of authority — producing different downstream contamination predictions for neighboring religious-governance constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: professed versus operative warrant for the arrangement''s authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 1868, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_buddha_partition_tr_t1868, kami_buddha_ontology__domain_partition, theater_ratio, 1868, 0.14).
narrative_ontology:measurement(kami_buddha_partition_tr_t1890, kami_buddha_ontology__domain_partition, theater_ratio, 1890, 0.19).
narrative_ontology:measurement(kami_buddha_partition_tr_t1912, kami_buddha_ontology__domain_partition, theater_ratio, 1912, 0.24).
narrative_ontology:measurement(kami_buddha_partition_tr_t1945, kami_buddha_ontology__domain_partition, theater_ratio, 1945, 0.31).
narrative_ontology:measurement(kami_buddha_partition_tr_t1970, kami_buddha_ontology__domain_partition, theater_ratio, 1970, 0.36).
narrative_ontology:measurement(kami_buddha_partition_tr_t2000, kami_buddha_ontology__domain_partition, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(kami_buddha_partition_tr_t2025, kami_buddha_ontology__domain_partition, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(kami_buddha_partition_be_t1868, kami_buddha_ontology__domain_partition, base_extractiveness, 1868, 0.74).
narrative_ontology:measurement(kami_buddha_partition_be_t1890, kami_buddha_ontology__domain_partition, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(kami_buddha_partition_be_t1912, kami_buddha_ontology__domain_partition, base_extractiveness, 1912, 0.64).
narrative_ontology:measurement(kami_buddha_partition_be_t1945, kami_buddha_ontology__domain_partition, base_extractiveness, 1945, 0.61).
narrative_ontology:measurement(kami_buddha_partition_be_t1970, kami_buddha_ontology__domain_partition, base_extractiveness, 1970, 0.54).
narrative_ontology:measurement(kami_buddha_partition_be_t2000, kami_buddha_ontology__domain_partition, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(kami_buddha_partition_be_t2025, kami_buddha_ontology__domain_partition, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(kami_buddha_partition_su_t1868, kami_buddha_ontology__domain_partition, suppression_requirement, 1868, 0.86).
narrative_ontology:measurement(kami_buddha_partition_su_t1890, kami_buddha_ontology__domain_partition, suppression_requirement, 1890, 0.77).
narrative_ontology:measurement(kami_buddha_partition_su_t1912, kami_buddha_ontology__domain_partition, suppression_requirement, 1912, 0.67).
narrative_ontology:measurement(kami_buddha_partition_su_t1945, kami_buddha_ontology__domain_partition, suppression_requirement, 1945, 0.74).
narrative_ontology:measurement(kami_buddha_partition_su_t1970, kami_buddha_ontology__domain_partition, suppression_requirement, 1970, 0.47).
narrative_ontology:measurement(kami_buddha_partition_su_t2000, kami_buddha_ontology__domain_partition, suppression_requirement, 2000, 0.39).
narrative_ontology:measurement(kami_buddha_partition_su_t2025, kami_buddha_ontology__domain_partition, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, resource_allocation).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'shinbutsu-shugo' conflates three structurally distinct claims — enforced functional partition (this file), hierarchical identity of kami as buddha-traces (honji_suijaku_monism), and institutionally sustained incoherence (incoherent_bundle). Their epsilon values differ widely: the partition arrangement shows moderate bounded extraction; the medieval monism arrangement concentrated extraction on kami-worship autonomy; the bundle arrangement's extraction oscillates with whichever commitment is locally enforced. Each story carries its own beneficiaries, victims, and claimed type; the family links run through network.affects_constraints in all three files. Upstream/downstream: the monism reading historically preceded and was displaced by the partition reading, whose enforcement destroyed the monism arrangement's institutional substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
