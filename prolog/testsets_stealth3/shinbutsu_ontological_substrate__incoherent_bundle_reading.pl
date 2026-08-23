% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Fusion Order - Incoherent Bundle Reading (State-Enforced Institutional Drift)
 *   domain: religious/history/political
 *
 * SUMMARY:
 *   From the eighth-century edicts ordering shrines to host sutra recitation
 *   through the 1868 separation edicts, the Japanese state bound kami cults
 *   and Buddhist institutions into a single administered religious order
 *   (shinbutsu shugo), layered through honji suijaku identifications,
 *   temple-shrine estate law, priestly rank exchange, and finally the
 *   Tokugawa parish-registration (terauke/danka) system. This file authors
 *   ONE reading of the kernel shinbutsu_ontological_substrate: the
 *   incoherent_bundle_reading, which holds that no coherent ontological
 *   kernel exists beneath the arrangement - it is accumulated institutional
 *   drift, held together by state enforcement, not a unified commitment. The
 *   epsilon referent is the standing enforced-fusion arrangement itself,
 *   assessed by this reading's own lights: practitioners bore contradictory
 *   obligations without resolution while the governing state collected
 *   administrative control and legitimation, which is why epsilon is high.
 *   The claim/metric pair is authored independently: claimed_type states this
 *   reading's structural verdict (snare - the harmony-and-protection framing
 *   is cover for enforced extraction); the metrics describe the arrangement's
 *   actual operation as the documentary record supports it. Sibling readings
 *   (syncretic_fusion_reading, domain_partition_reading) are separate
 *   constraint files with their own epsilon and are linked, not averaged,
 *   here.
 *
 * KEY AGENTS:
 *   - warrior_government: agenda-setter and principal beneficiary (institutional/arbitrage) - administers enforcement and collects control and administrative legibility
 *   - imperial_court_ritual_authorities: originating agenda-setter, continuing beneficiary (institutional/arbitrage) - authored the legitimating ideology of fused realm protection
 *   - great_monastic_complexes: organized beneficiary (organized/constrained) - collects revenue, estates, and rank through the fused order
 *   - hereditary_shrine_priest_lineages: lineage-bound beneficiary (moderate/identity_locked) - office and family identity fused to the arrangement
 *   - danka_households: primary target (powerless/trapped) - bears compulsory registration, recurring fees, and doctrinal assent
 *   - subordinate_kami_priests: secondary target (moderate/constrained) - bears hierarchical subordination and upward remittances
 *   - heterodox_doctrine_advocates: secondary target (moderate/constrained) - bears censorship and censure for contradicting the fused order
 *   - unlicensed_ascetic_preachers: excluded voice (powerless/trapped) - barred from recognized practice, no seat in adjudication
 *   - historians_of_japanese_religion: analytical observer (analytical/analytical) - sees the full structure from court diaries, monastic archives, and village documents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.72).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.74).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Fusion Order - Incoherent Bundle Reading (State-Enforced Institutional Drift)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/history/political").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '3e31b491-d82b-4ee1-8baa-4376c793c02c').
narrative_ontology:cs_kernel_codification('3e31b491-d82b-4ee1-8baa-4376c793c02c', distributed).
narrative_ontology:cs_authority_grounding('3e31b491-d82b-4ee1-8baa-4376c793c02c', extraction).
narrative_ontology:cs_interpretation_layer_present('3e31b491-d82b-4ee1-8baa-4376c793c02c').
narrative_ontology:cs_reading_relation('3e31b491-d82b-4ee1-8baa-4376c793c02c', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('3e31b491-d82b-4ee1-8baa-4376c793c02c', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('3e31b491-d82b-4ee1-8baa-4376c793c02c', foundational, no_unifying_ontological_substrate).
narrative_ontology:cs_axiom_status(no_unifying_ontological_substrate, holdable).
narrative_ontology:cs_axiom_grounding('3e31b491-d82b-4ee1-8baa-4376c793c02c', no_unifying_ontological_substrate, empirically_contingent).
narrative_ontology:cs_axiom('3e31b491-d82b-4ee1-8baa-4376c793c02c', foundational, fused_order_depends_on_state_enforcement).
narrative_ontology:cs_axiom_status(fused_order_depends_on_state_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('3e31b491-d82b-4ee1-8baa-4376c793c02c', fused_order_depends_on_state_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('3e31b491-d82b-4ee1-8baa-4376c793c02c', autonomous_cult_institutional_baseline).
narrative_ontology:cs_drift_state('3e31b491-d82b-4ee1-8baa-4376c793c02c', late_tokugawa_mature_arrangement, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3e31b491-d82b-4ee1-8baa-4376c793c02c', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_court_ritual_authorities).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, warrior_government).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, great_monastic_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, hereditary_shrine_priest_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, danka_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, subordinate_kami_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, heterodox_doctrine_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, danka_households).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, subordinate_kami_priests).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__incoherent_bundle_reading, religion_as_state_administrative_instrument).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regulated the temple-shrine complex from its Kamakura headquarters onward: codes governing temple conduct, confirmation of abbots and estate holdings, and in the mature Edo phase the annual temple-certificate loop in which every household's registration fed a nationwide surveillance network. Converted the fused order into an instrument of social control and drew administrative legibility and ideological stability from it. Retained the capacity to redirect or withdraw enforcement at will, as its successor regime demonstrated in 1868.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, warrior_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, warrior_government, beneficiary).

% Issued the early edicts binding shrine cults to Buddhist rites and temples to kami veneration, beginning with orders that provincial shrines recite sutras for the realm's protection. Drew ideological legitimacy from the fused cult order and controlled appointments, ranks, and endowments flowing through it. Having authored the legitimating ideology, the court could always reframe the arrangement rather than exit it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_court_ritual_authorities, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_court_ritual_authorities, agenda_setter).

% Head temples held fused shrine-temple estates, tax exemptions, and armed retainers; collected funerary, memorial, and festival revenues from registered households; and ranked subordinate shrines within their hierarchies. Their privileges were constituted by the fused order itself, so defending it defended their position; leaving it meant surrendering estates, rank, and clientele.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, great_monastic_complexes, beneficiary,
    organized, biographical, constrained, regional).

% Hereditary custodial houses at major shrines held office by birth, took Buddhist ordination names and precepts as the price of rank, and received stipends routed through temple-shrine hierarchies. The office passed father to son; exiting the arrangement would have meant dissolving the lineage's office and with it the family's identity and livelihood.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, hereditary_shrine_priest_lineages, beneficiary,
    moderate, generational, identity_locked, regional).

% Registered compulsorily with a designated parish temple: funerals and memorial rites were obligatory, temple fees and festival levies recurred annually, and kami festivals were attended alongside Buddhist obligations. Received integrated ritual coverage across life events in return. Registration followed the household, and leaving the village meant leaving registration behind - practically unavailable.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, danka_households, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, danka_households, beneficiary).

% Village and provincial shrine priests held office below the temple hierarchies, were required to carry Buddhist ordination titles to serve legitimately, and remitted shares of offerings upward. Kept local ritual authority and a living from the shrine; resigning meant losing the office that defined both.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, subordinate_kami_priests, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, subordinate_kami_priests, beneficiary).

% Teachers whose doctrine contradicted the fused order - exclusivist Pure Land readings denying kami salvific power, Nichiren's polemics against kami worship, later nativist scholars - taught under licensing, censorship, and the threat of censure or exile. Their practical choice was qualified assent or clandestine speech; open exit meant silence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, heterodox_doctrine_advocates, payer,
    moderate, biographical, constrained, national).

% Wandering holy men, mountain ascetics, and healers operating outside licensed temple-shrine posts offered competing rites and teachings. Licensing and posting rules barred them from recognized practice, and they held no seat in the councils that set the terms they were excluded by.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, unlicensed_ascetic_preachers, excluded,
    powerless, biographical, trapped, regional).

% Contemporary scholars working from court diaries, monastic archives, shrine records, village documents, and domain papers. They assess whether the fused order reflected unified doctrine or accumulated enforcement, and publish findings without any stake in the arrangement itself.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, warrior_government).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement standardized relations between shrine cults and Buddhist institutions - ritual calendars, landholding, priestly ranks, festival financing - giving villages a single legible religious order and giving rulers one channel for administering religion across the archipelago. Stated without evaluation: whatever unified commitment existed or did not, this functional standardization is what the arrangement operationally delivered.
% TRANSFER_FUNCTION: Moved recurring fees, funerary-registration compliance, festival labor and financing, and doctrinal assent from commoner households and subordinate clergy to monastic complexes and the governing state; moved legitimation and administrative legibility back to the state, and office income and rank down to participating clergy.
% ABSENT_VOICES: Unlicensed ascetic preachers, household ritual specialists outside the licensed orders, and doctrinal dissidents whose theologies contradicted fusion were kept out of the adjudicating councils entirely. Dissent surfaced only episodically (uprisings, polemics, petitions) or after enforcement shifted sides in 1868 - unanimity about the arrangement's legitimacy arose in rooms the dissenting seats were barred from.
% DISAPPEARANCE_RATIONALE: When enforcement reversed in 1868, the fused order unraveled within years: shrines stripped temples of assets, tens of thousands of temples closed or merged, clergy were laicized en masse, and household practice reorganized along the newly drawn shrine/Buddhist line. Arrangements held by voluntary commitment do not disintegrate this fast; the parties' positions were constituted by the enforced order, and its removal rearranged them all.
% FOUNDING_PROBLEM: Integrate indigenous kami cults with imported Buddhist institutions so that both served state aims - realm protection, ritual precedence, administrative legibility - and neither undermined the other. The earliest instruments were edicts ordering shrines to host sutra recitation and temples to venerate kami protectors.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties by domain-level administrative records, village headman petitions complaining of compulsory registration and fees, post-Restoration council debates explicitly characterizing the old order as coercively maintained, and the modern documentary historiography built on shrine and monastic archives. The state's own continuity narratives attest the founding problem's liveness; no source outside the beneficiary set corroborates that the arrangement still served integration rather than control by its final century - that attestation does not exist, which is itself signal.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.72 characterizes the mature enforced phase (1650-1830), where the series peaks: compulsory parish registration, recurring temple fees, mandatory funerary rites, and doctrinal assent were extracted from households with no exit, while subordinate clergy remitted upward for the privilege of office. Suppression 0.74 is authored as a raw structural property - the arrangement's persistence demonstrably depended on edict, licensing, and the terauke surveillance loop, and suppression is NOT scaled by power or scope (only extractiveness is scaled, by the engine, from directionality and scope). Theater ratio 0.39: the ritual services were real and heavily used, but a growing share of activity - scholastic rationalization of ever-new kami-buddha identifications, licensing ceremonies, registration ritual - defended the appearance of coherence rather than delivering function, which is exactly what this reading predicts for an arrangement with no kernel. Accessibility collapse 0.55: alternatives never fully closed - exclusive schools (Pure Land, Nichiren, Zen) survived alongside, folk practice varied widely, and kokugaku eventually emerged - but each alternative operated under license and at doctrinal discount. Resistance 0.50: episodic and real (ikki uprisings, doctrinal polemic, village fee disputes, post-Restoration clerical resistance) but never sufficient to unwind the arrangement while enforcement held. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: machinery built up steadily (ritsuryo edicts, estate law, bakufu codes), hardened dramatically under Tokugawa jiin hatto and the danka system, then collapsed almost to zero in 1868 when the successor regime reversed enforcement. All three tracked metrics are authored on one shared ten-point time grid spanning 741-1868, so no metric row is ever substituted from the story-level scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the danka_households seat, the arrangement presents as compulsory payment and unresolved doctrinal contradiction with no exit - the full-target experience. From the great_monastic_complexes seat, the same structure is earned order: estates, rank, and clientele constituted by the fusion, experienced as legitimate coordination they staff and maintain. From the warrior_government seat, it is an administrative instrument - legibility, surveillance, legitimation - and its arbitrage-grade exit means the state experiences near-zero extraction while imposing it. Same-level divergence: subordinate_kami_priests and heterodox_doctrine_advocates both hold moderate power, but the priests hold office income and local ritual authority inside the arrangement (exit costs the office), while the advocates hold only a doctrine (exit costs the voice) - same nominal standing, structurally different exposure. Inter-institutionally, court and bakufu are successive agenda-setters with the same beneficiary position but different enforcement technologies; the monastic complexes are unusual beneficiaries who possessed coercive capacity of their own, sometimes acting as enforcers and occasionally as rivals to central enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (court ritual authorities, warrior government, great monastic complexes, hereditary shrine lineages) derive low directionality - the arrangement subsidizes them, so effective extraction damps toward or below zero for those seats. Declared victims (danka households, subordinate kami priests, heterodox advocates) derive high directionality; the danka households' trapped exit amplifies them nearest the full-target end, since registration followed the household and leaving meant abandoning village membership. Monastic complexes are beneficiaries whose constrained exit (privileges exist only inside the arrangement) keeps them firmly on the subsidy side despite their enforcement capacity. The excluded unlicensed ascetics derive a high-directionality profile without holding a payer role - exclusion is the enforcement object. The observer seat is analytical and feeds no derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding integration problem - fusing two cult systems so both served state aims without undermining each other - was administratively settled centuries before enforcement peaked; by the Tokugawa phase the arrangement's operative content was surveillance and revenue guarantee, not integration, which is why founding_problem_status is contested and the disappearance verdict is world_rearranges rather than unchanged. Classification discipline cuts both ways here: claiming snare prevents the arrangement from being credited as pure coordination (rope) on the strength of its genuine service-delivery surface - the harmony framing is exactly the cover this reading identifies. Conversely, the enforcement_vs_voluntary_commitment omega prevents overcorrection: if lapse-of-enforcement episodes showed durable voluntary fusion, the verdict should migrate toward tangled_rope, acknowledging a coordination residue. The kernel_absence omega carries the committer structure: this is one reading among three, and the classification is a claim OF this reading, falsifiable by the corpus against its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_absence_claim_status,
    'This story instantiates the incoherent_bundle_reading of kernel shinbutsu_ontological_substrate. The kernel contest - does the enforced kami-buddha fusion order instantiate a unified ontological commitment (syncretic_fusion_reading), a functional domain partition (domain_partition_reading), or no coherent principle at all (this reading) - remains open. What would adoption of a sibling reading change structurally?',
    'Sibling readings live as separate constraint files linked via network.affects_constraints; corpus comparison of their computed classifications against this reading''s snare verdict locates the disagreement, which sits entirely in whether a unifying commitment exists - not in any measured operational metric.',
    'If the fusion reading were correct, this arrangement gains a genuine coordination core and migrates toward tangled_rope with lower excess extraction; if the partition reading were correct, practitioners bear less contradictory burden and the extraction profile redistributes across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_absence_claim_status, conceptual, 'Committer routing: one reading of the shinbutsu kernel; sibling readings would alter beneficiary structure and computed type.').

omega_variable(
    enforcement_vs_voluntary_commitment,
    'Did the fused order persist because practitioners and institutions endorsed it, or because state enforcement held it together?',
    'Natural experiments where enforcement lapsed: Sengoku-era disruption of central authority, and the post-1868 separation. Rapid, near-total collapse after 1868 (shrine stripping of temples, mass closures, laicizations) supports enforcement dependence; durable voluntary continuation in enforcement gaps would support a residual coordination function.',
    'Resolves the snare-versus-tangled_rope boundary: demonstrated voluntary persistence would shift the computed classification toward tangled_rope and raise the coordination-function credit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_vs_voluntary_commitment, empirical, 'Persistence mechanism: coercion versus endorsement.').

omega_variable(
    practitioner_contradiction_burden,
    'How costly was the lived doctrinal contradiction for ordinary households - did fused demands (Buddhist funerary obligation alongside kami festival obligation, fees to both) register as burden, or as complementary practices?',
    'Village documents, household registers, complaint petitions, and diaries recording ritual expenditure and expressed grievance across the interval.',
    'Pervasive burden supports high extraction and the snare verdict; pervasive complementarity cuts effective extraction and weakens this reading against the domain_partition_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_contradiction_burden, empirical, 'Magnitude of the lived contradiction borne by payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 741, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t741, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 741, 0.14).
narrative_ontology:measurement_basis(shin_tr_t741, observed).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement_basis(shin_tr_t900, observed).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1050, 0.23).
narrative_ontology:measurement_basis(shin_tr_t1050, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1350, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1350, 0.32).
narrative_ontology:measurement_basis(shin_tr_t1350, observed).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1500, 0.37).
narrative_ontology:measurement_basis(shin_tr_t1500, observed).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1650, 0.41).
narrative_ontology:measurement_basis(shin_tr_t1650, observed).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1750, 0.39).
narrative_ontology:measurement_basis(shin_tr_t1750, observed).
narrative_ontology:measurement(shin_tr_t1830, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1830, 0.37).
narrative_ontology:measurement_basis(shin_tr_t1830, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1868, 0.33).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t741, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 741, 0.38).
narrative_ontology:measurement_basis(shin_be_t741, observed).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 900, 0.44).
narrative_ontology:measurement_basis(shin_be_t900, observed).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1050, 0.49).
narrative_ontology:measurement_basis(shin_be_t1050, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1200, 0.54).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1350, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1350, 0.57).
narrative_ontology:measurement_basis(shin_be_t1350, observed).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1500, 0.6).
narrative_ontology:measurement_basis(shin_be_t1500, observed).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1650, 0.69).
narrative_ontology:measurement_basis(shin_be_t1650, observed).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1750, 0.71).
narrative_ontology:measurement_basis(shin_be_t1750, observed).
narrative_ontology:measurement(shin_be_t1830, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1830, 0.72).
narrative_ontology:measurement_basis(shin_be_t1830, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1868, 0.7).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t741, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 741, 0.3).
narrative_ontology:measurement_basis(shin_su_t741, observed).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 900, 0.42).
narrative_ontology:measurement_basis(shin_su_t900, observed).
narrative_ontology:measurement(shin_su_t1050, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1050, 0.48).
narrative_ontology:measurement_basis(shin_su_t1050, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1200, 0.52).
narrative_ontology:measurement_basis(shin_su_t1200, observed).
narrative_ontology:measurement(shin_su_t1350, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1350, 0.55).
narrative_ontology:measurement_basis(shin_su_t1350, observed).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement_basis(shin_su_t1500, observed).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1650, 0.76).
narrative_ontology:measurement_basis(shin_su_t1650, observed).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1750, 0.77).
narrative_ontology:measurement_basis(shin_su_t1750, observed).
narrative_ontology:measurement(shin_su_t1830, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1830, 0.75).
narrative_ontology:measurement_basis(shin_su_t1830, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1868, 0.18).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% Kernel shinbutsu_ontological_substrate decomposes into three reading-constraints per the epsilon-invariance principle: this file (incoherent_bundle_reading - no coherent kernel, enforced drift, snare-flavored verdict), syncretic_fusion_reading (ontological unity, honji suijaku as metaphysical truth), and domain_partition_reading (functional separation of kami and buddha domains). Each story carries its own epsilon over the SAME standing enforced-fusion arrangement; whichever sibling attests a unifying commitment lowers this reading's excess-extraction credit, and this reading's enforcement-collapse evidence undermines theirs. Linked via affects_constraints as a constraint family; no averaging across readings occurs inside any single file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
