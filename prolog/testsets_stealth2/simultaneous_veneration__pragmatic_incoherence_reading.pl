% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Kami-Buddha Veneration — Pragmatic Incoherence Reading
 *   domain: religious studies/comparative religion/Japanese history
 *
 * SUMMARY:
 *   For roughly a millennium, Japanese religion ran on the combined
 *   veneration of kami and buddhas: shrines and temples shared grounds, kami
 *   were preached as local traces of buddhas, and every household honored the
 *   local kami while belonging to a Buddhist parish. This story authors ONE
 *   reading of that arrangement — the pragmatic incoherence reading — under
 *   which the arrangement never had coherent doctrinal content: practitioners
 *   held contradictory beliefs simultaneously (kami as powerful independent
 *   deities; kami as dependent traces of buddhas; kami as soteriologically
 *   powerless) without resolution, because no authority ever enforced
 *   adjudication of the question. On this reading the Meiji separation edicts
 *   of 1868 were not an imposed rupture of a coherent whole but the first
 *   enforcement pressure in the arrangement's history, whose arrival revealed
 *   how little internal coherence had ever held it up. The claim and the
 *   metrics are authored independently: the claimed type records my
 *   structural judgment that the arrangement combined genuine coordination
 *   (shared rites, death care, festival life) with real, actively enforced
 *   asymmetric precedence, while the reading's instability thesis is carried
 *   by the metric series — accumulating extractiveness, rising theatricality,
 *   and a suppression trajectory that collapses when the Meiji state flips
 *   enforcement against the arrangement. Sibling readings are separate
 *   constraint files linked in network.affects_constraints. KEY AGENTS (by
 *   structural relationship): buddhist_temple_establishment — agenda-setting
 *   beneficiary (institutional/arbitrage); shrine_priesthood — subordinated
 *   payer with secondary beneficiary position (organized/constrained);
 *   lay_practitioners — primary payer and incidental beneficiary
 *   (moderate/constrained); kami_cult_partisans — payer
 *   (organized/identity_locked); pure_land_exclusivist_schools — beneficiary
 *   (institutional/mobile); kami_exclusivist_devotees — excluded
 *   (powerless/trapped); meiji_state_ideologues — external terminator
 *   (institutional/mobile); doctrinal_analysts — analytical observer.
 *
 * KEY AGENTS:
 *   - buddhist_temple_establishment: agenda-setting beneficiary (institutional/arbitrage) — administers the combined complexes, collects their revenue, composes the doctrinal syntheses
 *   - shrine_priesthood: subordinated payer with secondary beneficiary position (organized/constrained) — keeps the kami cults running inside a structure that outranks them
 *   - lay_practitioners: primary payer and incidental beneficiary (moderate/constrained) — bears the unresolved contradiction and receives the combined system's services
 *   - kami_cult_partisans: payer (organized/identity_locked) — lineages whose vocation is the kami's independent dignity, fighting subordination from inside
 *   - pure_land_exclusivist_schools: beneficiary (institutional/mobile) — thrive inside the arrangement their own doctrine contradicts
 *   - kami_exclusivist_devotees: excluded (powerless/trapped) — worshippers for whom the kami suffice; no seat in the combined system
 *   - meiji_state_ideologues: external agenda-setter (institutional/mobile) — terminate the arrangement by edict in 1868
 *   - doctrinal_analysts: analytical observer (analytical/analytical) — authors the competing readings, including this one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.66).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.52).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Kami-Buddha Veneration — Pragmatic Incoherence Reading").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious studies/comparative religion/Japanese history").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '27f024cf-bbf6-4fe5-8242-fcb188fa3107').
narrative_ontology:cs_kernel_codification('27f024cf-bbf6-4fe5-8242-fcb188fa3107', distributed).
narrative_ontology:cs_authority_grounding('27f024cf-bbf6-4fe5-8242-fcb188fa3107', distributed).
narrative_ontology:cs_reading_relation('27f024cf-bbf6-4fe5-8242-fcb188fa3107', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('27f024cf-bbf6-4fe5-8242-fcb188fa3107', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('27f024cf-bbf6-4fe5-8242-fcb188fa3107', foundational, simultaneous_veneration_never_coherent).
narrative_ontology:cs_axiom_status(simultaneous_veneration_never_coherent, holdable).
narrative_ontology:cs_axiom_grounding('27f024cf-bbf6-4fe5-8242-fcb188fa3107', simultaneous_veneration_never_coherent, empirically_contingent).
narrative_ontology:cs_axiom('27f024cf-bbf6-4fe5-8242-fcb188fa3107', foundational, persistence_via_enforcement_absence).
narrative_ontology:cs_axiom_status(persistence_via_enforcement_absence, holdable).
narrative_ontology:cs_axiom_grounding('27f024cf-bbf6-4fe5-8242-fcb188fa3107', persistence_via_enforcement_absence, empirically_contingent).
narrative_ontology:cs_axiom('27f024cf-bbf6-4fe5-8242-fcb188fa3107', secondary, meiji_separation_reveals_latent_incoherence).
narrative_ontology:cs_axiom_status(meiji_separation_reveals_latent_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('27f024cf-bbf6-4fe5-8242-fcb188fa3107', meiji_separation_reveals_latent_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('27f024cf-bbf6-4fe5-8242-fcb188fa3107', unadjudicated_practical_coexistence).
narrative_ontology:cs_drift_state('27f024cf-bbf6-4fe5-8242-fcb188fa3107', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('27f024cf-bbf6-4fe5-8242-fcb188fa3107', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_temple_establishment).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, pure_land_exclusivist_schools).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, shrine_priesthood).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, kami_cult_partisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shrine_priesthood).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, gongen_worship_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the great temple networks and, through the betto system, administers many of the country's major shrines; operates combined shrine-temple complexes and, in the Edo period, holds the legal power to register every household as a Buddhist parishioner. Composes and transmits the teaching that presents kami as manifestations of buddhas. Collects offerings, funeral fees, and registration revenue from the combined system, and can reframe doctrine when challenged — as it did when it absorbed earlier kami-primacy claims into new synthesis teachings.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_temple_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_temple_establishment, beneficiary).

% Hereditary lineages tending the kami cults — festival calendars, purification rites, offerings. Under the combined arrangement many shrines sit under Buddhist administration or share their grounds with temple halls; the priesthood keeps its cults funded and its festivals attended but cedes precedence and, at many sites, administrative control. Those who assert the kami's independent dignity must argue from inside the arrangement, since leaving it means losing the complex's financing and lay base.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shrine_priesthood, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, shrine_priesthood, beneficiary).

% Farmers, townspeople, and village communities who call on kami for rain, harvest, and protection and turn to temples for funerals, memorial services, and the afterlife. They hold both devotions without ever being asked how the two fit together — no teacher, priest, or magistrate requires them to reconcile praying at a shrine with belonging to a Buddhist parish. When a school they belong to teaches that kami cannot save, the tension lands on them unresolved. Leaving the combined system would mean giving up either death care or the festival life that organizes the village year.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners, beneficiary).

% Scholar lineages and shrine houses — the Watarai of Ise, the Yoshida house, later the National Learning scholars — for whom the kami's independent dignity is the point of their whole vocation. Their claims are repeatedly absorbed into Buddhist-framed syntheses or subordinated as traces of higher buddhas; they cannot accept the buddha-superiority framing without dissolving their own project, so they fight from within, building alternative doctrinal systems that borrow the arrangement's forms while reversing its precedence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, kami_cult_partisans, payer,
    organized, generational, identity_locked, regional).

% The new Kamakura schools — Pure Land above all, and in a different register Nichiren's — teaching salvation through the buddha's vow alone. Their doctrine implies the kami offer no path to birth in the Pure Land: the sharpest contradiction the arrangement contains. Institutionally they thrive inside it — they take parish registrations and protections like every other school, and no authority ever compels them to square their teaching with their patrons' shrine devotions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, pure_land_exclusivist_schools, beneficiary,
    institutional, generational, mobile, national).

% Village worshippers for whom the kami are sufficient — who bring questions of fortune, illness, and protection to the shrine and have no use for Buddhist death care or doctrine. The combined system has no seat for them: shrine devotion is channeled through complexes their Buddhist parish taxes and administers, and no institution represents the view that the kami need no buddha behind them.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, kami_exclusivist_devotees, excluded,
    powerless, biographical, trapped, local).

% The National-Learning-influenced reformers who take power in 1868 and issue the separation edicts: shrines are stripped of Buddhist halls, icons, and names; shrine priests are redefined as state ritualists; combined complexes are dismantled within a few years. For them the old arrangement is a millennium-long error to be corrected, and they hold the power to impose the correction without negotiating with its beneficiaries.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state_ideologues, agenda_setter,
    institutional, generational, mobile, national).

% Modern historians and scholars of religion reconstructing what pre-modern practitioners actually believed. They see the whole structure at once — the elite theories, the popular practices, the enforcement records, the Meiji collapse — and author competing interpretations of it; this story is one of those interpretations.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, doctrinal_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_temple_establishment).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement let kami cults and Buddhist institutions share sites, festivals, funerals, and patronage without adjudicating their doctrinal relationship: communities obtained this-worldly rites and funerary and salvific services from one institutional fabric, and competing schools coexisted under a single administrative umbrella.
% TRANSFER_FUNCTION: Moved offerings, labor, and registration revenue from lay households to the combined shrine-temple complexes — concentrated in the Buddhist establishments that administered the shrines — and moved doctrinal precedence from kami cults to Buddhist schools, which taught the kami as traces of buddhas. It also moved the cost of unresolved contradiction onto practitioners, who were never required to reconcile their kami devotions with their Buddhist affiliations.
% ABSENT_VOICES: Kami-exclusivist worshippers had no institutional seat: the combined system channeled shrine devotion through Buddhist-administered structures, and no institution represented the position that the kami suffice without a buddha behind them. Ordinary practitioners who experienced the contradiction had no forum in which resolution could even be posed — the absence of any adjudicating authority is this reading's central mechanism, and it is also what kept the objection from ever being collectively voiced.
% DISAPPEARANCE_RATIONALE: The combined complexes were the religious infrastructure: funerary care, festival calendars, local financing, and school affiliation all ran through them. Overnight disappearance would force immediate reorganization of death care, this-worldly rite, and institutional revenue — as demonstrated in miniature by the actual Meiji separation, which compelled exactly this rearrangement at state speed and met popular anti-Buddhist energy the moment protection lifted.
% FOUNDING_PROBLEM: Early Japanese religion needed to integrate an indigenous kami cult with a continental Buddhist tradition that arrived with state power, literacy, and ritual technology: how could kami worship and Buddhist institution-building coexist without one absorbing or destroying the other?
% FOUNDING_PROBLEM_CORROBORATION: Court chronicles and state edicts from the Nara and Heian periods — produced before the temple establishment held its later dominance — attest that the integration problem was real and administratively urgent. The modern Kuroda Toshio school of historiography, working entirely outside the beneficiary set, corroborates that the arrangement was a governance solution rather than a doctrine. The death of the problem is attested by the Meiji separation edicts themselves, which dissolved the coexistence question by terminating coexistence rather than resolving it; no beneficiary-party source is relied on for either the founding claim or its status.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 for the mature arrangement: material transfer (offerings, funeral and registration revenue, shrine networks administered by temples under the betto system, compulsory parish affiliation under the Edo registration system) plus the cognitive cost this reading counts as the arrangement's distinctive extraction — centuries of practitioners holding beliefs their own schools implied were incompatible, with no institution ever required to reconcile them. Suppression at 0.52 is structural: institutional barriers to kami-only practice, registration compulsion, and — most distinctively — the absence of any forum in which the contradiction could even be posed; there was no doctrinal inquisition, and the arrangement suppressed resolution rather than belief. Theater at 0.52 reflects the growing share of arrangement-maintenance that was performative: the fusion doctrine recited ritually while the schools teaching it sharpened the contradictions it smoothed over, until National Learning scholars could plausibly call the syntheses fabrications. Accessibility_collapse at 0.50 records that alternatives existed but were costly: the Yoshida and Watarai systems built kami-primacy frameworks inside the arrangement's own forms, and Pure Land exclusivism offered a doctrinal exit that never became an institutional one. Resistance at 0.55 is sustained and ultimately decisive: shrine-house maneuvering, National Learning critique, and the popular anti-Buddhist energy that erupted the moment Meiji enforcement lifted the arrangement's protections. The temporal series run on one shared grid (800-1868) and show the reading's central dynamics: base extractiveness accumulating as doctrinal developments sharpened the contradiction the arrangement never resolved; theater rising as maintenance grew increasingly performative; suppression rising with Tokugawa registration compulsion and then collapsing at 1868 as the new state dismantled the enforcement machinery — the signature of an arrangement held up by enforcement on one side and non-adjudication on the other.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the buddhist_temple_establishment seat the arrangement is a legitimate order it built and administers — precedence earned by ritual service, revenue the price of maintenance. From the lay_practitioners seat it is simply the only religious infrastructure available: death care, festivals, and parish belonging arrive as a package, and the contradiction is a background condition — unless a school's own teaching makes it acute. From the shrine_priesthood seat it is subordination with a salary: cults kept funded and attended, autonomy and precedence ceded. From the kami_cult_partisans seat it is a millennium-long demotion of their object of devotion. The meiji_state_ideologues seat stands outside the structure entirely and reads it as error. A same-level comparison matters: shrine_priesthood and pure_land_exclusivist_schools are both organized religious institutions inside the same arrangement, but the priesthood's cult object is what the arrangement subordinates while the Pure Land schools collect its protections while teaching against its premise — same power level, opposite structural relationships, because the arrangement enforced precedence over kami institutions and never enforced coherence on Buddhist doctrine. Inter-institutionally, the court, the bakufu, the temple networks, and the shrine houses all produced competing framings without any single adjudicator — which is precisely why the contradiction survived. Coalition potential among the diffuse payer seats was structurally blocked: a coalition against unresolved contradiction cannot form when no forum exists in which the contradiction can be named.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the buddhist_temple_establishment collects the arrangement's revenue and holds its precedence (d near the beneficiary end); the pure_land_exclusivist_schools collect registration and protection without paying the arrangement's costs (low d). Victim declarations map the other way: lay_practitioners bear both the material transfers and the contradiction cost (high d, damped by the genuine services they receive); the shrine_priesthood bears subordination (high d, damped by cult funding); the kami_cult_partisans bear the demotion of their vocation (high d, amplified by identity lock — they cannot exit without dissolving the project that defines them; the identity fusion here is vocational and lineage-based, and if the kami-primacy frame broke, their position would collapse into ordinary shrine administration with far lower d). The meiji_state_ideologues are neither beneficiary nor target of this arrangement: they are its external terminator. No directionality override is authored — the override surface is keyed by power atom rather than agent, and an institutional-atom override would also misstate the temple establishment — so their seat rides the engine's structural fallback, with their external position documented here and in their situation text. The kami_exclusivist_devotees carry an excluded seat: their objection was never collectable because the arrangement had no place where it could be voiced.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy lens clarifies what this reading claims and does not claim. The arrangement did not outlive its founding problem — the coexistence problem stayed live, unresolved, for the entire interval — so this is not a classic mandate-outlived-function case and no mandatrophy_resolved flag is authored. What the reading adds is stranger: the arrangement never had a coherently defined function to outlive. The mislabeling risks run in both directions. Calling the arrangement a rope erases the extraction — the subordination of kami institutions, the compulsory parish revenue, and the contradiction cost this reading counts. Calling it a snare overclaims — the services were real and valued, the arrangement collapsed the moment enforcement flipped rather than resisting, and pure extraction does not die that quietly. The tangled rope classification with the rising theater series captures the drift toward inertial, increasingly performative maintenance in the later centuries without overclaiming piton status: concentrated beneficiaries collected throughout, and a constraint with a seat that captures its gains is not a piton. The Meiji separation resolved the question the arrangement spent a millennium not answering — by destruction rather than adjudication, which is itself this reading's strongest evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the pragmatic_incoherence_reading of the kernel simultaneous_veneration — would instantiating a sibling reading (ontological_fusion_reading or domain_partition_reading) change the classification, and where exactly do the readings disagree?',
    'The readings disagree on one empirical locus — whether pre-modern practitioners'' actual belief-sets contained unresolved contradictions — and one conceptual locus — whether a coherence available in theory but never achieved in practice counts as coherence of the arrangement. Authoring the sibling files and comparing their epsilon, victim sets, and computed types resolves the structural delta; the empirical locus is pursued under popular_belief_recoverability.',
    'The fusion and partition readings would author low-to-moderate extraction (no contradiction cost), smaller victim sets, and stable coordination structures — the same historical arrangement computes as a genuine rope from those seats. The classification of this file is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the classification: sibling readings of the same kernel instantiate different constraints.').

omega_variable(
    popular_belief_recoverability,
    'Can the actual content of pre-modern popular religious belief be recovered precisely enough to test whether lay practitioners held contradictory beliefs (this reading) or cleanly domain-specialized ones (partition reading)?',
    'Corpus work on popular didactic literature, votive inscriptions, liturgical texts, village registration records, and doctrinal history of what each school actually taught its parishioners, triangulated against Meiji-era conversion patterns.',
    'Clean partition evidence collapses this reading''s epsilon toward the partition reading''s; documented unresolved contradiction — parishioners of Pure Land schools maintaining active kami devotions for salvation-adjacent hopes, for example — confirms it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_belief_recoverability, empirical, 'Recoverability of popular belief content — the empirical hinge between this reading and the partition reading.').

omega_variable(
    contradiction_experience_ambiguity,
    'Did practitioners experience the unresolved contradiction as a cost (suppressed tension), or is the incoherence visible only to outside analysts while practitioners felt no tension at all?',
    'Evidence of popular awareness of the tension: didactic texts explaining the kami-buddha relationship to lay audiences, popular critiques, the speed and direction of Meiji-era conversion, and village disputes over shrine-temple fees and obligations.',
    'If practitioners never experienced tension, the cognitive-cost component of epsilon collapses and this reading''s extractiveness drops toward material transfer alone; the suppressed-contradiction framing would then be an analyst''s imposition rather than a borne cost. If tension was lived and suppressed, the reading''s distinctive extraction claim does real classification work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contradiction_experience_ambiguity, empirical, 'Whether the contradiction was a lived, suppressed cost or an analyst''s reconstruction — the suppression-mechanism ambiguity for this reading.').

omega_variable(
    meiji_counterfactual_decay,
    'Was the arrangement stable until the Meiji edicts, or already decaying under National Learning critique, temple-financial strain, and accumulating anti-clerical sentiment?',
    'Late-Tokugawa trend analysis: shrine-temple dispute frequency, temple debt records, circulation of National Learning texts, and the speed and popular participation in the anti-Buddhist destruction that followed the edicts.',
    'If already decaying, the revelation thesis strengthens — the incoherence was surfacing independently of the state; if stable until the edicts, the absence-of-enforcement mechanism is confirmed as the sole sustaining condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_counterfactual_decay, empirical, 'Counterfactual persistence of the arrangement absent Meiji enforcement pressure.').

omega_variable(
    extraction_composition_ambiguity,
    'How much of the authored epsilon is material transfer (offerings, registration revenue, shrine subordination) versus cognitive cost (unresolved contradiction borne by practitioners)?',
    'Decompose the arrangement''s revenue and labor flows (temple ledgers, village records) against the documented contradiction-cost evidence; the two components respond to different counterfactuals — enforcement removal versus doctrinal clarification.',
    'If epsilon is mostly material, the arrangement reads as conventional institutional extraction and the incoherence thesis matters less for classification; if mostly cognitive, this reading''s distinctive claim does the classification work and the sibling readings diverge sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_composition_ambiguity, conceptual, 'Composition of epsilon between material transfer and contradiction cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 800, 0.25).
narrative_ontology:measurement_basis(simu_tr_t800, observed).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement_basis(simu_tr_t1000, observed).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement_basis(simu_tr_t1200, observed).
narrative_ontology:measurement(simu_tr_t1400, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1400, 0.4).
narrative_ontology:measurement_basis(simu_tr_t1400, observed).
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1600, 0.46).
narrative_ontology:measurement_basis(simu_tr_t1600, observed).
narrative_ontology:measurement(simu_tr_t1750, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1750, 0.52).
narrative_ontology:measurement_basis(simu_tr_t1750, observed).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1868, 0.55).
narrative_ontology:measurement_basis(simu_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 800, 0.45).
narrative_ontology:measurement_basis(simu_be_t800, observed).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1000, 0.5).
narrative_ontology:measurement_basis(simu_be_t1000, observed).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1200, 0.56).
narrative_ontology:measurement_basis(simu_be_t1200, observed).
narrative_ontology:measurement(simu_be_t1400, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1400, 0.61).
narrative_ontology:measurement_basis(simu_be_t1400, observed).
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement_basis(simu_be_t1600, observed).
narrative_ontology:measurement(simu_be_t1750, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1750, 0.68).
narrative_ontology:measurement_basis(simu_be_t1750, observed).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1868, 0.66).
narrative_ontology:measurement_basis(simu_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement_basis(simu_su_t800, observed).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1000, 0.38).
narrative_ontology:measurement_basis(simu_su_t1000, observed).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1200, 0.4).
narrative_ontology:measurement_basis(simu_su_t1200, observed).
narrative_ontology:measurement(simu_su_t1400, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1400, 0.42).
narrative_ontology:measurement_basis(simu_su_t1400, observed).
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1600, 0.48).
narrative_ontology:measurement_basis(simu_su_t1600, observed).
narrative_ontology:measurement(simu_su_t1750, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1750, 0.52).
narrative_ontology:measurement_basis(simu_su_t1750, observed).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1868, 0.18).
narrative_ontology:measurement_basis(simu_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu shugo (simultaneous veneration)' covers three structurally distinct claims about whether the arrangement was coherent and what held it up. This file instantiates the pragmatic incoherence reading; ontological_fusion_reading and domain_partition_reading instantiate the coherence claims. The readings differ in epsilon (this reading authors high extraction from the contradiction cost the others deny exists), in victim sets (this reading counts lay practitioners' unresolved tension and the kami partisans' subordination as costs; the partition reading dissolves both as domain-appropriate specialization), and in what the Meiji separation was (revelation of latent incoherence versus imposed rupture of a coherent whole). Per the epsilon-invariance principle they are authored as separate constraints and linked here as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
