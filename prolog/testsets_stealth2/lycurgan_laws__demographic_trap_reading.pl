% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan No-Revision Settlement — Demographic Trap Reading
 *   domain: political philosophy/constitutional theory/commitment systems
 *
 * SUMMARY:
 *   This story instantiates the demographic-trap reading of the Lycurgan-laws
 *   kernel: the Great Rhetra's no-revision commitment, locking the kleros
 *   property settlement and the mess-contribution citizenship threshold in
 *   place, operated as an enforced barrier against every adaptation the
 *   city's demography demanded. The epsilon referent is the standing Lycurgan
 *   arrangement as this reading assesses it — the frozen settlement itself,
 *   not the revisable constitution this reading would have preferred. On that
 *   referent the reading authors high extraction (surplus and civic standing
 *   flowed upward as the citizen base shrank), very high suppression
 *   (revision was blocked by council control of the agenda, religious
 *   sanction, and finally the strangulation of a sitting king), and a rising
 *   theater ratio (forms maintained while substance emptied). Claim and
 *   metrics are independent facts here: the reading CLAIMS snare because it
 *   identifies a coordination cover story (peer equality, generational
 *   stability) wrapped around asymmetric transfer with identifiable victims
 *   and active enforcement; the metrics are authored from the historical
 *   record, not tuned to the claim. Sibling readings — sacral_fidelity (the
 *   ordinances as divine and inviolable) and adaptive_fiction (immutability
 *   as noble lie over covert adaptation) — are separate constraints linked
 *   through network.affects_constraints; this file does not average over
 *   them. KEY AGENTS (by structural relationship): landed_kleros_lineages:
 *   primary beneficiary (powerful/constrained) — holdings preserved, surplus
 *   secured by the no-revision settlement; gerousia_elder_class: agenda
 *   setter and beneficiary (institutional/identity_locked) — administers the
 *   settlement it profits from; ephors_overseers: enforcement arm
 *   (institutional/immediate horizon) — administers discipline, biased toward
 *   order over repair; reformist_kings: nullified agenda setter
 *   (powerful/trapped) — attempted revision from inside office and were
 *   destroyed; mess_defaulting_poorer_spartiates: payer approaching the
 *   threshold (moderate/trapped); hypomeiones_disenfranchised_spartiates:
 *   payer already struck from the rolls (powerless/constrained);
 *   helot_agricultural_underclass: deepest payer (powerless/trapped) — labor
 *   funds the settlement; perioikoi_communities: excluded seat
 *   (organized/constrained) — indispensable, unadmitted;
 *   hellenic_rival_poleis: institutional observer exploiting the decline;
 *   classical_political_philosophers: analytical observer (Aristotle,
 *   Politics II) supplying the external diagnosis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.84).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.88).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan No-Revision Settlement — Demographic Trap Reading").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political philosophy/constitutional theory/commitment systems").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '44d47896-5c14-4f92-895c-f0111ba858bf').
narrative_ontology:cs_kernel_codification('44d47896-5c14-4f92-895c-f0111ba858bf', fixed_text).
narrative_ontology:cs_authority_grounding('44d47896-5c14-4f92-895c-f0111ba858bf', extraction).
narrative_ontology:cs_interpretation_layer_present('44d47896-5c14-4f92-895c-f0111ba858bf').
narrative_ontology:cs_reading_relation('44d47896-5c14-4f92-895c-f0111ba858bf', lycurgan_laws__sacral_fidelity_reading, influences).
narrative_ontology:cs_reading_relation('44d47896-5c14-4f92-895c-f0111ba858bf', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('44d47896-5c14-4f92-895c-f0111ba858bf', foundational, unrevisability_blocks_demographic_adaptation).
narrative_ontology:cs_axiom_status(unrevisability_blocks_demographic_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('44d47896-5c14-4f92-895c-f0111ba858bf', unrevisability_blocks_demographic_adaptation, empirically_contingent).
narrative_ontology:cs_axiom('44d47896-5c14-4f92-895c-f0111ba858bf', foundational, fixity_defeats_constitutive_aim_of_preserving_peers).
narrative_ontology:cs_axiom_status(fixity_defeats_constitutive_aim_of_preserving_peers, holdable).
narrative_ontology:cs_axiom_grounding('44d47896-5c14-4f92-895c-f0111ba858bf', fixity_defeats_constitutive_aim_of_preserving_peers, instrumental).
narrative_ontology:cs_reference_frame('44d47896-5c14-4f92-895c-f0111ba858bf', binding_immutable_rhetra_order).
narrative_ontology:cs_drift_state('44d47896-5c14-4f92-895c-f0111ba858bf', post_leuctra_terminal_phase, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('44d47896-5c14-4f92-895c-f0111ba858bf', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, landed_kleros_lineages).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, gerousia_elder_class).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, hypomeiones_disenfranchised_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, mess_defaulting_poorer_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helot_agricultural_underclass).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, reformist_kings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold hereditary allotments (kleroi) worked by helot labor. The allotment funds their mess contributions and underwrites full civic standing. Inheritance, dowry, and endogamous marriage concentrate holdings over generations; households that lapse are absorbed by kin lines. Their wealth is fixed in Laconian and Messenian land; leaving means abandoning both estate and rank. The settlement keeps their holdings intact and their surplus flowing; when redistribution was proposed, they defended the existing allotments through the councils they fill.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, landed_kleros_lineages, beneficiary,
    powerful, generational, constrained, national).

% Twenty-eight elders plus the kings, serving for life after age sixty, drawn overwhelmingly from the landed houses. They prepare business for the assembly, judge capital cases, and decide what counts as faithful adherence to the founder's ordinances. Their own estates and dignity rest on the arrangements they guard; retirement from the council is not a thing anyone does — the council is what they are.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, gerousia_elder_class, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, gerousia_elder_class, beneficiary).

% Five annual magistrates elected from the citizen body, sworn to uphold the established order. They convene and dismiss the assembly, prosecute officials including kings, and administer the surveillance and discipline that keep the subject population and the rank-and-file in line. Their term lasts one year, so their attention runs to immediate order rather than long-run repair; challenging the founder's framework has never been part of the office.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephors_overseers, agenda_setter,
    institutional, immediate, constrained, national).

% Kings who inherited the throne and read the city's shrinking muster rolls as an emergency. Agis IV proposed cancelling debts and restoring equal allotments; the council and ephors blocked him, his coalition faltered, and he was tried and strangled. Cleomenes III forced a similar program through decades later by violence and exile of opponents, held it briefly, and lost everything at Sellasia. Office gave them a platform, not protection: the moment their agenda touched the property settlement, the machinery turned on them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, reformist_kings, agenda_setter,
    powerful, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, reformist_kings, payer).

% Full citizens whose allotments no longer yield the fixed contribution their mess requires. Each shortfall moves them toward demotion; once they cannot pay, they lose the standing their entire upbringing prepared them for. Leaving the city means losing citizenship outright; staying means watching the threshold approach. They sit in the assembly with less and less to lose.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, mess_defaulting_poorer_spartiates, payer,
    moderate, biographical, trapped, national).

% Men raised through the same training as the peers who fell below the contribution line and were struck from the citizen rolls. They keep their arms, their skills, and their grievances, but no vote, no mess seat, and no path back. Some take service abroad as mercenaries; most remain inside the society that demoted them, counted unreliable by the regime's police.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, hypomeiones_disenfranchised_spartiates, payer,
    powerless, biographical, constrained, national).

% A conquered population bound to the land, farming it for masters who take the surplus. They outnumber their lords many times over. Movement is punishable; the state declares ritual war to legitimize killing them; runaway communities survive at the margins or abroad. Their labor funds every allotment; their subjection is the precondition of the whole citizen order.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_agricultural_underclass, payer,
    powerless, generational, trapped, regional).

% Free inhabitants of the towns scattered through Laconia and Messenia — traders, craftsmen, and soldiers who fight in the city's wars but hold no share in its citizenship. As the citizen rolls shrink, their levies grow more indispensable, yet admission remains closed. They can see the arrangement's growing dependence on them from the outside; they have no seat from which to say so.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, perioikoi_communities, excluded,
    organized, generational, constrained, regional).

% Neighboring states and leagues that watch the city's muster shrink year over year. They fight it, subsidize its enemies, liberate its subject territories when the balance tips, and record its decline as strategic intelligence. Nothing binds them to the city's internal settlements; they read its rigidity as opportunity.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, hellenic_rival_poleis, observer,
    institutional, generational, analytical, continental).

% Analysts writing constitutions up as comparative theory. One of them, examining the city's institutions from the outside, counts the muster rolls, traces the land concentration and the inheritance customs, and concludes the framework is destroying the citizen body it was built to preserve. They owe the arrangement nothing and can say what participants cannot.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, classical_political_philosophers, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, landed_kleros_lineages).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settlement solves a real collective-action problem for a small master class: keeping a few thousand full citizens cohesive, uniformly trained, and permanently under arms amid a subject population many times their number and hostile neighbors on every side. Common messes, shared upbringing, and equal allotments coordinate the citizen body; the founder's ordinances coordinate expectations across generations by promising that the rules themselves will not move.
% TRANSFER_FUNCTION: Moves the agricultural product of the helot villages to Spartiate households through the allotments; moves civic standing from poorer citizens to the propertied core whenever fixed mess contributions outrun shrinking incomes; and, across generations, moves land and rank from lapsed households into consolidating ones — with the no-revision rule ensuring none of these flows is ever corrected by law.
% ABSENT_VOICES: The helots are present as mute instruments and absent as voices; the perioikoi stand outside the constitution entirely; the hypomeiones are physically in the agora and politically erased; and Spartan women — who accumulate roughly two-fifths of the land under the inheritance customs Aristotle censures — deliberate nowhere, though the property settlement turns on their marriages and inheritances. Unanimity around the founder's framework was purchased by keeping every seat that would amend it out of the room.
% DISAPPEARANCE_RATIONALE: Strike the no-revision rule overnight and the constitution becomes amendable: debt relief, allotment restoration, widened citizenship, and mess-contribution reform all become live motions instead of impieties. The reform programs of Agis and Cleomenes were written proof that the arrangements depended on the rule — each began by attacking the rule itself. Property titles, council composition, and the citizen-roll threshold all rearrange within a generation.
% FOUNDING_PROBLEM: Secure a small Dorian citizen community against civil strife and conquest: fix the allotments so no citizen falls into debt-dependency, bind the peers to common mess and training, and freeze the settlement so faction can never reopen it. The founder's wager was that permanence itself would guarantee the equality and cohesion the city needed to survive.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Aristotle (Politics II.9–10) attests the failure from an external analytical seat — the citizen body fell from a field army of thousands to under a thousand, which he traces to the ungoverned inheritance and property customs the frozen settlement never addressed. The reformist kings' programs constitute insider testimony under duress, not independent corroboration. The decisive external attestation is comparative: poleis that revised citizenship and land law under comparable pressure did not exhibit the collapse. No defender of the settlement outside the benefiting houses attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.84 at interval end) reflects the settlement's operation: helot surplus funded the allotments throughout, while the mess threshold converted every economic shock into a transfer of standing from poor to propertied — Aristotle's count (under 1,000 citizens where once 10,000) is the endpoint of a century-long ratchet. Suppression (0.88) is this reading's core claim and is authored as a raw structural property, unscaled by power or scope: the no-revision rule was not self-enforcing — it ran on council control of the agenda, religious terror around the founder's ordinances, annual ephors sworn to the status quo, and, when persuasion failed, the trial and execution of a king. Theater (0.52) rises through the interval: the agoge, the rites, and the rhetoric of peer equality continued faithfully while the substance — a self-reproducing body of equals — emptied out. Accessibility collapse (0.55) sits mid-range because alternatives never vanished conceptually — redistribution was imagined, proposed, and twice attempted — but every practical channel closed: alternatives collapsed in practice while surviving in idea. Resistance (0.70) is high for an ancient polity: a helot coalition revolted outright in 464–460 BC and took a decade of war to crush; the disenfranchised defected and mercenarized; two kings staked their lives on reversal — the coalition possibility among powerless victims was real and was answered with terror calibrated to keep it expensive. Coordination type is authored as identity_coordination: the settlement's primary function is boundary maintenance of the citizen body — defining who counts as a peer through mess membership, training, and the contribution threshold. The FNL gaming risk is acute and acknowledged: 'we are the peers' is precisely the identity narrative that can launder asymmetric transfer, so the coupling test should scrutinize whether the identity function's complexity offset excuses a settlement concentrating standing on the propertied at polis-wide scope. No floor override is declared. The measurement series share one nine-point grid (T=0 approximately 550 BC through T=320 approximately 230 BC); the trajectory is a monotonic spiral punctuated, not reversed, by war shocks (the earthquake-and-revolt crisis, Leuctra). The limited oscillation is intermittent-reinforcement shaped: periodic crises briefly opened reform windows that enforcement then slammed shut, teaching each generation that motion was fatal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the gerousia's chair the settlement is sacred stability and secure title; the same no-revision rule reads, from a demoting household's chair, as the machine that strips status without appeal. The ephors supply a fifth perspective: enforcers with one-year tenures and biographically immediate horizons, structurally incapable of sponsoring repair no matter their private views. Same-nominal-status divergence is sharpest among the peers themselves: landed and landless homoioi held identical rank on paper, but the mess threshold made their exits and fates diverge completely — one seat's heritage is the other's trap. Identity lock binds the elder class: the councilors did not merely administer the founder's framework, they were constituted by it; break the frame and their dignity, genealogy, and function dissolve together — which is why the frame broke the reformers (Agis strangled, Cleomenes destroyed) before it was ever allowed to break. Inter-institutionally, kings, council, and ephorate experienced one constitution as three different arrangements — the kings commanded its army, the council owned its past, the ephors patrolled its present — and only the ephorate's incentives aligned with perpetual enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Landed kleros lineages sit nearest the beneficiary pole (d near 0): the settlement subsidizes them — holdings intact, surplus flowing, potential rivals for land extinguished by the threshold. The gerousia doubles as administrator and beneficiary; the ephors enforce without collecting much directly, placing them mid-range (roughly 0.35–0.45) — the structural declarations carry this without an override, since their enforcement role and absence from the beneficiary list derive a moderate d. The demoting and demoted seats sit near the target pole (d near 1): trapped or constrained exit amplifies their effective burden — a mess-defaulter cannot leave without forfeiting everything the upbringing installed. Helots occupy the extreme: powerless, trapped, hereditary. The reformist kings are the deliberately dual-positioned seat — nominal agenda-setters whose agenda-setting the machinery nullified, bearing payer costs (one strangled, one exiled and broken); the secondary_role declaration lets the engine price both positions. Perioikoi are authored as excluded rather than seated in the derivation: their objection is real but was never admitted to the conversation, which is itself part of the settlement's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cohesion through frozen equality — is dead: the freeze consumed the citizen body it was meant to perpetuate. Yet the world rearranges if the rule vanishes, because concentrated beneficiaries still collect from it. That combination (dead founding problem plus live dependency) is the zombie signature, and it resolves the classification away from both neighboring errors. It is not a piton: a piton persists by inertia with no one profiting enough to maintain it, whereas here identifiable houses profit enormously and demonstrably maintain it — they strangled a king to keep it. It is not a rope: the coordination story (peer equality, generational stability) is real as cover, but the settlement's net operation moves standing from the many to the few through a threshold no one was permitted to reopen. Resolving the mandatrophy correctly prevents the two standard mislabels: sentimental readings call the settlement pure coordination and miss the bodies; cynical readings call it mere inertia and miss the executions. The receipt surface records the truth: gains accrue to the landed lineages by name, and fixing was priced in royal blood — prohibitive by demonstration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the lycurgan_laws kernel (reading: demographic_trap_reading). What would each sibling reading change structurally if adopted as the operative account?',
    'Cross-reading comparison: instantiate sacral_fidelity_reading and adaptive_fiction_reading as separate constraints and compare epsilon, victim sets, and computed types against this file; convergence or divergence localizes which structural element carries the contest.',
    'The sacral reading would drive measured suppression toward zero (fidelity as voluntary devotion) and recast the settlement as sacred order; the adaptive-fiction reading would cut measured suppression sharply (the rule never really bound) and soften the death-spiral causal chain. This file''s classification stands or falls with the claim that the rule bound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three readings of the Lycurgan-laws kernel; siblings would alter epsilon, suppression, and victim sets.').

omega_variable(
    counterfactual_adaptability,
    'Was demographic collapse contingent on the no-revision rule, or would the citizen body have declined even under a revisable constitution (war losses, helot demography, earthquake shock)?',
    'Comparative constitutional demography: track citizen-body trajectories of poleis facing comparable shocks that did revise citizenship and land law (Rome''s continuous adaptation; cities widening citizenship after war losses) versus rigid peers.',
    'If collapse was overdetermined, the no-revision rule''s causal share shrinks and the account softens toward inertial persistence with theatrical maintenance; if revision-capable peers recovered while Sparta spiraled, the causal core of this reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_adaptability, empirical, 'Whether unrevisability was the binding cause of collapse or one contributor among several.').

omega_variable(
    elite_capture_vs_collective_blindness,
    'Did the propertied houses understand the mechanism and defend the freeze knowingly to protect holdings, or did the whole citizen body fail to perceive the threshold''s ratchet until too late?',
    'Reconstruct the deliberative record: council and assembly responses to every reform motion, the framing of Agis''s trial, ephoral rhetoric across the crisis decades.',
    'Knowing defense by identifiable beneficiaries confirms the capture structure this story authors; distributed blindness would shift the account toward inertia-plus-theater and weaken the named-seat receipt claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_vs_collective_blindness, empirical, 'Conscious entrenchment versus collective failure to perceive the demographic ratchet.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was adherence to the frozen settlement maintained by structural coercion (ephoral terror, trials, executions) or by internalized identity (agoge-formed selves for whom the founder''s framework was selfhood)?',
    'Post-exit trajectory: track peers removed from the framework — mercenaries abroad, exiles, captives — for whether the ethos persisted without enforcement machinery.',
    'A large internalized share means suppression travels with the agents after any structural opening, raising the effective barrier to reform beyond what the enforcement record alone shows; a predominantly structural share means removing the enforcers unlocks revision quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized shares of the settlement''s hold on its subjects.').

omega_variable(
    spartiate_count_uncertainty,
    'How steep was the actual citizen-body decline? The anchor figures (thousands at the Persian Wars; under 1,000 by Aristotle''s day) bracket a curve whose intermediate shape is reconstructed from fragmentary testimony.',
    'Prosopography of mess rosters and muster lists, archaeology of rural site abandonment in Laconia and Messenia, re-dating of the inheritance-concentration evidence.',
    'A shallower curve spreads the ratchet across more generations and dates the transition later; a steeper confirmed collapse tightens the death-spiral periodization this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spartiate_count_uncertainty, empirical, 'Uncertainty in the demographic curve anchoring the death-spiral claim.').

omega_variable(
    constraint_boundary_decomposition,
    'Is epsilon stable for the no-revision rule considered alone, or does measuring the whole Lycurgan package (allotment regime, agoge, mess system together) yield a different epsilon?',
    'Author a sibling story for the kleros property-citizenship regime itself with its own epsilon and stakeholder set; compare classifications. If the two stories diverge, the colloquial label ''Lycurgan laws'' covered two constraints.',
    'This file treats the freeze as the constraint and the property settlement as its locked content; a decomposed family would assign the property regime its own (likely high) epsilon and link both through network.affects_constraints, sharpening rather than overturning the present classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constraint_boundary_decomposition, conceptual, 'Epsilon-invariance check: immutability rule versus the full Lycurgan package as candidate constraint boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 320).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t40, lycurgan_laws__demographic_trap_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t80, lycurgan_laws__demographic_trap_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t120, lycurgan_laws__demographic_trap_reading, theater_ratio, 120, 0.27).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t160, lycurgan_laws__demographic_trap_reading, theater_ratio, 160, 0.33).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.4).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t240, lycurgan_laws__demographic_trap_reading, theater_ratio, 240, 0.46).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t280, lycurgan_laws__demographic_trap_reading, theater_ratio, 280, 0.5).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t320, lycurgan_laws__demographic_trap_reading, theater_ratio, 320, 0.52).

% Extraction over time
narrative_ontology:measurement(lycurgan_demographic_trap_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t40, lycurgan_laws__demographic_trap_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t80, lycurgan_laws__demographic_trap_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t120, lycurgan_laws__demographic_trap_reading, base_extractiveness, 120, 0.7).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t160, lycurgan_laws__demographic_trap_reading, base_extractiveness, 160, 0.74).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.78).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t240, lycurgan_laws__demographic_trap_reading, base_extractiveness, 240, 0.81).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t280, lycurgan_laws__demographic_trap_reading, base_extractiveness, 280, 0.83).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t320, lycurgan_laws__demographic_trap_reading, base_extractiveness, 320, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_demographic_trap_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t40, lycurgan_laws__demographic_trap_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t80, lycurgan_laws__demographic_trap_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t120, lycurgan_laws__demographic_trap_reading, suppression_requirement, 120, 0.68).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t160, lycurgan_laws__demographic_trap_reading, suppression_requirement, 160, 0.74).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.8).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t240, lycurgan_laws__demographic_trap_reading, suppression_requirement, 240, 0.85).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t280, lycurgan_laws__demographic_trap_reading, suppression_requirement, 280, 0.87).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t320, lycurgan_laws__demographic_trap_reading, suppression_requirement, 320, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Lycurgan laws' conflates three structurally distinct commitments: a theological claim (divine ordinance demanding fidelity), a historiographical claim (immutability as performed fiction over quiet adaptation), and a constitutional-causal claim (genuine unrevisability producing demographic collapse). Per the epsilon-invariance principle these are three constraints, not one constraint measured three ways: the sacral reading authors negligible extraction by its own lights, the fiction reading authors low binding suppression, and this reading authors high extraction and maximal suppression on the same referent. Family edges run from this file to both siblings. Upstream/downstream ordering is contested within the family: the sacral reading supplies the legitimacy language this reading identifies as cover, while the fiction reading shares this reading's object (the rule's actual force) and contradicts its finding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
