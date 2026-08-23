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
 *   human_readable: Simultaneous Kami-Buddha Veneration (Pragmatic Incoherence Reading)
 *   domain: religious studies/comparative religion/japanese history
 *
 * SUMMARY:
 *   For over a millennium Japanese religious life ran on the simultaneous
 *   veneration of kami and buddhas: shared precincts, fused liturgies, kami
 *   ranked as manifestations of buddhas, temples administering shrines, and
 *   households paying both sides across the lifecycle. This file instantiates
 *   ONE reading of that arrangement — the pragmatic-incoherence reading —
 *   under which the composite never formed a coherent commitment:
 *   practitioners held mutually incompatible teachings at once without
 *   resolution, and the arrangement survived not because anyone reconciled it
 *   but because no authority ever enforced consistency upon it. On this
 *   reading Meiji shinbutsu-bunri (1868) is revelation rather than rupture:
 *   the separation edicts functioned as an enforced consistency probe, and
 *   the composite collapsed almost instantly because nothing load-bearing
 *   stood underneath except the absence of a demand for coherence. KEY AGENTS
 *   (by structural relationship): metropolitan_temple_establishment —
 *   agenda-setter and principal collector (institutional/arbitrage), runs
 *   shrine-temple administration and receives offering shares and funeral
 *   fees; imperial_court_and_bakufu — beneficiary and occasional arbiter
 *   (institutional/mobile), gains a unified ritual order and holds the legal
 *   power that finally dismantles it; hereditary_shrine_priest_lineages —
 *   primary cost-bearing seat (moderate/trapped), officiate subordinated kami
 *   rites with no viable exit; village_worshipper_communities — cost-bearing
 *   participants with offsetting gains (organized/constrained), manage
 *   incompatible teachings by compartmentalization;
 *   shugendo_mountain_ascetic_networks — niche beneficiaries existentially
 *   bound to the fusion (organized/identity_locked); nativist_kokugakusha —
 *   excluded critics (moderate/constrained) documenting the incoherence from
 *   outside the settlement's councils; religious_history_scholars —
 *   analytical observers. Per the epsilon-invariance principle this story
 *   authors epsilon ONLY for the standing arrangement as this reading sees
 *   it; the ontological-fusion and domain-partition readings are separate
 *   stories over the same referent with their own epsilon, linked through
 *   network.affects_constraints. The claim and the metrics are independent
 *   authored facts: the type is claimed from the structure as this reading
 *   finds it (real coordination plus asymmetric extraction plus selective
 *   enforcement), and the metrics describe the arrangement's actual
 *   operation.
 *
 * KEY AGENTS:
 *   - metropolitan_temple_establishment: agenda-setter and principal collector (institutional/arbitrage) — administers the shrine-temple system, teaches the fusion doctrines, receives offering shares and funeral fees
 *   - imperial_court_and_bakufu: beneficiary and occasional arbiter (institutional/mobile) — gains a unified ritual order; holds the legal power ultimately used to dismantle the arrangement
 *   - hereditary_shrine_priest_lineages: primary cost-bearing seat (moderate/trapped) — serve subordinated kami cults with no viable path out
 *   - village_worshipper_communities: cost-bearing participants with offsetting gains (organized/constrained) — compartmentalize the conflicting teachings they receive
 *   - shugendo_mountain_ascetic_networks: niche beneficiaries existentially bound to the fusion (organized/identity_locked)
 *   - nativist_kokugakusha: excluded critics (moderate/constrained) — attack the manifestation doctrines from outside the settlement's governance
 *   - religious_history_scholars: analytical observers (analytical/analytical) — reconstruct the structure from records and post-separation testimony
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.7).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.3).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Kami-Buddha Veneration (Pragmatic Incoherence Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious studies/comparative religion/japanese history").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '44e60cc3-4d62-44d7-ad4f-d58106cd4bc3').
narrative_ontology:cs_kernel_codification('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', distributed).
narrative_ontology:cs_authority_grounding('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', distributed).
narrative_ontology:cs_reading_relation('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', foundational, composite_lacked_resolvable_commitment_core).
narrative_ontology:cs_axiom_status(composite_lacked_resolvable_commitment_core, holdable).
narrative_ontology:cs_axiom_grounding('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', composite_lacked_resolvable_commitment_core, empirically_contingent).
narrative_ontology:cs_axiom('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', secondary, consistency_probes_expose_latent_structure).
narrative_ontology:cs_axiom_status(consistency_probes_expose_latent_structure, holdable).
narrative_ontology:cs_axiom_grounding('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', consistency_probes_expose_latent_structure, instrumental).
narrative_ontology:cs_reference_frame('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', unresolved_composite_accommodation).
narrative_ontology:cs_drift_state('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', meiji_separation_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('44e60cc3-4d62-44d7-ad4f-d58106cd4bc3', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, metropolitan_temple_establishment).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, imperial_court_and_bakufu).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shugendo_mountain_ascetic_networks).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, hereditary_shrine_priest_lineages).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, village_worshipper_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, village_worshipper_communities).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, shugendo_mountain_ascetic_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the great Nara and Kyoto monasteries and, through the shrine-temple system, hundreds of shrines across the provinces. Its doctrinal schools teach that kami are manifestations of buddhas, it appoints shrine administrators, collects shares of shrine offerings and land income, and holds the funeral and memorial rite business for registered households. It defends ritual precedence against rival monasteries and against shrines seeking independence, and its academies produce the fusion theology that frames official teaching.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, metropolitan_temple_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Issues the decrees that confer buddhist ranks on kami, mandate sutra recitation at shrines, and confirm monastery rights over shrine properties. Gains a single workable ritual order spanning the archipelago and the avoidance of open religious war between cult factions. Arbitrates temple-shrine disputes, usually confirming the stronger monastery, and retains throughout the legal power to reorganize the whole settlement, as it ultimately does in 1868.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, imperial_court_and_bakufu, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, imperial_court_and_bakufu, agenda_setter).

% Serve kami cults passed down within particular families, from the great Ise and Izumo houses down to village shrine keepers. Many shrines sit under monastery administration: the monastery takes a share of offerings, controls appointments, and requires buddhist rites for the kami it supervises. Severing the monastery tie would forfeit festival funding, building upkeep, and legal standing; remaining means officiating rites that rank their own deity below another tradition's figures.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, hereditary_shrine_priest_lineages, payer,
    moderate, generational, trapped, national).

% Organize village festivals around kami processions while registering households with local temples for funerals and gravesites under the parish guarantee system. Pay both sides: festival dues to shrines, funeral and memorial fees to temples. Receive teachings from each side that cannot be squared — kami as ancient sovereign powers and as buddhist ordinands — and manage the tension by keeping the two sets of observances in separate compartments rather than reconciling the doctrines.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, village_worshipper_communities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, village_worshipper_communities, beneficiary).

% Build their entire mountain practice on combining kami cults, esoteric buddhist rites, and ascetic discipline; their peaks, lineages, and charters exist only inside the fused settlement. They gain a protected institutional niche and pilgrimage traffic, and answer to monastery hierarchies that license them. Their corporate identity is the combination itself — a settlement that separates the elements dissolves their schools outright, as occurs when the 1868 edicts outlaw the blended orders.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shugendo_mountain_ascetic_networks, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, shugendo_mountain_ascetic_networks, payer).

% Eighteenth- and nineteenth-century scholars arguing that the fusion doctrines are foreign contamination layered over an older native way of honoring kami. They publish philological critiques of the manifestation theories and petition for shrine restoration. They hold no seat in the settlement's governance: their licenses route through channels the fused order controls, and their program becomes state policy only after 1868 removes the old authorities.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, nativist_kokugakusha, excluded,
    moderate, generational, constrained, national).

% Modern historians and comparativists reconstructing the arrangement from court chronicles, land records, liturgies, and post-separation testimony. Neither collect nor pay within it; they assess which descriptions of it hang together.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, religious_history_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, metropolitan_temple_establishment).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates an imported universalist religion with territorial kami cults into one workable ritual economy: a single sacred geography of shared precincts, complementary lifecycle coverage (festivals and this-worldly petitions on the kami side, funerals and afterlife on the buddha side), and a common framework that keeps the two clergy from fighting over the same population.
% TRANSFER_FUNCTION: Moves offering shares, shrine land income, appointment fees, and compulsory funeral and memorial payments from shrine treasuries and lay households to monastery institutions; moves doctrinal deference upward, reframing kami as junior manifestations requiring buddhist rites; and, under the parish guarantee system, moves household affiliation itself to the temples.
% ABSENT_VOICES: Nativist scholars and the heirs of the old kami-side priesthoods (Inbe-lineage traditionalists) would object that the fusion subordinates kami and contaminates native rite; they sat outside the councils where the settlement was negotiated and renewed. Ordinary worshippers were never asked whether the two bodies of teaching cohere — the compartment strategy was theirs, but no seat represented it.
% DISAPPEARANCE_RATIONALE: When the arrangement was forcibly removed in 1868 the world rearranged immediately and violently: separation edicts split shared precincts, the haibutsu kishaku wave destroyed several thousand temples and countless buddhist images, shrine monks were forcibly laicized, the blended mountain orders were banned outright, the parish guarantee system was wound down, and a separate State Shinto was erected on the cleared ground. Arrangements the world depends on do not usually vanish this completely this fast — the speed and totality of the rearrangement is itself evidence about what the arrangement was holding in place.
% FOUNDING_PROBLEM: Integration of buddhist universality with kami sovereignty without sectarian war.
% FOUNDING_PROBLEM_CORROBORATION: See above — corroborated by Nakatomi/Mononobe resistance records, Inbe petitions, nativist philology, and Kuroda-line scholarship; the liveness verdict is disputed between the doctrinal readings (solved by fusion or by partition) and this reading (suspended, never solved).
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.70 at interval end) because the settlement's benefits and burdens are sharply asymmetric: monasteries collect offering shares, appointment control, and a compulsory funeral franchise (parish guarantee), while shrine lineages surrender autonomy and revenue and households carry the fees of both cults plus the unpriced labor of holding incompatible teachings apart. Suppression is authored at 0.30 as the arrangement's characteristic operative level across its working life — structural exit-binding (shrine dependence on monastery administration, household dependence on funeral franchises) with comparatively little ideological policing. The suppression_requirement series deliberately diverges from that scalar and is authored because enforcement-capacity change IS this story's traced dynamic: enforcement built through the medieval period (court mandates, armed monastery politics, precedence litigation), matured into administration under Tokugava pacification and the parish guarantee registry, decayed through the late eighteenth and early nineteenth centuries as bakufu capacity eroded, and collapsed to near zero at the 1868 boundary when the state reversed course and began enforcing separation instead. The terminal scalar-measurement divergence is a fact about the arrangement's end, not an inconsistency: the scalar describes the standing arrangement's mode of operation; the last series point records the moment its enforcement evaporated. Accessibility_collapse is low-moderate (0.38) because alternatives — separate veneration, single-tradition practice — never collapsed; they remained conceivable throughout and were realized almost overnight once the state demanded coherence. Resistance is moderate-high (0.52): Inbe petitions, Yoshida-school inversion attempts, sustained nativist philology, and recurring shrine-monastery conflicts. Theater_ratio climbs from 0.10 to 0.48 as living doctrine (serious medieval theological elaboration) hardens into customary maintenance that fewer participants could defend on its own terms. All three series run on one shared eight-point time grid; every tracked metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by design. From the metropolitan_temple_establishment seat the settlement is a legitimate order it built, staffed, and defended — integration as achievement. From the hereditary_shrine_priest_lineages seat the same structure is subordination without exit: officiating rites that rank their own deity junior to another tradition, funded and supervised by rivals for their office. Village_worshipper_communities occupy a third position: genuine service received from both sides, genuine fees paid to both sides, and a private resolution (compartments) that the official theology never ratified. The excluded nativist_kokugakusha seat perceives corruption where the agenda-setter seat perceives synthesis. Identity-lock dynamics concentrate in the shugendo networks — institutional-professional identity fusion: their schools, charters, and sacred geography exist only as combinations, so the arrangement's removal is their dissolution, which is exactly what the 1868 edicts inflicted. Were that identity frame to break internally rather than by edict, the fusion would lose its most committed constituency decades earlier. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows the declared flows. metropolitan_temple_establishment sits near the beneficiary pole (receives the transfers, writes the rules, holds arbitrage-grade options). imperial_court_and_bakufu sits near it too — order, legitimacy, and the avoidance of religious war flow to it, and its mobility means the arrangement subsidizes rather than binds it. hereditary_shrine_priest_lineages sit near the full-target pole: trapped exit (severing monastery ties forfeits funding, upkeep, and standing) places them at maximum effective exposure. village_worshipper_communities derive a high-but-tempered target value from their dual declaration: heavy payment streams and cognitive burden, offset by real services received. shugendo networks derive low directionality (the arrangement creates their niche) while their identity_lock records the reverse face of that subsidy — total existential exposure to its removal. nativist_kokugakusha are excluded rather than positioned: per the R3 ruling their absence feeds the consensus-provenance check, not any classification override. The selective-enforcement pattern is central to this reading's arithmetic: enforcement pressed on the hierarchy (precedence, appointments, revenue) while doctrinal consistency — the one demand that would have exposed the incoherence — was never pressed at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is tripartite. Against a rope reading: ignoring the shrine subordination, the funeral franchise, and the parish guarantee would mistake an enforced asymmetry for frictionless coordination — the beneficiaries are identifiable and the collectors are concentrated. Against a snare reading: the coordination function is real and substantial (an imported universal religion was absorbed without religious war, with complete lifecycle coverage), so the coordination story is not mere cover; what makes the arrangement hybrid rather than predatory is that the same structure that delivers integration also transfers the surplus. Against a piton reading: although theater_ratio climbs late and enforcement decays before 1868, the arrangement is not inertial residue — a concentrated collector existed throughout (receipt surface names it), and the pre-1868 failure to fix it reflects prohibitive fixing cost, not absent benefit. The receipt surface records the capture honestly: gains accrue to the metropolitan temple establishment, and for any holder of power to fix or remove the arrangement short of revolution was prohibitive — which is why revelation had to wait for a state willing to pay the demolition bill. The founding-problem interview returns status contested against verdict world_rearranges: the parties dispute whether the integration problem was ever solved, while the overnight rearrangement proves the arrangement was load-bearing for something — on this reading, for the suspension of the question itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the standing pre-Meiji arrangement better described by this pragmatic-incoherence reading, or by a sibling reading (ontological fusion or domain partition) under which the composite coheres?',
    'Historical-theological analysis of whether practitioners treated the kami-buddha relation as settled: partition evidence would be consistently separate ritual calendars and jurisdictions with no cross-ranking claims; fusion evidence would be explicit identity doctrines taught, licensed, and acted upon across regions; incoherence evidence is the documented coexistence of mutually incompatible rank-claims with no adjudicating authority.',
    'If a sibling reading is correct, epsilon drops sharply (the arrangement coordinates without suppressed contradiction) and the computed type moves toward rope; if this reading is correct, the authored high-extraction profile stands and Meiji separation reads as revelation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the simultaneous-veneration kernel the historical evidence supports.').

omega_variable(
    subjective_dissonance_cost,
    'Did lay practitioners experience the contradictory teachings as a cost while the arrangement stood, or is the dissonance a retrospective diagnosis imposed by analysts and by the nativist critics?',
    'Close reading of diaries, sermon records, village headman documents, and post-1868 testimonials for signs that the contradiction was felt (complaint, anxiety, relief at separation) versus handled invisibly (unremarked compartmentalization).',
    'If the dissonance was never subjectively borne, the cognitive component of epsilon is analyst-imputed, epsilon falls materially, and the arrangement looks closer to benign dual practice; if it was felt, the suppressed-contradiction extraction is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subjective_dissonance_cost, empirical, 'Whether the incoherence extracted real cognitive cost from practitioners or only from hindsight.').

omega_variable(
    meiji_revelation_vs_imposition,
    'Did the 1868 separation reveal latent incoherence, or impose rupture on a functioning whole?',
    'Analyze the speed and voluntariness of collapse, the geographic and social distribution of haibutsu kishaku violence (state-driven versus locally spontaneous), and continuity of practice after separation; rapid voluntary abandonment and locally spontaneous iconoclasm favor revelation, uniform coercion-compliance favors imposition.',
    'This is the reading''s central evidentiary wager: revelation supports the authored profile; imposition would relocate the extraction story to Meiji policy itself and force re-authoring of epsilon and of the persistence mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_revelation_vs_imposition, conceptual, 'What the Meiji separation event measures about the pre-Meiji arrangement.').

omega_variable(
    enforcement_selectivity_scope,
    'Does ''sustained by lack of enforcement pressure'' accurately describe the whole arrangement, or only the lay-doctrinal layer while the institutional hierarchy was actively enforced?',
    'Compare the enforcement record across layers: court mandates, monastery litigation, and armed precedence politics (hierarchy layer) versus the total absence of any doctrinal-consistency tribunal or orthodoxy mechanism (doctrine layer).',
    'If enforcement was pervasive across layers, suppression rises above the authored 0.30 and the computed classification leans snare-ward; if enforcement was minimal everywhere, the tangled-rope premise of active enforcement weakens toward rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_scope, empirical, 'Scope of the enforcement vacuum this reading attributes the persistence to.').

omega_variable(
    counterfactual_persistence_without_meiji,
    'Absent the Meiji shock, would the arrangement have persisted indefinitely, or was it already decaying under nativist spread, bakufu fiscal decline, and shrine independence litigation?',
    'Trace late-Edo indicators: growth of nativist networks and licensing, frequency of shrine-monastery property suits, bakufu enforcement capacity, and clerical attrition in combined institutions.',
    'Genuine decay before 1868 strengthens the reading (the arrangement was brittle because incoherent) and raises late-interval theater_ratio credulity; indefinite persistence absent shock would suggest a stabler composite than this reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_persistence_without_meiji, empirical, 'Whether the terminal collapse indicates structural brittleness or merely defeat by a stronger power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 780, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t780, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 780, 0.1).
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t1000, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t1200, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t1400, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1400, 0.28).
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t1600, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1600, 0.33).
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t1750, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1750, 0.38).
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t1830, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1830, 0.44).
narrative_ontology:measurement(sv_pragmatic_incoherence_tr_t1868, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1868, 0.48).

% Extraction over time
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t780, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 780, 0.28).
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t1000, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t1200, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1200, 0.5).
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t1400, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1400, 0.57).
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t1600, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t1750, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1750, 0.65).
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t1830, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1830, 0.67).
narrative_ontology:measurement(sv_pragmatic_incoherence_be_t1868, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1868, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t780, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 780, 0.18).
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t1000, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1000, 0.26).
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t1200, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1200, 0.34).
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t1400, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1400, 0.36).
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t1600, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1600, 0.32).
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t1750, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1750, 0.27).
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t1830, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1830, 0.21).
narrative_ontology:measurement(sv_pragmatic_incoherence_su_t1868, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1868, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'shinbutsu-shugo / simultaneous veneration' covers three structurally distinct claims over one referent. ontological_fusion_reading (doctrine true; low epsilon), domain_partition_reading (clean functional specialization; low-to-moderate epsilon), and this file, pragmatic_incoherence_reading (composite never coherent; high epsilon from suppressed contradiction and enforced subordination). The doctrinal readings are upstream: their claims are cited as evidence that coherence existed, so this reading structurally responds to and contests their warrant. Each member links the others through affects_constraints; each authors its own epsilon, beneficiary/victim weights, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
