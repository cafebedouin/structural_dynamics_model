% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji-Suijaku Doctrine: Kami as Phenomenal Traces of Buddha-Grounds (Monist Reading)
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   The honji-suijaku doctrine — systematized from roughly the ninth century
 *   onward by Tendai and Shingon scholiasts and dominant in Japan until the
 *   Meiji separation edicts of 1868 — declares kami and buddhas ontologically
 *   identical: kami are phenomenal traces (suijaku) of the original Buddhist
 *   ground (honji). The framework ran on real institutional machinery:
 *   combined shrine-temple complexes (jinguji), monk-officiated shrine rites,
 *   exegetical schools that assigned each kami a buddha-ground, and
 *   court/bakufu sanction. It solved a genuine millennium-old integration
 *   problem while imposing a strict hierarchy: buddhas prior, kami
 *   derivative, shrine constituencies defined from outside their own
 *   tradition. The arc runs from early reciprocal assimilation (kami as
 *   dharma-protectors) through hardening hierarchy (Ryobu Shinto, Sanno
 *   worship, mandalic mapping of shrines) through mounting nativist
 *   contestation (Kitabatake Chikafusa, Yoshida Kanetomo's inverted theology,
 *   kokugaku philology) to abrupt state abolition. The claim/metric gap is
 *   deliberate: claimed_type is stated from structural analysis (genuine
 *   coordination function + asymmetric extraction + active enforcement),
 *   while the metrics describe the arrangement's actual operation
 *   independently. The epsilon referent is the standing honji-suijaku
 *   arrangement itself, assessed through this monist reading's own lights —
 *   under which subordination reads partly as soteriological elevation ('kami
 *   also desire enlightenment'), a discount carried explicitly by the
 *   soteriology_vs_subordination omega rather than silently baked into the
 *   scalar. Interval mapping: t=0 is approximately 850 CE (early
 *   systematization), t=1000 approximately 1865 CE (eve of shinbutsu bunri);
 *   the post-interval collapse is analyzed in commentary, not measured, so
 *   the terminal values remain the standing arrangement's.
 *
 * KEY AGENTS:
 *   - buddhist_monastic_establishments: Primary agenda-setter and principal beneficiary (institutional power, effectively unrestricted exit) — authors the doctrine, administers shrine-temple complexes, receives the upstream flow of offerings and fees
 *   - court_aristocracy: Secondary beneficiary (powerful, bound to the settlement) — purchases ideological coherence and institutional ranking
 *   - hereditary_shrine_priesthoods: Principal elite target (moderate power, constrained exit) — bears the ontological subordination of their own deities with partial prestige offset
 *   - village_shrine_communities: Mass target (powerless, immovable) — local cults absorbed into the temple orbit
 *   - commoner_worshippers: Near-symmetric participants (powerless, embedded) — integrated ritual life, diffuse costs
 *   - kami_primacy_advocates: Excluded voice (moderate power, marginal circulation) — nativist critics outside the doctrinal venues
 *   - religious_historians: Analytical observer — sees the full structure across the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.62).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.72).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji-Suijaku Doctrine: Kami as Phenomenal Traces of Buddha-Grounds (Monist Reading)").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '3045268c-b9ba-4f3b-8e60-6d9e160701e6').
narrative_ontology:cs_kernel_codification('3045268c-b9ba-4f3b-8e60-6d9e160701e6', formalized).
narrative_ontology:cs_authority_grounding('3045268c-b9ba-4f3b-8e60-6d9e160701e6', lineage).
narrative_ontology:cs_interpretation_layer_present('3045268c-b9ba-4f3b-8e60-6d9e160701e6').
narrative_ontology:cs_reading_relation('3045268c-b9ba-4f3b-8e60-6d9e160701e6', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('3045268c-b9ba-4f3b-8e60-6d9e160701e6', kami_buddha_ontology__incoherent_bundle, forecloses).
narrative_ontology:cs_axiom('3045268c-b9ba-4f3b-8e60-6d9e160701e6', foundational, kami_are_conditional_manifestations).
narrative_ontology:cs_axiom_status(kami_are_conditional_manifestations, overridden).
narrative_ontology:cs_axiom_grounding('3045268c-b9ba-4f3b-8e60-6d9e160701e6', kami_are_conditional_manifestations, theological).
narrative_ontology:cs_axiom('3045268c-b9ba-4f3b-8e60-6d9e160701e6', secondary, ritual_precedence_follows_ontological_priority).
narrative_ontology:cs_axiom_status(ritual_precedence_follows_ontological_priority, holdable).
narrative_ontology:cs_axiom_grounding('3045268c-b9ba-4f3b-8e60-6d9e160701e6', ritual_precedence_follows_ontological_priority, conventional).
narrative_ontology:cs_reference_frame('3045268c-b9ba-4f3b-8e60-6d9e160701e6', buddhaprior_single_ground_cosmology).
narrative_ontology:cs_drift_state('3045268c-b9ba-4f3b-8e60-6d9e160701e6', late_edo_kokugaku_ascendancy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3045268c-b9ba-4f3b-8e60-6d9e160701e6', '2026-06-13T09:24:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, court_aristocracy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, commoner_worshippers).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, hereditary_shrine_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, village_shrine_communities).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, kami_primacy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, hereditary_shrine_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, commoner_worshippers).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, honji_suijaku_hierarchical_monism).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, dharmakaya_manifestation_theory).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, kenmitsu_institutional_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great temple networks and their scholastic academies (Tendai, Shingon, and the medieval kenmitsu orders) articulate the doctrinal framework, staff shrine rites with monks, administer combined shrine-temple complexes, and train the exegetes who interpret oracles and prodigies within the framework. Offerings, land income, and liturgical fees from shrine constituencies flow into temple economies, and the schools control which texts and interpretations count as authoritative. Their position is effectively unrestricted: they wrote the framework and can reinterpret or extend it as conditions change.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_monastic_establishments, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The court and great houses adopt the framework as administrative theology: one cosmology legitimates joint patronage of shrines and temples, ranks institutions against one another, and channels disputes into a single doctrinal vocabulary. They fund both sides and preside over rank assignments; their gain is governable plurality rather than direct revenue. Because their settlement depends on continued acceptance by both clerical and shrine elites, they cannot discard the framework without renegotiating the entire arrangement.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, court_aristocracy, beneficiary,
    powerful, generational, constrained, national).

% Hereditary shrine lineages (the Nakatomi/Onakatomi, the Imibe, and provincial priest houses) conduct the rites their families have held for generations, but under the framework their deities are defined from outside: a kami's identity, rank, and salvific capacity are assigned by Buddhist exegesis, and major shrines host monk-performed rites and temple buildings on their own grounds. They gain scriptural prestige, imperial patronage channeled through Buddhist forms, and answers to death-pollution questions their own tradition left open; they lose final interpretive authority over their own gods. Leaving the framework would mean forfeiting the prestige and patronage that now reach shrines through it.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, hereditary_shrine_priesthoods, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, hereditary_shrine_priesthoods, beneficiary).

% Local cult communities maintain village shrines, festivals, and priestly employment. As the framework spreads, their sacred sites are identified as manifestations of particular buddhas, their festivals acquire Buddhist liturgical layers, and disputes over land and dues are adjudicated by temple-backed authorities. Their ties are local and immovable — the mountain, grove, or spring is where it is — so they absorb the redefinition where they stand.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, village_shrine_communities, payer,
    powerless, generational, trapped, local).

% Ordinary worshippers move between shrines and temples as occasions demand: births and purities at shrines, funerals and memorial services under Buddhist rites, festivals drawing on both. The framework gives them a single usable religious world without requiring them to master its ontology. They carry the costs diffusely — duplicate fees, temple dues, festival levies — and their participation is what makes the combined shrine-temple economy viable.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, commoner_worshippers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, commoner_worshippers, payer).

% Nativist literati and some shrine-aligned scholars argue that the kami are complete in themselves, prior to and independent of the buddhas, and that deriving them from foreign grounds diminishes both. They produce critiques — court genealogical tracts, inverted-theology treatises, nativist philology — but sit outside the temple academies and court bureaus where authoritative interpretation is manufactured, so their arguments circulate at the margins until political conditions change.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kami_primacy_advocates, excluded,
    moderate, generational, constrained, national).

% Modern historians of Japanese religion reconstruct the framework's composition, diffusion, and abolition from chronicles, shrine and temple archives, and material remains. They hold no position inside the arrangement and can compare its operation across regions and centuries, including phases its participants could not see.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the integration problem created by Buddhism's arrival: provides a single ontological framework under which indigenous kami cults and imported Buddhist institutions can share sacred sites, ritual calendars, soteriology, and legitimacy — shrine-temple complexes, monk-officiated shrine rites, and kami inclusion in Buddhist salvation become coordinable instead of competitive.
% TRANSFER_FUNCTION: Moves doctrinal authority and institutional precedence upward from kami cults to Buddhist establishments; moves material support (offerings, land income, liturgical fees, labor for complex upkeep) from shrine constituencies and court patrons toward temple networks; moves salvific legitimacy downward from buddhas to kami, conditionally on the kami's status as traces.
% ABSENT_VOICES: Kami-primacy advocates — nativist literati such as Kitabatake Chikafusa, Yoshida Kanetomo, and later Motoori Norinaga — held that the kami are self-complete and prior, but stood outside the scholastic venues (temple academies, court doctrinal bureaus) where the framework was articulated; hereditary shrine lineages who resented subordination lacked independent doctrinal standing and negotiated privately rather than entering the record as dissent.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, shrine-temple complexes would split into competing institutions, Buddhist establishments would lose shrine jurisdictions and their revenue, kami worship would reorganize around independent priestly lineages, and the soteriological question the framework answered (can kami save? do kami need buddhas?) would reopen as live controversy. The Meiji separation edicts of 1868 approximated this experiment: within years thousands of shrine-temples were dissolved, tens of thousands of temples were destroyed in the ensuing haibutsu kishaku, and Shinto reorganized as an independent national cult — large-scale rearrangement confirming that surrounding arrangements depended on the framework.
% FOUNDING_PROBLEM: The arrival and court adoption of Buddhism (6th century onward) posed an unresolved question: what is the relation between the imported buddhas and the indigenous kami who had guarded the realm? Early answers oscillated between hostility (kami as wrathful guardians of purity offended by foreign rites, as in the Soga-Mononobe conflict) and loose assimilation (kami as protectors of the dharma). Honji-suijaku was systematized to solve this: one cosmos, buddhas as original ground, kami as manifestations — ending the rivalry by subsumption.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by: the Nihon Shoki's record of the Soga-Mononobe conflict and the Usa Hachiman oracle's approval of the Todaiji Daibutsu (contemporary court chronicles, not temple self-description); archaeological and documentary evidence of shrine-temple complex construction; and modern religious historiography — notably Kuroda Toshio's kenmitsu-taisei analysis — which reconstructs both the integration problem and the doctrine's constructed character from the documentary record. No neutral source attests that the problem is simply dead: Meiji state pronouncements assert resolution, but they were the abolishing party, not disinterested attestation, while lived dual practice continues without ontological settlement.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the arrangement moved real resources uphill (offerings, land income, fees, jurisdiction) and imposed a deep identity-level cost — shrine constituencies lost final interpretive authority over their own gods — but the reading's own lights discount part of this as the price of liberation, and both sides drew genuine benefit for centuries; the scalar sits below a purely external description would give. Suppression 0.72: persistence required continuous machinery — monk-officiants installed at shrines, exegetical monopolies, court ranking, bakufu-era temple registration — to hold the hierarchy against persistent practical separation and nativist critique; suppression here is structural-institutional (external barriers, not internalized cognition) and is authored as a raw unscaled property, with only extractiveness scaled by directionality and scope downstream. Theater ratio 0.45: early systematization was functional, but mature-phase scholasticism grew baroque (shrines mapped onto Womb and Diamond mandalas, esoteric etymologies of kami names), and by the late Edo period a rising share of maintenance was performative defense against kokugaku rather than integration work. Accessibility collapse 0.60: within elite discourse alternatives largely closed — Yoshida Kanetomo had to invent a new revelation to invert the hierarchy rather than argue within it — but folk practice retained dual patterns throughout, so collapse is far short of natural-law levels. Resistance 0.65: sustained across the whole interval — early sectarian reluctance to admit kami, regional separation practices, the Jinno Shotoki's kami-primacy argument, Yoshida's inversion, kokugaku's assault, culminating in state abolition. All three series run on one shared seven-point grid so every metric is authored at every examined time point; the trajectories are wave-shaped rather than cyclical (Nanbokucho critiques, Muromachi systematization, Edo nativism), driven by successive contestation fronts rather than an oscillation mechanism. Axiom status note: the foundational axiom is marked overridden because the framework was formally superseded in its own legal history by the 1868 Dajokan separation edicts — an external-legal repudiation rather than internal doctrinal collapse — while the secondary precedence axiom remains internally coherent and privately holdable.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the scholiast seat the framework is cosmic ordering it articulates and extends — coordination it built, with discipline as error-correction. From the hereditary priesthood seat the same structure is dispossession of interpretive authority over its own deities, softened by prestige returns. From the village seat it is an immovable redefinition of sacred ground. From the commoner seat it is nearly invisible — a seamless ritual world. From the historian seat it is a constructed system whose enforcement-dependence is legible in the speed of its 1868 collapse. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Buddhist monastic establishments sit nearest the beneficiary end (they collect the upstream flow and control the rules, with effectively unrestricted exit). The court sits low-but-not-floor: it gains coherence while paying patronage costs. Commoner worshippers sit near symmetric — genuine integration benefit, diffuse duplicate costs. Hereditary priesthoods sit well toward the target end: they bear subordination with partial offset, and their exit is constrained by the patronage and prestige now routed through the framework. Village shrine communities sit nearer the full-target end still: immovable sites, no offset, no exit. Kami-primacy advocates are the suppressed margin — the framework's enforcement falls on them directly. No directionality overrides are authored: the declarations plus exit options already yield the correct ordering, and the dual-positioned agents (priesthoods, commoners) are handled by their paired role declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating two cultic systems into one cosmos — was progressively accomplished: by the late medieval period dual practice was habitual culture, no longer requiring doctrinal production to sustain it. The mandate thus outlived its function while the precedence and revenue flows persisted, which is why mandatrophy is declared resolved in substance. Yet the arrangement never drifted into inertial performance, because its beneficiaries were concentrated and its enforcement strong: it remained a living hybrid until external abolition, with the rising theater_ratio marking the final century's slide toward performative maintenance. The classification prevents mislabeling in both directions: calling this a pure rope erases the documented shrine-side costs and the enforcement machinery; calling it a pure snare erases the genuine integration achievement both sides drew on for a thousand years and the sincere soteriological uptake the shrine-side record shows. The tangled-rope reading keeps both halves on the table and routes the residual question — how much uptake was sincere — to the omega layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading — honji_suijaku_monism — of the contested kernel kami_buddha_ontology. Would adopting a sibling reading (domain_partition or incoherent_bundle) change the constraint''s structure?',
    'Comparative institutional analysis of which reading governed practice in which periods and regions: if shrine and temple practice consistently treated kami and buddhas as functionally separate, the domain_partition reading better describes the standing arrangement; if the record shows sustained contradiction held together only by institutional interest, the incoherent_bundle reading dissolves this single-constraint frame.',
    'Under domain_partition the ontological-subordination victims disappear and epsilon falls sharply (separate domains, no hierarchy to enforce); under incoherent_bundle this story splits into multiple weaker constraints and the tangled_rope classification loses its single referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Kernel-membership commitment: this constraint is the monist reading; siblings instantiate different constraints with different victim sets.').

omega_variable(
    ontological_priority_disagreement_location,
    'Where exactly do the readings disagree — is the dispute located in the priority premise (buddhas prior to kami, as this reading holds; neither prior, per domain_partition; or the coherence question itself malformed, per incoherent_bundle)?',
    'Close reading of the scholastic corpus against practice records to locate which structural element each reading actually contests: ontological identity, priority ordering, or the independence of kami.',
    'Confirming priority as the locus validates the forecloses relations authored here (identity contradicts distinctness; coherent monism contradicts the no-coherent-kernel claim); locating the dispute elsewhere would downgrade those relations to influence or coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_priority_disagreement_location, conceptual, 'The disagreement is located in the buddha-priority premise; sibling readings deny it or deny the kernel''s coherence.').

omega_variable(
    soteriology_vs_subordination,
    'How much of the shrine-side cost was experienced as imposed subordination versus embraced soteriology — kami ''desiring enlightenment'' and receiving Buddhist liberation?',
    'Shrine-side documents (oracle pronouncements, engi narratives, petitionary records) compared with temple-side polemic: sincere requests for sutra readings and goma rites indicate internalized acceptance; coerced subscriptions and resentful petitions indicate extraction.',
    'Higher sincere acceptance lowers effective extraction for the shrine seats and pushes the computed type toward rope; higher coercion confirms the tangled_rope asymmetry and raises chi for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soteriology_vs_subordination, empirical, 'Whether the hierarchy''s costs were internalized as liberation or borne as subordination.').

omega_variable(
    enforcement_dependence_counterfactual,
    'Would the framework have persisted without state and temple enforcement, or was it already decaying under nativist pressure before the 1868 edicts?',
    'Counterfactual analysis of late-Edo trajectories: kokugaku circulation, domain-level Shinto bureaucratization, shrine-temple dispute rates, and the speed and completeness of the post-1868 collapse (haibutsu kishaku) as evidence of latent fragility.',
    'If enforcement alone held it, the tangled_rope classification with high suppression stands; if it was already hollow, the final decades were drifting toward inertial performance and the theater_ratio trajectory understates late-stage decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependence_counterfactual, empirical, 'Enforcement-dependence versus latent decay of the framework before abolition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(kami_tr_t0, observed).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 150, 0.14).
narrative_ontology:measurement_basis(kami_tr_t150, observed).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 300, 0.2).
narrative_ontology:measurement_basis(kami_tr_t300, observed).
narrative_ontology:measurement(kami_tr_t500, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 500, 0.27).
narrative_ontology:measurement_basis(kami_tr_t500, observed).
narrative_ontology:measurement(kami_tr_t700, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 700, 0.34).
narrative_ontology:measurement_basis(kami_tr_t700, observed).
narrative_ontology:measurement(kami_tr_t850, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 850, 0.4).
narrative_ontology:measurement_basis(kami_tr_t850, observed).
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1000, 0.45).
narrative_ontology:measurement_basis(kami_tr_t1000, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(kami_be_t0, observed).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 150, 0.41).
narrative_ontology:measurement_basis(kami_be_t150, observed).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 300, 0.52).
narrative_ontology:measurement_basis(kami_be_t300, observed).
narrative_ontology:measurement(kami_be_t500, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 500, 0.58).
narrative_ontology:measurement_basis(kami_be_t500, observed).
narrative_ontology:measurement(kami_be_t700, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 700, 0.61).
narrative_ontology:measurement_basis(kami_be_t700, observed).
narrative_ontology:measurement(kami_be_t850, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 850, 0.62).
narrative_ontology:measurement_basis(kami_be_t850, observed).
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement_basis(kami_be_t1000, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(kami_su_t0, observed).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 150, 0.38).
narrative_ontology:measurement_basis(kami_su_t150, observed).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 300, 0.52).
narrative_ontology:measurement_basis(kami_su_t300, observed).
narrative_ontology:measurement(kami_su_t500, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 500, 0.6).
narrative_ontology:measurement_basis(kami_su_t500, observed).
narrative_ontology:measurement(kami_su_t700, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 700, 0.66).
narrative_ontology:measurement_basis(kami_su_t700, observed).
narrative_ontology:measurement(kami_su_t850, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 850, 0.7).
narrative_ontology:measurement_basis(kami_su_t850, observed).
narrative_ontology:measurement(kami_su_t1000, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1000, 0.72).
narrative_ontology:measurement_basis(kami_su_t1000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, incoherent_bundle).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu-shugo' (kami-buddha fusion) decomposes, per the epsilon-invariance principle, into three structurally distinct readings of the kernel kami_buddha_ontology. This story is the honji_suijaku_monism member: single ground, buddha-priority, kami as dependent traces — its epsilon indexes the subordination costs a hierarchy imposes. The domain_partition sibling (ontological distinctness, separate functional domains) carries a different victim set — no ontological subordination, but boundary-policing costs instead — and hence a different epsilon. The incoherent_bundle sibling denies that a single coherent constraint exists at all (fusion and separation held together institutionally) and therefore cannot share this story's epsilon or stakeholder surface. Direction of influence: this monist reading's systematization supplied the vocabulary that the nativist and separatist movements later defined themselves against, so this story links to both siblings as the family's hierarchical pole.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
