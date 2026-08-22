% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Syncretic-Fusion Order (Kami-Buddha Ontological Unity)
 *   domain: religious/historical/commitment-systems
 *
 * SUMMARY:
 *   This story instantiates the syncretic-fusion reading of the shinbutsu
 *   ontological-substrate kernel: the claim that kami and buddhas are
 *   ontologically one, with honji suijaku stating a metaphysical truth rather
 *   than a mere administrative compromise. The standing arrangement under
 *   measurement is the fused order itself — the shrine-temple compounds, the
 *   esoteric doctrinal apparatus, the parishioner system — assessed by this
 *   reading's own lights: because the reading holds the ontology to be real,
 *   it rates the institutional capture of that ontology (monastic centers
 *   converting a truth-claim into interpretive supremacy over shrine lineages
 *   and households) as grave distortion rather than harmless accommodation.
 *   Sibling readings (domain-partition, incoherent-bundle) are separate
 *   constraint files linked through network.affects_constraints; their
 *   epsilon values differ because they measure different structures, not
 *   because this constraint's epsilon varies by observer. KEY AGENTS (by
 *   structural relationship): - esoteric_monastic_centers: agenda-setting
 *   beneficiary (institutional/arbitrage) — formulates the identifications,
 *   collects the routed income - imperial_court: beneficiary
 *   (institutional/constrained) — receives unified ritual legitimacy -
 *   warrior_regimes: dual-positioned beneficiary-payer
 *   (institutional/constrained) — patronage and taxation run in both
 *   directions - hereditary_shrine_lineages: primary payer
 *   (moderate/identity_locked) — ancestral offices subordinated to the fused
 *   framework - kami_exclusivist_traditions: excluded voice
 *   (moderate/trapped) — holds the rival ontology without a sanctioned
 *   platform - rural_parish_households: payer with incidental benefit
 *   (powerless/trapped) — bears duplicate obligations -
 *   religious_studies_historians: analytical observer (analytical/analytical)
 *   — reconstructs the full structure from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku Syncretic-Fusion Order (Kami-Buddha Ontological Unity)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/historical/commitment-systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '09f87825-84c3-4a5a-ace6-651493d14817').
narrative_ontology:cs_kernel_codification('09f87825-84c3-4a5a-ace6-651493d14817', distributed).
narrative_ontology:cs_authority_grounding('09f87825-84c3-4a5a-ace6-651493d14817', lineage).
narrative_ontology:cs_interpretation_layer_present('09f87825-84c3-4a5a-ace6-651493d14817').
narrative_ontology:cs_reading_relation('09f87825-84c3-4a5a-ace6-651493d14817', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('09f87825-84c3-4a5a-ace6-651493d14817', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('09f87825-84c3-4a5a-ace6-651493d14817', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('09f87825-84c3-4a5a-ace6-651493d14817', kami_buddha_ontological_identity, theological).
narrative_ontology:cs_axiom('09f87825-84c3-4a5a-ace6-651493d14817', foundational, esoteric_transmission_discloses_unity).
narrative_ontology:cs_axiom_status(esoteric_transmission_discloses_unity, holdable).
narrative_ontology:cs_axiom_grounding('09f87825-84c3-4a5a-ace6-651493d14817', esoteric_transmission_discloses_unity, theological).
narrative_ontology:cs_reference_frame('09f87825-84c3-4a5a-ace6-651493d14817', buddha_ground_kami_trace_hierarchy).
narrative_ontology:cs_drift_state('09f87825-84c3-4a5a-ace6-651493d14817', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('09f87825-84c3-4a5a-ace6-651493d14817', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, esoteric_monastic_centers).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_regimes).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, hereditary_shrine_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_exclusivist_traditions).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, rural_parish_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, rural_parish_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_regimes).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, ryobu_sanno_syncretic_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the great mountain complexes (Hiei, Koya) and the shrine-temple compounds attached to major shrines. They formulate the doctrinal identifications binding particular kami to particular buddhas, train the clergy who perform combined rites, supervise or absorb shrine clergy, and receive land income, offerings, and ritual fees routed through the shrine-temple compounds. When a shrine resists incorporation they can call on court and warrior patrons and, in the medieval centuries, on their own armed retainers. They can and do revise local formulations of the framework freely; the framework itself is theirs.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, esoteric_monastic_centers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, esoteric_monastic_centers, beneficiary).

% Sponsors and legitimates the unified cultic order: court rank for kami, Buddhist state-protective rites, an integrated ritual calendar. Receives ideological coherence — a single ritual vocabulary covering both the foreign religion and the land's native deities — and the loyalty that flows from mediating both. Bears costs when monastic centers leverage court sanction to expand their estate holdings. Cannot step outside the framework without dissolving its own claim to stand between heaven and the land.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).

% Patronize the great complexes with land grants and protection charters, receiving legitimation from both sides of the fused order — shrines sanctify their rule locally, temples bless it cosmically. They also tax monastic estates and periodically burn recalcitrant monasteries, so their position carries real costs alongside the gains. Their backing is selective: they uphold the framework when it serves order and overlook its demands when those fall on their allies.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_regimes, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_regimes, payer).

% Hereditary priestly families whose offices, lands, and family identities are bound to specific shrines. Under the fused order their deities are re-described as manifestations of buddhas, their rites are supplemented or supervised by Buddhist clergy, and their sons may take Buddhist ordination as a career path. They receive protection, festival traffic, and shared legitimacy; they lose sole interpretive authority over their own gods. Leaving would mean abandoning an ancestral office that constitutes the family's name and livelihood.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, hereditary_shrine_lineages, payer,
    moderate, generational, identity_locked, regional).

% Scholars and priestly movements — the Ise tradition, the Yoshida tradition, later nativist learning — who hold that the kami are original and prior, not traces of anything else. For most of the interval they lack a sanctioned platform: their writings circulate narrowly, their institutional bids are absorbed or co-opted (the Yoshida inversion succeeds partly by adopting the fusion's own genealogical method), and open anti-Buddhist agitation invites censure — nativist networks were purged in the 1840s. They are inside the religious world but outside the doctrinal conversation that sets its terms.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_exclusivist_traditions, excluded,
    moderate, biographical, trapped, national).

% Village households registered with a local temple under the parishioner system, owing temple dues and funeral-and-grave obligations while also funding shrine festivals and harvest rites. They receive an integrated ritual life — birth, death, festival, and field blessing handled in one landscape — but carry duplicate obligations to both sides of the fused order. Moving means losing registration, burial rights, and community standing.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, rural_parish_households, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, rural_parish_households, beneficiary).

% Modern scholars reconstructing the arrangement from doctrinal texts, institutional archives, and separation-era records. They read every seat's surviving documents against the others, test the monastic-center account against shrine-lineage and nativist sources, and produced the twentieth-century reframings that participants inside the arrangement could not see from within.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, religious_studies_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, esoteric_monastic_centers).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates an imported universal religion with the land's indigenous deity cult into one operable ritual order: it settles the theological collision (are the kami real? are the buddhas local?), lets one festival calendar, one sacred geography, and one clerical career structure serve both traditions, and gives the state a single legitimating register.
% TRANSFER_FUNCTION: Moves interpretive authority over the kami from hereditary shrine lineages to monastic scholastic centers; moves land income, offerings, and ritual fees through shrine-temple compounds toward those centers; moves funerary and memorial obligations and their fees from households to temples; moves legitimation downward from court sanction through both cults to local communities.
% ABSENT_VOICES: Kami-exclusivist traditions would object that the fusion demotes the land's own gods to appendages of a foreign metaphysics; they were kept out of the doctrinal conversation, which was conducted in scholastic Buddhist Chinese by monastic elites. Village households carrying duplicate obligations had no seat either; their dissent surfaces only in separation-era petitions after the arrangement fell.
% DISAPPEARANCE_RATIONALE: Overnight removal would unravel the shrine-temple compound system, the parishioner registration that anchored rural administration, the temples' funerary role, the festival calendar, and the land-tenure arrangements layered through monastic estates — the entire medieval and early-modern Japanese religious-administrative landscape would reorganize, which is approximately what the forced separation of 1868 actually required a decade of state coercion to accomplish.
% FOUNDING_PROBLEM: From the sixth century onward, the arrival of Buddhism posed an unresolved question: what relation holds between the buddhas who govern universal liberation and the kami who guard this land and its harvests? Early answers alternated between hostility and bare coexistence; the fusion reading was built to solve integration by declaring the kami to be the buddhas' own local appearances.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: nativist scholars (Motoori Norinaga and the Kokugaku line) attest the problem was answered wrongly — that the fusion was contamination requiring removal; the drafters of the Meiji separation edicts acted on that testimony; and modern historians (Kuroda Toshio foremost) corroborate from the archival record that the arrangement's operative core was institutional power and not metaphysics alone. No contemporary neutral insider attests the founding problem's resolution — dissent inside the system was suppressed, which is itself signal.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 at interval end) is high because the fusion converted a doctrinal claim into a routing table for authority and income: interpretive supremacy over the kami, shrine-temple compound revenues, and — under the Edo parishioner system — a compulsory funerary relationship with every household in the archipelago. Suppression (0.70) is higher than extraction because the framework's persistence depended on actively foreclosing the rival ontology: doctrinal gatekeeping in scholastic Chinese, monastic force in the medieval centuries, and state-backed parish registration under the warrior regimes. Theater (0.38) stays below the atrophy threshold honestly: the combined rites did real integrative work throughout, though a growing share of scholastic production defended the framework's privileges rather than articulated the unity. Accessibility collapse (0.45) is moderate — once inside the framework, kami-without-buddhas became nearly unsayable in official registers, yet exclusivist currents survived at the margins and eventually supplied the separation program. Resistance (0.42) was persistent but intermittent: shrine-lineage pushback, the Yoshida inversion, nativist learning — crushed or absorbed for a millennium, then victorious in a decade. The three measurement series share one time grid (850, 1150, 1450, 1650, 1800, 1868) with every metric authored at every point. The suppression_requirement series is authored deliberately: this story specifically tracks enforcement-capacity change, from persuasion-plus-court-sanction through monastic armed force to compulsory parish registration — a hardening ratchet, not a static picture. Claim and metrics are independent authored facts: the tangled_rope claim states what I believe is structurally true; the metrics state what I believe descriptively happened.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types. From the monastic-center seat the arrangement is a disclosure it stewards: the unity is real, the combined rites enact it, and the routed income is the price of maintaining universal teaching in a particular land. From the hereditary shrine-lineage seat the same structure operates as subordination of ancestral gods to a foreign metaphysics administered by competitors. From the parish-household seat it is simply duplicated obligation wrapped around a genuinely integrated ritual life. The excluded exclusivist seat experiences the framework as a closure of the sayable. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Esoteric monastic centers sit at the beneficiary pole (d near 0): they set the identifications and collect the flows, with arbitrage-grade freedom to reformulate. The imperial court sits near them as a net beneficiary — it receives legitimacy and pays little directly. Warrior regimes are dual-positioned: legitimation received, taxation and enforcement costs borne, placing them nearer symmetric than the derivation from their beneficiary role alone would suggest. Hereditary shrine lineages are the primary targets: they surrender interpretive authority over their own deities, and their identity_locked exit (lineage, office, and family name fused with the subordinated position) amplifies their effective extraction toward the full-target end. Kami-exclusivist traditions are targets of the suppression machinery itself — their exclusion is what the enforcement maintains. Rural parish households sit mid-range: real coordination benefit (one integrated ritual landscape) against duplicate material obligations, with trapped exit pushing their effective burden upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two misreadings. Reading the fusion as a snare would erase the genuine coordination it performed: it really did solve the kami-buddha collision, really did integrate festival calendars, sacred geography, and clerical careers across a millennium, and participants at every seat received something. Reading it as a rope would erase the asymmetric capture: the same structure that integrated also subordinated shrine lineages, monopolized funerary life, and foreclosed the rival ontology by force. Tangled rope holds both facts. It is not a piton: the function had not atrophied — theater_ratio peaks at 0.38, and the arrangement was destroyed from outside by state violence in 1868 while still performing its integrative work, not abandoned after decaying into performance. On the R5 mismatch consumer: founding_problem_status is contested (not dead), so the dead-plus-world_rearranges zombie flag does not fire — the parties genuinely dispute whether the founding problem was ever resolved or was dissolved by administrative fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the shinbutsu_ontological_substrate kernel does the historical record instantiate — this fusion reading, the domain-partition reading, or the incoherent-bundle reading?',
    'Comparative analysis of the three sibling constraint stories against shared evidence — doctrinal texts, institutional records, and the mechanics of the 1868 separation: whichever reading''s predicted signatures (enforcement profile, victim structure, persistence mode) best fit the record resolves the kernel.',
    'If the domain-partition reading is correct, this constraint''s epsilon drops sharply (functional coexistence between separate beings extracts little); if the incoherent-bundle reading is correct, there is no unified commitment here to classify at all and this story decomposes into accumulated drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which sibling reading captures the kernel; determines whether this story''s structure exists as described.').

omega_variable(
    metaphysical_truth_vs_constructed_doctrine,
    'Is the kami-buddha ontological unity a discovered metaphysical fact that the arrangement discloses, or a constructed doctrine that identifiable institutions deployed to capture shrine autonomy?',
    'Track honji identifications against institutional advantage: if new kami-to-buddha identifications systematically follow land-revenue contests, appointment struggles, and patronage opportunities rather than revelatory or devotional sequence, the constructed-doctrine side strengthens.',
    'A discovered-fact resolution pushes the constraint toward mountain-like treatment with negligible authored extraction; a constructed-doctrine resolution confirms the tangled-rope reading and validates suspicion of the framework''s self-presentation as timeless truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_constructed_doctrine, empirical, 'Whether the fusion''s ontology is natural truth or institutional construction benefiting identifiable actors.').

omega_variable(
    persistence_enforcement_dependence,
    'How much of the fusion order''s thousand-year persistence rested on active enforcement rather than sincere conviction?',
    'The speed and completeness of the 1868 collapse once state enforcement flipped: a self-evident eternal truth that required armed demolition, mass priest laicization, and statue destruction within roughly four years was enforcement-dependent to a measurable degree; residual post-separation popular adherence would index the conviction share.',
    'High enforcement dependence raises effective suppression for the trapped seats and secures the tangled-rope-over-mountain classification; low dependence would indicate the arrangement was approaching self-sustaining status and soften the suppression profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(persistence_enforcement_dependence, empirical, 'Enforcement share of the arrangement''s persistence, evidenced by collapse dynamics at the interval endpoint.').

omega_variable(
    subordination_experience_accounting,
    'Did hereditary shrine lineages experience their incorporation as extraction (loss of autonomous authority over their own deities) or as elevation (participation in universal dharma)?',
    'Emic sources: shrine diaries, complaint petitions, priest correspondence — especially episodes where lineages resisted Buddhist takeover of rites versus where they solicited incorporation for prestige and protection.',
    'If elevation dominates, the payer-seat directionality is overstated and effective extraction falls; if extraction dominates, the victim declarations stand and the asymmetry is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_experience_accounting, conceptual, 'Emic versus etic accounting of shrine-lineage subordination within the fused order.').

omega_variable(
    kenmitsu_reframing_validity,
    'Does Kuroda Toshio''s kenmitsu-taisei reframing — the fusion as ideological superstructure of monastic power — correctly identify the arrangement''s extractive core, or does it over-impose modern economic categories on sincere religiosity?',
    'Test the reframing''s predictions against pre-modern evidence untouched by modern secular historiography: estate ledgers, ordination-platform politics, and the distribution of ritual fees across the shrine-temple compounds.',
    'Validation raises the credible epsilon floor for this reading and supports the victim declarations; refutation would lower epsilon substantially and pull the story toward a rope-like assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kenmitsu_reframing_validity, empirical, 'Validity of the modern historiographic identification of monastic rent capture inside the fused order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 850, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t850, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 850, 0.15).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_tr_t850, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1150, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1150, 0.2).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_tr_t1150, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1450, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1450, 0.28).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_tr_t1450, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1650, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1650, 0.3).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_tr_t1650, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1800, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1800, 0.34).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_tr_t1800, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1868, 0.38).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t850, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 850, 0.38).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_be_t850, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1150, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1150, 0.48).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_be_t1150, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1450, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1450, 0.56).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_be_t1450, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1650, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1650, 0.6).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_be_t1650, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1800, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_be_t1800, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1868, 0.68).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t850, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 850, 0.35).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_su_t850, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1150, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1150, 0.5).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_su_t1150, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1450, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1450, 0.55).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_su_t1450, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1650, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1650, 0.62).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_su_t1650, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1800, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1800, 0.66).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_su_t1800, observed).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1868, 0.7).
narrative_ontology:measurement_basis(shinbutsu_syncretic_fusion_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, meiji_shinbutsu_bunri_separation_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu shugo' (kami-buddha harmonization) covers three structurally distinct claims, decomposed per the epsilon-invariance principle into three linked stories sharing the kernel shinbutsu_ontological_substrate. This file (syncretic_fusion_reading) authors the claim as metaphysical truth with high institutional entanglement — epsilon 0.68 over the fused order's actual operation. The domain_partition_reading authors functional coexistence of separate beings (low epsilon, coordination-dominant). The incoherent_bundle_reading authors accumulated drift under state enforcement (no unified commitment; classification dissolves). The upstream/downstream edge to meiji_shinbutsu_bunri_separation_regime records that this arrangement's enforcement-dependent collapse is the direct precondition of the separation regime's construction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
