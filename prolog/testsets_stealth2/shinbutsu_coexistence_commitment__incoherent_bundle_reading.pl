% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Enforced Incoherent Bundle (Incoherent-Bundle Reading)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   For roughly eleven centuries the sacred order of the Japanese islands ran
 *   kami shrines and Buddhist temples as one interlocking establishment:
 *   temples stood inside shrine precincts, monks administered shrines, kami
 *   received Buddhist names and ranks as protectors of the dharma, and priest
 *   lineages ordained into Buddhist orders. This story authors ONE reading of
 *   that arrangement — the incoherent_bundle_reading of the
 *   shinbutsu_coexistence_commitment kernel: the claim that the arrangement
 *   never rested on a stable ontology of the kami-Buddha relation, that
 *   categorical questions ('what IS a kami?') were kept unaskable by design,
 *   and that the Meiji separation edicts (shinbutsu bunri, 1868-1872)
 *   revealed rather than created the incoherence by switching the enforcement
 *   off. The ε referent is the standing coexistence arrangement itself,
 *   assessed by this reading's lights: a substantially extractive structure
 *   in which doctrinal ambiguity functioned as protective cover for the
 *   monastic centers that supervised shrines — not the post-separation order
 *   this reading might prefer. Per the ε-invariance principle, the sibling
 *   readings (syncretic_fusion_reading: honji suijaku as genuine ontological
 *   unification; domain_partition_reading: stable jurisdictional division
 *   without unification) are separate constraint stories with their own ε
 *   values, beneficiaries, and classifications, linked through
 *   network.affects_constraints rather than averaged here. The claimed type
 *   and the metrics are authored independently: the claim states what this
 *   reading takes the structure to be; the metrics state what the record
 *   shows of its operation. KEY AGENTS (by structural relationship): -
 *   temple_complexes: Primary agenda-setter (institutional/arbitrage) —
 *   administers shrines, controls the doctrinal vocabulary, collects revenue
 *   and appointment rights - hereditary_shrine_priests: Primary target
 *   (moderate/identity_locked) — bears supervision and liturgical
 *   subordination, cannot leave the lineage vocation -
 *   local_kami_cult_communities: Diffuse targets (powerless/trapped) —
 *   absorbed cults, doubled obligations, no exit - shugendo_practitioners:
 *   Fused-practice payers (organized/identity_locked) — destroyed by the
 *   bundle's unraveling - court_ritual_establishment: Dual-positioned
 *   beneficiary-payer (powerful/constrained) — legitimation rents against
 *   fiscal and ideological costs - kokugaku_scholars: Excluded critics
 *   (organized/constrained) — built the outside platform that armed the
 *   separation - modern_historians_of_religion: Analytical observer
 *   (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.68).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Enforced Incoherent Bundle (Incoherent-Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '28c5ccf0-7070-4044-9f7f-5ee797ad52bb').
narrative_ontology:cs_kernel_codification('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', distributed).
narrative_ontology:cs_authority_grounding('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', extraction).
narrative_ontology:cs_interpretation_layer_present('28c5ccf0-7070-4044-9f7f-5ee797ad52bb').
narrative_ontology:cs_reading_relation('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', shinbutsu_coexistence_commitment__syncretic_fusion_reading, influences).
narrative_ontology:cs_reading_relation('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', foundational, no_stable_kami_buddha_ontology).
narrative_ontology:cs_axiom_status(no_stable_kami_buddha_ontology, holdable).
narrative_ontology:cs_axiom_grounding('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', no_stable_kami_buddha_ontology, empirically_contingent).
narrative_ontology:cs_axiom('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', foundational, enforcement_substituted_for_coherence).
narrative_ontology:cs_axiom_status(enforcement_substituted_for_coherence, holdable).
narrative_ontology:cs_axiom_grounding('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', enforcement_substituted_for_coherence, empirically_contingent).
narrative_ontology:cs_reference_frame('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', enforced_institutional_bundle).
narrative_ontology:cs_drift_state('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', contemporary_revisionist_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('28c5ccf0-7070-4044-9f7f-5ee797ad52bb', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, temple_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_establishment).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, hereditary_shrine_priests).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_kami_cult_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_establishment).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, ambiguity_substitutes_for_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great monastic centers and their branch networks — Tōdaiji with the Kasuga-Kōfukuji complex, Enryakuji with Hiyoshi — held appointment rights over shrines, built temple halls inside shrine precincts, performed Buddhist rites for kami, trained the clergy who served both sides, and drew land revenue, offerings, and ritual fees from shrine estates. They supplied the doctrinal vocabulary in which questions about what the kami are could be posed, and controlled which answers circulated. When the framing shifted, they moved between trace-origination teaching, dual-system constructions, and protector cults as advantage dictated.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, temple_complexes, agenda_setter,
    institutional, generational, arbitrage, national).

% The court and its ritual offices gained a single ceremonial order: kami cult legitimated the polity while Buddhist institutions handled the death pollution the kami cult forbade, and ritual law could reference one establishment instead of two rival ones. It also financed the apparatus, adjudicated the disputes the entanglement generated, and by the eighteenth century faced an intellectual movement claiming the entanglement had corrupted the imperial cult's own foundations. Rebuilding ritual law from scratch against entrenched holders was not a live option for it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_establishment, beneficiary,
    powerful, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_establishment, payer).

% Priest lineages transmitted office and rite within families across generations. Supervision ran through monastic channels: confirmation of appointments, ordination expectations, liturgical forms framed in Buddhist terms. A lineage that articulated an independent account of its kami invited conflict with the temple that confirmed its head; a lineage that abandoned the office dissolved its vocation entirely. Most accommodated, and taught accommodation to their sons.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, hereditary_shrine_priests, payer,
    moderate, generational, identity_locked, regional).

% Village congregations around ujigami shrines saw their cults renamed, re-dedicated under Buddhist titles, registered through temples, and staffed at festivals by Buddhist officiants; halls and images appeared in precincts. Households carried obligations to both sides of the compound. The village's god belonged to the village's ground; taking the cult elsewhere was not an option anyone had.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_kami_cult_communities, payer,
    powerless, generational, trapped, local).

% Mountain ascetic orders made their living and their selves inside the fused landscape: kami peaks as Buddhist sacred geography, rites drawing on both registers, confraternities spanning villages and courts. When the separation edicts came, the order was abolished outright and its practitioners laicized or reassigned within a few years. Nothing in their training equipped a life outside the fused frame.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_practitioners, payer,
    organized, biographical, identity_locked, national).

% Philologists of the National Learning movement argued from the oldest texts that kami worship was an autonomous tradition overlaid by Buddhist institutions, and that the overlay was recent and removable. For most of the arrangement's lifetime they had no seat in the institutional conversation; they printed through commercial networks, built study groups, and endured prosecutions (Hirata-school arrests in the 1840s). Their lexicon became the working vocabulary of the Meiji separation.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kokugaku_scholars, excluded,
    organized, biographical, constrained, national).

% Revisionist historians reconstruct the arrangement from estate documents, liturgical manuals, and dispute records, testing competing accounts of what held it together. They collect no revenue and bear no ritual obligation from any resolution; their stake is interpretive.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, modern_historians_of_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, temple_complexes).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ran one ritual establishment covering both this-worldly and other-worldly needs: kami cults handled purity, harvest, and protection while Buddhist institutions handled death rites and salvation, with shared sites, a shared calendar, and shared administrative channels; it also incorporated large numbers of local cults into a national order without requiring them to abandon their gods.
% TRANSFER_FUNCTION: Moved appointment rights, doctrinal authority, land and ritual revenue, and liturgical control from shrine lineages and local cults toward monastic centers and the court establishment; moved legitimacy in the reverse direction, as kami protector-cult status anchored temples locally.
% ABSENT_VOICES: Autonomous kami-theologians before the eighteenth century — shrine priests who might have articulated an independent doctrine of the kami had an institutional platform existed — and village congregations whose cults were renamed without consultation. They were absent because the supervisory structure ran through the very temples whose position an independent theology would threaten; kokugaku eventually built an outside platform, and its marginalization for a century measures how closed the conversation was.
% DISAPPEARANCE_RATIONALE: Between 1868 and 1872 the arrangement vanished by edict: shrine-temples were forcibly separated, jingū-ji demolished, kami statues burned or discarded in the haibutsu kishaku excesses, Shugendō abolished as an order, tens of thousands of clergy reclassified or laicized, and shrine lineages rewritten as an autonomous priesthood. Every dependent arrangement — funeral practice, festival calendars, mountain religiosity, the court's ritual law — reorganized within a generation.
% FOUNDING_PROBLEM: Accommodate a universalist salvation religion (Buddhism, arriving with literacy, ritual technology, and continental prestige) within a polity whose sacred landscape and legitimating cults were kami-based, without either side destroying the other or the court losing a usable ritual order.
% FOUNDING_PROBLEM_CORROBORATION: Kokugaku scholarship, from outside the benefiting parties, attested from the mid-Edo period that the integrative work was long finished and the arrangement persisted as institutional subordination (Norinaga's attack on honji suijaku as usurpation); Meiji reformers cited that lineage; modern revisionist historiography corroborates from the analytical seat that late-phase persistence tracked institutional interest rather than an unresolved integrative need. Temple-complex apologia disputed this throughout — hence contested rather than dead.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the bundle delivered real services (death rites, literacy, festival infrastructure, incorporation of local cults) while transferring appointment rights, land and ritual revenue, and doctrinal authority from shrine lineages to monastic centers; under this reading the ambiguity itself was part of the extraction mechanism, since an unformulable kami-theology left supervisory rights unchallengeable. Suppression is 0.68 and structural in the main — appointment control, ordination gates, and the Tokugawa terauke registration system made affiliation compulsory and independent articulation costly — with a smaller internalized component among priest lineages raised inside the supervisory frame. Theater_ratio 0.46: doctrinal production (ryōbu shintō treatises, honji suijaku liturgics) increasingly maintained the ambiguity rather than described practice, while core services remained real. Accessibility_collapse 0.52: exits existed and were taken — Yoshida Shintō briefly inverted the kami-Buddha hierarchy, kokugaku built an outside platform — but each was costly and contested, so alternatives were suppressed rather than erased. Resistance 0.60: opposition accumulated across the whole interval, from medieval shrine-court disputes through kokugaku prosecution-evasion to the Meiji state itself reversing enforcement; notably, the payer coalition succeeded only by capturing state power, never through the arrangement's internal channels, which is why resistance stayed below success threshold for a millennium. The measurement series runs on one shared grid (710-1872, eight points, all three metrics at every point). The suppression_requirement cliff at 1872 (0.68 to 0.12) is this reading's central signature: enforcement substituted for coherence, so the bundle collapsed within four years of the enforcement flip rather than resisting.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From temple_complexes the arrangement is the religious order they built, staffed, and policed — coordination they administer, with the ambiguity as their working medium (arbitrage across doctrinal framings). From hereditary_shrine_priests and shugendo_practitioners — identity_locked payers — the same structure operates as subordination they could neither name nor leave; local_kami_cult_communities, trapped, experience it as doubled obligation and a god they may not define. court_ritual_establishment splits internally: collector of legitimation rents, payer of fiscal support and, eventually, of the ideological crisis kokugaku opened. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive temple_complexes and court_ritual_establishment toward the beneficiary end; victim declarations drive the three payer seats toward the target end, amplified by exit structure — trapped (local cults) and identity_locked (priest lineages, shugendō) sit nearer full-target than mobile agents would. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope. One override is declared: the derivation reads court_ritual_establishment as a plain beneficiary (low d), but its situation includes financing the apparatus and absorbing the ideological costs of the ambiguity it legitimated, so its true position sits nearer symmetric than the beneficiary label alone yields; the override sets d = 0.32 at the 'powerful' atom — the only powerful seat in the story, so the atom-level override lands on it alone. kokugaku_scholars carry role 'excluded': they were outside the conversation for most of the interval and feed no directionality; their exclusion is recorded in absent_voices and in their stakeholder situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Calling the arrangement a rope would erase the identifiable payers the enforcement machinery produced; calling it a snare would erase the genuine integrative services that made participation rational for centuries and that the fusion and partition readings correctly emphasize. The R5 interview records the founding problem (accommodating a universalist salvation religion within a kami-based polity) as contested: temple apologia treated integration as complete and beneficial; kokugaku and its Meiji heirs treated the arrangement as a finished integration hardened into usurpation. Because status is contested rather than dead, the status-by-verdict consumer finds no automatic zombie flag — the mandatrophy question is genuinely disputed between seats, which is itself the finding this reading contributes. The temporal series carries the abductive weight instead: a millennium-long rise in base_extractiveness alongside enforcement build-up, ending in a four-year collapse once enforcement flipped, is the signature the incoherent-bundle reading predicts and the coherent-kernel readings do not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the shinbutsu_coexistence_commitment kernel; how would classification shift under the sibling readings?',
    'Generate and compare the sibling stories (syncretic_fusion_reading, domain_partition_reading) on the same interval and stakeholder surface; divergence in computed type and effective extraction locates what each reading changes.',
    'The fusion reading would likely lower ε (a coherent doctrine genuinely believed extracts less through ambiguity) and restructure beneficiaries toward the doctrinal schools; the partition reading would shrink the victim set to boundary-crossing cases and could push toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    deliberate_vs_emergent_ambiguity,
    'Was the categorical ambiguity deliberately maintained by institutional actors, or an emergent selection effect in which ambiguous arrangements survived because they provoked fewer disputes?',
    'Archival study of documented kami-status disputes (court debates, shrine-temple sōron litigation, Ise controversies): did actors consciously deploy or defend ambiguity as a strategy, or did explicit clarifications fail for unrelated reasons?',
    'Deliberate maintenance strengthens the extraction reading (agency behind the suppression of categorical questions) and pushes effective extraction upward; purely emergent ambiguity softens the arrangement toward inertial persistence and lowers the snare-flavored component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_vs_emergent_ambiguity, empirical, 'Whether the ambiguity was strategy or selection.').

omega_variable(
    counterfactual_collapse_test,
    'Did the arrangement collapse because it was latently incoherent, or because the Meiji state applied unprecedented coercive force against the religious establishment — would it have survived modernization without bunri enforcement?',
    'Comparative analysis: fusion arrangements in regions where bunri was weakly enforced (remote provinces, where syncretic practice persisted for decades) and other modernizing states'' treatment of embedded syncretisms.',
    'If fusion persisted wherever enforcement lapsed, the ''revealing'' claim weakens toward ''state destruction manufactured the appearance of incoherence''; if it collapsed wherever enforcement flipped, the enforcement-substitution axiom strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_collapse_test, empirical, 'Latent incoherence versus state violence as the cause of collapse.').

omega_variable(
    victimhood_under_unformulable_terms,
    'Can agents be victims of an arrangement whose governing terms they could not articulate — does subordination under deliberately unformulable doctrine constitute extraction?',
    'Conceptual analysis cross-checked against the record of shrine-lineage petitions and complaints: did subordinated priests experience and frame their position as loss, and on what grounds?',
    'If subordination-under-ambiguity does not count as extraction, ε drops materially and the computed type drifts toward rope or piton; if it does, the victim declarations stand as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victimhood_under_unformulable_terms, conceptual, 'Whether unformulable subordination constitutes extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 710, 1872).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t710, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 710, 0.15).
narrative_ontology:measurement_basis(shin_tr_t710, observed).
narrative_ontology:measurement(shin_tr_t950, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 950, 0.25).
narrative_ontology:measurement_basis(shin_tr_t950, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1200, 0.32).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1450, 0.38).
narrative_ontology:measurement_basis(shin_tr_t1450, observed).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement_basis(shin_tr_t1600, observed).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1750, 0.47).
narrative_ontology:measurement_basis(shin_tr_t1750, observed).
narrative_ontology:measurement(shin_tr_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1850, 0.52).
narrative_ontology:measurement_basis(shin_tr_t1850, observed).
narrative_ontology:measurement(shin_tr_t1872, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1872, 0.58).
narrative_ontology:measurement_basis(shin_tr_t1872, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t710, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 710, 0.34).
narrative_ontology:measurement_basis(shin_be_t710, observed).
narrative_ontology:measurement(shin_be_t950, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 950, 0.44).
narrative_ontology:measurement_basis(shin_be_t950, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1200, 0.57).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1450, 0.61).
narrative_ontology:measurement_basis(shin_be_t1450, observed).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1600, 0.66).
narrative_ontology:measurement_basis(shin_be_t1600, observed).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1750, 0.69).
narrative_ontology:measurement_basis(shin_be_t1750, observed).
narrative_ontology:measurement(shin_be_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1850, 0.71).
narrative_ontology:measurement_basis(shin_be_t1850, observed).
narrative_ontology:measurement(shin_be_t1872, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1872, 0.73).
narrative_ontology:measurement_basis(shin_be_t1872, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t710, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 710, 0.2).
narrative_ontology:measurement_basis(shin_su_t710, observed).
narrative_ontology:measurement(shin_su_t950, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 950, 0.3).
narrative_ontology:measurement_basis(shin_su_t950, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1200, 0.45).
narrative_ontology:measurement_basis(shin_su_t1200, observed).
narrative_ontology:measurement(shin_su_t1450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1450, 0.5).
narrative_ontology:measurement_basis(shin_su_t1450, observed).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1600, 0.62).
narrative_ontology:measurement_basis(shin_su_t1600, observed).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1750, 0.65).
narrative_ontology:measurement_basis(shin_su_t1750, observed).
narrative_ontology:measurement(shin_su_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement_basis(shin_su_t1850, observed).
narrative_ontology:measurement(shin_su_t1872, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1872, 0.12).
narrative_ontology:measurement_basis(shin_su_t1872, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, state_shinto_construction).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu-shugo' conflates at least three structurally distinct claims about the kami-Buddha arrangement: that it instantiated genuine ontological unification (syncretic_fusion_reading), that it rested on a stable division of jurisdictions (domain_partition_reading), and that it was an enforced bundle with no stable ontology (this story). Each claim has its own ε, its own failure modes, and its own beneficiary/victim structure, so each is authored as a separate constraint story; the ε values differ because the referent assessments differ — fusion reads the arrangement through its professed doctrine, partition through its jurisdictional settlements, the bundle reading through its enforcement record. This story links to both siblings and to the downstream state_shinto_construction, which was assembled from the bundle's wreckage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
