% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition (Functional Division Reading)
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   From roughly the twelfth century onward, Japanese religious life settled
 *   into a functional division: kami cults governed this-worldly concerns —
 *   agricultural fertility, purity, protection from pollution, life-course
 *   beginnings — while Buddhist institutions governed death, funerals,
 *   memorial services, and salvation. The division was maintained less by
 *   doctrine than by practice: pollution taboos kept death rites out of
 *   shrine precincts, custom allocated ritual occasions, and from the
 *   seventeenth century the warrior government's parish-registration system
 *   gave the death side legal teeth. This story instantiates the
 *   domain_partition_reading of the shinbutsu_coexistence_commitment kernel:
 *   two parallel systems with boundary maintenance, low demand for doctrinal
 *   consistency, popular practice as the operative authority, and functional
 *   coexistence without theological resolution. The epsilon referent is the
 *   standing partition arrangement as it actually operated across the
 *   interval, assessed by this reading's own lights — not the fused ontology
 *   the honji suijaku tradition asserted, and not the pure power account the
 *   bundle reading offers. KEY AGENTS (by structural relationship): -
 *   buddhist_funeral_establishment: primary administrator-collector
 *   (institutional/identity_locked) — runs the death-side monopoly and the
 *   registration rolls - shrine_priesthoods: beneficiary and life-side
 *   boundary-keeper (organized/identity_locked) - danka_households: primary
 *   target (powerless/trapped) — bears the double ritual obligation under
 *   compulsory registration - village_ritual_communities: coordinated middle
 *   (organized/constrained) — allocates obligations, negotiates fees -
 *   bakufu_shogunate: enforcement sponsor (institutional/arbitrage) — backs
 *   registration for surveillance and census purposes -
 *   rival_ritual_specialists: excluded competitors (moderate/constrained) -
 *   kokugaku_nativist_scholars: analytical observer (moderate/analytical) —
 *   supplies the repudiating analysis the Meiji state enacts
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.52).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.55).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Kami-Buddha Domain Partition (Functional Division Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious/philosophical/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'c1b0c0dd-3207-4ea3-9c50-7d06f245600e').
narrative_ontology:cs_kernel_codification('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', distributed).
narrative_ontology:cs_authority_grounding('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', practice).
narrative_ontology:cs_interpretation_layer_present('c1b0c0dd-3207-4ea3-9c50-7d06f245600e').
narrative_ontology:cs_reading_relation('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', foundational, existential_domain_dual_jurisdiction).
narrative_ontology:cs_axiom_status(existential_domain_dual_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', existential_domain_dual_jurisdiction, conventional).
narrative_ontology:cs_axiom('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', foundational, practice_authority_over_doctrine).
narrative_ontology:cs_axiom_status(practice_authority_over_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', practice_authority_over_doctrine, conventional).
narrative_ontology:cs_axiom('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', secondary, death_pollution_boundary_inviolable).
narrative_ontology:cs_axiom_status(death_pollution_boundary_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', death_pollution_boundary_inviolable, conventional).
narrative_ontology:cs_reference_frame('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', dual_domain_practice_order).
narrative_ontology:cs_drift_state('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c1b0c0dd-3207-4ea3-9c50-7d06f245600e', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_funeral_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, village_ritual_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, danka_households).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, rival_ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, village_ritual_communities).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, ritual_jurisdictional_division_of_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary custodial families tending specific kami shrines: they conduct purification rites, harvest festivals, and prayers for this-worldly protection, living on offerings, festival fees, and endowed lands. The customary division of ritual labor reserves life-side ceremonies for them and keeps death-related pollution out of their precincts, which their purity rules require. Leaving the vocation means abandoning a hereditary sacred office bound to a particular shrine lineage.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods, agenda_setter).

% Temple networks and their clergy, who hold the mortuary side of the ritual calendar: funerals, grave sites, and memorial services for ancestors. From the seventeenth century their parishes were registered under warrior-government ordinance, giving them a legally secured congregation owing annual dues and funeral payments. Clergy are ordained into lineages with doctrinal commitments; leaving means leaving orders entirely. They keep the registration rolls and discipline parishes that neglect dues.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_funeral_establishment, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_funeral_establishment, beneficiary).

% Rural and town households registered with a local temple: they owe the temple funeral services, memorial observances, and annual dues, and separately owe the village shrine its festival contributions and purification fees. Individually they have little leverage; registration is compulsory and transferring to another temple requires official permission. Collectively, villages sometimes bargain over fees or petition against increases, with mixed results.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, danka_households, payer,
    powerless, generational, trapped, national).

% Village assemblies that organize the yearly round of observances, splitting occasions between the shrine (planting, harvest, boundary festivals) and the temple (memorial days, funerals). They allocate costs across households, negotiate with both clergy and priests, and keep the two institutions from contesting the same occasion. Their members bear the combined cost burden, but the settlement spares them open rivalry between the two religious establishments.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, village_ritual_communities, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, village_ritual_communities, payer).

% The warrior government that legislated temple registration in the seventeenth century, chiefly to detect and suppress Christianity and to census the population through the parishes. It enforces the registration system through temple codes and judicial rulings but collects no ritual revenue itself; its interest is administrative. It can reconfigure the arrangement by edict, as it periodically does when revising temple codes.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, bakufu_shogunate, agenda_setter,
    institutional, generational, arbitrage, national).

% Itinerant ascetics, mountain-religion practitioners, Confucian-minded burial reformers, and advocates of shrine-based funerals who offer rites crossing the customary division. Shrines bar them from precincts under pollution rules and temples treat them as poaching on parish dues; some operate at the margins of villages, others abandon the trade. Their services persist only in the gaps the two establishments leave uncovered.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, rival_ritual_specialists, excluded,
    moderate, biographical, constrained, regional).

% Nativist scholars of the eighteenth and nineteenth centuries who study ancient texts and argue that mingling kami worship with Buddhist doctrine corrupted an original Japanese way. They collect nothing from the existing arrangement and bear little of its cost; their seat is analytical, but their conclusions supply the program the Meiji government enacts as separation edicts in 1868.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, kokugaku_nativist_scholars, observer,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_funeral_establishment).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the ritual year and the life course between two religious economies: shrines handle purity, agriculture, and this-worldly protection; temples handle death, funerals, and ancestral memory. The division keeps the two institutions from competing for the same occasions, lets kami cults maintain their pollution rules without forgoing mortuary care for their adherents, and gives households an unambiguous map of which door to knock on for which need.
% TRANSFER_FUNCTION: Moves offerings, festival contributions, funeral payments, and annual parish dues from households to shrine and temple institutions; moves ritual legitimacy and commemorative continuity back to households. After the seventeenth-century registration ordinances, it additionally moves compulsory dues from registered households to temples under legal compulsion.
% ABSENT_VOICES: Households too poor to carry the double obligation had no seat in fee-setting; rival ritual specialists barred from both precincts and parishes could not argue their case inside the arrangement; and doctrinal purists on each side — shrine traditionalists rejecting Buddhist mortuary claims, Buddhist schools resenting kami autonomy — were muted because the division rested on practice rather than on any forum where such objections could be heard.
% DISAPPEARANCE_RATIONALE: If the division vanished overnight, the two establishments would contest the same ritual occasions — funerals at shrines, harvest rites claimed by temples — households would face conflicting demands and duplicate billing without a map, and the pollution rules governing shrine space would collapse into constant negotiation. When the Meiji government forcibly separated the systems in 1868, precisely this rearrangement followed: shrine and temple estates were untangled, temples lost their parishes, and an anti-Buddhist destruction movement swept the country.
% FOUNDING_PROBLEM: Two religious systems — an indigenous kami cult complex with strict death-pollution rules and a Buddhist establishment claiming universal salvific authority over the mortuary sphere — occupied the same islands and served the same population. Each needed the resources and deference the other also sought, and open competition threatened both. The arrangement was built to let them share one population without destroying each other.
% FOUNDING_PROBLEM_CORROBORATION: Warrior-government statutory records (temple codes, registration ordinances) attest the administrative problem — census, Christian detection, parish discipline — that pushed the division into legal form, from a seat outside both clergies; village covenant documents and headman records show communities negotiating the split of occasions and costs as a practical matter; modern historiography of Japanese religion treats the functional division as an empirically documented pattern rather than clerical self-description. The problem of plural ritual economies sharing one population is corroborated from administrative, communal, and scholarly seats outside the benefiting parties.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is scored 0.52 for the standing arrangement: the partition created captive ritual markets on both sides, households owed both establishments, and the death side's rents hardened sharply once registration became statutory (series peak 0.62 circa 1750, easing with rural distress and collapsing as the Meiji state dismantled the machinery). Suppression is scored 0.55 as a raw structural property — pollution sanction, institutional discipline, and bakufu statute — deliberately unscaled; the engine applies directionality and scope scaling to extractiveness only. Theater is 0.28: while practice carried the arrangement, little doctrinal performance was needed; the ratio rises as elite discourse (elaborated honji suijaku rhetoric, then nativist polemic) decouples from a functioning practice order. Accessibility_collapse is 0.38 — alternatives (shrine funerals, Confucian burial, itinerant rites) remained possible at social cost but were foreclosed at the institutional center. Resistance is 0.45 — recurring fee disputes, shrine-temple litigation, the Shinto funeral movement, and nativist critique. The claimed type (tangled_rope) is authored from the structural reading — genuine coordination function plus asymmetric extraction with active enforcement — independently of these metric values; divergence between claim and computed type is signal, not error. All three temporal series share one eight-point grid (1200-1868); suppression_requirement is tracked because enforcement capacity is the traced dynamic (statutory ratchet under the warrior government, collapse at the separation edicts). Receipt: the largest monetized flow — compulsory parish dues and funeral payments — demonstrably accrued to the temple establishment, which is why gain_flow names that seat despite the real but smaller and more localized shrine share. Fixing cost: prohibitive — the registration system was load-bearing for census and Christian surveillance, and the one historical attempt to fix the arrangement by force (1868) triggered estate seizure, parish loss, and the anti-Buddhist destruction movement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the administrator seats should compute different types from identical structure. From the danka_household position the arrangement is compulsory payment for services framed as unavoidable, backed by law and pollution sanction; from the buddhist_funeral_establishment position it is pastoral duty and ancestral care that households would neglect without discipline; from the shrine position it is the purity order that makes kami worship possible at all; from the village position it is a workable settlement that spares the community institutional war. The engine computes these divergences from power, exit, and role data; this story authors the structure and declines to adjudicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (shrine_priesthoods, buddhist_funeral_establishment, village_ritual_communities) drive those seats toward the beneficiary end; victim declarations (danka_households, rival_ritual_specialists) drive them toward the target end. Trapped and identity_locked exits matter: households cannot legally leave their registered parish, and both clergies are bound by ordination and hereditary office, which pins the targets near the full-target end and stabilizes the collectors. The bakufu sits partial-beneficiary — it collects no ritual revenue but draws administrative utility (census, surveillance) at little cost borne. National spatial scope modestly amplifies effective extraction through verification difficulty: parish boundaries and fee schedules were hard to audit at distance, which favored the registering temple.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against two symmetrical errors. Reading the arrangement as pure extraction ignores the coordination function that made it stable for six centuries with remarkably light doctrinal machinery: jurisdictional clarity, conflict reduction between two armed institutional economies, and pollution management the kami cults genuinely required. Reading it as pure coordination ignores the compulsory registration layer, the monopoly rents, and the excluded specialists. It is not a scaffold — no sunset clause was ever declared; the arrangement ended by external state violence, not by designed transition. It is not a piton — the function was live and load-bearing until 1868, and theater stayed subordinate to practice throughout. The founding problem (plural ritual economies sharing one population) remains live, so no mandatrophy resolution is declared: the live-status x world_rearranges profile marks a functional arrangement killed by repudiation, not an obsolete one kept alive by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the standing kami-Buddha arrangement accurately read as a stable domain partition maintained by practice (this reading), as an ontological fusion of kami and Buddhas (syncretic_fusion_reading), or as an incoherent bundle sustained by deliberate ambiguity and institutional power (incoherent_bundle_reading)?',
    'Comparative analysis of practice records against doctrinal texts across the interval: if boundary behavior tracks functional needs rather than ontological claims, the partition reading holds; if practice presupposes kami as manifestations of Buddhist truth, the fusion reading holds; if boundary maintenance appears only where enforcement is documented, the bundle reading holds.',
    'Under the fusion reading the partition is surface phenomenon over deep unity and measured extraction drops (there is no genuine boundary to police); under the bundle reading the partition is post-hoc rationalization and enforcement carries the whole weight, pushing the classification toward pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the shinbutsu coexistence kernel the structural evidence supports.').

omega_variable(
    partition_vs_danka_extraction_attribution,
    'How much of the measured extraction belongs to the domain partition as such, and how much to the compulsory parish-registration system (danka/terauke) that rode on its death side from the seventeenth century?',
    'Decompose the arrangement at the statutory seam — customary allocation before 1600 versus registration ordinances from 1635 onward — compare extraction indicators across it, and author the registration layer as its own constraint story linked to this one.',
    'If most extraction is attributable to the registration layer, this constraint''s epsilon falls toward coordination-cost levels and the reading shifts toward rope, with the registration layer carrying the extractive classification as a separate story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_danka_extraction_attribution, conceptual, 'Attribution of measured extraction between the partition and the danka registration overlay.').

omega_variable(
    practice_authority_vs_enforcement,
    'Did the boundary hold because popular practice sincerely took the division for granted (this reading''s authority claim), or because institutional enforcement and pollution sanction punished crossings?',
    'Track boundary-crossing episodes (shrine funerals, Confucian burials, itinerant rites) across periods of weak versus strong enforcement: if crossings proliferate whenever enforcement slackens, enforcement carries the boundary; if they stay rare even in Sengoku-era enforcement gaps, practice carries it.',
    'If enforcement dominates, the suppression score understates the arrangement''s coercive core and the practice-authority axiom loses its grounding; if practice dominates, the arrangement is more genuinely coordinated than the enforcement record alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_authority_vs_enforcement, empirical, 'Whether boundary maintenance ran on sincere practice or on sanction.').

omega_variable(
    compliance_internalization_after_meiji,
    'Was household compliance with the double ritual obligation structural (compelled by registration law and social sanction) or internalized (sincere acceptance of the division as the natural order of things)?',
    'Post-1868 natural experiment: once the separation edicts abolished compulsory registration, observe whether households voluntarily maintained dual affiliation — continuing shrine festivals and Buddhist funerals without legal compulsion. Persistent voluntary dual practice indicates internalized acceptance; rapid abandonment indicates the compulsion was doing the work.',
    'If compliance was substantially internalized, the arrangement''s effective suppression exceeded what statute alone explains and its persistence was overdetermined; if compliance collapsed with the law, the measured suppression was almost entirely structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internalization_after_meiji, empirical, 'Structural versus internalized compliance with the dual ritual obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 1200, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_partition_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1200, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1300, 0.16).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1300, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1400, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1400, 0.18).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1400, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1550, 0.2).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1550, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1650, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1650, 0.24).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1650, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1750, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1850, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1850, 0.33).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1850, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1868, 0.4).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_partition_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.3).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1200, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1300, 0.34).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1300, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1400, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1400, 0.38).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1400, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1550, 0.42).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1550, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1650, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1650, 0.58).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1650, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1750, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1850, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1850, 0.56).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1850, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1868, 0.44).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_partition_su_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1200, 0.25).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1200, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1300, 0.28).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1300, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1400, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1400, 0.32).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1400, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1550, 0.4).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1550, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1650, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1650, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1750, 0.63).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1750, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1850, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1850, 0.5).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1850, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, danka_terauke_compulsory_registration).

% DUAL FORMULATION NOTE:
% Constraint family: the shinbutsu_coexistence_commitment kernel decomposes into three reading-stories — this domain_partition_reading, syncretic_fusion_reading (ontological unification via honji suijaku), and incoherent_bundle_reading (ambiguity-plus-power account). Each is a separate epsilon-invariant constraint over the same historical material, and this story links both siblings. Additionally, per the epsilon-invariance principle, the compulsory parish-registration layer is decomposed into its own downstream story (danka_terauke_compulsory_registration): the partition enabled it, and extraction attributable to statutory registration is attributed there rather than double-counted here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
