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
 *   human_readable: Kami-Buddha Domain Partition: Life/Death Jurisdictional Division
 *   domain: religion/history
 *
 * SUMMARY:
 *   For most of a millennium, Japanese religious life ran on two parallel
 *   institutional systems with a maintained boundary between them: shrines
 *   held jurisdiction over the life side of existence — purity, harvest,
 *   protection, festivity — while temples held the death side — funerals,
 *   graves, ancestral memorials, salvation. The boundary was policed
 *   concretely (death pollution excluded from shrine precincts, mortuary
 *   business ceded to temples) and required no agreement about what kami and
 *   Buddhas ultimately are; the arrangement ran on jurisdictional
 *   complementarity, not theological resolution. This story instantiates the
 *   domain-partition reading of the shinbutsu coexistence kernel: the
 *   commitment under contest is taken to be a functional division of
 *   existential labor sustained by practice, with low constraint on doctrinal
 *   consistency. The sibling readings — ontological fusion through honji
 *   suijaku, and the claim that the whole was an incoherent bundle held up by
 *   institutional power — are separate constraint stories with their own
 *   epsilon values and classifications; they are linked, not averaged, here.
 *   The epsilon referent throughout is the standing partition arrangement as
 *   this reading assesses it: a working dual system whose real coordination
 *   value coexists with real, unevenly distributed costs. KEY AGENTS (by
 *   structural relationship): - commoner_households: Primary target and
 *   secondary beneficiary (organized/trapped) — bear mortuary fees, compelled
 *   registration, and festival obligations; receive low-cost routing of
 *   existential needs - buddhist_temple_establishment: Agenda-setter and
 *   principal beneficiary (institutional/constrained) — administers mortuary
 *   jurisdiction, collects the largest flows - shrine_priesthoods:
 *   Beneficiary (organized/identity_locked) — exclusive life-side
 *   jurisdiction, death-free ritual space - death_trade_outcaste_communities:
 *   Concentrated target (powerless/trapped) — bear the purity boundary's
 *   stigma and death-handling burden - imperial_and_shogunal_authorities:
 *   Secondary agenda-setter (institutional/arbitrage) — leverage the
 *   partition as an administrative grid - kokugaku_nativist_scholars:
 *   Excluded critic (organized/mobile) — rejection program later implemented
 *   by the Meiji state - historians_of_japanese_religion: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.45).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.4).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Kami-Buddha Domain Partition: Life/Death Jurisdictional Division").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religion/history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '0e7b49d0-8853-445b-8e03-b7c0f884b7ed').
narrative_ontology:cs_kernel_codification('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', distributed).
narrative_ontology:cs_authority_grounding('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', practice).
narrative_ontology:cs_interpretation_layer_present('0e7b49d0-8853-445b-8e03-b7c0f884b7ed').
narrative_ontology:cs_reading_relation('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', foundational, kami_buddha_domain_complementarity).
narrative_ontology:cs_axiom_status(kami_buddha_domain_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', kami_buddha_domain_complementarity, conventional).
narrative_ontology:cs_axiom('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', foundational, practice_authority_over_doctrinal_resolution).
narrative_ontology:cs_axiom_status(practice_authority_over_doctrinal_resolution, holdable).
narrative_ontology:cs_axiom_grounding('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', practice_authority_over_doctrinal_resolution, conventional).
narrative_ontology:cs_axiom('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', secondary, death_kegare_boundary_assigns_mortuary_jurisdiction).
narrative_ontology:cs_axiom_status(death_kegare_boundary_assigns_mortuary_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', death_kegare_boundary_assigns_mortuary_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', complementary_existential_jurisdiction).
narrative_ontology:cs_drift_state('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0e7b49d0-8853-445b-8e03-b7c0f884b7ed', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, commoner_households).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, commoner_households).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, death_trade_outcaste_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_and_shogunal_authorities).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, existential_domain_complementarity).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, death_pollution_boundary_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Farm and town households route each existential need to its assigned institution: births, harvests, purification, and protection from misfortune go to the local shrine; funerals, graves, and memorial services for ancestors go to the parish temple. They pay festival dues, offering rice, and mortuary fees, and from the seventeenth century they are required by law to register with a temple and obtain a temple certificate confirming deaths in the household. Leaving the arrangement would mean finding another way to bury their dead and abandoning ancestral graves — no such infrastructure exists outside it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, commoner_households, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, commoner_households, beneficiary).

% Temple networks hold exclusive charge of death: funerals, burial, gravestones, and periodic memorial rites for ancestors, for which they collect fees and annual dues from parish households. From the seventeenth century they also administer the government's parishioner registry, issuing the certificates households need to prove compliance. Senior clerics maintain doctrinal teachings about the relation of kami to Buddhas, but the institution's daily operation does not depend on laypeople accepting any particular doctrine. A temple withdrawing from mortuary work would forfeit its parish dues and its registered constituency.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishment, beneficiary).

% Hereditary shrine families conduct the festivals, purifications, and harvest rites that mark the life side of the calendar, and receive offerings, land income, and communal labor in return. Their precincts exclude death: corpses, blood, and mourning are kept out, and mortuary business is left entirely to the temples. A priestly house abandoning its jurisdiction would dissolve the ritual identity and endowment that constitute it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods, beneficiary,
    organized, generational, identity_locked, national).

% Settled communities of leatherworkers, slaughterers, grave-diggers, and execution attendants perform the occupations that the purity boundary defines as defiling. Villages assign them death-related tasks no one else will touch; they live in designated hamlets, marry within their group, and are barred from ordinary shrine participation. Their status is fixed at birth and enforced by the surrounding communities' avoidance.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, death_trade_outcaste_communities, payer,
    powerless, generational, trapped, national).

% The court first and later the warrior governments regulate the boundary between shrine and temple jurisdictions, confirm institutional lands and ranks, and from the seventeenth century require every household to register with a Buddhist temple — turning the existing mortuary arrangement into an instrument for monitoring religious affiliation and suppressing Christianity. The rulers stand outside the religious economy they administer: they collect compliance information and public order, not fees.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_and_shogunal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_and_shogunal_authorities, beneficiary).

% Nativist scholars argue that kami worship is a self-sufficient way older than Buddhism and that shrine practice must be cleansed of Buddhist doctrine, terminology, and institutions. They publish, teach, and build networks of adherents through the eighteenth and nineteenth centuries, but hold no office in either system and are excluded from the councils where shrine-temple affairs are settled. Their program waits for a government willing to enact it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, kokugaku_nativist_scholars, excluded,
    organized, biographical, mobile, national).

% Scholars reconstruct how the dual system formed, who administered its boundaries, and what it cost whom, working from institutional archives, village records, and doctrinal texts. They take no part in the arrangement and weigh competing accounts of what held it together.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishment).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of two totalizing religious systems claiming the same population: divides existential labor so shrines handle life-side needs (harvest, purification, protection, festivity) and temples handle death-side needs (funerals, graves, ancestral memorials, salvation), letting both operate without jurisdictional collision and giving households a stable decision rule for where to take which need.
% TRANSFER_FUNCTION: Moves material support (offerings, mortuary fees, annual dues, rice stipends, festival labor) and deference from commoner households to shrine and temple institutions; moves the death-handling burden onto hereditary outcaste communities; under the Tokugawa regime, moves compliance information (registration and death verification) from households to the state through the temples.
% ABSENT_VOICES: Nativist (kokugaku) scholars who rejected Buddhist contamination of kami worship were excluded from the arrangement's legitimacy structure for the entire interval; peasant households bearing registration burdens had no formal seat in temple-bakufu negotiations; outcaste communities bore the boundary's heaviest costs with no seat anywhere. Unanimity about the arrangement's desirability arose partly because these dissenting and burden-bearing seats were never in the room where it was administered.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, households lose the routing rule for every birth, harvest, illness, and death; temples lose the mortuary economy that funds them; shrines confront death pollution their entire ritual architecture is built to refuse; the state loses the registration grid it borrowed for surveillance and must build a replacement; the outcaste occupations lose the boundary that defines and confines them. Every named seat's daily arrangements depend on the division persisting.
% FOUNDING_PROBLEM: Two expansionist religious systems — a universalist, salvation-centered Buddhism arriving with scriptural and institutional ambition, and territorial kami cults bound to land, lineage, and purity — needed to occupy the same archipelago without annihilating each other or duplicating every ritual function twice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic historiography of Japanese religion (notably Kuroda Toshio's analysis of the medieval kenmitsu order and subsequent scholarship on the danka/terauke parish system) attests that the partition originated in real jurisdictional conflicts between court-backed shrine complexes and temple networks, not in beneficent design; Meiji-era nativist writings and government inquiry records attest from the opposing side that the arrangement had outlived voluntary consent by the nineteenth century. No attestation comes only from the temple and shrine establishments that collected from the arrangement.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.45: the partition delivered genuine routing value, but from the seventeenth century the mortuary side carried compelled parishioner registration, certificate fees, and memorial obligations, and the purity boundary concentrated defiling labor on hereditary outcaste communities. Suppression is 0.40 as a raw structural property — unscaled by power or scope: compulsion was real (registration was legally mandatory; no non-temple mortuary infrastructure existed) but large stretches of the arrangement ran on customary, voluntary participation, and the coercive layer concentrates late in the interval. Theater ratio 0.28: the ritual functions were real throughout, with formalization and ceremonial elaboration growing in the Edo period. Accessibility collapse 0.40: once the boundary is understood, alternatives do not fully vanish — households negotiated dues, shifted affiliation between temples, and layered private devotion over institutional forms — but the two core exits (burial outside the temple system, death-handling outside the outcaste occupations) were effectively closed. Resistance 0.35: village disputes over dues, occasional uprisings against temple exactions, and the sustained nativist critique that eventually armed the Meiji state. Claimed type and metrics are authored independently: I claim tangled_rope because the structure shows a genuine coordination function (jurisdictional complementarity solving a real two-systems-one-population problem) with asymmetric extraction riding it and active enforcement holding the boundary; the metric values are my descriptive estimates, and the engine computes per-seat types from the structural data. The temporal series share one grid (seven points from 794 to 1868) so every metric is authored at every examined time point. Suppression_requirement is tracked because the story's enforcement history is its central dynamic: a slow ratchet from customary boundary-keeping through medieval institutional enforcement to the Tokugawa registration system, then collapse at the Meiji separation, when the enforcing machinery was abolished by edict — the 1868 drop is enforcement decay, not liberalization of the arrangement's content.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the temple seat the arrangement is a jurisdiction it staffs and funds — coordination it operates; from the household seat it is a routing rule that saves decisions but bills every death; from the outcaste seat the same purity boundary that gives shrines their clean ritual space is the wall of their segregation; from the state seat it is an administrative grid that arrived free. The engine derives these divergences from power, exit, and directional position. The sharpest divergence in the story is between the household seat (near-symmetric, dual-listed as beneficiary and payer) and the outcaste seat (full target, trapped, no offsetting benefit) — exactly the asymmetry that a pure-harmony account of shinbutsu coexistence erases. The shrine seat adds an identity-lock dimension: the priesthood's professional and institutional identity is constituted by its purity jurisdiction, so exit is unthinkable without dissolving what the priesthood is.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The temple establishment and shrine priesthoods sit at the beneficiary end: they collect jurisdiction, fees, and offerings and bear almost none of the arrangement's costs; the temples additionally enforce it, which amplifies their effective position. Commoner households are dual-listed — beneficiary for the routing value, payer for fees, compelled registration, and festival labor — placing them near symmetric, with the Tokugawa-era compulsion pushing their experienced extraction above what symmetry alone implies. Death-trade outcaste communities are full targets with trapped exit: they receive nothing from the boundary they maintain and cannot leave it. Imperial and shogunal authorities derive low directionality through arbitrage: they take compliance information and public order from the arrangement while standing outside its cost structure. The nativist scholars are excluded rather than positioned — their absence from the arrangement's legitimacy structure is itself structural data, carried in absent_voices rather than in a directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a universalist, death-oriented soteriology and a territorial, purity-oriented kami cult occupy the same population without destroying each other — was genuinely solved by the partition, and solved early. Mandatrophy analysis therefore cuts against two lazy readings. Against the romance: the arrangement was not timeless harmony; its extraction component thickened precisely as its founding problem receded, with the Tokugawa registration system converting a customary division of labor into compelled enrollment — the signature of a mandate outliving its function. Against the condemnation: the arrangement was not mere racket; the routing function was real, valued, and used by the very households who paid for it. Classifying it as tangled_rope keeps both truths load-bearing: the coordination half explains why households complied for centuries without constant coercion, and the extraction half explains why the nativist critique found traction the moment enforcement slackened. The R5 interview records the founding problem as contested rather than dead because the underlying human problem — where a community takes its dead and its fears — never expired; what expired was the necessity of this particular institutional answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Does the domain-partition reading instantiate the operative structure of the shinbutsu coexistence kernel, or do the fusion or bundle readings better model what persisted?',
    'Comparative per-seat classification across the three sibling stories: whichever reading''s structural data reproduces the observed seat divergences (household near-symmetry, outcaste full-target, temple capture) is the better instantiation.',
    'If the fusion reading is adopted, epsilon rises because kami institutions appear as subordinated extractees rather than parallel partners; if the bundle reading is adopted, the coordination-function gate fails and the arrangement reclassifies toward snare at every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which sibling reading models the kernel''s actual structure.').

omega_variable(
    partition_or_subordination,
    'Was the life/death division a genuinely parallel jurisdictional split, or did it operate inside Buddhist institutional dominance (shrine-temple complexes, jinguji subordination of shrines to temples)?',
    'Institutional records: shrine governance documents (miyaza council minutes, jinguji registers, combined shrine-temple estate finances) showing who appointed whom and who controlled revenues across the boundary.',
    'If subordination dominated, the shrine seat''s directionality moves toward target and the arrangement''s measured extraction rises; the parallel-systems framing of this reading would be a partial idealization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_or_subordination, empirical, 'Parallel jurisdictions versus Buddhist-dominated hierarchy.').

omega_variable(
    custom_versus_compulsion_share,
    'How much of household compliance was voluntary custom rather than legal compulsion, before and after the Tokugawa registration mandates?',
    'Village registers and temple death books compared across the seventeenth-century introduction of compulsory parishioner certification; discontinuities in compliance mark the compulsion frontier.',
    'If most compliance was customary, the household seat''s suppression experience drops and the arrangement computes closer to coordination at that seat; if compulsion dominated, suppression attribution shifts fully structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_versus_compulsion_share, empirical, 'Voluntary custom versus enforced registration in household compliance.').

omega_variable(
    stigma_origin_attribution,
    'Did the purity/death boundary generate the outcaste stigma, or did it map onto and reinforce a pre-existing status discrimination?',
    'Occupational and residential status records predating the partition''s consolidation (Nara and Heian tax and residence rolls) compared with later status edicts.',
    'If the boundary generated the stigma, the partition carries that harm in its epsilon; if the stigma predates it, the partition''s share of the harm is smaller and the residual belongs to the older status system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_origin_attribution, empirical, 'Origin of the death-trade stigma borne by outcaste communities.').

omega_variable(
    counterfactual_persistence_without_meiji_force,
    'Would the partition have persisted absent the Meiji state''s forcible separation, or was it already hollow?',
    'Compare trajectories of analogous multi-clergy arrangements that faced no forced separation (e.g., Chinese Buddhist-Daoist-local cult divisions) for voluntary convergence or decay patterns.',
    'Persistence-by-fit supports this reading''s practice-authority claim; persistence-only-under-compulsion supports the bundle reading and raises the enforcement dependence of the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_persistence_without_meiji_force, conceptual, 'Whether the arrangement survived on fit or on force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 794, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t794, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 794, 0.08).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1050, 0.11).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1300, 0.16).
narrative_ontology:measurement(shin_tr_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1550, 0.22).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1700, 0.29).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1800, 0.32).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1868, 0.36).

% Extraction over time
narrative_ontology:measurement(shin_be_t794, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 794, 0.22).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1050, 0.27).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1300, 0.33).
narrative_ontology:measurement(shin_be_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1550, 0.41).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1700, 0.52).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1868, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t794, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 794, 0.1).
narrative_ontology:measurement(shin_su_t1050, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1050, 0.17).
narrative_ontology:measurement(shin_su_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1300, 0.25).
narrative_ontology:measurement(shin_su_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1550, 0.36).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1700, 0.52).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1800, 0.57).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1868, 0.14).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_shinbutsu_bunri_edicts).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu-shugo' conflates three structurally distinct commitments, decomposed per the epsilon-invariance principle into three stories sharing the kernel shinbutsu_coexistence_commitment. This file instantiates the domain-partition reading (functional jurisdictional complementarity, practice-grounded, doctrinally undemanding; moderate epsilon). The syncretic-fusion reading (ontological unity via honji suijaku) carries a different epsilon — kami institutions appear as subordinated extractees under a Buddhist hierarchy — and the incoherent-bundle reading carries a different coordination structure altogether (ambiguity and power in place of a kernel). The upstream/downstream gradient runs from this reading (highest empirical confidence about what popular practice did) toward the fusion reading (elite doctrine cited as evidence for the whole) and the bundle reading (retrospective analytic claim tested at the Meiji collapse). Each member links to the others via network.affects_constraints; the meiji_shinbutsu_bunri_edicts edge records the external event that terminated the arrangement this reading describes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
