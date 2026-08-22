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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Kami-Buddha Domain Partition (Functional Coexistence Reading)
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   From roughly the 12th century onward, Japanese religious life
 *   institutionalized a functional partition: Buddhist institutions (temples,
 *   monastic orders, soteriological doctrine) governed death rites, karmic
 *   accountability, and salvation; Shinto institutions (shrines, priestly
 *   networks) governed fertility, purification, protection, and seasonal
 *   agricultural cycles. This reading construes the partition as a stable,
 *   functional commitment grounded in practice rather than theology — neither
 *   system required the other to be false; they divided the religious domain.
 *   Lay practitioners engaged both across their lifespans (Shinto for births,
 *   marriages, local festivals; Buddhism for deaths and karmic concerns)
 *   without theological incoherence because the partition itself was the
 *   commitment: separate authorities for separate existential domains. This
 *   reading COMPETES with the syncretic_fusion_reading (which claims
 *   ontological unification through honji suijaku doctrine) and the
 *   incoherent_bundle_reading (which claims the partition was never coherent
 *   but maintained through deliberate ambiguity and collapsed under Meiji
 *   pressure). The domain_partition_reading asserts that the partition WAS
 *   the coherent principle — not a failed synthesis, but a successful
 *   functional division.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy — maintains eschatological authority, soteriological doctrine, death rites
 *   - Shinto shrine networks — maintain purification, fertility, seasonal festivals, life-cycle protection
 *   - Lay practitioners — engage both systems across the lifespan without requiring theological choice
 *   - Theological systematizers — bear the cost of maintaining doctrinal partition without unification
 *   - Meiji state reformers (excluded) — would prefer clarity through subordination or explicit separation
 *   - Honji suijaku advocates (excluded) — would force explicit ontological synthesis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.38).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Kami-Buddha Domain Partition (Functional Coexistence Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'a8167912-527c-4a4a-bffd-90ed25fc1634').
narrative_ontology:cs_kernel_codification('a8167912-527c-4a4a-bffd-90ed25fc1634', distributed).
narrative_ontology:cs_authority_grounding('a8167912-527c-4a4a-bffd-90ed25fc1634', practice).
narrative_ontology:cs_interpretation_layer_present('a8167912-527c-4a4a-bffd-90ed25fc1634').
narrative_ontology:cs_reading_relation('a8167912-527c-4a4a-bffd-90ed25fc1634', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8167912-527c-4a4a-bffd-90ed25fc1634', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('a8167912-527c-4a4a-bffd-90ed25fc1634', foundational, partition_is_functional_principle).
narrative_ontology:cs_axiom_status(partition_is_functional_principle, holdable).
narrative_ontology:cs_axiom_grounding('a8167912-527c-4a4a-bffd-90ed25fc1634', partition_is_functional_principle, conventional).
narrative_ontology:cs_axiom('a8167912-527c-4a4a-bffd-90ed25fc1634', foundational, practice_authority_supersedes_doctrine).
narrative_ontology:cs_axiom_status(practice_authority_supersedes_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a8167912-527c-4a4a-bffd-90ed25fc1634', practice_authority_supersedes_doctrine, conventional).
narrative_ontology:cs_reference_frame('a8167912-527c-4a4a-bffd-90ed25fc1634', functional_partition_equilibrium).
narrative_ontology:cs_drift_state('a8167912-527c-4a4a-bffd-90ed25fc1634', meiji_reformation_1868, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a8167912-527c-4a4a-bffd-90ed25fc1634', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrine_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, theological_systematizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains authority over death rites, salvation narratives, and afterlife theology. Benefits from domain partition by concentrating institutional legitimacy on eschatology and monastic practice without competing with kami-based agricultural/fertility authority. Can shift theological commitments without losing institutional function — the partition allows doctrinal flexibility.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_hierarchy, beneficiary,
    institutional, civilizational, mobile, national).

% Maintains authority over life-cycle events, purification, harvest, and local presence. Benefits from domain partition by concentrating shrine function on practical life concerns (births, marriages, seasonal festivals) without theological competition. Can invoke Buddhist cosmology for death without threatening shrine practice — the partition secures both institutional niches.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrine_networks, beneficiary,
    organized, civilizational, mobile, national).

% Engage both kami (for immediate life needs, seasonal abundance, protection) and Buddhist practice (for death preparation, karmic accountability, salvation) within a single biographical trajectory. The partition allows practical engagement with both without requiring doctrinal consistency or choosing between institutional authorities. Identity is constituted through both — Japanese religious identity fuses kami participation and Buddhist practice.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners, beneficiary,
    powerless, biographical, identity_locked, local).

% Scholars and educated clergy attempting to construct coherent theological frameworks. Bear the cognitive and institutional cost of maintaining two ontologically separate systems without formal unification. Their professional legitimacy depends on systematicity, but the constraint requires them to work with partition rather than synthesis. Cannot force unification without destabilizing institutional authorities on both sides.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, theological_systematizers, payer,
    moderate, generational, constrained, national).

% Would have preferred clear separation or subordination (Buddhism to Shinto, or vice versa) for administrative clarity and nationalist ideology. Are excluded from the commitment because both institutional networks and lay practice resist formal state determination of the boundary. State pressure accelerates toward the constraint's collapse after Meiji 1868.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state_reformers, excluded,
    institutional, generational, trapped, national).

% Monks and theologians advocating for explicit ontological synthesis (kami as manifestations of Buddhas). Are excluded from determining the constraint's legitimacy because popular practice and institutional authorities on both sides benefit from partition without synthesis. Their inclusion would force doctrinal resolution that neither Buddhism nor Shinto wants institutionally.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, honji_suijaku_advocates, excluded,
    moderate, biographical, constrained, national).

% Examines how two ontologically distinct systems coexist without formal unification, what institutional arrangements make this possible, and how the partition breaks under external pressure (Meiji secularization, post-war reorganization).
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits both institutional networks (Buddhist monastic hierarchy and Shinto shrines) to maintain distinct functional authority and lay practitioners to engage both systems sequentially across the lifespan without requiring theological consistency or institutional subordination of either.
% TRANSFER_FUNCTION: Transfers religious legitimacy (the right to adjudicate certain life domains) to each system without forcing one to eclipse the other: Buddhism monopolizes death/karma/salvation; Shinto monopolizes life/purity/harvest. Lay participation in both becomes institutionally normal rather than theologically incoherent.
% ABSENT_VOICES: Meiji state administrators seeking administrative clarity and nationalist ideological unity; honji suijaku advocates seeking explicit ontological synthesis; Western-trained theologians expecting doctrinal unification. These voices would argue for either formal separation or formal synthesis, but are structurally excluded from determining the constraint because both institutional networks and lay practice benefit from the status quo partition.
% DISAPPEARANCE_RATIONALE: Under this reading, if the partition disappeared — if state or theological pressure forced explicit unification or subordination — both institutional networks would lose functional independence and lay practitioners would face identity rupture (forced to choose between kami-identity and Buddhist-identity). The constraint's persistence enables both systems to operate. However, rivals to this reading (the incoherent_bundle_reading and the syncretic_fusion_reading) would dispute this — they would argue the partition is itself unstable fiction that the Meiji reform only made explicit, or that synthesis was always the authentic state of affairs.
% FOUNDING_PROBLEM: Early Japanese religious development produced two institutional networks with distinct metaphysical commitments and functional domains (kami-based agricultural/protective systems; Buddhist soteriological/death-rites systems). A single lay population needed to engage both without either institutional authority subordinating the other. The founding problem was not doctrinal reconciliation but functional integration without institutional warfare.
% FOUNDING_PROBLEM_CORROBORATION: Shrine priests and Buddhist monks attest that lay practitioners continue to engage both systems across the lifespan (births at shrines, marriages and funerals often blended, seasonal festivals and Buddhist memorial services coexist). Anthropological and ethnographic work by scholars outside both institutional networks (from Yanagita Kunio onward) attests that the partition reflects lived practice rather than theological doctrine. Institutional authorities from both Buddhism and Shinto have, from medieval times onward, managed the boundary through negotiation rather than attempted synthesis — evidence from temple-shrine contracts, ritual protocols, and ordination records outside both benefiting parties.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 at interval end) because both institutional networks extract legitimacy and resources from lay participation, but neither extracts from the other — the partition prevents institutional predation. Suppression is low (0.22) because lay practitioners freely engage both systems; no coercive mechanism forces the partition on unwilling participants. Theater is modest (0.28) because the partition is genuinely functional — ceremonies happen, farmers prepare fields at shrine festivals, death rites follow Buddhist protocols — but some performative work maintains the boundary (theological systematizers produce explicit partition justifications that do not convince everyone). Accessibility_collapse is moderate (0.45) because alternatives exist (one could theoretically adopt Buddhism exclusively or Shinto exclusively, or accept the honji suijaku synthesis) but lay identity is constituted through both. Resistance is moderate (0.42) because theological systematizers and some monks resist the partition's lack of coherence, pushing toward synthesis; but institutional authorities and lay practice stabilize it. The measurement series show remarkable stability across 668 years — the partition maintained extractiveness and theater ratios nearly constant, suggesting the commitment is durable rather than precarious.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional perspective, the partition secures their authority over death and karma without interference from shrine practice. From the Shinto shrine perspective, the partition secures their authority over life-cycle protection and fertility without Buddhist metaphysical competition. From the lay practitioner perspective, the partition is invisible — it is the normal, expected structure within which biography unfolds (shrine for this life event, Buddhism for that death preparation). From the theological systematizer's perspective, the partition is a failure — two incoherent systems forced into coexistence without rational unity. These seats should compute differently: beneficiary seats perceive coordination and functional division; payer seats perceive doctrinal incoherence they must manage; excluded seats perceive institutional failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist and Shinto institutional authorities are beneficiaries (d near 0.1–0.2): they extract legitimacy and resource flows from lay engagement while bearing minimal cost — the partition protects them from mutual predation. Lay practitioners are also beneficiaries but through a different mechanism (d near 0.3–0.4): they gain the ability to engage both systems without institutional exclusion, but they are also identity-locked into the partition (cannot exit to pure Buddhism or pure Shinto without losing cultural identity). Theological systematizers are partial payers (d near 0.6–0.7): they bear the cognitive cost of maintaining two ontologically separate systems, constrained by institutional authorities who refuse synthesis. Excluded parties (Meiji reformers, honji suijaku advocates) have no directionality within this constraint — they are outside it, pushing against it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT claim the founding problem is dead — it asserts the founding problem (coordinating two institutional networks without mutual subordination) remains live and the partition solves it. This prevents mandatrophy classification. Under the incoherent_bundle_reading, by contrast, the founding problem would be diagnosed as dead (no coherent principle existed; the 'coordination' was always fiction maintained by power). Under the syncretic_fusion_reading, the founding problem would be contested (synthesis advocates claim honji suijaku WAS the solution all along). This reading's R5 verdict (founding_problem_status: live) is its vaccination against mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_synthesis_coherence,
    'Is functional partition without doctrinal unity a coherent religious commitment, or is the absence of explicit ontological synthesis evidence that the system was always incoherent and maintained only by institutional power?',
    'Examine medieval and early-modern religious texts, temple-shrine contracts, and lay practice narratives to determine whether the partition was consciously theorized as principle (coherent) or maintained as unexamined practice (potentially incoherent with power suppressing awareness). Look for explicit partition defenses vs. partition avoidance in doctrinal writings.',
    'If conscious principle (theorized and defended), the domain_partition_reading holds and extraction is moderate. If unexamined power arrangement (not theorized, deliberately ambiguous), the incoherent_bundle_reading gains support and extraction rises as hidden institutional coercion. Classification shifts from rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_synthesis_coherence, conceptual, 'Whether partition is coherent principle or incoherent maintenance through power.').

omega_variable(
    lay_identity_lock_mechanism,
    'Is lay practitioner identity-lock to both systems (identity_locked exit option) a product of religious authenticity (genuine fusion in lived experience) or institutional/cultural conditioning (the partition is internalized as natural)?',
    'Post-Meiji secularization and minority-position case studies: when lay practitioners are offered clear Buddhist-only or Shinto-only alternatives (e.g., Christian conversion, or Meiji-era youth rejecting shrine participation), how many maintain both engagements vs. choose one? What narratives do they author for their choices?',
    'If identity-lock is genuine fusion, lay practitioners are true beneficiaries despite constraint; extraction remains moderate. If identity-lock is conditioning, lay practitioners are partial victims (constrained, identity_locked, internalized suppression); extraction rises and suppression becomes internalized suppression rather than structural suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_identity_lock_mechanism, empirical, 'Whether lay identity-lock to both systems reflects authentic religious commitment or internalized conditioning.').

omega_variable(
    honji_suijaku_latency,
    'Was honji suijaku doctrine (kami as manifestations of Buddhas) a genuine but suppressed alternative that the partition excluded, or a minority intellectual position that never captured institutional or lay practice?',
    'Textual and institutional analysis: how widely was honji suijaku taught in temples and shrines? Did it compete with partition doctrine, or was it confined to scholarly circles? Did lay practitioners know and use it to resolve the partition, or remain unaware of it?',
    'If suppressed alternative, the honji_suijaku_advocates are excluded because institutional authorities benefit from partition; suppression rises, and this reading becomes politically contingent on institutional power rather than a natural principle. If minority position, the advocates remain excluded by intellectual/institutional closure rather than active suppression; partition remains more stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honji_suijaku_latency, empirical, 'Whether honji suijaku synthesis was suppressed institutional alternative or minor scholarly position.').

omega_variable(
    partition_reading_vs_kernel,
    'Is this reading a description of what the kernel shinbutsu_coexistence_commitment actually instantiates, or is it a post-hoc rationalization that imposes coherence on an always-ambiguous institutional arrangement?',
    'This is a committer-framing omega: examine whether medieval and early-modern actors explicitly theorized the partition as principle (reading affirmed) or whether the partition was implicit, never consciously articulated until modern scholarship imposed the framework (reading is committer imposition, not historical fact).',
    'If partition was explicit principle, the reading is historically grounded. If partition was implicit and retrospectively named, the reading is a modern committer frame, not a medieval institutional reality — it may still be coherent for interpreting historical materials, but it is an analytical rather than lived commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_reading_vs_kernel, conceptual, 'Whether partition-as-principle was historically instantiated or is modern analytical framing imposed on ambiguous institutional arrangements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 1200, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.25).
narrative_ontology:measurement(shin_tr_t1350, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1350, 0.26).
narrative_ontology:measurement(shin_tr_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1550, 0.27).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement(shin_tr_t1820, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1820, 0.29).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1868, 0.28).

% Extraction over time
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.35).
narrative_ontology:measurement(shin_be_t1350, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1350, 0.36).
narrative_ontology:measurement(shin_be_t1550, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1550, 0.37).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1750, 0.38).
narrative_ontology:measurement(shin_be_t1820, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1820, 0.39).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1868, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_coexistence_commitment__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel constrains three distinct readings. The domain_partition_reading (this story) asserts partition into separate existential domains as the coherent principle. The syncretic_fusion_reading asserts honji suijaku doctrine unified kami and Buddhas ontologically. The incoherent_bundle_reading asserts the partition was never coherent but maintained through institutional ambiguity and power. Each reading has different ε, different beneficiary structures, different founding problem status, and different CS axioms. They are linked as network.affects_constraints entries in each story, forming a constraint family. The partition_reading influences both siblings by establishing partition as an available interpretive frame; the syncretic_fusion_reading coexists with partition (different institutional factions hold different readings); the incoherent_bundle_reading competes with partition on the question of historical coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
