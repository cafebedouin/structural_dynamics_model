% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Domain Partition: Functional Coordination without Ontological Fusion
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   The constraint instantiates a reading of the kernel
 *   'kami_buddha_ontology' that asserts kami and buddhas are ontologically
 *   distinct entities governing complementary but separate functional
 *   domains: Shinto authorities claim primacy over life-events, purity, and
 *   the realm of living beings; Buddhist authorities claim primacy over
 *   death, impurity, memorial services, and the post-mortem trajectory of
 *   consciousness. This reading (domain_partition) coexists with two sibling
 *   readings: honji_suijaku_monism asserts ontological identity beneath
 *   phenomenal distinction; incoherent_bundle asserts the entire arrangement
 *   is a contradiction-laden institutional accommodation without
 *   philosophical coherence. The domain-partition reading claims that kami
 *   and buddhas are metaphysically distinct and their functional
 *   complementarity reflects that distinction rather than concealing it.
 *   Historical emergence: the partition crystallized from the 8th century
 *   onward as Buddhist institutional power grew in Japan without displacing
 *   autochthonous kami-worship—a pragmatic accommodation that solved the
 *   institutional conflict. Persistence: the partition remains the operative
 *   framework for religious practice, institutional licensing, state
 *   regulation, and theological teaching in contemporary Japan, though it is
 *   continuously contested by honji-suijaku theologians within the Buddhist
 *   establishment.
 *
 * KEY AGENTS:
 *   - Shinto institutional authorities (shrine networks, doctrinal schools): maintain the kami-domain as ontologically primary and operationally distinct; justify separation by appealing to unbroken lineage and functional necessity.
 *   - Buddhist monastic institutions (temples, ordination lineages, doctrinal schools): maintain the buddha-domain as exclusive over death-rituals and soteriological transformation; some lineages assert honji-suijaku monism, others defend separatism.
 *   - Practitioners seeking dual religious services (organized as communities of faith): benefit from dual access without doctrinal conflict; exit is constrained by absence of competing institutions with equivalent scope.
 *   - State religious administrators (Edo shogunate, Meiji government, postwar Ministry of Education): use the partition to regulate institutions without theological adjudication; benefit from practitioner acceptance of dual participation.
 *   - Honji-suijaku monist theologians (excluded from this reading's framework): develop alternative interpretations asserting ontological identity; their exclusion is definitional to the domain-partition reading.
 *   - Theological competitors (heterodox movements, charismatic practitioners): structurally suppressed by the requirement that both kami and buddha access flow through certified institutional gatekeepers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.41).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.38).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.41).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Domain Partition: Functional Coordination without Ontological Fusion").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '54dbd509-2631-4cc9-94c1-88009eac3230').
narrative_ontology:cs_kernel_codification('54dbd509-2631-4cc9-94c1-88009eac3230', distributed).
narrative_ontology:cs_authority_grounding('54dbd509-2631-4cc9-94c1-88009eac3230', lineage).
narrative_ontology:cs_interpretation_layer_present('54dbd509-2631-4cc9-94c1-88009eac3230').
narrative_ontology:cs_reading_relation('54dbd509-2631-4cc9-94c1-88009eac3230', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('54dbd509-2631-4cc9-94c1-88009eac3230', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('54dbd509-2631-4cc9-94c1-88009eac3230', foundational, kami_buddha_ontological_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinction, holdable).
narrative_ontology:cs_axiom_grounding('54dbd509-2631-4cc9-94c1-88009eac3230', kami_buddha_ontological_distinction, conventional).
narrative_ontology:cs_axiom('54dbd509-2631-4cc9-94c1-88009eac3230', foundational, functional_domain_complementarity).
narrative_ontology:cs_axiom_status(functional_domain_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('54dbd509-2631-4cc9-94c1-88009eac3230', functional_domain_complementarity, instrumental).
narrative_ontology:cs_reference_frame('54dbd509-2631-4cc9-94c1-88009eac3230', dual_ontological_framework).
narrative_ontology:cs_drift_state('54dbd509-2631-4cc9-94c1-88009eac3230', contemporary_postwar, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('54dbd509-2631-4cc9-94c1-88009eac3230', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_institutional_authorities).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_monastic_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, practitioners_seeking_dual_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, state_religious_administrators).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, state_religious_administrators).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_complementarity_thesis).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, ontological_separatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and interpret Shinto doctrines governing life, purity, purification, and the realm of living kami. Authority derives from claimed continuity with pre-Buddhist Japanese tradition and control over shrine networks, ritual certification, and the production of Shinto theological literature. Define kami as ontologically prior to any Buddhist reinterpretation and assert functional primacy over all domains involving living beings and purity restoration.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_institutional_authorities, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Maintain and interpret Buddhist doctrines governing death, impurity, the deceased, and soteriological transformation. Authority derives from lineage claims to Indian Buddhist transmission and institutional control over temples, monastic ordination, funeral rites, and doctrinal interpretation. Assert that kami have no role in death-rituals, memorial services, or the post-mortem trajectory of consciousness, which belong exclusively to Buddhist cosmology and practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_monastic_institutions, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Seek purification and blessing from Shinto practitioners for life events (birth, coming-of-age, marriage, recovery from illness), then funeral and ancestral memorial services from Buddhist monastics. The partition allows both institutions to serve without doctrinal collision in the practitioner's lived religious calendar. Exit is constrained by the absence of alternative institutions offering equivalent scope and social legitimacy in early-modern to contemporary Japan.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, practitioners_seeking_dual_services, beneficiary,
    organized, biographical, constrained, national).

% Develop and promote the honji-suijaku framework asserting ontological identity between kami and buddhas. They are excluded from the domain-partition framing by definition—their entire project is to show that the partition rests on a false dualism. Their presence in the conversation would force explicit doctrinal debate about the nature of the partition itself.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, honji_suijaku_monist_theologians, excluded,
    moderate, biographical, constrained, national).

% Use the domain partition to regulate religious institutions and tax collection without enforcing theological uniformity. The partition allows state systems to recognize Shinto and Buddhism as functionally distinct (enabling different licensing, tax treatment, and regulatory oversight) without requiring the state to adjudicate their ontological relationship. They benefit from institutional stability and practitioners' acceptance of dual participation. They also bear administrative cost of managing two parallel institutional systems.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, state_religious_administrators, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, state_religious_administrators, payer).

% Indigenous or heterodox religious movements that might claim legitimacy through direct kami or buddhas without institutional mediation are structurally excluded. The partition requires practitioners to access both kami and buddhas through certified institutional gatekeepers (Shinto and Buddhist establishments), not through unmediated revelation or charismatic claim.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, theological_competitors, excluded,
    powerless, biographical, trapped, local).

% Examines the historical emergence, institutional operation, and theoretical coherence of the domain-partition framework without participating in its authority structure. Takes evidence from textual, archaeological, and institutional sources spanning the Heian to Edo periods and contemporary practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, diffuse).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables practitioners to access purification and life-blessing services (Shinto) and death-management and ancestral memorial services (Buddhism) through distinct but mutually recognized institutional channels. The partition solves the coordination problem of integrating two powerful, historically distinct traditions without requiring theological synthesis or institutional merger.
% TRANSFER_FUNCTION: Transfers practitioner authority and devotional allegiance to two institutional structures according to life-phase: Shinto institutions govern purity-maintenance and kami-worship during living life; Buddhist institutions govern ritual for death, impurity-management, and soteriological transition. The partition also transfers economic resources (donations, ritual fees, land rights) to both institutions according to domain.
% ABSENT_VOICES: Honji-suijaku monists (who argue kami and buddhas are ontologically identical) are excluded by the partition's core claim and thus unable to contest it from within the framework. Indigenous or heterodox practitioners who might claim direct kami/buddha access without institutional mediation are structurally suppressed by the requirement that both domains be professionally administered.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished and practitioners had to choose a single institutional path for all life-cycle needs, both Shinto and Buddhist institutions would lose practitioners; state administrative systems would have to redesign licensing and tax jurisdiction; and practitioners would either develop new synthetic frameworks or fragment into denomination-specific communities with different ritual access. The contemporary religious landscape in Japan is unintelligible without the partition.
% FOUNDING_PROBLEM: From the 8th century onward, two powerful, historically distinct religious systems (autochthonous Shinto kami-worship and imported Buddhism) claimed authority over overlapping domains of human concern. Direct theological synthesis was impossible without both traditions abandoning core commitments. The domain partition emerged as a practical accommodation: let each system govern its functionally appropriate domain (life/purity vs. death/impurity) without requiring ontological merger or hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion (Pierre Hadot, Kuroda Toshio, Jeffrey Robbins) corroborate that the partition solved a genuine institutional conflict between Shinto and Buddhist establishments during the Heian period and continues to structure practice today. Shinto and Buddhist authorities themselves attest that the partition persists as the operative framework; however, honji-suijaku theologians (from within the Buddhist establishment) attest that the partition rests on a false dualism and has been contested since the 9th century. The founding problem is live because the domain partition actively structures institutional practice, taxation, state regulation, and practitioner expectations, and because the theoretical adequacy of the partition remains contested within Buddhist philosophy.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).
:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.41 at interval end, rising from 0.28 at t0) because the constraint functions as genuine coordination (enabling practitioners to access both traditions) while also concentrating institutional authority and economic resources in Shinto and Buddhist establishments. The rise from t800 to t1868 (0.28 to 0.48) reflects intensifying institutional gatekeeping and doctrinal formalization during the medieval and early-modern periods when the partition crystallized from pragmatic accommodation into systematic doctrine. The decline from 1868 to 2026 (0.48 to 0.41) reflects postwar religious pluralization, state disestablishment, and declining practitioner dependency on institutional mediation (modern funeral homes offer alternatives to monastic services; state-mandated secular education provides alternative authorities on life-meaning). Suppression is moderate (0.38, rising from 0.25 at t0) because the partition is maintained by theological closure and institutional gatekeeping rather than legal prohibition. Practitioners who accept the partition are not coerced into it—they choose it for practical convenience and cultural legitimacy. But practitioners who reject the partition (honji-suijaku monists, heterodox seekers) face institutional exclusion and social delegitimacy. Theater_ratio is elevated (0.52 at interval end, rising from 0.35 at t0) because increasing institutional effort goes to performing the separation and defending it against monist critique rather than functioning as pure coordination. The high theater at t1868-1945 (0.55-0.58) reflects the Meiji Restoration's ideological drive to assert Shinto priority and state Shinto ideology, which required intensive performance of the separation to overcome honji-suijaku scholarship. The slight decline at t2026 (0.52) reflects postwar normalization but persisting institutional investment in maintaining the boundary. One shared time grid ensures all metrics are authored at the same examination points (t800, t1200, t1600, t1868, t1945, t2026).
 *
 * PERSPECTIVAL GAP:
 *   From the seat of Shinto institutional authorities, the domain partition is an accurate reflection of cosmic order (kami governing life/purity) and a legitimate assertion of institutional authority. From the seat of Buddhist monastic authorities, the partition is pragmatically necessary but philosophically problematic—honji-suijaku monism (held by some Buddhist lineages) asserts kami are manifestations of buddhas, which logically incompatible with the partition. From the seat of practitioners, the partition is an unquestioned convenience enabling dual participation. From the seat of honji-suijaku theologians, the partition conceals an incoherent institutional compromise disguised as metaphysical truth. The engine computes these divergences from the structural data: Shinto authorities and Buddhist authorities are both institutional power-atoms with identity_locked exit, beneficiaries of the partition, but their theoretical commitments conflict (Shinto asserts separation; some Buddhist lineages assert identity). Practitioners are organized, constrained-exit beneficiaries. Honji-suijaku theologians are excluded (role=excluded) and thus unable to contest the partition's framing from within. The perspectival gap is not a defect in the classification but the central diagnostic signal: institutional interests (gatekeeping, revenue, authority) are woven into the partition in a way that makes its truth-value contestable despite its practical utility.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto institutional authorities derive d near 0.0-0.2 (full beneficiaries with powerful institutional position and identity-locked commitment to the partition). Buddhist monastic authorities derive d near 0.15-0.35 (beneficiaries of the partition even while philosophically contesting it through honji-suijaku theology; their identity-lock to Buddhism creates tension between theoretical commitments and institutional interest). Practitioners derive d near 0.35-0.5 (modest beneficiaries of convenience; their constrained exit means they depend on the partition's continuation but don't directly enforce it). Honji-suijaku theologians are excluded, so directionality does not apply to them—they are outside the authorization structure the partition creates. The constraint's beneficiaries cluster in institutional positions with power and identity-lock; the victimization is diffuse (practitioners lose direct access to heterodox religious alternatives; theological competitors are suppressed) rather than concentrated in an identifiable victimized group. This produces the measured extractiveness of 0.41: substantial but not extreme, because the constraint genuinely coordinates practitioners' access and doesn't concentrate harm on a powerless subset—the suppressees are either institutional competitors (other institutions) or would-be theological innovators (relatively rare and identity-selected).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (institutional conflict between Shinto and Buddhism in the early-middle periods) remains LIVE in the sense that the partition continues to structure institutional practice and state regulation. However, the founding_problem's functional necessity has shifted: the partition originally solved the problem of preventing institutional warfare and allowing practitioners to access both traditions without doctrinal collision. In contemporary practice, the partition persists not because the problem it solved is still acute (institutional coexistence is now normalized and practitioners' religious pluralism is widely accepted), but because the partition has become embedded in institutional identity, revenue flows, and state licensing. The constraint exhibits a form of attenuated mandate-drift: the original coordination function persists weakly (practitioners still use both traditions) but a growing share of institutional energy goes to performing the separation and defending it theoretically against honji-suijaku scholarship. The theater_ratio increase (0.35 to 0.52 over the interval) is a signal of this drift—institutional performance is outpacing functional necessity. The classification as rope (pure coordination with minimal coercive overhead) remains justified because the suppression is not catastrophic and practitioners genuinely benefit from the dual access. But the rising theater and modest extractiveness indicate the constraint is drifting toward tangled_rope territory: the coordination function persists but an increasing portion of institutional effort goes to defending the partition itself rather than enabling dual participation. This is not yet full mandatrophy (which would require the founding problem to be dead and the constraint to persist solely by institutional inertia and performance), but it is a drift in that direction that should be tracked in longitudinal analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_separatism_vs_monism,
    'Are kami and buddhas truly ontologically distinct entities, or does the domain partition rest on a foundational false dualism that conceals underlying ontological identity?',
    'Systematic analysis of early Shinto and Buddhist doctrinal texts for claims of shared metaphysical grounding; examination of honji-suijaku theological literature for internal coherence and refutations from separatist theologians; comparative analysis with other religious traditions managing multiple ontologies.',
    'If kami and buddhas are ontologically identical (as honji-suijaku monists argue), the domain partition is a pragmatic accommodation of institutional interests, not a reflection of reality—reclassification to tangled_rope or snare would follow. If genuinely distinct, the partition is a coherent coordination of complementary domains—classification as rope is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_separatism_vs_monism, conceptual, 'Whether the partition reflects genuine ontological distinction or conceals underlying monism.').

omega_variable(
    institutional_motivation_vs_truth,
    'To what extent does the domain partition persist because it is philosophically true versus because it serves the institutional interests of Shinto and Buddhist establishments in maintaining separate authority domains and collecting separate revenues?',
    'Historical analysis of periods when the partition was contested or weakened (Edo sectarian debates, Meiji Shinto-promotion ideology, postwar religious liberalization) and the institutional stakes revealed in those contests; comparison of countries where Buddhism and Shinto-like traditions coexist without domain partition (Mongolia, Korea) to identify whether alternative partitioning is possible.',
    'High institutional motivation combined with weak philosophical grounding would indicate the partition functions as a snare or tangled_rope (institutional coordination with asymmetric benefit). Weak institutional motivation combined with strong philosophical grounding would confirm rope classification. The current measurement (extractiveness 0.41, suppression 0.38) sits ambiguously between these poles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_motivation_vs_truth, empirical, 'Degree to which the partition''s persistence is driven by its truth versus institutional benefit.').

omega_variable(
    suppression_mechanism_theological_vs_structural,
    'Is the suppression of honji-suijaku monism and heterodox practitioners structural (institutional gatekeeping, economic dependency, legal restriction) or theological (the monist arguments are genuinely weaker and fail to convince on their merits)?',
    'Examination of institutional actions taken against honji-suijaku theologians (censorship, doctrinal condemnation, exclusion from ordination or temple positions) versus the argumentative record of monist philosophy; survey of contemporary practitioners regarding awareness of the monist alternative and reasons for adhering to the partition; analysis of whether monist ideas spread when institutional suppression weakens.',
    'If suppression is primarily structural, the constraint''s classification shifts toward snare or tangled_rope and the theater_ratio becomes more significant as a marker of institutional maintenance cost. If suppression is primarily theological (weaker arguments fail naturally), the rope classification is strengthened and theater_ratio reflects normal institutional pedagogy rather than performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_theological_vs_structural, empirical, 'Whether suppression of monism is structural or arises from genuine theoretical weakness of the monist position.').

omega_variable(
    identity_lock_mechanism_shinto_buddhist_authorities,
    'Is the exit_options=''identity_locked'' classification for both Shinto and Buddhist institutional authorities accurate, or could they exit the domain-partition framework if institutional incentives changed?',
    'Historical study of institutional evolution during periods of state pressure (Meiji Restoration''s Shinto promotion, postwar religious disestablishment) to assess whether institutional elites abandoned partition commitments under coercion or reorganized to defend them; contemporary interviews with institutional leaders regarding perceived necessity of the partition to institutional identity versus perceived dependency on partition-enforced revenue flows.',
    'If identity_lock is genuine, the authorities cannot exit regardless of cost—the partition is woven into their fundamental self-understanding. If only rhetorical, institutional positions could shift if revenue dependency decreased—suggesting the partition is maintained primarily by economic incentive rather than metaphysical conviction, which would shift classification toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_shinto_buddhist_authorities, empirical, 'Whether institutional identity-fusion is genuine or rhetorical cover for economic interest.').

omega_variable(
    incoherent_bundle_alternative_framing,
    'Is the domain partition a coherent reading of a unified kernel (as this story asserts), or is it one element of an institutionally sustained bundle of contradictory commitments (fusion and separation, hierarchical and reciprocal) that would be classified as incoherent_bundle rather than domain_partition?',
    'Systematic examination of institutional practices and doctrinal claims across Shinto and Buddhist establishments for internal contradiction: e.g., simultaneous assertions of domain-partition and doctrinal hierarchy; separate institutional authority and doctrinal equivalence; functional complementarity and competitive exclusion. Analysis of whether these contradictions are resolved by sophisticated philosophical mediation or sustained through theatrical separation.',
    'If the institutional bundle is genuinely incoherent (contradictions unresolved), reclassification to incoherent_bundle constraint (a sibling reading) would be appropriate, and the high theater_ratio (0.52) would become diagnostic of incoherent maintenance. If the contradictions are resolvable through consistent application of the domain-partition principle, rope classification remains justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherent_bundle_alternative_framing, conceptual, 'Whether the partition is a coherent frame or part of an incoherent bundle of contradictory commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__domain_partition, theater_ratio, 800, 0.35).
narrative_ontology:measurement_basis(kami_tr_t800, projected).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__domain_partition, theater_ratio, 1200, 0.4).
narrative_ontology:measurement_basis(kami_tr_t1200, observed).
narrative_ontology:measurement(kami_tr_t1600, kami_buddha_ontology__domain_partition, theater_ratio, 1600, 0.48).
narrative_ontology:measurement_basis(kami_tr_t1600, observed).
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__domain_partition, theater_ratio, 1868, 0.55).
narrative_ontology:measurement_basis(kami_tr_t1868, observed).
narrative_ontology:measurement(kami_tr_t1945, kami_buddha_ontology__domain_partition, theater_ratio, 1945, 0.58).
narrative_ontology:measurement_basis(kami_tr_t1945, observed).
narrative_ontology:measurement(kami_tr_t2026, kami_buddha_ontology__domain_partition, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(kami_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__domain_partition, base_extractiveness, 800, 0.28).
narrative_ontology:measurement_basis(kami_be_t800, projected).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__domain_partition, base_extractiveness, 1200, 0.35).
narrative_ontology:measurement_basis(kami_be_t1200, observed).
narrative_ontology:measurement(kami_be_t1600, kami_buddha_ontology__domain_partition, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement_basis(kami_be_t1600, observed).
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__domain_partition, base_extractiveness, 1868, 0.48).
narrative_ontology:measurement_basis(kami_be_t1868, observed).
narrative_ontology:measurement(kami_be_t1945, kami_buddha_ontology__domain_partition, base_extractiveness, 1945, 0.44).
narrative_ontology:measurement_basis(kami_be_t1945, observed).
narrative_ontology:measurement(kami_be_t2026, kami_buddha_ontology__domain_partition, base_extractiveness, 2026, 0.41).
narrative_ontology:measurement_basis(kami_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t800, kami_buddha_ontology__domain_partition, suppression_requirement, 800, 0.25).
narrative_ontology:measurement_basis(kami_su_t800, projected).
narrative_ontology:measurement(kami_su_t1200, kami_buddha_ontology__domain_partition, suppression_requirement, 1200, 0.32).
narrative_ontology:measurement_basis(kami_su_t1200, observed).
narrative_ontology:measurement(kami_su_t1600, kami_buddha_ontology__domain_partition, suppression_requirement, 1600, 0.38).
narrative_ontology:measurement_basis(kami_su_t1600, observed).
narrative_ontology:measurement(kami_su_t1868, kami_buddha_ontology__domain_partition, suppression_requirement, 1868, 0.42).
narrative_ontology:measurement_basis(kami_su_t1868, observed).
narrative_ontology:measurement(kami_su_t1945, kami_buddha_ontology__domain_partition, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement_basis(kami_su_t1945, observed).
narrative_ontology:measurement(kami_su_t2026, kami_buddha_ontology__domain_partition, suppression_requirement, 2026, 0.38).
narrative_ontology:measurement_basis(kami_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__domain_partition, 0.1).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% The kernel kami_buddha_ontology decomposes into three structurally distinct constraint stories, one per reading. Domain_partition asserts ontological distinction and functional complementarity (ε ≈ 0.41, rope). Honji_suijaku_monism asserts ontological identity beneath phenomenal difference (ε expected higher, tangled_rope or snare if institutional extraction is revealed). Incoherent_bundle asserts the entire partition is a performative institutional accommodation sustaining contradictions (ε expected highest, piton or tangled_rope with high theater). These three stories share the same kernel but produce different ε values and classifications because they measure different constraints—the truth-value of the partition itself. Each story should be authored independently with its own stakeholder seats, measurements, and omega variables. This story (domain_partition) links to both siblings via network.affects_constraints; each sibling story should link back here and to the third sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
