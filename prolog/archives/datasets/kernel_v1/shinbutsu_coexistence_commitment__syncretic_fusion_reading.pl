% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Shinbutsu Coexistence: Syncretic Fusion Reading (Kami-Buddha Unification via Honji Suijaku)
 *   domain: religious_philosophy/japanese_history/theological_synthesis
 *
 * SUMMARY:
 *   This constraint represents ONE reading of a contested
 *   religious-philosophical kernel: the relationship between indigenous
 *   Japanese kami and imported Buddhist theology. The syncretic fusion
 *   reading holds that kami and Buddha are ontologically unified — kami are
 *   local manifestations of universal Buddhist truth (honji suijaku). This
 *   reading emerged in the Heian period, particularly within Tendai Buddhism,
 *   and became institutionalized through the jinguji (shrine-temple) system.
 *   The constraint exhibits tangled rope structure: it coordinates genuine
 *   theological and institutional integration while simultaneously
 *   suppressing local kami traditions' autonomy and imposing doctrinal
 *   uniformity under elite theological authority. The rising theater ratio
 *   (0.35 to 0.65) reflects the constraint's evolution from genuine
 *   theological synthesis (Heian-Kamakura) toward increasingly performative
 *   maintenance of dual shrines and temples as institutional forms (Edo
 *   period and later), culminating in the formal abolition of shinbutsu
 *   coexistence during the Meiji Restoration (1868). The suppression
 *   trajectory (0.50 to 0.68) tracks the increasing institutional enforcement
 *   necessary to maintain the syncretic fusion as alternative theological
 *   frameworks became theoretically available and folk resistance
 *   accumulated.
 *
 * KEY AGENTS:
 *   - Theological Elite (Tendai/Shingon Schools): Primary beneficiary (institutional/arbitrage) — architects and maintainers of honji suijaku doctrine; expand institutional authority through unified kami-Buddhist governance
 *   - Local Kami Shrine Communities: Primary victim (powerless/trapped) — forced reinterpretation of local deities as manifestations of universal Buddhist truth; lose theological autonomy
 *   - Regional Buddhist Priesthoods: Secondary actor (moderate/constrained) — benefit from expanded congregational and institutional reach; constrained by doctrinal consistency requirements
 *   - Imperial Court / Bakufu: Institutional beneficiary (institutional/constrained) — uses syncretic framework to unify religious governance; dependent on theological elite for interpretation authority
 *   - Jinguji Institutions: Structural embodiment (institutional/constrained) — shrine-temple amalgamations that performatively enact the syncretic fusion; increasingly theatrical by Edo period
 *   - Folk Religious Practitioners: Diffuse victim (powerless/identity_locked by later period) — initially suppressed through enforcement; gradually identity-fused with syncretic framing across generations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.52).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.68).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Shinbutsu Coexistence: Syncretic Fusion Reading (Kami-Buddha Unification via Honji Suijaku)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_philosophy/japanese_history/theological_synthesis").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'db25e54a-414b-4f3f-8030-9802ecaea119').
narrative_ontology:cs_kernel_codification('db25e54a-414b-4f3f-8030-9802ecaea119', fixed_text).
narrative_ontology:cs_authority_grounding('db25e54a-414b-4f3f-8030-9802ecaea119', lineage).
narrative_ontology:cs_interpretation_layer_present('db25e54a-414b-4f3f-8030-9802ecaea119').
narrative_ontology:cs_reading_relation('db25e54a-414b-4f3f-8030-9802ecaea119', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('db25e54a-414b-4f3f-8030-9802ecaea119', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('db25e54a-414b-4f3f-8030-9802ecaea119', foundational, buddha_nature_manifests_locally_as_kami).
narrative_ontology:cs_axiom_status(buddha_nature_manifests_locally_as_kami, holdable).
narrative_ontology:cs_axiom_grounding('db25e54a-414b-4f3f-8030-9802ecaea119', buddha_nature_manifests_locally_as_kami, deontological).
narrative_ontology:cs_axiom('db25e54a-414b-4f3f-8030-9802ecaea119', foundational, theological_elite_canonical_interpretation_authority).
narrative_ontology:cs_axiom_status(theological_elite_canonical_interpretation_authority, overridden).
narrative_ontology:cs_axiom_grounding('db25e54a-414b-4f3f-8030-9802ecaea119', theological_elite_canonical_interpretation_authority, conventional).
narrative_ontology:cs_reference_frame('db25e54a-414b-4f3f-8030-9802ecaea119', unified_buddha_kami_ontology).
narrative_ontology:cs_drift_state('db25e54a-414b-4f3f-8030-9802ecaea119', edo_meiji_transition, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('db25e54a-414b-4f3f-8030-9802ecaea119', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theological_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_authority).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_traditions).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, folk_religious_autonomy).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_doctrinal_purity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL KAMI SHRINE COMMUNITIES (SNARE) — Folk practitioners embedded in regional kami veneration traditions face mandatory reinterpretation of local deities as manifestations of distant universal Buddhist truths. Their shrine autonomy and local theological authority are suppressed by the constraint that kami must be philosophically unified with Buddhism. Exit is impossible — refusing the unification doctrine risks loss of institutional legitimacy and patronage. Maximal extraction: local traditions must subordinate their theological framing to imperial-elite-approved synthesis.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL BUDDHIST PRIESTHOODS (TANGLED ROPE) — Provincial monastic communities benefit from integration with kami shrine networks (expanded congregations, land holdings, ritual authority) while bearing costs of theological constraint: they must maintain doctrinal coherence with honji suijaku synthesis or lose elite patronage. Constrained exit: leaving the syncretic framework risks loss of institutional resources and political protection, but staying requires continuous doctrinal work to maintain consistency.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THEOLOGICAL ELITE / TENDAI AND SHINGON SCHOOLS (ROPE) — The doctrinal architects of honji suijaku (particularly Tendai theologians like Saicho) experience the constraint as a coordination mechanism solving a fundamental problem: how to integrate the indigenous kami cult with universal Buddhist truth without doctrinal fragmentation. The syncretic framework expands their institutional authority, integrates kami shrine patronage networks, and provides philosophical justification for their expanded religious jurisdiction. Net beneficiary with high agency — they designed and maintain the synthesis.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL COURT / BAKUFU AUTHORITY (TANGLED ROPE) — The imperial and military authorities benefit from the syncretic framework: it legitimizes unified religious governance (both kami shrines and Buddhist temples answer to a coherent imperial religious ideology) and enables political control through the theological elite. Constrained exit: abandoning syncretic fusion risks fragmentation of religious authority structure and loss of institutional control over both priest and shrine networks.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JINGUJI INSTITUTIONS (PITON) — The shrine-temple hybrid structures (jinguji) that embody the syncretic fusion are increasingly performative by the Edo period. The doctrine persists through institutional inertia — jinguji maintain both kami altar and Buddhist altar, perform dual rituals — but the original theological necessity has atrophied. The constraint persists because institutional disentanglement would require explicit formal dissolution (requiring imperial permission), not because the syncretic framework retains functional coherence. Theater ratio high: the jinguji maintain visible synthesis regardless of whether practitioners understand or accept the honji suijaku logic.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/metaphysical perspective, the syncretic fusion appears as a necessary reconciliation of two universal religious truths: local manifestation (kami as embodied spirits) and universal principle (Buddha-nature pervading all existence). The honji suijaku doctrine resolves this as an immutable metaphysical law — kami ARE local manifestations of universal Buddhist truth by their very nature. This perspective risks naturalizing what the engine will identify as a false summit: the theological construction created by an elite interpretive authority is reframed as metaphysical necessity.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__syncretic_fusion_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, TR),
    TR >= 0.70.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The syncretic fusion reading generates asymmetric extraction: theological elites consolidate doctrinal authority over both kami shrines and Buddhist temples; local shrine communities are forced to reframe their deities in terms set by elite interpreters; imperial authority gains unified religious governance. The constraint is not maximum extraction (0.66+) because genuine coordination benefits exist — shrine-temple integration does provide institutional efficiencies, shared ritual calendars, expanded resource pools. Rising trajectory (0.38→0.52) reflects that early syncretic synthesis genuinely solved coordination problems, but over time the constraint's extractive function (maintaining elite authority, suppressing alternative theologies) becomes more prominent than its coordination function. Suppression (0.68): High. Multiple suppression mechanisms: local shrines cannot maintain autonomous theology; alternative doctrinal framings (domain partition, incoherent bundle) are de-legitimized; folk practitioners' theological autonomy is subordinated to imperial-approved synthesis. The rising trajectory (0.50→0.68) indicates increasing enforcement as the constraint's naturalness eroded — the doctrine required more active maintenance and suppression of alternatives as time passed, suggesting it was less inherently binding than the natural-law framing claimed. Theater ratio (0.65): High. By the Edo period, the syncretic synthesis had become substantially performative: jinguji maintained dual altars and dual priestly roles not because the theological coherence was obvious, but because the institutional form was mandated. Priests performed integrated rituals whose internal logic (honji suijaku) was increasingly opaque to folk practitioners and even to many monks. Rising trajectory (0.35→0.65) tracks the shift from active theological synthesis (Heian) to institutional theater (Edo-Meiji).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The theological elite experience a rope-type coordination mechanism (solving a real problem of integrating two religious traditions). Local shrine communities experience a snare-type extraction mechanism (mandatory reinterpretation with no exit). The jinguji experience a piton-type degraded institution (maintaining dual forms through institutional inertia rather than functional necessity). The imperial authority experiences tangled rope (genuine governance coordination plus constrained maintenance of authority structure). The analytical observer risks seeing mountain (immutable metaphysical truth) but the engine's false summit detector will flag that beneficiaries exist and doctrinal alternatives are suppressed, revealing the mountain as a naturalized institutional construction. The core gap: at immediate time horizon, beneficiaries see coordination (rope); at biographical time horizon, victims see extraction (snare); at generational and civilizational horizons, the constraint appears to degrade from genuine synthesis toward theatrical maintenance (piton).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim structure determines directionality values. Theological elites experience low directionality (d ≈ 0.15-0.25) — they are beneficiaries with institutional arbitrage (can exit the constraint by simply ceasing to promote syncretic fusion). Local shrine communities experience high directionality (d ≈ 0.85-0.95) — they are victims with trapped exit options (cannot revert to pre-syncretic theology without losing imperial sanction and institutional legitimacy). Regional priesthoods experience moderate directionality (d ≈ 0.55-0.65) — they are partial beneficiaries (institutional expansion) and partial victims (doctrinal constraint), with constrained exit (leaving syncretic framework risks resource loss). The imperial authority occupies institutional beneficiary position with high d value (d ≈ 0.70) due to constrained exit — while they benefit from unified religious governance, they cannot easily dissolve the constraint without losing the theological legitimacy framework they have constructed. The analytical observer at civilizational scope risks high d (d ≈ 0.72-0.75) if they adopt the natural-law framing, because the metaphysical characterization of kami-Buddha unity becomes unfalsifiable and the observer becomes locked into the elite's theological framework.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: This constraint resolves mandatrophy by explicitly positioning itself as ONE reading of a contested kernel. The syncretic fusion reading must demonstrate how it differs structurally from the domain-partition reading (kami and Buddha as separate realms) and the incoherent-bundle reading (kami and Buddha as incompatible traditions forced together by political expediency). The syncretic fusion reading's core axiom is that universal Buddhist truth manifests locally through kami — a unified metaphysical ontology. This axiom distinguishes it from domain partition (which denies unified ontology) and incoherent bundle (which acknowledges incompatibility). The classification as tangled rope reflects that this reading genuinely coordinates kami and Buddhist institutional structures while simultaneously extracting from local traditions. If domain partition were adopted, the constraint would dissolve into separate kami and Buddhist systems (two constraints, not one). If incoherent bundle were adopted, the constraint would classify as snare (pure political extraction with no genuine synthesis). The tangled rope classification is stable only for the syncretic fusion reading — other readings produce different types from the same base properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honji_suijaku_metaphysical_status,
    'Is honji suijaku a metaphysical truth about the nature of kami and Buddha, or a theological construction imposed by elite interpreters to achieve political-institutional coherence?',
    'Historical analysis of honji suijaku doctrine emergence (Heian period); evidence of pre-syncretic kami theology in local shrine traditions; documentation of resistance or alternative framings by folk practitioners',
    'If metaphysical truth: constraint is mountain (immutable nature of kami-Buddha relationship). If theological construction: constraint is tangled rope or snare (extraction mechanism maintaining elite authority over local traditions). If incoherent construction: constraint shifts toward piton (maintained by institutional inertia rather than logical necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_metaphysical_status, conceptual, 'Metaphysical necessity vs. theological construction status of honji suijaku').

omega_variable(
    kami_autonomy_suppression_mechanism,
    'What proportion of folk kami shrine suppression is structural (enforced by imperial authority) versus internalized (practitioners have adopted honji suijaku framing as their own theology)?',
    'Analysis of folk shrine ritual texts and oral traditions; study of shrine resistance during Edo/Meiji periods; examination of post-Meiji Shinto revival theology (when syncretic constraint formally dissolved) to assess whether folk practitioners reverted to pre-syncretic theology or had become genuinely identity-fused with the syncretic framework',
    'If primarily structural: suppression metric stands at 0.68. If significantly internalized: suppression persists post-dissolution due to identity fusion — indicates deep cognitive capture of folk traditions by elite doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_autonomy_suppression_mechanism, empirical, 'Structural vs. internalized suppression of local kami traditions').

omega_variable(
    theological_elite_genuine_belief,
    'Did Tendai and Shingon theological elites genuinely believe in the metaphysical truth of honji suijaku, or did they construct and deploy the doctrine instrumentally to expand institutional power?',
    'Textual analysis of elite doctrinal works (Saicho''s writings, Kukai''s Buddhist-kami integration texts); examination of correspondence and institutional records for evidence of private doubts or instrumental framing; comparison of public theological position with private institutional decisions',
    'If genuine belief: the tangled rope classification stands (elites experience the constraint as legitimate coordination + institutional benefit). If instrumental: the constraint shifts toward piton/snare (the doctrine is maintained for power, not truth; the theological work is more performative than substantive).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_elite_genuine_belief, conceptual, 'Epistemological status of theological elite belief in honji suijaku doctrine').

omega_variable(
    domain_partition_reading_availability,
    'Was a coherent domain-partition reading (kami and Buddha as separate ontological realms, each legitimate within its domain) ever available as a genuine alternative to syncretic fusion within medieval Japanese theological discourse?',
    'Textual archaeology of pre-Tendai and non-Tendai Buddhist kami theories; examination of whether domain-partition theology was suppressed or simply never theorized; analysis of Korean and Chinese Buddhist-indigenous-deity theories as potential parallel developments',
    'If domain partition was available and suppressed: the syncretic fusion reading actively excludes an alternative (closer to foreclosure). If domain partition was never coherent within the theological tradition: the fusion reading coexists with alternatives rather than foreclosing them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_reading_availability, empirical, 'Historical availability of domain-partition alternative to syncretic fusion').

omega_variable(
    meiji_abolition_reversibility,
    'When the Meiji government formally abolished shinbutsu coexistence (separating Shinto and Buddhism), why did the syncretic doctrine not spontaneously reconstitute in folk and regional practices? What prevented reinstitutionalization of honji suijaku after institutional pressure ended?',
    'Historical analysis of Meiji Shinto revival theology; evidence of whether folk shrines attempted to preserve syncretic fusion post-separation; study of how completely and how quickly the syncretic doctrine was abandoned once imperial enforcement ceased',
    'If syncretic fusion was thoroughly abandoned: indicates the constraint was pure institutional extraction (suppression lifted, the constraint dissolved). If fragments persisted despite institutional pressure against it: indicates genuine identity-fusion or path-dependency (constraint persists even without enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_abolition_reversibility, empirical, 'Post-Meiji trajectory of honji suijaku doctrine and its reversibility').

omega_variable(
    jinguji_institutional_functionality,
    'Beyond ritual performance and symbolic integration, did jinguji institutions serve genuine coordination functions (resource sharing, dual-tradition knowledge transmission) that would be lost if the shrine-temple amalgamation dissolved?',
    'Economic and institutional history of jinguji: analysis of resource flows, shrine-temple land sharing, shared personnel, integrated ritual calendars; comparison with pure-shrine and pure-temple resource bases',
    'If genuine coordination functions: the constraint retains tangled rope character (real coordination benefit plus extraction). If primarily symbolic/performative: piton classification is confirmed (maintained by inertia and theater, not functional necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jinguji_institutional_functionality, empirical, 'Functional coordination value of jinguji institutional structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t3, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t6, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(shinbutsu_syncretic_be_t3, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(shinbutsu_syncretic_be_t6, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(shinbutsu_syncretic_su_t3, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_su_t6, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_shinto_state_purification).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_dissolution).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel decomposes into three structurally distinct constraint stories, one per reading. Each reading has its own ε, its own beneficiary/victim structure, and its own terminal classification. The syncretic_fusion_reading (this file) has ε=0.52 and tangled_rope at the primary analytical perspective. The domain_partition_reading would have lower ε (pure coordination, no suppression of alternatives) and rope classification. The incoherent_bundle_reading would have higher ε (pure extraction, maximum suppression) and snare classification. They are linked as alternative readings of a single kernel, not as observable-dependent versions of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
