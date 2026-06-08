% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology_flat_control, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology_flat_control
 *   human_readable: Kami-Buddha Ontological Partition in Japanese Religious Practice
 *   domain: religious_studies/japanese_religious_history/ontology
 *
 * SUMMARY:
 *   The kami-buddha ontological partition in Japanese religious practice
 *   represents a structural constraint whose status remains contested between
 *   institutional actors and folk practitioners. Beginning in the Meiji
 *   Restoration (1868), the Japanese state enforced a legal and conceptual
 *   separation between kami veneration (assigned to Shinto as national
 *   identity) and buddha veneration (assigned to Buddhism as personal
 *   philosophical choice). This partition contradicts centuries of syncretic
 *   practice (honji suijaku, shinbutsu konkogumi) where kami and buddha were
 *   understood as aspects of a unified cosmology. The constraint extracts
 *   through multiple mechanisms: it fragments practitioners' cosmological
 *   coherence, subordinates folk understanding to institutional categories,
 *   concentrates authority in competing ecclesiastical structures, and
 *   maintains power distributions favoring state-aligned institutions. Yet
 *   the constraint also solves genuine institutional coordination problems:
 *   it clarifies state religious authority, allows Buddhism institutional
 *   space distinct from Shinto nationalism, and provides practitioners with
 *   categorical frameworks for navigating religious pluralism. The
 *   extractiveness arc shows sharp escalation from 1868 to 1920
 *   (institutional consolidation and state enforcement) and partial
 *   relaxation after 1945 (postwar disestablishment), but the constraint
 *   persists in contemporary practice through inertia, legal codification,
 *   and the identity fusion of practitioners raised within the partition
 *   framework.
 *
 * KEY AGENTS:
 *   - Folk practitioners (household/village level): Powerless/identity_locked — structurally mobile but identity-fused with inherited syncretic practice; experience the constraint as cognitive fragmentation between lived practice and doctrinal categories.
 *   - Village ritual specialists: Moderate/constrained — benefit from institutional legitimation but constrained by ecclesiastical hierarchy and licensing requirements; coordinate genuine folk religious function while extracting status asymmetry.
 *   - Institutional Buddhism (monastic orders, formal priesthood): Institutional/arbitrage — primary beneficiary; chose not to abandon syncretic framework because partition redistributes authority favorably; experiences the constraint as coordination solving legitimacy problem.
 *   - Meiji Restoration bureaucracy / State Shinto movement: Organized/mobile — coordinating religious rationalization for state legitimacy; saw partition as politically necessary for unified authority; benefits from clear institutional boundaries.
 *   - Shinto priesthood (post-Meiji): Institutional/constrained — constrained by dependence on state patronage; benefits from exclusive kami cosmology but dependent on state funding; cannot exit without losing institutional legitimacy.
 *   - Analytical observer: Analytical/analytical — risks naturalizing contingent institutional partition as logical necessity reflecting genuine categorical difference between kami (immanent) and buddha (transcendent).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology_flat_control, 0.35).
domain_priors:suppression_score(kami_buddha_ontology_flat_control, 0.48).
domain_priors:theater_ratio(kami_buddha_ontology_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology_flat_control, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology_flat_control, "Kami-Buddha Ontological Partition in Japanese Religious Practice").
narrative_ontology:topic_domain(kami_buddha_ontology_flat_control, "religious_studies/japanese_religious_history/ontology").

domain_priors:requires_active_enforcement(kami_buddha_ontology_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(kami_buddha_ontology_flat_control, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, institutional_buddhism).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, shinto_priesthood).
narrative_ontology:constraint_victim(kami_buddha_ontology_flat_control, unified_cosmology_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology_flat_control, folk_religious_epistemic_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOLK PRACTITIONER (SNARE) — Structurally mobile (could relocate, change practices) but identity-fused with inherited syncretic practice. Cannot articulate their own cosmology without abandoning the identity frame their family and community assigned to them. Experiences the constraint as a trap: told the categories are separate, yet their lived practice unifies them; the divergence between doctrine and practice extracts through cognitive dissonance and identity fragmentation. No exit without becoming a different person.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE RITUAL SPECIALIST (TANGLED ROPE) — Constrained by licensing requirements and community expectations; benefits from institutional validation of their role while paying through dependence on ecclesiastical hierarchy. Genuinely coordinates folk religious practice (addresses local crises, maintains calendar, transmits knowledge) while extracting status asymmetry from the constraint. Exit available but costly: would require abandoning specialized knowledge and community position.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL BUDDHISM (ROPE) — Benefits from codification of the partition; avoids competition with kami veneration by claiming separate domains. Experiences the constraint as coordination: the partition solves the genuine problem of institutional legitimacy by allowing Buddhism to claim exclusive authority over cosmological/soteriological questions while Shinto governs naturalistic/protective functions. Arbitrage exit: can abandon the framework and return to syncretic cosmology, but chooses not to because the partition redistributes authority favorably. Net beneficiary.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEIJI RESTORATION BUREAUCRACY (ROPE) — Organized agents coordinating religious rationalization as state legitimacy strategy. The partition solves the genuine political problem of unified state authority by assigning kami veneration to national identity (Shinto) and Buddhism to private philosophical choice. Sees the constraint as coordination achieving state modernization. Mobile exit (could restore syncretism) but strategically chooses partition because it consolidates state power more effectively than alternative cosmologies.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SHINTO PRIESTHOOD (TANGLED ROPE) — Constrained by dependence on state patronage; benefits from exclusive claim to kami cosmology (avoids Buddhist competition). Coordination function: maintains ritual authority and genealogical knowledge. Extraction: forced specialization narrows scope of practice and fragments cosmological authority. Constrained exit: could abandon partition and return to syncretic practice, but would lose institutional legitimacy and state funding.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MEIJI-ERA INSTITUTIONAL FRAMEWORK (PITON) — The partition constraint persists through institutional inertia and legal codification long after its original political rationale has attenuated. Modern practitioners encounter the partition as a bureaucratic requirement (separate shrine and temple taxes, separate licensing, separate curricula) rather than as a living theological principle. Theater ratio (0.58) reflects that much of the constraint's persistence is performative maintenance of Meiji categories without corresponding cosmological conviction. The framework has degraded from functional partition (solving genuine legitimacy problem) to inertial ritual.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal analytical perspective, kami and buddha represent logically distinct ontological categories: kami as immanent protective spirits bound to place/lineage/function; buddha as transcendent being representing salvation path outside the natural order. The partition reflects genuine categorical difference, not institutional construction. Sees the constraint as a natural law following from the incompatibility of these cosmologies. However: this perspective risks false-summiting by naturaling what emerged from specific historical contingencies (Meiji political strategy, institutional competition, state rationalization). The 'logical incompatibility' is coherent but not inevitable — folk practice and earlier syncretism showed how to hold both frames simultaneously.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kami_buddha_ontology_flat_control, TR),
    TR >= 0.70.

:- end_tests(kami_buddha_ontology_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35, contemporary): Moderate. Historical measurements show sharp escalation from 1868 (0.15, early pragmatic reorganization) to 1920 (0.50, peak state enforcement and spiritual coercion through State Shinto nationalism). Contemporary value reflects partial relaxation postwar and attenuation of active coercion, though institutional embedding persists. The reduction from historical peak reflects that postwar disestablishment removed direct state enforcement; institutional Buddhism and Shinto survive through inertia rather than state power. Suppression (0.48): Moderate-high. Institutional barriers include legal categorization, separate licensing/taxation, educational separation, and differential state patronage. But suppression is not total — syncretic practice continues at household level; no absolute legal prohibition on practitioners holding both cosmologies; some ritual specialists practice across boundaries despite bureaucratic discouragement. Contemporary suppression reflects institutional inertia rather than active legal coercion. Theater ratio (0.58): Moderate-high. Contemporary maintenance of the partition is substantially performative. Meiji bureaucratic rationale (state legitimacy, religious rationalization) has attenuated; the partition persists through institutional embedding, educational curricula, and administrative categories rather than through living cosmological conviction. The sharp rise from 1868 (0.15) to 1920 (0.65) reflects transition from functional state strategy to bureaucratic ritual; contemporary value suggests partial breakdown of theater as practitioners increasingly experience the partition as arbitrary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across all indexical dimensions. Folk practitioners (powerless/trapped) see snare: they cannot articulate a unified cosmology without violating institutional categories, and their identity is fused with syncretic practice. Institutional Buddhism (institutional/arbitrage) sees rope: the partition solved the genuine legitimacy problem of competing religious authorities and distributed authority favorably. The Meiji bureaucracy (organized/mobile) saw rope: the partition was a strategic coordination mechanism achieving state modernization and unified authority. The Shinto priesthood (institutional/constrained) sees tangled rope: genuine coordination of kami cosmology combined with forced specialization narrowing their scope. The analytical observer at civilizational scope risks seeing mountain (logical incompatibility of kami/buddha categories as natural law), but this risks false-summiting by naturalizing historical institutional choices. The greatest perspectival gap lies between folk practitioners' experience (snare — cognitive fragmentation imposed from above) and institutional actors' experience (rope — coordination solving real problems). This gap reveals the constraint's asymmetric extraction: it coordinates institutional authority while fragmenting folk cosmology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Institutional Buddhism (arbitrage exit, beneficiary status): d ≈ 0.2 (low d → low chi → net subsidized). Meiji bureaucracy (mobile exit, beneficiary status): d ≈ 0.25 (low d → coordination benefit). Shinto priesthood (constrained exit, beneficiary status, but also partly victim of enforcement dependence): d ≈ 0.45 (moderate d → moderate chi → mixed). Village specialists (constrained exit, victim status): d ≈ 0.65 (high d → substantial chi → extracted from). Folk practitioners (identity_locked exit, victim status): d ≈ 0.78 (very high d → very high chi → maximum experienced extraction). The identity_locked exit option for folk practitioners is critical: they are structurally mobile (could adopt different cosmologies, relocate, enter urban secular lifestyles) but identity-fused with inherited syncretic practice. Their inability to exercise exit derives from internal cognitive frame rather than external material barriers — they carry the suppression with them even if institutional barriers were removed. This produces a perspectival gap between trapped/constrained agents and identity_locked agents at the same biographical time: trapped/constrained see the constraint as unchangeable (mountain); identity_locked see it as changeable in principle (rope) but cannot perceive the change from within their identity frame. This gap reveals the constraint's binding mechanism as cognitive rather than purely structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The kami-buddha partition was mandated to solve genuine institutional problems: (1) State legitimacy in Meiji modernization required unified religious authority; (2) Institutional Buddhism faced competition from Shinto revival movements; (3) Folk religious syncretism needed categorization for state administration. These mandates were real and the partition functionally addressed them. However, mandatrophy has occurred. The postwar disestablishment (1945) removed the state's direct need for unified religious authority. Institutional Buddhism no longer faces Shinto competition in the same way (both are now treated as private rather than state religions). Contemporary folk practitioners navigate pluralism effectively despite the partition (or despite being told the partition is natural law). The constraint persists not because its mandate is live but because it is embedded in licensing, education, and institutional structure. The archetype is piton: the partition was functional coordination (rope) that became inertial performance (piton) when its original mandate died. The theater ratio arc (0.15 → 0.65 → 0.58) captures this: initial genuine change (low theater), peak institutional solidification (high theater), then partial breakdown as performers stop believing their own performance. The mandatrophy is neither fully resolved (the partition persists and continues extracting) nor fully denied (institutional actors continue performing their Meiji-era roles). Contemporary constraint is tangled rope with piton characteristics: real institutional coordination (Buddhism/Shinto institutional separation still produces benefits for ecclesiastical actors) layered with inertial performance (bureaucratic categories maintained past their functional necessity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unified_vs_partitioned_cosmology,
    'Is the kami-buddha relationship fundamentally a unified cosmology that Meiji institutional separation artificially partitioned, or genuinely distinct ontological domains that folk practice naively conflates?',
    'Textual analysis of pre-Meiji cosmological frameworks (Edo syncretic theology, honji suijaku doctrine); comparative ethnography of contemporary folk practitioners'' actual metaphysical frameworks vs stated institutional categories; linguistic analysis of kami/buddha conceptualization in ritual contexts.',
    'If unified-then-separated: the constraint is a snare hiding coordination under extraction rhetoric (folk practitioners were wronged; institutional actors benefited from partition). If genuinely distinct: the constraint is coordination (partition reflects real category boundaries) with some extraction side effects. If ambiguous: the constraint remains tangled rope—genuine coordination with genuine asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_vs_partitioned_cosmology, conceptual, 'Whether kami-buddha partition reflects natural categories or historical institutional construction').

omega_variable(
    folk_practice_authenticity,
    'Do contemporary folk practitioners'' syncretic practices represent authentic continuation of pre-Meiji cosmology, or post-Meiji reconstructed tradition reframing institutional separation as ''folk authenticity''?',
    'Diachronic ritual documentation from Edo through Meiji to present; household shrine inventories and practice records across generations; interviews with practitioners asking about taught vs inherited vs invented elements of their practice.',
    'If authentically continuous: folk practitioners are preserving valid alternative to institutional partition (victimhood is real; exit through cultural revitalization is viable). If post-Meiji reconstruction: the folk practice itself is downstream of the partition (no uncontaminated alternative exists; extraction is total). If mixed: some lineages preserved continuity while others reconstructed, stratifying the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_practice_authenticity, empirical, 'Whether syncretic folk practice is authentic continuation or post-Meiji reconstruction').

omega_variable(
    institutional_necessity,
    'Did the kami-buddha partition genuinely solve an institutional legitimacy problem in Meiji state formation, or was it a choice among viable alternatives that happened to concentrate power favorably?',
    'Comparative analysis: how did other East Asian polities with similar religious pluralism (China, Korea, Vietnam) structure institutional religious authority? Were there Meiji-era Japanese proposals for syncretism-preserving institutional frameworks that were rejected in favor of partition? Analysis of primary sources showing whether contemporary leaders framed partition as necessary or strategic.',
    'If genuinely necessary: the constraint was rope (coordination solving real problem). If strategic choice: the constraint was snare (unnecessary extraction disguised as coordination). If necessary-but-strategically-amplified: the constraint was tangled rope (solved real problem but extracted more than necessary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_necessity, empirical, 'Whether kami-buddha partition was institutionally necessary or strategically chosen').

omega_variable(
    identity_lock_mechanism,
    'For folk practitioners who accept the partition, is the binding mechanism structural (material barriers to exit: legal categorization, licensing requirements, economic dependence) or primarily identity-based (cognitive frame that makes syncretic cosmology literally unthinkable)?',
    'Ethnographic observation of practitioners who leave syncretic practice or attempt cosmological innovation; semi-structured interviews asking about perceived vs actual barriers to crossing institutional boundaries; longitudinal tracking of how practitioners'' cosmologies shift when structural barriers are removed (e.g., diaspora communities without licensing infrastructure).',
    'If structural: exit is possible if barriers are removed (policy solutions exist). If identity-based: exit requires identity transformation (deeper binding than material barriers); classification is identity_locked rather than trapped. If both: suppression profile is higher than the base_properties value suggests (internalized suppression persists after structural barriers are removed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether folk practitioner binding is structural or identity-based').

omega_variable(
    meiji_framework_persistence_rationale,
    'Does the kami-buddha partition persist because contemporary actors find it functionally useful, because it is institutionally embedded and costly to change, or because it produces authority distributions that benefit incumbent institutions?',
    'Cost-benefit analysis: what would institutional Buddhism and Shinto priesthood gain/lose if partition were formally dissolved and syncretism legally permitted? Survey of contemporary Buddhist and Shinto institutional actors asking about perceived costs of maintaining vs abandoning partition. Policy-simulation analysis: if partition were removed tomorrow, which institutional arrangements would change and who would bear costs?',
    'If functionally useful: constraint is rope (coordination with real benefits). If institutionally embedded: constraint is piton (inertial maintenance of degraded framework). If authority-preserving: constraint is snare (extraction disguised as coordination). Understanding the persistence rationale separates institutional necessity from entrenched power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_framework_persistence_rationale, empirical, 'Why the kami-buddha partition persists in contemporary practice').

omega_variable(
    syncretism_cosmological_coherence,
    'If folk practitioners simultaneously venerate kami and buddha in the same framework, what is the actual cosmological structure they hold? Is it incoherent (arbitrary conflation), pragmatically functional (different domains without unified ontology), or coherently unified (single underlying principle)?',
    'Detailed elicitation of folk practitioners'' cosmological frameworks through narrative interviews, ritual questioning, and decision-tree analysis of how practitioners navigate contradictions; comparative analysis with historical syncretic theologies (honji suijaku, shinbutsu konkogumi); linguistic analysis of how practitioners talk about kami and buddha causality, personhood, and agency.',
    'If incoherent: folk practice is naïve conflation that partition could clarify (extraction may be justified). If pragmatically functional: folk practice solves real coordination problem (syncretic cosmology works; partition is unnecessary extraction). If coherently unified: folk practice instantiates valid alternative ontology that partition violently suppresses (victims are right; false-summit mountain claim is exposed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretism_cosmological_coherence, conceptual, 'Actual ontological structure of simultaneous kami-buddha veneration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology_flat_control, 1868, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kamib_theater_1868_genuine_change, kami_buddha_ontology_flat_control, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(kamib_theater_1890_bureaucratic_solidification, kami_buddha_ontology_flat_control, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(kamib_theater_1920_state_shinto_ritual, kami_buddha_ontology_flat_control, theater_ratio, 1920, 0.65).
narrative_ontology:measurement(kamib_theater_1945_postwar_maintenance, kami_buddha_ontology_flat_control, theater_ratio, 1945, 0.7).
narrative_ontology:measurement(kamib_theater_2000_inertial_framework, kami_buddha_ontology_flat_control, theater_ratio, 2000, 0.58).

% Extraction over time
narrative_ontology:measurement(kamib_extractiveness_1868_meiji_restoration, kami_buddha_ontology_flat_control, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(kamib_extractiveness_1890_institutionalization, kami_buddha_ontology_flat_control, base_extractiveness, 1890, 0.45).
narrative_ontology:measurement(kamib_extractiveness_1920_state_shinto_peak, kami_buddha_ontology_flat_control, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(kamib_extractiveness_1945_postwar_ambiguity, kami_buddha_ontology_flat_control, base_extractiveness, 1945, 0.48).
narrative_ontology:measurement(kamib_extractiveness_2000_contemporary, kami_buddha_ontology_flat_control, base_extractiveness, 2000, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(kamib_suppression_1868_initial_enforcement, kami_buddha_ontology_flat_control, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(kamib_suppression_1890_peak_enforcement, kami_buddha_ontology_flat_control, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(kamib_suppression_1920_state_power, kami_buddha_ontology_flat_control, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(kamib_suppression_1945_postwar_relaxation, kami_buddha_ontology_flat_control, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(kamib_suppression_2000_institutional_inertia, kami_buddha_ontology_flat_control, suppression_requirement, 2000, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology_flat_control, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology_flat_control, state_shinto_nationalism).
narrative_ontology:affects_constraint(kami_buddha_ontology_flat_control, buddhist_institutional_autonomy).
narrative_ontology:affects_constraint(kami_buddha_ontology_flat_control, syncretic_ritual_suppression).

% DUAL FORMULATION NOTE:
% The kami-buddha ontological partition is a family of related constraints. State Shinto nationalism (upstream) depends on the ontological partition for its claim that kami veneration is uniquely Japanese national identity. Buddhist institutional autonomy (lateral) derives coordinated benefit from separation while also constraining scope. Syncretic ritual suppression (downstream) is the manifest extraction mechanism through which the ontological partition enforces institutional separation at the folk level. This story models the partition itself as a tangled coordination-extraction hybrid. Sibling constraints would model the nationalist appropriation (snare) and the suppression mechanisms separately (snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology_flat_control, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
