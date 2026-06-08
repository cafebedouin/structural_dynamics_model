% ============================================================================
% CONSTRAINT STORY: meiji_separation_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meiji_separation_enforcement, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: meiji_separation_enforcement
 *   human_readable: Meiji Shinbutsu Bunri: Forced Separation of Kami and Buddha Worship
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The Meiji shinbutsu bunri (kami-buddha separation) edicts (1868-1874)
 *   forcibly dismantled a millennium of syncretic religious practice in which
 *   kami and buddha veneration were institutionally and cosmologically
 *   integrated. The state apparatus, driven by Kokugaku nativist ideology and
 *   nation-building imperatives, redefined 'authentic' Shinto as
 *   non-Buddhist, ordered physical separation of temple-shrine complexes
 *   (jinguji), reclassified dual clergy (shasou) into distinct Buddhist or
 *   Shinto roles, and in many regions triggered violent haibutsu kishaku
 *   (anti-Buddhist) movements. This constraint is a paradigmatic snare: the
 *   coordination story (administrative modernization, jurisdictional clarity)
 *   masks systematic extraction from syncretic practitioners and institutions
 *   who bore the full cost of ideological consolidation. The constraint
 *   exhibits a false summit: nativist scholars perceived separation as
 *   restoration of natural order, but 'pure Shinto' was a Meiji construction,
 *   not a recovered antiquity. Measurements show peak extraction and
 *   suppression in 1870-1872 (height of enforcement), with theater ratio
 *   rising as initial violent enforcement gave way to bureaucratic ritual
 *   compliance.
 *
 * KEY AGENTS:
 *   - Syncretic Practitioners: Primary victim (powerless/trapped) — village-level practitioners whose cosmology fused kami-buddha veneration; faced legal punishment and social pressure for continuing ancestral practice; no exit option due to geographic rootedness and identity-constitution through syncretic tradition
 *   - Jinguji Temple Complexes: Institutional victim (moderate/constrained) — centuries-old temple-shrine complexes forced to separate architecturally, divide assets, and reclassify clergy; some negotiation capacity with local officials but could not preserve syncretic structure without state approval
 *   - Shasou Dual Clergy: Individual victim (moderate/constrained) — clergy who had served both kami and buddha ritual functions forced to choose single identity; those who became 'Shinto priests' gained state recognition but lost doctrinal autonomy
 *   - Meiji State Apparatus: Primary beneficiary (institutional/arbitrage) — extracted ideological coherence, administrative control, and nationalist legitimacy from separation; could modulate enforcement intensity regionally
 *   - Newly Created Shinto Priesthood: Mixed beneficiary-victim (organized/constrained) — former shasou who chose Shinto identity gained state patronage and professional standardization but lost syncretic flexibility; tangled rope position
 *   - Kokugaku Nativist Scholars: Ideological beneficiary (institutional/analytical) — separation vindicated their 'pure Shinto' doctrine and many were integrated into Meiji religious bureaucracy; perceived separation as natural restoration (false summit)
 *   - Honji-Suijaku Doctrinal Tradition: Abstract victim (powerless/trapped) — Buddhist scholastic framework that had provided cosmological integration for syncretic practice; no institutional advocate after separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meiji_separation_enforcement, 0.78).
domain_priors:suppression_score(meiji_separation_enforcement, 0.85).
domain_priors:theater_ratio(meiji_separation_enforcement, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meiji_separation_enforcement, extractiveness, 0.78).
narrative_ontology:constraint_metric(meiji_separation_enforcement, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(meiji_separation_enforcement, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meiji_separation_enforcement, snare).
narrative_ontology:human_readable(meiji_separation_enforcement, "Meiji Shinbutsu Bunri: Forced Separation of Kami and Buddha Worship").
narrative_ontology:topic_domain(meiji_separation_enforcement, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(meiji_separation_enforcement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(meiji_separation_enforcement, '7825a334-fcba-4e8e-9ced-3bb1bb8c4f47').
narrative_ontology:cs_kernel_codification('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', distributed).
narrative_ontology:cs_authority_grounding('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', extraction).
narrative_ontology:cs_reading_relation('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', meiji_separation_enforcement__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', meiji_separation_enforcement__pragmatic_incoherence_reading, influences).
narrative_ontology:cs_axiom('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', foundational, kami_buddha_ontological_separation).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_separation, holdable).
narrative_ontology:cs_axiom_grounding('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', kami_buddha_ontological_separation, conventional).
narrative_ontology:cs_axiom('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', foundational, pure_shinto_historical_priority).
narrative_ontology:cs_axiom_status(pure_shinto_historical_priority, overridden).
narrative_ontology:cs_axiom_grounding('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', pure_shinto_historical_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', edo_period_syncretic_equilibrium).
narrative_ontology:cs_drift_state('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', post_separation_enforcement, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('7825a334-fcba-4e8e-9ced-3bb1bb8c4f47', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, meiji_state_nationalist_project).
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, newly_created_shinto_priesthood).
narrative_ontology:constraint_victim(meiji_separation_enforcement, syncretic_practitioners).
narrative_ontology:constraint_victim(meiji_separation_enforcement, jinguji_temple_complexes).
narrative_ontology:constraint_victim(meiji_separation_enforcement, shasou_dual_clergy).
narrative_ontology:constraint_victim(meiji_separation_enforcement, honji_suijaku_doctrinal_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, kokugaku_nativist_scholars).
narrative_ontology:constraint_victim(meiji_separation_enforcement, village_syncretic_practitioners).
narrative_ontology:constraint_victim(meiji_separation_enforcement, newly_created_shinto_priesthood).
narrative_ontology:constraint_vindicates(meiji_separation_enforcement, pure_shinto_antiquity_doctrine).
narrative_ontology:constraint_vindicates(meiji_separation_enforcement, state_shinto_ideological_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village-level practitioners whose religious life integrated kami and buddha veneration for generations. The separation edicts criminalize their cosmology and force them to choose between abandoning ancestral practice or facing legal punishment. They cannot exit: their identity is constituted through syncretic practice, they are geographically rooted, and their economic survival depends on the local ritual economy. They bear the full cost of ideological consolidation with no compensating benefit.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, village_syncretic_practitioners, payer,
    powerless, biographical, trapped, local).

% Temple-shrine complexes that had operated as unified ritual sites for centuries. The separation edicts force architectural separation, asset division, and clergy reclassification. They have some institutional capacity to negotiate with local officials but cannot preserve their syncretic structure without state approval. They face physical destruction of buildings, loss of endowments, and dissolution of centuries-old institutional arrangements.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, jinguji_temple_complexes, payer,
    moderate, biographical, constrained, regional).

% Clergy who had served both kami and buddha ritual functions, often within the same institutional complex. The separation edicts force them to choose a single identity: Buddhist monk or Shinto priest. Those who choose Shinto gain state recognition and patronage but lose doctrinal autonomy and the syncretic tradition they were trained in. Those who choose Buddhism lose access to kami ritual sites and state support. They bear the cost of forced reclassification regardless of choice.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, shasou_dual_clergy, payer,
    moderate, biographical, constrained, regional).

% The Meiji government apparatus implementing separation edicts as part of nation-building strategy. Sets the agenda through legal edicts, bureaucratic monitoring, and enforcement infrastructure. Extracts ideological coherence, administrative control, and nationalist legitimacy from the separation. Can modulate enforcement intensity regionally and temporally based on resistance levels and strategic priorities. Experiences the constraint as beneficial coordination: creating clear jurisdictional boundaries and ideological unity.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, meiji_state_apparatus, agenda_setter,
    institutional, immediate, arbitrage, national).

% Former shasou who chose Shinto identity and gained state recognition as 'pure' Shinto priests. They benefit from state patronage, professional standardization, and newly clarified institutional identity. But they also bear costs: loss of doctrinal flexibility, forced conformity to state ideology, and severance from Buddhist elements of their training. They are organized through state-sponsored Shinto associations but constrained by state control. Mixed position: genuine beneficiaries of professional recognition, genuine victims of doctrinal coercion.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, newly_created_shinto_priesthood, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(meiji_separation_enforcement, newly_created_shinto_priesthood, payer).

% Nativist scholars whose 'pure Shinto' ideology is vindicated by the separation edicts. Many are integrated into the Meiji religious bureaucracy as advisors and administrators. They benefit from ideological legitimation and institutional power. They perceive the separation as restoration of natural order rather than state-imposed construction, experiencing it as mountain (natural law) rather than snare.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, kokugaku_nativist_scholars, beneficiary,
    institutional, civilizational, analytical, national).

% The Buddhist scholastic framework (honji-suijaku theory) that had provided cosmological integration for syncretic practice. Not an agent but an abstract collective good. The separation edicts suppress this doctrinal tradition: it is banned from public teaching, its institutional advocates (Buddhist clergy in jinguji) are reclassified or expelled, and its cosmological claims are delegitimized. It has no institutional advocate after separation and cannot be revived without state permission.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, honji_suijaku_doctrinal_tradition, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(meiji_separation_enforcement, honji_suijaku_doctrinal_tradition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The separation edicts solve administrative problems for the Meiji state: creating clear jurisdictional boundaries between religious institutions, standardizing clergy roles for bureaucratic legibility, and establishing ideological unity for nation-building. From the state's perspective, the constraint coordinates religious life by eliminating 'confusion' and 'foreign contamination.'
% TRANSFER_FUNCTION: The arrangement transfers ideological coherence, administrative control, and nationalist legitimacy from syncretic practitioners and institutions to the Meiji state. It transfers institutional resources (jinguji assets, clergy labor, ritual authority) from syncretic complexes to newly created 'pure' Shinto institutions. It transfers cosmological authority from Buddhist scholastic frameworks (honji-suijaku) to state-defined 'authentic' Shinto.
% ABSENT_VOICES: Buddhist institutional hierarchies (Tendai, Shingon) who had provided doctrinal authority for syncretic practice were present but subordinated — their objections were overridden by state power. Village practitioners had no representation in the policy process — the edicts were imposed from above with no consultation of those whose practice would be criminalized. The honji-suijaku doctrinal tradition had no institutional advocate after the separation began. The absent voices are those who would have defended syncretic cosmology as legitimate rather than 'confused.'
% DISAPPEARANCE_RATIONALE: If the separation edicts disappeared overnight, the world would rearrange substantially: jinguji would re-integrate architecturally, dual clergy roles would re-emerge, syncretic practice would be re-legitimized, and the 'pure Shinto' category would lose its enforcement mechanism. The constraint is not a natural fact but a state-imposed arrangement that requires active maintenance. Post-1945 religious freedom partially confirms this: some syncretic elements re-emerged when state enforcement ended, though the institutional rupture was not fully reversed.
% FOUNDING_PROBLEM: The Meiji state faced a nation-building problem: how to create ideological unity and administrative legibility in a religiously diverse population with no prior concept of 'religion' as a distinct institutional domain. The separation edicts were designed to solve this by creating clear categorical boundaries (Shinto vs. Buddhism), establishing state control over religious institutions, and constructing a 'native' tradition (Shinto) that could serve nationalist ideology. The founding problem was real from the state's perspective: administrative modernization and ideological consolidation for a new regime.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (nation-building through religious reorganization) was specific to the early Meiji period and was resolved by the 1880s when State Shinto was institutionally established. Post-1945, the problem is dead: Japan is a stable nation-state with no need for forced religious separation to maintain unity. Historical consensus (Hardacre, Ketelaar, Josephson) confirms that the separation was a Meiji-specific project, not an ongoing necessity. The constraint persists in attenuated form (institutional separation of shrines and temples) but the founding problem (ideological consolidation for new regime) no longer exists. Corroboration comes from historians outside the beneficiary set (Meiji state apparatus), not from state sources alone.
narrative_ontology:disappearance_verdict(meiji_separation_enforcement, world_rearranges).
narrative_ontology:founding_problem_status(meiji_separation_enforcement, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYNCRETIC PRACTITIONER (SNARE) — Village practitioners whose cosmology fused kami and buddha veneration face state violence for continuing millennium-old practices. Cannot exit: geographic rootedness, economic dependence on local ritual economy, identity constituted through syncretic practice. Maximum extraction: forced to choose between abandoning ancestral practice or facing legal/economic punishment.
constraint_indexing:constraint_classification(meiji_separation_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JINGUJI INSTITUTION (SNARE) — Temple-shrine complexes (jinguji) that had operated as unified ritual sites for centuries face forced architectural separation, asset confiscation, and clergy reclassification. Constrained exit: some institutional capacity to negotiate with local officials, but cannot preserve syncretic structure without state approval. High extraction: physical destruction of buildings, loss of endowments, dissolution of centuries-old institutional arrangements.
constraint_indexing:constraint_classification(meiji_separation_enforcement, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEIJI STATE (ROPE) — State sees separation as necessary coordination: creating ideological unity for nation-building, establishing clear jurisdictional boundaries between religious institutions, modernizing 'confused' folk practice. Net beneficiary: extracts legitimacy, administrative control, and ideological coherence from the separation. Arbitrage exit: state can modify enforcement intensity regionally and temporally based on resistance levels.
constraint_indexing:constraint_classification(meiji_separation_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NEW SHINTO CLERGY (TANGLED ROPE) — Former shasou (dual Buddhist-Shinto clergy) forced to choose single identity gain institutional recognition and state support as 'pure' Shinto priests, but lose doctrinal flexibility and autonomy. Mixed position: benefit from state patronage and newly clarified professional identity, but constrained by state ideology and loss of syncretic tradition. Genuine coordination function (professional standardization) entangled with extraction (forced doctrinal conformity).
constraint_indexing:constraint_classification(meiji_separation_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: KOKUGAKU NATIVIST (MOUNTAIN) — Nativist scholars see separation as restoration of natural order: Buddhism was foreign contamination, 'pure' Shinto is Japan's authentic indigenous tradition, separation merely removes historical accretion to reveal underlying truth. Perceives constraint as natural law: the ontological distinction between kami and buddha was always there, just obscured. This is a false summit — the 'pure Shinto' category is a Meiji construction, not a recovered antiquity.
constraint_indexing:constraint_classification(meiji_separation_enforcement, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational distance, the separation edicts appear as state-imposed categorical violence: a millennium of syncretic practice forcibly reclassified to serve nationalist ideology. High extraction: destruction of cultural heritage, epistemic violence (redefining 'authentic' practice), suppression of alternatives. The coordination story (administrative clarity, modernization) is cover for ideological consolidation and state control of religious life.
constraint_indexing:constraint_classification(meiji_separation_enforcement, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meiji_separation_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meiji_separation_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meiji_separation_enforcement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meiji_separation_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(meiji_separation_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The state extracted ideological coherence and administrative control while syncretic practitioners bore the full cost: destruction of jinguji architecture, loss of ritual autonomy, forced reclassification of clergy, suppression of honji-suijaku cosmology. The extraction was not total (0.78 rather than 0.95) because some practitioners adapted by maintaining syncretic practice covertly, some jinguji negotiated partial preservation, and enforcement varied regionally. Measurements show peak extraction in 1870 (0.82) during most intense enforcement, declining slightly by 1874 (0.72) as some accommodations emerged. Suppression (0.85): Very high. The constraint required massive active enforcement: separation edicts backed by legal penalties, bureaucratic monitoring of clergy reclassification, physical destruction or conversion of jinguji buildings, and in many regions mob violence (haibutsu kishaku). Alternatives were systematically suppressed: continuing syncretic practice was illegal, honji-suijaku doctrine was banned from public teaching, and dual clergy roles were abolished. Measurements show suppression peaked in 1870 (0.90) and remained high (0.80) through 1876 as enforcement infrastructure matured. Theater ratio (0.42): Moderate. Initial enforcement (1868-1870) was functionally violent with low theater (0.25-0.35): jinguji were physically separated, clergy were forcibly reclassified, and resisters faced real punishment. By 1872-1874, theater increased (0.42-0.48) as bureaucratic compliance rituals emerged: pro forma separation of already-distinct buildings, paper reclassifications of clergy who continued mixed practice covertly, and performative 'pure Shinto' rituals that incorporated Buddhist elements under different names. The theater trajectory shows Goodhart drift: as enforcement became routinized, the metric (separation compliance) diverged from the goal (ideological purification).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by structural position and identity-lock. The Meiji state sees coordination (rope): necessary administrative modernization and ideological unification for nation-building. The newly created Shinto priesthood sees mixed coordination-extraction (tangled rope): they gain professional recognition and state support but lose doctrinal autonomy and syncretic tradition. Syncretic practitioners see pure extraction (snare): their cosmology is criminalized, their institutions destroyed, their identity invalidated, with no exit option. The nativist ideologue sees natural law (mountain): separation merely reveals the authentic Shinto that was always there beneath Buddhist contamination — but this is a false summit, as 'pure Shinto' is a Meiji construction. The analytical observer sees snare: state-imposed categorical violence serving nationalist ideology, with the coordination story as cover. The gap between the state's rope and the practitioner's snare is the core measurement: the same edicts that the state experiences as beneficial coordination are experienced by practitioners as existential threat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. The Meiji state is the primary beneficiary: it extracts ideological coherence, administrative control, and nationalist legitimacy from the separation. The state's directionality is near 0.0 (full beneficiary) — the constraint subsidizes the state's nation-building project. The newly created Shinto priesthood has mixed directionality (~0.35): they benefit from state patronage and professional standardization but bear costs through loss of doctrinal flexibility and forced conformity to state ideology. Syncretic practitioners are primary victims with directionality near 1.0 (full target): they bear the full cost of separation through destruction of institutions, criminalization of practice, and invalidation of cosmology, with no compensating benefit. Jinguji institutions have high directionality (~0.75): they face asset confiscation and architectural destruction, with only marginal benefit from administrative clarity. The honji-suijaku doctrinal tradition, as an abstract collective good with no institutional advocate, has directionality of 1.0 — pure victim with no agency. Exit options modulate these values: the state has arbitrage exit (can adjust enforcement regionally), the new Shinto clergy have constrained exit (can negotiate within state framework), and syncretic practitioners have trapped exit (cannot abandon practice without abandoning identity, cannot relocate due to geographic rootedness).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the snare classification is not a misidentification of coordination as extraction. The state genuinely experiences coordination: the separation edicts solve real administrative problems (jurisdictional clarity between religious institutions, standardization of clergy roles, ideological unification for nation-building). But this coordination function is inseparable from asymmetric extraction: the benefits flow to the state while the costs are borne by syncretic practitioners who had no voice in the arrangement and no exit option. The tangled rope perspective (new Shinto clergy) confirms this: they experience both coordination (professional standardization) and extraction (forced doctrinal conformity) through the same mechanism. The constraint is not 'really' a rope mislabeled as a snare, nor 'really' a snare with a fake coordination story. It is genuinely both, and the perspectival gap between the state's rope and the practitioner's snare is the structural fact the classification system exists to measure. The false summit (nativist mountain perspective) adds a third layer: the naturalization of this contingent state project as restoration of authentic tradition, which the omega variables identify as ideological construction rather than historical recovery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_practice_coherence,
    'Did pre-Meiji practitioners hold a unified syncretic cosmology (honji-suijaku as lived ontology), operate with domain-partitioned pragmatism (kami for life, buddhas for death), or navigate contradictory frameworks without resolution?',
    'Ethnographic reconstruction from pre-Meiji ritual records, pilgrimage patterns, votive inscriptions, and folk religious texts; comparison of doctrinal claims (Buddhist scholastic) vs. practice patterns (village ritual calendars)',
    'If unified cosmology: separation is destruction of coherent worldview (higher extractiveness). If domain-partitioned: separation is jurisdictional clarification with high enforcement costs (moderate extractiveness). If pragmatic incoherence: separation is one framework imposed on many (extractiveness depends on enforcement intensity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(syncretic_practice_coherence, empirical, 'Whether pre-Meiji syncretic practice constituted unified cosmology or pragmatic coexistence').

omega_variable(
    pure_shinto_antiquity,
    'Is ''pure Shinto'' a recovered ancient tradition or a Meiji ideological construction? Did a non-Buddhist Shinto exist as a distinct tradition before honji-suijaku synthesis?',
    'Historical analysis of pre-Buddhist Japanese religious practice; archaeological and textual evidence for kami worship independent of continental influence; genealogy of ''Shinto'' as category term (first systematic use in Edo nativism, codified in Meiji)',
    'If recovered antiquity: separation is restoration (mountain from nativist perspective is genuine). If Meiji construction: separation is state-imposed categorical invention (false summit, snare from all non-state perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pure_shinto_antiquity, empirical, 'Whether ''pure Shinto'' is historical recovery or ideological construction').

omega_variable(
    enforcement_regional_variation,
    'Did enforcement intensity vary systematically by region, and if so, what determined variation: local official zeal, resistance capacity, strategic state priorities, or geographic remoteness?',
    'Regional comparison of jinguji destruction rates, clergy reclassification records, and haibutsu kishaku (anti-Buddhist violence) incidents; correlation with proximity to administrative centers, local economic conditions, and presence of Buddhist institutional power',
    'If uniform enforcement: suppression metric is accurate nationally. If high regional variation: suppression experienced heterogeneously, and some practitioners had de facto exit through geographic arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_regional_variation, empirical, 'Regional variation in separation enforcement intensity').

omega_variable(
    honji_suijaku_revival_possibility,
    'Could honji-suijaku cosmology have been revived after Meiji enforcement ended, or did the separation create irreversible institutional and epistemic rupture?',
    'Post-1945 religious freedom era: examination of whether syncretic practice re-emerged, whether jinguji were re-established, whether dual clergy roles returned; comparison of contemporary practice to pre-Meiji patterns',
    'If revival occurred: separation was temporary suppression (scaffold-like, despite lack of sunset clause). If irreversible rupture: separation successfully destroyed the tradition (snare with permanent effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_revival_possibility, empirical, 'Whether syncretic tradition could be revived post-enforcement').

omega_variable(
    false_summit_beneficiary_structure,
    'Does the nativist mountain perspective (separation as natural restoration) naturalize a contingent state project? Are there identifiable beneficiaries of the ''pure Shinto'' framing?',
    'Analysis of who gained institutional power, economic resources, and ideological authority from the separation; tracking of Kokugaku scholars'' integration into Meiji state apparatus; examination of State Shinto''s role in imperial ideology',
    'If beneficiaries identifiable and extraction measurable: mountain perspective is false summit. If no systematic beneficiaries: mountain perspective may be genuine (though historical consensus is against this).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_beneficiary_structure, empirical, 'Whether nativist natural-law framing naturalizes state extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meiji_separation_enforcement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meiji_sep_theater_1868, meiji_separation_enforcement, theater_ratio, 0, 0.25).
narrative_ontology:measurement(meiji_sep_theater_1870, meiji_separation_enforcement, theater_ratio, 2, 0.35).
narrative_ontology:measurement(meiji_sep_theater_1872, meiji_separation_enforcement, theater_ratio, 4, 0.42).
narrative_ontology:measurement(meiji_sep_theater_1874, meiji_separation_enforcement, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(meiji_sep_extract_1868, meiji_separation_enforcement, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(meiji_sep_extract_1870, meiji_separation_enforcement, base_extractiveness, 2, 0.82).
narrative_ontology:measurement(meiji_sep_extract_1872, meiji_separation_enforcement, base_extractiveness, 4, 0.78).
narrative_ontology:measurement(meiji_sep_extract_1874, meiji_separation_enforcement, base_extractiveness, 6, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(meiji_sep_suppress_1868, meiji_separation_enforcement, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(meiji_sep_suppress_1870, meiji_separation_enforcement, suppression_requirement, 2, 0.9).
narrative_ontology:measurement(meiji_sep_suppress_1874, meiji_separation_enforcement, suppression_requirement, 4, 0.85).
narrative_ontology:measurement(meiji_sep_suppress_1876, meiji_separation_enforcement, suppression_requirement, 6, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meiji_separation_enforcement, identity_coordination).
narrative_ontology:affects_constraint(meiji_separation_enforcement, state_shinto_ideology).
narrative_ontology:affects_constraint(meiji_separation_enforcement, kokugaku_nativist_doctrine).
narrative_ontology:affects_constraint(meiji_separation_enforcement, honji_suijaku_cosmology).

% DUAL FORMULATION NOTE:
% The Meiji separation is downstream of the contested kami-buddha ontology kernel and upstream of State Shinto ideology. It is structurally distinct from the honji-suijaku cosmology constraint (which has low extractiveness — a coordination mechanism for syncretic practice) and from the State Shinto ideology constraint (which has different victims and beneficiaries). The separation edicts are the enforcement mechanism that attempts to foreclose the syncretic reading and instantiate the partition reading through state violence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meiji_separation_enforcement, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
